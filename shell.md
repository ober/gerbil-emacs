# Integrating gerbil-shell (gsh) into gemacs

## Overview

Replace gemacs's bash-forking shell modes with gsh — a full POSIX-compatible shell written in Gerbil Scheme. This eliminates external shell dependencies, gives eshell real shell semantics, and unifies all command execution under one engine.

### Current State

gemacs has 4 shell-related modules:

| Module | What it does | Limitations |
|--------|-------------|-------------|
| `eshell.ss` | Toy Gerbil shell (~15 builtins, regex "parser") | No real pipes, redirects, variables, glob, quoting |
| `shell.ss` | Spawns `$SHELL` (bash) via PTY, strips ANSI | Forks bash, no Gerbil integration |
| `terminal.ss` | Spawns `$SHELL` via PTY with ANSI color rendering | Forks bash, full PTY dependency |
| `subprocess.ss` | Runs `/bin/sh -c cmd` for one-shot commands | Forks sh for every grep/make/compile |

Plus ~50 direct `open-process` calls in `editor-cmds-b.ss`, `editor-extra-vcs.ss`, `editor-extra.ss`, and Qt `commands-*.ss` for git, grep, make, etc.

### What gsh Provides

gsh is a complete POSIX shell with bash compatibility, built from first principles in Gerbil:

- **Full lexer → parser → expander → executor pipeline** (no regex hacks)
- **35+ builtins**: echo, printf, cd, pwd, export, declare, local, read, mapfile, test, history, alias, eval, exec, source, jobs, fg, bg, wait, kill, trap, etc.
- **Pipelines**: `|`, `|&` with real fd plumbing via `ffi-pipe-raw` + `ffi-dup2`
- **Redirections**: `<`, `>`, `>>`, `2>&1`, `<>`, heredocs (`<<`, `<<<`), fd dup, named fds
- **Variable expansion**: `${var:-default}`, `${var##pattern}`, `${var//old/new}`, `${#var}`, substring, indirect, arrays, associative arrays
- **Compound commands**: if/elif/else, for, while, until, case, select, `[[ ]]`, `(( ))`
- **Brace expansion**: `{a,b,c}`, `{1..10..2}`, nested
- **Glob**: `*`, `?`, `[...]`, extglob (`@()`, `?()`, `*()`, `+()`, `!()`)
- **C-like arithmetic**: `$((2 ** 10))`, `((x++))`, hex/octal
- **Job control**: fg, bg, wait, Ctrl-Z
- **Signal handling**: trap, SIGCHLD management via C-level flag handlers
- **History**: persistent, `!!`/`!N` expansion
- **Completion**: command, filename, variable
- **Gerbil REPL**: `,expr` syntax for evaluating Gerbil expressions at the shell prompt
- **FFI**: fork-exec, dup2, pipe, termios, signal masking — all in C, no external deps

---

## Phase 1: Add gsh as a Dependency

### 1a. Link gerbil-shell locally

```bash
gerbil pkg link gerbil-shell ~/mine/gerbil-shell
```

This makes `:gsh/...` modules importable via `GERBIL_LOADPATH`.

### 1b. Update `gerbil.pkg`

```scheme
(package: gemacs)
(depend: (github: "ober/gerbil-pcre2")
         (github: "ober/gerbil-scintilla")
         (github: "ober/gerbil-qt")
         (github: "ober/gerbil-shell"))
```

### 1c. Update `build.ss`

Add gsh package resolution and link flags:

```scheme
;; gerbil-shell package path
(def gsh-base (or (getenv "GEMACS_GSH_BASE" #f)
                  (find-pkg-source "gerbil-shell")))
```

gsh's `ffi.ss` compiles C code (fork-exec, dup2, pipe, termios, signal handlers). The gemacs exe targets pick up these `.o` files automatically through gxc module compilation — no manual linking needed as long as gsh modules are imported.

### 1d. Update Makefile / Docker

The static build (`make static-qt`) needs gsh compiled inside the Docker image:

```dockerfile
# In Dockerfile.deps
RUN gerbil pkg install github.com/ober/gerbil-shell
```

Add to `Makefile`:

```makefile
# Build gsh dependency before gemacs
build-deps:
	cd $(HOME)/mine/gerbil-shell && gerbil build
```

### 1e. Tier Selection

gsh has a tiered build system:

| Tier | Includes | Size Impact |
|------|----------|-------------|
| **tiny** | Core shell, builtins, pipes, redirects, job control, history, completion | Minimal |
| small | +eval builtin (Gerbil eval at shell prompt) | Small |
| medium | +in-process Gerbil compiler | Moderate |
| large | +GNU coreutils equivalents (grep, sed, awk, etc.) | Large |

**Recommendation**: Use **tiny** tier for gemacs. The core shell is all we need — gemacs already has its own grep, compile, and tool-running infrastructure. The tiny tier avoids pulling in `gerbil-coreutils` as a transitive dependency.

### 1f. Shared Dependencies

Both gemacs and gsh depend on `gerbil-pcre2`. No conflict — Gerbil's package system deduplicates shared dependencies.

---

## Phase 2: Replace eshell.ss with gsh-powered eshell

**Effort**: Medium | **Impact**: Massive — real shell semantics in the editor

The current `eshell.ss` is a ~600-line toy. gsh replaces it with a complete POSIX shell.

### 2a. Create `gsh-eshell.ss`

New module that wraps gsh for in-process eshell use:

```scheme
(export gsh-eshell-init!
        gsh-eshell-execute
        gsh-eshell-complete
        gsh-eshell-prompt)

(import :gsh/environment
        :gsh/lexer
        :gsh/parser
        :gsh/executor
        :gsh/expander
        :gsh/builtins
        :gsh/registry
        :gsh/functions
        :gsh/arithmetic
        :gsh/ffi
        :gsh/signals
        :gsh/jobs
        :gsh/redirect
        :gsh/pipeline
        :gsh/control
        :gsh/history
        :gsh/startup
        :gsh/completion)
```

Each eshell buffer gets its own `shell-environment` (from `make-shell-environment`). Commands are parsed via `parse-complete-command` and executed via `execute-command`.

### 2b. Wire up gsh callbacks

gsh uses parameters to break circular dependencies. These must be set before first use:

```scheme
(def (gsh-eshell-init! env)
  ;; Set the execute-input callback (needed for $() and eval)
  (*execute-input* (lambda (input env) (execute-string input env)))
  ;; Set arithmetic evaluator (needed for integer variable attrs)
  (*arith-eval-fn* arith-eval)
  ;; No interactive signal traps in embedded mode
  (*process-traps-fn* (lambda (env) (void)))
  ;; Not an interactive tty shell
  (*interactive-shell* #f))
```

### 2c. Capture command output

The tricky part. gsh builtins write to `current-output-port`, but external commands write to real fd 1.

**Strategy**: Create a pipe pair, redirect fd 1 to the write end, execute the command, read from the read end:

```scheme
(def (gsh-eshell-execute input env)
  "Parse and execute INPUT via gsh, return (values output-string exit-status)."
  (let-values (((read-fd write-fd) (ffi-pipe-raw)))
    ;; Save real fd 1, redirect to pipe write end
    (let ((saved-fd1 (ffi-dup 1)))
      (ffi-dup2 write-fd 1)
      (ffi-close-fd write-fd)
      ;; Execute with captured stdout
      (let ((status
             (parameterize ((current-output-port (fdopen write-fd "w")))
               (let ((ast (parse-complete-command
                            (make-shell-lexer input) env)))
                 (if ast (execute-command ast env) 0)))))
        ;; Restore fd 1
        (ffi-dup2 saved-fd1 1)
        (ffi-close-fd saved-fd1)
        ;; Read captured output
        (let ((output (read-all-from-fd read-fd)))
          (ffi-close-fd read-fd)
          (values output status))))))
```

The exact fd plumbing will need refinement — gsh's redirect module already has `apply-redirections` / `restore-redirections` that handle save/restore of fds. Reuse those.

### 2d. Handle `cd` synchronization

gsh's `cd` builtin modifies its environment's `$PWD` and the process `current-directory`. Keep this in sync with the eshell buffer's "current directory" concept:

```scheme
;; After each command execution:
(let ((new-cwd (env-get env "PWD")))
  (when new-cwd
    (hash-put! *eshell-state* buf (hash (cwd new-cwd) ...))))
```

### 2e. Gerbil expression evaluation

gsh supports `,expr` for evaluating Gerbil at the shell prompt. This replaces eshell's `(expr)` syntax and is strictly more powerful — full Gerbil stdlib access, persistent definitions.

### 2f. Delete old `eshell.ss`

Once gsh-eshell works, remove the toy implementation and update `build.ss` to compile `gsh-eshell.ss` instead.

---

## Phase 3: Replace subprocess.ss with gsh execution

**Effort**: Medium | **Impact**: Eliminates bash fork for M-!, grep, compile

Currently `subprocess.ss` forks `/bin/sh -c cmd` for every command invocation.

### 3a. Create `gsh-subprocess.ss`

```scheme
(export run-command-capture
        run-command-capture/qt)

(def (run-command-capture cmd-string
                         cwd: (cwd (current-directory))
                         stdin-text: (stdin-text #f)
                         env-vars: (env-vars []))
  "Parse and execute CMD-STRING via gsh, capturing stdout+stderr as a string.
   Returns (values output-string exit-status)."
  ...)
```

Internally:
- Create a fresh `shell-environment` (or reuse a shared one per cwd)
- Parse `cmd-string` via `parse-complete-command`
- Execute via `execute-command` with output capture (same fd plumbing as Phase 2)
- Return captured output + exit status from `env-get-last-status`

### 3b. Add interruptibility

C-g must be able to abort running commands:

- **TUI**: Check `peek-event` between pipeline stages. For long-running external processes: gsh's `ffi-fork-exec` returns a pid — send SIGTERM on interrupt.
- **Qt**: Check `quit-flag?` between pipeline stages. Same SIGTERM-on-interrupt for external processes.

### 3c. Migrate callers gradually

Replace `run-process-interruptible` calls one by one:

1. `cmd-shell-command` (M-!) — highest value, exercises full shell parsing
2. `cmd-grep` / `cmd-rgrep` — benefit from gsh's glob and quoting
3. `cmd-project-compile` — benefit from proper env variable handling
4. Keep direct `open-process` calls for simple git commands initially

---

## Phase 4: Replace shell.ss with gsh interactive mode

**Effort**: Medium | **Impact**: Native gsh REPL in shell buffers

### 4a. In-process shell buffer

Instead of spawning bash via PTY:

- Create a gsh environment for the buffer
- On user input (Enter), parse and execute via gsh
- Capture output and insert into the buffer
- Display gsh's prompt (from `prompt.ss`)

```scheme
(def (shell-buffer-execute! buf input)
  (let* ((env (hash-get *shell-envs* buf))
         (output status) (gsh-eshell-execute input env))
    (buffer-insert! buf output)
    (buffer-insert! buf (gsh-prompt env))))
```

### 4b. Completion integration

gsh has `completion.ss` with command, filename, and variable completion. Wire this to gemacs's completion UI:

```scheme
(def (shell-complete prefix env)
  "Return completion candidates for PREFIX using gsh completion engine."
  (complete-word prefix env))
```

Connect to helm or minibuffer tab-complete.

### 4c. History integration

gsh has `history.ss` with persistent history, `!!`/`!N` expansion, and reverse search. Options:

- **Replace** gemacs's `shell-history.ss` entirely with gsh's history
- **Bridge**: Sync gsh's history with gemacs's history ring on each command

### 4d. Delete old `shell.ss`

Remove and update `build.ss`.

---

## Phase 5: Terminal mode — hybrid approach

**Effort**: Large | **Impact**: Keep ANSI rendering, use gsh dispatch

This is the most complex phase. `terminal.ss` provides a vterm-like experience with ANSI colors for programs like `vim`, `htop`, `less`.

### 5a. Keep `terminal.ss` ANSI rendering

The ANSI parser (`parse-ansi-segments`, `color-to-style`, `terminal-insert-styled!`) is independent of the shell backend. Keep it.

### 5b. Use gsh for command dispatch

Replace the PTY-backed bash subprocess with gsh:

- gsh parses the command
- **Builtins / simple commands**: Execute in-process, capture output, render in buffer
- **Interactive programs** (vim, less, htop): Spawn via gsh's `ffi-fork-exec` with a PTY allocated by gemacs's terminal module, feed PTY output through the existing ANSI parser

### 5c. Detection heuristic

Determine whether a command needs a PTY:

```scheme
(def *interactive-programs* '("vim" "vi" "nano" "less" "more" "htop" "top" "man" "ssh"))

(def (needs-pty? cmd-name)
  (or (member cmd-name *interactive-programs*)
      ;; Heuristic: if stdout is used interactively
      (isatty? 1)))
```

### 5d. Long-term: proper terminal emulator

A real terminal emulator (cursor positioning, scrollback, alternate screen buffer) is a separate project. The hybrid approach above covers 90% of use cases without it.

---

## Phase 6: Migrate one-shot process calls

**Effort**: Small per call | **Priority**: Low — do opportunistically

### 6a. Utility function

```scheme
(def (gsh-exec cmd . args)
  "Execute CMD with ARGS via gsh, return output string."
  (let-values (((output status) (run-command-capture (shell-quote-join cmd args))))
    output))
```

### 6b. Migration targets

The ~50 direct `open-process` calls across:

- `editor-cmds-b.ss` — git status, git log, git diff, git blame, grep, make, ls
- `editor-extra-vcs.ss` — git add, pull, push, tag, stash
- `editor-extra.ss` — rgrep, date, shell commands
- Qt `commands-*.ss` — same patterns mirrored for Qt

**Recommendation**: Migrate only when touching these files for other reasons. The direct `open-process` calls work fine and are simple. Git commands in particular gain nothing from shell parsing.

---

## Build & Linking Details

### Symbol Conflicts

gsh uses `(export #t)` in most modules. Watch for name collisions:

| gsh symbol | gemacs symbol | Resolution |
|-----------|--------------|------------|
| `history` module | `shell-history.ss` | Use `(only-in :gsh/history ...)` |
| `completion` module | helm/minibuffer completion | Use `(only-in :gsh/completion complete-word)` |
| `strip-ansi-codes` | `shell.ss` exports same | Remove gemacs's version, use gsh's or keep separate |

Use selective imports everywhere:

```scheme
(import (only-in :gsh/executor execute-command)
        (only-in :gsh/parser parse-complete-command parser-needs-more?)
        (only-in :gsh/environment make-shell-environment env-get env-set!)
        ...)
```

### Static Build

The Docker deps image needs gsh. Add to `Dockerfile.deps`:

```dockerfile
COPY gerbil-shell /src/gerbil-shell
RUN cd /src/gerbil-shell && GSH_TIER=tiny gerbil build
```

### Link Flags

gsh's `ffi.ss` C code links against libc only (no additional libraries for tiny tier). The existing `-lpthread` in gemacs's link flags covers everything gsh needs. No new `-l` flags required.

---

## Key Risks & Mitigations

### 1. FFI Conflicts: Gambit scheduler fd management

**Risk**: gsh calls `move-internal-fds-high!` at startup to relocate Gambit's scheduler pipe fds to >= 100, freeing fds 3-9 for shell redirections. If this runs inside gemacs, it could break gemacs's event loop.

**Mitigation**: Add an `embedded-mode` flag to gsh that skips `move-internal-fds-high!`. Gemacs doesn't need fds 3-9 for user shell redirects — it controls the execution environment.

### 2. SIGCHLD Handling

**Risk**: gsh uses C-level flag handlers (`ffi-sigchld-block`/`ffi-sigchld-unblock`) around fork-exec. gemacs Qt already has SIGCHLD issues (why `run-process-interruptible/qt` skips `process-status`).

**Mitigation**: Use gsh's `ffi-waitpid` instead of Gambit's `process-status` everywhere. gsh's approach (C-level SIGCHLD masking + explicit waitpid) is more robust than Gambit's async signal handling.

### 3. Global State

**Risk**: gsh uses parameters (`*execute-input*`, `*interactive-shell*`, `*in-subshell*`, etc.) that assume single-shell-per-process. Multiple eshell buffers could conflict.

**Mitigation**: Create a `with-gsh-context` macro that `parameterize`s all gsh parameters for each buffer's execution. Each buffer has its own `shell-environment`; the parameters just need to be set per-invocation.

```scheme
(def (with-gsh-context env thunk)
  (parameterize ((*execute-input* (lambda (input e) (execute-string input e)))
                 (*arith-eval-fn* arith-eval)
                 (*interactive-shell* #f)
                 (*in-subshell* #f)
                 (*process-traps-fn* (lambda (e) (void))))
    (thunk)))
```

### 4. Build Time

**Risk**: gsh is ~30 modules. `arithmetic.ss` alone is 28k lines. Static builds will take longer.

**Mitigation**: Use tiny tier. Build gsh modules with parallelism (already the default in `build.ss`). gsh modules only compile once and are cached.

### 5. Terminal Programs

**Risk**: Programs like vim/htop need a real PTY. In-process gsh execution doesn't provide one.

**Mitigation**: Phase 5's hybrid approach — detect interactive programs and spawn them with a PTY. Most shell-in-editor usage is non-interactive commands (ls, grep, make, git) that work fine without a PTY.

---

## Execution Order

| Step | Phase | Effort | Impact | Dependencies |
|------|-------|--------|--------|-------------|
| 1 | Package linkage | Small | Unblocks everything | None |
| 2 | Replace eshell.ss | Medium | Real shell in editor | Phase 1 |
| 3 | Replace subprocess.ss | Medium | No more bash forks for M-!, grep, compile | Phase 1 |
| 4 | Replace shell.ss | Medium | Native gsh REPL in shell buffers | Phases 2-3 |
| 5 | Terminal hybrid | Large | ANSI rendering + gsh dispatch | Phase 4 |
| 6 | Migrate one-shot calls | Small/each | Low priority | Phase 3 |

Phases 2 and 3 can be done in parallel. Phase 4 builds on lessons from both. Phase 5 is optional and can be deferred. Phase 6 is opportunistic — do it when touching those files.

---

## Files Changed (Summary)

### New files
- `gsh-eshell.ss` — gsh-powered eshell (replaces `eshell.ss`)
- `gsh-subprocess.ss` — gsh-powered command runner (replaces `subprocess.ss`)

### Modified files
- `gerbil.pkg` — add gsh dependency
- `build.ss` — add gsh package resolution, update module list
- `Makefile` — add gsh build step
- `Dockerfile.deps` — add gsh to static build
- `editor-text.ss` — update `cmd-shell-command` to use gsh
- `editor-cmds-b.ss` — migrate grep/compile callers
- `qt/commands-*.ss` — mirror changes for Qt layer
- `app.ss` / `qt/app.ss` — update shell/terminal buffer polling

### Deleted files
- `eshell.ss` — replaced by `gsh-eshell.ss`
- `subprocess.ss` — replaced by `gsh-subprocess.ss` (after full migration)
- `shell.ss` — replaced by gsh interactive mode (Phase 4)
