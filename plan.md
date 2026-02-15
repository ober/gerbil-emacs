# Gerbil-Emacs: Comprehensive Plan to Replace Doom Emacs

Based on analysis of `~/doomacs/config.el`, `init.el`, and `packages.el`.

## Build

```
HOME=/home/jafourni GERBIL_LOADPATH=/home/jafourni/.gerbil/lib gerbil build
```

## Critical Rules

- ALWAYS use `let*` when bindings reference each other
- ALWAYS grep for existing defs before adding new ones (avoid duplicate rebind)
- `string-split` in Gerbil only takes char separator, NOT predicate
- `echo-message!` / `echo-error!` / `qt-echo-read-string` for echo area
- After modifying `.ss` files, always build and fix errors before moving on
- Run `gerbil_check_balance` before building large files

---

## Tier 0: Showstoppers (Must Have Before Daily Use)

These are features the user relies on every single day. Without them, gerbil-emacs
cannot replace Doom Emacs.

### 0.1 Key-Chord System (CRITICAL)

The user has ~40+ key-chord bindings (two keys pressed rapidly within a time
window trigger a command). This is the #1 workflow dependency.

**User's chords from config.el:**
```
AL → add-label           AM → assign-me          AS → beginning-of-buffer
AI → assign-issue        BI → branch-issue-repo   BM → mark-whole-buffer
BV → switch-buffer       CI → comment-issue       CM → change-management
CP → clipboard-copy      CV → copy-to-register    CW → window-config-to-register
CX → execute-command     DC → org-table-del-col   DD → helm-dash
DI → dump-issues         EE → eshell              FG → find-file
GB → helm-buffers-list   GG → keyboard-quit       GI → insert-char
GP → goto-progress-dir   GT → goto-issue          IR → insert-register
JK → vterm-copy-done     JW → jump-to-register    KB → kill-buffer
KI → close-issue         LK → vterm-copy-mode     MT → multi-vterm
NM → rename-buffer       RB → revert-buffer       RL → remove-label
RO → reopen-issue        RT → random-theme        SA → sre-accepted
SD → search-devops-docs  SH → search-ansi-logs    SI → show-issue
SO → just-one-space      SU → sre-underway        SW → set-weight
VL → visual-line-mode    ZX → end-of-buffer
```

**Implementation plan:**

1. **`*chord-map*`** — new hash-table keymap mapping 2-char strings to commands
2. **`*chord-timeout*`** — configurable window (default 200ms)
3. **State machine in `qt/app.ss` key handler:**
   - On any printable key press, record `(char . timestamp)`
   - If a second printable key arrives within `*chord-timeout*` ms:
     - Concatenate both chars (uppercase), look up in `*chord-map*`
     - If found: execute command, consume both keys
     - If not found: replay first key as self-insert, process second normally
   - If timeout expires with no second key: replay first key as self-insert
   - Use a Qt single-shot timer (already have timer infrastructure)
4. **`key-chord-define-global`** — bind a 2-char chord to a command symbol
5. **`key-chord-mode`** — toggle chord detection on/off
6. **Chord display** — show active chord prefix in echo area
7. **Persist chord bindings** — load from `~/.gerbil-emacs-chords` or init file

**TUI equivalent:** Same logic in `app.ss` TUI key handler using ncurses timestamps.

**Files to modify:** `core.ss` (chord-map), `qt/app.ss` (key handler + timer),
`qt/commands.ss` or `qt/commands-config.ss` (chord commands)

### 0.2 Key Translation Map (Bracket/Paren Swap)

The user swaps `[` ↔ `(` and `]` ↔ `)` globally for Lisp editing.

**Implementation:**
1. **`*key-translation-map*`** — hash-table mapping char→char
2. Apply translations in key handler before any other processing
3. `(key-translate! from to)` — register a translation
4. Default config: `(key-translate! #\[ #\() (key-translate! #\( #\[)` etc.
5. Per-buffer or global toggle

**Files:** `core.ss`, `qt/app.ss`

### 0.3 Shell Command Integration Framework

The user runs dozens of external shell scripts and displays results in buffers.
Pattern from config.el:

```elisp
(defun show-issue ()
  (let* ((cmd (format "~/bin/show-issue %s" issue))
         (results (shell-command-to-string cmd)))
    (switch-to-buffer buffer)
    (insert results)
    (org-mode)))
```

**Need:**
1. **`shell-command-to-string`** — run command, return stdout as string
2. **`async-shell-command`** with output buffer — run command, stream to buffer
3. **`call-process-shell-command`** — fire-and-forget
4. **Display pattern:** create/reuse buffer, insert output, set mode, make read-only
5. **User-definable commands** — framework for binding shell scripts to commands

**Files:** `qt/commands.ss` (or a new `qt/commands-shell-ext.ss`)

### 0.4 Multi-Terminal (vterm equivalent)

User uses `multi-vterm` (chord `MT`) for multiple terminal buffers.

**Need:**
- `cmd-multi-vterm` — create a new uniquely-named terminal buffer
- Terminal buffer naming: `*vterm*`, `*vterm<2>*`, `*vterm<3>*`, etc.
- `cmd-vterm-copy-mode` (chord `LK`) — toggle terminal read-only for text selection
- `cmd-vterm-copy-mode-done` (chord `JK`) — exit copy mode, resume terminal
- Terminal buffers should support ANSI colors

**Check existing:** Terminal support exists. Need to verify multi-buffer and
copy-mode features.

### 0.5 Helm/Fuzzy Completion for Buffers and Commands

User has `helm` as their completion framework. Uses:
- `C-x b` → `helm-buffers-list` (fuzzy buffer switching)
- `CX` chord → `execute-extended-command` (M-x with fuzzy matching)
- Fuzzy matching everywhere in minibuffer

**Need:**
1. **Fuzzy matching engine** — score candidates by substring/subsequence match
2. **Interactive completion popup** — show candidates as you type, update live
3. **Buffer switcher** — fuzzy filter buffer list, show in minibuffer or popup
4. **Command executor** — fuzzy filter command list for M-x
5. **File finder** — fuzzy filter files for C-x C-f

**Implementation options:**
- Qt: Use QListWidget popup below echo area
- TUI: Use ncurses popup/overlay

---

## Tier 1: Core Editor Polish (First Week)

Features that make editing comfortable.

### 1.1 Delete Trailing Whitespace on Save

User has `(add-hook 'before-save-hook 'delete-trailing-whitespace)`.

**Need:** Hook into save that strips trailing whitespace from all lines.
Also: `(setf require-final-newline t)` — ensure file ends with newline.

### 1.2 F11/F12 for Comment/Uncomment

```elisp
(global-set-key [(f11)] 'uncomment-region)
(global-set-key [(f12)] 'comment-region)
```

**Need:** Bind F11 → uncomment-region, F12 → comment-region in global keymap.
Verify these commands exist and work correctly.

### 1.3 Registers (file, text, window config)

User uses registers actively via chords:
- `CV` → copy-to-register
- `IR` → insert-register
- `JW` → jump-to-register
- `CW` → window-configuration-to-register
- `(set-register ?i (cons 'file "~/doomacs/config.el"))` — file bookmark in register

**Need:** Verify register system supports:
- Text registers (copy/paste named clips)
- Point registers (save/restore cursor position)
- File registers (jump to a file)
- Window configuration registers (save/restore window layout)

### 1.4 Line Numbers

User has `(setq display-line-numbers-type t)` — absolute line numbers.

**Need:** Line numbers displayed in gutter. Both Qt (already has?) and TUI.

### 1.5 Visible Window Dividers

User configures `window-divider-mode` with blue (#51afef) dividers.

**Need:** Visual separator between split windows in Qt mode.

### 1.6 Revert Buffer

User has chord `RB` → `my-revert-buffer` (reload file from disk without confirm).

**Need:** `cmd-revert-buffer` that reloads file content from disk.

---

## Tier 2: Workflow Features (First Two Weeks)

### 2.1 Magit-Level Git Interface

User has `magit` enabled. This is the #1 reason many stay on Emacs.

**Minimum viable magit:**
1. **Status buffer** (`magit-status`) — show staged/unstaged/untracked files
   - Single-key actions: `s` (stage), `u` (unstage), `k` (discard)
   - Section folding for diff hunks
2. **Commit** — `c c` to compose commit message, `C-c C-c` to finalize
3. **Log** — `l l` for commit log with one-line format
4. **Diff** — inline diff display with +/- coloring
5. **Push/Pull** — `P p` to push, `F p` to pull
6. **Branch** — `b b` to switch branch, `b c` to create branch
7. **Stash** — `z z` to stash, `z p` to pop

**Implementation:** Dedicated `*magit*` buffer with its own keymap. Run git
commands via `open-process`, parse output, render with formatting.

### 2.2 Org-Mode Improvements

User uses org-mode extensively for issue display. Needs:
1. **org-cycle** (`TAB`) — fold/unfold headings (partially exists)
2. **org-table operations** — `DC` chord for `org-table-delete-column`
3. **visual-line-mode** — soft word wrap (chord `VL`)
4. **ANSI color rendering** — `ansi-color-apply-on-region` for command output
5. **org-babel** — not critical but config references it

### 2.3 Workspaces / Perspectives

User has `workspaces` module enabled — tab-like workspace separation.

**Need:**
1. Named workspaces (workspace-1, workspace-2, etc.)
2. Each workspace has its own buffer list and window layout
3. Switch between workspaces (keybinding)
4. Persist workspaces across sessions

### 2.4 Code Folding

User has `fold` module enabled.

**Need:**
1. Fold/unfold at point (toggle)
2. Fold all / unfold all
3. Fold level N (fold to depth N)
4. Works with: braces `{}`, indentation, org headings, Scheme forms

### 2.5 hl-todo Highlighting

User has `hl-todo` — highlight TODO/FIXME/NOTE/HACK/REVIEW in comments.

**Need:** Syntax highlighting rule that colors these keywords in comments.

### 2.6 vc-gutter (diff indicators)

User has `(vc-gutter +pretty)` — show git change indicators in the gutter.

**Need:**
- Green bar for added lines
- Red bar for deleted lines
- Yellow bar for modified lines
- Recalculate on save or after git operations

---

## Tier 3: External Integrations (Weeks 2-4)

### 3.1 User-Definable Gerbil Init File

User needs equivalent of `config.el` — a `~/.gerbil-emacs-init.ss` that runs
at startup. This enables:
- Custom chord bindings
- Custom functions
- Key translations
- Variable settings

**Implementation:**
1. At startup, check for `~/.gerbil-emacs-init.ss`
2. Load and evaluate it in the editor's environment
3. Provide API: `key-chord-define-global`, `global-set-key`, `defcommand`, etc.

### 3.2 Shell Script Runner Framework

Many of the user's Emacs functions follow this pattern:
```
1. Read input from minibuffer (issue number, search pattern, etc.)
2. Run a shell command with that input
3. Display output in a named buffer
4. Set buffer to org-mode and read-only
```

**Need a macro/function:**
```scheme
(define-shell-command name prompt command-template
  buffer-name-template mode)
```

Example:
```scheme
(define-shell-command 'show-issue "Issue: " "~/bin/show-issue ~a"
  "gl-issue-~a" 'org)
```

### 3.3 Mattermost Integration (Low Priority)

User has custom functions to send messages to team members via terminal.
Could be reimplemented as shell commands once the framework exists.

### 3.4 SuperGenPass (Low Priority)

Password generation tool. Can be implemented in pure Gerbil using
`:std/crypto` for SHA-512 and base64.

---

## Tier 4: Advanced Features (Month 2+)

### 4.1 LSP Client (eglot equivalent)

User has eglot configured for Gerbil. This requires:
1. JSON-RPC over stdio communication with language server
2. textDocument/didOpen, didChange, didSave notifications
3. textDocument/completion for auto-complete
4. textDocument/definition for go-to-definition
5. textDocument/hover for eldoc-like info

**Big project.** Consider starting with just gerbil-lsp support since user
already has the server at `/usr/local/bin/gerbil-lsp`.

### 4.2 Snippets / File Templates

User has `snippets` and `file-templates` modules.

**Need:**
1. Snippet expansion (type abbreviation + TAB to expand)
2. Snippet fields (tab stops for filling in placeholders)
3. File templates (auto-insert template when creating new file of certain type)

### 4.3 AI Integration (gptel/copilot)

User has `gptel` (OpenAI) and `copilot` packages.

**Lower priority** — can use external tools. But inline completion would
be a differentiator.

### 4.4 Spell Checking

User has `(spell +flyspell)` and `grammar` modules.

**Need:** Integration with aspell/hunspell for on-the-fly spell checking.

### 4.5 Syntax Checking (flycheck/flymake)

User has `syntax` checker module.

**Need:** Run linters asynchronously, display errors in gutter/echo area.

---

## Implementation Order

```
Phase 1 (Week 1): Key-chord system + key translation + basic integrations
  0.1  Key-chord system (core infrastructure)
  0.2  Key translation map (bracket/paren swap)
  1.1  Delete trailing whitespace on save
  1.2  F11/F12 comment bindings
  1.3  Verify register system completeness
  1.6  Revert buffer command

Phase 2 (Week 2): Completion + terminal + shell
  0.5  Fuzzy completion (helm-like buffer/command switching)
  0.3  Shell command integration framework
  0.4  Multi-terminal support
  1.4  Line numbers in gutter (verify)
  1.5  Window dividers (verify)

Phase 3 (Weeks 3-4): Git + org + workspaces
  2.1  Magit-level git interface
  2.2  Org-mode improvements
  2.4  Code folding
  2.5  hl-todo highlighting
  2.6  vc-gutter diff indicators

Phase 4 (Weeks 4-6): Init system + advanced
  3.1  User init file (~/.gerbil-emacs-init.ss)
  3.2  Shell script runner framework
  2.3  Workspaces / perspectives

Phase 5 (Month 2+): LSP + snippets + integrations
  4.1  LSP client
  4.2  Snippets / file templates
  4.4  Spell checking
  4.5  Syntax checking
```

---

## Architecture Notes

### Qt Backend Modules
| Module            | Purpose                                      |
|-------------------|----------------------------------------------|
| `qt/commands.ss`  | Facade: selective exports + cross-cutting fns |
| `qt/commands-*.ss`| Command implementations (chain of 10 modules)|
| `qt/app.ss`       | Event loop, key dispatch, timers             |
| `qt/highlight.ss` | Syntax highlighting                          |
| `qt/window.ss`    | Window/split management                      |
| `qt/keymap.ss`    | Key event → Emacs key string conversion      |
| `qt/menubar.ss`   | Menu bar and toolbar                         |
| `qt/buffer.ss`    | Buffer create/kill/attach                    |
| `qt/modeline.ss`  | Status bar                                   |
| `qt/echo.ss`      | Echo area / minibuffer                       |

### Command Module Chain Order
core → edit → search → file → sexp → ide → vcs → shell → modes → config → facade

### Key Infrastructure (for chord implementation)
- Timer system: `qt-timer-create!`, `qt-timer-start!`, `qt-timer-stop!`
- Which-key already uses 500ms timer for prefix key display
- Key handler: `qt-on-key-press-consuming!` in `qt/app.ss`
- Key state machine: `key-state` struct with `keymap` + `prefix-keys`

### Buffer Display Pattern
```scheme
(let* ((buf (or (buffer-by-name "*Name*") (qt-buffer-create! "*Name*" ed #f))))
  (qt-buffer-attach! ed buf)
  (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
  (qt-plain-text-edit-set-text! ed content)
  (qt-plain-text-edit-set-read-only! ed #t)
  (qt-modeline-update! app))
```
