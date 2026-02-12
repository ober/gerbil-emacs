# gerbil-emacs Feature Plan

## Current State
- Emacs-like editor with TUI (Scintilla+termbox) and Qt backends
- Basic editing, navigation, window splitting, file/buffer management
- Dired directory browser
- REPL buffer (gxi subprocess) and M-: eval-expression
- Syntax highlighting for 20+ languages (Gerbil, Python, Rust, Go, etc.)
- Image viewing (Qt backend, with zoom controls)
- Auto-save and backup file support
- Shell (external $SHELL) and Eshell (built-in Gerbil shell) modes
- 67 test cases, 360 checks passing

---

## ⚠️ Implementation Audit (2026-02-12)

### Summary
A detailed code review revealed that while core features marked as "completed" are genuinely
implemented, the codebase contains **~756 stub functions** (down from ~850 after implementation work).
These stubs provide command registration and keybindings but NO actual functionality.

**Implementation Progress (2026-02-12):**
- Started with ~850 stubs
- Implemented 92+ stub functions with real functionality
- Current stub count: ~756

### Recently Implemented Features

| Feature | Commands Implemented |
|---------|---------------------|
| Spell checking | `ispell-word`, `ispell-buffer`, `ispell-region` |
| Winner mode | `winner-undo`, `winner-redo`, `winner-mode` |
| Tab management | `tab-new`, `tab-close`, `tab-next`, `tab-previous`, `tab-rename`, `tab-move` |
| Flycheck | `flycheck-mode`, `flycheck-next-error`, `flycheck-previous-error`, `flycheck-list-errors` |
| Git gutter | `git-gutter-mode`, `git-gutter-next-hunk`, `git-gutter-previous-hunk`, `git-gutter-revert-hunk`, `git-gutter-stage-hunk` |
| Abbreviations | `abbrev-mode`, `define-abbrev`, `expand-abbrev`, `list-abbrevs` |
| Multiple cursors | `mc-mark-next-like-this`, `mc-mark-previous-like-this`, `mc-mark-all-like-this`, `mc-edit-lines` |
| Xref | `xref-find-definitions`, `xref-find-references`, `xref-find-apropos`, `xref-go-back`, `xref-go-forward` |
| Project.el | `project-switch-project`, `project-find-regexp`, `project-shell`, `project-dired`, `project-eshell` |
| Treemacs | `treemacs`, `treemacs-find-file` |
| Calendar | `calendar-goto-date`, `calendar-holidays` |
| Auto-insert | `auto-insert`, `auto-insert-mode` |

### Stub Count by File
| File | Stub Count | Notes |
|------|------------|-------|
| `editor.ss` | 84 | Various toggles, modes, and minor features |
| `editor-extra.ss` | 672 | Major feature categories (see below) |

### Features That Are STUBS (Not Implemented)

#### Org-mode (12 stubs)
- `org-mode`, `org-todo`, `org-schedule`, `org-deadline`, `org-agenda`
- `org-export`, `org-table-create`, `org-link`, `org-store-link`
- `org-open-at-point`, `org-cycle`, `org-shift-tab`

#### Smartparens / Paredit (6+ stubs)
- `sp-forward-slurp-sexp`, `sp-forward-barf-sexp`
- `sp-backward-slurp-sexp`, `sp-backward-barf-sexp`
- `paredit-raise-sexp`, `paredit-wrap-round`

#### ~~Multiple Cursors (4 stubs)~~ ✅ IMPLEMENTED
#### ~~Xref / Navigation (5 stubs)~~ ✅ IMPLEMENTED
#### ~~Flycheck (4 stubs)~~ ✅ IMPLEMENTED

#### LSP Support (6 stubs)
- `lsp-find-declaration`, `lsp-find-implementation`
- `lsp-rename`, `lsp-format-buffer`
- `lsp-execute-code-action`, `lsp-describe-thing-at-point`

#### DAP (Debug Adapter Protocol) (6 stubs)
- `dap-debug`, `dap-breakpoint-toggle`, `dap-continue`
- `dap-next`, `dap-step-in`, `dap-step-out`

#### ~~Spell Checking (8+ stubs)~~ ✅ PARTIALLY IMPLEMENTED
- `ispell-word`, `ispell-buffer`, `ispell-region` - ✅ IMPLEMENTED
- `flyspell-mode`, `flyspell-buffer`, `flyspell-correct-word` - stub
- `flyspell-auto-correct-word`, `flyspell-goto-next-error` - stub

#### Package Management (4 stubs)
- `package-list-packages`, `package-install`
- `package-delete`, `package-refresh-contents`

#### ~~Tabs (6 stubs)~~ ✅ IMPLEMENTED
#### ~~Winner Mode (2 stubs)~~ ✅ IMPLEMENTED
#### ~~Git gutter (5 stubs)~~ ✅ IMPLEMENTED

#### EWW Browser (8 stubs)
- `eww`, `eww-browse-url`, `browse-url-at-point`
- `eww-back-url`, `eww-forward-url`, `eww-reload`
- `eww-download`, `eww-copy-page-url`

#### Mail / News (3 stubs)
- `compose-mail`, `rmail`, `gnus`

#### Customize / Themes (6+ stubs)
- `customize-group`, `customize-variable`, `customize-themes`
- `load-theme`, `disable-theme`, `describe-theme`

#### ~~Project.el (5 stubs)~~ ✅ IMPLEMENTED

#### ~~Treemacs / Speedbar (3 stubs)~~ ✅ PARTIALLY IMPLEMENTED
- `treemacs`, `treemacs-find-file` - ✅ IMPLEMENTED
- `speedbar` - stub
- `project-shell`, `project-dired`, `project-eshell`

#### Treemacs / Speedbar (3 stubs)
- `treemacs`, `treemacs-find-file`, `speedbar`

#### Diff / Ediff (6 stubs)
- `diff-mode`, `diff-apply-hunk`, `diff-revert-hunk`, `diff-goto-source`
- `ediff-files`, `ediff-regions-linewise`, `ediff-merge-files`

#### Modern Completion (Vertico/Consult/etc) (12+ stubs)
- `vertico-mode`, `consult-line`, `consult-grep`, `consult-buffer`
- `corfu-mode`, `orderless-mode`, `marginalia-mode`
- `embark-act`, `embark-dwim`, `cape-dabbrev`, `cape-file`

#### Programming Modes (15+ stubs)
- `python-mode`, `c-mode`, `c++-mode`, `java-mode`
- `rust-mode`, `go-mode`, `js-mode`, `typescript-mode`
- `html-mode`, `css-mode`, `lua-mode`, `ruby-mode`, `sh-mode`
- `scheme-mode`, `gerbil-mode`, `emacs-lisp-mode`

#### Debugger (GUD/GDB) (6 stubs)
- `gdb`, `gud-break`, `gud-remove`, `gud-cont`, `gud-next`, `gud-step`

#### Snippets (3 stubs)
- `yas-insert-snippet`, `yas-new-snippet`, `yas-visit-snippet-file`

#### Git Gutter (5 stubs)
- `git-gutter-mode`, `git-gutter:next-hunk`, `git-gutter:previous-hunk`
- `git-gutter:revert-hunk`, `git-gutter:stage-hunk`

#### AI/Copilot (4 stubs)
- `copilot-mode`, `copilot-accept-completion`, `copilot-next-completion`
- `gptel`, `gptel-send`

#### And Many More...
- Abbreviations (abbrev-mode, define-abbrev, etc.)
- Auto-insert templates
- Artist mode (ASCII drawing)
- Calendar/diary beyond basic display
- Docker integration
- Evil/Meow modal editing
- Folding (origami, hs-minor-mode)
- Games (tetris, snake, dunnet, etc.)
- EMMS (media player)
- PDF viewing
- Printing
- SLIME/SLY
- TRAMP (remote editing)
- Undo-tree visualization
- Wgrep (editable grep)
- And ~600 more toggle/mode commands

### Features That ARE Fully Implemented

| Feature | Implementation | Files |
|---------|----------------|-------|
| Syntax highlighting | Full Scintilla lexer + Qt highlighter | `highlight.ss`, `qt/highlight.ss` |
| Image viewing | Full Qt dialog with zoom | `qt/image.ss` |
| Shell mode | Full subprocess with ANSI stripping | `shell.ss`, `qt/commands.ss` |
| Eshell | 18+ builtins, pipelines, Gerbil eval | `eshell.ss` |
| M-x execute-command | Full with completion | `editor.ss` |
| Goto line | Full implementation | `editor.ss` |
| Query replace | Full y/n/!/q support | `editor.ss` |
| Rectangle ops | kill/yank/open/string-rectangle | `editor.ss` |
| Universal argument | C-u, C-u C-u, digit args | `editor.ss` |
| Auto-save | 30s timer, #file# naming | `editor.ss`, `qt/app.ss` |
| Help system | describe-key/command/bindings | `editor.ss` |
| Buffer list | Full buffer list display | `editor.ss` |
| Completion (Qt) | QCompleter integration | `qt/commands.ss` |
| Goto matching paren | Full brace matching | `editor.ss` |
| Line numbers | Toggle display | `editor.ss` |
| Dired | Directory browsing, navigation | `editor.ss`, `qt/commands.ss` |
| REPL | Full gxi subprocess | `repl.ss` |
| Window splitting | Horizontal split, other-window | `window.ss`, `qt/window.ss` |

---

## Phase 1: Gerbil Syntax Highlighting ✅ COMPLETED

### Goal
Minimum viable syntax highlighting for `.ss` / `.scm` files using colors from `gerbil-mode.el`.

### Color Scheme (from gerbil-mode.el)
| Category | Emacs Face | Color (dark theme) |
|----------|-----------|-------------------|
| Keywords | font-lock-keyword-face | #cc99cc (purple) |
| Builtins | font-lock-builtin-face | #66cccc (cyan) |
| Functions | font-lock-function-name-face | #6699cc (blue) |
| Variables | font-lock-variable-name-face | #f99157 (orange) |
| Types | font-lock-type-face | #ffcc66 (yellow) |
| Constants | font-lock-constant-face | #99cc99 (green) |
| Strings | font-lock-string-face | #99cc99 (green) |
| Comments | font-lock-comment-face | #999999 (gray) |
| Errors | font-lock-warning-face | #f2777a (red) |

### Keyword Lists (from gerbil-mode.el)

**Keywords:** `def`, `defvalues`, `defalias`, `defsyntax`, `defrule`, `defrules`,
`defstruct`, `defclass`, `defmethod`, `defgeneric`, `deftype`, `defmessage`,
`import`, `export`, `declare`, `include`, `module`, `extern`, `cond-expand`,
`if`, `when`, `unless`, `cond`, `case`, `match`, `match*`, `with`, `with*`,
`begin`, `begin0`, `let`, `let*`, `letrec`, `letrec*`, `lambda`, `set!`,
`try`, `catch`, `finally`, `raise`, `error`, `and`, `or`, `not`,
`for`, `for*`, `for/collect`, `for/fold`, `while`, `until`,
`spawn`, `spawn*`, `spawn/name`, `sync`, `wait`,
`test-suite`, `test-case`, `check`

**Builtins:** `=>`, `...`, `<>`, `<...>`, `#t`, `#f`, `#!void`, `#!eof`,
`values`, `apply`, `eval`, `call/cc`, `call/values`, `cut`,
`syntax`, `quasisyntax`, `quote-syntax`

**Types (after def-forms):** struct/class/interface names

### TUI Backend (Scintilla)
Scintilla has a built-in Lisp lexer (`SCLEX_LISP` / `SCE_LISP_*` styles).
- Set lexer to `SCLEX_LISP` on Gerbil buffers
- Configure keyword lists via `SCI_SETKEYWORDS`
- Map `SCE_LISP_KEYWORD` → purple, `SCE_LISP_STRING` → green, etc.
- Apply style colors via `editor-style-set-foreground/background`

### Qt Backend
`QSyntaxHighlighter` is not yet wrapped in gerbil-qt (Phase 21 planned).
**Approach:** Add minimal `QTextCharFormat` + `QSyntaxHighlighter` support to gerbil-qt:
- C shim: subclass `QSyntaxHighlighter`, call Gerbil callback per line
- Gerbil side: regex-based tokenizer that classifies tokens and applies formats

**Alternative (simpler, Phase 1a):** Timer-based rehighlighting:
- After text changes, re-scan visible lines
- Apply formatting via `QTextCursor` + `QTextCharFormat` (needs wrapping)

### Files
- `highlight.ss` — Shared: token classification regexes, keyword sets
- `editor.ss` — TUI: Scintilla lexer setup for Gerbil buffers
- `qt/highlight.ss` — Qt: syntax highlighter using QTextCharFormat
- Need to add lexer/style APIs to gerbil-qt

---

## Phase 2: Image Viewing (Qt only) ✅ COMPLETED

### Goal
Open image files (PNG, JPG, GIF, BMP) in a read-only buffer with the image displayed.

### Implementation
- `qt/image.ss` — Image detection and viewing dialog
- `qt/app.ss` — Integrates image detection into `qt-open-file!`
- Supports: PNG, JPG, JPEG, GIF, BMP, WEBP, SVG, ICO, TIFF
- Zoom controls: +/- buttons and keyboard, Fit (0), 100% (1)
- Scrollable view for large images

---

## Phase 3: Terminal Mode ✅ COMPLETED

### Goal
Run `$SHELL` (bash/zsh) in a buffer, supporting ANSI escape codes, like `M-x shell` in Emacs.

### Qt Backend (Primary — using QProcess)
gerbil-qt has full QProcess support (Phase 15):
- `qt-process-create`, `qt-process-start!`
- `qt-process-write!`, `qt-process-read-stdout`
- `qt-process-on-ready-read!` — streaming callback
- `qt-process-on-finished!` — completion callback

**Architecture:**
1. Create a `*shell*` buffer with `buffer-lexer-lang` = `'shell`
2. Spawn `$SHELL` via `qt-process-start!`
3. On `qt-process-on-ready-read!`: append stdout to buffer
4. On Enter: extract input after prompt, write to process stdin
5. On C-c: send interrupt signal
6. Handle basic ANSI escape codes (colors, cursor movement) by parsing and applying `QTextCharFormat`

**ANSI Color Mapping:**
| ANSI Code | Color |
|-----------|-------|
| 30/40 | Black |
| 31/41 | Red |
| 32/42 | Green |
| 33/43 | Yellow |
| 34/44 | Blue |
| 35/45 | Magenta |
| 36/46 | Cyan |
| 37/47 | White |

### TUI Backend
More complex — Scintilla doesn't natively handle terminal escape codes.
**Approach:** Same subprocess pattern as REPL but with ANSI stripping:
- Spawn `$SHELL` via `open-process` with pseudo-terminal
- Strip ANSI codes before inserting into Scintilla editor
- Basic color support via Scintilla styles if feasible

### Files
- `shell.ss` — Shared: ANSI parser, shell state struct
- `qt/commands.ss` — `cmd-shell` command
- `qt/app.ss` — Shell output polling (like REPL timer)
- `editor.ss` — TUI shell command

### Keybindings
- `C-x s` or `M-x shell` — Open/switch to shell buffer
- Enter — Send input to shell
- C-c — Send interrupt (SIGINT)
- C-d — Send EOF

---

## Phase 4: Eshell (Gerbil Shell) ✅ COMPLETED

### Goal
Emacs eshell equivalent: a shell implemented in Gerbil with built-in commands, pipeline support, and Gerbil expression evaluation.

### Architecture
```
User input → Parse →
  If starts with ( → eval as Gerbil expression
  If builtin → run Gerbil implementation
  If external → spawn subprocess
```

### Built-in Commands
| Command | Implementation |
|---------|---------------|
| `cd` | `current-directory` / `set! (current-directory) dir` |
| `ls` | `directory-files` with formatting |
| `pwd` | `current-directory` |
| `cat` | `read-file-as-string` |
| `echo` | `display` |
| `mkdir` | `create-directory` |
| `rm` | `delete-file` / `delete-directory` |
| `cp` | `copy-file` |
| `mv` | `rename-file` |
| `find` | Recursive `directory-files` with pattern matching |
| `grep` | Line-by-line file search with regex |
| `head` / `tail` | First/last N lines |
| `wc` | Word/line/char count |
| `which` | Search PATH for executable |
| `env` | Environment variables |
| `export` | `setenv` |
| `clear` | Clear buffer |
| `exit` | Close eshell buffer |

### Pipeline Support
```
ls | grep ".ss" | wc -l
```
- Parse `|` as pipe separator
- Chain commands via Gerbil ports (output of one → input of next)
- Last command's output goes to buffer

### Gerbil Eval Integration
```
gerbil> (+ 1 2)
3
gerbil> (directory-files ".")
("core.ss" "repl.ss" ...)
gerbil> ls -la | (lambda (line) (string-contains line ".ss"))
```

### Files
- `eshell.ss` — Parser, builtins, pipeline, eval integration
- `qt/commands.ss` — `cmd-eshell` command
- `editor.ss` — TUI eshell command

### Keybindings
- `C-x e` or `M-x eshell` — Open/switch to eshell buffer

---

## Phase 5: Enhanced Emacs Features ✅ MOSTLY COMPLETED

### 5a. M-x Command Execution ✅
- Echo area prompt showing all available commands
- Tab completion for command names
- `M-x` bound to command execution

### 5b. Goto Line ✅
- `M-g g` — Prompt for line number, jump to it
- Show current line/column in modeline

### 5c. Replace String ✅
- `M-%` — Interactive search and replace
- Query replace: y/n/!/q for each match

### 5d. Rectangle Operations ✅
- `C-x r k` — Kill rectangle
- `C-x r y` — Yank rectangle
- `C-x r o` — Open (insert space) rectangle

### 5e. Multiple Cursors / Repeat
- ❌ Multiple cursors (STUB - 4 functions)
- ✅ `C-u N command` — Repeat command N times (IMPLEMENTED)
- ✅ Universal argument support: digits and negative prefixes (IMPLEMENTED)

### 5f. Auto-save and Backup ✅
- Timer-based auto-save to `#filename#` (Qt backend, 30s interval)
- Backup files as `filename~` (created on first save)
- Auto-save file removed after successful save

### 5g. Help System ✅
- `C-h k` — Describe key binding
- `C-h f` — Describe command
- `C-h b` — List all bindings

### 5h. Buffer List (ibuffer) ✅
- `C-x C-b` — Show buffer list in a buffer
- Navigate and switch buffers from the list

---

## Phase 6: Advanced Features ✅ MOSTLY COMPLETED

### 6a. Completion Framework ✅
- In-buffer completion with popup (Qt backend)
- File path completion in find-file
- Buffer name completion

### 6b. Parenthesis Matching ✅
- Highlight matching parens for Scheme editing
- Visual brace matching in Qt backend

### 6b. Parenthesis Matching ✅
- Highlight matching parens for Scheme editing
- Jump to matching paren with `C-M-f` / `C-M-b`

### 6c. Line Numbers ✅
- ✅ Toggle line number display (IMPLEMENTED)
- ❌ Relative line numbers mode (STUB)

### 6d. Undo Tree ❌ STUB
- ❌ Visualize undo history (STUB)
- ❌ Branch between undo states (STUB)

---

## Implementation Status

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 1a: TUI Syntax Highlighting | ✅ Completed | Full Scintilla lexer |
| Phase 1b: Qt Syntax Highlighting | ✅ Completed | 20+ languages |
| Phase 2: Image Viewing | ✅ Completed | Qt only, with zoom |
| Phase 3: Terminal Mode | ✅ Completed | Shell subprocess |
| Phase 4: Eshell | ✅ Completed | 18+ builtins |
| Phase 5a: M-x | ✅ Completed | With completion |
| Phase 5b: Goto Line | ✅ Completed | |
| Phase 5c: Replace String | ✅ Completed | Interactive y/n/!/q |
| Phase 5d: Rectangle Ops | ✅ Completed | kill/yank/open/string |
| Phase 5e: Universal Argument | ✅ Completed | C-u N, digit args |
| Phase 5f: Auto-save/Backup | ✅ Completed | 30s timer |
| Phase 5g: Help System | ✅ Completed | describe-key/command |
| Phase 5h: Buffer List | ✅ Completed | |
| Phase 6a: Completion | ✅ Completed | Qt QCompleter |
| Phase 6b: Paren Matching | ✅ Completed | |
| Phase 6c: Line Numbers | ✅ Completed | Toggle only |
| Phase 6d: Undo Tree | ❌ Stub | Display only |

### Stub Categories (NOT Implemented)

| Category | Stub Count | Priority |
|----------|------------|----------|
| Org-mode | 12 | Low (complex) |
| LSP/DAP | 12 | High (useful) |
| Smartparens/Paredit | 6 | Medium |
| Multiple Cursors | 4 | Medium |
| Flycheck | 4 | High |
| Spell checking | 8 | Medium |
| Git gutter | 5 | Medium |
| Mode toggles | ~100 | Low (mostly display) |
| Programming modes | 15 | Low (syntax only) |
| Modern completion | 12 | Medium |
| Other features | ~600 | Varies |
| **TOTAL** | **~850** | |

---

## Testing Strategy

### Headless Qt Testing
```bash
QT_QPA_PLATFORM=offscreen make test
```
gerbil-qt supports offscreen rendering via Qt's platform plugin.
Use this for automated testing of Qt features.

### Test Categories
- **Unit tests** — Pure logic (keyword classification, ANSI parsing, eshell builtins)
- **Integration tests** — Buffer operations, command execution
- **Subprocess tests** — REPL, shell, eshell process lifecycle

### Git Workflow
- Commit + push after each passing test suite
- One commit per feature addition
