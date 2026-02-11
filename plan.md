# gerbil-emacs Feature Plan

## Current State
- Emacs-like editor with TUI (Scintilla+termbox) and Qt backends
- Basic editing, navigation, window splitting, file/buffer management
- Dired directory browser
- REPL buffer (gxi subprocess) and M-: eval-expression
- 18 test cases, 72 checks passing

---

## Phase 1: Gerbil Syntax Highlighting

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

## Phase 2: Image Viewing (Qt only)

### Goal
Open image files (PNG, JPG, GIF, BMP) in a read-only buffer with the image displayed.

### Approach (Qt)
- Detect image files by extension in `qt-open-file!`
- Create a QLabel with `qt-pixmap-load` + `qt-label-set-pixmap!`
- Support zoom with `qt-pixmap-scaled`
- Keybindings: `+`/`-` for zoom, `0` for fit-to-window

### Available APIs
- `qt-pixmap-load` — Load image from file
- `qt-pixmap-scaled` — Scale with aspect ratio
- `qt-label-set-pixmap!` — Display in label
- `qt-pixmap-width`, `qt-pixmap-height` — Get dimensions

### Files
- `qt/commands.ss` — Add `cmd-view-image`, integrate into file opening
- `qt/window.ss` — May need image-specific window type

---

## Phase 3: Terminal Mode

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

## Phase 4: Eshell (Gerbil Shell)

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

## Phase 5: Enhanced Emacs Features

### 5a. M-x Command Execution
- Echo area prompt showing all available commands
- Tab completion for command names
- `M-x` bound to command execution

### 5b. Goto Line
- `M-g g` — Prompt for line number, jump to it
- Show current line/column in modeline

### 5c. Replace String
- `M-%` — Interactive search and replace
- Query replace: y/n/!/q for each match

### 5d. Rectangle Operations
- `C-x r k` — Kill rectangle
- `C-x r y` — Yank rectangle
- `C-x r o` — Open (insert space) rectangle

### 5e. Multiple Cursors / Repeat
- `C-u N command` — Repeat command N times
- Universal argument support

### 5f. Auto-save and Backup
- Timer-based auto-save to `#filename#`
- Backup files as `filename~`

### 5g. Help System
- `C-h k` — Describe key binding
- `C-h f` — Describe command
- `C-h b` — List all bindings

### 5h. Buffer List (ibuffer)
- `C-x C-b` — Show buffer list in a buffer
- Navigate and switch buffers from the list

---

## Phase 6: Advanced Features

### 6a. Completion Framework
- In-buffer completion with popup
- File path completion in find-file
- Buffer name completion

### 6b. Parenthesis Matching
- Highlight matching parens for Scheme editing
- Jump to matching paren with `C-M-f` / `C-M-b`

### 6c. Line Numbers
- Toggle line number display
- Relative line numbers mode

### 6d. Undo Tree
- Visualize undo history
- Branch between undo states

---

## Implementation Order

Priority-ordered, each phase builds on the previous:

1. **Phase 1a: TUI Syntax Highlighting** — Scintilla lexer for Gerbil
2. **Phase 5b: Goto Line** — Quick win, useful for development
3. **Phase 5a: M-x** — Command discovery
4. **Phase 4: Eshell** — Built-in shell (no external deps needed)
5. **Phase 3: Terminal Mode** — External shell support
6. **Phase 1b: Qt Syntax Highlighting** — Requires gerbil-qt additions
7. **Phase 2: Image Viewing** — Qt pixmap display
8. **Phase 5c: Replace String** — Interactive replace
9. **Phase 5h: Buffer List** — ibuffer
10. **Phase 6b: Paren Matching** — Scheme editing essential
11. **Phase 5g: Help System** — Self-documenting
12. Remaining Phase 5 and 6 items

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
