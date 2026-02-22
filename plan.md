# Gemacs Feature Parity Plan

## Status: Batches 1–15 Complete + Enhanced Help System In Progress

All alias/parity batches through Batch 15 are committed and pushed to master.
Org-mode test integration (17 test files, 141 checks) committed and pushed.

---

## Completed Work

### Alias Batches (all committed & pushed)
- **Batch 1–11**: Core Emacs command aliases (undo, redo, kill, yank, mark, search, replace, window, buffer, file, dired, org, magit, paredit, help, project, bookmarks, rectangles, registers, macros, completion, flycheck, treesit, mc, helm, etc.)
- **Batch 12** (`5be60a0`): undo-redo, outline, flymake, dired, text-scale, tab-bar, mode toggles
- **Batch 13** (`e83078e`): mode-name aliases (transient-mark, highlight-changes, delete-trailing-whitespace, menu-bar, tool-bar), set-visited-file-name, sort stubs, apropos-variable
- **Batch 14** (`2b90666`): visual-line commands, kill-emacs, forward-list/backward-list, goto-address-mode
- **Batch 15** (`0bab81a`): insert-tab, keep-matching-lines, calc-dispatch

### Org-mode Tests (committed & pushed)
- **`d1c29ba`**: Integrate 17 org-mode test files from gemacs-org-tests
- **`4e5dd5d`**: Fix all 141 org-mode test failures across 17 test files

### Qt Module Split (committed)
- **`11fcaf9`**: Split `qt/commands.ss` — extracted registrations + utilities into `qt/commands-aliases.ss`

### Earlier Work (committed)
- Theme switching fix (`qt-apply-editor-theme!` reads from face system)
- 50/50 split sizing fix
- LSP visual integration (margin setup on start, cleanup on stop)
- Multiple cursors (Group 20 tests)
- Qt functional test Groups 1–21

---

## In Progress: Enhanced Help/Describe System

### What was done (building, not yet committed)
- Added `*command-docs*` hash table to `core.ss` for command docstring registry
- Added `register-command-doc!`, `command-doc`, `command-name->description`, `find-keybinding-for-command` to core
- Added `setup-command-docs!` — registers 100+ docstrings for the most commonly used commands
- Enhanced TUI `describe-command` (`editor-ui.ss`) — now shows help in *Help* buffer with name, keybinding, and description (was: just "is a command")
- Enhanced TUI `describe-key` (`editor-ui.ss`) — now shows full help in *Help* buffer (was: just "runs X")
- Enhanced TUI `describe-function` (`editor-cmds-c.ss`) — shows help in *Help* buffer with completion (was: just "command registered")
- Enhanced Qt `describe-command` (`qt/commands-file.ss`) — shows help in *Help* buffer (was: just "is on [key]")
- Enhanced Qt `describe-function` (`qt/commands-vcs.ss`) — shows help in *Help* buffer with completion (was: just "is a registered command")
- Auto-generated fallback: `command-name->description` converts symbol names to human-readable strings (e.g. `find-file` → "Find file")
- Called from both `app.ss` and `qt/app.ss` initialization

### Files modified
- `core.ss` — new exports + command doc registry + `setup-command-docs!`
- `editor-ui.ss` — enhanced `cmd-describe-key`, `cmd-describe-command`, added `format-command-help`
- `editor-cmds-c.ss` — enhanced `cmd-describe-function`, `cmd-describe-variable`
- `qt/commands-file.ss` — enhanced `cmd-describe-command`, added `qt-format-command-help`
- `qt/commands-vcs.ss` — enhanced `cmd-describe-function`, `cmd-describe-variable`
- `app.ss` — added `(setup-command-docs!)` call
- `qt/app.ss` — added `(setup-command-docs!)` call

---

## Architecture Notes

### TUI Registration Scopes (3-tier)
1. **`editor.ss`** (facade) — has access to all modules; for cross-chain aliases
2. **`editor-extra.ss`** — editor-extra chain scope
3. **`editor-extra-regs.ss`** — editor-cmds chain scope

### Qt Registration Scopes (2-tier)
1. **`qt/commands-aliases.ss`** — chain scope (after commands-parity, before facade)
2. **`qt/commands.ss`** (facade) — for forward-ref functions like `cmd-quit`

### File Size Status
| File | Lines | Status |
|------|-------|--------|
| `editor-extra-editing.ss` | ~2075 | Over limit (pre-existing) |
| `qt/commands-edit.ss` | ~2063 | Over limit |
| `editor-extra-media.ss` | ~2013 | Over limit (pre-existing) |
| `qt/commands-vcs.ss` | ~2010 | Over limit |
| `qt/commands-file.ss` | ~2008 | At limit |
| `qt/commands-sexp.ss` | ~2000 | At limit |
| `editor-advanced.ss` | ~2000 | At limit |
| `editor-extra.ss` | ~1982 | Near limit |

---

## Codebase Audit Results (Feb 2026)

### Feature Coverage Summary
The codebase is **extremely comprehensive** with 1,001+ registered commands covering:
- File/buffer/window operations, navigation, editing, search/replace
- Org-mode (headings, TODO, scheduling, agenda, export, babel, tables)
- VCS (magit status/log/commit/diff/blame/stash/push/pull)
- Shell/REPL (eshell, shell, gxi, eval-expression)
- Modern packages (fuzzy M-x, which-key, auto-revert, recentf, session save/restore)
- Electric pair, paredit, multiple cursors, rectangles, registers, macros
- LSP (Qt only), compilation, grep, occur, wgrep
- Themes, rainbow delimiters, hl-todo, indent guides
- Ediff, bookmarks, narrowing, sort, align, fill-paragraph

### True Stubs (very few)
- `debug-on-entry` / `cancel-debug-on-entry` — echo-only stubs
- `org-archive-subtree` — "not yet implemented"
- Some modal editing flags (evil-mode/meow-mode) — toggles only, no vi keybindings

### Architectural Limitations
- Single-frame only (no `make-frame` / `other-frame`)
- LSP only in Qt layer (TUI shows "use gemacs-qt")
- DAP/debugger commands are stubs

---

## Next Steps (Prioritized)

### High Impact — Feature Quality
1. **Persistent M-x command history** — track frequently used commands, sort to top, persist across sessions
2. **Proper narrow-to-defun** — detect function boundaries for Scheme/Lisp and common languages
3. **Line number display mode** — Scintilla margin line numbers (display-line-numbers-mode)
4. **Better describe system** — register docstrings from cmd-* function definitions (auto-extract)

### Medium Impact — Polish
5. **Org-mode archive/refile** — complete org workflow
6. **Better which-key delay display** — show descriptions alongside key names
7. **Occur mode improvements** — read-only, better navigation
8. **Dired improvements** — more operations, better display

### Maintenance
9. **Split over-limit files** — `editor-extra-media.ss`, `editor-extra-editing.ss`, `qt/commands-edit.ss`, `qt/commands-vcs.ss`
10. **Test coverage** — add functional tests for untested command groups

### Aspirational
- Evil-mode (real vi keybindings)
- LSP in TUI
- DAP/debugger integration
- Multi-frame support
- Spell checking integration
