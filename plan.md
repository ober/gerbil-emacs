# Gemacs Development Plan

## Completed: Autocomplete Popups & Diagnostic Tooltips (`cd07ae3`)

Implemented popup UX matching Ruby Scintilla editors — inline autocomplete dropdown at cursor and floating diagnostic tooltips on hover.

- **Qt idle autocomplete** — Rewrote `lsp-auto-complete!` to use Scintilla native `SCI_AUTOCSHOW` (replaced broken `QCompleter` approach). Added `sci-word-prefix` using `SCI_WORDSTARTPOSITION`. Popup appears on 500ms idle with 3+ char prefix.
- **Qt diagnostic calltips** — Added `lsp-diagnostic-calltip!` using `SCI_CALLTIPSHOW` with severity-colored backgrounds (red/yellow/blue/grey). Auto-shows/hides on idle cursor movement over diagnostic lines.
- **TUI calltip helpers** — Added `show-calltip!`/`cancel-calltip!` in `editor-text.ss`, ready for future TUI LSP integration.
- **gerbil-scintilla** (`ce04f66`) — Added `SCI_WORDSTARTPOSITION`, `SCI_WORDENDPOSITION`, `SCI_ISRANGEWORD`, `SCI_SETMOUSEDWELLTIME`, `SCI_GETMOUSEDWELLTIME` constants.

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

## Backlog (Prioritized)

### High Impact
- **Persistent M-x command history** — track frequently used commands, sort to top, persist across sessions
- **Proper narrow-to-defun** — detect function boundaries for Scheme/Lisp and common languages

### Medium Impact
- **Org-mode archive/refile** — complete org workflow
- **Better which-key delay display** — show descriptions alongside key names
- **Occur mode improvements** — read-only, better navigation
- **Dired improvements** — more operations, better display

### Maintenance
- **Split over-limit files** — `editor-extra-media.ss`, `editor-extra-editing.ss`, `qt/commands-edit.ss`, `qt/commands-vcs.ss`
- **Test coverage** — add functional tests for untested command groups

### Aspirational
- LSP in TUI
- DAP/debugger integration
- Multi-frame support
- Spell checking integration
