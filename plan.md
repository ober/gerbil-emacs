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
| `qt/commands-edit.ss` | ~1155 | OK (split into edit + edit2) |
| `qt/commands-edit2.ss` | ~1325 | OK |
| `editor-extra-editing.ss` | ~1097 | OK (split into editing + editing2) |
| `editor-extra-editing2.ss` | ~1130 | OK |
| `editor-extra-media.ss` | ~2013 | Slightly over limit |
| `qt/commands-vcs.ss` | ~2003 | Slightly over limit |

---

## Backlog (Prioritized)

### High Impact
- ~~**Helm-style C-x C-f find-file**~~ — DONE: popup file selector with default path pre-filled, incremental narrowing, directory descent on Enter
- ~~**Persistent M-x command history**~~ — DONE: frequency-based sorting, persisted to ~/.gemacs-mx-history, auto-load/save
- ~~**Proper narrow-to-defun**~~ — DONE: multi-language defun detection (Scheme/Lisp paren-based, Python/Ruby/C/Go/Rust/etc indentation-based)

### Medium Impact
- ~~**Org-mode archive/refile**~~ — DONE: archive-subtree writes to `<file>_archive` with ARCHIVE_TIME/ARCHIVE_FILE properties, refile already implemented
- ~~**Better which-key delay display**~~ — DONE: shows human-readable descriptions (e.g. "s → Save buffer") instead of raw symbol names
- ~~**Occur mode improvements**~~ — DONE: read-only *Occur* buffer, occur-next/occur-prev navigation commands
- ~~**Dired improvements**~~ — DONE: human-readable file sizes (K/M/G), read-only dired buffer, rename/copy at point (Qt), shared dired-format-listing

### Maintenance
- ~~**Split over-limit files**~~ — DONE: split `qt/commands-edit.ss` (2457→1155+1325) and `editor-extra-editing.ss` (2204→1097+1130). Remaining `editor-extra-media.ss` (2013) and `qt/commands-vcs.ss` (2003) are barely over limit
- ~~**Test coverage**~~ — DONE: added Group 13 tests (dired, which-key, zoom, compile/search, bookmarks, M-x history). 181 total TUI functional tests, 307 Qt tests

### Recently Completed
- ~~**Vterm terminal fixes**~~ — DONE: PTY window size from editor dimensions, PS1 prompt cleanup, batched rendering for speed
- ~~**Whitespace-mode & display-line-numbers-mode**~~ — DONE: Emacs-style SCI_SETVIEWWS/SCI_SETVIEWEOL toggle, line number gutter toggle (both TUI and Qt)
- ~~**Magit commit composition buffer**~~ — DONE: `*Magit: Commit*` buffer with diff preview, C-c C-c / C-c C-k, comment stripping
- ~~**Interactive magit log**~~ — DONE: date/author/subject format, Enter shows commit diff with highlighting, mode keymaps
- ~~**Magit amend**~~ — DONE: `a` in magit opens commit buffer pre-filled with previous message, `--amend` flag

### Aspirational
- LSP in TUI
- DAP/debugger integration
- Multi-frame support
- ~~Spell checking integration~~ — DONE: flyspell-mode, ispell-word, ispell-buffer, ispell-region, dictionary switching (both TUI and Qt)
