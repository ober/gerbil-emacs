# Gemacs Feature Parity Plan

## Status: Batches 1–15 Complete (Committed & Pushed)

All alias/parity batches through Batch 15 are committed and pushed to master.

---

## Completed Work

### Alias Batches (all committed & pushed)
- **Batch 1–11**: Core Emacs command aliases (undo, redo, kill, yank, mark, search, replace, window, buffer, file, dired, org, magit, paredit, help, project, bookmarks, rectangles, registers, macros, completion, flycheck, treesit, mc, helm, etc.)
- **Batch 12** (`5be60a0`): undo-redo, outline, flymake, dired, text-scale, tab-bar, mode toggles
- **Batch 13** (`e83078e`): mode-name aliases (transient-mark, highlight-changes, delete-trailing-whitespace, menu-bar, tool-bar), set-visited-file-name, sort stubs, apropos-variable
- **Batch 14** (`2b90666`): visual-line commands, kill-emacs, forward-list/backward-list, goto-address-mode
- **Batch 15** (`0bab81a`): insert-tab, keep-matching-lines, calc-dispatch

### Qt Module Split (committed)
- **`11fcaf9`**: Split `qt/commands.ss` — extracted registrations + utilities into `qt/commands-aliases.ss`

### Earlier Work (committed)
- Theme switching fix (`qt-apply-editor-theme!` reads from face system)
- 50/50 split sizing fix
- LSP visual integration (margin setup on start, cleanup on stop)
- Multiple cursors (Group 20 tests)
- Qt functional test Groups 1–21

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
| `editor-extra-media.ss` | ~2013 | Over limit (pre-existing) |
| `editor-extra-editing.ss` | ~2075 | Over limit (pre-existing) |
| `editor-extra.ss` | ~1982 | Near limit |
| `qt/commands-edit.ss` | ~1984 | Near limit |
| `qt/commands-vcs.ss` | ~1899 | Near limit |

---

## Next Steps (Not Yet Started)

### High-Impact Gaps to Investigate
- Fuzzy command/completion UI improvements
- Which-key display improvements
- Dired improvements
- Read-only occur mode
- Async command execution
- More missing standard Emacs commands (to be audited)

### Maintenance
- Split over-limit files (`editor-extra-media.ss`, `editor-extra-editing.ss`)
