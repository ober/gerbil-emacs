# Gemacs Development Plan

## Current: Autocomplete Popups & Diagnostic Tooltips

Goal: Match the popup UX seen in Ruby Scintilla editors — inline autocomplete dropdown at cursor and floating diagnostic tooltips on hover.

### Infrastructure Already in Place

- **`sci-send` / `sci-send/string`** — generic Scintilla message API works in both TUI and Qt
- **All `SCI_AUTOC*` and `SCI_CALLTIP*` constants** accessible from `:gerbil-scintilla/constants`
- **TUI autocomplete** — `cmd-complete-at-point` (`editor-text.ss:1916-1988`) already uses `SCI_AUTOCSHOW` with word-based candidates
- **Qt `QCompleter` FFI** — `qt-completer-create`, `qt-completer-complete-rect!`, etc. all wired up
- **LSP completion requests** — `textDocument/completion` working in `qt/commands-lsp.ss`
- **LSP diagnostics** — squiggly underlines + margin markers already rendered via Scintilla indicators
- **Idle timer** — 500ms timer fires in `qt/app.ss:756`, calls `lsp-auto-complete!`

### Task 1: Qt Idle Autocomplete Popup

`lsp-auto-complete!` in `qt/commands-lsp.ss:1668-1702` is wired up but calls two **undefined helpers**, so it silently fails.

**Option A — Use Scintilla native popup (recommended)**
- Simpler, already proven in TUI, no Qt widget management
- Feed LSP completion items as newline-separated string to `SCI_AUTOCSHOW`
- Steps:
  1. Implement `get-word-prefix` — use `SCI_WORDSTARTPOSITION` + `SCI_GETRANGEPOINTER` to extract prefix at cursor
  2. Rewrite `lsp-auto-complete!` to call `SCI_AUTOCSHOW` with LSP results instead of `QCompleter`
  3. Configure popup: `SCI_AUTOCSETSEPARATOR` (newline), `SCI_AUTOCSETIGNORECASE`, `SCI_AUTOCSETMAXHEIGHT`, `SCI_AUTOCSETORDER`
  4. Handle `SCI_AUTOCCANCEL` on cursor move / escape

**Option B — Finish QCompleter approach**
- Implement `get-word-prefix` (same as above)
- Implement `get-or-create-completer!` — cache a `QCompleter` per editor widget
- Call `qt-completer-complete-rect!` with coordinates from `SCI_POINTXFROMPOSITION` / `SCI_POINTYFROMPOSITION`

**Files to modify:**
- `qt/commands-lsp.ss` — fix `lsp-auto-complete!`, add helper functions
- Possibly `qt/sci-shim.ss` — if new SCI constants are needed

### Task 2: Diagnostic Hover Tooltips

Currently hover info shows in the **echo area** at the bottom. Goal: floating tooltip near the diagnostic squiggle.

**Option A — Scintilla calltips (recommended)**
- Use `SCI_CALLTIPSHOW(pos, text)` to display tooltip at a position
- Set background color with `SCI_CALLTIPSETBACK` (yellow like the Ruby screenshot)
- Hook into `SCN_DWELLSTART` notification (fires when mouse hovers over a position)
- Look up diagnostics at the hovered position and show the message
- Dismiss on `SCN_DWELLEND` with `SCI_CALLTIPCANCEL`
- ~20 lines of code, no new C FFI needed

**Option B — Qt tooltip widget (richer)**
- Create `QToolTip` or custom `QLabel` popup via new C shim functions
- More control over styling, positioning, multi-line layout
- Requires new FFI bindings in qt_shim

**Steps (Option A):**
1. Enable dwell events: `SCI_SETMOUSEDWELLTIME(ms)` — e.g. 500ms
2. Handle `SCN_DWELLSTART` in the Qt event loop (`qt/app.ss`)
3. Look up stored diagnostics for the hovered line/position
4. Show diagnostic text via `SCI_CALLTIPSHOW`
5. Handle `SCN_DWELLEND` → `SCI_CALLTIPCANCEL`
6. Style: `SCI_CALLTIPSETBACK` with yellow background, `SCI_CALLTIPSETFOREHLT` for severity coloring

**Files to modify:**
- `qt/app.ss` — handle `SCN_DWELLSTART` / `SCN_DWELLEND` notifications
- `qt/commands-lsp.ss` — store diagnostics in a position-indexed structure for fast lookup
- `qt/sci-shim.ss` — add `SCI_CALLTIP*` and `SCI_SETMOUSEDWELLTIME` constants if not present

### Task 3: TUI Parity (Optional)

- TUI already has `SCI_AUTOCSHOW` for word completion
- Could extend to support LSP completions if LSP is ever added to TUI
- Calltips would work the same way via `sci-send` in the TUI Scintilla widget
- Lower priority since TUI has no LSP integration currently

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
