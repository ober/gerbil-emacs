# Pending Work: Bug Fixes for Themes, Splits, LSP Visuals

## Status: Code Written, Needs Clean Build + Test + Commit

All code changes are DONE. `make clean` was run (all artifacts removed). Need `make build` then test.

---

## What Was Changed

### Bug 1: Erratic Frame Splitting → FIXED
**File: `qt/window.ss`**
- Added `qt-splitter-set-sizes!` calls after each split case in `qt-frame-do-split!`
- Case A (same orientation): equalizes all children with `(cons 500 acc)` loop
- Case B (first split): `(list 500 500)` for 50/50
- Case C (nested splitter): `(list 500 500)` for 50/50
- All calls wrapped in `(with-catch void ...)` for resilience against headless mode quirks

### Bug 2: Theme Switching Broken → FIXED
**File: `qt/window.ss`**
- Added import: `:gemacs/face`
- Added new function `qt-apply-editor-theme!` that reads ALL colors from the face system:
  - `'default` face → editor bg/fg + caret color
  - `'line-number` face → gutter bg/fg
  - `'cursor-line` face → caret line highlight bg
  - `'region` face → selection background (SCI_SETSELBACK = 2068)
  - Includes fallback hardcoded values when face system not yet initialized
- Modified `qt-scintilla-setup-editor!` to call `qt-apply-editor-theme!` instead of hardcoded hex colors
- Exported `qt-apply-editor-theme!`

**File: `qt/commands-core.ss`**
- Modified `apply-theme!` to iterate ALL visible editor windows and call `qt-apply-editor-theme!` on each
- This applies Scintilla base colors (bg, fg, caret, selection, line numbers) directly via SCI_* API
- Removed the TODO comments about updating visual decorations (now done)
- Kept existing: stylesheet for non-editor widgets, line-number-area widget colors, syntax re-highlighting

### Bug 3: LSP Visual Integration → IMPROVED
**File: `qt/commands-lsp.ss`**
- Added `lsp-clear-all-indicators!` function: clears all diagnostic underlines + margin markers from current editor
- Modified `cmd-toggle-lsp`:
  - When LSP starts: proactively calls `lsp-ensure-diagnostic-margin!` so indicators are visible immediately when diagnostics arrive
  - When LSP stops: calls `lsp-clear-all-indicators!` to remove stale visual markers

### Tests Added
**File: `qt-functional-test.ss`**
- Added Group 21: Theme Switching, Split Sizing, LSP Indicators (11 tests)
- Added imports: `qt-splitter-size-at`, `qt-apply-editor-theme!`, face system functions (`face-get`, `face-bg`, `face-fg`, `define-face!`, `define-standard-faces!`, `parse-hex-color`)
- Theme tests: verify `qt-apply-editor-theme!` sets bg/fg/cursor-line/line-number/selection from face system
- Split tests: verify first horizontal and vertical splits produce equal sizes
- LSP tests: verify toggle-lsp, lsp-restart, lsp-find-references commands registered
- Group 21 is called from `main` after Group 20

---

## To Complete This Work

```bash
# 1. Full build (make clean was already run, artifacts are gone)
make build

# 2. Verify both binaries
.gerbil/bin/gemacs --version
QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version

# 3. Run TUI tests
HOME=/home/jafourni LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/opt/openssl@3/lib \
  GERBIL_LOADPATH=/home/jafourni/.gerbil/lib timeout 120 gerbil test 2>&1

# 4. Run Qt tests (should now be 289+ tests with Group 21)
QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-functional-test

# 5. If all pass, commit and push
git add qt/window.ss qt/commands-core.ss qt/commands-lsp.ss qt-functional-test.ss
git commit -m "Fix theme switching, 50/50 splits, and LSP visual integration

- Theme: qt-apply-editor-theme! reads all Scintilla colors from face system
  instead of hardcoded hex values. apply-theme! now updates all visible editors.
- Splits: qt-frame-do-split! calls qt-splitter-set-sizes! after each case
  to enforce equal sizing (fixes erratic 90/10 splits).
- LSP: proactive margin setup on start, indicator cleanup on stop.
- 11 new Group 21 functional tests covering all three fixes.

Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>"
git push
```

---

## Files Modified (Summary)

| File | Changes |
|------|---------|
| `qt/window.ss` | +`qt-apply-editor-theme!`, face-based `qt-scintilla-setup-editor!`, split sizing, `:gemacs/face` import |
| `qt/commands-core.ss` | `apply-theme!` now calls `qt-apply-editor-theme!` on all editors |
| `qt/commands-lsp.ss` | +`lsp-clear-all-indicators!`, improved `cmd-toggle-lsp` |
| `qt-functional-test.ss` | +Group 21 (11 tests), new imports for face system + splitter sizes |

---

## Known Build Issue
- Previous build attempt hit GCC OOM during exe linking (transient resource issue)
- `modules_only` build confirmed all modules compile successfully (no code errors)
- The exe linking just needs enough memory; retry `make build` should work

---

## After These Bugs: Continue Feature Parity (User Request)

The user said: "commit and push once tests pass, and then continue making this the best alternative for emacs users who want to use gerbil have the best emacs experience possible!"

Previous analysis identified high-impact gaps:
- Fuzzy command/completion UI improvements
- Which-key display improvements
- Dired improvements (currently broken)
- Read-only occur mode
- Async command execution
