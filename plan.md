# Qt Window Management Fix — Status & Remaining Work

## What Was Done

### 1. Added `insertWidget`, `indexOf`, `widget` to gerbil-qt FFI

Files modified in `/home/jafourni/mine/gerbil-qt/`:
- `vendor/qt_shim.cpp` — Added `qt_splitter_insert_widget`, `qt_splitter_index_of`, `qt_splitter_widget`
- `libqt.ss` — Added c-lambda declarations AND added them to the `begin-ffi` export list (lines ~215)
- `qt.ss` — Added Gerbil wrappers `qt-splitter-insert-widget!`, `qt-splitter-index-of`, `qt-splitter-widget` + exports

gerbil-qt was rebuilt and artifacts synced to `~/.gerbil/lib/`.

### 2. Fixed Qt split ordering bug (Case C in `qt-frame-do-split!`)

File: `qt/window.ss`

**Root cause**: Case C created a nested QSplitter with `parent: parent-spl`, relying on Qt's implicit childEvent to position it. This was unreliable — the new splitter could end up at the wrong index.

**Fix**:
- Get `cur-idx-in-spl = qt-splitter-index-of parent-spl cur-container` BEFORE the split
- Create `new-spl` WITHOUT a parent (no `parent:` arg)
- After reparenting cur-container into new-spl, use `qt-splitter-insert-widget! parent-spl cur-idx-in-spl new-spl` to place it at the exact position

### 3. Fixed Qt delete-window unwrap ordering

File: `qt/window.ss`, in `qt-frame-delete-window!`

Same issue: when unwrapping a single-child node, `addWidget` appended to end. Fixed to use:
```scheme
(let ((spl-idx (qt-splitter-index-of dest-spl parent-spl)))
  (qt-splitter-insert-widget! dest-spl spl-idx only-qt-w))
```

### 4. Added Group 12 layout verification tests

File: `qt-functional-test.ss`

Added 8 layout tests (L1-L8) that verify the ACTUAL Qt widget hierarchy using `qt-splitter-index-of` and `qt-splitter-count`. These catch bugs where the logical tree is correct but Qt widgets are in the wrong position.

Key tests:
- **L3**: split-below then split-right in bottom pane (the exact reported bug)
- **L4**: split-below + 3× split-right in bottom (user's full scenario)
- **L6**: 3-way vertical then horizontal nest in middle
- **L7/L8**: full recursive layout verification

All 230 tests pass (185 existing + 45 new).

### 5. TUI window.ss partial fix

File: `window.ss`

Fixed `frame-split!` and `frame-split-right!` to:
- Only set global `split-direction` on the FIRST split (not override on subsequent splits)
- Insert new window after current window (not append to end)
- Update `current-idx` to point to the new window

**Note**: TUI still has no nested split support — it's a flat model. Mixed h/v splits aren't possible.

## What Still Needs To Be Done

### 1. Fix `other-window` (C-x o) not working in Qt — CRITICAL

**Problem**: `qt-frame-other-window!` in `qt/window.ss` line 444 only updates `current-idx` but does NOT give keyboard focus to the new editor widget. So pressing C-x o updates the internal state but keystrokes still go to the old editor.

**Fix needed** in `qt/window.ss`, replace the current `qt-frame-other-window!`:
```scheme
(def (qt-frame-other-window! fr)
  "Switch to the next window (wraps around)."
  (let ((n (length (qt-frame-windows fr))))
    (set! (qt-frame-current-idx fr)
          (modulo (+ (qt-frame-current-idx fr) 1) n))
    ;; Give keyboard focus to the new active editor
    (let ((win (list-ref (qt-frame-windows fr) (qt-frame-current-idx fr))))
      (qt-widget-set-focus! (qt-edit-window-editor win)))))
```

**Also**: Add focus after splits in `qt-frame-do-split!`. After the `qt-widget-resize!` restore block (~line 338), add:
```scheme
;; Focus the new editor
(when result (qt-widget-set-focus! result))
```

`qt-widget-set-focus!` is already exported from `:gerbil-qt/qt` and available via `:gemacs/qt/sci-shim` (which is already imported in `qt/window.ss`).

### 2. Build and test after focus fix

```bash
HOME=/home/jafourni GERBIL_LOADPATH=/home/jafourni/.gerbil/lib gerbil build
# Then run Qt tests:
QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-functional-test
# Then test binaries:
QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version
.gerbil/bin/gemacs --version
```

### 3. Commit and push

All changes span two repos:
- **gerbil-qt**: new splitter FFI functions (insertWidget, indexOf, widget)
- **gerbil-emacs**: Qt split fix, delete-window fix, layout tests, TUI split fix, focus fix

### 4. (Nice-to-have) Modeline visual indicator for active window

Currently the modeline doesn't visually indicate which window is active. After other-window works, the user needs a visual cue (e.g. different modeline background color for active vs inactive windows).

## File Change Summary

### gerbil-qt (already built and synced)
- `vendor/qt_shim.cpp` — 3 new C++ functions
- `libqt.ss` — 3 c-lambda declarations + begin-ffi exports
- `qt.ss` — 3 Gerbil wrappers + exports

### gerbil-emacs
- `qt/window.ss` — Case C fix (insertWidget), delete-window fix, focus fix needed (see above)
- `qt-functional-test.ss` — Group 12 layout tests (8 tests, ~45 checks), updated imports
- `window.ss` — TUI split-direction fix, insert-after-current fix
