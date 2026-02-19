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

### 5. TUI window.ss tree-based rewrite (IN PROGRESS)

File: `window.ss`

**COMPLETE ARCHITECTURAL REWRITE** to match Qt's tree-based model:

**What was implemented**:
- ✅ Added `split-leaf` and `split-node` structures (matching Qt)
- ✅ Changed `frame` structure: removed `split-direction`, added `root` tree
- ✅ Implemented tree helper functions: `split-tree-flatten`, `split-tree-find-parent`, `split-tree-find-leaf`, `split-tree-find-parent-of-node`, `split-tree-replace-child!`, `split-tree-remove-child!`
- ✅ Rewrote `frame-do-split!` with Cases A/B/C (same logic as Qt's `qt-frame-do-split!`)
- ✅ Rewrote `frame-delete-window!` and `frame-delete-other-windows!` for tree model
- ✅ Implemented recursive `split-tree-layout!` to compute x/y/w/h from tree
- ✅ Updated `frame-draw-dividers!` for recursive tree traversal
- ✅ Added helper functions: `list-index`, `last`

**Current status**: Code is architecturally complete but has **parenthesis balancing issues** preventing compilation.

**What needs to be done**:
1. Fix remaining paren balance errors in `split-tree-layout!` function
2. Build and verify compilation succeeds
3. Add comprehensive functional tests (equivalent to Qt Group 11)
4. Test that nested splits work correctly (hsplit → vsplit, vsplit → hsplit, etc.)

**Known issues**:
- Compilation error: "Bad syntax; empty body" in `split-tree-layout!`
- Likely cause: Missing or extra closing parens in nested let*/if/loop structures
- Recommendation: Use editor with paren-matching (Emacs paredit, VS Code Rainbow Brackets) or provide clean rewrite

## What Still Needs To Be Done

### 1. ✅ DONE: Fix TUI window.ss compilation errors

**File**: `window.ss`

**Status**: FIXED — used `gerbil_check_balance` to find two extra closing parens at lines 253 and 460. Removed both. Also changed `let` to `let*` in `frame-layout!` to allow binding references. File now compiles successfully.

**Changes**:
- Line 253: removed 1 extra `)` from `split-tree-layout!` horizontal branch
- Line 460: removed 1 extra `)` from `split-tree-draw-dividers!`
- Line 173: changed `let` to `let*` in `frame-layout!`

### 2. ✅ DONE: Add TUI Group 11 equivalent functional tests

**File**: `functional-test.ss`

**Status**: COMPLETE — Added 10 comprehensive window management tests (32 checks total).

**Tests added**:
1. split-window-below creates 2 windows (3 checks)
2. split-window-right creates 2 windows (3 checks)
3. delete-window restores single pane (2 checks)
4. delete-other-windows collapses to single pane (2 checks)
5. **hsplit → other-window → vsplit** (the reported bug) (8 checks)
6. vsplit → other-window → hsplit (8 checks)
7. four-pane grid 2x2 (1 check)
8. other-window cycles through all panes (1 check)
9. three-way horizontal split uses flat siblings (4 checks)

**Also updated**:
- Fixed `make-test-app` and `make-test-app-with-file` to use tree-based frame structure
- All tests use `execute-command!` per CLAUDE.md policy

**Results**: All 32 new checks pass ✅

### 3. ✅ DONE: Qt focus fixes already implemented

**Status**: Both focus fixes were already implemented in prior commits:
- `qt-frame-other-window!` (lines 446-453): already calls `qt-widget-set-focus!` on the new editor
- `qt-frame-do-split!` (lines 339-340): already calls `qt-widget-set-focus!` after splits

### 4. ✅ DONE: Build and test

**Status**: Built successfully and tested:
- `make clean && make build` — succeeded
- Both binaries work: `gemacs --version` and `gemacs-qt --version`
- Qt tests: **230/230 passed** (all Group 1-12 tests including layout verification)
- TUI tests: Pre-existing failures in VC commands (unrelated to window.ss changes)

### 5. ✅ DONE: Commit

**Status**: Committed as `d46a431`.

**Commit**: "Fix TUI window.ss compilation — tree-based split rewrite complete"

All changes span two repos:
- **gerbil-qt**: new splitter FFI functions (insertWidget, indexOf, widget) — already committed
- **gerbil-emacs**: window.ss tree-based rewrite + paren fixes — committed

### 6. ✅ DONE: Push to remote

**Status**: PUSHED successfully.

**Commits pushed**:
- `d46a431` - Fix TUI window.ss compilation (tree-based rewrite complete)
- `fdd9c6f` - Add comprehensive TUI window management tests (32 checks pass)

**Remote**: `github.com:ober/gerbil-emacs.git` (master branch)

### 7. ✅ DONE: Modeline visual indicator for active window

**Status**: IMPLEMENTED for both TUI and Qt.

**TUI Implementation**:
- Already had visual indicators built-in (discovered during review)
- `modeline-draw!` takes `is-current` parameter
- Active window: dark text (#x000000) on light gray (#xd8d8d8)
- Inactive windows: gray text (#x808080) on dark gray (#x282828)
- Called correctly from `draw-all-modelines!` in app.ss

**Qt Implementation** (newly added):
- Added `qt-frame-update-visual-indicators!` function
- Active window: 2px solid blue border (#51afef)
- Inactive windows: 1px subtle gray border (#3a3a3a)
- Called automatically after:
  - Frame initialization
  - Window splits (split-below, split-right)
  - Window deletion (delete-window, delete-other-windows)
  - Window navigation (other-window)

**Results**: All 230 Qt tests pass with visual indicators ✅

## File Change Summary

### gerbil-qt (already built and synced)
- `vendor/qt_shim.cpp` — 3 new C++ functions
- `libqt.ss` — 3 c-lambda declarations + begin-ffi exports
- `qt.ss` — 3 Gerbil wrappers + exports

### gerbil-emacs
- `qt/window.ss` — Case C fix (insertWidget), delete-window fix, focus already implemented
- `qt-functional-test.ss` — Group 12 layout tests (8 tests, ~45 checks), updated imports
- `window.ss` — ✅ TUI tree-based architecture rewrite + paren fixes (lines 253, 460, 173)
