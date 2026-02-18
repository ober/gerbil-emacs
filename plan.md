# Plan: Comprehensive Functional Test Suite for Gemacs

## Context

Org-mode `<s TAB` has regressed 5 times. Root cause: **all existing tests call leaf command functions directly** (e.g., `cmd-org-template-expand`), bypassing the dispatch chain that real users go through. When dispatch logic breaks, tests pass but features are broken. This isn't just an org-mode problem — it's a structural testing gap across the entire editor. The Qt binary has a completely separate `cmd-indent-or-complete` and many other commands that are **never tested at all**.

### What "functional test" means here

A functional test simulates what the user does:
1. Open a buffer with specific content and mode
2. Position cursor
3. Press a key (or key sequence)
4. Verify the resulting buffer text, cursor position, and editor state

This means calling `cmd-indent-or-complete` (the TAB handler), NOT `cmd-org-template-expand`. It means feeding key events through `key-state-feed!` for multi-key sequences, NOT calling `cmd-find-file` directly. **Test the dispatch chain, not the leaf.**

## Plan

### Step 1: Create `functional-test.ss` — comprehensive TUI functional tests

Single new file, organized into test groups by feature area. Every test goes through the real dispatch path.

#### 1a. Test Framework Helpers

```scheme
;; Create headless org editor with full command registry
(defrule (with-org-editor (ed app) body ...)
  (let* ((ed (create-scintilla-editor width: 80 height: 24))
         (buf (make-buffer "test.org" "/tmp/test.org"
                (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
         (win (make-edit-window ed buf 0 0 80 24 0))
         (fr (make-frame [win] 0 80 24 'vertical))
         (app (new-app-state fr)))
    (register-all-commands!)
    body ...))

;; Create headless editor for arbitrary file type
(defrule (with-editor (ed app filename) body ...)
  (let* ((ed (create-scintilla-editor width: 80 height: 24))
         (buf (make-buffer filename #f
                (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
         (win (make-edit-window ed buf 0 0 80 24 0))
         (fr (make-frame [win] 0 80 24 'vertical))
         (app (new-app-state fr)))
    (register-all-commands!)
    body ...))

;; Execute a command by symbol through the REAL dispatch chain
;; (execute-command! already exists in core.ss:1039)

;; Simulate a full key sequence: feed events through key-state-feed!
;; then execute the resulting command
(def (simulate-keys app keys) ...)
```

The `simulate-keys` helper converts key strings like `"C-x C-f"`, `"TAB"`, `"M-f"` into the same dispatch that `app.ss:dispatch-key-normal!` uses: `key-state-feed!` → `execute-command!`. This is the **critical missing piece** — it tests the full chain from keymap lookup through command execution.

#### 1b. Test Groups (~120+ test cases total)

**Group 1: Org-Mode TAB Dispatch (24 tests)** — the immediate fire

- All 9 templates via `cmd-indent-or-complete`: `<s`, `<e`, `<q`, `<v`, `<c`, `<C`, `<l`, `<h`, `<a`
- Dispatch priority: table line > template > heading > plain text indent
- Edge cases: indented template, mid-file template, content preservation, cursor positioning
- Negative: non-org buffer, unknown template `<z`, empty line
- Registration guards: `org-template-expand` and `org-cycle` are registered procedures
- Heading TAB → org-cycle (fold/unfold)
- Org table TAB → next-cell

**Group 2: Navigation (15 tests)**

- `forward-char` / `backward-char` — cursor moves, wraps at line boundary
- `next-line` / `previous-line` — moves line, preserves goal column
- `forward-word` / `backward-word` — word-boundary detection
- `beginning-of-line` / `end-of-line` — first/last position
- `beginning-of-buffer` / `end-of-buffer` — extreme positions
- `goto-line` — jump to specific line (via `execute-command!`)
- Edge: navigation at buffer start/end (no crash, no-op)

**Group 3: Basic Editing (20 tests)**

- Self-insert: character appears at cursor, cursor advances
- `delete-char` / `backward-delete-char` — character removal
- `kill-line` — kills to end of line, empty line kills newline
- `newline` — splits line, cursor on new line
- `open-line` — inserts newline but cursor stays
- `undo` / `redo` — reverses and re-applies edits
- `transpose-chars` — swaps characters around cursor
- Auto-pairing: `(` inserts `()`, cursor between
- Prefix argument: `C-u 3 a` inserts `aaa`
- Edge: delete at buffer end, backspace at buffer start

**Group 4: Kill Ring & Yank (10 tests)**

- `kill-line` then `yank` — round-trip
- `kill-region` then `yank` — region kill/paste
- `copy-region` then `yank` — copy without removing
- `yank-pop` — cycles through kill ring
- Multiple kills accumulate in ring
- Kill-line on empty line kills newline only

**Group 5: Mark & Region (10 tests)**

- `set-mark` then movement → active region
- `kill-region` — deletes marked region
- `exchange-point-and-mark` — swaps positions
- `mark-word` / `mark-whole-buffer` — sets region automatically
- Region persists through navigation

**Group 6: Search (10 tests)**

- `isearch-forward` — activate, type chars, match found
- `isearch-backward` — reverse direction
- `query-replace` — find and replace pattern
- `occur` — list matching lines (output buffer created)
- Edge: search with no matches, wraparound

**Group 7: Buffer Management (10 tests)**

- `switch-buffer` — changes current buffer
- `kill-buffer-cmd` — removes buffer from list
- `previous-buffer` / `next-buffer` — cycling
- Buffer creation via find-file flow
- `*scratch*` buffer always exists
- `toggle-read-only` — prevents editing

**Group 8: Window Management (8 tests)**

- `split-window` — creates two windows showing same buffer
- `other-window` — switches active window
- `delete-window` — removes split
- `delete-other-windows` — returns to single window
- `balance-windows` — equalizes sizes
- Window switching preserves buffer and cursor per-window

**Group 9: Text Transforms (8 tests)**

- `upcase-word` / `downcase-word` / `capitalize-word`
- `upcase-region` / `downcase-region`
- `comment-region` / `uncomment-region` / `toggle-comment`
- `sort-lines` — alphabetical sort of region
- `join-line` — joins current line with next

**Group 10: Org Commands Beyond TAB (15 tests)**

- `org-todo` via dispatch — cycles TODO states through `execute-command!`
- `org-promote` / `org-demote` via dispatch
- `org-move-subtree-up` / `org-move-subtree-down`
- `org-toggle-checkbox`
- `org-priority` cycling
- `org-insert-heading`
- `org-insert-src-block`
- `org-shift-tab` — global visibility cycling
- `org-store-link`
- Org table alignment via `cmd-indent-or-complete` on table line
- Org table row/column operations

**Group 11: Mode-Specific Dispatch (5 tests)**

- `.org` file → `org-buffer?` returns true, TAB dispatches to org
- `.py` file → TAB inserts spaces (no org behavior)
- `.ss` file → TAB inserts spaces
- Dired buffer → TAB is no-op
- REPL buffer → TAB inserts spaces only after prompt

**Group 12: Prefix Arguments (5 tests)**

- `C-u` sets prefix to 4
- `M-3` sets prefix to 3
- Prefix consumed by movement (forward-char 3x)
- Prefix consumed by insert (self-insert 4x)
- Prefix resets after command execution

### Step 2: Create `qt-functional-test.ss` — Qt functional tests (exe target)

Follows `qt-highlight-test.ss` pattern: exe binary, `with-qt-app`, manual pass/fail counting, runs with `QT_QPA_PLATFORM=offscreen`.

Tests the **Qt-specific** `cmd-indent-or-complete` from `qt/commands.ss:842` and `qt-try-org-template-expand` from `qt/commands.ss:495`.

**30 test cases** mirroring the most critical TUI tests:

- All 9 org template expansions via Qt TAB dispatch
- Qt dispatch priority (table > template > heading > snippet > completion > indent)
- `<s` with indentation preservation
- Non-org buffer exclusion
- Qt heading fold/unfold via TAB
- Qt table next-cell via TAB
- Content preservation around templates
- Cursor positioning after expansion
- Qt self-insert (character via QScintilla)
- Qt navigation (forward/backward char via QScintilla)
- Qt kill-line + yank round-trip
- Qt buffer switching
- Qt window split/other-window

Key structural difference from TUI tests: uses `qt-scintilla-create`, `qt-edit-window`, `qt-frame`, and `qt-register-all-commands!` instead of TUI equivalents.

### Step 3: Update `build.ss`

```
line ~151: add "functional-test" after "persist-test"
line ~181: add exe target for qt-functional-test (same cc/ld options as qt-highlight-test)
```

### Step 4: Update `Makefile`

Add targets:
```makefile
test-qt: build
	QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-highlight-test
	QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-functional-test

test-all: build test test-qt
```

### Step 5: Update CLAUDE.md with testing policy

Add a section requiring that any code change to a `cmd-*` function or dispatch path must be accompanied by a functional test in `functional-test.ss` (TUI) or `qt-functional-test.ss` (Qt) that exercises the change through `cmd-indent-or-complete` or `execute-command!` — never by calling the leaf function directly.

## Files to create/modify

| File | Action | Purpose |
|------|--------|---------|
| `functional-test.ss` | **CREATE** | TUI functional tests (~120+ cases) |
| `qt-functional-test.ss` | **CREATE** | Qt functional tests (~30 cases, exe) |
| `build.ss` | EDIT | Add test module + Qt exe target |
| `Makefile` | EDIT | Add `test-qt` and `test-all` targets |
| `CLAUDE.md` (project) | EDIT | Add testing policy for dispatch-level tests |

## Key imports

**`functional-test.ss`**:
- `:std/test`, `:std/srfi/13`
- `:gerbil-scintilla/scintilla`, `:gerbil-scintilla/constants`
- `:gemacs/core` (keymaps, execute-command!, find-command, app-state, buffer, make-frame, make-edit-window, register-command!)
- `:gemacs/buffer` (make-buffer, buffer-name, buffer-file-path, etc.)
- `:gemacs/window` (make-edit-window, make-frame, current-window)
- `:gemacs/echo` (echo state)
- `:gemacs/editor-core` (self-insert, navigation, editing primitives)
- `:gemacs/editor-ui` (cmd-indent-or-complete, org-buffer?, and other TUI dispatch functions)
- `:gemacs/keymap` (key-state-feed!, make-key-state — for simulate-keys helper)
- `:gemacs/editor` (register-all-commands!)
- `:gemacs/editor-extra-org` (org-table-on-table-line?, org-highlight-buffer!, etc.)

**`qt-functional-test.ss`**:
- `:std/sugar`
- `:gerbil-qt/qt`
- `:gerbil-scintilla/constants`
- `:gemacs/qt/sci-shim` (qt-plain-text-edit-*, qt-scintilla-create, sci-send)
- `:gemacs/core` (app-state, buffer structs)
- `:gemacs/qt/window` (qt-edit-window, qt-frame, qt-current-window)
- `:gemacs/qt/commands-core` (current-qt-editor, current-qt-buffer)
- `:gemacs/qt/commands` (cmd-indent-or-complete, qt-register-all-commands!, qt-org-buffer?)

## Verification

1. `make build` — both test files compile
2. `HOME=/home/jafourni GERBIL_LOADPATH=/home/jafourni/.gerbil/lib timeout 120 gerbil test 2>&1` — TUI functional tests pass
3. `QT_QPA_PLATFORM=offscreen .gerbil/bin/qt-functional-test` — Qt functional tests pass
4. `.gerbil/bin/gemacs --version` — TUI binary unaffected
5. `QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version` — Qt binary unaffected

## Why this catches regressions permanently

Every functional test calls through the **dispatch chain**, not leaf functions:
- `cmd-indent-or-complete` for TAB behavior (not `cmd-org-template-expand`)
- `execute-command!` for named commands (not `cmd-org-todo` directly)
- `simulate-keys` for multi-key sequences (not bypassing keymap lookup)

If anyone touches `cmd-indent-or-complete`, `org-buffer?`, `key-state-feed!`, the character gate in the template detector, the command registration, or ANY part of the dispatch chain — the functional tests fail. The leaf-function tests in `emacs-test.ss` remain for unit-level coverage. The functional tests in `functional-test.ss` ensure the pieces actually work together.

## Implementation order

1. `functional-test.ss` Group 1 (org TAB) — **immediate regression fix**
2. `functional-test.ss` Groups 2-5 (nav, editing, kill ring, mark) — **core editing**
3. `functional-test.ss` Groups 6-12 (search, buffers, windows, transforms, etc.) — **full coverage**
4. `qt-functional-test.ss` — **Qt parity**
5. `build.ss` + `Makefile` updates
6. `CLAUDE.md` testing policy
