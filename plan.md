# Comprehensive Org-Mode Table Commands Implementation Plan

## Context

The gemacs codebase has a **complete backend** for org-mode tables in `org-table.ss` (676 lines, 40 functions) covering parsing, alignment, navigation, sorting, formulas, and CSV import/export. However, only **1 command** (`cmd-org-table-delete-column`) is exposed to users in the Qt layer, and it doesn't even use the backend — it reimplements the logic inline. The Qt layer also duplicates 6 table helper functions (`qt-org-table-*`) in `qt/commands.ss:576-645` instead of importing `org-table.ss`.

**Goal**: Wire the existing backend to the Qt command layer and add the missing high-value table commands that org-mode users rely on daily.

---

## Current State

### What exists in `org-table.ss` (backend, TUI Scintilla API):
- Table detection, parsing, alignment
- Cell navigation (next/prev cell, next row same column)
- Column/row insert, delete, move
- Table sorting (numeric + alphabetic)
- Separator completion
- Formula parsing and evaluation (`#+TBLFM:`)
- CSV import/export

### What's exposed as Qt commands:
- `cmd-org-table-delete-column` — only one, and it's a crude inline reimplementation
- `qt-org-table-next-cell` — wired into TAB dispatch (works, but is also a reimplementation)

### Key problem:
The Qt layer **does not import `org-table.ss`** at all. It reimplements table logic using the Qt `qt-plain-text-edit-*` API instead of the Scintilla `editor-*` API that `org-table.ss` uses. The Qt layer must wrap each backend function with a Qt-specific command that bridges the API gap.

---

## Architecture Decision

**Approach: Write Qt wrapper commands in `qt/commands-modes.ss`**

The backend `org-table.ss` uses the TUI Scintilla API (`editor-get-line`, `editor-goto-pos`, etc.). The Qt layer uses a different API (`qt-plain-text-edit-text`, `qt-plain-text-edit-set-text!`, etc.). Rather than refactoring the backend (which would break the TUI), we'll write Qt-specific `cmd-org-table-*` commands following the same pattern as the existing `cmd-org-*` commands in `qt/commands-modes.ss`.

These commands will reuse the **string-based backend functions** where possible (`org-table-row?`, `org-table-separator?`, `org-table-parse-row`, `org-table-column-widths`, `org-table-format-row`, `org-table-format-separator`, `org-table-parse-tblfm`, `org-table-eval-formula`, `org-csv-to-table`, `csv-split-line`) since those are backend-agnostic.

The editor-specific operations (find bounds, get rows, replace rows, cursor positioning) will be implemented using the Qt API, following the pattern of `qt-org-table-next-cell` in `qt/commands.ss:647-745`.

---

## Implementation Plan

### Phase 1: Core Qt Table Infrastructure (~150 lines)

**File**: `qt/commands-modes.ss`

Add shared Qt table helper functions that mirror the backend's editor-specific operations but use the Qt API:

1. **`qt-org-table-find-bounds`** `(ed text) -> (values start-line end-line)`
   - Find contiguous table row region around cursor
   - Uses `sci-send ed SCI_LINEFROMPOSITION`, `SCI_POSITIONFROMLINE`, `SCI_GETLINEENDPOSITION`
   - Pattern: same as the inline logic in `qt-org-table-next-cell` (commands.ss:661-676)

2. **`qt-org-table-get-rows`** `(ed text start end) -> list`
   - Get rows as list-of-lists with `'separator` markers
   - Reuses `qt-org-table-parse-row` and `qt-org-table-separator?` from commands.ss

3. **`qt-org-table-replace-rows`** `(ed text start end rows widths) -> new-text`
   - Format and replace table region in text string
   - Returns new full text string for `qt-plain-text-edit-set-text!`
   - Reuses `qt-org-table-format-row` and `qt-org-table-format-separator`

4. **`qt-org-table-current-column`** `(ed text pos) -> int`
   - Count `|` chars before cursor to determine column index

5. **`qt-org-table-goto-column-pos`** `(ed line-num col widths) -> int`
   - Calculate cursor position for column `col` in `line-num`

### Phase 2: Table Manipulation Commands (~200 lines)

**File**: `qt/commands-modes.ss`

| Command | Symbol | Emacs Binding | Description |
|---------|--------|---------------|-------------|
| `cmd-org-table-align` | `org-table-align` | `C-c C-c` (on table) | Re-align entire table |
| `cmd-org-table-insert-row` | `org-table-insert-row` | `M-S-down` | Insert empty row above |
| `cmd-org-table-delete-row` | `org-table-delete-row` | `M-S-up` (kill) | Delete current row |
| `cmd-org-table-insert-column` | `org-table-insert-column` | `M-S-right` | Insert empty column after current |
| `cmd-org-table-move-column-left` | `org-table-move-column-left` | `M-left` (on table) | Move column left |
| `cmd-org-table-move-column-right` | `org-table-move-column-right` | `M-right` (on table) | Move column right |
| `cmd-org-table-move-row-up` | `org-table-move-row-up` | `M-up` (on table) | Move row up |
| `cmd-org-table-move-row-down` | `org-table-move-row-down` | `M-down` (on table) | Move row down |
| `cmd-org-table-insert-separator` | `org-table-insert-separator` | `C-c -` | Insert separator `\|---+---\|` below |

Rewrite `cmd-org-table-delete-column` to use the shared helpers instead of the current inline implementation.

### Phase 3: Table Sort & Formula Commands (~120 lines)

**File**: `qt/commands-modes.ss`

| Command | Symbol | Emacs Binding | Description |
|---------|--------|---------------|-------------|
| `cmd-org-table-sort` | `org-table-sort` | `C-c ^` | Sort by current column (prompt: alpha/numeric, asc/desc) |
| `cmd-org-table-recalculate` | `org-table-recalculate` | `C-c C-c` (on TBLFM) | Recalculate formulas |
| `cmd-org-table-sum` | `org-table-sum` | `C-c +` | Sum numeric cells in column, show in echo |

**Sort** prompts user: "Sort by column N: [a]lphabetic [n]umeric [A]lpha-rev [N]umeric-rev"
**Recalculate** detects `#+TBLFM:` line after table and evaluates formulas.
**Sum** collects numeric values in current column, displays sum in echo area (like Emacs `C-c +`).

### Phase 4: Table Creation & Import/Export (~100 lines)

**File**: `qt/commands-modes.ss`

| Command | Symbol | Emacs Binding | Description |
|---------|--------|---------------|-------------|
| `cmd-org-table-create` | `org-table-create` | `C-c \|` | Create table (prompt for cols) or convert CSV region |
| `cmd-org-table-export-csv` | `org-table-export-csv` | — | Export table to CSV in new buffer |
| `cmd-org-table-import-csv` | `org-table-import-csv` | — | Import CSV from region/file as org table |
| `cmd-org-table-transpose` | `org-table-transpose` | — | Swap rows and columns |

**Create** checks if region is active: if yes, treat as CSV and convert; if no, prompt for number of columns and insert template (following the TUI `cmd-org-table-create` pattern from `editor-extra-org.ss:272`).

**Transpose** swaps rows and columns (simple matrix transpose of the data cells, preserving separators as needed).

### Phase 5: Enhanced Formula Support (~80 lines)

**File**: `org-table.ss` (extend backend)

Add built-in formula functions to `org-table-eval-formula`:
- `vsum($N)` — sum of column N (already implicit, make explicit)
- `vmean($N)` — average of column N
- `vmin($N)` / `vmax($N)` — min/max of column N
- `vcount($N)` — count of non-empty cells

These are the most commonly used org-mode formula functions. The existing formula parser in `org-table.ss:522-549` handles `$N` column refs and basic arithmetic. We extend it to recognize `vsum(...)`, `vmean(...)`, etc.

### Phase 6: Command Registration & Keybindings

**File**: `qt/commands.ss` — Add to `qt-register-all-commands!`:
```scheme
(register-command! 'org-table-align cmd-org-table-align)
(register-command! 'org-table-insert-row cmd-org-table-insert-row)
(register-command! 'org-table-delete-row cmd-org-table-delete-row)
(register-command! 'org-table-insert-column cmd-org-table-insert-column)
(register-command! 'org-table-move-column-left cmd-org-table-move-column-left)
(register-command! 'org-table-move-column-right cmd-org-table-move-column-right)
(register-command! 'org-table-move-row-up cmd-org-table-move-row-up)
(register-command! 'org-table-move-row-down cmd-org-table-move-row-down)
(register-command! 'org-table-insert-separator cmd-org-table-insert-separator)
(register-command! 'org-table-sort cmd-org-table-sort)
(register-command! 'org-table-recalculate cmd-org-table-recalculate)
(register-command! 'org-table-sum cmd-org-table-sum)
(register-command! 'org-table-create cmd-org-table-create)
(register-command! 'org-table-export-csv cmd-org-table-export-csv)
(register-command! 'org-table-import-csv cmd-org-table-import-csv)
(register-command! 'org-table-transpose cmd-org-table-transpose)
```

**Keybindings**: All commands accessible via `M-x`. Org-mode-specific keybindings (C-c ^, C-c -, C-c +, C-c |) are context-sensitive in org-mode buffers only, handled by the existing org-mode keymap dispatch.

### Phase 7: Functional Tests

**File**: `qt-functional-test.ss` — Add test section for org-table commands:

1. **Table creation**: `execute-command! 'org-table-create` → verify table template inserted
2. **Alignment**: Insert unaligned table → `execute-command! 'org-table-align` → verify aligned
3. **Row operations**: Insert row, delete row, move row up/down → verify table state
4. **Column operations**: Insert/delete/move column → verify table state
5. **Sort**: Insert numeric data → sort → verify order
6. **Separator**: Insert separator → verify format
7. **CSV export**: Create table → export → verify CSV format
8. **Transpose**: Create table → transpose → verify rows/cols swapped
9. **Sum**: Column with numbers → sum → verify echo message

All tests go through `execute-command!` dispatch chain per project testing policy.

---

## Files Modified

| File | Current Lines | Est. Added | Changes |
|------|:---:|:---:|---------|
| `qt/commands-modes.ss` | 1434 | ~500 | Add all `cmd-org-table-*` functions + Qt helpers |
| `qt/commands.ss` | 2138 | ~20 | Register 16 new commands |
| `org-table.ss` | 676 | ~40 | Add `vsum`/`vmean`/`vmin`/`vmax`/`vcount` formula functions |
| `qt-functional-test.ss` | — | ~100 | Add table command tests |

**Note**: `qt/commands-modes.ss` will grow to ~1934 lines, still under the 2000-line limit.

---

## Reusable Functions (no reimplementation needed)

From `qt/commands.ss` (already available in Qt layer):
- `qt-org-table-row?` (line 579)
- `qt-org-table-separator?` (line 585)
- `qt-org-table-parse-row` (line 597)
- `qt-org-table-column-widths` (line 609)
- `qt-org-table-format-row` (line 626)
- `qt-org-table-format-separator` (line 640)

From `org-table.ss` (string-based, backend-agnostic):
- `org-table-parse-tblfm` (line 508)
- `org-table-eval-formula` (line 522)
- `org-csv-to-table` (line 586)
- `csv-split-line` (line 597)
- `org-table-to-csv` — needs adaptation (uses `ed`)
- `swap-list-elements`, `list-insert`, `list-remove-at`, `filter-map` (utility)

---

## Verification

1. **Build**: `make build` — verify clean compilation
2. **Binary smoke test**: `.gerbil/bin/gemacs --version` and `QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version`
3. **Functional tests**: `make test-qt` — all table tests pass through dispatch chain
4. **Manual verification**: Open an org file with tables, test each command interactively

---

## Priority Order

1. **Phase 1** (infrastructure) — foundation for everything else
2. **Phase 2** (manipulation) — highest user value: insert/delete/move rows+columns
3. **Phase 6** (registration) — make Phase 2 commands accessible
4. **Phase 3** (sort + formulas) — second-highest value
5. **Phase 4** (create + CSV) — convenience features
6. **Phase 5** (enhanced formulas) — nice-to-have
7. **Phase 7** (tests) — run throughout, add final comprehensive suite
