# Full Org-Mode Implementation Plan

## Context

gerbil-emacs has a basic org-mode with ~24 commands (TODO cycling, headings, folding, templates, basic scheduling, etc.) in `editor-extra-org.ss`. However critical features are missing or broken: tables are template-only (no alignment/navigation), there's no babel (code execution), the agenda is buggy, export is plain-text only, clock elapsed time is hardcoded to `0:00`, and there's no syntax highlighting. This plan delivers full Emacs-compatible org-mode with comprehensive tests.

## File Organization

The current `editor-extra-org.ss` will be refactored. New modules are added to the build chain:

| New File           | Purpose                                                               |
|--------------------|-----------------------------------------------------------------------|
| `org-parse.ss`     | Core parser: timestamps, headings, properties, planning lines, blocks |
| `org-table.ss`     | Table parsing, alignment, cell navigation, formulas, CSV              |
| `org-babel.ss`     | Code block execution, sessions, noweb, tangle                         |
| `org-agenda.ss`    | Agenda views, dispatcher, date navigation, filtering                  |
| `org-export.ss`    | Export backends: HTML, Markdown, LaTeX, PDF, plain text               |
| `org-clock.ss`     | Clock in/out fix, elapsed calculation, clock reports                  |
| `org-capture.ss`   | Capture templates, refile, target selection                           |
| `org-highlight.ss` | Custom container lexer for org syntax highlighting                    |
| `org-list.ss`      | List handling: ordered/unordered, checkboxes, statistics              |

**Build order** in `build.ss` (insert after `editor-extra-helpers`, before `editor-extra-org`):
```
"org-parse"
"org-highlight"
"org-table"
"org-babel"
"org-clock"
"org-list"
"org-agenda"
"org-export"
"org-capture"
"editor-extra-org"   ;; existing, trimmed — keeps heading/folding/template commands
```

All new modules are backend-agnostic (use Scintilla API only, no Qt imports) so they work for both TUI and Qt paths.

---

## Phase 1: Core Parser (`org-parse.ss`)

Everything depends on robust parsing. This module is pure logic with no editor deps.

### Data Structures

```scheme
(defstruct org-timestamp
  (type         ; 'active | 'inactive
   year month day
   day-name     ; string or #f ("Mon", "Tue")
   hour minute  ; integers or #f
   end-hour end-minute ; integers or #f (time ranges)
   repeater     ; string or #f ("+1w", ".+1d", "++1m")
   warning)     ; string or #f ("-3d")
  transparent: #t)

(defstruct org-heading
  (level keyword priority title tags
   scheduled deadline closed
   properties   ; hash-table
   clocks       ; list of (start-ts . end-ts-or-#f)
   line-number file-path)
  transparent: #t)
```

### Functions

- `org-parse-timestamp : string -> org-timestamp or #f` — Parse `<2024-01-15 Mon 10:00>` and `[...]` variants with repeaters/warnings
- `org-timestamp->date : org-timestamp -> srfi-19-date` — Convert for arithmetic
- `org-timestamp->string : org-timestamp -> string` — Render back
- `org-timestamp-elapsed : org-timestamp org-timestamp -> string` — Compute `"H:MM"` elapsed
- `org-parse-heading-line : string -> org-heading` (partial) — Extract level, keyword, priority, title, tags
- `org-parse-planning-line : string -> (values sched-ts dead-ts closed-ts)` — Parse SCHEDULED/DEADLINE/CLOSED
- `org-parse-properties : list start-idx -> hash-table` — Parse `:PROPERTIES:` drawer
- `org-parse-clock-line : string -> (values start-ts end-ts duration)` — Parse CLOCK entries
- `org-parse-buffer : string string -> list-of-org-heading` — Full buffer parse
- `org-parse-file : path -> list-of-org-heading` — Read file + parse
- `org-parse-buffer-settings : string -> hash-table` — Parse `#+TODO:`, `#+TITLE:`, `#+STARTUP:`, `#+TAGS:` etc.
- `org-heading-match-tags? : org-heading string -> boolean` — Tag expression matching (`+work-personal`)
- `org-apply-repeater : org-timestamp srfi-19-date -> org-timestamp` — Advance repeating timestamps

### Global State

```scheme
(def *org-todo-keywords* '(("TODO") . ("DONE")))  ;; customizable per-buffer
(def *org-agenda-files* '())                        ;; list of file paths
(def *org-buffer-settings* (make-hash-table-eq))    ;; buffer -> settings hash
```

### Tests (~25)

- Timestamp parsing: active, inactive, date-only, time-range, repeater, warning
- Heading parsing: full (`** TODO [#A] Title :tag1:tag2:`), minimal (`* Hello`), no keyword, no tags
- Planning line parsing: SCHEDULED only, DEADLINE only, both, CLOSED
- Property drawer parsing
- Clock line parsing and elapsed time calculation (0:00, 1:30, across midnight)
- Buffer-level parsing with multiple headings
- Buffer settings parsing (`#+TODO: TODO NEXT | DONE CANCELLED`)
- Tag matching expressions

### Files Modified
- `build.ss` — Add `"org-parse"` to compile order
- `editor-extra-org.ss` — Import `org-parse`, use its structs
- `emacs-test.ss` — Add parser tests

---

## Phase 2: Syntax Highlighting (`org-highlight.ss`)

No built-in Scintilla lexer for org exists. Use the manual `SCI_STARTSTYLING`/`SCI_SETSTYLING` approach (proven pattern from `terminal.ss` ANSI coloring).

### Style IDs (32-63, avoids collision with lexer styles 0-31 and terminal 64-79)

| ID    | Element                         | Style                                                      |
|-------|---------------------------------|------------------------------------------------------------|
| 32    | Default                         | plain text                                                 |
| 33-40 | Heading 1-8                     | blue/orange/green/cyan/purple/yellow bold, decreasing size |
| 41    | TODO                            | red bold                                                   |
| 42    | DONE                            | green bold                                                 |
| 43    | Tags                            | purple                                                     |
| 44    | Comment                         | gray italic                                                |
| 45    | Keyword (`#+TITLE:`)            | orange                                                     |
| 46    | Bold (`*text*`)                 | bold                                                       |
| 47    | Italic (`/text/`)               | italic                                                     |
| 48    | Underline (`_text_`)            | underline                                                  |
| 49    | Verbatim (`=text=`)             | monospace cyan                                             |
| 50    | Code (`~text~`)                 | monospace green                                            |
| 51    | Link (`[[url][desc]]`)          | blue underline                                             |
| 52    | Date (`<2024-01-15>`)           | magenta                                                    |
| 53    | Property drawer                 | dim                                                        |
| 54    | Block delimiter (`#+BEGIN_SRC`) | orange                                                     |
| 55    | Block body                      | subtle background                                          |
| 56    | Checkbox checked `[X]`          | green                                                      |
| 57    | Checkbox unchecked `[ ]`        | red                                                        |
| 58    | Table (`\| cell \|`)            | cyan                                                       |

### Functions

- `setup-org-styles! : editor -> void` — Configure all style colors/attributes
- `org-highlight-buffer! : editor string -> void` — Full-buffer highlighting with state machine (tracks block/drawer context)
- `org-highlight-range! : editor string start-line end-line -> void` — Incremental re-highlighting
- `org-set-fold-levels! : editor string -> void` — Set `SCI_SETFOLDLEVEL` per line for native fold margin

### Line Classification (state machine)

State: `'normal | 'src-block | 'drawer`
- Heading line → heading style by level + sub-ranges for TODO/DONE/priority/tags
- `#+BEGIN_SRC` → block-delim, enter `'src-block`; body → block-body; `#+END_SRC` → block-delim, exit
- `:PROPERTIES:` → drawer, enter `'drawer`; `:END:` → exit
- `# comment` → comment style
- `#+KEY:` → keyword style
- `| cell |` → table style
- Inline markup scan: `*bold*`, `/italic/`, `=verbatim=`, `~code~`, `[[link]]`, `<date>`, `[fn:N]`

### Integration Points

- **TUI**: `highlight.ss` `setup-highlighting-for-file!` — add `'org` case calling `org-highlight-buffer!`
- **Qt**: `qt/highlight.ss` `qt-setup-highlighting!` — replace `#f` return with custom styling
- Re-highlight on text changes (hook into existing idle/redraw)

### Tests (~10)

- Style verification with `SCI_GETSTYLEAT` for headings, TODO/DONE, code blocks, links
- Multi-line block context (BEGIN_SRC...END_SRC body gets block-body style)
- Fold level verification for heading hierarchy

### Files Modified
- `build.ss` — Add `"org-highlight"`
- `highlight.ss` — Add `'org` branch in `setup-highlighting-for-file!`
- `qt/highlight.ss` — Replace `#f` return for org with custom styling call

---

## Phase 3: Org Tables (`org-table.ss`)

### Parsing Functions

- `org-table-row? : string -> boolean` — Starts with `|`
- `org-table-separator? : string -> boolean` — `|---+---|` pattern
- `org-table-parse-row : string -> list-of-strings` — Split `| a | b |` into `("a" "b")`
- `org-table-find-bounds : list line-num -> (values start end)` — Find table region
- `org-table-column-widths : rows -> list-of-int` — Max width per column
- `org-table-current-column : editor pos -> int` — Which column cursor is in
- `org-numeric-cell? : string -> boolean` — Detect numbers for right-alignment

### Core Operations

- **`org-table-align : editor -> void`** — The critical function. Realign entire table: pad cells to uniform widths, numbers right-aligned, text left-aligned, rebuild separators. Uses full text rebuild via `editor-set-text`.
- `org-table-next-cell : editor -> void` — Move to next cell, create new row at end
- `org-table-prev-cell : editor -> void` — Move to previous cell
- `org-table-next-row-same-column : editor -> void` — RET behavior
- `org-table-complete-separator : editor -> void` — `|-` TAB → full separator

### TAB/RET Integration

Modify `cmd-indent-or-complete` in `editor-ui.ss:313` — add before heading check:
```scheme
((org-table-row? trimmed)
 (org-table-next-cell ed)
 (org-table-align ed))
((string=? (string-trim trimmed) "|-")
 (org-table-complete-separator ed)
 (org-table-align ed))
```

Modify `cmd-newline` in `editor-core.ss:517` — add org-table branch:
```scheme
((and (org-buffer? buf) (org-table-on-table-line? ed))
 (org-table-next-row-same-column ed))
```

### Column/Row Operations (commands)

- `cmd-org-table-move-column-left/right` — Swap columns
- `cmd-org-table-insert-column` / `cmd-org-table-delete-column`
- `cmd-org-table-move-row-up/down` — Swap rows
- `cmd-org-table-insert-row` / `cmd-org-table-delete-row`
- `cmd-org-table-insert-separator` — Add `|---+---|` line
- `cmd-org-table-sort` — Sort by column (alpha/numeric/time)

### Formulas

- `org-table-parse-tblfm : string -> list-of-(target . formula)` — Parse `#+TBLFM:` line
- `org-table-eval-formula : formula rows col -> string` — Evaluate `$1+$2`, `@2$3` references
- `cmd-org-table-recalculate` — Recalculate formulas on `C-c C-c`

### CSV Import/Export

- `cmd-org-table-import-csv` — Read CSV file, convert to org table
- `cmd-org-table-export-csv` — Export table as CSV

### Tests (~20)

- Parsing: `org-table-row?`, `org-table-parse-row`, `org-table-column-widths`
- Alignment: unaligned → aligned, with separator, numeric right-align
- TAB in table: move to next cell, new row at end, `|-` TAB → separator
- RET in table: next row same column
- Column ops: insert, delete, move left/right
- Row ops: insert, delete, move up/down
- Sort: alpha, numeric
- Formula: simple `$1+$2`, cell ref `@2$3`
- CSV round-trip

### Files Modified
- `build.ss` — Add `"org-table"`
- `editor-ui.ss:313` — Add table branch to `cmd-indent-or-complete`
- `editor-core.ss:517` — Add table branch to `cmd-newline`
- `core.ss` — Add keybindings for table column/row operations
- `editor-extra.ss` — Register new table commands

---

## Phase 4: Org Babel (`org-babel.ss`)

### Source Block Parsing

- `org-babel-find-src-block : editor -> (values lang header-args body begin-line end-line name) or #f`
- `org-babel-parse-header-args : string -> hash-table` — Parse `:var x=5 :results output :dir /tmp`
- `org-babel-inside-src-block? : lines line-num -> boolean`
- `org-babel-find-named-block : text name -> string or #f`

### Language Executors

Table mapping language → execution config:
```scheme
(def *org-babel-lang-commands*
  '((bash    "/bin/bash" file)
    (sh      "/bin/sh" file)
    (python  "python3" file)
    (ruby    "ruby" file)
    (node    "node" file)
    (gerbil  "gxi" file)
    (scheme  "gxi" file)
    (c       org-babel-compile-c file)
    (sql     "sqlite3" stdin)))
```

- `org-babel-execute-code : symbol string hash -> string` — Execute code in language, handle `:dir`, `:var`
- `org-babel-inject-variables : symbol list -> string` — Generate var preamble per language (bash: `X='5'`, python: `x = 5`, gerbil: `(def x 5)`)
- `org-babel-compile-and-run-c : string hash -> string` — gcc + run + capture

### Result Handling

- `org-babel-insert-result : editor end-line string symbol -> void` — Insert/replace `#+RESULTS:` block. Result types: `output` (`: ` prefix per line), `value` (single line), `table` (org table format), `file` (`[[file:path]]`)
- `org-babel-format-output-result : string -> string` — Prefix each line with `: `

### C-c C-c Dispatcher

`cmd-org-ctrl-c-ctrl-c : app -> void` — Context-sensitive:
- On table row → `org-table-align` + recalculate formulas
- Inside src block → execute block
- On checkbox → toggle checkbox + update statistics
- On `#+CALL:` → execute named block
- Default → echo "no action"

### Sessions

- `*org-babel-sessions*` — Hash: `"lang:session-name"` → process port
- `org-babel-get-session : symbol string -> port` — Get or create persistent REPL
- `org-babel-send-to-session : port string -> string` — Send code, read until sentinel

### Noweb & Tangle

- `org-babel-expand-noweb : text hash -> string` — Expand `<<block-name>>` references (max depth 10)
- `cmd-org-babel-tangle : app -> void` — Extract `:tangle` blocks to files (`C-c C-v t`)

### Tests (~15)

- Block parsing: find src block, parse header args
- Execution: bash `echo hello`, python `print(2+3)` (requires interpreters)
- Result insertion: new results, replace existing results
- Variable injection: per-language format
- Noweb expansion
- C-c C-c dispatch: table context, src context, checkbox context
- Format output result

### Files Modified
- `build.ss` — Add `"org-babel"`
- `core.ss:537` — Add `C-c C-c` binding, `C-c C-v t` (tangle)
- `editor-extra.ss` — Register babel commands

---

## Phase 5: Clock Fix (`org-clock.ss`)

### Fix Elapsed Calculation

Replace hardcoded `"0:00"` with real SRFI-19 arithmetic:
```scheme
(def (org-clock-elapsed start-ts end-ts)
  (let* ((d1 (org-timestamp->date start-ts))
         (d2 (org-timestamp->date end-ts))
         (diff (time-difference (date->time-utc d2) (date->time-utc d1)))
         (secs (time-second diff))
         (h (quotient secs 3600))
         (m (quotient (remainder secs 3600) 60)))
    (string-append (number->string h) ":"
                   (if (< m 10) "0" "") (number->string m))))
```

### Use SRFI-19 Instead of Shelling Out to `date`

Replace `(open-process (list path: "date" ...))` with:
```scheme
(def (org-current-timestamp-string)
  (date->string (current-date) "[~Y-~m-~d ~a ~H:~M]"))
```

### Clock State

```scheme
(def *org-clock-start* #f)      ; org-timestamp or #f
(def *org-clock-heading* #f)    ; string
(def *org-clock-history* '())   ; list of (file . line)
```

### New/Fixed Commands

- `cmd-org-clock-in` — Fixed: use SRFI-19, store `*org-clock-start*`
- `cmd-org-clock-out` — Fixed: compute real elapsed time
- `cmd-org-clock-cancel` — Remove open CLOCK entry
- `cmd-org-clock-display` — Sum all CLOCK entries for current heading
- `cmd-org-clock-report` — Insert `#+BEGIN: clocktable` with per-heading totals

### Mode-line Integration

Add to `modeline-draw!` in `modeline.ss`: if `*org-clock-start*` set, show `[Clocked: H:MM]`.

### Tests (~8)

- Elapsed: 0:00, 1:30, across midnight (23:00→01:30 = 2:30)
- Clock-in creates correct LOGBOOK drawer
- Clock-out updates line with correct elapsed
- Clock-display sums multiple entries
- Clock-report generates valid table

### Files Modified
- `build.ss` — Add `"org-clock"`
- `editor-extra-org.ss` — Remove old clock functions, import `org-clock`
- `modeline.ss` — Add clock indicator

---

## Phase 6: List Handling (`org-list.ss`)

### Detection

- `org-list-item? : string -> (values type indent marker) or #f` — Detect `-`, `+`, `*`, `1.`, `1)`, checkboxes, description lists

### Operations

- `cmd-org-meta-return` (`M-RET`) — Context-sensitive: on heading → new heading; on list item → new item; on checkbox → new checkbox
- `cmd-org-cycle-list-bullet` (`C-c -`) — Cycle `-` → `+` → `*` → `1.` → `1)`
- `cmd-org-indent-list` / `cmd-org-dedent-list` — Change list indentation
- `org-renumber-list! : editor -> void` — Renumber ordered list after changes
- `org-update-checkbox-statistics! : editor -> void` — Update `[2/5]` and `[40%]` cookies in parent

### Tests (~10)

- List detection: all types
- M-RET context: heading, unordered, ordered, checkbox
- Bullet cycling
- Indent/dedent with renumbering
- Checkbox statistics update

### Files Modified
- `build.ss` — Add `"org-list"`
- `editor-core.ss` — Wire M-RET
- `core.ss` — Add `M-RET`, `C-c -` bindings

---

## Phase 7: Agenda (`org-agenda.ss`)

### Agenda Dispatcher

`cmd-org-agenda-dispatch` (`C-c C-a`) — Menu: `a` (daily/weekly), `t` (TODO list), `m` (tag match), `s` (search)

### Core Functions

- `org-collect-agenda-items : date date symbol -> list-of-org-agenda-item` — Scan `*org-agenda-files*` + open buffers (fixes current bug of only scanning disk)
- `org-agenda-sort-items : items -> items` — Sort by time, priority, alpha
- `org-format-agenda-day : date items -> string` — Format one day with time grid
- `org-format-agenda-buffer : date span type -> string` — Build full agenda buffer

### Views

- `cmd-org-agenda-day/week` — Daily/weekly view with time grid
- `cmd-org-agenda-todo` — All TODO items across files
- `cmd-org-agenda-match` — Filter by tag expression
- `cmd-org-agenda-search` — Full-text search

### Navigation

- `cmd-org-agenda-forward/backward` — Move by span days
- `cmd-org-agenda-goto-today` — Jump to today
- `cmd-org-agenda-goto` — Jump to source of item at point
- `cmd-org-agenda-todo-change` — Change TODO state from agenda
- `cmd-org-agenda-refresh` — Re-scan and redisplay

### Recurring Items

- `org-expand-recurring : heading date date -> list-of-items` — Generate occurrences for `+1w`, `.+1d`, `++1m` repeaters

### Stuck Projects

- `org-find-stuck-projects` — Headings with sub-headings but no NEXT/TODO children

### Tests (~12)

- Item collection from mock buffer text
- Date range filtering
- Sort order (time, priority)
- Day formatting with time grid
- TODO list view
- Tag matching
- Recurring item expansion
- Navigation forward/backward

### Files Modified
- `build.ss` — Add `"org-agenda"`
- `core.ss` — Add `C-c C-a` binding (avoid conflict with existing `C-c a`)

---

## Phase 8: Export (`org-export.ss`)

### Backend Architecture

```scheme
(defstruct org-export-backend
  (name file-extension
   heading-fn paragraph-fn bold-fn italic-fn code-fn
   link-fn src-block-fn table-fn list-item-fn checkbox-fn
   footnote-fn toc-fn preamble-fn postamble-fn)
  transparent: #t)
```

### Block Parser (shared by all backends)

- `org-split-into-blocks : string -> list-of-org-block` — Parse into heading/paragraph/src/table/list/drawer/keyword blocks
- `org-export-inline : string backend -> string` — Convert `*bold*`, `/italic/`, `=code=`, `[[link]]`, etc.

### Backends

**HTML** — Full `<!DOCTYPE html>` with embedded CSS, `<h1>`-`<h6>`, `<table>`, `<pre><code>`, TOC as nested `<ul>`, footnotes
**Markdown** — GFM: `#` headings, `**bold**`, `` `code` ``, fenced code blocks, pipe tables
**LaTeX** — `\documentclass{article}`, `\section`/`\subsection`, `\textbf`, `\begin{verbatim}`, TOC via `\tableofcontents`
**PDF** — LaTeX backend → `pdflatex` subprocess
**Plain text** — Enhanced version of current (strip all markup, preserve structure)

### Export Options Parser

- `org-parse-export-options : string -> hash-table` — Parse `#+TITLE:`, `#+AUTHOR:`, `#+OPTIONS: toc:t num:t H:3`
- `org-export-buffer : string symbol boolean -> string` — Main export: parse options, split blocks, apply backend, wrap with preamble/postamble

### Commands

- `cmd-org-export-dispatch` (`C-c C-e`) — Menu: `h` HTML, `m` Markdown, `l` LaTeX, `p` PDF, `t` text
- `cmd-org-export-html/markdown/latex/pdf/text` — Individual export commands

### Tests (~12)

- Inline markup: bold → `<b>`, `**`, `\textbf{}`
- Link conversion per backend
- HTML escape (`<script>` → `&lt;script&gt;`)
- Heading levels per backend
- Code block rendering
- Table rendering
- TOC generation
- Full document export (small org → HTML/MD)

### Files Modified
- `build.ss` — Add `"org-export"`
- `core.ss` — Add `C-c C-e` binding (rebind or use org-keymap)

---

## Phase 9: Capture & Refile (`org-capture.ss`)

### Capture Templates

```scheme
(defstruct org-capture-template
  (key description type target template)
  transparent: #t)

(def *org-capture-templates*
  (list
    (make-org-capture-template key: "t" description: "TODO"
      type: 'entry target: '(file+headline "~/org/inbox.org" "Tasks")
      template: "* TODO %?\n  %U\n")
    (make-org-capture-template key: "n" description: "Note"
      type: 'entry target: '(file "~/org/notes.org")
      template: "* %?\n  %U\n")))
```

### Template Expansion

`org-capture-expand-template : string -> string` — Replace `%?` (cursor), `%U` (inactive timestamp), `%T` (active timestamp), `%a` (stored link), `%f` (filename), `%^{prompt}` (user input)

### Commands

- `cmd-org-capture` (`C-c c`) — Template menu, expand, create `*Org Capture*` buffer
- `cmd-org-capture-finalize` — `C-c C-c` in capture buffer: insert at target, kill capture buf
- `cmd-org-capture-abort` — `C-c C-k`: discard
- `cmd-org-refile` (`C-c C-w`) — Move heading to target with completion

### Tests (~6)

- Template expansion: `%U`, `%T`, `%f`
- Capture finalize: text inserted at target
- Refile: subtree moved correctly

---

## Phase 10: Org Keymap & Remaining Features

### Buffer-Local Keymap

Add `*org-keymap*` active only in org buffers. Modify key dispatch in `keymap.ss` to check buffer-local keymap before global.

Move org bindings from `*ctrl-c-map*` to `*org-keymap*` so they don't pollute non-org buffers.

### Speed Commands

At column 0 of heading line, single keys become commands: `t` (todo), `n` (next heading), `p` (prev), `u` (up), `:` (tags)

### Footnotes

- `cmd-org-footnote-action` (`C-c C-x f`) — Jump to def/ref, or create new
- `org-footnote-renumber!` — Sequential renumbering

### Drawers

- Drawer folding via fold levels
- `cmd-org-set-property` — Add/edit property in `:PROPERTIES:` drawer

### Archive

- `cmd-org-archive-subtree` (`C-c C-x C-a`) — Move to `filename.org_archive` with metadata

### Org Indent Mode

- `cmd-org-indent-mode` — Virtual indentation via `SCI_SETLINEINDENTATION` (non-destructive)

### Custom TODO Keywords

- Parse `#+TODO: TODO NEXT | DONE CANCELLED` from buffer settings
- Modify `cmd-org-todo` to cycle through custom sequence

### Sort Entries

- `cmd-org-sort-entries` — Sort headings by alpha/numeric/priority/time/todo

### Goto

- `cmd-org-goto` (`C-c C-j`) — Jump to heading with completion

---

## Keybinding Summary

New bindings (in `*org-keymap*` or `*ctrl-c-map*`):

| Key | Command | Phase |
|-----|---------|-------|
| `C-c C-c` | `org-ctrl-c-ctrl-c` (context dispatch) | 4 |
| `C-c C-a` | `org-agenda-dispatch` | 7 |
| `C-c C-e` | `org-export-dispatch` | 8 |
| `C-c C-w` | `org-refile` | 9 |
| `C-c C-x C-i` | `org-clock-in` | 5 |
| `C-c C-x C-o` | `org-clock-out` | 5 |
| `C-c C-x C-a` | `org-archive-subtree` | 10 |
| `C-c C-v t` | `org-babel-tangle` | 4 |
| `C-c -` | `org-cycle-list-bullet` | 6 |
| `C-c C-j` | `org-goto` | 10 |
| `M-RET` | `org-meta-return` | 6 |
| `M-S-RET` | `org-insert-todo-heading` | 6 |
| TAB (in table) | table next-cell + align | 3 |
| RET (in table) | table next-row-same-column | 3 |

---

## Verification Strategy

### After Each Phase

1. **Build**: `make build` (or `HOME=/home/jafourni GERBIL_LOADPATH=/home/jafourni/.gerbil/lib gerbil build`)
2. **Test**: `HOME=/home/jafourni LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/opt/openssl@3/lib GERBIL_LOADPATH=/home/jafourni/.gerbil/lib timeout 120 gerbil test`
3. **Both binaries**: `.gerbil/bin/gerbil-emacs --version` and `QT_QPA_PLATFORM=offscreen .gerbil/bin/gerbil-emacs-qt --version`
4. **MCP tools**: `gerbil_verify` on each new `.ss` file, `gerbil_check_balance` on large files

### Test Coverage Target

~120 total new tests across all phases:
- Phase 1 (parser): ~25 pure function tests
- Phase 2 (highlight): ~10 style verification tests
- Phase 3 (tables): ~20 headless editor tests
- Phase 4 (babel): ~15 headless + subprocess tests
- Phase 5 (clock): ~8 calculation tests
- Phase 6 (lists): ~10 headless tests
- Phase 7 (agenda): ~12 mixed tests
- Phase 8 (export): ~12 string comparison tests
- Phase 9 (capture): ~6 headless tests
- Phase 10 (keymap/misc): ~5 tests

### Test Pattern (headless)

```scheme
(test-case "headless: org-table-align basic"
  (let* ((ed (create-scintilla-editor width: 80 height: 24))
         (buf (make-buffer "test.org" "/tmp/test.org"
                (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
         (win (make-edit-window ed buf 0 0 80 24 0))
         (fr (make-frame [win] 0 80 24 'vertical))
         (app (new-app-state fr)))
    (editor-set-text ed "| a | bb |\n| ccc | d |")
    (editor-goto-pos ed 2)
    (org-table-align ed)
    (check (editor-get-text ed) => "| a   | bb |\n| ccc | d  |")))
```

---

## Implementation Order Summary

| Phase | Module | Key Deliverable | Depends On |
|-------|--------|----------------|------------|
| 1 | `org-parse.ss` | Timestamp/heading parser, buffer settings | — |
| 2 | `org-highlight.ss` | Syntax highlighting (visual impact) | — |
| 3 | `org-table.ss` | TAB alignment, cell navigation, formulas | — |
| 4 | `org-babel.ss` | Code execution, C-c C-c dispatch | Phase 3 (C-c C-c) |
| 5 | `org-clock.ss` | Fix elapsed time, clock reports | Phase 1 (timestamps) |
| 6 | `org-list.ss` | M-RET, checkbox stats, bullet cycling | — |
| 7 | `org-agenda.ss` | Daily/weekly views, TODO list, filtering | Phase 1 (parser) |
| 8 | `org-export.ss` | HTML, Markdown, LaTeX, PDF export | Phase 1 (parser) |
| 9 | `org-capture.ss` | Capture templates, refile | Phase 1 (parser) |
| 10 | Keymap + misc | Buffer-local keymap, footnotes, archive | Phases 1-9 |

Phases 1-3 can be done in parallel (independent). Phases 4-6 can follow. Phases 7-10 depend on Phase 1.
