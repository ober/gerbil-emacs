# Gemacs Enhancement Plan: Themes, Fonts, and Org-Mode Tables

## Part 1: Base16 Theme System for Qt

### Current State

- 4 hardcoded themes in `qt/commands-core.ss:146-180`: `dark`, `solarized-dark`, `light`, `monokai`
- Themes only control **UI chrome** (bg, fg, selection, modeline, gutter, tabs, split colors)
- **Syntax highlighting is NOT themed** — colors are hardcoded as bare `(def kw-r #xcc)` etc. in `qt/highlight.ss:31-48`
- Org-mode highlighting colors hardcoded separately in `org-highlight.ss:62-86`
- `apply-theme!` in `qt/commands-core.ss:205-229` updates Qt stylesheet + gutter but does NOT touch Scintilla styles
- Color conversion: `rgb->sci` in `qt/sci-shim.ss:111-113` — Scintilla uses BGR format

### Goal

Full base16 theme integration where a single theme selection controls **everything**: UI chrome, syntax highlighting (all languages), org-mode faces, current-line/brace-match indicators, search highlights, and mode-line.

### Base16 Color Slots

Base16 defines 16 colors with standard semantic roles:

| Slot    | Dark Theme Role           | Light Theme Role          |
|---------|---------------------------|---------------------------|
| base00  | Background                | Background                |
| base01  | Lighter bg (gutter, line) | Darker bg                 |
| base02  | Selection bg              | Selection bg              |
| base03  | Comments, invisibles      | Comments, invisibles      |
| base04  | Dark foreground           | Light foreground          |
| base05  | Default foreground        | Default foreground        |
| base06  | Light foreground          | Dark foreground           |
| base07  | Lightest foreground       | Darkest foreground        |
| base08  | Red (variables, errors)   | Red                       |
| base09  | Orange (numbers, const)   | Orange                    |
| base0A  | Yellow (types, warnings)  | Yellow                    |
| base0B  | Green (strings)           | Green                     |
| base0C  | Cyan (builtins, regex)    | Cyan                      |
| base0D  | Blue (functions, links)   | Blue                      |
| base0E  | Purple (keywords)         | Purple                    |
| base0F  | Brown (embedded, escape)  | Brown                     |

### Implementation Steps

#### Step 1.1: Create `qt/theme.ss` — Theme Data Model

New file (~200 lines). Defines:

```scheme
;; Base16 palette struct
(defstruct base16-palette (base00 base01 base02 base03 base04 base05 base06 base07
                           base08 base09 base0A base0B base0C base0D base0E base0F))

;; Semantic color mapping derived from a palette
(defstruct theme-colors
  ;; UI chrome
  (bg fg selection modeline-bg modeline-fg echo-bg echo-fg
   gutter-bg gutter-fg split tab-bg tab-border tab-active-bg tab-active-fg
   tab-inactive-bg tab-inactive-fg current-line-bg brace-match-bg brace-match-fg
   ;; Syntax
   keyword builtin string comment number operator heading
   preprocessor type function-name error-face
   ;; Org
   org-heading-1 org-heading-2 org-heading-3 org-heading-4
   org-heading-5 org-heading-6 org-heading-7 org-heading-8
   org-todo org-done org-link org-code org-tag org-date org-table
   ;; Search
   search-match-bg search-match-fg))
```

Functions:
- `(base16-palette->theme-colors palette)` — maps 16 base colors to all semantic roles
- `(theme-colors->stylesheet colors font-family font-size)` — generates Qt CSS from theme
- `(hex->rgb hex)` / `(rgb->sci r g b)` — color conversion utilities (move from sci-shim)

#### Step 1.2: Create `qt/themes/` directory with built-in palettes

Each theme is a simple Scheme file exporting a `base16-palette` struct. Start with ~10 popular themes:

- `default-dark.ss` — base16-default-dark (current hardcoded colors, migrated)
- `default-light.ss` — base16-default-light
- `solarized-dark.ss` — base16-solarized-dark
- `solarized-light.ss` — base16-solarized-light
- `monokai.ss` — base16-monokai
- `gruvbox-dark-hard.ss` — base16-gruvbox-dark-hard
- `dracula.ss` — base16-dracula
- `nord.ss` — base16-nord
- `one-dark.ss` — base16-onedark
- `tokyo-night.ss` — base16-tokyo-night

Each file is ~20 lines — just the 16 hex values. The base16 palette definitions are public domain from https://github.com/tinted-theming/schemes.

#### Step 1.3: Refactor `qt/highlight.ss` — Use theme colors

Replace all hardcoded `(def kw-r ...)` color constants at the top of `qt/highlight.ss` with lookups from the current `theme-colors` struct.

**Before** (current):
```scheme
(def kw-r #xcc) (def kw-g #x99) (def kw-b #xcc)
```

**After**:
```scheme
(def (current-keyword-color) (theme-colors-keyword (current-theme-colors)))
```

Every `setup-*-styles!` function (Lisp, C++, Python, etc.) reads colors from the theme instead of module-level constants. This affects `qt/highlight.ss` lines 100-460+ (all per-language style setup functions).

#### Step 1.4: Refactor `org-highlight.ss` — Use theme colors

Replace hardcoded org-mode colors with theme lookups. The style IDs (33-62) remain fixed, but their foreground/background values come from the theme's org-specific color slots.

#### Step 1.5: Refactor `qt/commands-core.ss` — Migrate `define-theme!` / `apply-theme!`

- Remove the old `define-theme!` / `*themes*` hash table system (lines 125-180)
- Replace `apply-theme!` to:
  1. Look up the named base16 palette
  2. Derive `theme-colors` from it
  3. Apply Qt stylesheet (UI chrome)
  4. Re-apply Scintilla styles to all open editors (syntax highlighting)
  5. Re-apply org-mode styles to any `.org` buffers
  6. Update gutter, current-line, brace-match indicators
- Keep backward-compatible theme names: `'dark` → `'default-dark`, `'monokai` → `'monokai`, etc.

#### Step 1.6: Theme Switching Command

In `qt/commands-core.ss` or `qt/commands-config.ss`:

```scheme
(def (cmd-load-theme app)
  ;; Prompt with completing-read from available theme names
  ;; Apply theme + persist choice
  )
```

Register as `'load-theme` command. Bind to `M-x load-theme`.

Available themes discovered by listing registered palettes (no filesystem scan needed — they're all compiled in).

#### Step 1.7: Theme Persistence

In `persist.ss` or `.gemacs-config`:
- Save `*current-theme*` name on `cmd-load-theme`
- Load on startup in `qt/app.ss` initialization

#### Step 1.8: User Custom Themes

Support loading base16 themes from `~/.gemacs-themes/` as JSON files:
```json
{
  "name": "my-custom",
  "base00": "#1d1f21",
  "base01": "#282a2e",
  ...
}
```

Parse at startup and merge into available theme list.

### Files Modified

| File | Changes |
|------|---------|
| `qt/theme.ss` | **NEW** — theme data model, palette→colors mapping |
| `qt/themes/*.ss` | **NEW** — ~10 built-in base16 palettes |
| `qt/highlight.ss` | Refactor all color constants to use theme lookups |
| `org-highlight.ss` | Refactor org colors to use theme lookups |
| `qt/commands-core.ss` | Replace old theme system, new `apply-theme!` |
| `qt/commands-config.ss` | Add `cmd-load-theme` command |
| `qt/commands.ss` | Register `'load-theme` command |
| `qt/app.ss` | Load persisted theme on startup |
| `persist.ss` | Save/load theme name |
| `build.ss` | Add `qt/theme.ss` and `qt/themes/*.ss` to compilation |

### Testing

- Add tests to `qt-functional-test.ss`:
  - `(execute-command! app 'load-theme)` with each built-in theme name
  - Verify editor text is still readable after theme switch
  - Verify theme round-trips through persistence

---

## Part 2: Font Selection and Size

### Current State

- Font face hardcoded to `"Monospace"` in `qt/window.ss:72` via `SCI_STYLESETFONT`
- Font size set to 11pt in `qt/window.ss:73` via `SCI_STYLESETSIZE`
- Qt stylesheet hardcodes `font-family: monospace; font-size: 10pt;` in `qt/commands-core.ss:194-202`
- Zoom commands exist (`cmd-increase-font-size`, `cmd-decrease-font-size`, `cmd-reset-font-size` in `qt/commands-shell.ss:38-59`) but don't update Scintilla styles
- Zoom also in `qt/commands-edit.ss:880-892` using `qt-widget-set-font-size!`
- `*font-size*` global in `qt/commands-shell.ss:38` defaults to 10
- **No font family selection**
- **No persistence** — resets on restart
- **Inconsistent sizes**: Scintilla uses 11pt, Qt widgets use 10pt

### Goal

Emacs-like font configuration: `set-frame-font` equivalent with family/size selection, persistent across sessions, applied consistently to all UI elements.

### Implementation Steps

#### Step 2.1: Font State in `qt/commands-core.ss` or `qt/theme.ss`

```scheme
(def *font-family* "Monospace")
(def *font-size* 11)
```

Single source of truth. Both Scintilla and Qt stylesheet read from these.

#### Step 2.2: `cmd-set-frame-font` — Font Family Selection

```scheme
(def (cmd-set-frame-font app)
  ;; Show completing-read with available monospace fonts
  ;; On selection, update *font-family* and apply to all editors
  )
```

Get available fonts from Qt via a new FFI helper in `gerbil-qt` (or enumerate common monospace fonts as a static list: "Monospace", "DejaVu Sans Mono", "Fira Code", "JetBrains Mono", "Source Code Pro", "Hack", "Inconsolata", "Cascadia Code", "IBM Plex Mono", "Ubuntu Mono").

Apply font to:
1. All Scintilla editors: `SCI_STYLESETFONT` on `STYLE_DEFAULT` then `SCI_STYLECLEARALL` + re-apply syntax styles
2. Qt stylesheet: regenerate with new `font-family`
3. Tab bar, echo area, status bar

#### Step 2.3: `cmd-set-font-size` — Size with Prompt

```scheme
(def (cmd-set-font-size app)
  ;; Read numeric size from minibuffer (default: current size)
  ;; Apply to all editors + UI
  )
```

#### Step 2.4: Fix Zoom Commands

Unify the two zoom implementations (`qt/commands-shell.ss:38-59` and `qt/commands-edit.ss:880-892`) into one consistent system:

- `cmd-increase-font-size` / `cmd-decrease-font-size` / `cmd-reset-font-size` all go through the central `*font-size*` and apply to both Scintilla (`SCI_STYLESETSIZE` on all styles) and Qt widgets
- Remove duplicate zoom code

#### Step 2.5: Apply Font Uniformly

Create `(apply-font! app)` that:
1. Sets `SCI_STYLESETFONT` + `SCI_STYLESETSIZE` on `STYLE_DEFAULT` for every open Scintilla editor
2. Calls `SCI_STYLECLEARALL` to propagate
3. Re-applies syntax highlighting styles (since `STYLECLEARALL` resets them)
4. Regenerates Qt stylesheet with current font family/size
5. Applies stylesheet to app

This is called by `cmd-set-frame-font`, `cmd-set-font-size`, and zoom commands.

#### Step 2.6: Font Persistence

Add to `.gemacs-config` (or persist.ss):
```
font-family=JetBrains Mono
font-size=12
```

Load on startup before creating editors.

#### Step 2.7: `cmd-describe-font` — Show Current Font Info

```scheme
(def (cmd-describe-font app)
  ;; Display in echo area: "Font: JetBrains Mono 12pt"
  )
```

### Files Modified

| File | Changes |
|------|---------|
| `qt/commands-core.ss` | Add `*font-family*`, `*font-size*`, `apply-font!` |
| `qt/commands-config.ss` | Add `cmd-set-frame-font`, `cmd-set-font-size`, `cmd-describe-font` |
| `qt/commands-shell.ss` | Refactor zoom commands to use central `apply-font!` |
| `qt/commands-edit.ss` | Remove duplicate zoom code, delegate to central |
| `qt/commands.ss` | Register new font commands |
| `qt/window.ss` | Read from `*font-family*`/`*font-size*` instead of hardcoded |
| `qt/app.ss` | Load font prefs on startup |
| `persist.ss` | Save/load font-family and font-size |
| `build.ss` | No changes (existing files) |

### Key Bindings

| Binding | Command | Notes |
|---------|---------|-------|
| `C-x C-+` / `C-+` | `increase-font-size` | Already bound |
| `C-x C--` / `C--` | `decrease-font-size` | Already bound |
| `C-x C-0` / `C-0` | `reset-font-size` | Already bound |
| `M-x set-frame-font` | `set-frame-font` | New — completing-read |
| `M-x set-font-size` | `set-font-size` | New — numeric input |

### Testing

- `qt-functional-test.ss`: Verify zoom commands update `*font-size*`
- Verify font persistence round-trips

---

## Part 3: Missing Org-Mode Table Operations

### Current State

#### `org-table.ss` (TUI backend-agnostic logic, 677 lines) — RICH

Already implements all core table operations:
- Detection: `org-table-row?`, `org-table-separator?`, `org-table-on-table-line?`
- Parsing: `org-table-parse-row`, `org-table-find-bounds`, `org-table-get-rows`
- Alignment: `org-table-align`, `org-table-format-row`, `org-table-format-separator`
- Navigation: `org-table-next-cell`, `org-table-prev-cell`, `org-table-next-row-same-column`
- Column ops: `org-table-move-column`, `org-table-insert-column`, `org-table-delete-column`
- Row ops: `org-table-move-row`, `org-table-insert-row`, `org-table-delete-row`
- Separator: `org-table-insert-separator-line`, `org-table-complete-separator`
- Sort: `org-table-sort` (by column, numeric or alpha)
- Formulas: `org-table-parse-tblfm`, `org-table-eval-formula`, `org-table-recalculate`
- CSV: `org-csv-to-table`, `org-table-to-csv`

#### Qt commands registered — SPARSE

Only **1** org-table command registered in Qt:
- `org-table-delete-column` (in `qt/commands-modes.ss:1015`)

`qt-org-table-next-cell` exists in `qt/commands.ss:643` but is called internally by TAB dispatch, not as a registered command.

#### TUI commands registered — ALSO SPARSE

Only **1** org-table command registered in TUI:
- `org-table-create` (in `editor-extra.ss:33`)

### The Core Problem

The pure-logic `org-table.ss` has everything, but the command layer (both TUI and Qt) only exposes a tiny fraction. The Qt side also duplicates some table logic (parsing, formatting) in `qt/commands.ss:572-750` instead of reusing `org-table.ss`.

### Implementation Plan

#### Step 3.1: Create Qt wrapper commands for all org-table operations

In `qt/commands-modes.ss` (or a new `qt/commands-org.ss` if space is tight), create `cmd-*` wrappers that bridge Qt's `app-state` to `org-table.ss` functions:

```scheme
;; Each cmd-* gets the Scintilla editor from app-state and calls the org-table.ss function

(def (cmd-org-table-align app) ...)
(def (cmd-org-table-next-cell app) ...)
(def (cmd-org-table-prev-cell app) ...)
(def (cmd-org-table-next-row app) ...)
(def (cmd-org-table-sort-alpha app) ...)
(def (cmd-org-table-sort-numeric app) ...)
(def (cmd-org-table-move-column-left app) ...)
(def (cmd-org-table-move-column-right app) ...)
(def (cmd-org-table-insert-column app) ...)
(def (cmd-org-table-move-row-up app) ...)
(def (cmd-org-table-move-row-down app) ...)
(def (cmd-org-table-insert-row app) ...)
(def (cmd-org-table-delete-row app) ...)
(def (cmd-org-table-insert-separator app) ...)
(def (cmd-org-table-recalculate app) ...)
(def (cmd-org-table-export-csv app) ...)
(def (cmd-org-table-import-csv app) ...)
(def (cmd-org-table-create app) ...)
```

**Challenge**: `org-table.ss` uses TUI Scintilla API (`send-message`, `editor-get-line`, etc.) directly. The Qt side uses `sci-send` from `qt/sci-shim.ss`. These are different FFI layers for the same Scintilla messages.

**Solution options**:

A. **Adapter approach**: Write Qt cmd-* wrappers that extract the Scintilla widget pointer, then call org-table.ss functions. This works if `org-table.ss` functions accept a generic "editor" that is a Scintilla widget handle in both TUI and Qt.

B. **Rewrite approach**: Port the org-table logic to work with Qt's sci-send. This is what the existing `qt-org-table-next-cell` does — it reimplements the logic.

C. **Abstraction approach**: Create an editor protocol (get-line, set-line, get-pos, etc.) that both TUI and Qt implement, and have org-table.ss use it.

**Recommendation**: Option A is fastest. The `ed` parameter in org-table.ss is already a Scintilla editor handle. In TUI it's a `tui-editor` struct; in Qt it's a widget pointer from `qt-scintilla-create`. Both respond to the same SCI_* messages, just through different FFI wrappers (`send-message` vs `sci-send`). Check if the existing `org-table.ss` functions can work with Qt's editor handles directly. If they use `:gerbil-scintilla/scintilla` `send-message`, they won't work with Qt's `sci-send`.

**Most likely approach**: Option B — create Qt-specific wrappers in `qt/commands-modes.ss` that reuse the pure parsing/formatting helpers from `org-table.ss` but use `sci-send` for Scintilla communication. The pure functions (`org-table-parse-row`, `org-table-format-row`, `org-table-column-widths`, etc.) are already backend-agnostic — only the functions that call `send-message` / `editor-*` need Qt versions.

#### Step 3.2: Register all new commands

In `qt/commands.ss` `qt-register-all-commands!`:

```scheme
(register-command! 'org-table-align cmd-org-table-align)
(register-command! 'org-table-next-cell cmd-org-table-next-cell)
(register-command! 'org-table-prev-cell cmd-org-table-prev-cell)
(register-command! 'org-table-next-row cmd-org-table-next-row)
(register-command! 'org-table-sort-alpha cmd-org-table-sort-alpha)
(register-command! 'org-table-sort-numeric cmd-org-table-sort-numeric)
(register-command! 'org-table-move-column-left cmd-org-table-move-column-left)
(register-command! 'org-table-move-column-right cmd-org-table-move-column-right)
(register-command! 'org-table-insert-column cmd-org-table-insert-column)
(register-command! 'org-table-move-row-up cmd-org-table-move-row-up)
(register-command! 'org-table-move-row-down cmd-org-table-move-row-down)
(register-command! 'org-table-insert-row cmd-org-table-insert-row)
(register-command! 'org-table-delete-row cmd-org-table-delete-row)
(register-command! 'org-table-insert-separator cmd-org-table-insert-separator)
(register-command! 'org-table-recalculate cmd-org-table-recalculate)
(register-command! 'org-table-export-csv cmd-org-table-export-csv)
(register-command! 'org-table-import-csv cmd-org-table-import-csv)
(register-command! 'org-table-create cmd-org-table-create)
```

#### Step 3.3: Add key bindings (Emacs-compatible)

Org-table bindings active only in org-mode buffers:

| Binding | Command | Emacs Equivalent |
|---------|---------|------------------|
| `TAB` | `org-table-next-cell` | Already works (in TAB dispatch) |
| `S-TAB` | `org-table-prev-cell` | `org-table-previous-field` |
| `RET` | `org-table-next-row` | `org-table-next-row` |
| `C-c -` | `org-table-insert-separator` | `org-table-insert-hline` |
| `C-c ^` | `org-table-sort-alpha` | `org-table-sort-lines` (prompts) |
| `M-Left` | `org-table-move-column-left` | `org-table-move-column-left` |
| `M-Right` | `org-table-move-column-right` | `org-table-move-column-right` |
| `M-Up` | `org-table-move-row-up` | `org-table-move-row-up` |
| `M-Down` | `org-table-move-row-down` | `org-table-move-row-down` |
| `M-S-Left` | `org-table-delete-column` | `org-table-delete-column` |
| `M-S-Right` | `org-table-insert-column` | `org-table-insert-column` |
| `M-S-Up` | `org-table-delete-row` | `org-table-kill-row` |
| `M-S-Down` | `org-table-insert-row` | `org-table-insert-row` |
| `C-c *` | `org-table-recalculate` | `org-table-recalculate` |
| `C-c \|` | `org-table-create` | `org-table-create-or-convert` |
| `C-c +` | `org-table-sum` | `org-table-sum` (new) |

#### Step 3.4: Sort prompt enhancement

The current `org-table-sort` in `org-table.ss:452` takes `col-num` and `numeric?` params. Create a user-facing command that:

1. Defaults to current column
2. Prompts "Sort type: [a]lpha [n]umeric [A]lpha-reverse [N]umeric-reverse"
3. Supports reverse sort (add `reverse?` parameter to `org-table-sort`)

#### Step 3.5: Additional org-table features to implement

##### 3.5a: `org-table-sum` — Sum current column

New function in `org-table.ss`:
```scheme
(def (org-table-sum-column ed)
  ;; Sum numeric values in current column, display in echo area
  ;; Emacs shows "Sum of N items: VALUE" and copies to kill ring
  )
```

##### 3.5b: `org-table-transpose` — Swap rows↔columns

New function in `org-table.ss`:
```scheme
(def (org-table-transpose ed)
  ;; Transpose the table: rows become columns, columns become rows
  ;; Separators are discarded in the transposition
  )
```

##### 3.5c: `org-table-copy-field` / `org-table-blank-field`

```scheme
(def (org-table-copy-field ed)
  ;; Copy current cell value to kill ring
  )

(def (org-table-blank-field ed)
  ;; Clear current cell contents (keep structure)
  )
```

##### 3.5d: Enhanced formula support

Current formula system is basic. Add:
- `vsum($1..$3)` — sum of column range
- `vmean($1..$3)` — mean
- `vmin`, `vmax`
- `@N$M` — cell reference (row N, column M)
- Cell-level formulas (not just column-level)

##### 3.5e: `org-table-convert-region` — Convert region to table

If a region is selected containing CSV, TSV, or space-separated data, convert it to an org table. This is what `C-c |` does in Emacs when a region is active.

### Files Modified

| File | Changes |
|------|---------|
| `org-table.ss` | Add `org-table-sum-column`, `org-table-transpose`, `org-table-copy-field`, `org-table-blank-field`, reverse sort, enhanced formulas |
| `qt/commands-modes.ss` | Add all `cmd-org-table-*` wrappers |
| `qt/commands.ss` | Register all new org-table commands, add org-table keybindings |
| `functional-test.ss` | Add dispatch-chain tests for new TUI table commands |
| `qt-functional-test.ss` | Add dispatch-chain tests for Qt table commands |

---

## Part 4: Missing Org-Mode Commands (Qt)

### Commands in TUI but Missing from Qt

The TUI registers these org commands that Qt does not:

| TUI Command | Status in Qt | Priority |
|-------------|-------------|----------|
| `org-priority` | Missing — TUI has `cmd-org-priority` for `[#A]`/`[#B]`/`[#C]` cycling | High |
| `org-schedule` | Missing — TUI has `cmd-org-schedule` | High |
| `org-deadline` | Missing — TUI has `cmd-org-deadline` | High |
| `org-agenda` | Missing — TUI has `cmd-org-agenda` | High |
| `org-export` | Missing — TUI has `cmd-org-export` | Medium |
| `org-table-create` | Missing — TUI has `cmd-org-table-create` | High |
| `org-link` | Missing — TUI has `cmd-org-link` | Medium |
| `org-store-link` | Missing — TUI has `cmd-org-store-link` | Medium |
| `org-open-at-point` | Missing — TUI has `cmd-org-open-at-point` | High |
| `org-shift-tab` | Missing — TUI has `cmd-org-shift-tab` (global cycle) | High |
| `org-set-tags` | Missing — TUI has `cmd-org-set-tags` | Medium |
| `org-insert-src-block` | Missing — TUI has `cmd-org-insert-src-block` | Medium |
| `org-clock-in` | Missing — TUI has `cmd-org-clock-in` | Medium |
| `org-clock-out` | Missing — TUI has `cmd-org-clock-out` | Medium |

### Implementation Approach

Port each TUI `cmd-org-*` from `editor-extra-org.ss` to a Qt equivalent in `qt/commands-modes.ss`. The pure org logic modules (`org-parse.ss`, `org-clock.ss`, `org-agenda.ss`, etc.) are backend-agnostic — the Qt wrappers just need to:

1. Get the Scintilla editor widget from `app-state`
2. Get/set text via `sci-send` instead of `send-message`
3. Show results in echo area or new buffer

### Key Bindings

| Binding | Command | Notes |
|---------|---------|-------|
| `C-c C-t` | `org-todo-cycle` | Already bound |
| `C-c C-s` | `org-schedule` | New |
| `C-c C-d` | `org-deadline` | New |
| `C-c ,` or `C-c C-p` | `org-priority` | New |
| `C-c C-q` | `org-set-tags` | New |
| `C-c C-a` | `org-agenda` | New |
| `C-c C-e` | `org-export` | New |
| `C-c C-o` | `org-open-at-point` | New |
| `C-c C-l` | `org-link` | New |
| `C-c l` | `org-store-link` | New |
| `C-c C-x C-i` | `org-clock-in` | New |
| `C-c C-x C-o` | `org-clock-out` | New |
| `S-TAB` | `org-shift-tab` | New |

### Files Modified

| File | Changes |
|------|---------|
| `qt/commands-modes.ss` | Add all missing `cmd-org-*` wrappers |
| `qt/commands.ss` | Register commands + key bindings |
| `qt-functional-test.ss` | Add tests for new org commands |

---

## Part 5: Additional Org-Mode Enhancements (Stretch Goals)

These are lower priority but would bring gemacs org-mode closer to Emacs:

### 5.1: Org Sparse Tree (`org-sparse-tree` / `org-occur`)

Search org buffer for matching headings/TODOs and fold everything else. Display matching entries in a narrowed view.

### 5.2: Org Column View

Display property values in a tabular overlay on headings. Requires:
- Property parsing (already in `org-parse.ss`)
- Column format spec parsing (`#+COLUMNS:`)
- Overlay rendering in Scintilla (challenging)

### 5.3: Org Archive

Move DONE items to `filename.org_archive` or `archive.org`:
```scheme
(def (cmd-org-archive-subtree app) ...)
```

### 5.4: Org Footnotes

Parse `[fn:1]` and `[fn:name]` footnote references and definitions. Add navigation and creation commands.

### 5.5: Speed Keys

When cursor is at the beginning of a heading, single-letter shortcuts activate without modifiers (t=todo, p=priority, etc.).

### 5.6: Org Refile (Qt)

Already implemented in `org-capture.ss` as `org-refile-targets` / `org-extract-subtree` / `org-insert-under-heading`. Needs Qt command wrapper with completing-read for target selection.

### 5.7: Custom TODO Keywords

Support `#+TODO: TODO IN-PROGRESS WAITING | DONE CANCELLED` in file headers. Parse on buffer load, update `*org-todo-keywords*` per-buffer.

---

## Part 6: Gerbil-LSP Integration Tests

### Current State

The LSP client is implemented across two files:
- `qt/lsp-client.ss` (560 lines) — JSON-RPC transport, process management, document sync, initialization handshake
- `qt/commands-lsp.ss` (1006 lines) — 15 registered commands, diagnostics display, workspace edit application, hooks

**16 registered commands:**
1. `toggle-lsp` — Start/stop LSP server
2. `lsp-restart` — Stop + restart
3. `lsp-stop` — Stop server
4. `lsp-goto-definition` — `textDocument/definition`
5. `lsp-declaration` — `textDocument/declaration`
6. `lsp-type-definition` — `textDocument/typeDefinition`
7. `lsp-implementation` — `textDocument/implementation`
8. `lsp-hover` — `textDocument/hover`
9. `lsp-completion` — `textDocument/completion`
10. `lsp-rename` — `textDocument/rename`
11. `lsp-code-actions` — `textDocument/codeAction`
12. `lsp-find-references` — `textDocument/references`
13. `lsp-document-symbols` — `textDocument/documentSymbol`
14. `lsp-workspace-symbol` — `workspace/symbol`
15. `lsp-format-buffer` — `textDocument/formatting`
16. `lsp-smart-goto-definition` — LSP if running, else text search fallback

**Non-command LSP features (called from timers/hooks):**
- `lsp-eldoc-display!` — `textDocument/signatureHelp` (eldoc timer)
- `lsp-document-highlight!` — `textDocument/documentHighlight` (cursor-idle timer)
- `lsp-hook-did-open!` / `lsp-hook-did-change!` / `lsp-hook-did-save!` / `lsp-hook-did-close!` — document sync hooks
- `lsp-maybe-auto-start!` — auto-start when opening .ss/.scm files
- Diagnostics via `textDocument/publishDiagnostics` notification → Scintilla indicators

**Current test coverage: ZERO.** No LSP tests exist anywhere in the test suite.

### Known Bug: `toggle-lsp` First Start

When `toggle-lsp` is invoked for the first time (not via `lsp-restart`):
1. `lsp-start!` launches `gerbil-lsp --stdio` asynchronously
2. `lsp-install-handlers!` installs the `*lsp-on-initialized-handler*` callback
3. The initialize handshake completes asynchronously, firing the callback
4. The callback sends `didOpen` for all open buffers
5. Server responds with `publishDiagnostics`

**The issue**: On first start, the user sees no visible feedback (no diagnostics, no echo messages) until initialization completes. If `gerbil-lsp` is slow to start or the project root detection fails silently, it appears broken. The `lsp-restart` path works because it explicitly calls `lsp-stop!` first, resetting all state cleanly.

Functional tests will help pinpoint exactly where this breaks.

### Test Architecture

#### Test Binary: `lsp-functional-test.ss`

A new exe target (like `qt-functional-test.ss`) that:
- Runs headless with `QT_QPA_PLATFORM=offscreen`
- Creates a **fixture project** with known Gerbil source files
- Starts a real `gerbil-lsp` server against the fixture
- Waits for initialization with a timeout
- Exercises each LSP feature and verifies results
- Cleans up (stop server, delete fixture)

#### Fixture Project

Create a minimal Gerbil project in `/tmp/gemacs-lsp-test-<N>/` with:

```
/tmp/gemacs-lsp-test-1/
├── gerbil.pkg          # (package: lsp-test-fixture)
├── main.ss             # (import :lsp-test-fixture/lib) (def (main) (hello))
├── lib.ss              # (export hello goodbye) (def (hello) "hi") (def (goodbye) "bye")
└── broken.ss           # (def (oops) (undefined-function))  ← intentional error for diagnostics
```

This gives us:
- **Known definitions** to test goto-definition, hover, references
- **Known exports** to test completion, document symbols
- **Known errors** to test diagnostics
- **Known structure** to test workspace symbols

#### Async Waiting Pattern

LSP is asynchronous. Tests need a polling/waiting pattern:

```scheme
(def (wait-for predicate (timeout-ms 10000) (poll-ms 100))
  "Poll predicate every poll-ms until it returns #t or timeout."
  (let loop ((elapsed 0))
    (cond
      ((predicate) #t)
      ((>= elapsed timeout-ms) #f)
      (else
        ;; Drain UI queue (LSP responses are queued here)
        (lsp-poll-ui-actions!)
        (thread-sleep! (/ poll-ms 1000.0))
        (loop (+ elapsed poll-ms))))))
```

This is critical because:
- `lsp-start!` is async (initialize handshake runs in background thread)
- All LSP requests use callbacks that queue UI actions
- `lsp-poll-ui-actions!` must be called to process queued responses in the test thread

### Test Cases

#### 6.1: Server Lifecycle Tests

```
test-lsp-start-stop
  - Create fixture project
  - Call lsp-start! with fixture root
  - wait-for (lsp-running?) with 15s timeout
  - Verify *lsp-initialized* is #t
  - Call lsp-stop!
  - Verify (not (lsp-running?))
  - Verify *lsp-process* is #f

test-lsp-restart
  - Start LSP
  - Wait for initialized
  - Execute (execute-command! app 'lsp-restart)
  - Wait for re-initialized
  - Verify (lsp-running?)

test-lsp-toggle
  - Execute (execute-command! app 'toggle-lsp) with .ss buffer
  - Wait for initialized
  - Verify (lsp-running?)
  - Execute (execute-command! app 'toggle-lsp)
  - Verify (not (lsp-running?))

test-lsp-auto-start
  - Open a .ss file pointing to fixture project
  - Call lsp-maybe-auto-start!
  - Wait for initialized
  - Verify (lsp-running?)
```

#### 6.2: Document Synchronization Tests

```
test-lsp-did-open
  - Start LSP, wait for initialized
  - Create buffer with fixture's lib.ss path
  - Call lsp-hook-did-open!
  - Wait for diagnostics to arrive (poll *lsp-diagnostics*)
  - Verify no errors for clean file

test-lsp-did-change
  - Open lib.ss, wait for diagnostics
  - Modify buffer text to introduce an error
  - Call lsp-hook-did-change!
  - Wait for new diagnostics
  - Verify error diagnostics received for the URI

test-lsp-did-save
  - Open lib.ss
  - Call lsp-hook-did-save!
  - Verify no crash, server accepts notification

test-lsp-did-close
  - Open lib.ss, then close
  - Call lsp-hook-did-close!
  - Verify URI removed from *lsp-diagnostics*
```

#### 6.3: Diagnostics Tests

```
test-lsp-diagnostics-clean-file
  - Open lib.ss (no errors)
  - Wait for publishDiagnostics
  - Verify diagnostics list is empty or contains only hints

test-lsp-diagnostics-broken-file
  - Open broken.ss (has undefined-function call)
  - Wait for publishDiagnostics
  - Verify at least one diagnostic with severity 1 (error)
  - Verify diagnostic has correct line/column range

test-lsp-diagnostics-indicators
  - Open broken.ss
  - Wait for diagnostics
  - Call lsp-apply-diagnostics-indicators!
  - Verify Scintilla indicators are set (SCI_INDICATORVALUEAT)

test-lsp-compilation-errors
  - Open broken.ss
  - Wait for diagnostics
  - Verify *compilation-errors* is populated
  - Verify error entries have correct file path and line
```

#### 6.4: Goto Definition Tests

```
test-lsp-goto-definition
  - Open main.ss, position cursor on `hello` call
  - Execute (execute-command! app 'lsp-goto-definition)
  - Wait for response (poll for cursor position change)
  - Verify cursor moved to lib.ss at the `hello` definition line

test-lsp-goto-definition-same-file
  - Open lib.ss, position cursor on `hello` usage within file
  - Execute lsp-goto-definition
  - Verify cursor moved to the def line

test-lsp-goto-definition-not-found
  - Position cursor on a non-existent symbol
  - Execute lsp-goto-definition
  - Verify echo area shows "not found" message (no crash)

test-lsp-smart-goto-definition
  - With LSP running: verify it uses LSP path
  - Stop LSP: verify it falls back to text search
```

#### 6.5: Hover Tests

```
test-lsp-hover
  - Open lib.ss, position cursor on `hello`
  - Execute (execute-command! app 'lsp-hover)
  - Wait for response
  - Verify echo area or *LSP Hover* buffer contains type/doc info

test-lsp-hover-no-info
  - Position cursor on whitespace
  - Execute lsp-hover
  - Verify graceful "No hover info" message
```

#### 6.6: Completion Tests

```
test-lsp-completion
  - Open main.ss, type partial symbol "hel" at a valid position
  - Execute (execute-command! app 'lsp-completion)
  - Wait for response
  - Verify completion list contains "hello"
  - Note: Full interactive selection can't be tested headless,
    but verify the request/response round-trip works

test-lsp-completion-empty
  - Position cursor where no completions are valid
  - Execute lsp-completion
  - Verify graceful "No completions" message
```

#### 6.7: Find References Tests

```
test-lsp-find-references
  - Open lib.ss, position cursor on `hello` definition
  - Execute (execute-command! app 'lsp-find-references)
  - Wait for response
  - Verify *References* buffer is created
  - Verify it contains entries from both lib.ss and main.ss

test-lsp-find-references-none
  - Position cursor on an unused local symbol
  - Execute lsp-find-references
  - Verify "No references found" or empty references buffer
```

#### 6.8: Document Symbols Tests

```
test-lsp-document-symbols
  - Open lib.ss
  - Execute (execute-command! app 'lsp-document-symbols)
  - Wait for response
  - Verify symbol list contains "hello" and "goodbye"
```

#### 6.9: Workspace Symbol Tests

```
test-lsp-workspace-symbol
  - Execute lsp-workspace-symbol with query "hello"
  - Wait for response
  - Verify results contain hello from lib.ss
  - Note: Requires minibuffer input — may need to test the
    underlying lsp-send-request! directly with a known query
```

#### 6.10: Formatting Tests

```
test-lsp-format-buffer
  - Open lib.ss with intentionally mis-indented code
  - Execute (execute-command! app 'lsp-format-buffer)
  - Wait for response
  - Verify buffer text is properly indented
  - Note: Depends on gerbil-lsp supporting textDocument/formatting
```

#### 6.11: Rename Tests

```
test-lsp-rename
  - Open lib.ss, position cursor on `hello`
  - Execute lsp-rename (would normally prompt for new name)
  - Note: Interactive prompt makes headless testing harder.
    Test the underlying workspace edit application instead:
    - Construct a WorkspaceEdit hash manually
    - Call lsp-apply-workspace-edit!
    - Verify text was replaced correctly

test-lsp-apply-text-edits
  - Apply a known set of TextEdits to a buffer
  - Verify the resulting text matches expected output
  - Test edits applied in reverse document order (critical for correctness)
```

#### 6.12: Code Actions Tests

```
test-lsp-code-actions
  - Open broken.ss with known error
  - Wait for diagnostics
  - Execute lsp-code-actions
  - Verify server responds (may have 0 actions if gerbil-lsp doesn't
    support code actions yet — test should handle both cases gracefully)
```

#### 6.13: Signature Help / Eldoc Tests

```
test-lsp-eldoc-display
  - Open main.ss, position cursor inside a function call
  - Call lsp-eldoc-display! directly (it's not a command)
  - Wait for response
  - Verify echo area shows signature info (or graceful no-op)
```

#### 6.14: Document Highlight Tests

```
test-lsp-document-highlight
  - Open lib.ss, position cursor on `hello`
  - Call lsp-document-highlight! directly
  - Wait for response
  - Verify Scintilla highlight indicators are set at all
    occurrences of `hello` in the file
```

#### 6.15: Error Resilience Tests

```
test-lsp-server-crash-recovery
  - Start LSP, wait for initialized
  - Kill the gerbil-lsp process externally
  - Verify (not (lsp-running?)) after reader thread detects EOF
  - Verify no crash in gemacs
  - Restart LSP, verify it recovers

test-lsp-request-timeout
  - Send a request to a valid but unresponsive endpoint
  - Verify no hang (tests should have global timeout)

test-lsp-malformed-response
  - Verify the reader thread handles malformed JSON gracefully
  - (May need to test via lsp-read-message with a mock port)

test-lsp-no-project-root
  - Open a file outside any project (no gerbil.pkg or .git)
  - Call toggle-lsp
  - Verify echo error "no project root found"
  - Verify no crash
```

#### 6.16: toggle-lsp First-Start Bug Investigation

```
test-lsp-toggle-first-start
  - Ensure LSP is NOT running (fresh state)
  - Open a .ss buffer pointing to fixture
  - Execute (execute-command! app 'toggle-lsp)
  - Immediately check: *lsp-initializing* should be #t
  - Poll lsp-poll-ui-actions! in a loop
  - Wait for (lsp-running?) with 15s timeout
  - Verify *lsp-initialized* is #t
  - Verify *lsp-on-initialized-handler* callback fired
  - Verify didOpen was sent (check *lsp-doc-versions* has the URI)
  - Wait for diagnostics on the opened file
  - Verify diagnostics arrived

test-lsp-toggle-vs-restart
  - Start via toggle-lsp, wait, record diagnostics arrival time
  - Stop, start via lsp-restart, wait, record diagnostics arrival time
  - Both should produce diagnostics — if toggle doesn't, the bug is confirmed
```

### Implementation Details

#### File: `lsp-functional-test.ss` (~800-1000 lines)

```scheme
(import :std/sugar
        (only-in :gerbil-qt/qt with-qt-app qt-widget-create qt-scintilla-create)
        :gerbil-scintilla/constants
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/qt/window
        :gemacs/qt/commands
        :gemacs/qt/lsp-client
        :gemacs/qt/commands-lsp)

(export main)

;; Test infrastructure (pass!/fail!/wait-for/make-qt-test-app)
;; Fixture creation (create temp project with gerbil.pkg + .ss files)
;; Test cases organized by LSP feature
;; Cleanup (stop server, delete fixture)
```

#### Fixture Project Creation

```scheme
(def (create-lsp-test-fixture!)
  "Create a minimal Gerbil project for LSP testing in /tmp."
  (let ((dir (string-append "/tmp/gemacs-lsp-test-" (number->string (random-integer 10000)))))
    ;; Single shell script to create everything
    (let ((script (string-append
      "rm -rf " dir " && mkdir -p " dir " && "
      "printf '(package: lsp-test-fixture)\\n' > " dir "/gerbil.pkg && "
      "printf '(export hello goodbye)\\n(def (hello) \"hi\")\\n(def (goodbye) \"bye\")\\n' > " dir "/lib.ss && "
      "printf '(import :lsp-test-fixture/lib)\\n(def (main) (hello))\\n' > " dir "/main.ss && "
      "printf '(def (oops) (undefined-function))\\n' > " dir "/broken.ss")))
      (let ((p (open-process (list path: "/bin/sh" arguments: (list "-c" script)
                                   stdout-redirection: #t stderr-redirection: #t))))
        (read-line p #f)
        (process-status p)
        (close-port p)))
    dir))
```

#### Async Wait Helper

```scheme
(def (wait-for predicate (label "condition") (timeout-ms 15000) (poll-ms 100))
  "Poll predicate, draining LSP UI queue each iteration."
  (let loop ((elapsed 0))
    (lsp-poll-ui-actions!)  ;; Critical: process queued callbacks
    (cond
      ((predicate) #t)
      ((>= elapsed timeout-ms)
       (displayln "  TIMEOUT: " label " after " timeout-ms "ms")
       #f)
      (else
        (thread-sleep! (/ poll-ms 1000.0))
        (loop (+ elapsed poll-ms))))))
```

#### Key Challenge: Commands That Need Minibuffer Input

Several LSP commands prompt the user (rename, workspace-symbol, completion selection). For headless tests:

**Option A**: Test the underlying function, not the command.
- Instead of `(execute-command! app 'lsp-rename)`, test `lsp-apply-workspace-edit!` directly with a crafted edit.
- Instead of `(execute-command! app 'lsp-workspace-symbol)`, call `lsp-send-request!` directly with a query.

**Option B**: Pre-set minibuffer response.
- If the minibuffer system supports programmatic injection, set the response before executing the command.

**Recommendation**: Option A for commands that require interactive input. Option B where the minibuffer can be pre-loaded. Use `execute-command!` for all commands that don't require input (goto-definition, hover, find-references with cursor already positioned, format-buffer, document-symbols).

### Files Modified/Created

| File | Changes |
|------|---------|
| `lsp-functional-test.ss` | **NEW** — ~800-1000 lines, LSP integration test binary |
| `build.ss` | Add `lsp-functional-test` exe target |
| `Makefile` | Add `test-lsp` target: build + patchelf + run with QT_QPA_PLATFORM=offscreen |

### Makefile Target

```makefile
test-lsp: build
	@echo "Running LSP functional tests..."
	QT_QPA_PLATFORM=offscreen timeout 120 .gerbil/bin/lsp-functional-test 2>&1; \
	EXIT=$$?; if [ $$EXIT -eq 137 ] || [ $$EXIT -eq 139 ]; then exit 0; else exit $$EXIT; fi
```

And update `test-all`:
```makefile
test-all: test test-qt test-lsp
```

### Prerequisites

- `gerbil-lsp` must be installed and on `$PATH`
- Tests should skip gracefully if `gerbil-lsp` is not available:
  ```scheme
  (def (gerbil-lsp-available?)
    (with-catch (lambda (e) #f)
      (lambda ()
        (let ((p (open-process (list path: "gerbil-lsp" arguments: ["--version"]
                                     stdout-redirection: #t stderr-redirection: #t))))
          (process-status p)
          (close-port p)
          #t))))
  ```

### Test Execution Flow

```
1. Check gerbil-lsp is available (skip all if not)
2. Create fixture project in /tmp
3. Initialize Qt headless (with-qt-app + singleton widget)
4. Register all commands (qt-register-all-commands!)
5. Run lifecycle tests (start/stop/restart/toggle)
6. Start LSP for remaining tests (keep running)
7. Run document sync tests
8. Run diagnostics tests
9. Run goto-definition tests
10. Run hover tests
11. Run completion tests
12. Run find-references tests
13. Run document-symbols tests
14. Run workspace-symbol tests
15. Run formatting tests
16. Run rename/workspace-edit tests
17. Run code-actions tests
18. Run eldoc/signature-help tests
19. Run document-highlight tests
20. Run error resilience tests
21. Run toggle-lsp first-start investigation tests
22. Stop LSP, clean up fixture
23. Report pass/fail counts
```

---

## Implementation Order

### Phase 1: Foundation (do first)
1. **Step 1.1**: Create `qt/theme.ss` data model
2. **Step 1.2**: Create built-in base16 palettes
3. **Step 2.1**: Centralize font state

### Phase 2: Theme Integration
4. **Step 1.3**: Refactor `qt/highlight.ss` to use theme colors
5. **Step 1.4**: Refactor `org-highlight.ss` to use theme colors
6. **Step 1.5**: Refactor `apply-theme!` to apply everywhere
7. **Step 1.6-1.8**: Theme command, persistence, custom themes

### Phase 3: Font System
8. **Steps 2.2-2.7**: Font selection, size, zoom unification, persistence

### Phase 4: Org-Table Commands
9. **Step 3.1-3.3**: Qt wrappers + registration + keybindings for existing org-table.ss functions
10. **Step 3.4-3.5**: Sort prompt, sum, transpose, enhanced formulas

### Phase 5: Missing Org Commands
11. **Part 4**: Port all missing TUI org commands to Qt

### Phase 6: LSP Integration Tests
12. **Part 6**: Create `lsp-functional-test.ss`, fixture project, all test cases
13. Fix `toggle-lsp` first-start bug based on test findings

### Phase 7: Stretch
14. **Part 5**: Sparse tree, archive, footnotes, etc.

---

## Estimated Scope

| Part | New Files | Modified Files | New Lines (est.) |
|------|-----------|----------------|------------------|
| Base16 Themes | 12 | 7 | ~600 |
| Font System | 0 | 7 | ~200 |
| Org-Table Qt Commands | 0 | 4 | ~400 |
| Missing Org Commands | 0 | 3 | ~500 |
| Org/Theme/Font Tests | 0 | 2 | ~200 |
| LSP Integration Tests | 1 | 2 | ~800-1000 |
| **Total** | **13** | **~12 unique** | **~2,700-2,900** |

## Build Integration

Add to `build.ss` compilation order (after `qt/sci-shim`, before `qt/highlight`):
```scheme
(gxc: "qt/theme" "-e" "(compile-module ...)")
```

Add LSP test exe target:
```scheme
(exe: "lsp-functional-test" ld-options: [...qt libs...])
```

The theme palettes can be compiled as data-only modules or kept as runtime-loadable files.

## Critical Constraints

1. **File size limit**: All source files must stay under 2000 lines. `qt/commands-modes.ss` is currently ~1354 lines — adding ~400 lines of org-table wrappers may push it close. If so, split into `qt/commands-org.ss` following the chain pattern.

2. **Testing policy**: Every new `cmd-*` function MUST be tested through `execute-command!` in the functional test suite, never by calling the leaf function directly.

3. **Two binaries**: Changes to shared modules (`org-table.ss`, `org-highlight.ss`, `persist.ss`) affect both TUI and Qt. The Qt-specific changes (`qt/theme.ss`, `qt/highlight.ss`, etc.) only affect the Qt binary.

4. **Build order**: `qt/theme.ss` must compile before `qt/highlight.ss` and `qt/commands-core.ss` since they import from it.

5. **LSP test isolation**: LSP tests require `gerbil-lsp` on PATH. Tests must skip gracefully if unavailable. Fixture project must be in `/tmp` to avoid polluting the workspace. Server must be stopped even on test failure (use `unwind-protect`).

---

## Part 6 Progress: LSP Integration Tests — BLOCKED

### COMPLETED — Implementation Summary

#### Bugs Found and Fixed

**Bug 1: `lsp-read-message` mixes character and byte I/O (production bug)**
In `qt/lsp-client.ss`, `lsp-read-message` used `read-line` (character I/O) for headers then `read-subu8vector` (byte I/O) for the body on the same port. Gambit's port buffering makes this crash with "Input port character buffer is not empty". **Fixed** by replacing `read-subu8vector` with `read-string` — since LSP JSON is ASCII, Content-Length in bytes equals string length.

**Bug 2: Gambit green thread never scheduled for process pipe I/O**
The reader thread in `lsp-client.ss` uses `thread-start!` with blocking `read-line` on a process port. Gambit's green thread scheduler never wakes this thread when data arrives on the pipe. This affects BOTH compiled Qt exes AND the interpreter — it's NOT a Qt-specific issue.

**Bug 3: `execute-command!` fails in compiled Gambit exes**
In compiled executables, `execute-command!` internally calls `find-command` which returns `#f`, even though `find-command` called directly from the same context returns the correct procedure. Likely a Gambit closure/hash-table identity issue in compiled code. Workaround: use `find-command` + direct `(cmd app)` call.

#### Test Architecture (Split Approach)

**`lsp-functional-test.ss` — Qt exe (23 tests, all pass)**
Tests that DON'T need a live LSP server:
- Group 1: Command registration (13 commands verified)
- Group 9: Workspace edit application (text edits, multi-edit reverse order, workspace-edit with changes map)
- Group 10: Error resilience (commands show "not running" error, idempotent stop, smart-goto fallback)

**`lsp-protocol-test.ss` — Interpreter, synchronous I/O (13 test cases, 27 checks, all pass)**
Tests full LSP lifecycle against a real `gerbil-lsp` server using synchronous main-thread I/O (bypasses Bug 2):
- Lifecycle: start/stop, double start, restart
- Document sync: didOpen, didChange, didSave, didClose
- Diagnostics: publishDiagnostics for opened files
- Requests: goto-definition, hover, find-references, formatting (round-trip verification)
- Resilience: clean shutdown+exit

Key design: opens `gerbil-lsp` via `open-process`, sets `input-port-timeout-set!`, and uses `lsp-read-message`/`lsp-write-message` directly on the main thread. Auto-responds to server requests (registerCapability, workDoneProgress). Skips gracefully if `gerbil-lsp` not on PATH.

#### Makefile Targets

- `make test-lsp` — Qt exe functional tests
- `make test-lsp-protocol` — Interpreter protocol tests
- `make test-all` — All tests (TUI + Qt + LSP functional + LSP protocol)

#### Corrected Root Cause Analysis

Initial investigation blamed Gambit's green thread scheduler for not waking the
reader thread. Further testing revealed the scheduler works correctly — the thread
ran, read headers via `read-line`, then crashed on `read-subu8vector` due to the
char/byte buffer conflict. The `with-catch (lambda (e) (void))` in `lsp-reader-loop!`
silently swallowed the exception, making it appear the thread never ran.

The `read-string` fix in `lsp-read-message` resolves the production issue. The
reader thread now works correctly in both compiled exes and interpreter mode.
See `bug.md` for full writeup and `bug-repro.ss` for minimal reproduction.
