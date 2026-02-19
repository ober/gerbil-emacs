# Comprehensive Theme & Font Support for Gemacs

## Current State

### Theme System (Qt)
- **4 built-in themes** (`dark`, `solarized-dark`, `light`, `monokai`) in `qt/commands-core.ss:204-238`
- Themes are alists of UI chrome colors: `bg`, `fg`, `selection`, `modeline-*`, `echo-*`, `gutter-*`, `split`, `tab-*`
- `theme-stylesheet` generates Qt CSS for QPlainTextEdit, QLabel, QStatusBar, QLineEdit, QSplitter
- `apply-theme!` updates Qt stylesheet + gutter line-number-area colors
- **Gap**: Syntax highlighting colors are hardcoded in `qt/highlight.ss:31-48` — switching themes does NOT update syntax colors

### Theme System (TUI)
- Only `dark` and `light` in `editor-cmds-c.ss:1480-1500` — sets STYLE_DEFAULT fg/bg, nothing else
- Modeline colors hardcoded in `modeline.ss:167-169`
- Echo area colors hardcoded in `echo.ss:67-77`
- Terminal ANSI colors hardcoded in `terminal.ss:47-67`

### Font Handling (Qt)
- **Hardcoded** `"Monospace"` at size 11 in `qt/window.ss:184-185` (Scintilla STYLE_DEFAULT)
- **Hardcoded** `font-family: monospace; font-size: 10pt` in Qt CSS (~10 places across `commands-core.ss`, `app.ss`, `echo.ss`)
- `cmd-increase/decrease/reset-font-size` in `qt/commands-shell.ss:40-59` — uses `qt-widget-set-font-size!` on current editor only
- Zoom commands in `qt/commands-edit.ss:879-892` — uses `qt-widget-set-font-size!` (widget-level, not Scintilla-level)

### Font Handling (TUI)
- `cmd-increase/decrease/reset-font-size` in `editor-cmds-b.ss:1119-1127` — stubs ("not yet implemented")
- No font family or size configuration

### Configuration
- `.gemacs-init.ss` — full Scheme eval, loaded at startup (`qt/commands-modes.ss:589`)
- `.gemacs-init` — plaintext settings, loaded at startup (`persist.ss:460`)
- `.gemacs-config` — dir-locals, per-project (`qt/commands-core.ss:341`)
- No persistence of theme or font choices across sessions

---

## Implementation Plan

### Phase 1: Face System — The Foundation ✅ COMPLETE

**Goal**: Introduce an Emacs-like "face" abstraction that maps semantic names to visual properties, enabling theme-aware syntax highlighting.

**New file**: `face.ss` (~300 lines, shared between TUI and Qt)

#### 1.1 Define the face data structure ✅

```scheme
;; A face is a hash-table with keys: :fg :bg :bold :italic :underline
(defstruct face (fg bg bold italic underline))
```

#### 1.2 Define the global face registry ✅

```scheme
(def *faces* (make-hash-table-eq))  ;; symbol -> face

(def (define-face! name . props)
  "Register a face. Props are keyword args: fg: bg: bold: italic: underline:"
  ...)

(def (face-get name)
  "Look up a face by name. Returns the face struct or #f."
  ...)
```

#### 1.3 Define the standard face names ✅

These mirror Emacs face names for familiarity:

| Face Name | Purpose | Dark Default |
|-----------|---------|-------------|
| `default` | Base text | fg:#d8d8d8 bg:#181818 |
| `font-lock-keyword-face` | Language keywords | fg:#cc99cc bold |
| `font-lock-builtin-face` | Built-in functions | fg:#66cccc |
| `font-lock-string-face` | String literals | fg:#99cc99 |
| `font-lock-comment-face` | Comments | fg:#999999 italic |
| `font-lock-number-face` | Numbers | fg:#f99157 |
| `font-lock-operator-face` | Operators | fg:#b8b8b8 |
| `font-lock-type-face` | Type names | fg:#ffcc66 |
| `font-lock-preprocessor-face` | Preprocessor | fg:#f99157 |
| `font-lock-heading-face` | Headings | fg:#6699cc bold |
| `modeline` | Active modeline | fg:#d8d8d8 bg:#282828 |
| `modeline-inactive` | Inactive modeline | fg:#808080 bg:#282828 |
| `region` | Selection | bg:#404060 |
| `line-number` | Gutter | fg:#8c8c8c bg:#202020 |
| `cursor-line` | Current line highlight | bg:#222228 |
| `match` | Brace match | fg:#ffff00 bg:#404060 bold |
| `mismatch` | Brace mismatch | fg:#ff4040 bg:#602020 bold |
| `isearch` | Search highlight | fg:#000000 bg:#ffcc00 |
| `error` | Error text | fg:#ff4040 |
| `org-heading-1` through `org-heading-8` | Org headings | (existing colors) |
| `org-todo` | TODO keyword | fg:#dc3232 bold |
| `org-done` | DONE keyword | fg:#32b432 bold |
| `org-link` | Org links | fg:#5078dc underline |
| `org-code` | Org code | fg:#3ca03c |
| `org-verbatim` | Org verbatim | fg:#32b4b4 |
| `org-table` | Org tables | fg:#32b4b4 |
| `org-comment` | Org comments | fg:#828282 italic |
| `org-tag` | Org tags | fg:#9650b4 |
| `org-date` | Org timestamps | fg:#b450b4 |
| `org-property` | Org properties | fg:#646464 |
| `org-block-delimiter` | Org block begin/end | fg:#d28c32 |
| `org-block-body` | Org block content | fg:#646464 |
| `tab-active` | Active tab | fg:#ffffff bg:#404060 |
| `tab-inactive` | Inactive tab | fg:#a0a0a0 bg:#252525 |
| `window-border-active` | Active window | border:#51afef |
| `window-border-inactive` | Inactive window | border:#3a3a3a |

**Files modified**: ✅
- Create `face.ss` ✅
- `core.ss` — import and re-export face.ss ✅

---

### Phase 2: Theme System Overhaul

**Goal**: Themes define faces, not just UI chrome colors. Switching themes updates everything — syntax highlighting, UI chrome, org mode, terminal, decorations.

#### 2.1 Extend theme structure to include faces ✅ COMPLETE

Each theme is now an alist mapping face names to face property lists. The `*themes*` registry lives in `themes.ss`.

**File**: `qt/commands-core.ss` — rewrote theme system:
- Removed local `*themes*` hash table (now from themes.ss)
- Kept `*current-theme*` variable
- Removed old `define-theme!` and built-in theme definitions
- Kept `theme-color` for backward compatibility with legacy UI chrome keys
- Added `load-theme!` function that applies face definitions to global `*faces*` registry
- Updated `apply-theme!` to accept optional `theme-name:` parameter and call `load-theme!`

**File**: `qt/commands-config.ss` — updated `cmd-load-theme`:
- Changed to use `(theme-names)` instead of `(hash-keys *themes*)`
- Changed to use `(theme-get sym)` instead of `(hash-key? *themes* sym)`
- Now calls `(apply-theme! app theme-name: sym)` which loads faces first

#### 2.2 Expand built-in themes with syntax colors ✅ COMPLETE

Created 10 comprehensive themes with full face definitions:

- **dark** — Default dark theme (current hardcoded colors)
- **light** — Default light theme
- **solarized-dark** — Solarized dark palette
- **solarized-light** — Solarized light palette (NEW)
- **monokai** — Monokai palette
- **gruvbox-dark** — Popular warm dark theme (NEW)
- **gruvbox-light** — Warm light variant (NEW)
- **dracula** — Popular purple-based dark theme (NEW)
- **nord** — Arctic, north-bluish color palette (NEW)
- **zenburn** — Low-contrast dark theme (NEW)

**New file**: `themes.ss` (~1160 lines) — all built-in theme definitions with:
- Full face definitions for each theme (syntax, UI, org-mode, tabs, window borders)
- Legacy UI chrome keys for backward compatibility
- Theme registry functions: `register-theme!`, `theme-get`, `theme-names`

**Files modified**:
- Created `themes.ss` ✅
- `build.ss` — added "themes" to build order (after face, before core) ✅
- `core.ss` — import and export themes.ss ✅
- `qt/commands-core.ss` — rewrote theme system to use face-based structure ✅
- `qt/commands-config.ss` — updated cmd-load-theme ✅

#### 2.3 Make `apply-theme!` comprehensive (Qt) ✅ COMPLETE

Updated `apply-theme!` in `qt/commands-core.ss` to:
1. ✅ Load theme faces first (via `load-theme!` if `theme-name:` provided)
2. ✅ Update Qt stylesheet (from legacy UI chrome keys for backward compat)
3. ✅ Update gutter colors (line number area)
4. ✅ Re-apply syntax highlighting to all open buffers
5. TODO: Update visual decorations (cursor-line, brace match, search highlight)
6. TODO: Update tab colors and window borders
7. TODO: Update terminal ANSI colors (if theme defines them)

#### 2.4 Make `apply-theme!` work for TUI ✅ COMPLETE

Updated `editor-cmds-c.ss:cmd-load-theme` to:
1. ✅ Load theme faces from theme registry
2. ✅ Re-apply syntax highlighting for current buffer
3. ✅ Modeline now uses face system (see modeline.ss)
4. ✅ Echo area now uses face system (see echo.ss)

#### 2.5 Wire syntax highlighting to face system (Qt) ✅ COMPLETE

Modified `qt/highlight.ss`:
- ✅ Replaced hardcoded color constants with face-aware helper functions:
  - `(face-fg-rgb face-name)` → returns `(values r g b)` from face foreground
  - `(face-has-bold? face-name)` → checks if face has bold attribute
  - `(face-has-italic? face-name)` → checks if face has italic attribute
- ✅ Renamed `apply-base-dark-theme!` → `apply-base-theme!` (reads from `default` and `line-number` faces)
- ✅ Updated all `setup-*-styles!` functions to use face lookups:
  - `setup-cpp-styles!` (C/C++/Java/JS/Go/Rust/etc.)
  - `setup-python-styles!`
  - `setup-lisp-styles!` (Scheme/Lisp)
  - `setup-bash-styles!`
  - `setup-ruby-styles!`
  - `setup-lua-styles!`
  - `setup-sql-styles!`
  - `setup-perl-styles!`
  - Generic lexers (JSON, YAML, XML, CSS, Markdown, etc.)
- ✅ Updated `qt-setup-org-styles!` to read from org-mode faces (`org-heading-1` through `org-heading-8`, `org-todo`, `org-done`, `org-link`, `org-code`, `org-verbatim`, `org-table`, etc.)
- ✅ Initialize face system at startup (`qt/app.ss:qt-main` calls `define-standard-faces!` and `load-theme!`)

#### 2.6 Wire syntax highlighting to face system (TUI) ✅ COMPLETE

Modified `highlight.ss`:
- ✅ Added face helper functions: `face-fg-rgb`, `face-bg-rgb`, `face-has-bold?`, `face-has-italic?`
- ✅ Replaced hardcoded Scintilla style colors with face lookups
- ✅ All `editor-style-set-*` calls now use faces (`default`, `font-lock-*`, `org-*`)
- ✅ Updated `setup-gerbil-highlighting!` to be face-aware

Modified `modeline.ss`:
- ✅ Added `face-to-rgb-int` helper to convert face colors to RGB integers for `tui-print!`
- ✅ Modeline now reads from `modeline` and `modeline-inactive` faces

Modified `echo.ss`:
- ✅ Added `face-to-rgb-int` helper
- ✅ Echo area now reads from `default` and `error` faces

**Note**: `terminal.ss` ANSI color updates are optional and deferred (terminal colors are less critical than editor/UI)

#### 2.7 Persist theme choice

- Save current theme name to `~/.gemacs-settings` (or add to `.gemacs-init`)
- Load theme at startup before opening buffers
- `apply-theme!` writes the choice to disk

**Files modified**:
- `qt/commands-core.ss` — rewrite theme system
- `qt/highlight.ss` — face-aware syntax colors
- `qt/window.ss` — face-aware editor setup
- `qt/echo.ss` — face-aware echo/minibuffer styling
- `qt/app.ss` — face-aware tab bar styling
- `qt/modeline.ss` — face-aware modeline
- `qt/commands-config.ss` — update `cmd-load-theme` to re-highlight all buffers
- `highlight.ss` — face-aware TUI highlighting
- `editor-cmds-c.ss` — face-aware TUI theme switching
- `terminal.ss` — theme-aware terminal colors
- `modeline.ss` — face-aware TUI modeline
- `echo.ss` — face-aware TUI echo area
- `persist.ss` — save/load theme preference
- Create `themes.ss` — all built-in theme definitions
- Create `face.ss` — face system

---

### Phase 3: Font Configuration (3.1-3.3 COMPLETE ✅)

**Goal**: Users can set font family and size like Emacs (`set-frame-font`, `M-x customize-face RET default`).

#### 3.1 Global font state ✅

**File**: `face.ss` (extend)

```scheme
(def *default-font-family* "Monospace")
(def *default-font-size* 11)

(def (set-default-font! family size)
  "Set the default font for all editors."
  ...)
```

#### 3.2 Apply font to all Scintilla editors (Qt) ✅

**File**: `qt/window.ss` — modified `qt-scintilla-setup-editor!`

Replaced hardcoded:
```scheme
(sci-send/string ed SCI_STYLESETFONT "Monospace" STYLE_DEFAULT)
(sci-send ed SCI_STYLESETSIZE STYLE_DEFAULT 11)
```

With:
```scheme
(sci-send/string ed SCI_STYLESETFONT *default-font-family* STYLE_DEFAULT)
(sci-send ed SCI_STYLESETSIZE STYLE_DEFAULT *default-font-size*)
```

#### 3.3 Apply font to all Qt widgets ✅

**File**: `qt/commands-core.ss` — modified `theme-stylesheet` to use dynamic font CSS

Replaced hardcoded `font-family: monospace; font-size: 10pt;` with values from `*default-font-family*` and `*default-font-size*`.

Also updated hardcoded styles in:
- `qt/echo.ss:33-35` (echo label styles) ✅
- `qt/echo.ss:55-59` (minibuffer style converted to function `mb-style`) ✅
- `qt/app.ss:142-144` (tab bar button styles, with font size -2pt) ✅
- `qt/app.ss:203` (echo label initial style) ✅

#### 3.4 Font size commands — fix existing stubs

**File**: `qt/commands-shell.ss:40-59` — already implemented, but only updates current editor widget.

Enhance to:
1. Update `*default-font-size*`
2. Apply to ALL open editors (all windows, not just current)
3. Update Scintilla STYLE_DEFAULT size on each editor
4. Update Qt stylesheet (so chrome widgets match)
5. Persist to settings file

**File**: `editor-cmds-b.ss:1119-1127` — implement TUI font size stubs:
- TUI terminal font size is controlled by the terminal emulator, not the application
- These should echo a message explaining this: "Font size is controlled by your terminal emulator"
- Or, if using Scintilla TUI widget, use SCI_ZOOMIN/SCI_ZOOMOUT

#### 3.5 New commands: `set-frame-font` and `set-font-size`

**File**: `qt/commands-config.ss` — add new commands

```
M-x set-frame-font    → prompts for font family with completion from system fonts
M-x set-font-size     → prompts for size (number)
```

Implementation:
- `cmd-set-frame-font`: Prompt with completion from available monospace fonts
  - Use `fc-list :spacing=mono family` to enumerate available monospace fonts (Linux/macOS)
  - Set `*default-font-family*`
  - Apply to all editors via `SCI_STYLESETFONT` + `SCI_STYLECLEARALL`
  - Update Qt stylesheet
  - Persist choice
- `cmd-set-font-size`: Prompt for size number
  - Validate range (6-72)
  - Apply to all editors
  - Update Qt stylesheet
  - Persist choice

Register in `qt/commands.ss` (facade):
```scheme
(register-command! 'set-frame-font cmd-set-frame-font)
(register-command! 'set-font-size cmd-set-font-size)
```

#### 3.6 Font persistence

Add to `~/.gemacs-settings` or persist alongside theme:
```
font-family: JetBrains Mono
font-size: 12
```

Load at startup before creating editor widgets.

**Files modified**:
- `face.ss` — font state variables ✅
- `qt/window.ss` — use font variables in editor setup ✅
- `qt/commands-core.ss` — use font variables in stylesheet ✅
- `qt/echo.ss` — use font variables in echo/minibuffer styles ✅
- `qt/app.ss` — use font variables in tab bar styles ✅
- `qt/commands-shell.ss` — enhance font size commands (TODO)
- `qt/commands-config.ss` — new set-frame-font, set-font-size commands (TODO)
- `qt/commands.ss` — register new commands (TODO)
- `editor-cmds-b.ss` — implement/fix TUI font stubs (TODO)
- `persist.ss` — save/load font preferences (TODO)

---

### Phase 4: `customize-face` Interactive Command

**Goal**: Users can interactively modify individual face colors, like Emacs `M-x customize-face`.

#### 4.1 Implement `cmd-customize-face` (Qt)

Replace the stub in `qt/commands-config.ss:231-233`.

Behavior:
1. Prompt: "Customize face: " with completion from `(hash-keys *faces*)`
2. Show current face properties in echo area
3. Prompt: "Foreground (#hex or empty to keep): "
4. Prompt: "Background (#hex or empty to keep): "
5. Prompt: "Bold (y/n/empty): "
6. Prompt: "Italic (y/n/empty): "
7. Apply updated face to `*faces*`
8. Re-apply theme (which re-applies all faces)
9. Save customization to `~/.gemacs-custom-faces` for persistence

#### 4.2 Custom face persistence

**File**: `persist.ss` — add face customization save/load

Format in `~/.gemacs-custom-faces`:
```
font-lock-keyword-face	fg:#ff79c6	bold:true
font-lock-string-face	fg:#50fa7b
```

Custom faces overlay theme faces — theme provides defaults, user customizations override.

#### 4.3 Load order

At startup:
1. Load face defaults (from `face.ss`)
2. Load theme (applies theme's face definitions)
3. Load custom faces (overlays on top of theme)
4. Load font settings
5. Create editor widgets (which read from face system)

**Files modified**:
- `qt/commands-config.ss` — implement customize-face
- `persist.ss` — save/load custom faces
- `qt/app.ss` — load custom faces at startup

---

### Phase 5: Init File Theme/Font API

**Goal**: Users can configure themes and fonts from `.gemacs-init.ss` like Emacs.

#### 5.1 Expose API for init files

These symbols are already accessible via `eval` in `load-init-file!`, but ensure they're exported:

```scheme
;; In .gemacs-init.ss:
(load-theme 'dracula)
(set-frame-font "JetBrains Mono" 12)

;; Custom face override:
(set-face-attribute 'font-lock-keyword-face fg: "#ff79c6" bold: #t)

;; Define a custom theme:
(define-theme! 'my-theme
  '((default . (fg: "#e0e0e0" bg: "#1a1a2e"))
    (font-lock-keyword-face . (fg: "#e94560" bold: #t))
    ...))
(load-theme 'my-theme)
```

#### 5.2 Convenience functions

```scheme
(def (load-theme name)
  "Switch to named theme, like Emacs load-theme."
  (set! *current-theme* name)
  (apply-faces-from-theme! name)
  ...)

(def (set-face-attribute face-name . props)
  "Modify a face's properties, like Emacs set-face-attribute."
  ...)

(def (set-frame-font family size)
  "Set the default font, like Emacs set-frame-font."
  ...)
```

**Files modified**:
- `face.ss` — export convenience functions
- `qt/commands.ss` — ensure exports reach init file eval scope

---

### Phase 6: User-Defined Themes from Files

**Goal**: Users can create theme files and load them, like Emacs `~/.emacs.d/themes/`.

#### 6.1 Theme file format

Theme files live in `~/.gemacs-themes/` and are plain Scheme:

```scheme
;; ~/.gemacs-themes/my-dark-theme.ss
(define-theme! 'my-dark
  '((default        . (fg: "#c0c0c0" bg: "#0d0d0d"))
    (font-lock-keyword-face . (fg: "#ff6ac1" bold: #t))
    (font-lock-string-face  . (fg: "#5af78e"))
    (font-lock-comment-face . (fg: "#6272a4" italic: #t))
    ;; ... etc
    ))
```

#### 6.2 Theme discovery

`cmd-load-theme` enhanced to:
1. List built-in themes
2. Scan `~/.gemacs-themes/*.ss` for user themes
3. Show all in completion list
4. Load selected theme file if it's a user theme

#### 6.3 `describe-theme` command

New command: `M-x describe-theme` — opens a buffer showing all face definitions for the current (or named) theme.

**Files modified**:
- `qt/commands-config.ss` — enhance cmd-load-theme, add describe-theme
- `qt/commands.ss` — register describe-theme

---

## File Impact Summary

### New Files
| File | Lines (est.) | Purpose |
|------|-------------|---------|
| `face.ss` | ~250 | Face struct, registry, standard faces, font state |
| `themes.ss` | ~600 | All built-in theme definitions (face alists) |

### Modified Files (Major Changes)
| File | Current Lines | Changes |
|------|--------------|---------|
| `qt/highlight.ss` | 1079 | Replace hardcoded colors with face lookups |
| `qt/commands-core.ss` | 1438 | Rewrite theme system, face-aware stylesheet |
| `qt/commands-config.ss` | ~1817 | Implement customize-face, set-frame-font, set-font-size, describe-theme |
| `qt/window.ss` | 500 | Face-aware editor setup, font variables |
| `persist.ss` | 632 | Save/load theme, font, custom faces |
| `highlight.ss` | 1103 | Face-aware TUI highlighting |

### Modified Files (Minor Changes)
| File | Changes |
|------|---------|
| `qt/app.ss` | Font variables in tab styles, load custom faces at startup |
| `qt/echo.ss` | Font variables in echo/minibuffer styles |
| `qt/commands.ss` | Register new commands, export face/theme API |
| `qt/commands-shell.ss` | Enhance font size commands to apply globally |
| `qt/commands-edit.ss` | Zoom uses face-system default size for reset |
| `qt/modeline.ss` | Face-aware modeline colors |
| `terminal.ss` | Theme-aware ANSI colors |
| `core.ss` | Import/export face.ss |
| `editor-cmds-c.ss` | Face-aware TUI theme switching |
| `editor-cmds-b.ss` | Implement TUI font stubs |
| `modeline.ss` | Face-aware TUI modeline |
| `echo.ss` | Face-aware TUI echo |

---

## Implementation Order

1. **Phase 1** (face.ss) — ✅ COMPLETE - face system foundation in place
2. **Phase 3.1-3.3** (font state + wiring) — ✅ COMPLETE - all Qt components use dynamic fonts
3. **Phase 2.1-2.2** (theme structure + built-in themes) — ✅ COMPLETE - 10 comprehensive themes in themes.ss
4. **Phase 2.3 & 2.5** (Qt face-aware highlighting) — ✅ COMPLETE - Qt syntax highlighting uses face system
5. **Phase 2.4 & 2.6** (TUI face-aware highlighting) — ✅ COMPLETE - TUI syntax highlighting, modeline, echo use face system
6. **Phase 3.4-3.6** (font commands + persistence) — NEXT - enhance font size commands to be global, add set-frame-font
7. **Phase 2.7** (theme persistence) — depends on 2.3
8. **Phase 4** (customize-face) — depends on Phases 1-2
9. **Phase 5** (init file API) — depends on Phases 1-3
10. **Phase 6** (user theme files) — depends on Phase 5

---

## Testing Plan

### Unit tests (extend `emacs-test.ss`)
- Face creation and lookup
- Theme switching updates faces
- Font state management
- Hex color parsing

### Functional tests (extend `functional-test.ss`)
- `(execute-command! app 'load-theme)` with theme name
- `(execute-command! app 'set-font-size)` with size
- `(execute-command! app 'increase-font-size)` / `(execute-command! app 'decrease-font-size)`
- Theme persistence round-trip

### Qt functional tests (extend `qt-functional-test.ss`)
- `(execute-command! app 'load-theme)` changes Scintilla styles
- `(execute-command! app 'set-frame-font)` changes Scintilla font
- Font size commands update all editors
- Theme switch re-highlights all buffers
- Custom face override persists through theme switch

### Manual verification
- Visual inspection of each built-in theme
- Font rendering with various families (Monospace, JetBrains Mono, Fira Code, etc.)
- Org-mode highlighting correctness per theme
- Terminal ANSI color correctness per theme
- Tab bar, modeline, echo area styling consistency
