# Gemacs Feature Plan

## Feature 1: Full Magit Functional Testing

### Goal
Verify all magit/git operations work correctly through the dispatch chain, matching Emacs magit behavior. Both TUI and Qt paths need coverage.

### Current State
- **7 magit commands** registered in Qt: `magit-status`, `magit-stage`, `magit-unstage`, `magit-commit`, `magit-diff`, `magit-stage-all`, `magit-log`
- **4 quick-access git commands**: `show-git-status`, `show-git-log`, `show-git-diff`, `show-git-blame`
- **4 VC commands**: `vc-annotate`, `vc-diff-head`, `vc-log-file`, `vc-revert`
- **TUI has 8 basic VC commands**: `vc-register`, `vc-dir`, `vc-pull`, `vc-push`, `vc-create-tag`, `vc-print-log`, `vc-stash`, `vc-stash-pop`
- Existing tests: 8 TUI magit tests in `functional-test.ss`, 8 Qt helper tests + dispatch tests in `qt-functional-test.ss`

### What to Test
All tests go through `execute-command!` (never call leaf functions directly).

#### TUI Tests (`functional-test.ss`)
Each test creates a temp git repo, runs the command, and verifies the output buffer content.

1. **magit-status** — verify `*Magit*` buffer contains "Head:", staged/unstaged/untracked sections
2. **magit-stage** — stage a file, verify it moves from unstaged to staged in status
3. **magit-unstage** — unstage a file, verify it moves back to unstaged
4. **magit-commit** — commit staged changes, verify clean working tree after
5. **magit-diff** — verify diff output for a modified file shows `+`/`-` lines
6. **magit-stage-all** — verify all files move to staged
7. **magit-log** — verify log output contains commit hashes and messages
8. **show-git-status** — verify `*Git Status*` buffer content
9. **show-git-log** — verify `*Git Log*` buffer shows commit history
10. **show-git-diff** — verify `*Git Diff*` buffer shows changes
11. **show-git-blame** — verify `*Git Blame*` buffer for current file
12. **vc-annotate** — verify blame alias works
13. **vc-diff-head** — verify diff alias works
14. **vc-log-file** — verify per-file log works
15. **vc-revert** — verify file is reverted and buffer reloaded
16. **vc-register** — verify `git add` works
17. **vc-dir** — verify status display
18. **vc-pull/push** — smoke test (may need mock remote)
19. **vc-stash/stash-pop** — verify stash round-trip

#### Qt Tests (`qt-functional-test.ss`)
Mirror the TUI tests using the Qt dispatch chain with `execute-command!`.

#### Test Infrastructure
- Create a helper `with-temp-git-repo` that sets up a temporary git directory with initial commit
- Helper to create/modify/stage files in the temp repo
- Helper to override `*magit-dir*` for testing

### Missing Commands to Implement
- `magit-branch` — create/switch/delete branches (registered in test but not implemented)
- `magit-push` / `magit-pull` — interactive push/pull with remote selection
- `magit-stash` — stash management
- `magit-rebase` — interactive rebase
- `magit-cherry-pick` — cherry pick commits
- `magit-reset` — reset to commit
- `magit-refresh` — bound to `g` in magit buffer

### Implementation Steps
1. Add `with-temp-git-repo` test helper to both test files
2. Expand TUI magit tests to cover all commands with real git repos
3. Add Qt magit dispatch tests mirroring TUI
4. Implement missing commands (branch, push, pull, stash, rebase)
5. Add magit keymap for the `*Magit*` buffer (`s`=stage, `u`=unstage, `c`=commit, `d`=diff, `g`=refresh, `q`=quit, `b`=branch, `P`=push, `F`=pull, `z`=stash)

---

## Feature 2: Emacs Theme Support

### Goal
Load and apply color themes that define both UI chrome and syntax highlighting colors. Support importing from existing Emacs theme ecosystems.

### Current State
- **4 built-in themes** in `qt/commands-core.ss`: Dark, Solarized Dark, Light, Monokai
- Themes only affect **UI chrome** (tab bar, modeline, echo area backgrounds/foregrounds)
- **Syntax highlighting colors are hardcoded** in `highlight.ss` (TUI) and `qt/highlight.ss` (Qt) as compile-time constants
- The two layers are completely disconnected — switching themes does NOT change code colors

### Theme Architecture

#### Native Theme Format: `.gemacs-theme` files

A theme is an alist or simple S-expression file that defines ~20 semantic color slots:

```scheme
;; ~/.gemacs-themes/dracula.theme
(theme
  (name "Dracula")
  (variant dark)  ; dark or light

  ;; UI colors
  (bg         "#282a36")
  (fg         "#f8f8f2")
  (cursor     "#f8f8f0")
  (selection  "#44475a")
  (line-hl    "#2a2c3a")
  (modeline-bg "#282a36")
  (modeline-fg "#f8f8f2")
  (line-number-fg "#6272a4")
  (line-number-bg "#21222c")
  (brace-match-fg "#f1fa8c")
  (brace-match-bg "#44475a")
  (brace-bad-fg   "#ff5555")
  (brace-bad-bg   "#44475a")

  ;; Syntax colors
  (keyword     "#ff79c6" bold)
  (string      "#f1fa8c")
  (comment     "#6272a4" italic)
  (number      "#bd93f9")
  (type        "#8be9fd" italic)
  (builtin     "#8be9fd")
  (function    "#50fa7b")
  (operator    "#ff79c6")
  (preprocessor "#ff79c6")
  (constant    "#bd93f9")
  (error       "#ff5555")
  (doc-comment "#6272a4" italic)

  ;; Org-mode colors (optional)
  (org-heading-1 "#ff79c6" bold)
  (org-heading-2 "#bd93f9" bold)
  (org-heading-3 "#50fa7b" bold)
  (org-heading-4 "#f1fa8c" bold)
  (org-todo      "#ff5555" bold)
  (org-done      "#50fa7b" bold)
  (org-link      "#8be9fd" underline)
  (org-code-bg   "#21222c"))
```

#### Importing Themes — Three Approaches

**Approach A: Base16 YAML → gemacs converter (RECOMMENDED primary approach)**

- 250+ pre-made color schemes available at [tinted-theming/base16-schemes](https://github.com/tinted-theming/base16-schemes)
- Each scheme is a 16-color YAML file with fixed semantic roles
- Write a Gerbil script `base16-to-gemacs` that maps base16 colors to our ~20 slots:
  ```
  base00 → bg              base08 → error, constant
  base01 → line-number-bg  base09 → number, preprocessor
  base02 → selection       base0A → type
  base03 → comment         base0B → string
  base04 → line-number-fg  base0C → builtin
  base05 → fg, operator    base0D → function
  base06 → (unused)        base0E → keyword
  base07 → cursor          base0F → (deprecated markers)
  ```
- One-time conversion, then ship the result as `.gemacs-theme` files
- **Pro**: Instant access to 250+ themes. Simple YAML parsing.
- **Con**: Only 16 colors — some themes want more granularity.

**Approach B: Emacs `deftheme` elisp → gemacs converter**

- Parse the `custom-theme-set-faces` S-expression from `.el` files
- Extract `font-lock-*-face` colors and map to our semantic slots
- A restricted S-expression parser handles simple themes with literal hex colors
- For complex themes (doom-themes using `let*` + color functions), pre-resolve the palette by evaluating in Emacs and exporting the resolved faces:
  ```elisp
  ;; Run in Emacs to dump resolved face colors:
  (dolist (face '(font-lock-keyword-face font-lock-string-face ...))
    (let ((fg (face-attribute face :foreground)))
      (insert (format "%s %s\n" face fg))))
  ```
- **Pro**: Access to the full Emacs theme ecosystem.
- **Con**: Complex themes need Emacs to resolve; simple parser covers maybe 60% of themes.

**Approach C: VS Code JSON themes → gemacs converter**

- VS Code themes are pure JSON with `tokenColors` scope arrays
- Map TextMate scopes to our semantic slots:
  ```
  keyword → keyword
  string → string
  comment → comment
  constant.numeric → number
  entity.name.type → type
  entity.name.function → function
  ```
- **Pro**: 10,000+ themes. Already JSON, trivially parseable.
- **Con**: Scope mapping is imprecise; may need per-theme tweaks.

#### Implementation Steps

1. **Refactor highlight colors to be data-driven**:
   - Replace hardcoded `kw-r`/`kw-g`/`kw-b` variables in `highlight.ss` and `qt/highlight.ss` with a global `*current-theme*` hash table
   - `setup-lisp-styles!` reads colors from `*current-theme*` instead of constants
   - Same for all other language `setup-*-styles!` functions

2. **Define the `.gemacs-theme` file format** (S-expression as shown above)

3. **Theme loading**:
   - `load-theme!` reads a `.gemacs-theme` file, populates `*current-theme*`
   - Re-applies both UI chrome (existing `apply-theme!`) and syntax highlighting (new `apply-syntax-theme!`)
   - `apply-syntax-theme!` walks all open editors and calls `setup-*-styles!` to refresh colors

4. **Ship 8-10 built-in themes**:
   - Dark (current default), Light, Solarized Dark, Solarized Light, Monokai, Dracula, Nord, Gruvbox Dark, Catppuccin Mocha
   - Hand-tuned from official palettes for best results

5. **Write `base16-to-gemacs` converter script** for bulk importing

6. **`M-x load-theme` command**: Interactive theme selector with completion from `~/.gemacs-themes/` and built-in themes

7. **Persist theme choice** in `~/.gemacs-config` so it loads on startup

### Minimal Semantic Color Set (18 syntax + 12 UI = 30 slots)

| Category | Slots |
|----------|-------|
| **UI** | bg, fg, cursor, selection, line-hl, modeline-bg, modeline-fg, line-number-fg, line-number-bg, brace-match-fg, brace-match-bg, brace-bad-fg |
| **Syntax** | keyword, string, comment, number, type, builtin, function, operator, preprocessor, constant, error, doc-comment |
| **Org** | org-heading-1..4, org-todo, org-done, org-link, org-code-bg |
| **Modifiers** | Each syntax slot can optionally specify `bold`, `italic`, `underline` |

---

## Feature 3: Font Selection and Font Size

### Goal
Allow users to choose their preferred monospace font and default size, like Emacs `set-frame-font` / `customize-face default`.

### Current State
- Font is hardcoded to `"Monospace"` at 11pt in `qt/window.ss:72-73`
- Qt UI elements hardcode `font-family: monospace; font-size: 10pt` in stylesheets
- Zoom (C-=, C--, C-x C-0) changes size relative to base, but the base is fixed
- `*font-size*` global in `qt/commands-shell.ss` tracks current size but resets on restart

### Implementation Steps

1. **Add font configuration to `~/.gemacs-config`**:
   ```scheme
   (font-family "JetBrains Mono")
   (font-size 12)
   ```

2. **`M-x set-frame-font` command**:
   - Show completion list of available monospace fonts (query Qt for installed fonts)
   - Apply immediately to all open editors
   - Update `*font-family*` global

3. **`M-x set-font-size` command**:
   - Prompt for size (6-72 range)
   - Apply to all open editors and UI elements
   - Update `*font-size*` global and base size for zoom

4. **Startup font loading**:
   - On startup, read `font-family` and `font-size` from `~/.gemacs-config`
   - Apply to `STYLE_DEFAULT` before `STYLECLEARALL` in `qt-scintilla-setup-editor!`
   - Apply to all Qt stylesheets

5. **Persist font choice**:
   - Write font settings to `~/.gemacs-config` when changed via commands
   - Zoom level is NOT persisted (matches Emacs behavior)

6. **Qt implementation details**:
   - `qt-scintilla-setup-editor!` reads from `*font-family*` and `*font-size*` globals instead of hardcoded values
   - `theme-stylesheet` function interpolates current font into CSS
   - All `font-family: monospace; font-size: 10pt` in echo.ss, app.ss, etc. replaced with dynamic values

### Font Discovery
- Use Qt's `QFontDatabase` to enumerate installed monospace fonts
- Filter to monospace/fixed-width families
- Present as completion list in `set-frame-font`
- Will need a small FFI addition to `qt_shim` or use `qt-font-families` if available in gerbil-qt

---

## Priority Order

1. **Theme support** — highest visual impact, foundation for the other features
2. **Font selection** — quick win, small code change
3. **Magit testing** — important for correctness, can be done incrementally
