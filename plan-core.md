# Gemacs Customization System — Implementation Plan

**Goal**: Bring gemacs customization to Emacs-level by implementing Option A:
`defvar` with metadata + mode keymaps in core + comprehensive hooks.

## Phase 1: `defvar!` — Variable Registry with Metadata

**Status**: COMPLETE

### What exists today
- ~30 scattered `(def *var-name* value)` globals across core.ss, persist.ss, editor-extra-helpers.ss, editor-cmds-a.ss
- A stub `*custom-variables*` hash in editor-extra-helpers.ss (name → value only)
- A stub `*custom-groups*` hash with 7 hardcoded group entries
- Init file (`~/.gemacs-init`) directly sets globals via `set!`

### What we're building
A centralized variable registry where every customizable setting has:
- **name** (symbol): e.g. `'scroll-margin`
- **default**: the factory default value
- **value**: current value (starts as default)
- **docstring**: human-readable description
- **type**: one of `'boolean`, `'integer`, `'string`, `'symbol`, `'choice`, `'sexp`
- **type-args**: for `'integer` → `(min . max)`, for `'choice` → list of valid values
- **group**: symbol grouping (e.g. `'editing`, `'display`, `'files`)

### Implementation

1. **New module: `customize.ss`** — placed early in build order (after core, before persist)
   - `defvar!` procedure: registers a variable with metadata
   - `*custom-registry*` hash: symbol → custom-var struct
   - `custom-get` / `custom-set!`: type-validated access
   - `custom-reset!`: reset to default
   - `custom-list-group`: list all vars in a group
   - `custom-list-all`: list all registered vars
   - `custom-describe`: return docstring + current value + type

2. **Migrate existing globals**: Replace scattered `*var*` globals with `defvar!` calls:
   - `*scroll-margin*` → `(defvar! 'scroll-margin 0 "Lines of margin at top/bottom" type: 'integer type-args: '(0 . 20) group: 'display)`
   - `*save-place-enabled*` → `(defvar! 'save-place #f "Remember cursor positions" type: 'boolean group: 'files)`
   - etc. for all settings currently in `~/.gemacs-init`

3. **Update init file parser** (persist.ss): use `custom-set!` instead of `set!` for known variables

4. **Commands**: `M-x describe-variable`, `M-x set-variable`, `M-x customize-group`

### Files modified
- New: `customize.ss`
- Modified: `build.ss` (add module), `core.ss` (export), `persist.ss` (init-file-load!), editor commands for describe/set UI

---

## Phase 2: Mode Keymaps in Core

**Status**: COMPLETE

### What exists today
- Qt side: `*mode-keymaps*` hash in `qt/commands-config.ss` with ~15 mode-specific keymaps (dired, magit, compilation, etc.)
- TUI side: No mode keymaps — all bindings go in `*global-keymap*`
- Major mode detection: `detect-major-mode` in persist.ss maps file extensions → mode symbols
- Minor mode flags: `*mode-flags*` hash with simple boolean toggle

### What we're building
Move `*mode-keymaps*` to core so both TUI and Qt share the same mode keymap infrastructure.

### Implementation

1. **Move `*mode-keymaps*` to core.ss**
   - Add `*mode-keymaps*` hash table
   - Add `mode-keymap-set!` / `mode-keymap-get` accessors
   - Add `define-mode-keys!` helper: `(define-mode-keys! 'dired-mode ("g" 'revert-buffer) ("q" 'quit-window))`

2. **Keymap lookup chain** in core.ss
   - `keymap-lookup-chain`: given current major mode + active minor modes, look up a key through: active minor mode maps → major mode map → global map
   - This replaces the current flat `keymap-lookup` in the dispatch path

3. **Wire into TUI dispatch** (app.ss / keymap.ss)
   - The TUI key handler currently does `keymap-lookup *global-keymap*`
   - Update to use `keymap-lookup-chain` with the current buffer's major mode

4. **Wire into Qt dispatch** (qt/commands-config.ss)
   - Qt already has mode keymap lookup — refactor to use shared core `keymap-lookup-chain`
   - Move mode keymap definitions that are shared (dired, compilation, grep, etc.) to core

5. **Mode hooks**: `(add-hook! 'python-mode-hook fn)` — run when entering a major mode

### Files modified
- Modified: `core.ss` (add mode keymap infrastructure), `app.ss` (TUI dispatch), `qt/commands-config.ss` (refactor to shared core)

---

## Phase 3: Comprehensive Hooks

**Status**: Not started

### What exists today
- Hook system in core.ss: `*hooks*`, `add-hook!`, `remove-hook!`, `run-hooks!`
- Only 1 hook wired: `*post-buffer-attach-hook*` (and it's a single lambda, not even using the hook system)

### What we're building
Wire hooks into every major lifecycle event, matching Emacs conventions:

### Hook list (with where to add `run-hooks!` calls)

| Hook name | When fired | Location |
|-----------|-----------|----------|
| `after-init-hook` | After init file loaded | app.ss, qt/app.ss |
| `before-save-hook` | Before saving buffer | editor-core.ss (save fn) |
| `after-save-hook` | After saving buffer | editor-core.ss (save fn) |
| `find-file-hook` | After opening a file | editor-core.ss (find-file fn) |
| `kill-buffer-hook` | Before killing a buffer | editor commands (kill-buffer) |
| `after-change-major-mode-hook` | After major mode set | mode activation code |
| `window-configuration-change-hook` | After split/delete/switch window | window management |
| `minibuffer-setup-hook` | When minibuffer activated | helm/echo |
| `minibuffer-exit-hook` | When minibuffer deactivated | helm/echo |
| `<mode>-mode-hook` | When mode activated | mode activation code |

### Implementation

1. **Add `run-hooks!` calls** at each lifecycle point listed above
2. **Convert `*post-buffer-attach-hook*`** from single-lambda to use the hook system
3. **Mode hooks**: When `detect-major-mode` returns e.g. `'python-mode`, run `(run-hooks! 'python-mode-hook)`
4. **Document all hooks** in the customize system (register each hook name with a docstring)

### Files modified
- Modified: `core.ss`, `editor-core.ss`, `editor-cmds-a.ss` or `editor-cmds-b.ss`, `app.ss`, `qt/app.ss`, `qt/commands-config.ss`

---

## Phase 4: Commands and UI

**Status**: Not started

### Commands to add (both TUI and Qt)

| Command | Key | Description |
|---------|-----|-------------|
| `describe-variable` | `C-h v` | Show variable's value, default, type, docstring |
| `describe-function` | `C-h f` | Show command's docstring and keybinding |
| `set-variable` | `M-x set-variable` | Set a customizable variable interactively |
| `customize-group` | `M-x customize-group` | Browse and set variables by group |
| `describe-mode` | `C-h m` | Show current major/minor modes and their keymaps |

### Implementation
- Use existing helm/completion framework for variable/group selection
- Display in a read-only `*Help*` buffer (existing pattern)
- Both TUI and Qt get the same commands (feature parity)

### Files modified
- TUI: `editor-cmds-a.ss` or `editor-cmds-b.ss`
- Qt: `qt/commands-config.ss` or new `qt/commands-customize.ss`
- Both: keybinding registration in `setup-default-bindings!`

---

## Build Order

```
Phase 1 → Phase 2 → Phase 3 → Phase 4
  ↓          ↓          ↓          ↓
commit     commit     commit     commit
```

Each phase must pass `make build`, `make test`, `make test-qt`, and `make static-qt` before committing.
