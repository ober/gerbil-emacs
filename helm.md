# Helm for Gemacs — Implementation Plan

## Overview

Port the core Emacs Helm experience to Gemacs: an incremental narrowing framework with multi-source composition, rich action system, and live preview. This replaces the current minibuffer Tab-cycling completion with a full candidate-list UI.

## Current State — COMPLETE ✅

All planned features have been implemented.

### What's implemented ✅

- **Core framework** (`helm.ss`): `helm-source`, `helm-session`, `helm-candidate` structs; multi-match engine; filtering + scoring; session management; action dispatch; match position highlighting; `*helm-current-pattern*` for volatile sources
- **Multi-match engine**: Space-separated AND tokens, `!` negation, `^` prefix matching, fuzzy mode per-source
- **TUI renderer** (`helm-tui.ss`): Candidate list in auto-resizing bottom rows (4–12), C-n/C-p/C-v/M-v navigation, RET/C-g/C-j, typing updates pattern, follow mode (C-c C-f), action menu (TAB), M-a mark-all, match character highlighting (yellow), styled source headers
- **Qt renderer** (`qt/helm-qt.ss`): Narrowing-based candidate panel, same navigation keys, echo-area integration
- **16 commands** registered in both TUI and Qt: helm-M-x, helm-mini, helm-buffers-list, helm-find-files, helm-occur, helm-imenu, helm-show-kill-ring, helm-bookmarks, helm-mark-ring, helm-register, helm-apropos, helm-grep, helm-man, helm-resume, helm-mode, toggle-helm-mode
- **14 built-in sources** (`helm-sources.ss`): commands, buffers, recent-files, buffer-not-found (create), files, occur, imenu, kill-ring, bookmarks, mark-ring, registers, apropos, grep, man
- **Follow mode**: Toggle with C-c C-f; auto-runs persistent action on C-n/C-p; per-source `follow?` flag; initialized from source's follow? field
- **Action menu**: TAB shows numbered action list from current source; pick by number or RET for default; C-g to cancel; executes on marked candidates if any
- **Candidate highlighting**: Matched characters shown in yellow (#xffcc00); brighter when selected; works for substring, prefix, and fuzzy matches
- **Auto-resize**: Helm window grows/shrinks between 4–12 rows based on candidate count + source headers
- **Source headers**: Styled separator lines with `─── Source Name ───` format, distinct background color
- **helm-grep**: Volatile source using `rg` (fallback `grep`); helm pattern becomes the search query (≥3 chars); results as `file:line:content`
- **helm-man**: Cached `man -k` results (up to 2000 entries); fuzzy filtering
- **helm-find-files (Qt)**: Full helm file browser using helm-source-files (no longer delegates to find-file)
- **Session resume**: `helm-resume` restores last session with pattern and candidates
- **Helm-mode toggle**: Rebinds M-x, C-x b, C-x C-b, M-y, C-x r b to helm equivalents
- **Functional tests**: 37 checks in TUI (`functional-test.ss`), 9 checks in Qt (`qt-functional-test.ss`)

---

## Architecture

### New Files

| File | Purpose | Lines (est.) |
|------|---------|-------------|
| `helm.ss` | Core framework: sources, matching, actions, session state | ~600 |
| `helm-tui.ss` | TUI renderer: candidate list in terminal rows | ~400 |
| `qt/helm-qt.ss` | Qt renderer: QListWidget or custom painting in a panel | ~500 |
| `helm-sources.ss` | Built-in sources: buffers, recentf, commands, files, imenu, bookmarks, kill-ring, marks | ~800 |
| `helm-commands.ss` | TUI `cmd-helm-*` command functions | ~500 |
| `qt/helm-commands.ss` | Qt `cmd-helm-*` command functions | ~500 |

### Core Data Model (`helm.ss`)

```
helm-source
  name          : string          ; display header
  candidates    : (-> list)       ; thunk producing candidates
  match-fn      : (-> string candidate bool)  ; custom match (default: multi-match)
  filter-fn     : (-> string list list)       ; custom filter+sort
  actions       : alist           ; ((label . procedure) ...)
  persistent-action : (or procedure #f)       ; C-j preview
  display-fn    : (or (-> candidate string) #f)  ; custom display
  real-fn       : (or (-> candidate any) #f)     ; display->real for actions
  fuzzy?        : bool            ; enable fuzzy matching
  volatile?     : bool            ; rebuild candidates on every pattern change
  candidate-limit : integer       ; max candidates (default 100)
  keymap        : (or keymap #f)  ; source-local keymap override

helm-session
  sources       : list            ; active helm-source objects
  pattern       : string          ; current input
  candidates    : vector          ; filtered candidates (with source tags)
  selected      : integer         ; cursor index
  marked        : set             ; marked candidate indices
  buffer-name   : string          ; for resume
  actions       : alist           ; current action list (from selected source)
  scroll-offset : integer         ; visible window start
  follow?       : bool            ; auto-preview on navigate
  alive?        : bool            ; session running

helm-candidate
  display       : string          ; what the user sees
  real          : any             ; what actions receive
  source        : helm-source     ; which source produced this
```

### Matching Engine

The default matching mode is **multi-match**: space-separated tokens, all must match (AND logic).

```
Pattern         Behavior
─────────────────────────────────────────────
"foo bar"       AND: both "foo" and "bar" must match
"!test"         NOT: exclude candidates matching "test"
"^init"         PREFIX: must start with "init"
"foo !test ^s"  Combined: starts with "s", contains "foo", excludes "test"
```

When `fuzzy?` is `#t` on a source, each token uses `fuzzy-match?` instead of substring.

Implementation: extend `fuzzy-filter-sort` in `core.ss` with `helm-multi-match` that parses tokens and applies the matching rules.

### Action System

```
RET       Execute default action (first in alist) on selected/marked candidates
TAB       Show action menu in minibuffer — pick an alternative action
C-j       Execute persistent-action (preview) — session stays open
C-SPC     Mark/unmark candidate
M-a       Mark all visible
C-g       Quit — restore original state
```

### Session Lifecycle

1. `helm-run` called with sources, buffer-name, optional initial-input
2. Candidates computed for each source
3. Filtered by pattern; grouped by source with headers
4. Rendered in candidate list UI (TUI: terminal rows; Qt: widget panel)
5. User types → pattern updates → re-filter → re-render (debounced)
6. Navigation: C-n/C-p move cursor; C-v/M-v page; C-o next source
7. On RET: run default action on selected (or all marked); close session
8. On C-j: run persistent-action; keep session open
9. On C-g: abort; cleanup
10. Session stored in `*helm-sessions*` for `helm-resume`

### Rendering

#### TUI (`helm-tui.ss`)

Use the bottom N rows of the terminal (configurable, default: 10). Layout:

```
──────────────────────────────────────
 Buffers:                              ← source header
 > *scratch*                           ← selected (highlighted)
   main.ss
   core.ss
 Recent Files:                         ← source header
   ~/notes.org
   ~/.gemacs-init
──────────────────────────────────────
 Pattern: scr  [2/47]                  ← minibuffer + count
```

Rendering uses `tui-print!` at absolute row positions. The editor viewport shrinks by N rows during a helm session.

#### Qt (`qt/helm-qt.ss`)

Use a QListWidget (or custom-painted QWidget) docked below the editor area, above the echo line. Source headers are non-selectable list items with distinct styling. Selected candidate highlighted with cursor color.

Layout mirrors TUI but uses Qt stylesheets for coloring and font.

---

## Implementation Phases

### Phase 1: Core Framework + M-x (MVP) ✅ DONE

**Goal**: Replace M-x with a helm-style candidate list. This validates the entire rendering pipeline.

#### 1.1 Core data model (`helm.ss`) ✅

- Define `helm-source`, `helm-session`, `helm-candidate` structs
- `helm-multi-match`: parse space-separated tokens with `!`/`^` prefixes
- `helm-filter`: apply match to candidates, score, sort, limit
- `helm-run`: main entry point — create session, invoke renderer, handle result
- `helm-resume`: restore last session

#### 1.2 TUI renderer (`helm-tui.ss`) ✅

- `helm-tui-render!`: draw candidate list in bottom rows
- `helm-tui-input-loop!`: key handling (C-n, C-p, C-v, M-v, RET, C-g, C-j, TAB, C-SPC, typing)
- `helm-tui-resize!`: adjust editor viewport when helm opens/closes
- Input debouncing: re-filter after short delay (or on every keystroke if fast enough)

#### 1.3 Qt renderer (`qt/helm-qt.ss`) ✅

- `helm-qt-render!`: draw candidate list in a QWidget panel
- `helm-qt-input-loop!`: key handling via Qt event filter
- `helm-qt-resize!`: adjust splitter/layout when helm opens/closes

#### 1.4 helm-M-x ✅

- Source: `helm-source-commands` — candidates from `*all-commands*` hash, MRU ordered
- Display: command name + key binding (if any)
- Action: `execute-command!`
- Persistent action: show command docstring in echo area
- Register as `helm-M-x` command; optionally bind to `M-x` when helm-mode is on

### Phase 2: Buffers + Recent Files + Mini ✅ DONE

**Goal**: The quintessential Helm experience — multiple sources in one session.

#### 2.1 Buffer source ✅

- `helm-source-buffers`: candidates from `*buffer-list*` in MRU order
- Display: buffer name + modified indicator + mode + file path
- Filters: `*mode` (major mode), `@text` (buffer content search), `/path` (directory), `!exclude`
- Actions: switch-to-buffer (default), kill-buffer, save-buffer, diff
- Persistent action: show buffer in other window (preview)

#### 2.2 Recent files source ✅

- `helm-source-recentf`: candidates from `*recent-files*`
- Display: abbreviated file path
- Actions: find-file (default), find-file-other-window
- Persistent action: preview file content

#### 2.3 Create buffer source ✅

- `helm-source-buffer-not-found`: dummy source — user input becomes a new buffer name
- Action: echoes creation intent; when no candidates match, RET returns pattern text for create-buffer handling

#### 2.4 Compose ✅

- `helm-mini`: combines buffers + recentf + create-buffer
- `helm-buffers-list`: buffers source only (replaces current stub)

### Phase 3: File Navigation ✅ DONE

**Goal**: Replace `find-file` with a helm-style file browser.

#### 3.1 File source ✅

- `helm-source-files`: candidates from directory listing
- Display: filename (dirs with trailing `/`)
- Navigation: C-j descends into directory, C-l / DEL ascends to parent
- Fuzzy matching on filenames
- Tilde expansion, environment variables
- Wildcard support: `*.ss` glob filtering
- Actions: find-file (default), find-file-other-window, dired
- Persistent action: preview file content (first N lines in echo or split)

#### 3.2 Integration ✅

- `helm-find-files`: entry point with initial directory from current buffer (TUI and Qt)
- Bind to `C-x C-f` when helm-mode is on

### Phase 4: Search Commands ✅ DONE

**Goal**: Interactive search and navigation within and across buffers.

#### 4.1 helm-occur ✅

- `helm-source-occur`: candidates = lines of current buffer, numbered
- Live narrowing: re-filter lines as you type
- Actions: goto-line (default), save-occur-buffer
- Persistent action: jump to line without closing
- Follow mode natural here — navigate lines, see cursor move in buffer

#### 4.2 helm-imenu ✅

- `helm-source-imenu`: candidates = definitions in current buffer (functions, variables, classes)
- Parse via existing highlighting/definition infrastructure or simple regex
- Display: symbol name + kind tag (`[fn]`, `[var]`, `[struct]`)
- Actions: goto-definition (default)
- Persistent action: jump to definition without closing

#### 4.3 helm-grep ✅

- `helm-source-grep`: volatile source — runs `rg` (fallback `grep`) as subprocess
- Pattern typed in helm becomes the grep pattern via `*helm-current-pattern*`
- Candidates rebuild on every pattern change (volatile source, ≥3 chars)
- Display: `file:line: content`
- Actions: open-file-at-line (default)
- Uses synchronous `open-process` + `read-line` (fast enough with `--max-count=200`)

### Phase 5: Utility Commands ✅ DONE

**Goal**: Common Emacs Helm commands for daily use.

#### 5.1 helm-show-kill-ring ✅

- `helm-source-kill-ring`: candidates from kill ring
- Display: truncated text with line count indicator
- Actions: insert-at-point (default), append-to-kill-ring
- Multi-line candidate display for readability

#### 5.2 helm-bookmarks ✅

- `helm-source-bookmarks`: candidates from `*bookmarks*` hash
- Display: bookmark name + file path + position
- Actions: jump (default), delete-bookmark, rename-bookmark

#### 5.3 helm-mark-ring ✅

- `helm-source-mark-ring`: candidates from buffer mark ring and global mark ring
- Display: line content at mark position
- Actions: jump-to-mark (default)
- Persistent action: show mark position

#### 5.4 helm-register ✅

- `helm-source-registers`: candidates from register hash
- Display: register char + content preview
- Actions: insert (default), jump (for position registers)

#### 5.5 helm-apropos ✅

- Combines: command source + variable source + function source
- Multi-source: all matching symbols across categories
- Actions: describe (default), execute (commands), set (variables)

#### 5.6 helm-man ✅

- `helm-source-man`: candidates from cached `man -k .` output (up to 2000 entries)
- Fuzzy filtering on man page names
- Actions: open man page (default) — parses "name (section)" format

### Phase 6: Follow Mode + Resume + Polish ✅ DONE

**Goal**: Advanced Helm features that complete the experience.

#### 6.1 Follow mode ✅

- Toggle with `C-c C-f` during any helm session
- When on: persistent-action fires automatically on C-n/C-p navigation
- Configurable delay (`*helm-follow-delay*`) to avoid thrashing
- Per-source `follow?` slot; session inherits from source

#### 6.2 helm-resume ✅

- Store sessions in `*helm-sessions*` alist (buffer-name → session snapshot)
- `helm-resume`: reopen last session with pattern and cursor position intact
- `C-c n` during session: cycle through resumable sessions
- Limit stored sessions (default: 10)

#### 6.3 Polish ✅

- Candidate highlighting: matched characters shown in yellow (#xffcc00), brighter when selected
- Source headers: styled `─── Source Name ───` separators with distinct background color
- Candidate count in prompt: `[3/47]`
- Auto-resize: helm window grows/shrinks between 4–12 rows based on candidate + source count
- Mode-line integration: `*helm-mode*` flag in core.ss
- Filtering on every keystroke (fast for sync sources; volatile sources rebuild on pattern change)

---

## Key Bindings

When `helm-mode` is active, these bindings override defaults:

| Binding | Command | Replaces |
|---------|---------|----------|
| `M-x` | `helm-M-x` | `execute-extended-command` |
| `C-x b` | `helm-mini` | `switch-buffer` |
| `C-x C-f` | `helm-find-files` | `find-file` |
| `C-x C-b` | `helm-buffers-list` | `list-buffers` |
| `M-y` | `helm-show-kill-ring` | `yank-pop` |
| `C-x r b` | `helm-bookmarks` | `bookmark-jump` |
| `C-c h o` | `helm-occur` | — |
| `C-c h i` | `helm-imenu` | `imenu` |
| `C-c h a` | `helm-apropos` | `apropos-command` |
| `C-c h g` | `helm-grep` | `grep` |
| `C-c h m` | `helm-man` | `man` |
| `C-c h b` | `helm-resume` | — |
| `C-c h SPC` | `helm-mark-ring` | — |
| `C-c h r` | `helm-register` | — |

### Inside a Helm Session

| Key | Action |
|-----|--------|
| `C-n` / `<down>` | Next candidate |
| `C-p` / `<up>` | Previous candidate |
| `C-v` / `<next>` | Page down |
| `M-v` / `<prior>` | Page up |
| `M-<` | First candidate |
| `M->` | Last candidate |
| `C-o` | Next source |
| `RET` | Default action + close |
| `C-j` | Persistent action (keep open) |
| `TAB` | Action menu |
| `C-SPC` | Mark/unmark candidate |
| `M-a` | Mark all |
| `C-g` | Quit |
| `C-c C-f` | Toggle follow mode |

---

## Feature Parity: TUI and Qt

Per project rules, every helm feature must work in both TUI and Qt. The architecture achieves this through:

1. **Shared core** (`helm.ss`): All data model, matching, filtering, session management, action dispatch
2. **Renderer interface**: TUI and Qt each implement `helm-render!`, `helm-input-loop!`, `helm-resize!`
3. **Shared sources** (`helm-sources.ss`): Source definitions are backend-agnostic; they reference `*buffer-list*`, `*recent-files*`, `*all-commands*` etc. from `core.ss`
4. **Separate command files**: `helm-commands.ss` (TUI) and `qt/helm-commands.ss` (Qt) wire sources to their respective renderers

---

## Dependencies on Existing Code

| Existing module | What helm uses |
|----------------|----------------|
| `core.ss` | `fuzzy-match?`, `fuzzy-score`, `*all-commands*`, `*buffer-list*`, `buffer-by-name`, `execute-command!`, `keymap-*`, `echo-state` |
| `echo.ss` / `qt/echo.ss` | Echo area for messages during helm (e.g. action descriptions) |
| `persist.ss` | `*recent-files*`, `*bookmarks*`, `*kill-ring*`, `*mark-ring*` |
| `editor-core.ss` / `qt/commands-core.ss` | Existing buffer/file operations called by helm actions |
| `highlight.ss` | Definition parsing for imenu source |

---

## Estimated Effort by Phase

| Phase | What | New LOC | Complexity |
|-------|------|---------|-----------|
| 1 | Core + M-x | ~1500 | High (framework + two renderers) |
| 2 | Buffers + Recentf + Mini | ~400 | Medium |
| 3 | Find-files | ~500 | Medium-High (directory navigation state) |
| 4 | Occur + Imenu + Grep | ~600 | Medium-High (grep needs async) |
| 5 | Kill-ring, Bookmarks, Marks, Registers, Apropos, Man | ~500 | Low-Medium |
| 6 | Follow + Resume + Polish | ~300 | Medium |
| **Total** | | **~3800** | |

---

## Design Decisions

### 1. Rendering approach

**TUI**: Reserve bottom N terminal rows. The editor viewport (`tui-draw!`) must account for reduced height. This is the simplest approach — no popup windows, no separate buffer.

**Qt**: Insert a QWidget (QListWidget or custom) between the editor area and the echo line. Use a QVBoxLayout with show/hide. This avoids creating separate windows.

### 2. Input handling

Helm takes over the keyboard during a session. Both TUI and Qt must intercept all keystrokes and route them through the helm keymap before falling through to normal editing. The helm session runs a modal input loop that blocks normal editing until completion.

### 3. Async sources

For grep/locate/find, use Gerbil's `open-process` + green threads. A reader thread populates candidates incrementally; the renderer polls for new candidates on a timer. This avoids blocking the UI.

### 4. Candidate storage

For sync sources: plain list filtered on each keystroke (fast for <10K candidates).
For in-buffer sources: string buffer with line-based search (for very large candidate sets like locate results).
For async sources: growing list appended by reader thread.

### 5. Multi-match as default

Unlike Emacs Helm where multi-match is opt-out, here it's always on. Space separates AND tokens. This is the most useful default. Fuzzy matching is opt-in per source.

### 6. No helm-mode complexity

Rather than a full `helm-mode` that overrides all completion, implement individual `helm-*` commands that can be bound to keys. The `helm-mode` toggle simply rebinds the standard keys to their helm equivalents. This is simpler and less error-prone.

---

## Testing Strategy

### Unit tests (in `emacs-test.ss`)

- `helm-multi-match` parsing and matching logic
- `helm-filter` with various patterns and source types
- Source candidate generation (buffers, commands, files)
- Action dispatch (default, persistent, marked)
- Session create/resume lifecycle

### Functional tests (in `functional-test.ss` and `qt-functional-test.ss`)

Per CLAUDE.md rules, all tests go through `execute-command!`:

```scheme
;; Test helm-M-x dispatches correctly
(set! *test-helm-responses* '("switch-buffer"))
(execute-command! app 'helm-M-x)
;; Verify the command was executed

;; Test helm-mini with buffer switching
(set! *test-helm-responses* '("*scratch*"))
(execute-command! app 'helm-mini)
;; Verify buffer switched
```

### Manual testing

- Verify rendering in both TUI (terminal) and Qt (window)
- Test with large candidate sets (1000+ commands, 100+ buffers)
- Test async grep with large codebases
- Test follow mode responsiveness

---

## Out of Scope (Future Work)

These Helm features are not included in this plan but could be added later:

- **helm-find-files file operations**: Copy, rename, delete, symlink from the file browser
- **helm-top**: Process management UI
- **helm-google-suggest**: Web search integration
- **helm-colors / helm-ucs**: Color/unicode pickers
- **Childframe display**: Floating popup frame (Qt could do this with QDialog)
- **helm-org**: Org-specific navigation (headings, agenda)
- **Async candidate highlighting**: Highlighting match chars in async sources in real-time
- **helm-descbinds**: Searchable key binding reference
- **Project-wide sources**: helm-projectile equivalent (project files, project buffers)
