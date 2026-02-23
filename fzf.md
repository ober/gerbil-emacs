# FZF Equivalent Functionality for Gemacs

**Goal:** Bring fzf-grade fuzzy finding to gemacs — both as an internal helm source and as an integration layer for external fzf when available.

## Current State

### What gemacs already has
- **Helm framework** (`helm.ss`): multi-match engine with `!negate`, `^prefix`, substring, and fuzzy modes
- **Fuzzy matching** (`core.ss`): `fuzzy-match?`, `fuzzy-score`, `fuzzy-filter-sort`
- **13 helm sources**: commands, buffers, recent files, files, occur, imenu, kill-ring, bookmarks, mark-ring, registers, apropos, grep, man
- **TUI renderer** (`helm-tui.ss`): modal candidate list with real-time narrowing
- **Qt renderer** (`qt/helm-qt.ss`): modal dialog with search + list

### What's missing (fzf parity gaps)

| fzf Feature | Gemacs Status | Priority |
|---|---|---|
| Shell command history (Ctrl-R) | **No history file at all** | P0 |
| File finder (Ctrl-T) | helm-source-files exists (single dir only) | P1 |
| Directory jump (Alt-C) | Not implemented | P2 |
| Scoring scheme (Smith-Waterman) | Basic sequential match only | P2 |
| Extended search syntax | Partial (`!`, `^`, substring) — missing `$suffix`, `'exact`, `\|` OR | P2 |
| Preview pane | Not implemented | P2 |
| Streaming/chunked input | Helm loads all candidates up front | P3 |
| Multi-select (Tab/Shift-Tab) | Helm has marks but not toggle-style | P3 |
| HTTP API (`--listen`) | Not applicable | — |
| Tmux integration | Not applicable | — |

---

## Phase 0: Shell History (BLOCKING — required for everything else)

### Problem
Neither eshell nor shell-mode persist command history. There is no `~/.gsh_history` file. The only persisted history is `~/.gemacs-history` which stores M-x command names, not shell commands.

### Solution: `~/.gsh_history` with timestamps

**File format** — tab-separated, one entry per line:
```
UNIX_EPOCH\tCWD\tCOMMAND
```

Example:
```
1708700000\t/home/user/project\tgit status
1708700015\t/home/user/project\tmake build
1708700030\t/home/user\tcd mine/gerbil-emacs
```

**Why this format:**
- UNIX epoch is compact, sortable, and trivially parseable (`string->number`)
- Tab-separated is unambiguous (commands may contain spaces but rarely tabs)
- CWD enables directory-scoped history search (fzf's `--scheme=history` equivalent)
- One line per entry makes streaming/grep trivial
- No quoting needed for the command field (it's the last field, so take everything after second tab)

### Implementation

**New file: `shell-history.ss`** — backend-agnostic, no TUI/Qt imports.

Exports:
- `*gsh-history*` — in-memory ring buffer (vector + head/tail indices)
- `*gsh-history-file*` — `"~/.gsh_history"`
- `*gsh-history-max*` — max entries (default: 10000)
- `gsh-history-add! command cwd` — append entry with current timestamp
- `gsh-history-save!` — flush to disk (append-only, then truncate on threshold)
- `gsh-history-load!` — load from disk into ring buffer
- `gsh-history-search pattern` — fuzzy search, returns list of `(timestamp cwd command)` tuples sorted by recency
- `gsh-history-recent n` — last N entries
- `gsh-history-for-cwd cwd n` — last N entries from a specific directory
- `gsh-history-deduplicate` — collapse consecutive duplicates, keeping most recent timestamp

**Integration points:**
1. `eshell-process-input` (both TUI and Qt) — call `gsh-history-add!` after successful command execution
2. `shell-send!` — call `gsh-history-add!` when sending commands to PTY shell
3. Startup — call `gsh-history-load!` during init
4. Shutdown / periodic — call `gsh-history-save!`

**Disk I/O strategy:**
- On `gsh-history-add!`: append single line to file immediately (O(1) write)
- On startup: read entire file, deduplicate, load into memory
- Periodic compaction: when file exceeds 2x `*gsh-history-max*` lines, rewrite with only the latest `*gsh-history-max*` entries

---

## Phase 1: Helm History Source (Ctrl-R equivalent)

### `helm-source-shell-history`

A new helm source that searches `*gsh-history*` with fuzzy matching.

**Candidate format:**
```
[timestamp] command                              (cwd)
```

**Features:**
- Fuzzy matching on the command text
- Recency bonus in scoring (newer entries score higher)
- Optional CWD filtering: when invoked from an eshell buffer, default to showing history for that directory first
- Deduplication: show each unique command once, with the most recent timestamp
- Action: insert selected command at the eshell prompt (or yank into minibuffer)

**Keybinding:** `C-r` in eshell/shell buffers, `M-x helm-shell-history` globally.

### Eshell history navigation (Up/Down)

In eshell buffers, Up/Down arrow at the prompt should cycle through history (like bash). This doesn't need helm — just index into `*gsh-history*` filtered to the current prefix.

---

## Phase 2: Recursive File Finder (Ctrl-T equivalent)

### `helm-source-find-files`

Upgrade `helm-source-files` from single-directory listing to recursive project-wide search.

**Implementation:**
- Walk from project root (detect via `.git`, `gerbil.pkg`, etc.)
- Skip `.git`, `node_modules`, `.gerbil`, `__pycache__` directories
- Use `--scheme=path` scoring: bonus for basename match, path separator awareness
- Candidate format: relative path from project root
- Limit: 50,000 files max (stream chunks of 256 like fzf)

**Keybinding:** `C-x C-t` or `C-c f` (find-file-in-project).

### `helm-source-find-directories`

Same walker but directories only. Action: `cd` (in eshell) or `dired` (in editor).

**Keybinding:** `M-x helm-find-dir` or `C-c d`.

---

## Phase 3: Improved Scoring Algorithm

### Current: sequential char matching
The existing `fuzzy-score` in `core.ss` does basic sequential character matching. It works but doesn't produce fzf-quality rankings.

### Target: fzf V2 scoring (simplified)

Port the key scoring heuristics from fzf without the full Smith-Waterman DP:

1. **Boundary bonus** (+8): match at word boundary (after `/`, `-`, `_`, `.`, whitespace)
2. **CamelCase bonus** (+7): match at case transition
3. **Consecutive bonus** (+4): sequential characters in the match
4. **First-char multiplier** (2x): extra weight for matching the first pattern character at a boundary
5. **Gap penalty** (-3 start, -1 extend): penalize gaps between matches
6. **Path scheme**: bonus for matching in the basename vs directory components

This doesn't need the full DP matrix — a greedy forward+backward scan (fzf V1 style) with the bonus system handles 95% of cases.

### Extended search syntax

Complete the missing operators:
- `foo$` — suffix match (line ends with foo)
- `'foo` — exact substring match (disable fuzzy)
- `foo | bar` — OR logic between terms
- Current `!foo` (negate) and `^foo` (prefix) already work

---

## Phase 4: Preview Pane

### Design

Add an optional preview area to the helm UI (both TUI and Qt).

**TUI:** Split the bottom panel — left side shows candidates, right side shows preview content. Use Scintilla's multi-view or a second editor widget.

**Qt:** Add a `QTextEdit` or `QScintilla` widget to the right of the helm dialog's candidate list.

**Preview content by source:**
| Source | Preview |
|---|---|
| Files | First 50 lines of file content |
| Buffers | Current content snapshot |
| Shell history | Full command with timestamp, CWD, and frequency count |
| Occur | Surrounding context (5 lines above/below) |
| Grep | File content around the match |
| Imenu | Function body |
| Kill ring | Full kill text |

**Keybinding:** `C-j` to toggle preview in helm session (already partially wired in Qt helm).

---

## Phase 5: Streaming & Performance

### Chunked candidate loading

For large inputs (file trees, grep results), load candidates in chunks instead of all at once:

1. Source provides an iterator/generator instead of a list
2. Helm engine processes chunks of 256 candidates
3. Display updates as chunks arrive (show "N/M" progress)
4. Matcher runs on available chunks, merges results

### Parallel matching

For large candidate sets (>10k), split into partitions and match in parallel using Gerbil threads. Merge results by score.

### Performance targets
- <50ms to first results for <1000 candidates
- <200ms for 50,000 file paths
- <500ms for 100,000 grep results

---

## Phase 6: External fzf Integration (Optional)

If the user has `fzf` installed, provide a bridge:

- `M-x fzf` — launch external fzf in a terminal buffer with the current project's files
- `M-x fzf-grep` — pipe grep results through fzf
- `M-x fzf-history` — search `~/.gsh_history` with external fzf

This is a nice-to-have escape hatch but not the primary goal — the helm-native implementation should be the default.

---

## File Map

```
shell-history.ss          — NEW: persistent shell history engine
helm-sources.ss           — MODIFY: add helm-source-shell-history, helm-source-find-files
helm.ss                   — MODIFY: extend multi-match with $suffix, 'exact, | OR
core.ss                   — MODIFY: upgrade fuzzy-score with boundary/gap scoring
eshell.ss                 — MODIFY: call gsh-history-add! on command execution
shell.ss                  — MODIFY: call gsh-history-add! on shell-send!
persist.ss                — MODIFY: wire gsh-history-load!/save! into startup/shutdown
editor-core.ss            — MODIFY: add Ctrl-R binding, Up/Down history in eshell
qt/commands-edit.ss       — MODIFY: add Ctrl-R binding, Up/Down history in eshell
helm-tui.ss               — MODIFY (Phase 4): add preview pane
qt/helm-qt.ss             — MODIFY (Phase 4): add preview widget
```

---

## Implementation Order

1. **Phase 0**: `shell-history.ss` + wire into eshell/shell — gets us persistent `~/.gsh_history`
2. **Phase 1**: `helm-source-shell-history` + Ctrl-R + Up/Down — usable history search
3. **Phase 2**: Recursive file finder — Ctrl-T equivalent
4. **Phase 3**: Scoring upgrade — better ranking quality
5. **Phase 4**: Preview pane — visual context
6. **Phase 5**: Streaming — scale to large projects
7. **Phase 6**: External fzf bridge — optional power-user feature
