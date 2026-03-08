# Gemacs Development Plan

## Completed: Autocomplete Popups & Diagnostic Tooltips (`cd07ae3`)

Implemented popup UX matching Ruby Scintilla editors — inline autocomplete dropdown at cursor and floating diagnostic tooltips on hover.

- **Qt idle autocomplete** — Rewrote `lsp-auto-complete!` to use Scintilla native `SCI_AUTOCSHOW` (replaced broken `QCompleter` approach). Added `sci-word-prefix` using `SCI_WORDSTARTPOSITION`. Popup appears on 500ms idle with 3+ char prefix.
- **Qt diagnostic calltips** — Added `lsp-diagnostic-calltip!` using `SCI_CALLTIPSHOW` with severity-colored backgrounds (red/yellow/blue/grey). Auto-shows/hides on idle cursor movement over diagnostic lines.
- **TUI calltip helpers** — Added `show-calltip!`/`cancel-calltip!` in `editor-text.ss`, ready for future TUI LSP integration.
- **gerbil-scintilla** (`ce04f66`) — Added `SCI_WORDSTARTPOSITION`, `SCI_WORDENDPOSITION`, `SCI_ISRANGEWORD`, `SCI_SETMOUSEDWELLTIME`, `SCI_GETMOUSEDWELLTIME` constants.

---

## Architecture Notes

### TUI Registration Scopes (3-tier)
1. **`editor.ss`** (facade) — has access to all modules; for cross-chain aliases
2. **`editor-extra.ss`** — editor-extra chain scope
3. **`editor-extra-regs.ss`** — editor-cmds chain scope

### Qt Registration Scopes (2-tier)
1. **`qt/commands-aliases.ss`** — chain scope (after commands-parity, before facade)
2. **`qt/commands.ss`** (facade) — for forward-ref functions like `cmd-quit`

### File Size Status
All Qt command files are under the 2000-line limit after comprehensive splits.
26 command files total (13 pairs). Chain order:
```
core → core2 → edit → edit2 → search → search2 → file → file2 →
sexp → sexp2 → ide → ide2 → vcs → vcs2 → lsp → shell → shell2 →
modes → modes2 → config → config2 → parity → parity2 →
aliases → aliases2 → facade
```
Largest files: modes (~1851), lsp (~1802), search (~1778)
| TUI File | Lines | Status |
|----------|-------|--------|
| `editor-extra-media.ss` | ~2013 | Slightly over limit |

---

## Backlog (Prioritized)

### High Impact
- ~~**Helm-style C-x C-f find-file**~~ — DONE: popup file selector with default path pre-filled, incremental narrowing, directory descent on Enter
- ~~**Persistent M-x command history**~~ — DONE: frequency-based sorting, persisted to ~/.gemacs-mx-history, auto-load/save
- ~~**Proper narrow-to-defun**~~ — DONE: multi-language defun detection (Scheme/Lisp paren-based, Python/Ruby/C/Go/Rust/etc indentation-based)

### Medium Impact
- ~~**Org-mode archive/refile**~~ — DONE: archive-subtree writes to `<file>_archive` with ARCHIVE_TIME/ARCHIVE_FILE properties, refile already implemented
- ~~**Better which-key delay display**~~ — DONE: shows human-readable descriptions (e.g. "s → Save buffer") instead of raw symbol names
- ~~**Occur mode improvements**~~ — DONE: read-only *Occur* buffer, occur-next/occur-prev navigation commands
- ~~**Dired improvements**~~ — DONE: human-readable file sizes (K/M/G), read-only dired buffer, rename/copy at point (Qt), shared dired-format-listing

### Maintenance
- ~~**Split over-limit files**~~ — DONE: split `qt/commands-edit.ss` (2457→1155+1325) and `editor-extra-editing.ss` (2204→1097+1130). Remaining `editor-extra-media.ss` (2013) and `qt/commands-vcs.ss` (2003) are barely over limit
- ~~**Test coverage**~~ — DONE: added Group 13 tests (dired, which-key, zoom, compile/search, bookmarks, M-x history). 181 total TUI functional tests, 398 Qt tests

### Recently Completed
- ~~**Qt command file splits**~~ — DONE: split 8 oversized files (modes, search, shell, sexp, aliases, core, file, vcs) into paired *2.ss files. All 26 Qt command files now under 2000-line limit
- ~~**Overwrite mode upgrade**~~ — DONE: real SCI_SETOVERTYPE toggle (was display-only flag), modeline shows "Ovwrt" indicator
- ~~**Modeline indicators**~~ — DONE: overwrite mode and narrowing indicators wired via boxed callback providers
- ~~**Hippie-expand upgrade**~~ — DONE: multi-strategy expansion (dabbrev → file name → cycle reset)
- ~~**Quoted-insert (C-q)**~~ — DONE: real key interception in both Qt and TUI (was stub)
- ~~**goto-last-change-reverse**~~ — DONE: navigate forward through edit positions (both layers)
- ~~**rename-visited-file**~~ — DONE: rename file on disk + update buffer name (both layers)
- ~~**Edit position tracking**~~ — DONE: qt-record-edit-position!/tui-record-edit-position! hooked into self-insert
- ~~**Qt functional tests**~~ — 443 tests (Groups 31-36 added)
- ~~**Selective display (C-x $)**~~ — DONE: hide lines by indentation level using SCI_HIDELINES/SCI_SHOWLINES
- ~~**Show-paren-mode upgrade**~~ — DONE: real toggle of brace matching in qt-update-visual-decorations
- ~~**Delete-selection-mode upgrade**~~ — DONE: typing deselects before insert when mode OFF
- ~~**Encoding support**~~ — DONE: `set-buffer-file-coding-system` with 15 encodings, `revert-buffer-with-coding-system` with narrowing selection
- ~~**Winum-mode upgrade**~~ — DONE: real select-window-1..9 commands in both TUI and Qt
- ~~**Eldoc mode wire**~~ — DONE: cmd-eldoc-mode now toggles the real *eldoc-mode* flag
- ~~**Stub upgrades batch 2**~~ — DONE: `cape-keyword` (narrowing insert), `helm-dash` (man page search), `erc`/`rcirc` (IRC via TCP), `gnus` (RSS feeds), `mu4e`/`notmuch` (mail integration), `native-compile-file` (gxc -S), `eww-submit-form` (form parsing)
- ~~**Org-sort, Mail, Sorting, Native-compile upgrades**~~ — DONE: `org-sort` (alphabetical child heading sort), `compose-mail`/`message-send` (mail composition + msmtp/sendmail), `sort-columns`/`sort-regexp-fields`, `native-compile-async` (gxc compilation)
- ~~**PDF/DocView upgrade**~~ — DONE: Qt PDF viewing with `pdftotext` page extraction, navigation (next/prev/goto), DocView conversion for PDF/PS
- ~~**TRAMP upgrades**~~ — DONE: `tramp-remote-shell` (SSH session), `tramp-remote-compile` (remote compilation via SSH), `sudo-edit`/`find-file-sudo` (open file as root)
- ~~**Org-crypt**~~ — DONE: GPG symmetric encrypt/decrypt of org entry bodies (`org-encrypt-entry`, `org-decrypt-entry`), both TUI and Qt
- ~~**Vterm terminal fixes**~~ — DONE: PTY window size from editor dimensions, PS1 prompt cleanup, batched rendering for speed
- ~~**Whitespace-mode & display-line-numbers-mode**~~ — DONE: Emacs-style SCI_SETVIEWWS/SCI_SETVIEWEOL toggle, line number gutter toggle (both TUI and Qt)
- ~~**Magit commit composition buffer**~~ — DONE: `*Magit: Commit*` buffer with diff preview, C-c C-c / C-c C-k, comment stripping
- ~~**Interactive magit log**~~ — DONE: date/author/subject format, Enter shows commit diff with highlighting, mode keymaps
- ~~**Magit amend**~~ — DONE: `a` in magit opens commit buffer pre-filled with previous message, `--amend` flag
- ~~**Magit stash-show**~~ — DONE: Enter on stash shows diff in `*Magit Stash Diff*` with highlighting
- ~~**Interactive magit push/pull/fetch**~~ — DONE: upstream detection, auto-set `-u`, remote selection, cherry-pick/revert with narrowing
- ~~**Describe variable (C-h v)**~~ — DONE: narrowing selection, shows value and docs in `*Help*` buffer
- ~~**Eshell I/O & expansion**~~ — DONE: output redirect (`>`, `>>`), input redirect (`<`), glob expansion, `$VAR` expansion, `$(cmd)` substitution
- ~~**Set-variable improvements**~~ — DONE: narrowing selection, 6 settable vars (fill-column, tab-width, indent-tabs-mode, etc.)
- ~~**Magit worktree management**~~ — DONE: list/add/remove worktrees, `w` key in magit buffer
- ~~**Project keybindings (C-x p)**~~ — DONE: Emacs 28+ standard prefix map, narrowing for find-file/switch-buffer/switch-project
- ~~**Windmove keybindings**~~ — DONE: Shift+arrow keys for directional window navigation
- ~~**File splits: commands-config + commands-ide**~~ — DONE: split 2415→1087+1359 and 2513→1165+1540, all under 2000 limit
- ~~**Hook system & ctags integration**~~ — DONE: `add-hook!`/`remove-hook!`/`run-hooks!` in core.ss, hook management commands, mode hooks, ctags support (`visit-tags-table`, `find-tag` with completion) in both TUI and Qt
- ~~**Consult-ripgrep (M-s r)**~~ — DONE: interactive `rg` search with narrowing popup, auto-detect project root, jump to result
- ~~**Consult-bookmark**~~ — DONE: bookmark jump with file:position info in narrowing popup
- ~~**Fill-column indicator**~~ — DONE: visual vertical line using Scintilla edge mode (SCI_SETEDGEMODE), both TUI and Qt
- ~~**Goto-address-mode**~~ — DONE: real URL detection and highlighting with Scintilla indicators, both TUI and Qt
- ~~**Subword-mode navigation**~~ — DONE: CamelCase-aware word movement (subword-forward/backward/kill), both TUI and Qt

- ~~**File split: commands-parity**~~ — DONE: split 2111→1030+1118, both under 2000 limit
- ~~**Qt profiler (profiler-start/stop)**~~ — DONE: upgraded from stub to real `##process-statistics` profiling (wall/CPU/GC/allocation)
- ~~**Qt info reader (M-x info)**~~ — DONE: upgraded from stub to real GNU Info reader via subprocess, topic prompting, ANSI stripping
- ~~**Qt complete-at-point popup**~~ — DONE: upgraded from echo-area display to Scintilla native autocomplete popup (SCI_AUTOCSHOW)
- ~~**Qt describe-key (C-h k)**~~ — DONE: upgraded from stub to real key interception — captures next keypress, shows binding and help in *Help* buffer

- ~~**Periodic auto-save timer**~~ — DONE: writes modified buffers to `#filename#` auto-save files every 30s, respects per-buffer disable
- ~~**Pulse-on-jump highlighting**~~ — DONE: auto-detects cursor jumps >5 lines and briefly flashes landing line (Scintilla INDIC_FULLBOX), toggleable
- ~~**Region-aware count-words (M-=)**~~ — DONE: shows region lines/words/chars when mark active, buffer stats otherwise
- ~~**Qt recover-session + what-face upgrades**~~ — DONE: recover-session delegates to session-restore, what-face shows Scintilla style/colors
- ~~**ANSI color rendering in compilation buffers**~~ — DONE: full SGR parser (colors 0-7, bright 90-97, bold, bg), Scintilla manual styling, `M-x ansi-color-apply`
- ~~**Upgrade toggle stubs to real Scintilla**~~ — DONE: hl-line (SCI_SETCARETLINEVISIBLE), show-tabs (SCI_SETVIEWWS), show-eol (SCI_SETVIEWEOL), font-lock-mode (lexer on/off)
- ~~**Emacs-style uniquify buffer names**~~ — DONE: both old and new same-name buffers get `<parent>` suffix, applied in qt-open-file! and cmd-find-file
- ~~**diff-backup upgrade**~~ — DONE: real `diff -u` against `file~` backup, output in compilation buffer
- ~~**Consult-line with narrowing**~~ — DONE: upgraded from two-step to interactive narrowing popup with line numbers and jump-to-line
- ~~**Auto-highlight symbol under cursor**~~ — DONE: idle timer highlights all occurrences of word at point using Scintilla INDIC_ROUNDBOX indicators, word-boundary aware, toggleable
- ~~**Consult-outline**~~ — DONE: jump to headings/definitions with narrowing, multi-language (Org/Markdown/Scheme/Python/C/JS/Ruby/Go/Rust)

- ~~**Interactive ibuffer**~~ — DONE: upgraded from display-only to interactive mark/execute (d=delete, s=save, u=unmark, x=execute, /=filter, S=sort, RET=goto, t=toggle). Mode keymap bindings, read-only buffer with refresh
- ~~**Multi-language flycheck**~~ — DONE: extended from Gerbil-only to 7 languages (Python, JS/TS/eslint, Go/go-vet, Shell/shellcheck, C/C++/gcc, Ruby). Auto-detects by file extension, runs on save
- ~~**Flyspell visual indicators**~~ — DONE: red squiggly underlines using Scintilla INDIC_SQUIGGLE (indicator 28), cleared on toggle-off
- ~~**Multi-language which-function**~~ — DONE: upgraded from Scheme-only to 6 language families (Scheme/Gerbil, Python, C/Go/Rust, JS/TS). Both TUI and Qt
- ~~**Major mode switching**~~ — DONE: 18 language modes (Python, C, C++, JS, TS, Go, Rust, Ruby, Markdown, YAML, JSON, SQL, Lua, HTML, CSS, Scheme, Shell, Text) with real lexer switching via `M-x <lang>-mode`
- ~~**Flymake/flyspell delegate stubs**~~ — DONE: `toggle-flymake` delegates to flycheck, `toggle-flyspell` delegates to flyspell-mode
- ~~**Horizontal scroll**~~ — DONE: `scroll-left`/`scroll-right` via Scintilla SCI_LINESCROLL (10 columns per invocation)

- ~~**Hook system (add-hook/remove-hook/run-hooks)**~~ — DONE: real Emacs-style hook system in core.ss with `add-hook!`, `remove-hook!`, `run-hooks!`. Hooks wired into `before-save-hook`, `after-save-hook`, `find-file-hook`, `kill-buffer-hook`. Interactive `M-x add-hook`/`remove-hook`/`list-hooks` in both TUI and Qt
- ~~**Fullscreen/maximized toggle**~~ — DONE: proper toggle using `qt-widget-window-state` to detect current state, toggles back to normal
- ~~**Find-file-literally**~~ — DONE: opens file with syntax highlighting disabled (SCLEX_NULL)
- ~~**Backup file toggle**~~ — DONE: `*backup-files*` variable respected in Qt save-buffer

- ~~**Repeat-mode (Emacs 28+ transient repeat maps)**~~ — DONE: `*repeat-mode*` flag (boxed for cross-module mutation), repeat map infrastructure in core.ss, 6 default maps (other-window, buffer-nav, next-error, undo, page-nav, window-resize), dispatch hooks in both TUI and Qt. 443 Qt tests, 149 TUI functional tests

- ~~**Qt parity batch: 30+ missing commands**~~ — DONE: scroll-other-window (up/down/recenter), buffer-statistics, convert-line-endings, set-buffer-encoding, diff-two-files, insert utilities (file-name, env-var, separator, form-feed, fixme, todo, backslash, sequential-numbers, zero-width-space), hex-to-decimal/decimal-to-hex, shell-command-on-region-replace, shell-command-to-string, tabify-region, goto-scratch, org-store-link, word-frequency-analysis, display-cursor-position/column-number, narrow-to-page. 477 Qt tests

- ~~**Bulk toggle parity: 339 toggle commands**~~ — DONE: bulk registration infrastructure with shared hash-table state, auto-generated display names (e.g. "Global Rainbow Mode ON/OFF"), all 339 missing TUI toggles now available in Qt via M-x. 502 Qt tests

- ~~**Full Qt/TUI command parity**~~ — DONE: closed remaining 473-command gap with parity4 (339 toggles) and parity5 (63 mode toggles, 31 stubs, 24 aliases, 18 functional). Every TUI command now has a Qt counterpart. 576 Qt tests

- ~~**Format-on-save & embark upgrade**~~ — DONE: apheleia-mode wires `before-save-hook` to run language formatter on save (both layers), `apheleia-format-buffer` delegates to real `format-buffer`, TUI `cmd-save-buffer` now calls `before-save-hook`/`after-save-hook` (parity with Qt), `embark-act` upgraded to interactive contextual actions with target detection (URL/file/symbol) and narrowing popup (Qt) / key dispatch (TUI), `embark-dwim` executes default action. 586 Qt tests

### Aspirational
- LSP in TUI
- DAP/debugger integration
- Multi-frame support
- ~~Spell checking integration~~ — DONE: flyspell-mode, ispell-word, ispell-buffer, ispell-region, dictionary switching (both TUI and Qt)
