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

- ~~**Stub upgrades batch: calc, proced, eww**~~ — DONE: real RPN calculator stack (push/pop/dup/swap/+/-/*/÷), process manager with signal sending, EWW URL/search commands. 607 Qt tests

- ~~**Games & text transforms**~~ — DONE: Game of Life (glider, 5 generations), Dunnet text adventure, ELIZA Doctor chatbot, CSV column alignment with pipe separators, JSON key sorting, hex number increment at point. 607 Qt tests

- ~~**Help system, calendar, templates, themes, window mgmt**~~ — DONE: full help reference in *Help* buffer, real month calendar with day highlighting, auto-insert templates (13 languages + user template dir), Scintilla color reset, Qt minimize/restore. 619 Qt tests

- ~~**REST client, SQL client, denote**~~ — DONE: real curl-based HTTP requests with response buffer, interactive SQL connection/region send, denote timestamped note creation in ~/notes/ with org headers. 628 Qt tests

- ~~**GDB/MI debugger integration**~~ — DONE: real GDB process spawning with MI2 interpreter, breakpoint toggle via -break-insert/-break-delete, step over/in/out/continue via -exec-* commands, interactive GDB REPL, output accumulated in *GDB* buffer. 638 Qt tests

- ~~**GPTel AI chat, customize, package management**~~ — DONE: GPTel chat buffer with OpenAI API integration (curl-based, OPENAI_API_KEY), customize-group settings browser with toggle states, customize-themes theme catalog, full package management (list-packages via dpkg/rpm/brew, package-list-packages/install/delete/refresh via gerbil pkg, package-archives info). 653 Qt tests

- ~~**Stub cleanup + re-builder + doc/theme/mail upgrades**~~ — DONE: Upgraded re-builder from echo stub to real interactive regex builder with Scintilla match highlighting (INDIC_ROUNDBOX). Upgraded dash-at-point, devdocs-lookup, doom-themes, rmail, citar-insert-citation, facemenu-set-background from stubs to real. Cleaned up 37 parity3 stubs overridden by real implementations. Only 10 stubs remain across entire Qt codebase. 661 Qt tests

- ~~**Auto-revert mode**~~ — DONE: auto-revert unmodified buffers when files change on disk, both TUI and Qt
- ~~**Desktop save/restore**~~ — DONE: persist and restore session (open buffers, positions) across restarts
- ~~**Multi-cursor typing**~~ — DONE: Scintilla multi-selection support for simultaneous typing at multiple cursors
- ~~**Aggressive indent mode**~~ — DONE: auto-reindent on closing delimiters and newlines
- ~~**Which-key mode**~~ — DONE: shows available keybindings after prefix key delay (TUI countdown + Qt timer), toggleable
- ~~**Visual-line-mode**~~ — DONE: toggle word wrap via Scintilla SCI_SETWRAPMODE, both TUI and Qt
- ~~**Whitespace-mode / delete-trailing-whitespace**~~ — DONE: show/hide whitespace and EOL markers, strip trailing spaces

- ~~**Enriched-mode & picture-mode**~~ — DONE: enriched text formatting (bold/italic) and overwrite-mode picture drawing, both TUI and Qt
- ~~**Electric-pair-mode**~~ — DONE: `*electric-pair-mode*` variable in persist.ss with defvar! registration
- ~~**Auto-fill centralization**~~ — DONE: moved `*auto-fill-mode*` and `*fill-column*` to persist.ss, TUI self-insert auto-fill line breaking
- ~~**Abbrev mode variables**~~ — DONE: `*abbrev-table*` and `*abbrev-mode-enabled*` in persist.ss

- ~~**Winner-mode hooks**~~ — DONE: `winner-save-config!` hooked into TUI split/delete window commands
- ~~**Hungry-delete**~~ — DONE: `hungry-delete-forward/backward` delete all consecutive whitespace
- ~~**Crux-move-beginning-of-line**~~ — DONE: smart BOL toggle (first non-whitespace ↔ column 0)
- ~~**Isearch match counter**~~ — DONE: anzu-style [N/M] match counting in both TUI and Qt
- ~~**Ws-butler-mode**~~ — DONE: snapshot/clean whitespace on save (Qt)
- ~~**Elfeed RSS reader**~~ — DONE: real RSS/Atom feed reader with curl fetch, XML parsing, feed persistence, default feeds
- ~~**Wgrep upgrade**~~ — DONE: real editable grep results — parses filename:line:content, applies edits back to source files
- ~~**Direnv/envrc integration**~~ — DONE: load .envrc via `direnv export bash`, `direnv-allow` command
- ~~**Move-text (drag-stuff)**~~ — DONE: move current line up/down
- ~~**Transient keymaps**~~ — DONE: modal command menus with predefined maps (window-resize, zoom, navigate)
- ~~**Swiper/counsel wrappers**~~ — DONE: swiper→occur, counsel-M-x→execute-extended-command, counsel-rg→rgrep, counsel-find-file, counsel-recentf, counsel-bookmark, ivy-resume
- ~~**God mode**~~ — DONE: Ctrl-free command execution toggle (god-mode, god-local-mode, god-execute-with-current-bindings)
- ~~**Beacon mode**~~ — DONE: cursor flash on large movements toggle
- ~~**Volatile highlights**~~ — DONE: flash edited regions toggle
- ~~**Smartparens wrappers**~~ — DONE: smartparens-mode→auto-pair, smartparens-strict-mode→paredit-strict
- ~~**use-package/straight stubs**~~ — DONE: informational stubs (all packages built-in)
- ~~**Which-key enhancements**~~ — DONE: which-key-show-top-level, which-key-show-major-mode
- ~~**Dimmer mode**~~ — DONE: dim non-active windows toggle
- ~~**Nyan mode**~~ — DONE: fun position indicator toggle
- ~~**Centered cursor mode**~~ — DONE: keep cursor vertically centered toggle
- ~~**Format-all-buffer**~~ — DONE: real external formatter integration (black, prettier, gofmt, rustfmt, clang-format, jq, shfmt, etc.)
- ~~**Visual regexp**~~ — DONE: visual-regexp-replace, visual-regexp-query-replace delegating to query-replace-regexp
- ~~**Anzu mode**~~ — DONE: search match count indicator toggle
- ~~**Popwin mode**~~ — DONE: popup window management mode with close-popup
- ~~**Easy-kill**~~ — DONE: copy word at point without moving cursor
- ~~**Crux extras**~~ — DONE: open-with (xdg-open), duplicate-current-line, indent-defun, swap-windows, cleanup-buffer-or-region
- ~~**Selected mode**~~ — DONE: special keybindings when region active toggle
- ~~**Aggressive fill-paragraph**~~ — DONE: auto-reflow paragraphs mode toggle
- ~~**Hydra menus**~~ — DONE: extensible popup command menus (hydra-zoom, hydra-window with interactive key loops)
- ~~**Deadgrep**~~ — DONE: real ripgrep search with results buffer and navigation (TUI + Qt)
- ~~**String-edit-at-point**~~ — DONE: edit string at point
- ~~**Hideshow (hs-minor-mode)**~~ — DONE: real Scintilla fold margin setup with box markers, automatic fold, toggle/hide-all/show-all
- ~~**Prescient**~~ — DONE: completion sorting by frequency mode toggle
- ~~**No-littering**~~ — DONE: clean dotfile organization (gemacs uses ~/.gemacs-* by default)
- ~~**Benchmark-init / esup**~~ — DONE: startup profiling informational commands
- ~~**GCMH**~~ — DONE: adaptive GC threshold mode toggle
- ~~**Ligature mode**~~ — DONE: font ligature display toggle
- ~~**Mixed-pitch / variable-pitch**~~ — DONE: proportional font mode toggles
- ~~**Eldoc-box**~~ — DONE: eldoc popup mode and help-at-point
- ~~**Color-rg**~~ — DONE: real ripgrep search with file type filter and project root detection
- ~~**Ctrlf / phi-search**~~ — DONE: alternative isearch wrappers
- ~~**Toc-org**~~ — DONE: auto-generate org table of contents with heading extraction
- ~~**Org-super-agenda**~~ — DONE: enhanced agenda grouping mode toggle
- ~~**Nov.el (EPUB reader)**~~ — DONE: real EPUB text extraction via unzip+sed pipeline
- ~~**LSP-UI**~~ — DONE: mode toggle, doc-show, peek-find-definitions/references
- ~~**Emojify**~~ — DONE: emoji display mode + emoji insert-by-name (10 built-in emoji)
- ~~**Ef-themes / modus-themes**~~ — DONE: theme pack selection and toggle
- ~~**Circadian / auto-dark**~~ — DONE: time-based and OS-based theme switching toggles
- ~~**Breadcrumb / sideline / flycheck-inline**~~ — DONE: code context, side info, inline error display mode toggles
- ~~**Zone / fireplace**~~ — DONE: screen saver and decorative fireplace display
- ~~**DAP-UI**~~ — DONE: debugger UI panels mode toggle
- ~~**Poly-mode**~~ — DONE: multiple major modes in one buffer toggle
- ~~**Company-box**~~ — DONE: fancy completion popup mode toggle
- ~~**Impatient mode**~~ — DONE: live HTML preview mode toggle
- ~~**Mood-line / powerline**~~ — DONE: modeline theme toggles
- ~~**Centaur-tabs**~~ — DONE: tab bar for buffer groups toggle
- ~~**All-the-icons-dired / treemacs-icons**~~ — DONE: icon display in dired
- ~~**Nano-theme**~~ — DONE: nano-emacs theme selection

- ~~**Cross-buffer dabbrev expansion**~~ — DONE: M-/ (dabbrev-expand) now searches all open buffers' files on disk, not just current buffer (both TUI and Qt)
- ~~**CUA mode real keybinding swap**~~ — DONE: C-c=copy, C-v=paste, C-z=undo with save/restore of original bindings
- ~~**Debug-on-entry with Gambit trace**~~ — DONE: real trace/untrace integration, symbol tracking (both TUI and Qt)
- ~~**DevOps commands upgrade**~~ — DONE: ansible-mode/kubernetes-mode with real YAML lexer, ansible-playbook syntax-check, kubectl interactive, terraform-mode/terraform/terraform-plan, docker-compose/up/down (both TUI and Qt)
- ~~**Snippet disk persistence**~~ — DONE: snippet-define! persists to ~/.gemacs-snippets/<lang>/<trigger>, yas aliases point to real commands
- ~~**AI commands real API**~~ — DONE: ai-code-explain/refactor use real OpenAI API with JSON parsing (both TUI and Qt)
- ~~**EMMS/perspectives/org-roam real**~~ — DONE: EMMS mpv playback, persp-mode buffer groups, org-roam grep-based note search

- ~~**Git-gutter fringe markers**~~ — DONE: real Scintilla margin markers (green=add, blue=mod, red=del) in both TUI and Qt, toggle via M-x git-gutter-mode
- ~~**Qt GUD commands real GDB/MI**~~ — DONE: persistent GDB process with MI2 protocol for gud-break/cont/next/step/remove
- ~~**Qt memory-usage detailed report**~~ — DONE: *Memory* buffer with GC stats, heap, timing, allocation (matching TUI)
- ~~**TUI LSP delegation**~~ — DONE: LSP commands delegate to ctags/grep/formatter backends instead of showing "not available"
- ~~**Stub delegation batch**~~ — DONE: counsel-find-file→find-file, crux-indent-defun→indent-region, run-scheme/slime/sly→repl, themes→customize-themes, multi-vterm→shell, flycheck-prev-error→previous-error, helm-buffers-list→list-buffers, helpful-key→describe-key
- ~~**EOL conversion cycling**~~ — DONE: real SCI_SETEOLMODE cycling (LF→CRLF→CR) in Qt

- ~~**Eshell tab completion**~~ — DONE: TAB in eshell completes filenames (in CWD) and PATH commands (for first word). Single match auto-completes, multiple matches insert longest common prefix and show candidates in echo area. Both TUI and Qt. 668 Qt tests

- ~~**Qt stub upgrades batch**~~ — DONE: mc-mark-previous-like-this (real backward SCI_ADDSELECTION), eww-forward (forward history stack), reopen-killed-buffer (killed buffer tracking + disk reopen), add-dir-local-variable (prompt + write .gemacs-config), add-file-local-variable (insert/extend -*- header)

- ~~**Stub upgrades + notification log**~~ — DONE: `ediff-show-registry` (shows active *Ediff*/*Diff* buffers in registry buffer, both TUI and Qt), `menu-bar-open` (42-item narrowing menu popup across File/Edit/Search/View/Tools/Help), `notifications-list` (numbered *Notifications* buffer from ring buffer log), `server-start` upgraded (reads IPC server address from ~/.gemacs-server file), `inferior-lisp` Qt upgraded (delegates to eshell). Added `*notification-log*` ring buffer to core.ss — `echo-message!` and `echo-error!` now log all messages, `notification-get-recent` exposed. 677 Qt tests.

- ~~**Calc arithmetic ops, describe-mode, abbrev auto-expand**~~ — DONE: 18 RPN calc math commands (calc-add/sub/mul/div/mod/pow/neg/abs/sqrt/log/exp/sin/cos/tan/floor/ceiling/round/clear) in both TUI and Qt; `describe-mode` enhanced to show all 18 active minor modes in *Help* buffer; abbrev auto-expansion wired to Qt self-insert (space/newline/comma/period/semicolon triggers lookup). 693 Qt tests.

- ~~**String inflection, occur-edit mode, wdired**~~ — DONE (Batch 4): `string-inflection-cycle/snake-case/camelcase/upcase` cycle identifiers through snake→camel→PascalCase→UPPER→kebab with full tokenizer (handles all casing boundaries + UPPER_CASE detection); `occur-edit-mode` makes `*Occur*` buffer editable (snapshots lines, commits on C-c C-c to write changes back to source buffer); `wdired-mode/wdired-finish-edit/wdired-abort` enable filename renaming in dired buffers via `mv`. All 9 commands in both TUI and Qt. 706 Qt tests.

- ~~**project-query-replace, multi-file qreplace**~~ — DONE (Batch 5): `project-query-replace` / `project-query-replace-regexp` — grep project for files matching `from`, open each file, run interactive query-replace (Qt: y/n/!/q per match using existing qreplace key handler; TUI: batch replace-all per file). Extended `qreplace-finish!` with `*qreplace-files-remaining*` state to auto-advance to next project file. `insert-uuid` / `uuidgen` properly registered in both layers (Qt: calls `/usr/bin/uuidgen`; TUI: already in `editor-cmds-a.ss`). Fix: singleton Scintilla editor now resets `SCI_SETREADONLY 0` at start of each test group. 716 Qt tests.

### Current Status
- **716 Qt tests passing**, all TUI tests passing (vtscreen flaky but known)
- **~2185+ registered commands** across TUI and Qt
- **~650+ implemented features**
- Remaining stubs: 2 niche Qt stubs (all-the-icons-install-fonts, nerd-icons-install-fonts — informational only)
- Remaining yellow circles in gemacs-vs-emacs.md: ~12 (mostly fundamental platform limitations: tree-sitter, EWW CSS/JS, screen reader, tab-line per-window)

### In Progress: SMP / Qt Thread Safety (`fix-smp-qt` branch)

**Problem**: Gambit's M:N SMP scheduler migrates green threads between OS threads at heartbeat preemption points. Qt requires all widget operations on the OS thread where `QApplication` was created. This causes intermittent startup failures and crashes.

**What was done**:
- Added `QT_VOID` / `QT_RETURN` / `QT_RETURN_STRING` dispatch macros to all 803 C++ shim functions in `gerbil-qt/vendor/qt_shim.cpp`. These check `is_qt_main_thread()` and use `Qt::BlockingQueuedConnection` to marshal cross-thread calls.
- Restructured `qt-main` in `qt/app.ss`: extracted body into `qt-do-init!`, run synchronously before `qt-app-exec!`.
- Attempted `g_event_loop_running` atomic flag to skip dispatch before exec() — but exec() itself can be called from the wrong thread after migration.
- Attempted dedicated Qt pthread (`pthread_create` in `qt_application_create`, `pthread_join` in `qt_application_exec`) — exec() always on correct thread, but broke Qt functional tests with `QObject::setParent: Cannot set parent, new parent is in a different thread` errors and a segfault in scenario 19+.

**Current state of the branch**:
- Dynamic build works, both binaries start (`--version` works)
- Qt functional tests FAIL: 716→segfault with dedicated-pthread approach
- The root cause of test breakage is not yet identified — suspected Qt object thread-affinity issue when using a `pthread`-created thread vs Qt's native main thread

**Decision (2026-03-13)**: Switched to GAMBCOPT=,-:p1 (single-processor mode). After 6+ SMP deadlocks in 4 days (GC sync, device manager mutex, SIGCHLD race, QTimer circular deadlock, BQC reentrancy, callback starvation), SMP is not viable for Qt applications. Green threads still work cooperatively on the single VP. See ~/mine/gambit-smp-woes.md for the full post-mortem.

### Next Steps (for continuation)
- Fix `fix-smp-qt` branch (see above) — tests are currently broken
- Continue upgrading remaining stubs per standing instruction
- Potential features: dired improvements (mark by regex, shell command on marked), more text transforms
- Update gemacs-vs-emacs.md after each batch
- Static build verification (`make static-qt`) periodically

### Aspirational
- LSP in TUI (currently delegates to ctags/grep/formatter; full LSP protocol not yet implemented)
- ~~DAP/debugger integration~~ — DONE: real GDB/MI interface in both Qt and TUI (spawn, breakpoints, step, continue, REPL)
- ~~Multi-frame support~~ — DONE: virtual frame management (C-x 5 prefix), make-frame/delete-frame/other-frame
- ~~Spell checking integration~~ — DONE: flyspell-mode, ispell-word, ispell-buffer, ispell-region, dictionary switching (both TUI and Qt)
- ~~TRAMP remote editing~~ — DONE: SSH-based file open/save, remote shell, remote compile
- ~~AI Copilot integration~~ — DONE: OpenAI API inline code completion, copilot-complete/accept/dismiss
