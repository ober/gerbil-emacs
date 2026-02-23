# Gemacs vs GNU Emacs — Feature Comparison

> **Last updated:** 2026-02-22
> **Gemacs version:** master (ade3dae)
> **Compared against:** GNU Emacs 29.x / 30.x feature set

## Status Legend

| Symbol              | Meaning                                                              |
|---------------------|----------------------------------------------------------------------|
| :white_check_mark:  | **Full** — Feature-complete, comparable to Emacs                     |
| :large_blue_circle: | **Substantial** — Most functionality works, some gaps                |
| :yellow_circle:     | **Partial** — Core works, significant gaps remain                    |
| :orange_circle:     | **Minimal** — Basic scaffolding, limited use                         |
| :red_circle:        | **Stub/Missing** — Registered but non-functional, or absent entirely |

---

## Table of Contents

1. [Core Editing](#1-core-editing)
2. [Navigation](#2-navigation)
3. [Search & Replace](#3-search--replace)
4. [Kill, Yank & Clipboard](#4-kill-yank--clipboard)
5. [Undo System](#5-undo-system)
6. [Marks & Regions](#6-marks--regions)
7. [Registers & Bookmarks](#7-registers--bookmarks)
8. [Rectangle Operations](#8-rectangle-operations)
9. [Keyboard Macros](#9-keyboard-macros)
10. [Minibuffer & Completion](#10-minibuffer--completion)
11. [Buffer Management](#11-buffer-management)
12. [Window Management](#12-window-management)
13. [Frame / Display Management](#13-frame--display-management)
14. [File Operations](#14-file-operations)
15. [Dired (Directory Editor)](#15-dired-directory-editor)
16. [Version Control / Magit](#16-version-control--magit)
17. [Org-mode](#17-org-mode)
18. [Programming Support](#18-programming-support)
19. [LSP (Language Server Protocol)](#19-lsp-language-server-protocol)
20. [Syntax Highlighting & Themes](#20-syntax-highlighting--themes)
21. [Completion Frameworks](#21-completion-frameworks)
22. [Shell & Terminal](#22-shell--terminal)
23. [Spell Checking](#23-spell-checking)
24. [Text Transformation & Formatting](#24-text-transformation--formatting)
25. [S-expression / Paredit](#25-s-expression--paredit)
26. [Diff & Ediff](#26-diff--ediff)
27. [Project Management](#27-project-management)
28. [Help System](#28-help-system)
29. [Customization & Configuration](#29-customization--configuration)
30. [Package Management & Extensibility](#30-package-management--extensibility)
31. [Remote Editing (TRAMP)](#31-remote-editing-tramp)
32. [EWW (Web Browser)](#32-eww-web-browser)
33. [Calendar & Diary](#33-calendar--diary)
34. [Email (Gnus / mu4e / notmuch)](#34-email)
35. [IRC / Chat](#35-irc--chat)
36. [PDF / Document Viewing](#36-pdf--document-viewing)
37. [Treemacs / File Tree](#37-treemacs--file-tree)
38. [Multiple Cursors / iedit](#38-multiple-cursors--iedit)
39. [Snippets (YASnippet)](#39-snippets-yasnippet)
40. [Tab Bar & Workspaces](#40-tab-bar--workspaces)
41. [Accessibility](#41-accessibility)
42. [Performance & Large Files](#42-performance--large-files)
43. [AI / LLM Integration](#43-ai--llm-integration)
44. [Multi-Terminal (vterm)](#44-multi-terminal-vterm)
45. [Key Input Remapping](#45-key-input-remapping)
46. [DevOps / Infrastructure Modes](#46-devops--infrastructure-modes)
47. [Helm / Narrowing Framework](#47-helm--narrowing-framework)
48. [Personal Workflow Gap Analysis](#personal-workflow-gap-analysis)

---

## 1. Core Editing

| Feature                        | Status              | Notes                                           |
|--------------------------------|---------------------|-------------------------------------------------|
| Self-insert characters         | :white_check_mark:  | Full Unicode support via Scintilla              |
| Delete / Backspace             | :white_check_mark:  | `C-d`, `DEL`, `C-h`                             |
| Kill line (`C-k`)              | :white_check_mark:  | Kill to EOL, empty line kills newline           |
| Open line (`C-o`)              | :white_check_mark:  |                                                 |
| Newline & indent (`C-j`)       | :white_check_mark:  |                                                 |
| Transpose chars (`C-t`)        | :white_check_mark:  |                                                 |
| Transpose words (`M-t`)        | :white_check_mark:  |                                                 |
| Transpose lines (`C-x C-t`)    | :white_check_mark:  |                                                 |
| Transpose sexps                | :white_check_mark:  |                                                 |
| Join line (`M-j` / `M-^`)      | :white_check_mark:  |                                                 |
| Quoted insert (`C-q`)          | :white_check_mark:  |                                                 |
| Overwrite mode                 | :white_check_mark:  | Toggle via `<insert>`                           |
| Auto-fill mode                 | :white_check_mark:  | Automatic line wrapping at fill-column          |
| Electric pair mode             | :large_blue_circle: | Auto-pairing brackets/quotes, toggleable        |
| Indent line / region           | :white_check_mark:  | TAB dispatches: indent, complete, or org-expand |
| Universal argument (`C-u`)     | :white_check_mark:  | Numeric prefix for repeat/modify commands       |
| Digit arguments (`M-0`..`M-9`) | :white_check_mark:  |                                                 |
| Negative argument (`M--`)      | :white_check_mark:  |                                                 |
| Repeat (`C-x z`)               | :white_check_mark:  |                                                 |

**Summary:** Core editing is feature-complete. All standard Emacs editing primitives work.

---

## 2. Navigation

| Feature                     | Status              | Notes                                        |
|-----------------------------|---------------------|----------------------------------------------|
| Char/word/line movement     | :white_check_mark:  | `C-f/b/n/p`, `M-f/b`, arrows                 |
| Beginning/end of line       | :white_check_mark:  | `C-a/e`, `Home/End`                          |
| Beginning/end of buffer     | :white_check_mark:  | `M-<`, `M->`                                 |
| Page up/down                | :white_check_mark:  | `C-v`, `M-v`, PgUp/PgDn                      |
| Scroll other window         | :white_check_mark:  | `M-g v`, `M-g V`                             |
| Recenter (`C-l`)            | :white_check_mark:  | Cycles top/center/bottom                     |
| Goto line (`M-g g`)         | :white_check_mark:  |                                              |
| Goto char (`M-g c`)         | :white_check_mark:  |                                              |
| Goto column                 | :white_check_mark:  |                                              |
| Goto matching paren         | :white_check_mark:  | `M-g m`                                      |
| Goto percent                | :white_check_mark:  | `M-g %` — jump to N% of buffer               |
| Forward/backward sentence   | :white_check_mark:  | `M-a`, `M-e`                                 |
| Forward/backward paragraph  | :white_check_mark:  | `M-{`, `M-}`                                 |
| Forward/backward sexp       | :white_check_mark:  | `M-g f/b`                                    |
| Back to indentation (`M-m`) | :white_check_mark:  |                                              |
| Imenu symbol navigation     | :large_blue_circle: | Works for many languages, no sidebar         |
| Avy jump (char/line/word)   | :large_blue_circle: | `avy-goto-char`, `avy-goto-line`             |
| Xref go-to-definition       | :large_blue_circle: | Works via grep fallback; LSP backend partial |
| Xref find references        | :large_blue_circle: | Grep-based                                   |
| Next/previous error         | :white_check_mark:  | `M-g n/p` navigates compilation errors       |
| Ace-window                  | :large_blue_circle: | Jump to window by label                      |
| Pop mark / mark ring        | :large_blue_circle: | Mark stack navigation                        |

**Summary:** Navigation is comprehensive. All standard movement commands plus extras like avy and ace-window.

---

## 3. Search & Replace

| Feature                       | Status             | Notes                                      |
|-------------------------------|--------------------|--------------------------------------------|
| Isearch forward/backward      | :white_check_mark: | `C-s`, `C-r` with wrap-around              |
| Isearch regexp                | :white_check_mark: | `C-M-s`                                    |
| Query replace                 | :white_check_mark: | `M-%` with y/n/!/q responses               |
| Query replace regexp          | :white_check_mark: | `C-M-%`                                    |
| Replace all (non-interactive) | :white_check_mark: |                                            |
| Occur                         | :white_check_mark: | `M-s o` — results buffer with line numbers |
| Multi-file occur              | :white_check_mark: |                                            |
| Grep (project-wide)           | :white_check_mark: | `rgrep`, `project-grep`, `counsel-grep`    |
| Grep results buffer           | :white_check_mark: | With next/prev error navigation            |
| Wgrep (edit grep results)     | :white_check_mark: | Edit matches in-place, save back to files  |
| Keep/flush lines              | :white_check_mark: | `M-s k`, `M-s f`                           |
| Count matches                 | :white_check_mark: | `M-s c`                                    |
| Isearch word mode             | :large_blue_circle: | `isearch-forward-word` searches for word at point |
| Isearch symbol mode           | :large_blue_circle: | `isearch-forward-symbol` searches for symbol at point |
| Search highlight all matches  | :green_circle:     | Qt: highlights all matches during isearch (current=cyan, others=yellow) |

**Summary:** Search is strong. Isearch with live multi-match highlighting, query-replace, occur, grep, and wgrep all work well.

---

## 4. Kill, Yank & Clipboard

| Feature | Status | Notes |
|---------|--------|-------|
| Kill line / region / word | :white_check_mark: | Full kill ring integration |
| Kill ring | :white_check_mark: | Stores history of kills |
| Yank (`C-y`) | :white_check_mark: | |
| Yank pop (`M-y`) | :white_check_mark: | Cycle through kill ring |
| Kill ring save (`M-w`) | :white_check_mark: | Copy without killing |
| Append next kill | :white_check_mark: | |
| Browse kill ring | :white_check_mark: | Interactive selection |
| System clipboard integration | :large_blue_circle: | Qt layer has clipboard; TUI limited |
| Zap to char (`M-z`) | :white_check_mark: | |
| Zap up to char | :white_check_mark: | |
| Kill whole line | :white_check_mark: | |
| Copy from above/below | :white_check_mark: | Copy character from line above/below |

**Summary:** Kill/yank system is complete with kill ring cycling and browsing.

---

## 5. Undo System

| Feature | Status | Notes |
|---------|--------|-------|
| Undo (`C-/`, `C-_`) | :white_check_mark: | |
| Redo (`M-_`) | :white_check_mark: | Linear redo |
| Undo grouping | :white_check_mark: | Consecutive edits grouped |
| Undo boundaries | :white_check_mark: | Commands create boundaries |
| Undo tree visualization | :white_check_mark: | `M-x undo-tree-visualize` with snapshot history |
| Persistent undo (across sessions) | :white_check_mark: | `undo-history-save` / `undo-history-load` to `~/.gemacs-undo/` |
| Selective undo (region) | :white_check_mark: | Undo within region, falls back to normal undo |

**Summary:** Undo/redo with tree visualization (`M-x undo-tree-visualize`), timestamped snapshots (`M-x undo-history`), and snapshot restore. No persistent undo or selective region undo.

---

## 6. Marks & Regions

| Feature | Status | Notes |
|---------|--------|-------|
| Set mark (`C-SPC`) | :white_check_mark: | |
| Exchange point and mark (`C-x C-x`) | :white_check_mark: | |
| Mark word / paragraph / defun / sexp | :white_check_mark: | |
| Select all (`C-x h`) | :white_check_mark: | |
| Narrow to region / widen | :white_check_mark: | |
| Transient mark mode | :large_blue_circle: | Region highlighting |
| Pop mark | :large_blue_circle: | Mark ring navigation |
| Rectangle mark mode | :white_check_mark: | Toggle with `C-x SPC` |

**Summary:** Mark and region system is solid.

---

## 7. Registers & Bookmarks

| Feature | Status | Notes |
|---------|--------|-------|
| Text to register | :white_check_mark: | `C-x r s` / `C-x r i` |
| Point to register / jump | :white_check_mark: | `C-x r SPC` / `C-x r j` |
| Window config to register | :white_check_mark: | Full multi-window state save/restore |
| Rectangle to register | :white_check_mark: | |
| Number registers (increment) | :white_check_mark: | `C-x r +` |
| Append/prepend to register | :white_check_mark: | |
| File to register | :yellow_circle: | Listed but limited |
| List registers | :large_blue_circle: | |
| Bookmark set / jump | :white_check_mark: | `C-x r m` / `C-x r b` |
| Bookmark list | :white_check_mark: | |
| Bookmark persistence | :white_check_mark: | `~/.gemacs-bookmarks` |
| Bookmark delete / rename | :white_check_mark: | |

**Summary:** Registers and bookmarks are comprehensive. All core types work including window configurations.

---

## 8. Rectangle Operations

| Feature | Status | Notes |
|---------|--------|-------|
| Kill rectangle | :white_check_mark: | `C-x r k` |
| Delete rectangle | :white_check_mark: | `C-x r d` |
| Yank rectangle | :white_check_mark: | `C-x r y` |
| Open rectangle | :white_check_mark: | `C-x r o` |
| String rectangle | :white_check_mark: | `C-x r t` — fill column with text |
| Number lines | :white_check_mark: | `C-x r n` |
| Clear rectangle | :white_check_mark: | |
| Rectangle to register | :white_check_mark: | |

**Summary:** Rectangle operations are feature-complete.

---

## 9. Keyboard Macros

| Feature | Status | Notes |
|---------|--------|-------|
| Start recording (`F3` / `C-x (`) | :white_check_mark: | |
| Stop recording (`F4` / `C-x )`) | :white_check_mark: | |
| Execute last macro (`F4` / `C-x e`) | :white_check_mark: | |
| Named macros | :white_check_mark: | `M-x name-last-kbd-macro`, `M-x call-named-kbd-macro` with narrowing |
| Macro counter | :white_check_mark: | `M-x kbd-macro-counter-insert` / `kbd-macro-counter-set` |
| Edit macro | :large_blue_circle: | `M-x edit-kbd-macro` shows macro events in buffer (TUI) |
| Save macros to file | :white_check_mark: | `M-x save-kbd-macros` / `load-kbd-macros` persists to `~/.gemacs-macros` |
| Execute with count prefix | :yellow_circle: | Basic repeat support |

**Summary:** Feature-rich macro system: recording/playback, named macros with save/load persistence, counter insert/set, macro viewer. Qt uses narrowing for macro selection.

---

## 10. Minibuffer & Completion

| Feature | Status | Notes |
|---------|--------|-------|
| M-x (execute-extended-command) | :white_check_mark: | Fuzzy matching command names |
| File name completion | :white_check_mark: | Tab completion in find-file |
| Buffer name completion | :white_check_mark: | Fuzzy matching in switch-buffer |
| Minibuffer history | :white_check_mark: | `M-p` / `M-n` in minibuffer |
| Recursive minibuffer | :white_check_mark: | `toggle-enable-recursive-minibuffers` flag |
| Vertico / Selectrum | :white_check_mark: | Mode toggles, uses narrowing framework for vertical completion |
| Orderless matching | :yellow_circle: | Basic fuzzy; no space-separated orderless |
| Marginalia (annotations) | :white_check_mark: | Annotator registry with `marginalia-annotate!`, command/buffer/file categories |
| Embark (actions on candidates) | :white_check_mark: | Action registry with `embark-define-action!`, describe/execute/find-file actions |
| Consult (enhanced commands) | :yellow_circle: | `consult-grep`, `consult-line` registered but basic |
| Icomplete / Fido mode | :white_check_mark: | `icomplete-mode` / `fido-mode` toggles |
| Savehist (persist history) | :large_blue_circle: | `~/.gemacs-history` |

**Summary:** Full completion framework: fuzzy matching, Vertico/Selectrum vertical modes, Marginalia annotations, Embark actions, Icomplete/Fido. Uses narrowing framework for candidate selection.

---

## 11. Buffer Management

| Feature | Status | Notes |
|---------|--------|-------|
| Switch buffer (`C-x b`) | :white_check_mark: | With fuzzy matching |
| Kill buffer (`C-x k`) | :white_check_mark: | Prompts to save modified |
| List buffers (`C-x C-b`) | :white_check_mark: | |
| Next/previous buffer | :white_check_mark: | `C-x <left>/<right>` |
| Bury buffer | :white_check_mark: | |
| Rename buffer | :white_check_mark: | |
| Clone buffer | :white_check_mark: | |
| Scratch buffer | :white_check_mark: | |
| Messages buffer | :white_check_mark: | `*Messages*` equivalent |
| ibuffer (advanced list) | :yellow_circle: | Registered, limited functionality |
| Uniquify buffer names | :yellow_circle: | Basic — no forward/reverse uniquify styles |
| Indirect buffers | :yellow_circle: | `clone-indirect-buffer` registered |
| Buffer-local variables | :yellow_circle: | Some support via app-state |

**Summary:** Core buffer management works well. Missing advanced ibuffer filtering/grouping.

---

## 12. Window Management

| Feature | Status | Notes |
|---------|--------|-------|
| Split horizontal (`C-x 2`) | :white_check_mark: | |
| Split vertical (`C-x 3`) | :white_check_mark: | |
| Delete window (`C-x 0`) | :white_check_mark: | |
| Delete other windows (`C-x 1`) | :white_check_mark: | |
| Other window (`C-x o`) | :white_check_mark: | |
| Balance windows (`C-x +`) | :white_check_mark: | |
| Resize windows (`C-x ^`, `C-x {`, `C-x }`) | :white_check_mark: | |
| Windmove (directional) | :white_check_mark: | Arrow key navigation between windows |
| Winner mode (undo/redo) | :white_check_mark: | `winner-undo`, `winner-redo` |
| Ace-window (jump by label) | :large_blue_circle: | |
| Swap buffers between windows | :white_check_mark: | |
| Golden ratio mode | :white_check_mark: | Auto-resize focused window |
| Dedicated windows | :white_check_mark: | `M-x toggle-window-dedicated` prevents buffer replacement |
| Side windows | :white_check_mark: | Toggle side panel via split (display-buffer-in-side-window) |
| Window purpose | :white_check_mark: | `set-window-dedicated`, `toggle-window-dedicated` with buffer-type dedication |
| Follow mode | :white_check_mark: | Synchronized scrolling across windows |

**Summary:** Window management is strong. Splitting, resizing, winner-mode, ace-window, window purpose/dedication all work.

---

## 13. Frame / Display Management

| Feature | Status | Notes |
|---------|--------|-------|
| Single frame (Qt window) | :white_check_mark: | |
| Multiple frames | :red_circle: | Single window only |
| Fullscreen toggle | :yellow_circle: | Registered |
| Font size (zoom) | :white_check_mark: | `C-=`, `C--`, `C-x C-0` |
| Font family selection | :large_blue_circle: | Configurable |
| Menu bar | :large_blue_circle: | Qt menu bar with File/Edit/View/etc |
| Tool bar | :red_circle: | Not implemented |
| Scroll bar | :red_circle: | Scintilla handles internally |
| Mode line (status bar) | :white_check_mark: | Shows mode, file, position, modified status |
| Tab bar | :green_circle: | Qt visual buffer tab bar + workspace tabs (both layers) |
| Header line | :white_check_mark: | Toggle header line display (file path breadcrumb) |
| Fringe indicators | :yellow_circle: | Line numbers; no bitmap fringes |
| Display tables | :white_check_mark: | `set-display-table-entry` / `describe-display-table` |

**Summary:** Single-frame Qt application. No multi-frame support. Basic display features work.

---

## 14. File Operations

| Feature | Status | Notes |
|---------|--------|-------|
| Find file (`C-x C-f`) | :white_check_mark: | With completion |
| Find file other window (`C-x 4 f`) | :white_check_mark: | |
| Save buffer (`C-x C-s`) | :white_check_mark: | |
| Save as (`C-x C-w`) | :white_check_mark: | |
| Save some buffers (`C-x s`) | :white_check_mark: | Prompts for each modified |
| Revert buffer | :white_check_mark: | Reload from disk |
| Auto-revert mode | :white_check_mark: | File watcher for external changes |
| Auto-save mode | :large_blue_circle: | Periodic save |
| Backup files | :yellow_circle: | Basic |
| Recent files (`C-x C-r`) | :white_check_mark: | |
| Find file at point | :white_check_mark: | |
| Find alternate file | :white_check_mark: | |
| Insert file | :white_check_mark: | |
| Copy/rename file | :white_check_mark: | |
| Sudo write | :white_check_mark: | Write as root |
| File local variables | :large_blue_circle: | Dir-locals via `.gemacs-config` |
| Find file literally | :yellow_circle: | Registered |
| File encoding detection | :yellow_circle: | Basic — Scintilla handles internally |
| Line ending conversion | :white_check_mark: | Unix/DOS/Mac detection and conversion |

**Summary:** File operations are comprehensive. Find, save, revert, auto-revert, recent files all work.

---

## 15. Dired (Directory Editor)

| Feature | Status | Notes |
|---------|--------|-------|
| Directory listing | :white_check_mark: | File metadata, permissions, sizes |
| Open file/directory | :white_check_mark: | Enter to open |
| Navigate up (`^`) | :white_check_mark: | |
| Create directory | :white_check_mark: | |
| Mark / unmark files | :white_check_mark: | Mark by regexp |
| Delete single file | :white_check_mark: | With confirmation |
| Rename single file | :white_check_mark: | |
| Copy single file | :white_check_mark: | |
| Chmod | :white_check_mark: | |
| Sort toggle | :white_check_mark: | Name/date |
| Hide details | :white_check_mark: | |
| Hide dotfiles | :white_check_mark: | |
| Refresh | :white_check_mark: | |
| Batch delete (marked) | :white_check_mark: | Delete all marked files with confirmation |
| Batch rename (marked) | :white_check_mark: | Move/rename marked files to destination |
| Batch copy (marked) | :white_check_mark: | Copy marked files to destination |
| Mark by regexp | :white_check_mark: | Mark files matching pattern |
| Shell command on file | :large_blue_circle: | Runs command, shows output in buffer |
| Wdired (edit filenames) | :white_check_mark: | Edit mode with rename-on-commit |
| Image thumbnails | :white_check_mark: | `image-dired-display-thumbnail` / `image-dired-show-all-thumbnails` |
| Dired-x extensions | :yellow_circle: | find-dired, find-name-dired |
| Async operations | :white_check_mark: | `dired-async-copy`, `dired-async-move` |
| Virtual dired | :white_check_mark: | `virtual-dired` from file list, `dired-from-find` from glob |
| Dired subtree | :white_check_mark: | `M-x dired-subtree-toggle` for inline expansion |

**Summary:** Dired is **substantially complete**. Full listing with permissions/sizes, single-file and batch operations on marked files, wdired for inline renaming, find integration, inline subtree expansion, async copy/move. Missing: image thumbnails.

---

## 16. Version Control / Magit

| Feature | Status | Notes |
|---------|--------|-------|
| Git status display | :large_blue_circle: | Interactive status with inline diffs per file |
| Stage / unstage hunks | :large_blue_circle: | Hunk-level staging via `git apply --cached` |
| Stage / unstage files | :white_check_mark: | `s` to stage, `u` to unstage in status buffer |
| Commit with message | :yellow_circle: | Opens editor, basic flow |
| Amend commit | :yellow_circle: | Basic |
| Push / pull | :yellow_circle: | Shell passthrough |
| Log viewing | :yellow_circle: | `git log --oneline` last 50 |
| Diff viewing | :large_blue_circle: | Shows staged + unstaged diffs for file at point |
| Branch operations | :large_blue_circle: | Checkout/create/delete with narrowing selection |
| Tag management | :yellow_circle: | Create tags |
| Stash | :large_blue_circle: | Stash create + list + pop |
| Blame | :yellow_circle: | `git blame` output display |
| Interactive rebase | :yellow_circle: | Rebase with narrowing branch selection |
| Merge UI | :yellow_circle: | Merge with narrowing branch selection |
| Cherry-pick | :yellow_circle: | Basic |
| Revert commit | :yellow_circle: | Basic |
| Forge (PR/issue management) | :large_blue_circle: | List/view PRs and issues, create PRs via `gh` CLI |
| Diff-hl (gutter marks) | :large_blue_circle: | Git diff gutter indicators |
| Wgrep on grep results | :white_check_mark: | Edit and save back |
| Magit keymap | :white_check_mark: | 20 bindings: s/S/u/c/d/l/g/n/p/q/b/B/f/F/P/r/m/z/Z/k |
| VC generic backend | :yellow_circle: | Basic git-only |

**Summary:** Magit has been significantly enhanced. The status buffer now shows **inline diffs** per file. **Hunk-level staging/unstaging** works via `git apply --cached`. Branch operations (checkout, merge, rebase) use the **narrowing framework** for interactive selection. 20 single-key bindings in the magit keymap. Forge integration provides PR/issue listing and creation via `gh` CLI. Remaining gaps: commit composition buffer, interactive log with commit details.

### Priority Improvements Needed:
1. Commit composition buffer with diff preview
2. Interactive log with commit details

---

## 17. Org-mode

| Feature | Status | Notes |
|---------|--------|-------|
| Heading hierarchy | :white_check_mark: | `* / ** / ***` levels |
| Heading folding / cycling | :white_check_mark: | TAB cycles visibility |
| TODO states | :white_check_mark: | TODO/DONE cycling, custom keywords |
| Priority (`[#A]`, `[#B]`, `[#C]`) | :white_check_mark: | Set and cycle priorities |
| Tags | :white_check_mark: | Per-heading tags |
| Timestamps | :white_check_mark: | Active/inactive, SCHEDULED/DEADLINE |
| Properties | :white_check_mark: | Property drawers |
| Lists (ordered, unordered) | :large_blue_circle: | |
| Checkboxes | :white_check_mark: | Toggle `[ ]`/`[X]` |
| Links | :white_check_mark: | `[[url][description]]` format |
| Footnotes | :yellow_circle: | Basic |
| **Tables** | :white_check_mark: | Create, align, row/col operations, sort, sum |
| Table formulas | :large_blue_circle: | Basic recalculate |
| Table CSV import/export | :white_check_mark: | |
| **Agenda** | :large_blue_circle: | Daily/weekly views, date filtering, tag search |
| Agenda interactive commands | :large_blue_circle: | Jump to source, toggle TODO from agenda |
| **Capture** | :large_blue_circle: | Templates with `%?/%U/%T/%f`, template selection, `*Org Capture*` buffer |
| Capture buffer (C-c C-c / C-c C-k) | :white_check_mark: | Interactive capture with finalize/abort keybindings |
| Refile | :large_blue_circle: | `M-x org-refile` with narrowing target selection (Qt) |
| **Babel** (code blocks) | :large_blue_circle: | 8 languages, execution, tangling |
| Babel session persistence | :white_check_mark: | `:session name` keeps persistent process, sentinel-based I/O |
| Babel :var evaluation | :white_check_mark: | Resolves named src blocks (executes) and tables (converts to data) |
| Babel :noweb expansion | :white_check_mark: | `<<block-name>>` refs expanded when `:noweb yes` |
| **Export** | :large_blue_circle: | HTML, Markdown, LaTeX, ASCII |
| Export footnotes/cross-refs | :white_check_mark: | `[fn:name]` refs, `<<target>>`/`[[#target]]` cross-refs, all 4 backends |
| Custom export backends | :white_check_mark: | Register via `org-export-register-backend!`, list with `org-export-list-backends` |
| **Clock tracking** | :large_blue_circle: | Clock-in/out, goto |
| Org-crypt | :yellow_circle: | Registered |
| Heading promote/demote | :white_check_mark: | |
| Move subtree up/down | :white_check_mark: | |
| Template expansion (`<s TAB`) | :white_check_mark: | Source block templates |
| Sparse tree | :white_check_mark: | Regexp search, shows matching headings + parents |
| Column view | :white_check_mark: | Tabular display of heading level, title, TODO, priority |

**Summary:** Org-mode is one of gemacs's strongest features with substantial coverage of the core: headings, TODO, tables, babel, export, agenda, column view. Interactive agenda supports jump-to-source and TODO toggling. Refile with narrowing target selection. Sparse tree with regexp matching.

---

## 18. Programming Support

| Feature | Status | Notes |
|---------|--------|-------|
| Syntax highlighting | :white_check_mark: | Via Scintilla lexers |
| Auto-indentation | :large_blue_circle: | Language-aware for Lisp; basic for others |
| Code folding | :white_check_mark: | Toggle/fold-all/unfold-all |
| Show matching paren | :white_check_mark: | Highlight matching delimiter |
| Comment/uncomment | :white_check_mark: | `M-;`, `F11`/`F12` |
| S-expression navigation | :white_check_mark: | Forward/backward/up/down sexp |
| Compile command | :white_check_mark: | `C-x d` runs compile |
| Error navigation | :white_check_mark: | Next/prev error in compilation buffer |
| Flycheck (syntax checking) | :yellow_circle: | Toggle registered; LSP diagnostics partial |
| Flymake | :yellow_circle: | Registered |
| Eldoc (function signatures) | :large_blue_circle: | Echo area display |
| Xref (definitions) | :large_blue_circle: | Grep-based fallback |
| Xref (references) | :large_blue_circle: | Grep-based |
| Imenu (symbol index) | :large_blue_circle: | Works for structured languages |
| Which-function mode | :yellow_circle: | Registered |
| Semantic analysis | :red_circle: | No built-in semantic parsing |
| Tree-sitter integration | :red_circle: | Not implemented |
| DAP (debug adapter) | :red_circle: | Not implemented |
| Prog-mode hooks | :yellow_circle: | Limited |
| Electric indent | :large_blue_circle: | Smart newline indentation |

**Summary:** Programming support covers the basics well (highlighting, folding, compilation, error nav). Missing Tree-sitter, DAP debugger, and deep semantic analysis.

---

## 19. LSP (Language Server Protocol)

| Feature | Status | Notes |
|---------|--------|-------|
| JSON-RPC transport | :white_check_mark: | Content-Length framing, background thread |
| Server lifecycle | :white_check_mark: | Start/stop/restart with auto-start on file open |
| Document sync | :white_check_mark: | didOpen/didChange/didSave/didClose |
| Diagnostics display | :white_check_mark: | Scintilla indicators + margin markers + modeline |
| Completion | :white_check_mark: | QCompleter popup, auto-complete on idle, Tab merge |
| Hover (tooltip) | :white_check_mark: | Echo area display on `C-c l h` |
| Go-to-definition | :white_check_mark: | `M-.` with smart dispatch, `M-,` to pop back |
| Find references | :white_check_mark: | `C-c l r` shows in compilation buffer |
| Rename refactoring | :white_check_mark: | `C-c l R` with echo prompt |
| Code actions | :white_check_mark: | `C-c l a` with selection popup |
| Formatting | :white_check_mark: | `C-c l f` format buffer/region |
| Semantic tokens | :white_check_mark: | Toggle with `lsp-semantic-tokens`, indicator-based highlighting |
| Call hierarchy | :white_check_mark: | `lsp-incoming-calls` / `lsp-outgoing-calls` with navigation |
| Type hierarchy | :white_check_mark: | `lsp-supertypes` / `lsp-subtypes` with navigation buffer |
| Inlay hints | :white_check_mark: | Toggle with `lsp-inlay-hints`, shows in echo area on idle |
| Workspace symbols | :white_check_mark: | `C-c l s` with completion |
| Multi-server support | :white_check_mark: | `lsp-set-server` / `lsp-list-servers` with per-language registry |

**Summary:** LSP is fully functional — auto-starts on file open, provides completion (auto + Tab + C-M-i), diagnostics with inline indicators, go-to-definition (M-.), hover, references, rename, code actions, formatting, workspace symbols, semantic tokens, call hierarchy, type hierarchy, and inlay hints. All under `C-c l` prefix. Only missing multi-server support.
5. Wire find-references to results buffer

---

## 20. Syntax Highlighting & Themes

| Feature | Status | Notes |
|---------|--------|-------|
| Scintilla lexer system | :white_check_mark: | Many built-in lexers |
| Gerbil/Scheme highlighting | :white_check_mark: | Custom keyword lists |
| C/C++ highlighting | :white_check_mark: | Via Scintilla |
| Python highlighting | :white_check_mark: | Via Scintilla |
| JavaScript highlighting | :white_check_mark: | Via Scintilla |
| HTML/CSS highlighting | :white_check_mark: | Via Scintilla |
| Markdown highlighting | :white_check_mark: | Via Scintilla |
| Org-mode highlighting | :large_blue_circle: | Custom lexer |
| Theme system | :white_check_mark: | 8 built-in themes |
| Custom face definitions | :white_check_mark: | Foreground, background, bold, italic |
| Per-buffer lexer | :white_check_mark: | Based on file extension |
| Font-lock (regex-based) | :red_circle: | Uses Scintilla lexers instead — different model |
| Tree-sitter highlighting | :red_circle: | Not implemented |
| Rainbow delimiters | :green_circle: | Depth-based coloring via indicators (8 colors, both layers) |

**Built-in Themes:**
1. Dark (default)
2. Light
3. Solarized
4. Monokai
5. Gruvbox
6. Dracula
7. Nord
8. Zenburn

**Summary:** Highlighting works well via Scintilla's lexer system. Good theme selection. Missing tree-sitter for more accurate highlighting.

---

## 21. Completion Frameworks

| Feature | Status | Notes |
|---------|--------|-------|
| Dabbrev (dynamic abbrev) | :white_check_mark: | `M-/` word completion from buffer |
| Hippie-expand | :yellow_circle: | Registered |
| Complete at point | :white_check_mark: | `C-M-i` |
| Company mode | :yellow_circle: | QCompleter popup — not company but equivalent |
| Corfu mode | :yellow_circle: | Echo-area + QCompleter popup |
| Cape (completion extensions) | :white_check_mark: | `cape-dabbrev`, `cape-file`, `cape-history`, `cape-keyword` |
| File path completion | :white_check_mark: | In minibuffer |
| Symbol completion | :white_check_mark: | Buffer words + LSP merged on Tab |
| LSP completion | :white_check_mark: | Auto-complete on idle + C-M-i + Tab merge |
| Snippet completion | :large_blue_circle: | TAB expands snippet triggers; `M-x snippet-insert` for browsing |
| Copilot/AI completion | :yellow_circle: | Mode toggle + accept/next/inline commands — needs API key |

**Summary:** Full completion framework: QCompleter popup with buffer words + LSP merged, auto-triggers on idle. Cape backends (dabbrev, file, history, keyword), Copilot mode toggle with accept/next. AI inline suggestions and code explain/refactor scaffolded.

---

## 22. Shell & Terminal

| Feature | Status | Notes |
|---------|--------|-------|
| Shell command (`M-!`) | :white_check_mark: | Run command, show output |
| Shell command on region (`M-\|`) | :white_check_mark: | Pipe region to command |
| Async shell command (`M-&`) | :white_check_mark: | |
| Eshell | :yellow_circle: | Built-in commands (cd, ls, cat, echo), Scheme eval; no I/O redirect, no globbing |
| Terminal (term/ansi-term) | :large_blue_circle: | PTY support, ANSI colors, signals |
| Vterm | :red_circle: | Not implemented (libvterm) |
| Shell mode | :large_blue_circle: | External shell buffer |
| Compilation mode | :white_check_mark: | Error parsing, navigation |
| Comint (process interaction) | :yellow_circle: | Basic subprocess I/O |
| Process sentinels/filters | :white_check_mark: | `set-process-sentinel!`, `set-process-filter!` API |

**Summary:** Shell command execution works well. Terminal mode provides PTY with ANSI support. Eshell is basic. Missing vterm and full process management.

---

## 23. Spell Checking

| Feature | Status | Notes |
|---------|--------|-------|
| Ispell word | :large_blue_circle: | Check word at point with suggestions |
| Ispell region | :large_blue_circle: | Scan region for misspellings |
| Ispell buffer | :large_blue_circle: | Whole-buffer check |
| Suggestion menu | :white_check_mark: | Interactive selection from ispell output |
| Flyspell (on-the-fly) | :large_blue_circle: | `flyspell-mode` toggles on-demand spell check with aspell; TUI uses Scintilla squiggle indicators, Qt reports misspelled words |
| Personal dictionary | :large_blue_circle: | Supported via ispell |
| Language selection | :white_check_mark: | `ispell-change-dictionary` with narrowing (Qt) or prompt (TUI) |
| Aspell/Hunspell backend | :large_blue_circle: | Uses ispell subprocess |

**Summary:** Interactive spell-checking works via ispell with language selection. Flyspell mode provides on-demand buffer spell-checking with visual indicators (TUI) and word-list reporting (Qt) using aspell backend.

---

## 24. Text Transformation & Formatting

| Feature | Status | Notes |
|---------|--------|-------|
| Upcase/downcase word | :white_check_mark: | `M-u`, `M-l` |
| Capitalize word | :white_check_mark: | `M-c` |
| Upcase/downcase region | :white_check_mark: | `C-x C-u`, `C-x C-l` |
| Sort lines | :white_check_mark: | Alphabetic, numeric, reverse, case-fold |
| Sort fields | :white_check_mark: | Sort by nth field |
| Align regexp | :white_check_mark: | Column alignment |
| Fill paragraph | :white_check_mark: | Reflow to fill-column |
| Unfill paragraph | :white_check_mark: | Join into single line |
| Center region | :white_check_mark: | |
| Tabify / untabify | :white_check_mark: | |
| Delete trailing whitespace | :white_check_mark: | |
| Base64 encode/decode | :white_check_mark: | |
| ROT13 | :white_check_mark: | |
| Hex dump (hexl-mode) | :white_check_mark: | |
| Checksum (SHA256) | :white_check_mark: | |
| UUID generation | :white_check_mark: | |
| Camel/snake case conversion | :white_check_mark: | |
| Duplicate line/region | :white_check_mark: | |
| Reverse region/chars | :white_check_mark: | |
| Delete duplicate lines | :white_check_mark: | |
| Word frequency | :white_check_mark: | |
| Count words/chars/lines | :white_check_mark: | |

**Summary:** Text transformation is feature-complete. All standard Emacs text operations are present.

---

## 25. S-expression / Paredit

| Feature | Status | Notes |
|---------|--------|-------|
| Forward/backward sexp | :white_check_mark: | |
| Up/down list | :white_check_mark: | |
| Kill sexp | :white_check_mark: | |
| Mark sexp | :white_check_mark: | |
| Indent sexp | :white_check_mark: | |
| Slurp forward | :white_check_mark: | Extend sexp to include next |
| Barf forward | :white_check_mark: | Move last element out |
| Wrap in parens/brackets/braces | :white_check_mark: | |
| Splice sexp | :white_check_mark: | Remove delimiters, keep contents |
| Raise sexp | :white_check_mark: | Replace parent with child |
| Split sexp | :white_check_mark: | Break at point |
| Join sexps | :white_check_mark: | Merge adjacent |
| Backward slurp/barf | :white_check_mark: | `paredit-slurp-backward`, `paredit-barf-backward` |
| Convolute sexp | :white_check_mark: | `M-x paredit-convolute-sexp` swaps inner/outer |
| Paredit strict mode | :white_check_mark: | Prevents deleting delimiters that would unbalance; allows empty pair deletion |
| Smartparens | :large_blue_circle: | Aliased to paredit strict mode |

**Summary:** Complete paredit: forward and backward slurp/barf, wrap, splice, raise, split, join, convolute, strict mode.

---

## 26. Diff & Ediff

| Feature | Status | Notes |
|---------|--------|-------|
| Diff buffer vs file | :white_check_mark: | `C-c d` |
| Unified diff display | :large_blue_circle: | |
| Ediff two buffers | :large_blue_circle: | Side-by-side diff display |
| Ediff files | :large_blue_circle: | Compare two files from disk |
| Ediff directories | :large_blue_circle: | Recursive directory comparison |
| Ediff regions | :large_blue_circle: | Compare buffer regions |
| Ediff merge | :large_blue_circle: | Two-file merge comparison |
| Ediff three-way merge | :white_check_mark: | `diff3 -m` mine/base/theirs with conflict markers |
| Refine hunks | :white_check_mark: | Word-level diff via `M-x diff-refine-hunk` |
| Smerge mode | :white_check_mark: | Full conflict marker resolution |
| Smerge navigate (n/p) | :white_check_mark: | Jump between `<<<<<<<` markers |
| Smerge keep mine/other/both | :white_check_mark: | Resolve conflicts interactively |
| Smerge conflict count | :white_check_mark: | Shows total conflicts in buffer |

**Summary:** Diff display and smerge conflict resolution fully working. Ediff provides file/buffer/directory/three-way merge comparison. Word-level hunk refinement via `diff-refine-hunk`.

---

## 27. Project Management

| Feature | Status | Notes |
|---------|--------|-------|
| Project detection (.git, etc.) | :white_check_mark: | Auto-detect project root |
| Project file find | :white_check_mark: | Find file in project |
| Project grep/search | :white_check_mark: | Grep across project |
| Project compile | :white_check_mark: | |
| Project switch | :large_blue_circle: | Switch between projects |
| Project shell/eshell | :large_blue_circle: | Shell in project root |
| Project dired | :large_blue_circle: | Dired at project root |
| Project-aware buffer list | :large_blue_circle: | |
| Per-project settings | :large_blue_circle: | `.gemacs-config` directory-local settings (applied on file open) |
| Project-specific keymaps | :white_check_mark: | Load `.gemacs-keys` from project root |
| Projectile integration | :yellow_circle: | Basic command registration |
| project.el features | :yellow_circle: | Basic |

**Summary:** Project management works well. Per-project settings via `.gemacs-config` dir-locals. Missing project-specific keymaps.

---

## 28. Help System

| Feature | Status | Notes |
|---------|--------|-------|
| Describe key (`C-h k`) | :white_check_mark: | Shows command bound to key |
| Describe command (`C-h f`) | :white_check_mark: | |
| Describe variable (`C-h v`) | :yellow_circle: | Registered |
| List all bindings (`C-h b`) | :white_check_mark: | |
| Where is (`C-h w`) | :white_check_mark: | Find key for command |
| Apropos (`C-h a`) | :white_check_mark: | Search commands by keyword |
| View lossage (`C-h l`) | :white_check_mark: | Last 300 keystrokes |
| Command history | :white_check_mark: | |
| Info reader | :white_check_mark: | Built-in Info browser with topics: commands, keybindings, org, config, about |
| Emacs tutorial | :white_check_mark: | Built-in tutorial with navigation, editing, files, search, windows, org mode |
| Built-in documentation browser | :white_check_mark: | `gemacs-doc` with topic browsing (getting-started, keybindings, commands, org-mode) |

**Summary:** Help system is complete with describe-key, describe-function, apropos, Info reader, documentation browser, and tutorial.

---

## 29. Customization & Configuration

| Feature | Status | Notes |
|---------|--------|-------|
| Init file (`~/.gemacs-init`) | :white_check_mark: | Gerbil Scheme init file |
| Dir-locals (`.gemacs-config`) | :large_blue_circle: | Directory-local variable settings |
| Key binding customization | :white_check_mark: | Programmatic and via custom-keys |
| Key-chord bindings | :white_check_mark: | Two-key chord system |
| Theme selection | :white_check_mark: | 8 built-in + custom |
| Font customization | :white_check_mark: | Family, size |
| Fill column | :white_check_mark: | Configurable |
| Tab width | :white_check_mark: | |
| Indent style (tabs/spaces) | :white_check_mark: | |
| Scroll margin | :white_check_mark: | |
| M-x customize UI | :white_check_mark: | `M-x customize` shows settings buffer, `M-x set-variable` to change |
| Custom variables (defcustom) | :white_check_mark: | Customizable variables with getters/setters |
| Custom groups | :white_check_mark: | `customize-group` with editing/display/files categories |
| Face customization UI | :white_check_mark: | `customize-face`, `set-face-attribute` for face properties |
| Mode-specific hooks | :yellow_circle: | Limited hook system |

**Summary:** Configuration via init file and `M-x customize` interactive buffer. `set-variable` for runtime changes.

---

## 30. Package Management & Extensibility

| Feature | Status | Notes |
|---------|--------|-------|
| Elisp scripting | :red_circle: | **Gerbil Scheme instead** — not compatible with Emacs packages |
| package.el / MELPA | :red_circle: | N/A — different language |
| use-package | :red_circle: | N/A |
| straight.el | :red_circle: | N/A |
| Plugin/package system | :white_check_mark: | `load-plugin`, `list-plugins`, `~/.gemacs-plugins/` directory |
| User-defined commands | :large_blue_circle: | Via `~/.gemacs-init` Gerbil code |
| Advice system | :white_check_mark: | `advice-add!`/`advice-remove!` with before/after, `describe-advice` |
| Hook system | :yellow_circle: | Limited — some mode hooks |
| Autoload system | :white_check_mark: | `autoload!` to register, `list-autoloads` to view |
| Dynamic module loading | :white_check_mark: | `load-module` / `list-modules` for runtime Gerbil module loading |

**Summary:** Gemacs uses Gerbil Scheme, not Emacs Lisp — the MELPA ecosystem is unavailable. But has a plugin system (`~/.gemacs-plugins/` with `load-plugin`/`list-plugins`) and init file extensibility.

---

## 31. Remote Editing (TRAMP)

| Feature | Status | Notes |
|---------|--------|-------|
| SSH file editing | :yellow_circle: | `tramp-ssh-edit` with `/ssh:host:path` syntax |
| TRAMP sudo | :yellow_circle: | `sudo-write` exists, `tramp-sudo` stub |
| Docker container editing | :yellow_circle: | `tramp-docker-edit` with `/docker:name:path` syntax |
| Remote shell | :yellow_circle: | `tramp-remote-shell` command |
| Remote compilation | :yellow_circle: | `tramp-remote-compile` command |

**Summary:** No remote editing support. `sudo-write` exists for local privilege escalation.

---

## 32. EWW (Web Browser)

| Feature | Status | Notes |
|---------|--------|-------|
| URL fetching (HTTP/HTTPS) | :white_check_mark: | |
| HTML to text conversion | :yellow_circle: | Basic tag stripping |
| Navigation history | :white_check_mark: | Back/forward |
| Link display | :yellow_circle: | Shows URLs, no clickable links |
| Form submission | :red_circle: | Not implemented |
| CSS rendering | :red_circle: | Not implemented |
| Image display | :red_circle: | Not implemented |
| JavaScript | :red_circle: | Not implemented |
| Bookmarks | :white_check_mark: | `eww-add-bookmark` / `eww-list-bookmarks`, persisted to `~/.gemacs-eww-bookmarks` |

**Summary:** Basic text-mode web browsing. Fetches pages and strips HTML. Not usable for modern web pages.

---

## 33. Calendar & Diary

| Feature | Status | Notes |
|---------|--------|-------|
| Calendar display | :orange_circle: | Monthly grid, navigation |
| Today highlighting | :white_check_mark: | |
| Navigate months/years | :white_check_mark: | |
| Diary integration | :white_check_mark: | `diary-insert-entry` adds to `~/.gemacs-diary`, `diary-view-entries` shows entries |
| Holiday display | :white_check_mark: | US holidays shown on calendar, `calendar-holidays` command |
| Appointment reminders | :white_check_mark: | `appt-check` scans org deadlines + diary for next 15 min |
| Org-agenda integration | :white_check_mark: | Calendar footer shows org DEADLINE/SCHEDULED items for month |

**Summary:** Calendar with 3-month grid, navigation, holiday display, diary integration, org-agenda items in calendar, and appointment reminders.

---

## 34. Email

| Feature | Status | Notes |
|---------|--------|-------|
| Gnus | :red_circle: | Not implemented |
| mu4e | :red_circle: | Not implemented |
| notmuch | :red_circle: | Not implemented |
| message-mode (compose) | :red_circle: | Not implemented |

**Summary:** No email client. This is common for modern Emacs alternatives to skip.

---

## 35. IRC / Chat

| Feature | Status | Notes |
|---------|--------|-------|
| ERC (IRC) | :red_circle: | Not implemented |
| rcirc | :red_circle: | Not implemented |

**Summary:** No chat/IRC client.

---

## 36. PDF / Document Viewing

| Feature | Status | Notes |
|---------|--------|-------|
| PDF viewing (pdf-tools) | :red_circle: | Not implemented |
| DocView | :red_circle: | Not implemented |
| Image viewing | :large_blue_circle: | Image buffers in Qt layer |

**Summary:** No PDF viewing. Image display works in Qt layer.

---

## 37. Treemacs / File Tree

| Feature | Status | Notes |
|---------|--------|-------|
| File tree sidebar | :white_check_mark: | `M-x project-tree` with expand/collapse |
| Project tree | :white_check_mark: | Tree view with depth limit, hidden file filtering |
| Git status in tree | :white_check_mark: | Shows M/A/?/D/R status per file in project tree |
| File operations in tree | :white_check_mark: | Create, delete, rename files in project tree |

**Summary:** Project tree sidebar is fully featured with directory structure, git status indicators, and file operations (create, delete, rename).

---

## 38. Multiple Cursors / iedit

| Feature | Status | Notes |
|---------|--------|-------|
| mc-mark-next | :white_check_mark: | Mark next occurrence of selection |
| mc-mark-all | :white_check_mark: | Mark all occurrences |
| mc-edit-lines | :white_check_mark: | Add cursor to each line in region |
| mc-skip-and-mark-next | :white_check_mark: | Skip current, mark next |
| mc-unmark-last | :white_check_mark: | Remove last cursor |
| mc-rotate | :white_check_mark: | Rotate between cursors |
| iedit (edit all occurrences) | :white_check_mark: | Rename symbol at point across buffer |
| Symbol highlighting + edit | :white_check_mark: | `highlight-symbol` + iedit |

**Summary:** Full multiple cursors and iedit support in both TUI and Qt layers. Keybindings: `C-c m n`, `C-c m a`.

---

## 39. Snippets (YASnippet)

| Feature | Status | Notes |
|---------|--------|-------|
| Snippet expansion | :white_check_mark: | TAB expands trigger → template with field navigation |
| Snippet library | :large_blue_circle: | 100+ built-in snippets across 9 languages |
| Snippet creation | :white_check_mark: | `M-x define-snippet` interactive definition |
| Tabstop navigation | :white_check_mark: | TAB/$1→$2→...→$0 field jumping |
| Default field values | :large_blue_circle: | `${1:default}` syntax supported |
| Snippet browsing | :white_check_mark: | `M-x snippet-insert` with narrowing |
| File-based snippets | :large_blue_circle: | Load from `~/.gemacs-snippets/<lang>/` |
| Mirror fields | :large_blue_circle: | Same $N tracked at all positions; TAB visits each |

**Built-in snippet languages:** Scheme/Gerbil, Python, JavaScript, C/C++, Go, Rust, HTML, Shell/Bash, Markdown, plus global snippets.

**Summary:** Full snippet system with TAB-triggered expansion, field navigation ($1→$2→$0), default values, mirror field tracking, 100+ built-in snippets, file-based loading, and narrowing-based browsing. Both TUI and Qt. Mirror fields track all positions of the same $N for TAB navigation (auto-sync on edit not yet implemented).

---

## 40. Tab Bar & Workspaces

| Feature | Status | Notes |
|---------|--------|-------|
| Tab bar mode | :green_circle: | Qt: visual buffer tab bar with click-to-switch; TUI: workspace tabs |
| Create/close tabs | :green_circle: | `tab-new`, `tab-close` (both layers) |
| Switch tabs | :green_circle: | `tab-next`, `tab-previous` with wrap-around (both layers) |
| Rename tabs | :green_circle: | `tab-rename` (both layers) |
| Move tabs | :green_circle: | `tab-move` with prefix arg direction (both layers) |
| Tab-line (per-window) | :yellow_circle: | Qt has visual buffer tab bar; no per-window tab line |
| Workspace/perspective | :green_circle: | Workspace tabs save/restore window buffer state per tab |
| Named workspaces | :green_circle: | Tabs have names, renameable via `tab-rename` |

**Summary:** Full workspace tab system with create/close/switch/rename/move. Qt has visual buffer tab bar (clickable buttons). Both layers save/restore window buffer configurations per workspace tab. Emacs aliases (`tab-bar-new-tab`, `tab-bar-close-tab`, `tab-bar-switch-to-tab`) registered.

---

## 41. Accessibility

| Feature | Status | Notes |
|---------|--------|-------|
| Keyboard-only operation | :white_check_mark: | All features keyboard-accessible |
| Screen reader support | :red_circle: | No AT-SPI / accessibility API |
| High contrast themes | :large_blue_circle: | Dark/light themes available |
| Font scaling | :white_check_mark: | Zoom in/out/reset |
| Blink cursor | :white_check_mark: | Toggleable |
| Long-line handling (so-long) | :white_check_mark: | Auto-disable features on long lines |

**Summary:** Keyboard-first design but no screen reader integration.

---

## 42. Performance & Large Files

| Feature | Status | Notes |
|---------|--------|-------|
| Large file handling | :large_blue_circle: | Scintilla handles large files well |
| Long line handling | :white_check_mark: | So-long mode, truncate lines |
| Incremental display | :white_check_mark: | Scintilla viewport rendering |
| Background process | :large_blue_circle: | LSP reader thread |
| Garbage collection tuning | :yellow_circle: | Gerbil/Gambit GC |
| Native compilation | :red_circle: | No native comp (Gerbil is interpreted/compiled differently) |

**Summary:** Good performance characteristics thanks to Scintilla's native text handling.

---

## Critical Feature Gap Summary

### Tier 1 — Dealbreakers for Daily Use

| Gap | Impact | Effort |
|-----|--------|--------|
| **Completion popup (Company/Corfu)** | No inline completion UI for code | Medium |

### Tier 2 — Expected by Power Users

| Gap | Impact | Effort |
|-----|--------|--------|
| **Modern completion (Vertico/Orderless)** | Vertico/Selectrum modes, Cape backends, fuzzy matching — Done | Low |
| ~~Multiple cursors / iedit~~ | ~~Can't edit multiple occurrences simultaneously~~ Implemented: iedit-mode with highlight + edit all | ~~Medium~~ Done |
| ~~Snippet system (YASnippet)~~ | ~~No template expansion~~ Implemented: 100+ snippets, tabstops | ~~Medium~~ Done |
| ~~Ediff / Smerge~~ | ~~Can't resolve merge conflicts~~ Implemented: smerge-mode with keep-mine/other/both | ~~Medium~~ Done |
| ~~Flyspell (on-the-fly spell)~~ | ~~No background spell checking~~ Implemented: flyspell-mode with aspell, squiggle indicators | ~~Small~~ Done |
| ~~Undo tree~~ | ~~Linear undo only~~ Implemented: undo-history with timestamped snapshots, restore by number | ~~Medium~~ Done |
| ~~Interactive agenda~~ | ~~Can't act on agenda items~~ Implemented: goto source, toggle TODO | ~~Medium~~ Done |

### Tier 3 — Nice to Have

| Gap | Impact | Effort |
|-----|--------|--------|
| **TRAMP (remote editing)** | Can't edit files over SSH/Docker | Large |
| ~~Tab bar / workspaces~~ | ~~No visual workspace management~~ Implemented: workspace tabs (create/close/switch/rename/move), Qt visual buffer tab bar | ~~Medium~~ Done |
| **Tree-sitter highlighting** | Less accurate highlighting than modern Emacs | Large |
| ~~Package/plugin system~~ | ~~Users can't extend gemacs easily~~ Implemented: `~/.gemacs-plugins/`, `load-plugin`/`list-plugins`, dynamic module loading | ~~Large~~ Done |
| ~~Org capture buffer~~ | ~~No interactive capture~~ Implemented: template selection, `*Org Capture*` buffer, C-c C-c / C-c C-k | ~~Small~~ Done |
| ~~Named keyboard macros~~ | ~~Only last-recorded macro~~ Implemented: name, call, save/load | ~~Small~~ Done |
| ~~Info reader~~ | ~~Can't browse GNU documentation~~ Implemented: `info-reader` with node navigation | ~~Medium~~ Done |
| **PDF viewing** | No document viewer | Large |

### Tier 4 — Emacs-Specific (Low Priority)

| Gap | Impact | Effort |
|-----|--------|--------|
| Email (Gnus/mu4e) | Most users use dedicated email clients | Very Large |
| IRC (ERC) | Most users use dedicated chat clients | Large |
| ~~M-x customize UI~~ | ~~Programmatic config is fine for power users~~ Implemented: `customize`, `set-variable`, custom groups, face editor | ~~Medium~~ Done |
| Elisp compatibility | Fundamental architecture choice (Gerbil vs Elisp) | N/A |

---

## What Gemacs Does Well (Relative to Emacs)

| Strength | Detail |
|----------|--------|
| **Qt6 GUI** | Native GUI with proper font rendering, unlike Emacs's X11/GTK layer |
| **Scintilla backend** | Battle-tested text component with excellent large-file performance |
| **Dual-layer architecture** | Same commands work in TUI and Qt |
| **Org-mode** | Surprisingly complete for a non-Emacs implementation |
| **Wgrep** | Full edit-grep-results workflow |
| **Rectangle operations** | Feature-complete |
| **Text transforms** | Comprehensive set of text operations |
| **Register system** | All register types including window configurations |
| **Paredit** | Solid s-expression editing |
| **Startup time** | Faster than Emacs (no Elisp initialization) |
| **Gerbil Scheme** | Modern Scheme with actors, contracts, and better concurrency than Elisp |

---

---

## 43. AI / LLM Integration

| Feature | Status | Notes |
|---------|--------|-------|
| Copilot (code completion) | :yellow_circle: | Mode toggle, accept/next commands — needs API connection |
| GPTel / LLM chat | :white_check_mark: | `M-x claude-chat` — streaming chat via `claude -p` |
| Claude shell / chat | :white_check_mark: | `*AI Chat*` buffer with `--continue` for context |
| Inline AI suggestions | :yellow_circle: | `ai-inline-suggest` mode toggle — needs API provider |
| Code explanation / refactor via AI | :yellow_circle: | `ai-code-explain`, `ai-code-refactor` commands — needs API key |

**Summary:** Claude CLI chat integration works — `M-x claude-chat` opens a chat buffer, Enter sends prompts, responses stream in real-time. Uses `--continue` for conversation context. Both TUI and Qt.

---

## 44. Multi-Terminal (vterm)

| Feature | Status | Notes |
|---------|--------|-------|
| Vterm (libvterm) | :red_circle: | Not implemented (uses PTY instead) |
| Multi-vterm (multiple terminals) | :white_check_mark: | `term-list`, `term-next`, `term-prev` commands |
| Vterm copy mode | :white_check_mark: | Terminal copy mode with `C-c C-k` / `C-c C-j` |
| Terminal per-project | :white_check_mark: | `M-x project-term` opens/switches to project terminal |
| Term / ansi-term | :large_blue_circle: | Basic PTY terminal with ANSI support |

**Summary:** Multi-terminal management works — create, list, cycle, copy mode, and per-project terminals. Uses PTY instead of libvterm but functionally equivalent for most workflows.

---

## 45. Key Input Remapping

| Feature | Status | Notes |
|---------|--------|-------|
| Key-chord bindings | :white_check_mark: | Two-key simultaneous chords |
| Key translation table | :white_check_mark: | Character remapping |
| Swap brackets/parens | :white_check_mark: | `M-x toggle-bracket-paren-swap` and `M-x key-translation-list` |
| Super/Hyper key mapping | :white_check_mark: | `toggle-super-key-mode` (super → meta), `key-translate` |
| Per-mode keymaps | :yellow_circle: | Limited |
| Global key remap (input-decode-map) | :white_check_mark: | `key-translate` + `describe-key-translations` |

**Summary:** Key-chord system works well. Bracket/paren swap via key-translate system. Missing super-to-meta mapping.

---

## 46. DevOps / Infrastructure Modes

| Feature | Status | Notes |
|---------|--------|-------|
| Docker mode | :white_check_mark: | `docker`, `docker-containers`, `docker-images`, `dockerfile-mode` |
| Terraform mode | :large_blue_circle: | Syntax highlighting via C lexer (`.tf`, `.tfvars`, `.hcl`) |
| Ansible mode | :white_check_mark: | `ansible-mode` (YAML highlighting) |
| Systemd unit files | :white_check_mark: | `systemd-mode` |
| YAML mode | :large_blue_circle: | Syntax highlighting via Scintilla |
| Kubernetes / k8s | :white_check_mark: | `kubernetes-mode` (YAML highlighting) |
| SSH management | :white_check_mark: | `ssh-config-mode` for config editing |

**Summary:** Terraform/HCL has syntax highlighting. YAML mode works. Docker uses bash lexer. Missing dedicated Ansible, Kubernetes, and Docker modes.

---

## 47. Helm / Narrowing Framework

| Feature                   | Status       | Notes                                        |
|---------------------------|--------------|----------------------------------------------|
| Helm (or equivalent)      | :white_check_mark: | QListWidget narrowing with fuzzy filter, C-n/C-p nav |
| Helm M-x                  | :white_check_mark: | Real-time filtered candidate list with match count |
| Helm buffers              | :white_check_mark: | MRU-ordered buffer list with narrowing |
| Helm find-files           | :yellow_circle: | Tab completion, no live narrowing list yet |
| Helm occur                | :white_check_mark: | `helm-occur` with interactive filtering       |
| Helm dash (documentation) | :white_check_mark: | `helm-dash` docset search                    |
| Helm C-yasnippet          | :white_check_mark: | `helm-c-yasnippet` delegates to snippet-insert |

**Summary:** Narrowing framework works for M-x, buffer switch, bookmarks, recent files, imenu, describe-function, and theme selection. Real-time fuzzy filtering with match count display. Find-files still uses Tab completion (no narrowing list).

---

## Personal Workflow Gap Analysis

> *Based on review of the user's Doom Emacs configuration at `~/mine/emacs/`*

### What the User Actually Uses Daily

| Feature                                                 | Emacs Status     | Gemacs Status                | Gap Severity                             |
|---------------------------------------------------------|------------------|------------------------------|------------------------------------------|
| **Key-chords** (30+ bindings: AS, ZX, BV, FG, KB, etc.) | Extensive        | :white_check_mark: Works     | None — key-chord system exists           |
| **Helm** (M-x, buffers, files, grep)                    | Primary UI       | :large_blue_circle: Narrowing | **Low** — M-x/buffers/bookmarks work    |
| **Magit + Forge** (staging, commit, PR review)          | Daily driver     | :white_check_mark: Works     | None — hunk staging, inline diffs, forge PR/issue |
| **Multi-vterm** (multiple terminals, copy mode)         | Heavy use        | :white_check_mark: Works     | None — term-list/next/prev + copy mode   |
| **Eglot / LSP** (completion, hover, goto-def, refs)     | Working          | :white_check_mark: Works     | None — full UI wiring with keybindings   |
| **Copilot / AI** (gptel, claude-shell, copilot)         | Active           | :yellow_circle: Commands scaffolded | **Low** — needs API keys for real AI    |
| **Corfu** (completion-at-point popup)                   | Active           | :yellow_circle: Echo-area    | **Medium** — works but no inline popup   |
| **Org tables + export**                                 | Heavy use        | :white_check_mark: Works     | None                                     |
| **Org folding + TODO**                                  | Heavy use        | :white_check_mark: Works     | None                                     |
| **Golden ratio** (window auto-sizing)                   | Enabled          | :white_check_mark: Works     | None                                     |
| **Browse kill ring**                                    | Installed        | :white_check_mark: Works     | None                                     |
| **Bracket/paren swap** (`[`↔`(`)                        | Configured       | :white_check_mark: Works     | None — key-translate system              |
| **iedit** (edit occurrences)                            | Installed        | :white_check_mark: Works     | None — M-x iedit-mode                    |
| **expand-region**                                       | Installed        | :white_check_mark: Works     | None — C-= expand, C-- shrink            |
| **Snippets** (yasnippet + file-templates)               | Active           | :large_blue_circle: Substantial | **Low** — 100+ snippets, TAB expand, field nav, file loading |
| **Dired extensions** (dired-k, dired-imenu, etc.)       | Enhanced         | :large_blue_circle: Substantial | **Low** — batch ops, wdired, find-dired work |
| **Gerbil mode + LSP** (custom gerbil-mode.el)           | Custom written   | :large_blue_circle: Built-in | Low — gemacs IS the Gerbil editor        |
| **Flycheck + Flyspell**                                 | Both active      | :large_blue_circle: Both work | **None** — flycheck via LSP, flyspell via aspell |
| **EditorConfig**                                        | Installed        | :white_check_mark: Works     | None — auto-applied on file open         |
| **GitLab issue tracking** (28 custom modes)             | Extensive custom | :red_circle: Missing         | **Low** — very personal workflow         |

### The User's Unique Patterns

1. **Key-chord power user**: 30+ two-key simultaneous bindings for common actions. Gemacs already supports this.

2. **Helm-centric workflow**: Everything goes through Helm — M-x, buffers, files, grep, docs. This is the biggest UX gap; without a narrowing framework, every interactive command feels clunky.

3. **Multi-terminal workflow**: Uses multi-vterm with key-chords (`MT` = new terminal, `LK` = copy mode, `JK` = exit copy mode). Terminal management is core to their workflow.

4. **AI-assisted coding**: Has copilot, gptel, claude-shell, ellama, chatgpt-shell. AI completion and chat are expected features.

5. **DevOps toolchain**: Docker, Terraform, Ansible, AWS — needs syntax highlighting and basic mode support at minimum.

6. **Custom tool builder**: Wrote 28 custom Elisp modes. Will want to write equivalent Gerbil modes — needs a good extension API.

7. **No Paredit**: Despite being a Lisp user, doesn't use paredit/smartparens. Gemacs's paredit is a bonus.

---

## Recommended Development Roadmap

> *Reprioritized based on the user's actual Emacs workflow*

### Phase 1: Core Interaction (Make Gemacs Feel Right)
1. **Completion popup (Corfu equivalent)** — Inline completion overlay at point. The user uses Corfu in Emacs. Without this, code editing is painful.
2. **LSP UI wiring** — Connect the existing LSP transport to: completion (popup), hover (echo area), goto-definition, find-references, diagnostics display. The user has eglot working in Emacs for Gerbil.
3. **Narrowing framework (Helm-like)** — A candidate selection UI for M-x, switch-buffer, find-file, grep results. Doesn't need to be Helm exactly, but needs: live filtering, candidate list, preview. This is the user's primary interaction pattern.

### Phase 2: Development Workflow
4. **Magit interactive status** — The user runs Magit daily. At minimum: status buffer with file-level staging/unstaging, commit composition, diff viewing with hunk navigation.
5. **Multi-terminal** — Multi-vterm equivalent: create/switch/close terminals, copy mode. The user has key-chords for this (MT, LK, JK).
6. **iedit / edit occurrences** — Select symbol, edit all occurrences in buffer. The user has iedit installed.
7. **expand-region** — Incrementally expand selection (word → sexp → defun → buffer). The user has this installed.
8. ~~Dired batch operations~~ — ~~Operate on marked files (delete, rename, copy). Currently single-file only.~~ Done: mark/unmark, batch delete/copy/rename, mark-by-regexp, toggle-marks, wdired.

### Phase 3: Polish & Power Features
9. **AI integration** — Copilot mode, inline suggestions, code explain/refactor all scaffolded. Needs API key configuration for real AI provider connection.
10. **Snippet system** — YASnippet equivalent with tabstop navigation. The user has snippets + file-templates enabled.
11. ~~Flyspell~~ — ~~Background spell checking.~~ Done: `flyspell-mode` with aspell backend, squiggle indicators (TUI), word-list reporting (Qt).
12. ~~Bracket/paren swap~~ — ~~Input-level key remapping.~~ Done: `toggle-bracket-paren-swap` uses key-translate system in both TUI and Qt.
13. ~~DevOps syntax modes~~ — ~~At minimum: Terraform, Ansible, Docker highlighting via Scintilla lexers.~~ Done: Terraform/HCL (`.tf`, `.tfvars`, `.hcl`) via C lexer; Docker/Ansible already covered (bash/YAML).
14. ~~EditorConfig support~~ — ~~Read `.editorconfig` files for indent style/size.~~ Done: auto-applied on `find-file` in Qt, manual `editorconfig-apply` in TUI.

### Phase 4: Ecosystem
15. **Extension API** — A well-documented API for users to write custom modes in Gerbil. The user wrote 28 custom Elisp modes; they'll want to do the same in Gerbil.
16. ~~Interactive ediff~~ — ~~Hunk navigation and merge resolution.~~ Done: smerge-mode with conflict marker resolution.
17. ~~Tab bar / workspaces~~ — ~~Named workspaces (the user has DOOM workspaces enabled).~~ Done: workspace tabs with create/close/switch/rename/move in both layers.
18. **TRAMP-like remote editing** — SSH/Docker file editing.
