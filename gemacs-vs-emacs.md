# Gemacs vs GNU Emacs — Feature Comparison

> **Last updated:** 2026-02-22
> **Gemacs version:** master (c39a615)
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
| Isearch word mode             | :red_circle:       | Not implemented                            |
| Isearch symbol mode           | :red_circle:       | Not implemented                            |
| Search highlight all matches  | :yellow_circle:    | Basic — no lazy highlight during isearch   |

**Summary:** Search is strong. Isearch, query-replace, occur, grep, and wgrep all work well. Missing some isearch sub-modes.

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
| Undo tree visualization | :red_circle: | Not implemented — linear only |
| Persistent undo (across sessions) | :red_circle: | Not implemented |
| Selective undo (region) | :red_circle: | Not implemented |

**Summary:** Basic undo/redo works well but is linear. No tree visualization or branching history like `undo-tree` or `vundo`.

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
| Named macros | :red_circle: | Only last-recorded macro |
| Macro counter | :red_circle: | No `C-x C-k C-i` counter |
| Edit macro | :red_circle: | No `C-x C-k C-e` editor |
| Save macros to file | :red_circle: | No persistence |
| Execute with count prefix | :yellow_circle: | Basic repeat support |

**Summary:** Basic macro recording/playback works. Missing named macros, editing, counters, and persistence.

---

## 10. Minibuffer & Completion

| Feature | Status | Notes |
|---------|--------|-------|
| M-x (execute-extended-command) | :white_check_mark: | Fuzzy matching command names |
| File name completion | :white_check_mark: | Tab completion in find-file |
| Buffer name completion | :white_check_mark: | Fuzzy matching in switch-buffer |
| Minibuffer history | :white_check_mark: | `M-p` / `M-n` in minibuffer |
| Recursive minibuffer | :red_circle: | Not supported |
| Vertico / Selectrum | :red_circle: | No vertical completion UI |
| Orderless matching | :yellow_circle: | Basic fuzzy; no space-separated orderless |
| Marginalia (annotations) | :red_circle: | No rich annotations |
| Embark (actions on candidates) | :red_circle: | Not implemented |
| Consult (enhanced commands) | :yellow_circle: | `consult-grep`, `consult-line` registered but basic |
| Icomplete / Fido mode | :red_circle: | Not implemented |
| Savehist (persist history) | :large_blue_circle: | `~/.gemacs-history` |

**Summary:** Basic minibuffer works with fuzzy completion. Modern completion frameworks (Vertico, Orderless, Marginalia, Embark) are absent. This is one of the biggest gaps for modern Emacs workflows.

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
| Dedicated windows | :red_circle: | Not implemented |
| Side windows | :red_circle: | No `display-buffer-in-side-window` |
| Window purpose | :red_circle: | Not implemented |
| Follow mode | :white_check_mark: | Synchronized scrolling across windows |

**Summary:** Window management is strong. Splitting, resizing, winner-mode, and ace-window all work. Missing display-buffer customization.

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
| Tab bar | :yellow_circle: | Registered but minimal |
| Header line | :red_circle: | Not implemented |
| Fringe indicators | :yellow_circle: | Line numbers; no bitmap fringes |
| Display tables | :red_circle: | Not implemented |

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
| Batch delete (flagged) | :red_circle: | No `D` flag + `x` execute workflow |
| Batch rename (marked) | :red_circle: | No multi-file rename |
| Batch copy (marked) | :red_circle: | No multi-file copy |
| Shell command on file | :red_circle: | Not implemented |
| Wdired (edit filenames) | :red_circle: | Not implemented |
| Image thumbnails | :red_circle: | Not implemented |
| Dired-x extensions | :yellow_circle: | Minimal |
| Async operations | :red_circle: | All synchronous |
| Virtual dired | :red_circle: | Not implemented |
| Dired subtree | :red_circle: | Not implemented |

**Summary:** Dired handles basic single-file operations and listing. Missing batch operations on marked files, wdired, and shell command integration. The gap is significant for power users.

---

## 16. Version Control / Magit

| Feature | Status | Notes |
|---------|--------|-------|
| Git status display | :orange_circle: | Shows `git status --short` output |
| Stage / unstage hunks | :red_circle: | **Critical gap** — no interactive staging |
| Stage / unstage files | :red_circle: | No interactive staging |
| Commit with message | :yellow_circle: | Opens editor, basic flow |
| Amend commit | :yellow_circle: | Basic |
| Push / pull | :yellow_circle: | Shell passthrough |
| Log viewing | :yellow_circle: | `git log --oneline` last 50 |
| Diff viewing | :yellow_circle: | Plain `git diff` output |
| Branch operations | :yellow_circle: | Create, checkout, delete via prompts |
| Tag management | :yellow_circle: | Create tags |
| Stash | :yellow_circle: | Basic stash commands |
| Blame | :yellow_circle: | `git blame` output display |
| Interactive rebase | :red_circle: | Not implemented |
| Merge UI | :red_circle: | Not implemented |
| Cherry-pick | :yellow_circle: | Basic |
| Revert commit | :yellow_circle: | Basic |
| Forge (PR/issue management) | :red_circle: | Not implemented |
| Diff-hl (gutter marks) | :large_blue_circle: | Git diff gutter indicators |
| Wgrep on grep results | :white_check_mark: | Edit and save back |
| VC generic backend | :yellow_circle: | Basic git-only |

**Summary:** This is the **biggest gap** for daily use. Magit in Emacs is a killer app — gemacs has only basic git command wrappers. No interactive staging, no hunk selection, no diff navigation, no commit composition buffer. Most operations are shell passthrough.

### Priority Improvements Needed:
1. Interactive status buffer with staging/unstaging (file and hunk level)
2. Commit composition buffer with diff preview
3. Interactive log with commit details
4. Diff buffer with hunk navigation
5. Branch/merge/rebase UI

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
| Agenda interactive commands | :red_circle: | No `a/t/d` keys in agenda buffer |
| **Capture** | :yellow_circle: | Templates with `%?/%U/%T/%f` placeholders |
| Capture buffer (C-c C-c / C-c C-k) | :red_circle: | No interactive capture buffer |
| Refile | :red_circle: | Not implemented |
| **Babel** (code blocks) | :large_blue_circle: | 8 languages, execution, tangling |
| Babel session persistence | :red_circle: | Sessions stored but not reused |
| Babel :var evaluation | :red_circle: | Only literal values |
| Babel :noweb expansion | :red_circle: | Not implemented |
| **Export** | :large_blue_circle: | HTML, Markdown, LaTeX, ASCII |
| Export footnotes/cross-refs | :red_circle: | Not implemented |
| Custom export backends | :red_circle: | Not implemented |
| **Clock tracking** | :large_blue_circle: | Clock-in/out, goto |
| Org-crypt | :yellow_circle: | Registered |
| Heading promote/demote | :white_check_mark: | |
| Move subtree up/down | :white_check_mark: | |
| Template expansion (`<s TAB`) | :white_check_mark: | Source block templates |
| Sparse tree | :red_circle: | Not implemented |
| Column view | :red_circle: | Not implemented |

**Summary:** Org-mode is one of gemacs's strongest features with substantial coverage of the core: headings, TODO, tables, babel, export, agenda. Key gaps: interactive agenda commands, capture buffer UI, refile, sparse tree.

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
| Server lifecycle | :white_check_mark: | Start/stop/restart |
| Document sync | :white_check_mark: | didOpen/didChange/didSave/didClose |
| Diagnostics display | :large_blue_circle: | Collected and stored |
| Completion | :red_circle: | Backend exists but no UI integration |
| Hover (tooltip) | :red_circle: | Not connected to UI |
| Go-to-definition | :red_circle: | Not connected to UI |
| Find references | :red_circle: | Not connected to UI |
| Rename refactoring | :red_circle: | Not connected to UI |
| Code actions | :red_circle: | Not connected to UI |
| Formatting | :yellow_circle: | Registered |
| Semantic tokens | :red_circle: | Not implemented |
| Call hierarchy | :red_circle: | Not implemented |
| Type hierarchy | :red_circle: | Not implemented |
| Inlay hints | :red_circle: | Not implemented |
| Workspace symbols | :yellow_circle: | Registered |
| Multi-server support | :red_circle: | Single server per project |

**Summary:** The LSP transport layer is solid (JSON-RPC, background thread, document sync). However, **none of the user-facing features actually work** — completion, hover, go-to-definition, references, rename, and code actions are all registered as commands but not wired to the LSP backend. This is a critical gap.

### Priority Improvements Needed:
1. Wire completion to LSP completionProvider
2. Wire go-to-definition to LSP definitionProvider
3. Wire hover to display in echo area
4. Wire diagnostics to flycheck-style inline display
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
| Rainbow delimiters | :yellow_circle: | Registered |

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
| Company mode | :red_circle: | **Stub** — toggle flag only, no popup UI |
| Corfu mode | :red_circle: | **Stub** — toggle flag only |
| Cape (completion extensions) | :red_circle: | Not implemented |
| File path completion | :white_check_mark: | In minibuffer |
| Symbol completion | :yellow_circle: | Basic — no ranked/contextual |
| LSP completion | :red_circle: | Backend exists but no frontend |
| Snippet completion | :red_circle: | No YASnippet integration |
| Copilot/AI completion | :yellow_circle: | Registered but not connected |

**Summary:** Only dabbrev and basic completion-at-point work. **No popup completion UI** (company/corfu equivalent). This is a major gap for programming workflows.

### Priority Improvements Needed:
1. Implement a completion popup (company/corfu equivalent)
2. Wire to LSP completionProvider
3. Add file-path and symbol backends

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
| Process sentinels/filters | :red_circle: | Not implemented |

**Summary:** Shell command execution works well. Terminal mode provides PTY with ANSI support. Eshell is basic. Missing vterm and full process management.

---

## 23. Spell Checking

| Feature | Status | Notes |
|---------|--------|-------|
| Ispell word | :large_blue_circle: | Check word at point with suggestions |
| Ispell region | :large_blue_circle: | Scan region for misspellings |
| Ispell buffer | :large_blue_circle: | Whole-buffer check |
| Suggestion menu | :white_check_mark: | Interactive selection from ispell output |
| Flyspell (on-the-fly) | :red_circle: | Not implemented — no background checking |
| Personal dictionary | :large_blue_circle: | Supported via ispell |
| Language selection | :red_circle: | Not implemented |
| Aspell/Hunspell backend | :large_blue_circle: | Uses ispell subprocess |

**Summary:** Interactive spell-checking works via ispell. Missing flyspell for real-time as-you-type checking.

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
| Backward slurp/barf | :red_circle: | Not implemented |
| Convolute sexp | :red_circle: | Not implemented |
| Paredit strict mode | :red_circle: | No delimiter balance enforcement |
| Smartparens | :red_circle: | Not implemented |

**Summary:** Solid paredit implementation for forward operations. Missing backward slurp/barf and strict mode.

---

## 26. Diff & Ediff

| Feature | Status | Notes |
|---------|--------|-------|
| Diff buffer vs file | :white_check_mark: | `C-c d` |
| Unified diff display | :large_blue_circle: | |
| Ediff two buffers | :orange_circle: | Side-by-side display |
| Ediff three-way merge | :red_circle: | Not implemented |
| Navigate hunks (n/p) | :red_circle: | No hunk navigation |
| Accept change (a/b) | :red_circle: | No interactive merge |
| Refine hunks | :red_circle: | No word-level diff |
| Ediff regions | :red_circle: | Not implemented |
| Emerge (merge tool) | :red_circle: | Not implemented |
| Smerge mode | :red_circle: | No Git conflict resolution |

**Summary:** Basic diff display works. **No interactive merge capabilities.** This is a significant gap for development workflows.

### Priority Improvements Needed:
1. Hunk navigation in diff buffers
2. Interactive accept/reject per hunk
3. Smerge mode for Git conflict markers

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
| Per-project settings | :red_circle: | No `.dir-locals.el` equivalent per-project |
| Project-specific keymaps | :red_circle: | Not implemented |
| Projectile integration | :yellow_circle: | Basic command registration |
| project.el features | :yellow_circle: | Basic |

**Summary:** Basic project management works (detection, search, compile). Missing per-project configuration.

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
| Info reader | :red_circle: | Not implemented |
| Emacs tutorial | :red_circle: | Not implemented |
| Built-in documentation browser | :red_circle: | No hyperlinked docs |

**Summary:** Key-based help works well. Missing the Info reader and comprehensive documentation browser.

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
| M-x customize UI | :red_circle: | No interactive customization buffer |
| Custom variables (defcustom) | :red_circle: | No typed, documented variables |
| Custom groups | :red_circle: | Not implemented |
| Face customization UI | :red_circle: | No interactive face editor |
| Mode-specific hooks | :yellow_circle: | Limited hook system |

**Summary:** Configuration via init file works. Missing the entire `M-x customize` interactive customization system.

---

## 30. Package Management & Extensibility

| Feature | Status | Notes |
|---------|--------|-------|
| Elisp scripting | :red_circle: | **Gerbil Scheme instead** — not compatible with Emacs packages |
| package.el / MELPA | :red_circle: | N/A — different language |
| use-package | :red_circle: | N/A |
| straight.el | :red_circle: | N/A |
| Plugin/package system | :red_circle: | No plugin architecture |
| User-defined commands | :large_blue_circle: | Via `~/.gemacs-init` Gerbil code |
| Advice system | :red_circle: | No function advice |
| Hook system | :yellow_circle: | Limited — some mode hooks |
| Autoload system | :red_circle: | Not implemented |
| Dynamic module loading | :red_circle: | Not implemented |

**Summary:** This is a **fundamental architectural difference**. Gemacs uses Gerbil Scheme, not Emacs Lisp, so the entire Emacs package ecosystem (MELPA, ~5,000+ packages) is unavailable. Extensibility exists via Gerbil init file but there's no package manager or plugin system. This is by design — gemacs is a reimplementation, not a compatibility layer.

---

## 31. Remote Editing (TRAMP)

| Feature | Status | Notes |
|---------|--------|-------|
| SSH file editing | :red_circle: | Not implemented |
| TRAMP sudo | :red_circle: | Not implemented (but `sudo-write` exists) |
| Docker container editing | :red_circle: | Not implemented |
| Remote shell | :red_circle: | Not implemented |
| Remote compilation | :red_circle: | Not implemented |

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
| Bookmarks | :red_circle: | Not implemented |

**Summary:** Basic text-mode web browsing. Fetches pages and strips HTML. Not usable for modern web pages.

---

## 33. Calendar & Diary

| Feature | Status | Notes |
|---------|--------|-------|
| Calendar display | :orange_circle: | Monthly grid, navigation |
| Today highlighting | :white_check_mark: | |
| Navigate months/years | :white_check_mark: | |
| Diary integration | :red_circle: | No diary entries |
| Holiday display | :red_circle: | Not implemented |
| Appointment reminders | :red_circle: | Not implemented |
| Org-agenda integration | :red_circle: | Calendar doesn't link to org |

**Summary:** Calendar is a display-only monthly grid. No diary, holidays, or org integration.

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
| File tree sidebar | :yellow_circle: | Registered but minimal |
| Project tree | :red_circle: | Not implemented |
| Git status in tree | :red_circle: | Not implemented |
| File operations in tree | :red_circle: | Not implemented |

**Summary:** No functional file tree sidebar.

---

## 38. Multiple Cursors / iedit

| Feature | Status | Notes |
|---------|--------|-------|
| Multiple cursors | :red_circle: | Not implemented |
| iedit (edit all occurrences) | :red_circle: | Not implemented |
| Symbol highlighting + edit | :red_circle: | `highlight-symbol` highlights but can't edit |

**Summary:** No multi-cursor editing. This is a commonly expected feature in modern editors.

---

## 39. Snippets (YASnippet)

| Feature | Status | Notes |
|---------|--------|-------|
| Snippet expansion | :yellow_circle: | Org-mode `<s TAB` works; no general snippet system |
| Snippet library | :red_circle: | No snippet collections |
| Snippet creation | :red_circle: | Not implemented |
| Tabstop navigation | :red_circle: | Not implemented |
| Mirror fields | :red_circle: | Not implemented |

**Summary:** No general snippet system. Org template expansion (`<s TAB` etc.) exists as a special case.

---

## 40. Tab Bar & Workspaces

| Feature | Status | Notes |
|---------|--------|-------|
| Tab bar mode | :yellow_circle: | Registered |
| Create/close tabs | :red_circle: | Not functional |
| Switch tabs | :red_circle: | Not functional |
| Tab-line (per-window) | :red_circle: | Not implemented |
| Workspace/perspective | :yellow_circle: | Session save/restore provides basic workspace support |
| Named workspaces | :red_circle: | Not implemented |

**Summary:** No functional tab bar. Session management provides basic workspace persistence.

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
| **Magit / interactive Git** | Can't stage hunks, compose commits, or navigate diffs interactively | Large |
| **Completion popup (Company/Corfu)** | No inline completion UI for code | Medium |
| **LSP UI wiring** | LSP transport works but no user-facing features (completion, hover, goto-def) | Medium |
| **Dired batch operations** | Can't operate on marked files | Medium |

### Tier 2 — Expected by Power Users

| Gap | Impact | Effort |
|-----|--------|--------|
| **Modern completion (Vertico/Orderless)** | No vertical minibuffer completion, no space-separated filtering | Medium |
| **Multiple cursors / iedit** | Can't edit multiple occurrences simultaneously | Medium |
| **Snippet system (YASnippet)** | No template expansion with tabstops | Medium |
| **Ediff / Smerge** | Can't resolve merge conflicts interactively | Medium |
| **Flyspell (on-the-fly spell)** | No background spell checking | Small |
| **Undo tree** | Linear undo only, no branch visualization | Medium |
| **Interactive agenda** | Org agenda displays but can't act on items | Medium |

### Tier 3 — Nice to Have

| Gap | Impact | Effort |
|-----|--------|--------|
| **TRAMP (remote editing)** | Can't edit files over SSH/Docker | Large |
| **Tab bar / workspaces** | No visual workspace management | Medium |
| **Tree-sitter highlighting** | Less accurate highlighting than modern Emacs | Large |
| **Package/plugin system** | Users can't extend gemacs easily | Large |
| **Org capture buffer** | No interactive capture with C-c C-c | Small |
| **Named keyboard macros** | Only last-recorded macro available | Small |
| **Info reader** | Can't browse GNU documentation | Medium |
| **PDF viewing** | No document viewer | Large |

### Tier 4 — Emacs-Specific (Low Priority)

| Gap | Impact | Effort |
|-----|--------|--------|
| Email (Gnus/mu4e) | Most users use dedicated email clients | Very Large |
| IRC (ERC) | Most users use dedicated chat clients | Large |
| M-x customize UI | Programmatic config is fine for power users | Medium |
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
| Copilot (code completion) | :red_circle: | Toggle registered, not connected |
| GPTel / LLM chat | :red_circle: | Not implemented |
| Claude shell / chat | :red_circle: | Not implemented |
| Inline AI suggestions | :red_circle: | Not implemented |
| Code explanation / refactor via AI | :red_circle: | Not implemented |

**Summary:** No AI integration. The user's Emacs config includes copilot, gptel, claude-shell, ellama, and chatgpt-shell — AI-assisted coding is part of their workflow.

---

## 44. Multi-Terminal (vterm)

| Feature | Status | Notes |
|---------|--------|-------|
| Vterm (libvterm) | :red_circle: | Not implemented |
| Multi-vterm (multiple terminals) | :red_circle: | Not implemented |
| Vterm copy mode | :red_circle: | Not implemented |
| Terminal per-project | :red_circle: | Not implemented |
| Term / ansi-term | :large_blue_circle: | Basic PTY terminal exists |

**Summary:** The user uses multi-vterm extensively with key-chord shortcuts for copy mode. Gemacs has basic terminal support but no vterm or multi-terminal management.

---

## 45. Key Input Remapping

| Feature | Status | Notes |
|---------|--------|-------|
| Key-chord bindings | :white_check_mark: | Two-key simultaneous chords |
| Key translation table | :white_check_mark: | Character remapping |
| Swap brackets/parens | :red_circle: | User swaps `[`↔`(` and `]`↔`)` — not yet configurable |
| Super/Hyper key mapping | :red_circle: | No super-key-as-meta mapping |
| Per-mode keymaps | :yellow_circle: | Limited |
| Global key remap (input-decode-map) | :red_circle: | No input-level key translation |

**Summary:** Key-chord system works well. Missing input-level remapping (bracket swap, super-to-meta).

---

## 46. DevOps / Infrastructure Modes

| Feature | Status | Notes |
|---------|--------|-------|
| Docker mode | :red_circle: | Not implemented |
| Terraform mode | :red_circle: | Not implemented |
| Ansible mode | :red_circle: | Not implemented |
| Systemd unit files | :red_circle: | Not implemented |
| YAML mode | :large_blue_circle: | Syntax highlighting via Scintilla |
| Kubernetes / k8s | :red_circle: | Not implemented |
| SSH management | :red_circle: | Not implemented |

**Summary:** No DevOps-specific modes. The user has extensive Ansible, Terraform, Docker, and AWS tooling in their Emacs config.

---

## 47. Helm / Narrowing Framework

| Feature                   | Status       | Notes                                        |
|---------------------------|--------------|----------------------------------------------|
| Helm (or equivalent)      | :red_circle: | No narrowing framework                       |
| Helm M-x                  | :red_circle: | M-x has fuzzy matching but no candidate list |
| Helm buffers              | :red_circle: | Buffer switch has fuzzy but no live preview  |
| Helm find-files           | :red_circle: | No incremental file browser                  |
| Helm occur                | :red_circle: | Occur exists but not helm-style              |
| Helm dash (documentation) | :red_circle: | Not implemented                              |
| Helm C-yasnippet          | :red_circle: | Not implemented                              |

**Summary:** The user's primary completion framework is Helm. Gemacs has no equivalent narrowing/selection framework. This fundamentally changes the feel of M-x, buffer switching, file finding, and every interactive command.

---

## Personal Workflow Gap Analysis

> *Based on review of the user's Doom Emacs configuration at `~/mine/emacs/`*

### What the User Actually Uses Daily

| Feature                                                 | Emacs Status     | Gemacs Status                | Gap Severity                             |
|---------------------------------------------------------|------------------|------------------------------|------------------------------------------|
| **Key-chords** (30+ bindings: AS, ZX, BV, FG, KB, etc.) | Extensive        | :white_check_mark: Works     | None — key-chord system exists           |
| **Helm** (M-x, buffers, files, grep)                    | Primary UI       | :red_circle: Missing         | **Critical** — core interaction paradigm |
| **Magit + Forge** (staging, commit, PR review)          | Daily driver     | :orange_circle: Minimal      | **Critical**                             |
| **Multi-vterm** (multiple terminals, copy mode)         | Heavy use        | :red_circle: Missing         | **High**                                 |
| **Eglot / LSP** (completion, hover, goto-def, refs)     | Working          | :white_check_mark: Works     | None — full UI wiring with keybindings   |
| **Copilot / AI** (gptel, claude-shell, copilot)         | Active           | :red_circle: Missing         | **High**                                 |
| **Corfu** (completion-at-point popup)                   | Active           | :yellow_circle: Echo-area    | **Medium** — works but no inline popup   |
| **Org tables + export**                                 | Heavy use        | :white_check_mark: Works     | None                                     |
| **Org folding + TODO**                                  | Heavy use        | :white_check_mark: Works     | None                                     |
| **Golden ratio** (window auto-sizing)                   | Enabled          | :white_check_mark: Works     | None                                     |
| **Browse kill ring**                                    | Installed        | :white_check_mark: Works     | None                                     |
| **Bracket/paren swap** (`[`↔`(`)                        | Configured       | :white_check_mark: Works     | None — key-translate system              |
| **iedit** (edit occurrences)                            | Installed        | :white_check_mark: Works     | None — M-x iedit-mode                    |
| **expand-region**                                       | Installed        | :white_check_mark: Works     | None — C-= expand, C-- shrink            |
| **Snippets** (yasnippet + file-templates)               | Active           | :red_circle: Missing         | **Medium**                               |
| **Dired extensions** (dired-k, dired-imenu, etc.)       | Enhanced         | :yellow_circle: Basic        | **Medium**                               |
| **Gerbil mode + LSP** (custom gerbil-mode.el)           | Custom written   | :large_blue_circle: Built-in | Low — gemacs IS the Gerbil editor        |
| **Flycheck + Flyspell**                                 | Both active      | :yellow_circle: Flycheck OK  | **Low** — flycheck via LSP, no flyspell  |
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
8. **Dired batch operations** — Operate on marked files (delete, rename, copy). Currently single-file only.

### Phase 3: Polish & Power Features
9. **AI integration** — At minimum: Copilot-like inline suggestions or a chat buffer for LLM interaction. The user has 6 AI packages installed.
10. **Snippet system** — YASnippet equivalent with tabstop navigation. The user has snippets + file-templates enabled.
11. **Flyspell** — Background spell checking. User has flyspell enabled.
12. **Bracket/paren swap** — Input-level key remapping to swap `[`↔`(`. User has this configured.
13. **DevOps syntax modes** — At minimum: Terraform, Ansible, Docker highlighting via Scintilla lexers.
14. **EditorConfig support** — Read `.editorconfig` files for indent style/size.

### Phase 4: Ecosystem
15. **Extension API** — A well-documented API for users to write custom modes in Gerbil. The user wrote 28 custom Elisp modes; they'll want to do the same in Gerbil.
16. **Interactive ediff** — Hunk navigation and merge resolution.
17. **Tab bar / workspaces** — Named workspaces (the user has DOOM workspaces enabled).
18. **TRAMP-like remote editing** — SSH/Docker file editing.
