# gerbil-emacs

An Emacs-like text editor written in [Gerbil Scheme](https://cons.io/) with
**dual backends** — a terminal UI (TUI) using [Scintilla](https://www.scintilla.org/)
and termbox, and a native GUI using Qt6. Over **1,000 registered commands**
covering the breadth of GNU Emacs functionality.

## Features

- **1,001 commands** — navigation, editing, search, buffers, windows, files,
  version control, programming, org-mode, shells, and more
- **Dual backends** — run in the terminal (`gerbil-emacs`) or as a native
  Qt desktop application (`gerbil-emacs-qt`)
- **Gerbil syntax highlighting** — built-in lexer-based highlighting for
  Gerbil Scheme source files
- **Embedded shells** — eshell (built-in Gerbil shell), external `$SHELL`
  subprocess, and Gerbil REPL (`gxi`)
- **Emacs keybindings** — familiar `C-x`, `C-c`, `M-x` prefix sequences
  with a hash-table keymap state machine
- **Scintilla text engine** — undo/redo history, reference-counted document
  pointers, folding, and multi-cursor support
- **Kill ring, registers, bookmarks, marks** — standard Emacs text management
- **Macro recording** — `C-x (` / `C-x )` / `C-x e` keyboard macros
- **Window management** — splitting, resizing, balancing, tab-bar, winner-mode
- **In-process eval** — `M-:` evaluates Gerbil expressions from the echo area

## Dependencies

- [Gerbil Scheme](https://cons.io/) (v0.19+)
- [gerbil-scintilla](https://github.com/ober/gerbil-scintilla) — Scintilla + termbox FFI bindings (required for TUI backend)
- [gerbil-qt](https://github.com/ober/gerbil-qt) — Qt6 FFI bindings (optional, for Qt backend)

## Build

```sh
# Build gerbil-scintilla first (required)
cd ~/mine/gerbil-scintilla && make build

# Optionally build gerbil-qt for the Qt backend
cd ~/mine/gerbil-qt && make build

# Build gerbil-emacs (both TUI and Qt executables)
cd ~/mine/gerbil-emacs
make          # compile all modules + executables
make test     # run unit tests (55+ checks, 30+ test cases)
make install  # install binaries to ~/.local/bin/
```

Override the install prefix:

```sh
make install PREFIX=/usr/local
```

## Usage

```sh
# Terminal UI
gerbil-emacs                    # open with *scratch* buffer
gerbil-emacs file1.txt file2.ss # open files

# Qt GUI
gerbil-emacs-qt                 # open with *scratch* buffer
gerbil-emacs-qt file1.txt       # open files
```

## Key Bindings

### Navigation

| Key | Command | Description |
|-----|---------|-------------|
| `C-f` / `Right` | forward-char | Move forward one character |
| `C-b` / `Left` | backward-char | Move backward one character |
| `C-n` / `Down` | next-line | Move to next line |
| `C-p` / `Up` | previous-line | Move to previous line |
| `C-a` / `Home` | beginning-of-line | Move to beginning of line |
| `C-e` / `End` | end-of-line | Move to end of line |
| `M-f` | forward-word | Move forward one word |
| `M-b` | backward-word | Move backward one word |
| `M-<` | beginning-of-buffer | Move to beginning of buffer |
| `M->` | end-of-buffer | Move to end of buffer |
| `C-v` / `PgDn` | scroll-down | Scroll down one page |
| `M-v` / `PgUp` | scroll-up | Scroll up one page |
| `C-l` | recenter | Recenter view on cursor |
| `M-g g` | goto-line | Go to a specific line number |

### Editing

| Key | Command | Description |
|-----|---------|-------------|
| `C-d` / `Delete` | delete-char | Delete character at point |
| `DEL` / `C-h` | backward-delete-char | Delete character before point |
| `C-k` | kill-line | Kill from point to end of line |
| `C-y` | yank | Yank (paste) last killed text |
| `M-y` | yank-pop | Cycle through kill ring |
| `C-w` | kill-region | Kill region (mark to point) |
| `M-w` | copy-region | Copy region without killing |
| `C-@` | set-mark | Set mark at point |
| `C-_` | undo | Undo last change |
| `C-x u` | redo | Redo last undone change |
| `C-o` | open-line | Open a new line below |
| `C-m` / `C-j` | newline | Insert newline |
| `M-q` | fill-paragraph | Reflow paragraph |
| `M-;` | comment-dwim | Toggle comment on region/line |
| `C-t` | transpose-chars | Transpose characters |
| `M-t` | transpose-words | Transpose words |
| `M-u` | upcase-word | Uppercase word |
| `M-l` | downcase-word | Lowercase word |
| `M-c` | capitalize-word | Capitalize word |

### Search and Replace

| Key | Command | Description |
|-----|---------|-------------|
| `C-s` | search-forward | Incremental search forward |
| `C-r` | search-backward | Incremental search backward |
| `M-%` | query-replace | Interactive search and replace |
| `M-x occur` | occur | List matching lines |

### Files and Buffers

| Key | Command | Description |
|-----|---------|-------------|
| `C-x C-f` | find-file | Open a file |
| `C-x C-s` | save-buffer | Save current buffer |
| `C-x C-w` | write-file | Save buffer to a new file |
| `C-x b` | switch-buffer | Switch to a buffer by name |
| `C-x k` | kill-buffer | Kill current buffer |
| `C-x C-b` | buffer-list | List all buffers |
| `C-x d` | dired | Directory editor |

### Windows

| Key | Command | Description |
|-----|---------|-------------|
| `C-x 2` | split-window | Split window vertically |
| `C-x 3` | split-window-right | Split window horizontally |
| `C-x o` | other-window | Switch to other window |
| `C-x 0` | delete-window | Delete current window |
| `C-x 1` | delete-other-windows | Delete all other windows |
| `C-x +` | balance-windows | Balance window sizes |

### Help

| Key | Command | Description |
|-----|---------|-------------|
| `C-h k` | describe-key | Describe what a key does |
| `C-h f` | describe-function | Describe a command |
| `C-h b` | describe-bindings | List all key bindings |
| `C-h a` | apropos-command | Search commands by name |
| `C-h w` | where-is | Find key binding for a command |

### Shells and REPL

| Key | Command | Description |
|-----|---------|-------------|
| `C-x r` | repl | Open Gerbil REPL (`gxi` subprocess) |
| `M-x eshell` | eshell | Open built-in Gerbil shell |
| `M-x shell` | shell | Open external `$SHELL` subprocess |
| `M-:` | eval-expression | Evaluate Gerbil expression in echo area |

### Bookmarks and Registers

| Key | Command | Description |
|-----|---------|-------------|
| `C-x r m` | bookmark-set | Set a bookmark |
| `C-x r b` | bookmark-jump | Jump to a bookmark |
| `C-x r s` | copy-to-register | Save region to register |
| `C-x r i` | insert-register | Insert register contents |

### Macros

| Key | Command | Description |
|-----|---------|-------------|
| `C-x (` | start-kbd-macro | Start recording a macro |
| `C-x )` | end-kbd-macro | Stop recording |
| `C-x e` | call-last-kbd-macro | Execute last macro |

### Other

| Key | Command | Description |
|-----|---------|-------------|
| `M-x` | execute-extended-command | Run a command by name |
| `C-g` | keyboard-quit | Cancel current operation |
| `C-x C-c` | quit | Exit the editor |

## Command Categories

The 1,001 commands span these areas:

| Category | Examples |
|----------|----------|
| **Core editing** | kill/yank, undo/redo, transpose, case conversion, fill, indent |
| **Navigation** | goto-line, imenu, avy, bookmarks, mark ring, sexp movement |
| **Search** | isearch, query-replace, occur, grep, rg, deadgrep |
| **Buffers & Windows** | split, resize, balance, tab-bar, winner, ibuffer, windmove |
| **File management** | find-file, save, revert, dired, recent files, auto-save |
| **Programming** | syntax highlighting, paren matching, compile, xref, LSP, DAP, flycheck |
| **Language modes** | Gerbil, Scheme, Emacs Lisp, Python, Ruby, JavaScript, TypeScript, Go, Rust, C/C++, Java, Haskell, OCaml |
| **Version control** | magit (status, log, commit, diff, blame, stash, fetch, push, pull, rebase, merge), git-gutter |
| **Text processing** | sort, align, rectangles, string inflection, multiple cursors, paredit, smartparens |
| **Org-mode** | headings, TODO cycling, scheduling, agenda, export, babel |
| **Shell & REPL** | eshell (built-in), shell (external), Gerbil REPL (gxi), eval-expression |
| **Modern packages** | consult, embark, corfu, cape, vertico, marginalia, orderless, which-key |
| **Writing** | olivetti, writeroom, focus-mode, spell checking (jinx, flyspell) |
| **Utilities** | calc, calendar, EWW browser, EMMS media, PDF tools, markdown, snippets, ediff |
| **Appearance** | themes, rainbow delimiters, hl-todo, indent guides, minimap, doom-modeline |
| **Modal editing** | evil-mode, meow-mode stubs |

## Architecture

```
gerbil-emacs/
  core.ss           Shared state, buffer predicates, eval helpers, default keybindings
  keymap.ss         Key event translation and keymap state machine
  buffer.ss         Buffer management with Scintilla document pointers
  window.ss         Frame and window layout (splitting, resizing)
  modeline.ss       Status line rendering (file, line/col, modified, mode)
  echo.ss           Echo area / minibuffer (messages and blocking prompts)
  highlight.ss      Gerbil syntax highlighting via Scintilla lexers
  editor.ss         527 editor commands + command registry
  editor-extra.ss   474 additional commands (overflow module)
  app.ss            Main event loop, rendering, event dispatch
  main.ss           TUI executable entry point
  repl.ss           Gerbil REPL subprocess management (gxi)
  eshell.ss         Built-in Gerbil shell (pwd, cd, ls, echo, eval, which)
  shell.ss          External $SHELL subprocess terminal
  emacs-test.ss     Unit tests (55+ checks, 30+ test cases)

  qt/
    keymap.ss       Qt key event translation
    buffer.ss       Qt buffer management
    window.ss       Qt window/frame layout
    modeline.ss     Qt status line
    echo.ss         Qt echo area
    commands.ss     Qt editor commands
    app.ss          Qt main event loop
    main.ss         Qt executable entry point
```

### Module Details

**core.ss** — Backend-agnostic shared state: buffer predicates (`repl-buffer?`,
`eshell-buffer?`, `shell-buffer?`), `eval-expression-string` for in-process
Gerbil eval, and default keybinding setup for both backends.

**keymap.ss** — Translates terminal/Qt key events into Emacs-style key strings
(`C-x`, `M-f`, `<up>`, etc.) and implements a state machine for multi-key
prefix sequences. Keymaps are hash tables mapping key strings to command
symbols or nested sub-keymaps.

**buffer.ss** — Each buffer owns a reference-counted Scintilla document
pointer. Switching buffers calls `SCI_SETDOCPOINTER` on the editor widget,
preserving per-document undo history, cursor position, and content. Tracks
file path, modified state, and lexer language.

**window.ss** — Manages a frame containing one or more vertically-stacked
edit windows. Each window owns a Scintilla editor instance. Supports
splitting, deletion, resizing, and balancing.

**modeline.ss** — Draws the status line at the bottom of each window:
`-UU-:**-  buffer-name    (line,col)  Mode` with reversed colors for the
active window.

**echo.ss** — The bottom row of the terminal. Displays messages, errors
(in red), and handles blocking text input with prompt display for commands
like find-file and switch-buffer.

**editor.ss + editor-extra.ss** — Define all 1,001 editor commands and the
`app-state` struct that ties everything together. Commands are stored in a
registry and dispatched by symbol name from the keymap. The split into two
files is for build performance.

**app.ss** — Initialization (terminal setup, keybindings, command registration,
buffer creation) and the main event loop: process notifications, refresh
windows, draw modelines and echo area, present to terminal, poll for input,
dispatch events, poll REPL/shell output.

**repl.ss** — Manages `gxi` subprocesses for the embedded Gerbil REPL.
Handles process lifecycle (start/send/read/stop) and non-blocking output
polling integrated into the event loop.

**eshell.ss** — A built-in shell implemented entirely in Gerbil. Supports
`pwd`, `cd`, `ls`, `echo`, `which`, `eval`, `clear`, `exit`, and falls
through to external commands via `open-process`.

**shell.ss** — Spawns an external `$SHELL` process with pseudo-terminal
emulation, ANSI code stripping, and bidirectional I/O integrated into the
event loop.

## How It Works

The editor uses Scintilla as its text engine and termbox (bundled inside
gerbil-scintilla) for terminal I/O. The event loop:

1. Processes Scintilla notifications
2. Polls REPL, shell, and eshell buffers for subprocess output
3. Refreshes all editor windows
4. Draws modelines and echo area
5. Presents to terminal via `tui-present!`
6. Positions the cursor at the caret
7. Polls for input with a 50ms timeout
8. Dispatches key/mouse/resize events through the keymap state machine

Printable characters with no modifier are self-inserted. Unbound key
sequences display an error in the echo area. `M-x` opens a command
prompt for executing any of the 1,001 registered commands by name.

The Qt backend follows the same architecture but uses Qt widgets for
rendering and Qt's event system for input handling, producing a native
desktop application with the same command set.

## License

MIT
