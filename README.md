# gerbil-emacs

An Emacs-like terminal text editor written in Gerbil Scheme, using
[Scintilla](https://www.scintilla.org/) for text editing and
termbox for terminal rendering.

## Dependencies

- [Gerbil Scheme](https://cons.io/) (v0.19+)
- [gerbil-scintilla](https://github.com/user/gerbil-scintilla) — built and
  available at `~/mine/gerbil-scintilla/`

## Build

```sh
# Build gerbil-scintilla first
cd ~/mine/gerbil-scintilla && make build

# Build gerbil-emacs
cd ~/mine/gerbil-emacs
make          # compile all modules + executable
make test     # run unit tests (55 checks, 13 test cases)
make install  # install binary to ~/.local/bin/gerbil-emacs
```

Override the install prefix:

```sh
make install PREFIX=/usr/local
```

## Usage

```sh
gerbil-emacs                    # open with *scratch* buffer
gerbil-emacs file1.txt file2.ss # open files
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

### Editing

| Key | Command | Description |
|-----|---------|-------------|
| `C-d` / `Delete` | delete-char | Delete character at point |
| `DEL` / `C-h` | backward-delete-char | Delete character before point |
| `C-k` | kill-line | Kill from point to end of line |
| `C-y` | yank | Yank (paste) last killed text |
| `C-w` | kill-region | Kill region (mark to point) |
| `M-w` | copy-region | Copy region without killing |
| `C-@` | set-mark | Set mark at point |
| `C-_` | undo | Undo last change |
| `C-o` | open-line | Open a new line below |
| `C-m` / `C-j` | newline | Insert newline |

### Search

| Key | Command | Description |
|-----|---------|-------------|
| `C-s` | search-forward | Incremental search forward |
| `C-r` | search-backward | Incremental search backward |

### Files and Buffers

| Key | Command | Description |
|-----|---------|-------------|
| `C-x C-f` | find-file | Open a file |
| `C-x C-s` | save-buffer | Save current buffer |
| `C-x b` | switch-buffer | Switch to a buffer by name |
| `C-x k` | kill-buffer | Kill current buffer |

### Windows

| Key | Command | Description |
|-----|---------|-------------|
| `C-x 2` | split-window | Split window vertically |
| `C-x o` | other-window | Switch to other window |
| `C-x 0` | delete-window | Delete current window |
| `C-x 1` | delete-other-windows | Delete all other windows |

### Other

| Key | Command | Description |
|-----|---------|-------------|
| `C-g` | keyboard-quit | Cancel current operation |
| `C-x C-c` | quit | Exit the editor |

## Architecture

```
gerbil-emacs/
  keymap.ss       Key event translation and keybinding state machine
  buffer.ss       Buffer management with Scintilla document pointers
  window.ss       Frame and window layout (vertical splitting)
  modeline.ss     Status line rendering
  echo.ss         Echo area / minibuffer for messages and prompts
  editor.ss       Editor commands and app state
  app.ss          Main event loop, initialization, event dispatch
  main.ss         Executable entry point
  emacs-test.ss   Unit tests
```

### Module Overview

**keymap.ss** -- Translates terminal key events into Emacs-style key strings
(`C-x`, `M-f`, `<up>`, etc.) and implements a state machine for multi-key
prefix sequences. Keymaps are hash tables mapping key strings to command
symbols or nested sub-keymaps.

**buffer.ss** -- Each buffer owns a reference-counted Scintilla document
pointer. Switching buffers calls `SCI_SETDOCPOINTER` on the editor widget,
which preserves per-document undo history, cursor position, and content.

**window.ss** -- Manages a frame containing one or more vertically-stacked
edit windows. Each window owns a Scintilla editor instance. Supports
splitting, deletion, and resizing.

**modeline.ss** -- Draws the status line at the bottom of each window:
`-UU-:**-  buffer-name    (line,col)` with reversed colors for the
active window.

**echo.ss** -- The bottom row of the terminal. Displays messages, errors
(in red), and handles blocking text input with prompt display for commands
like find-file and switch-buffer.

**editor.ss** -- Defines all editor commands and the `app-state` struct
that ties everything together. Commands are stored in a registry and
dispatched by symbol name from the keymap.

**app.ss** -- Initialization (terminal setup, keybindings, command
registration, buffer creation) and the main event loop (render, present,
poll events, dispatch).

## How It Works

The editor uses Scintilla as its text engine and termbox (bundled inside
gerbil-scintilla) for terminal I/O. The event loop:

1. Processes Scintilla notifications
2. Refreshes all editor windows
3. Draws modelines and echo area
4. Presents to terminal via `tui-present!`
5. Positions the cursor at the caret
6. Polls for input with a 50ms timeout
7. Dispatches key/mouse/resize events through the keymap state machine

Printable characters with no modifier are self-inserted. Unbound key
sequences display an error in the echo area.

## License

MIT
