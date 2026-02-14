# Gerbil-Emacs Implementation Plan

## Phase 1: Fix Python Syntax Highlighting (TUI)

**Problem**: Python files load but show no syntax highlighting in the TUI.
The code is all in place (highlight.ss has complete Python lexer setup), and the
Lexilla Python lexer (`LexPython.o`) is compiled and linked into the binary.

**Root cause investigation**:
1. The highlighting setup runs once in `open-file-in-app!` but the editor has no
   per-buffer style restoration — switching buffers clobbers styles since
   Scintilla style colors are per-editor, not per-document
2. Need to verify `CreateLexer("python")` returns a valid lexer at runtime

**Tasks**:
- [ ] Add `setup-highlighting-for-file!` call after every `buffer-attach!` in
      `cmd-switch-buffer`, `cmd-next-buffer`, `cmd-previous-buffer`, etc.
- [ ] Add more languages to `detect-file-language` and `setup-highlighting-for-file!`
- [ ] Verify highlighting works by testing with the TUI binary
- [ ] Add headless highlighting tests (lexer detection, language mapping)

## Phase 2: Terminal Mode (vterm-like) via gerbil-shell Integration

**Goal**: Real terminal buffer running bash/zsh with PTY support, like Emacs vterm.
Use `~/mine/gerbil-shell` as a dependency for the built-in shell; use PTY-based
$SHELL for `M-x terminal`.

**Architecture**:
- Terminal buffer: full PTY with ANSI processing, not just stripped text
- gerbil-shell integration for `M-x eshell` (replaces current basic eshell)
- Terminal grid buffer (rows × cols of styled cells) for `M-x terminal`

**Tasks**:
- [ ] Add `gsh` as a dependency in gerbil.pkg, build.ss, Makefile
- [ ] Create `terminal.ss` module:
  - PTY spawn via Gambit `open-process` with `pseudo-terminal: #t`
  - ANSI escape sequence parser (cursor movement, colors, erase)
  - Terminal grid buffer (rows × cols of styled cells)
  - Input forwarding (raw key events → PTY)
- [ ] Bind `M-x terminal` / `M-x vterm` command
- [ ] Handle terminal resize (SIGWINCH to child)
- [ ] Map ANSI colors to termbox rendering

## Phase 3: Additional Language Highlighting

Lexilla has 100+ lexers. Add support for the most common languages:

- [ ] JavaScript/TypeScript (.js/.ts/.jsx/.tsx) — SCLEX_CPP with JS keywords
- [ ] JSON — SCLEX_JSON
- [ ] YAML — SCLEX_YAML
- [ ] Markdown (.md) — SCLEX_MARKDOWN
- [ ] Bash/Shell (.sh/.bash/.zsh) — SCLEX_BASH
- [ ] Ruby (.rb) — SCLEX_RUBY
- [ ] Rust (.rs) — SCLEX_RUST
- [ ] Go (.go) — SCLEX_CPP with Go keywords
- [ ] HTML (.html/.htm) — SCLEX_HTML
- [ ] CSS (.css) — SCLEX_CSS
- [ ] Makefile — SCLEX_MAKEFILE
- [ ] SQL (.sql) — SCLEX_SQL
- [ ] Lua (.lua) — SCLEX_LUA
- [ ] Java (.java) — SCLEX_CPP with Java keywords
- [ ] TOML (.toml) — SCLEX_TOML
- [ ] Diff (.diff/.patch) — SCLEX_DIFF

## Phase 4: Critical Emacs Improvements

- [ ] Search highlighting (highlight all matches)
- [ ] Minibuffer tab completion for commands/files/buffers
- [ ] Minibuffer history (M-p / M-n)
- [ ] Auto-save / backup files
- [ ] Detect external file modifications

## Implementation Order

1. **Phase 1**: Fix highlighting (immediate user-facing bug)
2. **Phase 3**: Add more languages (high impact, mechanical)
3. **Phase 2**: Terminal mode (biggest feature)
4. **Phase 4**: Critical improvements (ongoing)

Each feature: implement → test → build → verify binary → commit → push.
