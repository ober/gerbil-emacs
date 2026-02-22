# Gemacs Project — Claude Instructions

## Testing Policy: Dispatch-Level Tests Required

When modifying any `cmd-*` function or dispatch path in this codebase, you **MUST** add or update a functional test in `functional-test.ss` (TUI) or `qt-functional-test.ss` (Qt) that exercises the change through the real dispatch chain.

### Required: Test the dispatch chain, NOT the leaf function

**WRONG** (tests the leaf, bypasses dispatch):
```scheme
(cmd-org-template-expand app)
```

**RIGHT** (tests through cmd-indent-or-complete → dispatch → leaf):
```scheme
(execute-command! app 'indent-or-complete)
```

**RIGHT** (tests through keymap → execute-command! → leaf):
```scheme
(sim-key! app tab-ev)
```

### Why this matters

Org-mode `<s TAB` has regressed 5 times. Each time, the leaf-function tests in `emacs-test.ss` passed but users were broken — because dispatch logic broke, not the leaf. The functional tests in `functional-test.ss` test the full chain from keymap lookup through command execution.

### Rules

1. **TAB behavior**: Test via `(execute-command! app 'indent-or-complete)`, not `(cmd-org-template-expand app)` or `(cmd-org-cycle app)`.
2. **Named commands**: Test via `(execute-command! app 'command-name)`, not by calling `cmd-*` functions directly.
3. **Key sequences**: Test via `(sim-key! app ev)` for single keys, or multiple `sim-key!` calls for multi-key sequences.
4. **Command registration**: Always call `(register-all-commands!)` and `(setup-default-bindings!)` at the start of functional tests.

## Build Verification: MANDATORY Before Commit

**You MUST verify ALL builds and run tests BEFORE committing and pushing.** Never commit code that hasn't been verified to build and pass tests. Follow this checklist in order:

1. **`make build`** — must complete without errors
2. **Verify binaries**: `.gerbil/bin/gemacs --version` and `QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version`
3. **`make test`** — all TUI tests must pass
4. **`make static-qt`** — static Qt Docker build must complete and produce a working binary
5. **Verify static binary**: `QT_QPA_PLATFORM=offscreen .gerbil/bin/gemacs-qt --version`

If any step fails, **fix it before committing**. Do not push broken builds.

### Build commands

```bash
make build      # Build TUI and Qt binaries (dynamic)
make test       # Build + run all TUI tests
make test-qt    # Build + run Qt headless tests
make test-all   # Build + run all tests (TUI + Qt)
make static-qt  # Static Qt binary via Docker (requires docker-deps image)
```

Individual test file:
```bash
HOME=/home/user LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/opt/openssl@3/lib \
  GERBIL_LOADPATH=/home/user/.gerbil/lib gxi functional-test.ss
```

## Feature Parity: "As is in Qt, it is in TUI!"

Every command implemented in the Qt layer (`qt/commands-*.ss`) **MUST** have a corresponding implementation in the TUI layer (`editor-*.ss`), and vice versa. When adding a new `cmd-*` function to one layer, always check the other layer and add the equivalent there too. Both layers should expose the same set of user-facing commands via `M-x`.

## Feature Comparison: MANDATORY After Major Features

After implementing any major new feature or completing a significant milestone, you **MUST** regenerate `gemacs-vs-emacs.md` to reflect the current state of the project. This file is the canonical feature comparison between gemacs and GNU Emacs.

To update it:
1. Review the feature area(s) affected by recent changes
2. Update the status symbols and notes in the relevant sections
3. Update the "Last updated" date and commit hash at the top
4. Update the Critical Feature Gap Summary if gaps have been closed
5. Update the Recommended Development Roadmap if priorities have shifted

This file should always accurately reflect what gemacs can and cannot do today.

## NO Evil Mode

Gemacs is an Emacs-keybinding editor. There is **NO** Evil mode (Vim emulation) and there never will be. Do not add Evil mode commands, variables, keymaps, or any Vim-style modal editing. If you encounter any remaining Evil mode references in the codebase, remove them.

## File size limit

All source files must stay below 2000 lines. If a file approaches this limit, split it following the existing `commands-*.ss` chain pattern.
