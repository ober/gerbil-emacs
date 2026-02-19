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

## Build

```bash
make build    # Build TUI and Qt binaries
make test     # Build + run all tests
make test-qt  # Build + run Qt headless tests
make test-all # Build + run all tests (TUI + Qt)
```

Individual test file:
```bash
HOME=/home/user LD_LIBRARY_PATH=/home/linuxbrew/.linuxbrew/opt/openssl@3/lib \
  GERBIL_LOADPATH=/home/user/.gerbil/lib gxi functional-test.ss
```

## File size limit

All source files must stay below 2000 lines. If a file approaches this limit, split it following the existing `commands-*.ss` chain pattern.
