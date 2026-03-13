# Plan: `--repl` TCP Debug Server for gemacs/gemacs-qt

## Goal

Add a `--repl <port>` CLI option to both `gemacs` and `gemacs-qt` that starts a
TCP REPL server on the given port. This allows Claude (or any tool with TCP
access) to connect to a **running** gemacs instance — even when it's hung — and
interrogate Gambit/Gerbil internals: threads, backtraces, buffer state, etc.

## Why This Works Even When Hung

Gambit's green thread scheduler is preemptive at the Scheme level. A hung
gemacs typically means one thread is stuck (infinite loop, deadlocked mutex,
blocking FFI call), but the Gambit scheduler still runs other green threads.
A TCP REPL server running in its own green thread will remain responsive as
long as:

1. The Gambit scheduler itself isn't wedged (rare — only happens in C-level
   deadlocks or SMP processor stalls)
2. The REPL thread doesn't try to acquire a mutex held by the hung thread

For the common case (Scheme-level hang, blocked I/O, Qt event loop stall),
the REPL thread will be fully functional.

## Architecture

```
┌─────────────────────────────────────────┐
│  gemacs / gemacs-qt process             │
│                                         │
│  ┌─────────┐  ┌────────┐  ┌──────────┐ │
│  │ UI/Edit  │  │ Master │  │ LSP      │ │
│  │ thread   │  │ Timer  │  │ thread   │ │
│  └─────────┘  └────────┘  └──────────┘ │
│                                         │
│  ┌──────────────────────────────────┐   │
│  │ REPL TCP Server (green thread)   │   │
│  │  - open-tcp-server on port N     │   │
│  │  - one handler thread per client │   │
│  │  - full eval access              │   │
│  │  - introspection commands        │   │
│  └──────────────────────────────────┘   │
│                                         │
└─────────────────────────────────────────┘
          ▲
          │ TCP :port
          ▼
   Claude / netcat / telnet
```

## Design Decisions

### Transport: TCP (plain)

- Gambit has built-in `open-tcp-server` — zero dependencies
- Claude can connect via MCP tools (`gerbil_eval` with `open-tcp-client`) or
  via shell (`nc localhost <port>`)
- No TLS needed — binds to `127.0.0.1` only (localhost)
- Alternative considered: Unix domain socket — rejected because `open-tcp-server`
  is simpler and Claude tools work better with TCP

### Protocol: Line-oriented S-expression REPL

Each line from the client is read as a Scheme expression, evaluated, and the
result is written back followed by a newline. Errors are caught and reported
as text. A few special commands (prefixed with `,`) provide structured
introspection without requiring the user to remember Gambit internals.

```
gemacs-dbg> (+ 1 2)
3
gemacs-dbg> ,threads
#  name=primordial  state=running
#  name=master-timer  state=waiting
#  name=lsp-reader  state=waiting
gemacs-dbg> ,bt master-timer
[0] thread-sleep!
[1] master-timer-tick!
[2] (loop)
gemacs-dbg> ,buffers
*scratch*  [modified]  (no file)
main.ss    /home/user/project/main.ss
gemacs-dbg> ,state
app-state: frame=#<qt-frame ...>  current-buffer=*scratch*
gemacs-dbg> (buffer-list)
(#<buffer *scratch*> #<buffer main.ss>)
gemacs-dbg> ,quit
Connection closed.
```

### Security: Localhost-Only + Optional Auth Token

- Binds to `127.0.0.1` only — no remote access
- Optional `--repl-token <token>` flag; if set, the first line from each
  client must be the token or the connection is dropped
- The port file (`~/.gemacs-repl-port`) is written with mode `0600`
- **No** `system`, `shell-command`, or `open-process` in the REPL sandbox
  (the evaluator runs in a restricted environment that omits dangerous
  primitives — but since this is a debug tool for localhost, the restriction
  is advisory)

## Implementation Plan

### Step 1: New Module `debug-repl.ss`

Create `/home/jafourni/mine/gerbil-emacs/debug-repl.ss`:

```scheme
(export start-debug-repl! stop-debug-repl! debug-repl-port)

(import :std/sugar
        :std/format
        :std/misc/threads
        :std/srfi/13       ;; string-trim, string-prefix?
        :gemacs/core)
```

**Key definitions:**

#### `(start-debug-repl! port-num [token: #f])`

1. Call `open-tcp-server` on `127.0.0.1:port-num`
2. Save the server port in `*debug-repl-server*`
3. Write `~/.gemacs-repl-port` with the actual port number (supports port 0
   for auto-assign)
4. Spawn an accept loop thread (`debug-repl-accept`) that calls `read` on
   the server to accept connections
5. For each connection, spawn a handler thread (`debug-repl-client-N`)

#### `(debug-repl-handle-client! client token)`

1. If `token` is set, read first line and verify; close on mismatch
2. Display banner: `"gemacs debug REPL — type ,help for commands\n"`
3. Loop:
   - Display prompt `"gemacs-dbg> "`
   - Read a line
   - If EOF → close
   - If starts with `,` → dispatch to command handler
   - Otherwise → `read` as S-expression, `eval`, `write` result
   - All wrapped in `with-exception-catcher` so errors don't kill the session

#### Comma Commands

| Command | Description |
|---------|-------------|
| `,help` | List available commands |
| `,threads` | List all threads with name and state |
| `,bt <name>` | Show backtrace for a named thread |
| `,bt-all` | Show backtrace for all threads |
| `,buffers` | List all open buffers with file paths |
| `,state` | Show app-state summary (current buffer, frame, mark, etc.) |
| `,vars` | Show key global variables (`*buffer-list*`, `*kill-ring*`, etc.) |
| `,locks` | Show mutex state for known mutexes |
| `,timers` | Show periodic task schedule |
| `,eval-in <thread-name> <expr>` | Inject eval into a specific thread via `thread-async!` |
| `,gc` | Force GC and report heap stats |
| `,quit` | Close the REPL connection |
| `,shutdown` | Gracefully quit gemacs (emergency escape) |

#### Thread Introspection Implementation

```scheme
(def (list-all-threads port)
  "Print all threads with name, state, and group."
  (for-each
    (lambda (t)
      (let ((name (thread-name t))
            (st (thread-state t)))
        (fprintf port "  ~a  name=~a  state=~a\n"
                 t
                 (or name "(unnamed)")
                 (cond
                   ((thread-state-running? st) "running")
                   ((thread-state-waiting? st) "waiting")
                   ((thread-state-normally-terminated? st)
                    (format "terminated(~a)" (thread-state-normally-terminated-result st)))
                   ((thread-state-abnormally-terminated? st)
                    (format "aborted(~a)" (thread-state-abnormally-terminated-reason st)))
                   (else "unknown")))))
    (all-threads)))

(def (show-thread-backtrace name port)
  "Find thread by name and display its continuation backtrace."
  (let ((t (find (lambda (t) (equal? (thread-name t) name))
                 (all-threads))))
    (if t
      (let ((st (##thread-state t)))
        (##display-continuation-backtrace st port #t #t 50 0))
      (fprintf port "No thread named ~a\n" name))))
```

#### `(stop-debug-repl!)`

1. Close `*debug-repl-server*`
2. Delete `~/.gemacs-repl-port`
3. Terminate all client handler threads

#### `(debug-repl-port)`

Returns the actual port number the server is listening on (useful when port 0
was requested).

### Step 2: Wire Into `main.ss` (TUI)

```scheme
;; In the arg parsing cond:
((member "--repl" args)
 => (lambda (rest)
      (let ((port-num (if (and (pair? (cdr rest))
                               (string->number (cadr rest)))
                        (string->number (cadr rest))
                        4242)))
        (let ((app (app-init! (remove-repl-args args))))
          (start-debug-repl! port-num)
          (try
            (app-run! app)
            (finally
              (stop-debug-repl!)
              ...existing cleanup...))))))
```

Also support `GEMACS_REPL_PORT` environment variable as an alternative to the
CLI flag (easier for always-on usage).

### Step 3: Wire Into `qt/main.ss` (Qt)

Same pattern — parse `--repl <port>` from args, call `start-debug-repl!`
before `qt-app-exec!`, call `stop-debug-repl!` in cleanup.

In `qt/app.ss :: qt-do-init!`, after the IPC server start:

```scheme
;; Start debug REPL if requested
(when *debug-repl-requested-port*
  (start-debug-repl! *debug-repl-requested-port*
                     token: *debug-repl-token*))
```

### Step 4: Port File Discovery

Write `~/.gemacs-repl-port` containing:
```
PORT=4242
PID=12345
```

Claude can discover the port by reading this file, then connecting via:
```scheme
(def client (open-tcp-client "127.0.0.1:4242"))
```

### Step 5: Claude Integration Helpers

Add a convenience function for Claude's `gerbil_eval` tool to connect:

```scheme
;; Claude can run this via gerbil_eval:
(begin
  (def (gemacs-repl-query expr-string)
    (let ((port-info (call-with-input-file
                       (path-expand "~/.gemacs-repl-port")
                       read-line)))
      ;; Parse port number
      (let* ((port-num (string->number
                         (cadr (string-split port-info #\=))))
             (client (open-tcp-client
                       (list server-address: "127.0.0.1"
                             port-number: port-num))))
        ;; Skip banner
        (read-line client)
        ;; Skip prompt
        (read-line client)
        ;; Send expression
        (display expr-string client)
        (newline client)
        (force-output client)
        ;; Read result
        (let ((result (read-line client)))
          (close-port client)
          result)))))
```

### Step 6: Add to `build.ss`

Add `debug-repl` to the module compilation list, between `ipc` and `async`
(since it imports `:gemacs/core` and `:std/misc/threads`).

### Step 7: Tests

Create `debug-repl-test.ss`:

1. **Server starts and accepts connections** — start server on port 0, read
   port file, connect, verify banner
2. **Eval works** — send `(+ 1 2)`, verify `3` response
3. **Comma commands** — test `,threads`, `,buffers`, `,help`
4. **Error handling** — send invalid expression, verify error message (not crash)
5. **Token auth** — start with token, verify rejected without it
6. **Multiple clients** — connect two clients simultaneously
7. **Clean shutdown** — `stop-debug-repl!` closes connections and removes port file

## Open Questions / Future Work

1. **JSON protocol option**: For tighter Claude integration, consider a
   `,json` mode that wraps responses in JSON objects. This would make parsing
   trivial for Claude tools. Could be a follow-up.

2. **MCP server bridge**: A future enhancement could expose the debug REPL as
   an MCP server tool, so Claude can call `gemacs_eval`, `gemacs_threads`,
   `gemacs_backtrace` etc. directly without TCP socket management.

3. **Multi-line input**: The initial implementation is line-oriented. For
   multi-line expressions, the client can use `\` continuation or send
   everything on one line. A follow-up could add bracket-aware multi-line
   reading.

4. **Read-only mode**: A `--repl-readonly` flag could restrict the REPL to
   introspection only (no `set!`, no mutation), useful for production
   monitoring.

5. **Automatic startup**: Consider always starting the debug REPL (on an
   ephemeral port) and just writing the port file. This way Claude can always
   connect without the user needing to remember `--repl`. The port file acts
   as the discovery mechanism.

## File Changes Summary

| File | Change |
|------|--------|
| `debug-repl.ss` | **NEW** — TCP REPL server module |
| `main.ss` | Parse `--repl` arg, start/stop debug REPL |
| `qt/main.ss` | Parse `--repl` arg, pass to `qt-main` |
| `qt/app.ss` | Start debug REPL in `qt-do-init!` |
| `build.ss` | Add `debug-repl` module |
| `debug-repl-test.ss` | **NEW** — test suite |
| `Makefile` | Add `test-repl` target |

## Estimated Complexity

- `debug-repl.ss`: ~200-250 lines
- CLI wiring: ~20 lines across 3 files
- Tests: ~150 lines
- Total: ~400 lines of new code
