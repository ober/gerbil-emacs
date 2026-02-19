# Bug: Mixing `read-line` and `read-subu8vector` on Gambit process ports crashes silently

## Summary

Calling `read-line` (character I/O) followed by `read-subu8vector` (byte I/O) on
the same bidirectional process port raises a
`nonempty-input-port-character-buffer-exception`. When this occurs inside a reader
thread wrapped in `with-catch`, the exception is silently swallowed, making the
thread appear to never run at all.

This bug blocked LSP integration in gemacs for months. The reader thread that
processes `gerbil-lsp` responses appeared "stuck" — all lifecycle tests timed out.
The real cause was that the thread ran, successfully read the response headers via
`read-line`, then crashed on `read-subu8vector` for the body, and the `with-catch`
handler returned `(void)` silently.

## Root Cause

Gambit maintains separate character and byte buffers for port input. `read-line`
reads ahead into the **character buffer** (decoding bytes into characters).
`read-subu8vector` then tries to read from the **byte buffer**, which is empty
because those bytes were already consumed into the character buffer. Gambit raises
`nonempty-input-port-character-buffer-exception` to signal this conflict.

## Affected Pattern

Any code that reads HTTP-style headers (text lines) then a binary body from the
same port — which is exactly the pattern used by LSP's JSON-RPC Content-Length
framed transport:

```
Content-Length: 1842\r\n    <-- read with read-line (char I/O)
\r\n                        <-- read with read-line (char I/O)
{"jsonrpc":"2.0",...}       <-- read with read-subu8vector (byte I/O) --> CRASH
```

## Minimal Reproduction

Save as `bug-repro.ss` and run with `gxi bug-repro.ss`:

```scheme
;;; bug-repro.ss — Demonstrates Gambit char/byte buffer conflict on process ports.
;;; Requires: gerbil-lsp on PATH (or any JSON-RPC server using Content-Length framing).
;;; Run: gxi bug-repro.ss

(import :std/text/json)

;;; ---- Transport helpers (same pattern as lsp-client.ss) ----

(def (write-lsp-message port msg-hash)
  (let* ((body (json-object->string msg-hash))
         (body-bytes (with-output-to-u8vector (lambda () (display body))))
         (content-length (u8vector-length body-bytes))
         (header (string-append "Content-Length: "
                                (number->string content-length) "\r\n\r\n"))
         (header-bytes (with-output-to-u8vector (lambda () (display header)))))
    (write-subu8vector header-bytes 0 (u8vector-length header-bytes) port)
    (write-subu8vector body-bytes 0 content-length port)
    (force-output port)))

(def (read-content-length port)
  "Read HTTP-style headers via read-line. Return Content-Length value."
  (let loop ((cl #f))
    (let ((line (read-line port)))
      (cond
        ((eof-object? line) #f)
        ((or (string=? line "") (string=? line "\r")) cl)
        ((string-prefix? "Content-Length: " line)
         (let* ((val (substring line 16 (string-length line)))
                (clean (if (and (> (string-length val) 0)
                                (char=? (string-ref val (- (string-length val) 1))
                                        #\return))
                         (substring val 0 (- (string-length val) 1))
                         val)))
           (loop (string->number clean))))
        (else (loop cl))))))

;;; ---- The bug: read-subu8vector after read-line ----

(def (read-body-BUGGY port content-length)
  "BUGGY: uses read-subu8vector (byte I/O) after headers were read with read-line
   (char I/O). Raises nonempty-input-port-character-buffer-exception."
  (let* ((buf (make-u8vector content-length 0))
         (n (read-subu8vector buf 0 content-length port)))
    (if (= n content-length)
      (let ((p (open-input-u8vector buf))) (read-line p #f))
      #f)))

;;; ---- The fix: read-string instead ----

(def (read-body-FIXED port content-length)
  "FIXED: uses read-string (char I/O), consistent with read-line for headers.
   Works because LSP JSON is ASCII, so Content-Length in bytes = string length."
  (let ((body (read-string content-length port)))
    (if (and (string? body) (= (string-length body) content-length))
      body
      #f)))

;;; ---- Test harness ----

(def (test-read-body reader-fn label)
  (displayln "--- Testing: " label " ---")
  (let ((proc (open-process
                (list path: "gerbil-lsp"
                      arguments: '("--stdio")
                      directory: "/tmp"
                      stdin-redirection: #t
                      stdout-redirection: #t
                      stderr-redirection: #t))))
    (input-port-timeout-set! proc 10.0)

    ;; Send an LSP initialize request
    (write-lsp-message proc
      (list->hash-table
        (list (cons "jsonrpc" "2.0")
              (cons "id" 1)
              (cons "method" "initialize")
              (cons "params"
                (list->hash-table
                  (list (cons "processId" 1)
                        (cons "rootUri" "file:///tmp")
                        (cons "capabilities" (make-hash-table))))))))

    ;; Read the response headers (char I/O)
    (let ((cl (read-content-length proc)))
      (displayln "  Content-Length: " cl)
      (if (not cl)
        (displayln "  FAIL: no Content-Length header")
        ;; Read the body using the provided reader function
        (with-catch
          (lambda (e)
            (displayln "  EXCEPTION: " e)
            (displayln "  Exception type: "
              (cond
                ((nonempty-input-port-character-buffer-exception? e)
                 "nonempty-input-port-character-buffer-exception")
                (else "other"))))
          (lambda ()
            (let ((body (reader-fn proc cl)))
              (if body
                (let ((json (string->json-object body)))
                  (displayln "  OK: got JSON response (id="
                             (hash-get json "id") ")"))
                (displayln "  FAIL: body read returned #f")))))))

    (with-catch void (lambda () (close-port proc)))))

;;; ---- Run both tests ----

(def (main . args)
  (test-read-body read-body-BUGGY "BUGGY: read-subu8vector after read-line")
  (displayln)
  (test-read-body read-body-FIXED "FIXED: read-string after read-line"))
```

## Expected Output

```
--- Testing: BUGGY: read-subu8vector after read-line ---
  Content-Length: 1842
  EXCEPTION: #<nonempty-input-port-character-buffer-exception #N>
  Exception type: nonempty-input-port-character-buffer-exception

--- Testing: FIXED: read-string after read-line ---
  Content-Length: 1842
  OK: got JSON response (id=1)
```

## Why This Was Hard to Find

The production code (`lsp-client.ss`) had a reader thread with this pattern:

```scheme
(thread-start!
  (make-thread
    (lambda ()
      (with-catch
        (lambda (e) (void))           ;; <-- silently swallows ALL exceptions
        (lambda ()
          (lsp-reader-loop! proc))))
    'lsp-reader))
```

The `(lambda (e) (void))` handler masked the crash entirely. The thread started,
read the headers successfully, crashed on `read-subu8vector`, and exited without
any visible error. From the outside, it appeared that the green thread scheduler
never woke the thread — but in reality the thread ran and died silently.

This led to months of investigation into wrong theories:
- "Gambit's green thread scheduler doesn't wake threads on process pipe I/O"
- "Qt's event dispatcher interferes with Gambit's I/O scheduler"
- "Process pipes need pseudo-terminal mode"

None of these were the issue. The fix was two lines:

```diff
 (def (lsp-read-message port)
   (let ((content-length (lsp-read-headers port)))
     (if content-length
-      (let* ((buf (make-u8vector content-length 0))
-             (n (read-subu8vector buf 0 content-length port)))
-        (if (= n content-length)
-          (let ((body (bytes->string buf)))
-            (with-catch (lambda (e) #f)
-              (lambda () (string->json-object body))))
-          #f))
+      (let ((body (read-string content-length port)))
+        (if (and (string? body) (= (string-length body) content-length))
+          (with-catch (lambda (e) #f)
+            (lambda () (string->json-object body)))
+          #f))
       #f)))
```

## Lessons

1. **Never use `(lambda (e) (void))` in background threads.** Always log exceptions,
   even if you think the thread "should never fail." At minimum:
   ```scheme
   (with-catch
     (lambda (e)
       (displayln "reader thread error: " e)
       (force-output (current-output-port)))
     thunk)
   ```

2. **Don't mix character I/O and byte I/O on the same Gambit port.** If you read
   headers with `read-line`, read the body with `read-string`, not `read-subu8vector`.
   For ASCII content (like JSON), `Content-Length` in bytes equals `string-length`
   in characters.

3. **When a thread appears to never run, check for silent exceptions first.** The
   Gambit green thread scheduler works correctly for process pipe I/O. If `read-line`
   or `read-string` appears to "hang," the thread probably crashed before reaching
   that call.

## Environment

- Gerbil v0.18.1-173
- Gambit v4.9.5 (included in Gerbil)
- Linux 6.17.0-14-generic
- Affects both compiled executables and interpreter (`gxi`)
