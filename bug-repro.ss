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
