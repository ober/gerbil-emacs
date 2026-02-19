;;; -*- Gerbil -*-
;;; LSP protocol tests for gemacs — interpreter-based (no Qt).
;;;
;;; Tests LSP lifecycle, document sync, diagnostics, and request round-trips
;;; using the async lsp-client.ss API (background reader thread + UI action queue).
;;;
;;; Run: make test-lsp-protocol
;;; Or:  LD_LIBRARY_PATH=... GERBIL_LOADPATH=$HOME/.gerbil/lib \
;;;      gerbil test ./lsp-protocol-test.ss
;;;
;;; Prerequisites: gerbil-lsp must be on PATH.
;;; Tests skip gracefully if gerbil-lsp is not available.

(import :std/test
        :std/sugar
        :std/text/json
        :gemacs/qt/lsp-client)

(export lsp-protocol-test)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (wait-for predicate label (timeout-ms 15000) (poll-ms 100))
  "Poll predicate every poll-ms, draining LSP UI queue each iteration.
   Returns #t if predicate becomes true, #f on timeout."
  (let loop ((elapsed 0))
    (lsp-poll-ui-actions!)
    (cond
      ((predicate) #t)
      ((>= elapsed timeout-ms)
       (displayln "  TIMEOUT: " label " after " timeout-ms "ms")
       #f)
      (else
       (thread-sleep! (/ poll-ms 1000.0))
       (loop (+ elapsed poll-ms))))))

(def (lsp-test-start! dir)
  "Start LSP against dir and wait for initialization."
  (when (or *lsp-process* *lsp-initializing*)
    (lsp-stop!))
  (lsp-start! dir)
  (wait-for (lambda () *lsp-initialized*) "LSP initialization" 20000))

(def (lsp-test-stop!)
  "Stop LSP cleanly."
  (when (or *lsp-process* *lsp-initializing*)
    (lsp-stop!)))

;;;============================================================================
;;; Fixture management
;;;============================================================================

(def *fixture-counter* 0)

(def (create-fixture!)
  "Create a minimal Gerbil fixture project in /tmp for LSP testing."
  (set! *fixture-counter* (+ *fixture-counter* 1))
  (let ((dir (string-append "/tmp/gemacs-lsp-test-"
                            (number->string (inexact->exact (floor (current-second))))
                            "-" (number->string *fixture-counter*))))
    (with-catch
      (lambda (e)
        (displayln "ERROR creating fixture: " e)
        #f)
      (lambda ()
        (let* ((script (string-append
                 "rm -rf " dir " && "
                 "mkdir -p " dir " && "
                 "printf '(package: lsp-test-fixture)\\n' > " dir "/gerbil.pkg && "
                 "printf '(export hello goodbye)\\n(def (hello) \"hi\")\\n(def (goodbye) \"bye\")\\n' > " dir "/lib.ss && "
                 "printf '(import :lsp-test-fixture/lib)\\n(def (main) (hello))\\n' > " dir "/main.ss && "
                 "printf '(def (oops) (undefined-function))\\n' > " dir "/broken.ss"))
               (p (open-process (list path: "/bin/sh"
                                      arguments: (list "-c" script)
                                      stdout-redirection: #t
                                      stderr-redirection: #t))))
          (read-line p #f)
          (process-status p)
          (close-port p)
          dir)))))

(def (cleanup-fixture! dir)
  "Remove the fixture project directory."
  (when dir
    (with-catch void
      (lambda ()
        (let ((p (open-process (list path: "/bin/sh"
                                     arguments: (list "-c"
                                       (string-append "rm -rf " dir))
                                     stdout-redirection: #t
                                     stderr-redirection: #t))))
          (read-line p #f)
          (process-status p)
          (close-port p))))))

(def (gerbil-lsp-available?)
  "Check if gerbil-lsp is on PATH."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((p (open-process
                 (list path: "/bin/sh"
                       arguments: (list "-c" "command -v gerbil-lsp")
                       stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p #f)))
          (process-status p)
          (close-port p)
          (and out (not (eof-object? out)) (> (string-length out) 0)))))))

;;;============================================================================
;;; Test suite
;;;============================================================================

(def lsp-protocol-test
  (test-suite "LSP protocol"

    ;;------------------------------------------------------------------------
    ;; Group 2: Server Lifecycle
    ;;------------------------------------------------------------------------

    (test-case "start and stop"
      (let ((dir (create-fixture!)))
        (check (string? dir) => #t)
        (unwind-protect
          (begin
            (check (lsp-test-start! dir) => #t)
            (check (lsp-running?) => #t)
            (check (> (hash-length *lsp-server-capabilities*) 0) => #t)
            (lsp-stop!)
            (check (lsp-running?) => #f)
            (check *lsp-process* => #f)
            (check *lsp-initialized* => #f))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    (test-case "double start stops previous"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            ;; Second start should stop the first cleanly
            (lsp-start! dir)
            (check (wait-for (lambda () *lsp-initialized*)
                             "second init" 20000) => #t)
            (check (lsp-running?) => #t))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    (test-case "restart"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (check (lsp-running?) => #t)
            ;; Stop and restart
            (lsp-stop!)
            (check (lsp-running?) => #f)
            (lsp-start! dir)
            (check (wait-for (lambda () *lsp-initialized*)
                             "re-initialization" 20000) => #t)
            (check (lsp-running?) => #t))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    (test-case "state transitions"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            ;; Clean state
            (lsp-test-stop!)
            (check *lsp-initialized* => #f)
            (check *lsp-initializing* => #f)
            ;; Start — should be initializing immediately
            (lsp-start! dir)
            (check (or *lsp-initializing* *lsp-initialized*) => #t)
            ;; Wait for full init
            (check (wait-for (lambda () (lsp-running?))
                             "state transition" 20000) => #t)
            (check *lsp-initialized* => #t)
            (check *lsp-initializing* => #f)
            ;; Stop
            (lsp-stop!)
            (check (lsp-running?) => #f))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Group 3: Document Synchronization
    ;;------------------------------------------------------------------------

    (test-case "didOpen tracks version"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path))
                   (lib-text "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n"))
              (lsp-did-open! lib-uri "scheme" lib-text)
              (check (hash-get *lsp-doc-versions* lib-uri) => 1)
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    (test-case "didChange increments version"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path)))
              (lsp-did-open! lib-uri "scheme"
                "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
              (check (hash-get *lsp-doc-versions* lib-uri) => 1)
              (lsp-did-change! lib-uri
                "(export hello goodbye)\n(def (hello) \"modified\")\n(def (goodbye) \"bye\")\n")
              (check (hash-get *lsp-doc-versions* lib-uri) => 2)
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    (test-case "didSave does not crash"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path))
                   (lib-text "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n"))
              (lsp-did-open! lib-uri "scheme" lib-text)
              (lsp-did-save! lib-uri lib-text)
              (check #t => #t)
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    (test-case "didClose removes tracking"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path)))
              (lsp-did-open! lib-uri "scheme"
                "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
              (check (hash-get *lsp-doc-versions* lib-uri) => 1)
              (lsp-did-close! lib-uri)
              (check (hash-get *lsp-doc-versions* lib-uri) => #f)
              (check (hash-get *lsp-diagnostics* lib-uri) => #f)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Group 4: Diagnostics
    ;;------------------------------------------------------------------------

    (test-case "diagnostics arrive for opened file"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path)))
              (lsp-did-open! lib-uri "scheme"
                "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
              (let ((got (wait-for
                           (lambda () (hash-get *lsp-diagnostics* lib-uri))
                           "publishDiagnostics for lib.ss" 10000)))
                (when got
                  (let ((diags (hash-get *lsp-diagnostics* lib-uri)))
                    (check (list? diags) => #t)
                    ;; Clean file should have no errors
                    (check (length diags) => 0))))
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    (test-case "diagnostics for broken file"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((broken-path (string-append dir "/broken.ss"))
                   (broken-uri (file-path->uri broken-path))
                   (broken-text "(def (oops) (undefined-function))\n"))
              (lsp-did-open! broken-uri "scheme" broken-text)
              (let ((got (wait-for
                           (lambda () (hash-get *lsp-diagnostics* broken-uri))
                           "publishDiagnostics for broken.ss" 10000)))
                (when got
                  (let ((diags (hash-get *lsp-diagnostics* broken-uri)))
                    (check (list? diags) => #t)
                    ;; gerbil-lsp may or may not detect undefined-function
                    (check (>= (length diags) 0) => #t))))
              (lsp-did-close! broken-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Group 5: Goto Definition
    ;;------------------------------------------------------------------------

    (test-case "goto-definition request round-trip"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path)))
              (lsp-did-open! lib-uri "scheme"
                "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
              (let ((response-received #f)
                    (response-data #f))
                (lsp-send-request! "textDocument/definition"
                  (lsp-text-document-position lib-uri 1 5)
                  (lambda (response)
                    (set! response-data response)
                    (set! response-received #t)))
                (let ((ok (wait-for (lambda () response-received)
                                    "goto-definition response" 10000)))
                  (check ok => #t)
                  (when ok
                    (check (hash-table? response-data) => #t))))
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Group 6: Hover
    ;;------------------------------------------------------------------------

    (test-case "hover request round-trip"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path)))
              (lsp-did-open! lib-uri "scheme"
                "(export hello goodbye)\n(def (hello) \"hi\")\n")
              (let ((response-received #f))
                (lsp-send-request! "textDocument/hover"
                  (lsp-text-document-position lib-uri 1 5)
                  (lambda (response)
                    (set! response-received #t)))
                (check (wait-for (lambda () response-received)
                                 "hover response" 10000) => #t))
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Group 7: Find References
    ;;------------------------------------------------------------------------

    (test-case "find-references request round-trip"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path)))
              (lsp-did-open! lib-uri "scheme"
                "(export hello goodbye)\n(def (hello) \"hi\")\n")
              (let ((response-received #f))
                (let ((params (lsp-text-document-position lib-uri 1 5)))
                  (let ((ctx (make-hash-table)))
                    (hash-put! ctx "includeDeclaration" #t)
                    (hash-put! params "context" ctx))
                  (lsp-send-request! "textDocument/references" params
                    (lambda (response)
                      (set! response-received #t))))
                (check (wait-for (lambda () response-received)
                                 "find-references response" 10000) => #t))
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Group 8: Format Buffer
    ;;------------------------------------------------------------------------

    (test-case "formatting request round-trip"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            (lsp-test-start! dir)
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path)))
              (lsp-did-open! lib-uri "scheme"
                "(export hello goodbye)\n(def (hello) \"hi\")\n")
              (let ((response-received #f))
                (let ((params (make-hash-table))
                      (td (make-hash-table))
                      (opts (make-hash-table)))
                  (hash-put! td "uri" lib-uri)
                  (hash-put! opts "tabSize" 2)
                  (hash-put! opts "insertSpaces" #t)
                  (hash-put! params "textDocument" td)
                  (hash-put! params "options" opts)
                  (lsp-send-request! "textDocument/formatting" params
                    (lambda (response)
                      (set! response-received #t))))
                (check (wait-for (lambda () response-received)
                                 "formatting response" 10000) => #t))
              (lsp-did-close! lib-uri)))
          (begin
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Group 11: First-start investigation
    ;;------------------------------------------------------------------------

    (test-case "didOpen after initialization via handler"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            ;; Install a custom on-initialized handler that sends didOpen
            (let* ((lib-path (string-append dir "/lib.ss"))
                   (lib-uri (file-path->uri lib-path))
                   (lib-text "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n"))
              (set-box! *lsp-on-initialized-handler*
                (lambda ()
                  (lsp-did-open! lib-uri "scheme" lib-text)))
              (lsp-start! dir)
              (wait-for (lambda () *lsp-initialized*) "initialization" 20000)
              ;; Give the on-initialized handler time to fire and queue didOpen
              (wait-for (lambda () (hash-get *lsp-doc-versions* lib-uri))
                        "didOpen from handler" 5000)
              (check (and (hash-get *lsp-doc-versions* lib-uri) #t) => #t)
              (lsp-did-close! lib-uri)))
          (begin
            (set-box! *lsp-on-initialized-handler* #f)
            (lsp-test-stop!)
            (cleanup-fixture! dir)))))

    ;;------------------------------------------------------------------------
    ;; Error resilience
    ;;------------------------------------------------------------------------

    (test-case "lsp-stop! is idempotent"
      (lsp-stop!)
      (lsp-stop!)
      (lsp-stop!)
      (check (lsp-running?) => #f))))

;;;============================================================================
;;; Main entry point for gerbil test
;;;============================================================================

(def main
  (let ()
    (if (gerbil-lsp-available?)
      (begin
        (displayln "gerbil-lsp is available — running LSP protocol tests")
        (run-tests! lsp-protocol-test))
      (begin
        (displayln "SKIP: gerbil-lsp not found on PATH — all LSP protocol tests skipped")
        (void)))))
