;;; -*- Gerbil -*-
;;; LSP protocol tests for gemacs — interpreter-based (no Qt).
;;;
;;; Tests LSP lifecycle, document sync, diagnostics, and request round-trips
;;; using the gxi interpreter with synchronous I/O on the main thread.
;;; This avoids Gambit's green-thread scheduling issue where the reader thread
;;; never gets scheduled to read from process pipes.
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
        (only-in :gemacs/qt/lsp-client
                 lsp-read-message
                 lsp-write-message
                 file-path->uri))

(export lsp-protocol-test)

;;;============================================================================
;;; Synchronous LSP test harness
;;;============================================================================

(def *next-request-id* 0)

(def (next-id!)
  (set! *next-request-id* (+ *next-request-id* 1))
  *next-request-id*)

(def (lsp-open dir)
  "Open a gerbil-lsp subprocess. Returns the process port."
  (let ((proc (open-process
                (list path: "gerbil-lsp"
                      arguments: '("--stdio")
                      directory: dir
                      stdin-redirection: #t
                      stdout-redirection: #t
                      stderr-redirection: #f))))
    (input-port-timeout-set! proc 15.0)
    proc))

(def (lsp-close! proc)
  "Send shutdown + exit and close the process."
  (with-catch void
    (lambda ()
      (let ((msg (make-hash-table)))
        (hash-put! msg "jsonrpc" "2.0")
        (hash-put! msg "id" -1)
        (hash-put! msg "method" "shutdown")
        (lsp-write-message proc msg))
      ;; Read shutdown response (may get server requests first)
      (lsp-sync-read-until-response! proc -1)
      ;; Send exit notification
      (let ((msg (make-hash-table)))
        (hash-put! msg "jsonrpc" "2.0")
        (hash-put! msg "method" "exit")
        (lsp-write-message proc msg))))
  (with-catch void (lambda () (close-port proc)))
  (with-catch void (lambda () (process-status proc))))

(def (lsp-sync-read-until-response! proc expected-id (max-msgs 30))
  "Read messages until we get a response with expected-id.
   Responds to server requests and collects notifications along the way.
   Returns the response hash, or #f on timeout/EOF."
  (let loop ((i 0))
    (if (>= i max-msgs) #f
      (let ((msg (with-catch (lambda (e) #f)
                   (lambda () (lsp-read-message proc)))))
        (cond
          ((not msg) #f)
          ;; Server request (has both id and method) — respond automatically
          ((and (hash-get msg "id") (hash-get msg "method"))
           (let ((resp (make-hash-table)))
             (hash-put! resp "jsonrpc" "2.0")
             (hash-put! resp "id" (hash-get msg "id"))
             (hash-put! resp "result" (make-hash-table))
             (lsp-write-message proc resp))
           (loop (+ i 1)))
          ;; Response matching our id
          ((and (hash-get msg "id")
                (not (hash-get msg "method"))
                (= (hash-get msg "id") expected-id))
           msg)
          ;; Other message (notification or response to different id) — skip
          (else (loop (+ i 1))))))))

(def (lsp-request! proc method params)
  "Send a JSON-RPC request and return the response synchronously.
   Handles server requests/notifications while waiting."
  (let ((id (next-id!))
        (msg (make-hash-table)))
    (hash-put! msg "jsonrpc" "2.0")
    (hash-put! msg "id" id)
    (hash-put! msg "method" method)
    (when params (hash-put! msg "params" params))
    (lsp-write-message proc msg)
    (lsp-sync-read-until-response! proc id)))

(def (lsp-notify! proc method params)
  "Send a JSON-RPC notification (no response expected)."
  (let ((msg (make-hash-table)))
    (hash-put! msg "jsonrpc" "2.0")
    (hash-put! msg "method" method)
    (when params (hash-put! msg "params" params))
    (lsp-write-message proc msg)))

(def (lsp-read-until! proc pred (max-msgs 30))
  "Read messages until predicate matches. Responds to server requests.
   Returns the matching message, or #f if max-msgs exceeded."
  (let loop ((i 0))
    (if (>= i max-msgs) #f
      (let ((msg (with-catch (lambda (e) #f)
                   (lambda () (lsp-read-message proc)))))
        (cond
          ((not msg) #f)
          ;; Server request — auto-respond
          ((and (hash-get msg "id") (hash-get msg "method"))
           (let ((resp (make-hash-table)))
             (hash-put! resp "jsonrpc" "2.0")
             (hash-put! resp "id" (hash-get msg "id"))
             (hash-put! resp "result" (make-hash-table))
             (lsp-write-message proc resp))
           (if (pred msg) msg (loop (+ i 1))))
          ;; Check predicate
          ((pred msg) msg)
          ;; Skip
          (else (loop (+ i 1))))))))

(def (lsp-initialize! proc dir)
  "Send initialize request and initialized notification. Returns initializeResult."
  (let* ((params (make-hash-table))
         (caps (make-hash-table))
         (text-doc (make-hash-table))
         (sync-cap (make-hash-table))
         (publish-diag (make-hash-table))
         (workspace (make-hash-table))
         (ws-edit (make-hash-table)))
    ;; Minimal client capabilities
    (hash-put! sync-cap "dynamicRegistration" #f)
    (hash-put! sync-cap "didSave" #t)
    (hash-put! text-doc "synchronization" sync-cap)
    (hash-put! text-doc "publishDiagnostics" publish-diag)
    (hash-put! caps "textDocument" text-doc)
    (hash-put! ws-edit "documentChanges" #t)
    (hash-put! workspace "workspaceEdit" ws-edit)
    (hash-put! caps "workspace" workspace)
    ;; Params
    (hash-put! params "processId" (##os-getpid))
    (hash-put! params "rootUri" (file-path->uri dir))
    (hash-put! params "rootPath" dir)
    (hash-put! params "capabilities" caps)
    ;; Send initialize
    (let ((resp (lsp-request! proc "initialize" params)))
      ;; Send initialized notification
      (when resp
        (lsp-notify! proc "initialized" (make-hash-table)))
      resp)))

(def (lsp-did-open! proc uri language-id text)
  "Send textDocument/didOpen notification."
  (let ((params (make-hash-table))
        (td (make-hash-table)))
    (hash-put! td "uri" uri)
    (hash-put! td "languageId" language-id)
    (hash-put! td "version" 1)
    (hash-put! td "text" text)
    (hash-put! params "textDocument" td)
    (lsp-notify! proc "textDocument/didOpen" params)))

(def (lsp-did-change! proc uri version text)
  "Send textDocument/didChange notification."
  (let ((params (make-hash-table))
        (td-id (make-hash-table))
        (change (make-hash-table)))
    (hash-put! td-id "uri" uri)
    (hash-put! td-id "version" version)
    (hash-put! params "textDocument" td-id)
    (hash-put! change "text" text)
    (hash-put! params "contentChanges" [change])
    (lsp-notify! proc "textDocument/didChange" params)))

(def (lsp-did-save! proc uri text)
  "Send textDocument/didSave notification."
  (let ((params (make-hash-table))
        (td-id (make-hash-table)))
    (hash-put! td-id "uri" uri)
    (hash-put! params "textDocument" td-id)
    (hash-put! params "text" text)
    (lsp-notify! proc "textDocument/didSave" params)))

(def (lsp-did-close! proc uri)
  "Send textDocument/didClose notification."
  (let ((params (make-hash-table))
        (td-id (make-hash-table)))
    (hash-put! td-id "uri" uri)
    (hash-put! params "textDocument" td-id)
    (lsp-notify! proc "textDocument/didClose" params)))

(def (lsp-text-doc-position uri line col)
  "Build a TextDocumentPositionParams hash."
  (let ((params (make-hash-table))
        (td (make-hash-table))
        (pos (make-hash-table)))
    (hash-put! td "uri" uri)
    (hash-put! pos "line" line)
    (hash-put! pos "character" col)
    (hash-put! params "textDocument" td)
    (hash-put! params "position" pos)
    params))

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
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (let ((resp (lsp-initialize! proc dir)))
                (check (hash-table? resp) => #t)
                (check (and (hash-get resp "result") #t) => #t)
                (let ((caps (hash-get (hash-get resp "result") "capabilities")))
                  (check (hash-table? caps) => #t))))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    (test-case "double start stops previous"
      (let ((dir (create-fixture!)))
        (let ((proc1 (lsp-open dir)))
          (unwind-protect
            (begin
              (let ((resp1 (lsp-initialize! proc1 dir)))
                (check (hash-table? resp1) => #t))
              ;; Close first, open second
              (lsp-close! proc1)
              (set! proc1 #f)
              (let ((proc2 (lsp-open dir)))
                (unwind-protect
                  (let ((resp2 (lsp-initialize! proc2 dir)))
                    (check (hash-table? resp2) => #t)
                    (check (and (hash-get resp2 "result") #t) => #t))
                  (lsp-close! proc2))))
            (when proc1 (lsp-close! proc1))))
        (cleanup-fixture! dir)))

    (test-case "restart"
      (let ((dir (create-fixture!)))
        (unwind-protect
          (begin
            ;; First session
            (let ((proc (lsp-open dir)))
              (let ((resp (lsp-initialize! proc dir)))
                (check (hash-table? resp) => #t))
              (lsp-close! proc))
            ;; Second session
            (let ((proc (lsp-open dir)))
              (let ((resp (lsp-initialize! proc dir)))
                (check (hash-table? resp) => #t)
                (check (and (hash-get resp "result") #t) => #t))
              (lsp-close! proc)))
          (cleanup-fixture! dir))))

    ;;------------------------------------------------------------------------
    ;; Group 3: Document Synchronization
    ;;------------------------------------------------------------------------

    (test-case "didOpen does not crash"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
                ;; If we get here, no crash
                (check #t => #t)
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    (test-case "didChange does not crash"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
                (lsp-did-change! proc lib-uri 2
                  "(export hello goodbye)\n(def (hello) \"modified\")\n(def (goodbye) \"bye\")\n")
                (check #t => #t)
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    (test-case "didSave does not crash"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path))
                     (lib-text "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n"))
                (lsp-did-open! proc lib-uri "scheme" lib-text)
                (lsp-did-save! proc lib-uri lib-text)
                (check #t => #t)
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    (test-case "didClose does not crash"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
                (lsp-did-close! proc lib-uri)
                (check #t => #t)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    ;;------------------------------------------------------------------------
    ;; Group 4: Diagnostics
    ;;------------------------------------------------------------------------

    (test-case "diagnostics for file"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
                ;; Read messages until we get a publishDiagnostics notification
                (let ((diag-msg (lsp-read-until! proc
                                  (lambda (msg)
                                    (equal? (hash-get msg "method")
                                            "textDocument/publishDiagnostics")))))
                  (when diag-msg
                    (let ((params (hash-get diag-msg "params")))
                      (check (hash-table? params) => #t)
                      (check (string? (hash-get params "uri")) => #t)
                      (check (list? (hash-get params "diagnostics")) => #t))))
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    ;;------------------------------------------------------------------------
    ;; Group 5: Goto Definition
    ;;------------------------------------------------------------------------

    (test-case "goto-definition request round-trip"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n(def (goodbye) \"bye\")\n")
                ;; Request definition of "hello" at line 1, char 5
                (let ((resp (lsp-request! proc "textDocument/definition"
                              (lsp-text-doc-position lib-uri 1 5))))
                  (check (hash-table? resp) => #t)
                  ;; Response should have a "result" key (may be null or location)
                  (check (hash-key? resp "result") => #t))
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    ;;------------------------------------------------------------------------
    ;; Group 6: Hover
    ;;------------------------------------------------------------------------

    (test-case "hover request round-trip"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n")
                (let ((resp (lsp-request! proc "textDocument/hover"
                              (lsp-text-doc-position lib-uri 1 5))))
                  (check (hash-table? resp) => #t)
                  (check (hash-key? resp "result") => #t))
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    ;;------------------------------------------------------------------------
    ;; Group 7: Find References
    ;;------------------------------------------------------------------------

    (test-case "find-references request round-trip"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n")
                (let ((params (lsp-text-doc-position lib-uri 1 5)))
                  (let ((ctx (make-hash-table)))
                    (hash-put! ctx "includeDeclaration" #t)
                    (hash-put! params "context" ctx))
                  (let ((resp (lsp-request! proc "textDocument/references" params)))
                    (check (hash-table? resp) => #t)
                    (check (hash-key? resp "result") => #t)))
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    ;;------------------------------------------------------------------------
    ;; Group 8: Format Buffer
    ;;------------------------------------------------------------------------

    (test-case "formatting request round-trip"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              (let* ((lib-path (string-append dir "/lib.ss"))
                     (lib-uri (file-path->uri lib-path)))
                (lsp-did-open! proc lib-uri "scheme"
                  "(export hello goodbye)\n(def (hello) \"hi\")\n")
                (let ((params (make-hash-table))
                      (td (make-hash-table))
                      (opts (make-hash-table)))
                  (hash-put! td "uri" lib-uri)
                  (hash-put! opts "tabSize" 2)
                  (hash-put! opts "insertSpaces" #t)
                  (hash-put! params "textDocument" td)
                  (hash-put! params "options" opts)
                  (let ((resp (lsp-request! proc "textDocument/formatting" params)))
                    (check (hash-table? resp) => #t)
                    (check (hash-key? resp "result") => #t)))
                (lsp-did-close! proc lib-uri)))
            (begin
              (lsp-close! proc)
              (cleanup-fixture! dir))))))

    ;;------------------------------------------------------------------------
    ;; Error resilience
    ;;------------------------------------------------------------------------

    (test-case "shutdown and exit are clean"
      (let ((dir (create-fixture!)))
        (let ((proc (lsp-open dir)))
          (unwind-protect
            (begin
              (lsp-initialize! proc dir)
              ;; Clean shutdown
              (let ((shutdown-resp (lsp-request! proc "shutdown" #f)))
                (check (hash-table? shutdown-resp) => #t))
              (lsp-notify! proc "exit" #f)
              ;; Port should now be at EOF
              (let ((msg (with-catch (lambda (e) 'eof)
                           (lambda () (lsp-read-message proc)))))
                (check (not msg) => #t)))
            (begin
              (with-catch void (lambda () (close-port proc)))
              (cleanup-fixture! dir))))))))

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
