;;; -*- Gerbil -*-
;;; LSP client protocol layer for gemacs.
;;; Pure I/O module (no Qt widget imports). Handles:
;;; - JSON-RPC transport over stdio (Content-Length framing)
;;; - Background reader thread + mutex-protected UI action queue
;;; - Document synchronization (didOpen/didChange/didSave/didClose)
;;; - Process management (start/stop gerbil-lsp)

(export #t)

(import :std/sugar
        :std/text/json
        :std/misc/string
        (only-in :gemacs/core *lsp-server-command*))

;;;============================================================================
;;; State
;;;============================================================================

;; LSP subprocess (bidirectional port from open-process)
(def *lsp-process* #f)

;; Background thread reading server stdout
(def *lsp-reader-thread* #f)

;; JSON-RPC request ID counter
(def *lsp-request-id* 0)

;; Pending requests: id -> callback thunk (called with response hash on UI thread)
(def *lsp-pending-requests* (make-hash-table))
(def *lsp-pending-mutex* (make-mutex 'lsp-pending))

;; UI action queue (ipc.ss pattern: background thread pushes, UI timer drains)
(def *lsp-ui-queue* [])
(def *lsp-ui-mutex* (make-mutex 'lsp-ui-queue))

;; Document version tracking: uri-string -> integer
(def *lsp-doc-versions* (make-hash-table))

;; Server capabilities from initialize response
(def *lsp-server-capabilities* (make-hash-table))

;; Connection state
(def *lsp-initialized* #f)
(def *lsp-initializing* #f)

;; Diagnostics: uri-string -> list of diagnostic hashes
(def *lsp-diagnostics* (make-hash-table))

;; Workspace root path
(def *lsp-workspace-root* #f)


;;;============================================================================
;;; UI action queue (thread-safe, ipc.ss pattern)
;;;============================================================================

(def (lsp-queue-ui-action! thunk)
  "Push a thunk onto the UI action queue (called from reader thread)."
  (mutex-lock! *lsp-ui-mutex*)
  (unwind-protect
    (set! *lsp-ui-queue* (append *lsp-ui-queue* [thunk]))
    (mutex-unlock! *lsp-ui-mutex*)))

(def (lsp-poll-ui-actions!)
  "Drain the UI action queue and execute thunks. Called from UI timer."
  (let ((actions
         (begin
           (mutex-lock! *lsp-ui-mutex*)
           (unwind-protect
             (let ((q *lsp-ui-queue*))
               (set! *lsp-ui-queue* [])
               q)
             (mutex-unlock! *lsp-ui-mutex*)))))
    (for-each (lambda (thunk)
                (with-catch
                  (lambda (e) (void))  ;; don't crash UI on handler error
                  thunk))
              actions)))

;;;============================================================================
;;; Pending request management (thread-safe)
;;;============================================================================

(def (lsp-store-pending! id callback)
  (mutex-lock! *lsp-pending-mutex*)
  (unwind-protect
    (hash-put! *lsp-pending-requests* id callback)
    (mutex-unlock! *lsp-pending-mutex*)))

(def (lsp-take-pending! id)
  "Remove and return the callback for request id, or #f."
  (mutex-lock! *lsp-pending-mutex*)
  (unwind-protect
    (let ((cb (hash-get *lsp-pending-requests* id)))
      (when cb (hash-remove! *lsp-pending-requests* id))
      cb)
    (mutex-unlock! *lsp-pending-mutex*)))

;;;============================================================================
;;; Transport: Content-Length framed JSON over stdio
;;;============================================================================

(def (lsp-read-message port)
  "Read one LSP message from port. Returns parsed JSON hash, or #f on EOF/error.
   Uses read-string (not read-subu8vector) to avoid Gambit char/byte buffer conflict
   when headers are read with read-line (character I/O)."
  (let ((content-length (lsp-read-headers port)))
    (if content-length
      (let ((body (read-string content-length port)))
        (if (and (string? body) (= (string-length body) content-length))
          (with-catch
            (lambda (e) #f)
            (lambda ()
              (string->json-object body)))
          #f))
      #f)))

(def (lsp-read-headers port)
  "Read HTTP-style headers, return Content-Length value or #f."
  (let loop ((content-length #f))
    (let ((line (read-line port)))
      (cond
        ((eof-object? line) #f)
        ;; Blank line signals end of headers
        ((or (string=? line "") (string=? line "\r"))
         content-length)
        ;; Content-Length header
        ((string-prefix? "Content-Length: " line)
         (let* ((val-str (substring line 16 (string-length line)))
                ;; Strip trailing \r if present
                (clean (if (and (> (string-length val-str) 0)
                                (char=? (string-ref val-str
                                          (- (string-length val-str) 1))
                                        #\return))
                         (substring val-str 0 (- (string-length val-str) 1))
                         val-str))
                (len (string->number clean)))
           (loop (or len content-length))))
        ;; Other headers — skip
        (else (loop content-length))))))

(def (lsp-write-message port msg-hash)
  "Write a JSON-RPC message with Content-Length framing."
  (let* ((body (json-object->string msg-hash))
         (body-bytes (string->bytes body))
         (content-length (u8vector-length body-bytes))
         (header (string-append "Content-Length: "
                                (number->string content-length)
                                "\r\n\r\n"))
         (header-bytes (string->bytes header)))
    (write-subu8vector header-bytes 0 (u8vector-length header-bytes) port)
    (write-subu8vector body-bytes 0 content-length port)
    (force-output port)))

;;;============================================================================
;;; JSON-RPC: send request / notification
;;;============================================================================

(def (lsp-send-request! method params callback)
  "Send a JSON-RPC request. Callback receives the response hash on UI thread."
  (when *lsp-process*
    (set! *lsp-request-id* (+ *lsp-request-id* 1))
    (let ((id *lsp-request-id*)
          (msg (make-hash-table)))
      (hash-put! msg "jsonrpc" "2.0")
      (hash-put! msg "id" id)
      (hash-put! msg "method" method)
      (when params
        (hash-put! msg "params" params))
      (lsp-store-pending! id callback)
      (with-catch
        (lambda (e) (void))
        (lambda () (lsp-write-message *lsp-process* msg)))
      id)))

(def (lsp-send-notification! method params)
  "Send a JSON-RPC notification (no response expected)."
  (when *lsp-process*
    (let ((msg (make-hash-table)))
      (hash-put! msg "jsonrpc" "2.0")
      (hash-put! msg "method" method)
      (when params
        (hash-put! msg "params" params))
      (with-catch
        (lambda (e) (void))
        (lambda () (lsp-write-message *lsp-process* msg))))))

;;;============================================================================
;;; Reader thread: dispatch responses, notifications, server requests
;;;============================================================================

(def (lsp-reader-loop! port)
  "Background thread: read LSP messages and dispatch them."
  (let loop ()
    (let ((msg (with-catch
                 (lambda (e) #f)
                 (lambda () (lsp-read-message port)))))
      (when (and msg (hash-table? msg))
        (let ((id     (hash-get msg "id"))
              (method (hash-get msg "method"))
              (result (hash-get msg "result"))
              (error  (hash-get msg "error")))
          (cond
            ;; Response to our request (has id, no method)
            ((and id (not method))
             (let ((cb (lsp-take-pending! id)))
               (when cb
                 (lsp-queue-ui-action!
                   (lambda () (cb msg))))))
            ;; Server notification (has method, no id)
            ((and method (not id))
             (lsp-handle-server-notification! method
               (hash-get msg "params")))
            ;; Server request (has both id and method)
            ((and id method)
             (lsp-handle-server-request! id method
               (hash-get msg "params")))
            (else (void))))
        (loop)))))

(def (lsp-handle-server-notification! method params)
  "Handle a notification from the server (dispatched on reader thread,
   but queues UI actions for actual processing)."
  (cond
    ((string=? method "textDocument/publishDiagnostics")
     (lsp-queue-ui-action!
       (lambda ()
         (lsp-store-diagnostics! params))))
    ((string=? method "window/logMessage")
     (void))  ;; silently ignore log messages
    ((string=? method "window/showMessage")
     (lsp-queue-ui-action!
       (lambda ()
         (lsp-store-show-message! params))))
    (else (void))))

(def (lsp-handle-server-request! id method params)
  "Handle a request from the server — respond immediately."
  (cond
    ((string=? method "window/workDoneProgress/create")
     ;; Accept the progress token
     (lsp-send-response! id (make-hash-table)))
    ((string=? method "client/registerCapability")
     ;; Accept dynamic registration
     (lsp-send-response! id (make-hash-table)))
    (else
     ;; Unknown server request — respond with empty result
     (lsp-send-response! id (make-hash-table)))))

(def (lsp-send-response! id result)
  "Send a response to a server request."
  (when *lsp-process*
    (let ((msg (make-hash-table)))
      (hash-put! msg "jsonrpc" "2.0")
      (hash-put! msg "id" id)
      (hash-put! msg "result" result)
      (with-catch
        (lambda (e) (void))
        (lambda () (lsp-write-message *lsp-process* msg))))))

;;;============================================================================
;;; Diagnostics storage (called on UI thread via queue)
;;;============================================================================

;; Callback for commands-lsp to install its diagnostics handler
(def *lsp-diagnostics-handler* (box #f))

;; Callback for show-message
(def *lsp-show-message-handler* (box #f))

;; Callback fired once on UI thread when LSP server becomes initialized
(def *lsp-on-initialized-handler* (box #f))

;; Tracks last didChange content sent per URI (deduplication)
(def *lsp-last-sent-content* (make-hash-table))

(def (lsp-store-diagnostics! params)
  "Store diagnostics from publishDiagnostics notification."
  (when (and params (hash-table? params))
    (let ((uri (hash-get params "uri"))
          (diags (hash-get params "diagnostics")))
      (when uri
        (let ((d (if diags diags [])))
          (hash-put! *lsp-diagnostics* uri d)
          ;; Call the UI handler if installed
          (let ((handler (unbox *lsp-diagnostics-handler*)))
            (when handler
              (handler uri d))))))))

(def (lsp-store-show-message! params)
  "Handle window/showMessage notification."
  (let ((handler (unbox *lsp-show-message-handler*)))
    (when (and params handler)
      (handler params))))

(def (lsp-content-changed? uri text)
  "Return #t if text differs from last sent content for uri."
  (not (equal? (hash-get *lsp-last-sent-content* uri) text)))

(def (lsp-record-sent-content! uri text)
  "Record the last content sent via didOpen/didChange for uri."
  (hash-put! *lsp-last-sent-content* uri text))

;;;============================================================================
;;; Process management
;;;============================================================================

(def (lsp-start! workspace-root)
  "Start the LSP server subprocess and begin initialization."
  (when (or *lsp-process* *lsp-initializing*)
    (lsp-stop!))
  (set! *lsp-initializing* #t)
  (set! *lsp-workspace-root* workspace-root)
  (let ((proc (with-catch
                (lambda (e) #f)
                (lambda ()
                  (open-process
                    (list path: *lsp-server-command*
                          arguments: '("--stdio")
                          directory: workspace-root
                          stdin-redirection: #t
                          stdout-redirection: #t
                          stderr-redirection: #f))))))
    (if (not proc)
      (begin
        (set! *lsp-initializing* #f)
        #f)
      (begin
        (set! *lsp-process* proc)
        ;; Start reader thread
        (set! *lsp-reader-thread*
          (thread-start!
            (make-thread
              (lambda ()
                (with-catch
                  (lambda (e) (void))
                  (lambda ()
                    (lsp-reader-loop! proc))))
              'lsp-reader)))
        ;; Send initialize request
        (lsp-send-initialize! workspace-root)
        #t))))

(def (lsp-stop!)
  "Stop the LSP server — send shutdown, then exit, then kill process."
  (when *lsp-process*
    ;; Send shutdown request (don't wait for response)
    (with-catch void
      (lambda ()
        (let ((msg (make-hash-table)))
          (hash-put! msg "jsonrpc" "2.0")
          (hash-put! msg "id" -1)
          (hash-put! msg "method" "shutdown")
          (lsp-write-message *lsp-process* msg))))
    ;; Send exit notification
    (with-catch void
      (lambda ()
        (lsp-send-notification! "exit" #f)))
    ;; Close the process
    (with-catch void
      (lambda () (close-port *lsp-process*)))
    (with-catch void
      (lambda ()
        (when *lsp-process*
          (process-status *lsp-process*)))))
  ;; Reset state
  (set! *lsp-process* #f)
  (set! *lsp-reader-thread* #f)
  (set! *lsp-request-id* 0)
  (set! *lsp-pending-requests* (make-hash-table))
  (set! *lsp-initialized* #f)
  (set! *lsp-initializing* #f)
  (set! *lsp-server-capabilities* (make-hash-table))
  (set! *lsp-diagnostics* (make-hash-table))
  (set! *lsp-doc-versions* (make-hash-table))
  (set! *lsp-last-sent-content* (make-hash-table))
  (set! *lsp-ui-queue* []))

(def (lsp-running?)
  "True if the LSP server is initialized and running."
  (and *lsp-process* *lsp-initialized*))

;;;============================================================================
;;; Initialization handshake
;;;============================================================================

(def (lsp-send-initialize! workspace-root)
  "Send the initialize request with client capabilities."
  (let ((params (make-hash-table))
        (caps (make-hash-table))
        (text-doc (make-hash-table))
        (sync (make-hash-table))
        (completion (make-hash-table))
        (hover (make-hash-table))
        (sig-help (make-hash-table))
        (definition (make-hash-table))
        (references (make-hash-table))
        (doc-symbol (make-hash-table))
        (formatting (make-hash-table))
        (rename (make-hash-table))
        (code-action (make-hash-table))
        (publish-diag (make-hash-table))
        (workspace (make-hash-table))
        (ws-edit (make-hash-table)))
    ;; textDocument capabilities
    (hash-put! sync "dynamicRegistration" #f)
    (hash-put! sync "didSave" #t)
    (hash-put! text-doc "synchronization" sync)
    (hash-put! text-doc "completion" completion)
    (hash-put! text-doc "hover" hover)
    (hash-put! text-doc "signatureHelp" sig-help)
    (hash-put! text-doc "definition" definition)
    (hash-put! text-doc "references" references)
    (hash-put! text-doc "documentSymbol" doc-symbol)
    (hash-put! text-doc "formatting" formatting)
    (hash-put! text-doc "rename" rename)
    (hash-put! text-doc "codeAction" code-action)
    (hash-put! text-doc "publishDiagnostics" publish-diag)
    ;; Semantic tokens
    (let ((sem-tokens (make-hash-table))
          (sem-full (make-hash-table)))
      (hash-put! sem-tokens "dynamicRegistration" #f)
      (hash-put! sem-full "delta" #f)
      (hash-put! sem-tokens "requests" (let ((h (make-hash-table)))
                                         (hash-put! h "full" sem-full)
                                         h))
      (hash-put! sem-tokens "tokenTypes"
        ["namespace" "type" "class" "enum" "interface" "struct"
         "typeParameter" "parameter" "variable" "property" "enumMember"
         "event" "function" "method" "macro" "keyword" "modifier"
         "comment" "string" "number" "regexp" "operator" "decorator"])
      (hash-put! sem-tokens "tokenModifiers"
        ["declaration" "definition" "readonly" "static" "deprecated"
         "abstract" "async" "modification" "documentation" "defaultLibrary"])
      (hash-put! sem-tokens "formats" ["relative"])
      (hash-put! text-doc "semanticTokensProvider" sem-tokens))
    ;; Call hierarchy
    (let ((call-hier (make-hash-table)))
      (hash-put! call-hier "dynamicRegistration" #f)
      (hash-put! text-doc "callHierarchy" call-hier))
    ;; Inlay hints
    (let ((inlay (make-hash-table)))
      (hash-put! inlay "dynamicRegistration" #f)
      (hash-put! text-doc "inlayHint" inlay))
    ;; Type hierarchy
    (let ((type-hier (make-hash-table)))
      (hash-put! type-hier "dynamicRegistration" #f)
      (hash-put! text-doc "typeHierarchy" type-hier))
    (hash-put! caps "textDocument" text-doc)
    ;; workspace capabilities
    (hash-put! ws-edit "documentChanges" #t)
    (hash-put! workspace "workspaceEdit" ws-edit)
    (hash-put! caps "workspace" workspace)
    ;; params
    (hash-put! params "processId" (##os-getpid))
    (hash-put! params "rootUri" (file-path->uri workspace-root))
    (hash-put! params "rootPath" workspace-root)
    (hash-put! params "capabilities" caps)
    (hash-put! params "clientInfo"
      (let ((h (make-hash-table)))
        (hash-put! h "name" "gemacs")
        (hash-put! h "version" "1.0")
        h))
    ;; Send initialize
    (lsp-send-request! "initialize" params
      (lambda (response)
        ;; Store server capabilities
        (let ((result (hash-get response "result")))
          (when (and result (hash-table? result))
            (let ((caps (hash-get result "capabilities")))
              (when (and caps (hash-table? caps))
                (set! *lsp-server-capabilities* caps)))))
        ;; Send initialized notification
        (lsp-send-notification! "initialized" (make-hash-table))
        (set! *lsp-initialized* #t)
        (set! *lsp-initializing* #f)
        ;; Fire post-initialization callback on UI thread (sends didOpen for open buffers)
        (let ((h (unbox *lsp-on-initialized-handler*)))
          (when h (lsp-queue-ui-action! h)))))))


;;;============================================================================
;;; Document sync notifications
;;;============================================================================

(def (lsp-did-open! uri language-id text)
  "Notify server that a document was opened."
  (when (lsp-running?)
    (let ((version 1)
          (params (make-hash-table))
          (td (make-hash-table)))
      (hash-put! *lsp-doc-versions* uri version)
      (hash-put! td "uri" uri)
      (hash-put! td "languageId" language-id)
      (hash-put! td "version" version)
      (hash-put! td "text" text)
      (hash-put! params "textDocument" td)
      (lsp-send-notification! "textDocument/didOpen" params))))

(def (lsp-did-change! uri text)
  "Notify server that a document changed (full content sync)."
  (when (lsp-running?)
    (let* ((version (+ 1 (or (hash-get *lsp-doc-versions* uri) 0)))
           (params (make-hash-table))
           (td-id (make-hash-table))
           (change (make-hash-table)))
      (hash-put! *lsp-doc-versions* uri version)
      (hash-put! td-id "uri" uri)
      (hash-put! td-id "version" version)
      (hash-put! params "textDocument" td-id)
      (hash-put! change "text" text)
      (hash-put! params "contentChanges" [change])
      (lsp-send-notification! "textDocument/didChange" params))))

(def (lsp-did-save! uri text)
  "Notify server that a document was saved."
  (when (lsp-running?)
    (let ((params (make-hash-table))
          (td-id (make-hash-table)))
      (hash-put! td-id "uri" uri)
      (hash-put! params "textDocument" td-id)
      (hash-put! params "text" text)
      (lsp-send-notification! "textDocument/didSave" params))))

(def (lsp-did-close! uri)
  "Notify server that a document was closed."
  (when (lsp-running?)
    (let ((params (make-hash-table))
          (td-id (make-hash-table)))
      (hash-put! td-id "uri" uri)
      (hash-put! params "textDocument" td-id)
      (lsp-send-notification! "textDocument/didClose" params)
      (hash-remove! *lsp-doc-versions* uri)
      (hash-remove! *lsp-diagnostics* uri))))

;;;============================================================================
;;; Helper utilities
;;;============================================================================

(def (file-path->uri path)
  "Convert a file path to a file:// URI."
  (string-append "file://" (path-expand path)))

(def (uri->file-path uri)
  "Convert a file:// URI to a file path."
  (if (string-prefix? "file://" uri)
    (substring uri 7 (string-length uri))
    uri))

(def (lsp-text-document-position uri line col)
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

(def (lsp-language-id path)
  "Map file extension to LSP languageId string."
  (let ((ext (path-extension path)))
    (cond
      ((or (string=? ext ".ss") (string=? ext ".scm")
           (string=? ext ".sld") (string=? ext ".sls"))
       "scheme")
      ((string=? ext ".el") "emacs-lisp")
      ((string=? ext ".py") "python")
      ((string=? ext ".js") "javascript")
      ((string=? ext ".ts") "typescript")
      ((string=? ext ".c") "c")
      ((string=? ext ".h") "c")
      ((string=? ext ".cpp") "cpp")
      ((string=? ext ".rs") "rust")
      ((string=? ext ".go") "go")
      (else "plaintext"))))

(def (bytes->string bv)
  "Convert a u8vector to a string (UTF-8)."
  (let ((port (open-input-u8vector bv)))
    (read-line port #f)))

(def (string->bytes str)
  "Convert a string to a u8vector (UTF-8)."
  (let ((port (open-output-u8vector)))
    (display str port)
    (get-output-u8vector port)))
