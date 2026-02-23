;;; -*- Gerbil -*-
;;; Qt LSP commands — diagnostics, goto-definition, hover, completion,
;;; rename, references, symbols, formatting, code actions, and keybindings.
;;; Part of the qt/commands-*.ss module chain (between vcs and shell).

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/json
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/terminal
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/lsp-client
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-vcs)

;;;============================================================================
;;; Scintilla indicator IDs for LSP diagnostics
;;;============================================================================

(def *indic-lsp-error*   12)
(def *indic-lsp-warning* 13)
(def *indic-lsp-info*    14)
(def *indic-lsp-hint*    15)
(def *indic-lsp-highlight* 16)

;; Scintilla indicator style constants
(def INDIC_SQUIGGLE 1)
(def INDIC_DOTS     4)
(def INDIC_SQUIGGLEPIXMAP 16)
(def INDIC_FULLBOX  16)
(def INDIC_ROUNDBOX  7)

;;;============================================================================
;;; Margin marker IDs for diagnostic gutter icons
;;;============================================================================

;; Marker numbers 0-7 are used by folding. We use 8-11 for diagnostics.
(def *marker-lsp-error*   8)
(def *marker-lsp-warning* 9)
(def *marker-lsp-info*    10)
(def *marker-lsp-hint*    11)

;; Scintilla message constants for markers
(def SCI_MARKERDEFINE    2040)
(def SCI_MARKERSETFORE   2041)
(def SCI_MARKERSETBACK   2042)
(def SCI_MARKERADD       2043)
(def SCI_MARKERDELETEALL 2045)
(def SCI_SETMARGINWIDTHN 2242)
(def SCI_SETMARGINTYPEN  2240)
(def SCI_SETMARGINMASKN  2244)
;; SC_MARK_CIRCLE = 0, SC_MARK_LEFTRECT = 27
(def SC_MARK_CIRCLE      0)
(def SC_MARK_LEFTRECT    27)
(def SC_MARGIN_SYMBOL    0)

;;;============================================================================
;;; Auto-start
;;;============================================================================

(def (lsp-maybe-auto-start! app buf)
  "Auto-start LSP when opening a Gerbil/Scheme file if not running."
  (when (and (not (lsp-running?))
             (not *lsp-initializing*)
             (buffer-file-path buf))
    (let ((path (buffer-file-path buf)))
      (when (and path
                 (let ((ext (path-extension path)))
                   (or (string=? ext ".ss") (string=? ext ".scm"))))
        (let ((root (lsp-find-project-root path)))
          (when root
            (echo-message! (app-state-echo app) "LSP: starting gerbil-lsp...")
            (if (lsp-start! root)
              (echo-message! (app-state-echo app) "LSP: initializing...")
              (echo-error! (app-state-echo app) "LSP: failed to start gerbil-lsp"))))))))

(def (lsp-find-project-root path)
  "Walk up from PATH to find a directory containing gerbil.pkg or .git."
  (let loop ((dir (path-directory (path-expand path))))
    (cond
      ((or (string=? dir "/") (string=? dir ""))
       #f)
      ((or (file-exists? (path-expand "gerbil.pkg" dir))
           (file-exists? (path-expand ".git" dir)))
       dir)
      (else
       (loop (path-directory (path-strip-trailing-separator dir)))))))

(def (path-strip-trailing-separator path)
  "Remove trailing / from path."
  (let ((len (string-length path)))
    (if (and (> len 1) (char=? (string-ref path (- len 1)) #\/))
      (substring path 0 (- len 1))
      path)))

;;;============================================================================
;;; Diagnostic margin setup (gutter markers)
;;;============================================================================

(def (lsp-setup-diagnostic-margin! ed)
  "Configure margin 1 as a symbol margin for diagnostic markers."
  ;; Margin 1: symbol margin for diagnostic markers
  (sci-send ed SCI_SETMARGINTYPEN 1 SC_MARGIN_SYMBOL)
  (sci-send ed SCI_SETMARGINWIDTHN 1 16)
  ;; Mask: allow only our diagnostic markers (bits 8-11)
  (sci-send ed SCI_SETMARGINMASKN 1
    (+ (arithmetic-shift 1 *marker-lsp-error*)
       (arithmetic-shift 1 *marker-lsp-warning*)
       (arithmetic-shift 1 *marker-lsp-info*)
       (arithmetic-shift 1 *marker-lsp-hint*)))
  ;; Define marker styles: colored left rectangles
  ;; Error: red left bar
  (sci-send ed SCI_MARKERDEFINE *marker-lsp-error* SC_MARK_LEFTRECT)
  (sci-send ed SCI_MARKERSETFORE *marker-lsp-error* (rgb->sci 255 60 60))
  (sci-send ed SCI_MARKERSETBACK *marker-lsp-error* (rgb->sci 255 60 60))
  ;; Warning: yellow left bar
  (sci-send ed SCI_MARKERDEFINE *marker-lsp-warning* SC_MARK_LEFTRECT)
  (sci-send ed SCI_MARKERSETFORE *marker-lsp-warning* (rgb->sci 255 200 0))
  (sci-send ed SCI_MARKERSETBACK *marker-lsp-warning* (rgb->sci 255 200 0))
  ;; Info: blue left bar
  (sci-send ed SCI_MARKERDEFINE *marker-lsp-info* SC_MARK_LEFTRECT)
  (sci-send ed SCI_MARKERSETFORE *marker-lsp-info* (rgb->sci 100 150 255))
  (sci-send ed SCI_MARKERSETBACK *marker-lsp-info* (rgb->sci 100 150 255))
  ;; Hint: grey left bar
  (sci-send ed SCI_MARKERDEFINE *marker-lsp-hint* SC_MARK_LEFTRECT)
  (sci-send ed SCI_MARKERSETFORE *marker-lsp-hint* (rgb->sci 150 150 150))
  (sci-send ed SCI_MARKERSETBACK *marker-lsp-hint* (rgb->sci 150 150 150)))

(def *lsp-margin-setup-done* (make-hash-table)) ;; ed -> #t

(def (lsp-ensure-diagnostic-margin! ed)
  "Ensure diagnostic margin is set up for this editor (idempotent)."
  (unless (hash-get *lsp-margin-setup-done* ed)
    (lsp-setup-diagnostic-margin! ed)
    (hash-put! *lsp-margin-setup-done* ed #t)))

;;;============================================================================
;;; Diagnostics handler (installed into lsp-client callbacks)
;;;============================================================================

(def (lsp-install-handlers! app)
  "Install UI handlers into the lsp-client module."
  (set-box! *lsp-diagnostics-handler*
    (lambda (uri diags)
      (lsp-handle-diagnostics! app uri diags)))
  (set-box! *lsp-show-message-handler*
    (lambda (params)
      (let ((msg (hash-get params "message")))
        (when msg
          (echo-message! (app-state-echo app)
            (string-append "LSP: " msg))))))
  ;; After initialization, send didOpen for all currently open .ss/.scm buffers.
  ;; This is needed because lsp-hook-did-open! is called immediately after
  ;; lsp-start!, before the async initialize handshake completes.
  (set-box! *lsp-on-initialized-handler*
    (lambda ()
      (let* ((ed (current-qt-editor app))
             (cur-buf (current-qt-buffer app)))
        (for-each
          (lambda (buf)
            (when (buffer-file-path buf)
              (let* ((path (buffer-file-path buf))
                     (ext (path-extension path)))
                (when (or (string=? ext ".ss") (string=? ext ".scm"))
                  (let* ((uri (file-path->uri path))
                         (lang-id (lsp-language-id path))
                         (text (if (eq? buf cur-buf)
                                 (qt-plain-text-edit-text ed)
                                 (or (read-file-as-string path) ""))))
                    (lsp-record-sent-content! uri text)
                    (lsp-did-open! uri lang-id text))))))
          *buffer-list*))))
  ;; Install LSP modeline provider
  (set-box! *lsp-modeline-provider*
    (lambda ()
      (if (lsp-running?)
        (let ((errors 0) (warnings 0))
          (hash-for-each
            (lambda (uri diags)
              (for-each
                (lambda (d)
                  (when (hash-table? d)
                    (let ((sev (or (hash-get d "severity") 1)))
                      (cond ((= sev 1) (set! errors (+ errors 1)))
                            ((= sev 2) (set! warnings (+ warnings 1)))))))
                diags))
            *lsp-diagnostics*)
          (string-append "[LSP"
            (if (or (> errors 0) (> warnings 0))
              (string-append " E:" (number->string errors)
                             " W:" (number->string warnings))
              "")
            "]"))
        #f))))

(def (lsp-handle-diagnostics! app uri diags)
  "Handle publishDiagnostics — update indicators and compilation error list."
  (lsp-apply-diagnostics-indicators! app uri diags)
  (lsp-update-compilation-errors!))

(def (lsp-apply-diagnostics-indicators! app uri diags)
  "Apply Scintilla squiggly underline indicators and margin markers for diagnostics."
  (let* ((file-path (uri->file-path uri))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (buf-path (buffer-file-path buf)))
    ;; Only apply indicators to the currently visible buffer
    (when (and buf-path file-path
               (string=? (path-expand buf-path) (path-expand file-path)))
      (let ((sci ed))
        ;; Ensure diagnostic margin is configured
        (lsp-ensure-diagnostic-margin! sci)
        ;; Clear previous LSP indicators
        (for-each
          (lambda (indic)
            (sci-send sci SCI_SETINDICATORCURRENT indic)
            (sci-send sci SCI_INDICATORCLEARRANGE 0
              (sci-send sci SCI_GETLENGTH)))
          (list *indic-lsp-error* *indic-lsp-warning*
                *indic-lsp-info* *indic-lsp-hint*))
        ;; Clear previous margin markers
        (sci-send sci SCI_MARKERDELETEALL *marker-lsp-error*)
        (sci-send sci SCI_MARKERDELETEALL *marker-lsp-warning*)
        (sci-send sci SCI_MARKERDELETEALL *marker-lsp-info*)
        (sci-send sci SCI_MARKERDELETEALL *marker-lsp-hint*)
        ;; Apply new indicators and margin markers
        (for-each
          (lambda (diag)
            (when (hash-table? diag)
              (let* ((range (hash-get diag "range"))
                     (severity (or (hash-get diag "severity") 1))
                     (indic (case severity
                              ((1) *indic-lsp-error*)
                              ((2) *indic-lsp-warning*)
                              ((3) *indic-lsp-info*)
                              ((4) *indic-lsp-hint*)
                              (else *indic-lsp-info*)))
                     (marker (case severity
                               ((1) *marker-lsp-error*)
                               ((2) *marker-lsp-warning*)
                               ((3) *marker-lsp-info*)
                               ((4) *marker-lsp-hint*)
                               (else *marker-lsp-info*))))
                (when (and range (hash-table? range))
                  (let* ((start-pos (lsp-range-start->pos sci range))
                         (end-pos (lsp-range-end->pos sci range))
                         (len (max 1 (- end-pos start-pos)))
                         (start-obj (hash-get range "start"))
                         (line (and start-obj (hash-get start-obj "line"))))
                    ;; Configure indicator style
                    (sci-send sci SCI_INDICSETSTYLE indic
                      (if (or (= severity 1) (= severity 2))
                        INDIC_SQUIGGLE INDIC_DOTS))
                    (sci-send sci SCI_INDICSETFORE indic
                      (case severity
                        ((1) (rgb->sci 255 0 0))     ;; red
                        ((2) (rgb->sci 255 200 0))   ;; yellow
                        ((3) (rgb->sci 100 150 255)) ;; blue
                        ((4) (rgb->sci 150 150 150)) ;; grey
                        (else (rgb->sci 100 150 255))))
                    (sci-send sci SCI_SETINDICATORCURRENT indic)
                    (sci-send sci SCI_INDICATORFILLRANGE start-pos len)
                    ;; Add margin marker on the diagnostic line
                    (when line
                      (sci-send sci SCI_MARKERADD line marker)))))))
          diags)))))

(def (lsp-range-start->pos sci range)
  "Convert LSP range start (line, character) to Scintilla position."
  (let* ((start (hash-get range "start"))
         (line (hash-get start "line"))
         (char (hash-get start "character")))
    (if (and line char)
      (+ (sci-send sci SCI_POSITIONFROMLINE line) char)
      0)))

(def (lsp-range-end->pos sci range)
  "Convert LSP range end (line, character) to Scintilla position."
  (let* ((end (hash-get range "end"))
         (line (hash-get end "line"))
         (char (hash-get end "character")))
    (if (and line char)
      (+ (sci-send sci SCI_POSITIONFROMLINE line) char)
      0)))

(def (lsp-update-compilation-errors!)
  "Rebuild *compilation-errors* from LSP diagnostics for M-g n/p navigation."
  (set! *compilation-errors* [])
  (set! *compilation-error-index* -1)
  (hash-for-each
    (lambda (uri diags)
      (let ((file (uri->file-path uri)))
        (for-each
          (lambda (diag)
            (when (hash-table? diag)
              (let* ((range (hash-get diag "range"))
                     (start (and range (hash-get range "start")))
                     (line (and start (+ 1 (hash-get start "line"))))
                     (col (and start (+ 1 (hash-get start "character"))))
                     (msg (or (hash-get diag "message") "unknown")))
                (when (and line col)
                  (set! *compilation-errors*
                    (append *compilation-errors*
                      (list (list file line col msg))))))))
          diags)))
    *lsp-diagnostics*))

;;;============================================================================
;;; Helper: get current cursor position as LSP params
;;;============================================================================

(def (lsp-current-params app)
  "Build TextDocumentPositionParams from current cursor position."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      #f
      (let ((uri (file-path->uri path))
            (line (qt-plain-text-edit-cursor-line ed))
            (col (qt-plain-text-edit-cursor-column ed)))
        (lsp-text-document-position uri line col)))))

;;;============================================================================
;;; Goto definition
;;;============================================================================

(def (cmd-lsp-goto-definition app)
  "Jump to definition of symbol at point via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (begin
          (echo-message! (app-state-echo app) "LSP: finding definition...")
          (lsp-send-request! "textDocument/definition" params
            (lambda (response)
              (lsp-handle-location-response! app response "definition"))))))))

(def (cmd-lsp-declaration app)
  "Jump to declaration via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (begin
          (echo-message! (app-state-echo app) "LSP: finding declaration...")
          (lsp-send-request! "textDocument/declaration" params
            (lambda (response)
              (lsp-handle-location-response! app response "declaration"))))))))

(def (cmd-lsp-type-definition app)
  "Jump to type definition via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (lsp-send-request! "textDocument/typeDefinition" params
          (lambda (response)
            (lsp-handle-location-response! app response "type definition")))))))

(def (cmd-lsp-implementation app)
  "Jump to implementation via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (lsp-send-request! "textDocument/implementation" params
          (lambda (response)
            (lsp-handle-location-response! app response "implementation")))))))

(def (lsp-handle-location-response! app response kind)
  "Handle a definition/declaration/implementation response — jump to location."
  (let ((result (hash-get response "result"))
        (error (hash-get response "error")))
    (cond
      (error
       (echo-error! (app-state-echo app)
         (string-append "LSP " kind ": "
           (or (and (hash-table? error) (hash-get error "message"))
               "error"))))
      ((not result)
       (echo-message! (app-state-echo app)
         (string-append "LSP: no " kind " found")))
      ;; Single Location object
      ((and (hash-table? result) (hash-get result "uri"))
       (xref-push-location! app)
       (lsp-goto-location! app result))
      ;; Array of Location objects
      ((and (list? result) (not (null? result)))
       (if (= (length result) 1)
         (begin
           (xref-push-location! app)
           (lsp-goto-location! app (car result)))
         (lsp-show-locations! app (string-append "LSP " kind) result)))
      (else
       (echo-message! (app-state-echo app)
         (string-append "LSP: no " kind " found"))))))

(def (lsp-goto-location! app location)
  "Jump to an LSP Location (hash with uri, range)."
  (let* ((uri (hash-get location "uri"))
         (range (hash-get location "range"))
         (start (and range (hash-get range "start")))
         (line (and start (hash-get start "line")))
         (col (and start (hash-get start "character")))
         (file (uri->file-path uri))
         (fr (app-state-frame app))
         (ed (current-qt-editor app)))
    (when (and file line)
      ;; Open file if different from current
      (let* ((cur-buf (current-qt-buffer app))
             (cur-path (buffer-file-path cur-buf)))
        (when (or (not cur-path)
                  (not (string=? (path-expand cur-path)
                                 (path-expand file))))
          ;; Find or create buffer for file
          (let* ((existing (let loop ((bufs *buffer-list*))
                             (if (null? bufs) #f
                               (let ((b (car bufs)))
                                 (if (and (buffer-file-path b)
                                          (string=? (path-expand (buffer-file-path b))
                                                    (path-expand file)))
                                   b (loop (cdr bufs)))))))
                 (buf (or existing
                          (let ((b (qt-buffer-create!
                                     (path-strip-directory file) ed file)))
                            (when (file-exists? file)
                              (let ((text (read-file-as-string file)))
                                (when text
                                  (qt-buffer-attach! ed b)
                                  (set! (qt-edit-window-buffer (qt-current-window fr)) b)
                                  (qt-plain-text-edit-set-text! ed text)
                                  (qt-text-document-set-modified! (buffer-doc-pointer b) #f)))
                              (qt-setup-highlighting! app b))
                            b))))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf))))
      ;; Navigate to position
      (let* ((target-line (+ line 1))  ;; text-line-position uses 1-based
             (text (qt-plain-text-edit-text ed))
             (line-pos (text-line-position text target-line))
             (pos (+ line-pos (or col 0))))
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

;;;============================================================================
;;; Hover
;;;============================================================================

(def (cmd-lsp-hover app)
  "Show hover information for symbol at point via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (lsp-send-request! "textDocument/hover" params
          (lambda (response)
            (let ((result (hash-get response "result"))
                  (error (hash-get response "error")))
              (cond
                (error
                 (echo-error! (app-state-echo app)
                   (string-append "LSP hover: "
                     (or (and (hash-table? error) (hash-get error "message"))
                         "error"))))
                ((not result)
                 (echo-message! (app-state-echo app) "LSP: no hover info"))
                (else
                 (let ((contents (hash-get result "contents")))
                   (cond
                     ;; MarkedString or MarkupContent with "value" key
                     ((and (hash-table? contents) (hash-get contents "value"))
                      (let ((text (hash-get contents "value")))
                        (lsp-show-hover-text! app text)))
                     ;; Plain string
                     ((string? contents)
                      (lsp-show-hover-text! app contents))
                     ;; Array of MarkedString
                     ((list? contents)
                      (let ((texts (map (lambda (c)
                                          (if (hash-table? c)
                                            (or (hash-get c "value") "")
                                            (if (string? c) c "")))
                                        contents)))
                        (lsp-show-hover-text! app
                          (string-join texts "\n"))))
                     (else
                      (echo-message! (app-state-echo app)
                        "LSP: no hover info")))))))))))))

(def (lsp-show-hover-text! app text)
  "Display hover text — short text in echo area, long text in *Hover* buffer."
  (if (< (string-length text) 200)
    (echo-message! (app-state-echo app) text)
    ;; Long text: show in a buffer
    (let* ((fr (app-state-frame app))
           (ed (current-qt-editor app))
           (buf-name "*LSP Hover*")
           (existing (buffer-by-name buf-name))
           (buf (or existing
                    (qt-buffer-create! buf-name ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;;;============================================================================
;;; Completion
;;;============================================================================

(def (cmd-lsp-completion app)
  "Request completion at point via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (lsp-send-request! "textDocument/completion" params
          (lambda (response)
            (let ((result (hash-get response "result"))
                  (error (hash-get response "error")))
              (cond
                (error
                 (echo-error! (app-state-echo app) "LSP completion: error"))
                ((not result)
                 (echo-message! (app-state-echo app) "LSP: no completions"))
                (else
                 (let* ((items (cond
                                 ((list? result) result)
                                 ((and (hash-table? result)
                                       (hash-get result "items"))
                                  (hash-get result "items"))
                                 (else [])))
                        (labels (filter-map
                                  (lambda (item)
                                    (and (hash-table? item)
                                         (hash-get item "label")))
                                  items)))
                   (if (null? labels)
                     (echo-message! (app-state-echo app) "LSP: no completions")
                     ;; Show completion list using echo area
                     (let ((choice (qt-echo-read-string-with-completion
                                     app "Complete: " labels)))
                       (when (and choice (not (string=? choice "")))
                         (lsp-insert-completion! app choice))))))))))))))

(def (lsp-insert-completion! app text)
  "Insert completion text, replacing the symbol at point."
  (let* ((ed (current-qt-editor app))
         (full-text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length full-text))
         ;; Find symbol start
         (sym-char? (lambda (ch)
                      (or (char-alphabetic? ch) (char-numeric? ch)
                          (char=? ch #\-) (char=? ch #\_)
                          (char=? ch #\?) (char=? ch #\!))))
         (start (let loop ((i pos))
                  (if (and (> i 0) (sym-char? (string-ref full-text (- i 1))))
                    (loop (- i 1)) i)))
         ;; Build new text
         (new-text (string-append
                     (substring full-text 0 start)
                     text
                     (substring full-text pos len))))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length text)))))

;;;============================================================================
;;; Rename
;;;============================================================================

(def (cmd-lsp-rename app)
  "Rename symbol at point via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (let ((new-name (qt-echo-read-string app "LSP rename to: ")))
          (when (and new-name (not (string=? new-name "")))
            (hash-put! params "newName" new-name)
            (lsp-send-request! "textDocument/rename" params
              (lambda (response)
                (let ((result (hash-get response "result"))
                      (error (hash-get response "error")))
                  (cond
                    (error
                     (echo-error! (app-state-echo app)
                       (string-append "LSP rename: "
                         (or (and (hash-table? error) (hash-get error "message"))
                             "error"))))
                    ((not result)
                     (echo-message! (app-state-echo app) "LSP: rename failed"))
                    (else
                     (lsp-apply-workspace-edit! app result)
                     (echo-message! (app-state-echo app)
                       (string-append "LSP: renamed to '" new-name "'")))))))))))))

;;;============================================================================
;;; Code actions
;;;============================================================================

(def (cmd-lsp-code-actions app)
  "Request code actions at point via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let* ((ed (current-qt-editor app))
           (buf (current-qt-buffer app))
           (path (buffer-file-path buf)))
      (if (not path)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (let* ((uri (file-path->uri path))
               (line (qt-plain-text-edit-cursor-line ed))
               (col (qt-plain-text-edit-cursor-column ed))
               (params (make-hash-table))
               (td (make-hash-table))
               (range-h (make-hash-table))
               (pos-h (make-hash-table))
               (ctx (make-hash-table)))
          (hash-put! td "uri" uri)
          (hash-put! pos-h "line" line)
          (hash-put! pos-h "character" col)
          (hash-put! range-h "start" pos-h)
          (hash-put! range-h "end" pos-h)
          (hash-put! ctx "diagnostics"
            (or (hash-get *lsp-diagnostics* uri) []))
          (hash-put! params "textDocument" td)
          (hash-put! params "range" range-h)
          (hash-put! params "context" ctx)
          (lsp-send-request! "textDocument/codeAction" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error
                   (echo-error! (app-state-echo app) "LSP code actions: error"))
                  ((or (not result) (null? result))
                   (echo-message! (app-state-echo app) "LSP: no code actions"))
                  (else
                   (let* ((titles (map (lambda (a)
                                         (if (hash-table? a)
                                           (or (hash-get a "title") "?")
                                           "?"))
                                       result))
                          (choice (qt-echo-read-string-with-completion
                                    app "Code action: " titles)))
                     (when (and choice (not (string=? choice "")))
                       ;; Find the matching action
                       (let loop ((actions result))
                         (when (pair? actions)
                           (let ((a (car actions)))
                             (if (and (hash-table? a)
                                      (equal? (hash-get a "title") choice))
                               ;; Apply action
                               (let ((edit (hash-get a "edit")))
                                 (when edit
                                   (lsp-apply-workspace-edit! app edit))
                                 (echo-message! (app-state-echo app)
                                   (string-append "Applied: " choice)))
                               (loop (cdr actions))))))))))))))))))

;;;============================================================================
;;; Find references
;;;============================================================================

(def (cmd-lsp-find-references app)
  "Find all references to symbol at point via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (begin
          ;; Add context.includeDeclaration
          (let ((ctx (make-hash-table)))
            (hash-put! ctx "includeDeclaration" #t)
            (hash-put! params "context" ctx))
          (echo-message! (app-state-echo app) "LSP: finding references...")
          (lsp-send-request! "textDocument/references" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error
                   (echo-error! (app-state-echo app) "LSP references: error"))
                  ((or (not result) (null? result))
                   (echo-message! (app-state-echo app) "LSP: no references found"))
                  (else
                   (lsp-show-locations! app "LSP References" result)))))))))))

;;;============================================================================
;;; Document symbols
;;;============================================================================

(def (cmd-lsp-document-symbols app)
  "List symbols in current document via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let* ((ed (current-qt-editor app))
           (buf (current-qt-buffer app))
           (path (buffer-file-path buf)))
      (if (not path)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (let ((params (make-hash-table))
              (td (make-hash-table)))
          (hash-put! td "uri" (file-path->uri path))
          (hash-put! params "textDocument" td)
          (lsp-send-request! "textDocument/documentSymbol" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error
                   (echo-error! (app-state-echo app) "LSP symbols: error"))
                  ((or (not result) (null? result))
                   (echo-message! (app-state-echo app) "LSP: no symbols"))
                  (else
                   ;; Build list of symbol names for completion
                   (let* ((names (map (lambda (sym)
                                        (if (hash-table? sym)
                                          (or (hash-get sym "name") "?")
                                          "?"))
                                      result))
                          (choice (qt-echo-read-string-with-completion
                                    app "Symbol: " names)))
                     (when (and choice (not (string=? choice "")))
                       ;; Find matching symbol and jump to it
                       (let loop ((syms result))
                         (when (pair? syms)
                           (let ((s (car syms)))
                             (if (and (hash-table? s)
                                      (equal? (hash-get s "name") choice))
                               ;; Jump to symbol location
                               (let* ((loc (or (hash-get s "location")
                                               s))
                                      (range (hash-get loc "range"))
                                      (start (and range (hash-get range "start")))
                                      (line (and start (hash-get start "line"))))
                                 (when line
                                   (xref-push-location! app)
                                   (let* ((text (qt-plain-text-edit-text ed))
                                          (pos (text-line-position text (+ line 1))))
                                     (qt-plain-text-edit-set-cursor-position! ed pos)
                                     (qt-plain-text-edit-ensure-cursor-visible! ed))))
                               (loop (cdr syms))))))))))))))))))

;;;============================================================================
;;; Workspace symbol
;;;============================================================================

(def (cmd-lsp-workspace-symbol app)
  "Search workspace symbols via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((query (qt-echo-read-string app "Workspace symbol: ")))
      (when (and query (not (string=? query "")))
        (let ((params (make-hash-table)))
          (hash-put! params "query" query)
          (lsp-send-request! "workspace/symbol" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error
                   (echo-error! (app-state-echo app) "LSP workspace symbol: error"))
                  ((or (not result) (null? result))
                   (echo-message! (app-state-echo app) "LSP: no symbols found"))
                  (else
                   ;; Show as locations
                   (let ((locations (filter-map
                                     (lambda (sym)
                                       (and (hash-table? sym)
                                            (hash-get sym "location")))
                                     result)))
                     (if (null? locations)
                       (echo-message! (app-state-echo app) "LSP: no symbols found")
                       (lsp-show-locations! app "LSP Symbols" locations)))))))))))))

;;;============================================================================
;;; Formatting
;;;============================================================================

(def (cmd-lsp-format-buffer app)
  "Format current buffer via LSP."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let* ((buf (current-qt-buffer app))
           (path (buffer-file-path buf)))
      (if (not path)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (let ((params (make-hash-table))
              (td (make-hash-table))
              (opts (make-hash-table)))
          (hash-put! td "uri" (file-path->uri path))
          (hash-put! opts "tabSize" 2)
          (hash-put! opts "insertSpaces" #t)
          (hash-put! params "textDocument" td)
          (hash-put! params "options" opts)
          (lsp-send-request! "textDocument/formatting" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error
                   (echo-error! (app-state-echo app) "LSP format: error"))
                  ((or (not result) (null? result))
                   (echo-message! (app-state-echo app) "LSP: no formatting changes"))
                  (else
                   (lsp-apply-text-edits! app (current-qt-editor app) result)
                   (echo-message! (app-state-echo app)
                     "LSP: buffer formatted")))))))))))

(def (cmd-lsp-format-region app)
  "Format selected region via LSP rangeFormatting."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let* ((ed (current-qt-editor app))
           (buf (current-qt-buffer app))
           (path (buffer-file-path buf)))
      (if (not path)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (let* ((sel-start (sci-send ed 2143))  ;; SCI_GETSELECTIONSTART
               (sel-end (sci-send ed 2145))     ;; SCI_GETSELECTIONEND
               (start-line (sci-send ed 2166 sel-start))  ;; SCI_LINEFROMPOSITION
               (start-col (- sel-start (sci-send ed 2167 start-line)))  ;; SCI_POSITIONFROMLINE
               (end-line (sci-send ed 2166 sel-end))
               (end-col (- sel-end (sci-send ed 2167 end-line))))
          (if (= sel-start sel-end)
            (echo-message! (app-state-echo app) "LSP: no region selected, use format-buffer")
            (let ((params (make-hash-table))
                  (td (make-hash-table))
                  (range (make-hash-table))
                  (range-start (make-hash-table))
                  (range-end (make-hash-table))
                  (opts (make-hash-table)))
              (hash-put! td "uri" (file-path->uri path))
              (hash-put! range-start "line" start-line)
              (hash-put! range-start "character" start-col)
              (hash-put! range-end "line" end-line)
              (hash-put! range-end "character" end-col)
              (hash-put! range "start" range-start)
              (hash-put! range "end" range-end)
              (hash-put! opts "tabSize" 2)
              (hash-put! opts "insertSpaces" #t)
              (hash-put! params "textDocument" td)
              (hash-put! params "range" range)
              (hash-put! params "options" opts)
              (lsp-send-request! "textDocument/rangeFormatting" params
                (lambda (response)
                  (let ((result (hash-get response "result"))
                        (error (hash-get response "error")))
                    (cond
                      (error
                       (echo-error! (app-state-echo app) "LSP range format: error"))
                      ((or (not result) (null? result))
                       (echo-message! (app-state-echo app) "LSP: no formatting changes"))
                      (else
                       (lsp-apply-text-edits! app ed result)
                       (echo-message! (app-state-echo app)
                         "LSP: region formatted")))))))))))))

;;;============================================================================
;;; Signature help (eldoc integration)
;;;============================================================================

(def (lsp-eldoc-display! app)
  "Query LSP signatureHelp and display in echo area. Called from eldoc timer."
  (when (lsp-running?)
    (let ((params (lsp-current-params app)))
      (when params
        (lsp-send-request! "textDocument/signatureHelp" params
          (lambda (response)
            (let ((result (hash-get response "result")))
              (when (and result (hash-table? result))
                (let* ((sigs (hash-get result "signatures"))
                       (active (or (hash-get result "activeSignature") 0))
                       (sig (and sigs (list? sigs) (not (null? sigs))
                                 (list-ref sigs (min active (- (length sigs) 1))))))
                  (when (and sig (hash-table? sig))
                    (let ((label (hash-get sig "label")))
                      (when label
                        (echo-message! (app-state-echo app) label)))))))))))))

;;;============================================================================
;;; Diagnostic at cursor (cursor-idle echo)
;;;============================================================================

(def (lsp-diagnostic-at-cursor! app)
  "Show diagnostic message in echo area if cursor is on a line with diagnostics."
  (when (lsp-running?)
    (let* ((ed (current-qt-editor app))
           (buf (current-qt-buffer app))
           (path (buffer-file-path buf)))
      (when path
        (let* ((uri (file-path->uri path))
               (diags (or (hash-get *lsp-diagnostics* uri) []))
               (line (qt-plain-text-edit-cursor-line ed)))
          (let loop ((ds diags))
            (when (pair? ds)
              (let ((d (car ds)))
                (if (and (hash-table? d)
                         (let* ((range (hash-get d "range"))
                                (start (and range (hash-get range "start")))
                                (start-line (and start (hash-get start "line"))))
                           (and start-line (= start-line line))))
                  (let* ((msg (or (hash-get d "message") ""))
                         (sev (or (hash-get d "severity") 1))
                         (prefix (case sev
                                   ((1) "Error: ")
                                   ((2) "Warning: ")
                                   ((3) "Info: ")
                                   (else "Hint: "))))
                    (echo-message! (app-state-echo app) (string-append prefix msg)))
                  (loop (cdr ds)))))))))))

;;;============================================================================
;;; Document highlight (cursor-idle)
;;;============================================================================

(def (lsp-document-highlight! app)
  "Highlight all occurrences of symbol under cursor via indicator 16."
  (when (lsp-running?)
    (let* ((ed (current-qt-editor app))
           (sci ed))
      ;; Clear previous highlights
      (sci-send sci SCI_SETINDICATORCURRENT *indic-lsp-highlight*)
      (sci-send sci SCI_INDICATORCLEARRANGE 0
        (sci-send sci SCI_GETLENGTH))
      ;; Request highlights
      (let ((params (lsp-current-params app)))
        (when params
          (lsp-send-request! "textDocument/documentHighlight" params
            (lambda (response)
              (let ((result (hash-get response "result")))
                (when (and result (list? result) (not (null? result)))
                  ;; Configure highlight indicator
                  (sci-send sci SCI_INDICSETSTYLE *indic-lsp-highlight*
                    INDIC_ROUNDBOX)
                  (sci-send sci SCI_INDICSETFORE *indic-lsp-highlight*
                    (rgb->sci 200 200 255))
                  (sci-send sci SCI_SETINDICATORCURRENT *indic-lsp-highlight*)
                  ;; Fill ranges
                  (for-each
                    (lambda (hl)
                      (when (hash-table? hl)
                        (let ((range (hash-get hl "range")))
                          (when range
                            (let* ((start (lsp-range-start->pos sci range))
                                   (end (lsp-range-end->pos sci range))
                                   (len (max 1 (- end start))))
                              (sci-send sci SCI_INDICATORFILLRANGE
                                start len))))))
                    result))))))))))

;;;============================================================================
;;; Workspace edit application
;;;============================================================================

(def (lsp-apply-workspace-edit! app edit)
  "Apply a WorkspaceEdit (changes or documentChanges)."
  (when (hash-table? edit)
    (let ((changes (hash-get edit "changes"))
          (doc-changes (hash-get edit "documentChanges")))
      (cond
        ;; documentChanges (array of TextDocumentEdit)
        (doc-changes
         (for-each
           (lambda (tde)
             (when (hash-table? tde)
               (let* ((td (hash-get tde "textDocument"))
                      (uri (and td (hash-get td "uri")))
                      (edits (hash-get tde "edits")))
                 (when (and uri edits)
                   (lsp-apply-edits-to-uri! app uri edits)))))
           doc-changes))
        ;; changes (object: uri -> TextEdit[])
        (changes
         (hash-for-each
           (lambda (uri edits)
             (lsp-apply-edits-to-uri! app uri edits))
           changes))))))

(def (lsp-apply-edits-to-uri! app uri edits)
  "Open the file for URI and apply text edits."
  (let* ((file (uri->file-path uri))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (cur-path (and (buffer-file-path buf) (path-expand (buffer-file-path buf)))))
    (when file
      ;; If it's the current buffer, apply directly
      (if (and cur-path (string=? cur-path (path-expand file)))
        (lsp-apply-text-edits! app ed edits)
        ;; Otherwise find/open buffer
        (let* ((existing (let loop ((bufs *buffer-list*))
                           (if (null? bufs) #f
                             (let ((b (car bufs)))
                               (if (and (buffer-file-path b)
                                        (string=? (path-expand (buffer-file-path b))
                                                  (path-expand file)))
                                 b (loop (cdr bufs)))))))
               (target-buf (or existing
                              (qt-buffer-create!
                                (path-strip-directory file) ed file))))
          (qt-buffer-attach! ed target-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) target-buf)
          (when (and (not existing) (file-exists? file))
            (let ((text (read-file-as-string file)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer target-buf) #f))))
          (lsp-apply-text-edits! app ed edits))))))

(def (lsp-apply-text-edits! app ed edits)
  "Apply a list of TextEdit hashes to an editor. Sorts in reverse order."
  (let* ((sorted (sort edits
                   (lambda (a b)
                     ;; Sort in reverse document order so earlier edits
                     ;; don't shift positions of later ones
                     (let* ((ra (hash-get a "range"))
                            (rb (hash-get b "range"))
                            (sa (and ra (hash-get ra "start")))
                            (sb (and rb (hash-get rb "start")))
                            (la (or (and sa (hash-get sa "line")) 0))
                            (lb (or (and sb (hash-get sb "line")) 0)))
                       (if (= la lb)
                         (> (or (and sa (hash-get sa "character")) 0)
                            (or (and sb (hash-get sb "character")) 0))
                         (> la lb))))))
         (text (qt-plain-text-edit-text ed))
         (sci ed))
    ;; Apply each edit
    (for-each
      (lambda (edit)
        (when (hash-table? edit)
          (let* ((range (hash-get edit "range"))
                 (new-text (or (hash-get edit "newText") ""))
                 (start-pos (lsp-range-start->pos sci range))
                 (end-pos (lsp-range-end->pos sci range)))
            ;; Replace the range
            (let* ((cur-text (qt-plain-text-edit-text ed))
                   (before (substring cur-text 0 start-pos))
                   (after (substring cur-text end-pos (string-length cur-text)))
                   (result (string-append before new-text after)))
              (qt-plain-text-edit-set-text! ed result)))))
      sorted)))

;;;============================================================================
;;; References/locations display (grep-style buffer)
;;;============================================================================

(def (lsp-show-locations! app title locations)
  "Display LSP locations in a *References* buffer with file:line format."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf-name "*References*")
         (existing (buffer-by-name buf-name))
         (buf (or existing
                  (qt-buffer-create! buf-name ed #f)))
         (lines (filter-map
                  (lambda (loc)
                    (when (hash-table? loc)
                      (let* ((uri (hash-get loc "uri"))
                             (range (hash-get loc "range"))
                             (start (and range (hash-get range "start")))
                             (line (and start (+ 1 (hash-get start "line"))))
                             (file (and uri (uri->file-path uri))))
                        (when (and file line)
                          (string-append file ":"
                            (number->string line))))))
                  locations))
         (text (string-append title " (" (number->string (length lines)) " results)\n\n"
                 (string-join lines "\n"))))
    ;; Populate grep results for M-g n/p navigation
    (set! *grep-results*
      (filter-map
        (lambda (loc)
          (when (hash-table? loc)
            (let* ((uri (hash-get loc "uri"))
                   (range (hash-get loc "range"))
                   (start (and range (hash-get range "start")))
                   (line (and start (+ 1 (hash-get start "line"))))
                   (file (and uri (uri->file-path uri))))
              (when (and file line)
                (list file line "")))))
        locations))
    (set! *grep-result-index* -1)
    ;; Show buffer
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append title ": " (number->string (length lines)) " results"))))

;;;============================================================================
;;; Semantic Tokens
;;;============================================================================

;; Additional indicator IDs for semantic highlighting
(def *indic-sem-keyword*  17)
(def *indic-sem-function* 18)
(def *indic-sem-type*     19)
(def *indic-sem-string*   20)
(def *indic-sem-comment*  21)
(def *indic-sem-variable* 22)
(def *indic-sem-number*   23)
(def *indic-sem-macro*    24)

;; LSP semantic token type names → indicator IDs
(def *semantic-token-types*
  ["namespace" "type" "class" "enum" "interface" "struct"
   "typeParameter" "parameter" "variable" "property" "enumMember"
   "event" "function" "method" "macro" "keyword" "modifier"
   "comment" "string" "number" "regexp" "operator" "decorator"])

(def (semantic-token-type->indicator type-index)
  "Map a semantic token type index to a Scintilla indicator ID."
  (cond
    ((or (= type-index 15) (= type-index 16)) *indic-sem-keyword*)  ; keyword, modifier
    ((or (= type-index 12) (= type-index 13)) *indic-sem-function*) ; function, method
    ((or (= type-index 1) (= type-index 2) (= type-index 3)
         (= type-index 4) (= type-index 5)) *indic-sem-type*)       ; type, class, enum, interface, struct
    ((= type-index 18) *indic-sem-string*)    ; string
    ((= type-index 17) *indic-sem-comment*)   ; comment
    ((or (= type-index 8) (= type-index 7) (= type-index 9)
         (= type-index 0)) *indic-sem-variable*) ; variable, parameter, property, namespace
    ((= type-index 19) *indic-sem-number*)    ; number
    ((= type-index 14) *indic-sem-macro*)     ; macro
    (else #f)))

(def *lsp-semantic-tokens-enabled* #f)

(def (lsp-setup-semantic-indicators! sci)
  "Configure Scintilla indicators for semantic tokens."
  (let ((setup-indic
          (lambda (indic style color)
            (sci-send sci SCI_INDICSETSTYLE indic style)
            (sci-send sci SCI_INDICSETFORE indic color))))
    ;; INDIC_TEXTFORE (17) = change text foreground color
    (setup-indic *indic-sem-keyword*  17 #x0000FF)   ; blue for keywords
    (setup-indic *indic-sem-function* 17 #x795E26)   ; brown for functions
    (setup-indic *indic-sem-type*     17 #x267F99)   ; teal for types
    (setup-indic *indic-sem-string*   17 #xA31515)   ; red for strings
    (setup-indic *indic-sem-comment*  17 #x008000)   ; green for comments
    (setup-indic *indic-sem-variable* 17 #x001080)   ; dark blue for variables
    (setup-indic *indic-sem-number*   17 #x098658)   ; green for numbers
    (setup-indic *indic-sem-macro*    17 #x800080)))  ; purple for macros

(def (lsp-clear-semantic-tokens! sci)
  "Clear all semantic token indicators."
  (let ((len (sci-send sci SCI_GETLENGTH 0)))
    (for-each (lambda (indic)
                (sci-send sci SCI_SETINDICATORCURRENT indic)
                (sci-send sci SCI_INDICATORCLEARRANGE 0 len))
      [*indic-sem-keyword* *indic-sem-function* *indic-sem-type*
       *indic-sem-string* *indic-sem-comment* *indic-sem-variable*
       *indic-sem-number* *indic-sem-macro*])))

(def (lsp-apply-semantic-tokens! sci data)
  "Apply semantic token data (flat array) to Scintilla editor.
   LSP semantic tokens are encoded as: [deltaLine deltaStart length tokenType tokenModifiers ...]"
  (when (and data (list? data) (not (null? data)))
    (lsp-clear-semantic-tokens! sci)
    (let loop ((rest data) (line 0) (char 0))
      (when (>= (length rest) 5)
        (let* ((delta-line (list-ref rest 0))
               (delta-start (list-ref rest 1))
               (tok-length (list-ref rest 2))
               (tok-type (list-ref rest 3))
               ;; tok-modifiers = (list-ref rest 4)
               (cur-line (+ line delta-line))
               (cur-char (if (> delta-line 0) delta-start (+ char delta-start)))
               (pos (+ (sci-send sci SCI_POSITIONFROMLINE cur-line) cur-char))
               (indic (semantic-token-type->indicator tok-type)))
          (when (and indic (> tok-length 0))
            (sci-send sci SCI_SETINDICATORCURRENT indic)
            (sci-send sci SCI_INDICATORFILLRANGE pos tok-length))
          (loop (list-tail rest 5) cur-line cur-char))))))

(def (cmd-lsp-semantic-tokens app)
  "Toggle semantic token highlighting from LSP."
  (set! *lsp-semantic-tokens-enabled* (not *lsp-semantic-tokens-enabled*))
  (if *lsp-semantic-tokens-enabled*
    (begin
      (echo-message! (app-state-echo app) "LSP semantic tokens enabled")
      (lsp-request-semantic-tokens! app))
    (let ((sci (current-qt-editor app)))
      (lsp-clear-semantic-tokens! sci)
      (echo-message! (app-state-echo app) "LSP semantic tokens disabled"))))

(def (lsp-request-semantic-tokens! app)
  "Request full semantic tokens for current buffer."
  (when (and *lsp-semantic-tokens-enabled* (lsp-running?))
    (let* ((buf (current-qt-buffer app))
           (path (and buf (buffer-file-path buf))))
      (when path
        (let ((params (make-hash-table))
              (td (make-hash-table)))
          (hash-put! td "uri" (file-path->uri path))
          (hash-put! params "textDocument" td)
          (lsp-send-request! "textDocument/semanticTokens/full" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error #f)  ; silently ignore errors
                  ((and result (hash-table? result))
                   (let ((data (hash-get result "data")))
                     (when (and data (list? data))
                       (let ((sci (current-qt-editor app)))
                         (lsp-setup-semantic-indicators! sci)
                         (lsp-apply-semantic-tokens! sci data))))))))))))))

;;;============================================================================
;;; Call Hierarchy
;;;============================================================================

(def (cmd-lsp-incoming-calls app)
  "Show incoming calls (who calls this function) via LSP call hierarchy."
  (lsp-call-hierarchy! app 'incoming))

(def (cmd-lsp-outgoing-calls app)
  "Show outgoing calls (what this function calls) via LSP call hierarchy."
  (lsp-call-hierarchy! app 'outgoing))

(def (lsp-call-hierarchy! app direction)
  "Request call hierarchy: direction is 'incoming or 'outgoing."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (begin
          (echo-message! (app-state-echo app)
            (string-append "LSP: finding " (symbol->string direction) " calls..."))
          (lsp-send-request! "textDocument/prepareCallHierarchy" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error
                   (echo-error! (app-state-echo app) "LSP call hierarchy: error"))
                  ((or (not result) (null? result))
                   (echo-message! (app-state-echo app) "LSP: no call hierarchy item at cursor"))
                  (else
                   ;; Use first item from prepare result
                   (let* ((item (car result))
                          (method (if (eq? direction 'incoming)
                                    "callHierarchy/incomingCalls"
                                    "callHierarchy/outgoingCalls"))
                          (req-params (make-hash-table)))
                     (hash-put! req-params "item" item)
                     (lsp-send-request! method req-params
                       (lambda (ch-response)
                         (let ((ch-result (hash-get ch-response "result"))
                               (ch-error (hash-get ch-response "error")))
                           (cond
                             (ch-error
                              (echo-error! (app-state-echo app) "LSP call hierarchy: error"))
                             ((or (not ch-result) (null? ch-result))
                              (echo-message! (app-state-echo app)
                                (string-append "LSP: no " (symbol->string direction) " calls")))
                             (else
                              (lsp-show-call-hierarchy! app
                                (string-append (if (eq? direction 'incoming)
                                                 "Incoming calls to "
                                                 "Outgoing calls from ")
                                  (or (hash-get item "name") "?"))
                                ch-result direction)))))))))))))))))

(def (lsp-show-call-hierarchy! app title items direction)
  "Display call hierarchy results in a buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf-name "*Call Hierarchy*")
         (existing (buffer-by-name buf-name))
         (buf (or existing (qt-buffer-create! buf-name ed #f)))
         (lines (filter-map
                  (lambda (entry)
                    (when (hash-table? entry)
                      ;; For incoming: entry has "from" field
                      ;; For outgoing: entry has "to" field
                      (let* ((target-key (if (eq? direction 'incoming) "from" "to"))
                             (target (hash-get entry target-key))
                             (name (and target (hash-get target "name")))
                             (uri (and target (hash-get target "uri")))
                             (range (and target (hash-get target "range")))
                             (start (and range (hash-get range "start")))
                             (line (and start (+ 1 (hash-get start "line"))))
                             (file (and uri (uri->file-path uri))))
                        (when (and name file line)
                          (string-append "  " name "  ("
                            file ":" (number->string line) ")")))))
                  items))
         (text (string-append title " (" (number->string (length lines)) " results)\n\n"
                 (if (null? lines) "  (none)\n" (string-join lines "\n")))))
    ;; Populate grep results for M-g n/p navigation
    (set! *grep-results*
      (filter-map
        (lambda (entry)
          (when (hash-table? entry)
            (let* ((target-key (if (eq? direction 'incoming) "from" "to"))
                   (target (hash-get entry target-key))
                   (uri (and target (hash-get target "uri")))
                   (range (and target (hash-get target "range")))
                   (start (and range (hash-get range "start")))
                   (line (and start (+ 1 (hash-get start "line"))))
                   (file (and uri (uri->file-path uri))))
              (when (and file line)
                (list file line "")))))
        items))
    (set! *grep-result-index* -1)
    ;; Show buffer
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append title ": " (number->string (length lines)) " results"))))

;;;============================================================================
;;; Restart / Stop commands
;;;============================================================================

(def (cmd-lsp-restart app)
  "Restart the LSP server."
  (echo-message! (app-state-echo app) "LSP: restarting...")
  (lsp-stop!)
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (root (and path (lsp-find-project-root path))))
    (if root
      (begin
        (lsp-start! root)
        (lsp-install-handlers! app)
        (echo-message! (app-state-echo app) "LSP: restarted"))
      (echo-error! (app-state-echo app) "LSP: no project root found"))))

(def (cmd-lsp-stop app)
  "Stop the LSP server."
  (lsp-stop!)
  (echo-message! (app-state-echo app) "LSP: stopped"))

(def (lsp-clear-all-indicators! app)
  "Clear all LSP diagnostic indicators and margin markers from the current editor."
  (let ((ed (current-qt-editor app)))
    ;; Clear underline indicators
    (for-each
      (lambda (indic)
        (sci-send ed SCI_SETINDICATORCURRENT indic)
        (sci-send ed SCI_INDICATORCLEARRANGE 0
          (sci-send ed SCI_GETLENGTH)))
      (list *indic-lsp-error* *indic-lsp-warning*
            *indic-lsp-info* *indic-lsp-hint*))
    ;; Clear margin markers
    (sci-send ed SCI_MARKERDELETEALL *marker-lsp-error*)
    (sci-send ed SCI_MARKERDELETEALL *marker-lsp-warning*)
    (sci-send ed SCI_MARKERDELETEALL *marker-lsp-info*)
    (sci-send ed SCI_MARKERDELETEALL *marker-lsp-hint*)))

(def (cmd-toggle-lsp app)
  "Toggle LSP server on/off. Start if not running, stop if running."
  (if (lsp-running?)
    (begin
      (lsp-stop!)
      ;; Clear visual indicators from current buffer
      (lsp-clear-all-indicators! app)
      (echo-message! (app-state-echo app) "LSP: stopped"))
    (let* ((buf (current-qt-buffer app))
           (path (buffer-file-path buf))
           (root (and path (lsp-find-project-root path))))
      (if root
        (begin
          (lsp-start! root)
          (lsp-install-handlers! app)
          ;; Proactively set up diagnostic margin so indicators are visible immediately
          (let ((ed (current-qt-editor app)))
            (lsp-ensure-diagnostic-margin! ed))
          (echo-message! (app-state-echo app) "LSP: starting gerbil-lsp..."))
        (echo-error! (app-state-echo app) "LSP: no project root found")))))

;;;============================================================================
;;; Smart M-. dispatch
;;;============================================================================

(def (cmd-lsp-smart-goto-definition app)
  "Jump to definition: use LSP when running, else fallback to text search."
  (if (lsp-running?)
    (cmd-lsp-goto-definition app)
    (cmd-goto-definition app)))

;;;============================================================================
;;; didOpen/didSave/didClose hooks
;;;============================================================================

(def (lsp-hook-did-open! app buf)
  "Notify LSP server about a newly opened buffer."
  (when (and (lsp-running?) (buffer-file-path buf))
    (let* ((path (buffer-file-path buf))
           (uri (file-path->uri path))
           (lang-id (lsp-language-id path))
           (ed (current-qt-editor app))
           (text (qt-plain-text-edit-text ed)))
      (lsp-did-open! uri lang-id text))))

(def (lsp-hook-did-save! app buf)
  "Notify LSP server about a saved buffer."
  (when (and (lsp-running?) (buffer-file-path buf))
    (let* ((path (buffer-file-path buf))
           (uri (file-path->uri path))
           (ed (current-qt-editor app))
           (text (qt-plain-text-edit-text ed)))
      (lsp-did-save! uri text))))

(def (lsp-hook-did-close! app buf)
  "Notify LSP server about a closed buffer."
  (when (and (lsp-running?) (buffer-file-path buf))
    (let* ((path (buffer-file-path buf))
           (uri (file-path->uri path)))
      (lsp-did-close! uri))))

(def (lsp-hook-did-change! app buf)
  "Notify LSP server about buffer content change."
  (when (and (lsp-running?) (buffer-file-path buf))
    (let* ((path (buffer-file-path buf))
           (uri (file-path->uri path))
           (ed (current-qt-editor app))
           (text (qt-plain-text-edit-text ed)))
      (lsp-did-change! uri text))))

;;;============================================================================
;;; Inlay Hints
;;;============================================================================

(def *lsp-inlay-hints-enabled* #f)
(def *lsp-inlay-hints-cache* (make-hash-table)) ; uri -> list of hint strings per line

(def (cmd-lsp-inlay-hints app)
  "Toggle LSP inlay hints display in echo area."
  (set! *lsp-inlay-hints-enabled* (not *lsp-inlay-hints-enabled*))
  (echo-message! (app-state-echo app)
    (string-append "LSP inlay hints: " (if *lsp-inlay-hints-enabled* "ON" "OFF")))
  (when *lsp-inlay-hints-enabled*
    (lsp-request-inlay-hints! app)))

(def (lsp-request-inlay-hints! app)
  "Request inlay hints for the current buffer's visible range."
  (when (and *lsp-inlay-hints-enabled* (lsp-running?))
    (let* ((buf (current-qt-buffer app))
           (path (and buf (buffer-file-path buf))))
      (when path
        (let* ((ed (current-qt-editor app))
               (first-line (sci-send ed SCI_GETFIRSTVISIBLELINE 0))
               (lines-on-screen (sci-send ed SCI_LINESONSCREEN 0))
               (last-line (+ first-line lines-on-screen))
               (params (make-hash-table))
               (td (make-hash-table))
               (range (make-hash-table))
               (start-pos (make-hash-table))
               (end-pos (make-hash-table)))
          (hash-put! td "uri" (file-path->uri path))
          (hash-put! start-pos "line" first-line)
          (hash-put! start-pos "character" 0)
          (hash-put! end-pos "line" last-line)
          (hash-put! end-pos "character" 0)
          (hash-put! range "start" start-pos)
          (hash-put! range "end" end-pos)
          (hash-put! params "textDocument" td)
          (hash-put! params "range" range)
          (lsp-send-request! "textDocument/inlayHint" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (when (and (not error) result (list? result))
                  ;; Cache hints indexed by line number
                  (let ((by-line (make-hash-table)))
                    (for-each
                      (lambda (hint)
                        (when (hash-table? hint)
                          (let* ((pos (hash-get hint "position"))
                                 (line (and pos (hash-get pos "line")))
                                 (label (hash-get hint "label"))
                                 (kind (hash-get hint "kind"))
                                 (text (cond
                                         ((string? label) label)
                                         ((and (list? label) (not (null? label)))
                                          (string-join
                                            (filter-map
                                              (lambda (part)
                                                (and (hash-table? part)
                                                     (hash-get part "value")))
                                              label) ""))
                                         (else #f))))
                            (when (and line text)
                              (let ((existing (or (hash-get by-line line) [])))
                                (hash-put! by-line line
                                  (append existing
                                    (list (string-append
                                            (case kind
                                              ((1) ": ")   ; Type
                                              ((2) "")     ; Parameter
                                              (else ""))
                                            text)))))))))
                      result)
                    (hash-put! *lsp-inlay-hints-cache*
                      (file-path->uri path) by-line)))))))))))

(def (lsp-inlay-hint-at-cursor! app)
  "Show inlay hints for the current line in echo area. Called from idle timer."
  (when (and *lsp-inlay-hints-enabled* (lsp-running?))
    (let* ((buf (current-qt-buffer app))
           (path (and buf (buffer-file-path buf))))
      (when path
        (let* ((uri (file-path->uri path))
               (by-line (hash-get *lsp-inlay-hints-cache* uri))
               (ed (current-qt-editor app))
               (line (qt-plain-text-edit-cursor-line ed)))
          (when by-line
            (let ((hints (hash-get by-line line)))
              (when (and hints (not (null? hints)))
                (echo-message! (app-state-echo app)
                  (string-append "hints: " (string-join hints " | ")))))))))))

;;;============================================================================
;;; Type Hierarchy
;;;============================================================================

(def (cmd-lsp-supertypes app)
  "Show supertypes of the symbol at point via LSP type hierarchy."
  (lsp-type-hierarchy! app 'supertypes))

(def (cmd-lsp-subtypes app)
  "Show subtypes of the symbol at point via LSP type hierarchy."
  (lsp-type-hierarchy! app 'subtypes))

(def (lsp-type-hierarchy! app direction)
  "Request type hierarchy: direction is 'supertypes or 'subtypes."
  (if (not (lsp-running?))
    (echo-error! (app-state-echo app) "LSP: not running")
    (let ((params (lsp-current-params app)))
      (if (not params)
        (echo-error! (app-state-echo app) "LSP: buffer has no file")
        (begin
          (echo-message! (app-state-echo app)
            (string-append "LSP: finding " (symbol->string direction) "..."))
          (lsp-send-request! "textDocument/prepareTypeHierarchy" params
            (lambda (response)
              (let ((result (hash-get response "result"))
                    (error (hash-get response "error")))
                (cond
                  (error
                   (echo-error! (app-state-echo app) "LSP type hierarchy: error"))
                  ((or (not result) (null? result))
                   (echo-message! (app-state-echo app)
                     "LSP: no type hierarchy item at cursor"))
                  (else
                   (let* ((item (car result))
                          (method (if (eq? direction 'supertypes)
                                    "typeHierarchy/supertypes"
                                    "typeHierarchy/subtypes"))
                          (hier-params (make-hash-table)))
                     (hash-put! hier-params "item" item)
                     (lsp-send-request! method hier-params
                       (lambda (resp2)
                         (let ((items (hash-get resp2 "result"))
                               (err2 (hash-get resp2 "error")))
                           (cond
                             (err2
                              (echo-error! (app-state-echo app)
                                "LSP type hierarchy: error"))
                             ((or (not items) (null? items))
                              (echo-message! (app-state-echo app)
                                (string-append "LSP: no " (symbol->string direction))))
                             (else
                              (lsp-show-type-hierarchy! app direction items)))))))))))))))))

(def (lsp-symbol-kind->string kind)
  "Convert LSP SymbolKind number to a short string."
  (case kind
    ((1) "File") ((2) "Module") ((3) "Namespace") ((4) "Package")
    ((5) "Class") ((6) "Method") ((7) "Property") ((8) "Field")
    ((9) "Constructor") ((10) "Enum") ((11) "Interface") ((12) "Function")
    ((13) "Variable") ((14) "Constant") ((15) "String") ((16) "Number")
    ((17) "Boolean") ((18) "Array") ((19) "Object") ((20) "Key")
    ((21) "Null") ((22) "EnumMember") ((23) "Struct") ((24) "Event")
    ((25) "Operator") ((26) "TypeParameter")
    (else "Symbol")))

(def (lsp-show-type-hierarchy! app direction items)
  "Display type hierarchy results in a *Type Hierarchy* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf-name "*Type Hierarchy*")
         (existing (buffer-by-name buf-name))
         (buf (or existing (qt-buffer-create! buf-name ed #f)))
         (lines (map (lambda (item)
                       (let* ((name (or (hash-get item "name") "?"))
                              (kind (or (hash-get item "kind") 0))
                              (uri (hash-get item "uri"))
                              (range (hash-get item "range"))
                              (start (and range (hash-get range "start")))
                              (line (and start (hash-get start "line")))
                              (file (and uri (uri->file-path uri))))
                         (string-append
                           "  " (lsp-symbol-kind->string kind) " " name
                           "  (" (or file "?") ":"
                           (number->string (+ 1 (or line 0))) ")")))
                     items))
         (title (string-append "Type Hierarchy (" (symbol->string direction) ")"))
         (text (string-append title " — " (number->string (length items)) " results\n\n"
                 (if (null? lines) "  (none)\n" (string-join lines "\n")))))
    ;; Populate grep results for M-g n/p navigation
    (set! *grep-results*
      (filter-map
        (lambda (item)
          (let* ((uri (hash-get item "uri"))
                 (range (hash-get item "range"))
                 (start (and range (hash-get range "start")))
                 (line (and start (+ 1 (hash-get start "line"))))
                 (file (and uri (uri->file-path uri))))
            (when (and file line)
              (list file line ""))))
        items))
    (set! *grep-result-index* -1)
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append title ": " (number->string (length items)) " results"))))

;;;============================================================================
;;; Auto-completion on idle (Corfu-like)
;;;============================================================================

(def *lsp-auto-complete-enabled* #t)
(def *lsp-auto-complete-min-prefix* 3)

(def (lsp-auto-complete! app)
  "Request LSP completions and show in QCompleter popup if prefix is long enough."
  (when (and *lsp-auto-complete-enabled* (lsp-running?))
    (let* ((fr (app-state-frame app))
           (ed (qt-current-editor fr))
           (buf (qt-current-buffer fr)))
      (when (and buf (buffer-file-path buf)
                 (not (dired-buffer? buf))
                 (not (repl-buffer? buf))
                 (not (terminal-buffer? buf)))
        (let ((prefix (get-word-prefix ed)))
          (when (>= (string-length prefix) *lsp-auto-complete-min-prefix*)
            (let ((params (lsp-current-params app)))
              (when params
                (lsp-send-request! "textDocument/completion" params
                  (lambda (response)
                    (let ((result (hash-get response "result")))
                      (when result
                        (let* ((items (cond
                                        ((list? result) result)
                                        ((and (hash-table? result)
                                              (hash-get result "items"))
                                         (hash-get result "items"))
                                        (else [])))
                               (labels (filter-map
                                         (lambda (item)
                                           (and (hash-table? item)
                                                (hash-get item "label")))
                                         items)))
                          (when (not (null? labels))
                            (let* ((cur-prefix (get-word-prefix ed))
                                   (c (get-or-create-completer! ed app)))
                              (qt-completer-set-model-strings! c labels)
                              (qt-completer-set-completion-prefix! c cur-prefix)
                              (qt-completer-complete-rect! c 0 0 250 20))))))))))))))))
