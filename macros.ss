;;; macros.ss — Project-wide macros for boilerplate reduction
;;;
;;; This file defines all macros used across the gemacs codebase.
;;; IMPORTANT: This file must NOT import any :gemacs/... modules to avoid circular dependencies.
;;; It uses only bare Gerbil primitives.

(export #t)

;; Dummy runtime definition to avoid macro-only compilation issues
(def __gemacs-macros-loaded__ #t)

;;; ============================================================================
;;; Pattern 1: register-commands! — Command registration
;;; ============================================================================

;; TODO: Implement this macro - currently too complex for syntax-case
;; For now, we'll manually refactor command registration

;;; ============================================================================
;;; Pattern 2: bind-keys! — Keymap binding shorthand
;;; ============================================================================

(define-syntax bind-keys!
  (syntax-rules ()
    ((_ keymap (key cmd) ...)
     (begin (keymap-bind! keymap key 'cmd) ...))))

;;; ============================================================================
;;; Pattern 3: with-qt-editor — Qt state extraction
;;; ============================================================================

;; Most specific patterns first (more bindings)
(define-syntax with-qt-editor
  (syntax-rules ()
    ((_ app (ed text pos) body ...)
     (let* ((ed (current-qt-editor app))
            (text (qt-plain-text-edit-text ed))
            (pos (qt-plain-text-edit-cursor-position ed)))
       body ...))

    ((_ app (ed buf mark) body ...)
     (let* ((ed (current-qt-editor app))
            (buf (current-qt-buffer app))
            (mark (buffer-mark buf)))
       body ...))

    ((_ app (ed text) body ...)
     (let* ((ed (current-qt-editor app))
            (text (qt-plain-text-edit-text ed)))
       body ...))

    ((_ app (ed pos) body ...)
     (let* ((ed (current-qt-editor app))
            (pos (qt-plain-text-edit-cursor-position ed)))
       body ...))

    ((_ app (ed buf) body ...)
     (let* ((ed (current-qt-editor app))
            (buf (current-qt-buffer app)))
       body ...))

    ((_ app (ed) body ...)
     (let ((ed (current-qt-editor app))) body ...))))

;;; ============================================================================
;;; Pattern 3: with-editor — TUI state extraction
;;; ============================================================================

;; Most specific patterns first (more bindings)
(define-syntax with-editor
  (syntax-rules ()
    ((_ app (ed text pos) body ...)
     (let* ((ed (current-editor app))
            (text (editor-get-text ed))
            (pos (editor-get-current-pos ed)))
       body ...))

    ((_ app (ed buf mark) body ...)
     (let* ((ed (current-editor app))
            (buf (current-buffer-from-app app))
            (mark (buffer-mark buf)))
       body ...))

    ((_ app (ed text) body ...)
     (let* ((ed (current-editor app))
            (text (editor-get-text ed)))
       body ...))

    ((_ app (ed pos) body ...)
     (let* ((ed (current-editor app))
            (pos (editor-get-current-pos ed)))
       body ...))

    ((_ app (ed buf) body ...)
     (let* ((ed (current-editor app))
            (buf (current-buffer-from-app app)))
       body ...))

    ((_ app (ed) body ...)
     (let ((ed (current-editor app))) body ...))))

;;; ============================================================================
;;; Pattern 4: msg! / err! — Echo shorthand
;;; ============================================================================

(define-syntax msg!
  (syntax-rules ()
    ((_ app text args ...)
     (echo-message! (app-state-echo app) text args ...))
    ((_ app text)
     (echo-message! (app-state-echo app) text))))

(define-syntax err!
  (syntax-rules ()
    ((_ app text args ...)
     (echo-error! (app-state-echo app) text args ...))
    ((_ app text)
     (echo-error! (app-state-echo app) text))))

;;; ============================================================================
;;; Pattern 5: def-toggle — Boolean toggle commands
;;; ============================================================================

(define-syntax def-toggle
  (syntax-rules ()
    ((_ name var label)
     (def (name app)
       (set! var (not var))
       (msg! app (if var (string-append label " ON") (string-append label " OFF")))))))

;;; ============================================================================
;;; Pattern 6: def-toggle-mode — toggle-mode! commands
;;; ============================================================================

(define-syntax def-toggle-mode
  (syntax-rules ()
    ;; Simple case: "Label: on" / "Label: off"
    ((_ name mode-sym label)
     (def (name app)
       (let ((on (toggle-mode! 'mode-sym)))
         (msg! app (if on (string-append label ": on") (string-append label ": off"))))))

    ;; Custom messages case
    ((_ name mode-sym on-msg off-msg)
     (def (name app)
       (let ((on (toggle-mode! 'mode-sym)))
         (msg! app (if on on-msg off-msg)))))))

;;; ============================================================================
;;; Pattern 7: defalias — Delegation commands
;;; ============================================================================

(define-syntax defalias
  (syntax-rules ()
    ((_ new-name target)
     (def (new-name app) (target app)))))

;;; ============================================================================
;;; Pattern 8: defstub — Message-only commands
;;; ============================================================================

(define-syntax defstub
  (syntax-rules ()
    ((_ name message)
     (def (name app) (msg! app message)))))

;;; ============================================================================
;;; Pattern 9: set-face-style! — Highlight macros
;;; ============================================================================

;; Set foreground from face for a single style ID
(define-syntax set-face-style!
  (syntax-rules ()
    ((_ ed face-name style-id)
     (let-values (((r g b) (face-fg-rgb 'face-name)))
       (sci-send ed SCI_STYLESETFORE style-id (rgb->sci r g b))))))

;; Set foreground + bold from face
(define-syntax set-face-style/bold!
  (syntax-rules ()
    ((_ ed face-name style-id)
     (let-values (((r g b) (face-fg-rgb 'face-name)))
       (sci-send ed SCI_STYLESETFORE style-id (rgb->sci r g b))
       (when (face-has-bold? 'face-name)
         (sci-send ed SCI_STYLESETBOLD style-id 1))))))

;; Set foreground + italic from face
(define-syntax set-face-style/italic!
  (syntax-rules ()
    ((_ ed face-name style-id)
     (let-values (((r g b) (face-fg-rgb 'face-name)))
       (sci-send ed SCI_STYLESETFORE style-id (rgb->sci r g b))
       (when (face-has-italic? 'face-name)
         (sci-send ed SCI_STYLESETITALIC style-id 1))))))

;; Set foreground for multiple style IDs
(define-syntax set-face-styles!
  (syntax-rules ()
    ((_ ed face-name style-id ...)
     (let-values (((r g b) (face-fg-rgb 'face-name)))
       (for-each (lambda (s) (sci-send ed SCI_STYLESETFORE s (rgb->sci r g b)))
                 (list style-id ...))))))

;; Set foreground + italic for multiple style IDs
(define-syntax set-face-styles/italic!
  (syntax-rules ()
    ((_ ed face-name style-id ...)
     (let-values (((r g b) (face-fg-rgb 'face-name)))
       (for-each (lambda (s)
                   (sci-send ed SCI_STYLESETFORE s (rgb->sci r g b))
                   (when (face-has-italic? 'face-name)
                     (sci-send ed SCI_STYLESETITALIC s 1)))
                 (list style-id ...))))))

;; Set foreground + bold for multiple style IDs
(define-syntax set-face-styles/bold!
  (syntax-rules ()
    ((_ ed face-name style-id ...)
     (let-values (((r g b) (face-fg-rgb 'face-name)))
       (for-each (lambda (s)
                   (sci-send ed SCI_STYLESETFORE s (rgb->sci r g b))
                   (when (face-has-bold? 'face-name)
                     (sci-send ed SCI_STYLESETBOLD s 1)))
                 (list style-id ...))))))

;;; ============================================================================
;;; Pattern 10: define-mode-keymap! — Mode keymap definition
;;; ============================================================================

(define-syntax define-mode-keymap!
  (syntax-rules ()
    ((_ mode-name (key cmd) ...)
     (let ((km (make-keymap)))
       (keymap-bind! km key 'cmd) ...
       (hash-put! *mode-keymaps* 'mode-name km)))))

;;; ============================================================================
;;; Pattern 11: with-qt-region / with-region — Region transform commands
;;; ============================================================================

;; Qt version: extract region, apply transform, replace
(define-syntax with-qt-region
  (syntax-rules ()
    ((_ app (ed text start end region) body ...)
     (let* ((ed (current-qt-editor app))
            (buf (current-qt-buffer app))
            (mark (buffer-mark buf)))
       (if (not mark)
         (err! app "No mark set")
         (let* ((pos (qt-plain-text-edit-cursor-position ed))
                (start (min mark pos))
                (end (max mark pos))
                (text (qt-plain-text-edit-text ed))
                (region (substring text start end)))
           body ...))))))

;; TUI version
(define-syntax with-region
  (syntax-rules ()
    ((_ app (ed text start end region) body ...)
     (let* ((ed (current-editor app))
            (buf (current-buffer-from-app app))
            (mark (buffer-mark buf)))
       (if (not mark)
         (err! app "No mark set")
         (let* ((pos (editor-get-current-pos ed))
                (start (min mark pos))
                (end (max mark pos))
                (text (editor-get-text ed))
                (region (substring text start end)))
           body ...))))))

;;; ============================================================================
;;; Pattern 12: def-insert-template — Template insertion
;;; ============================================================================

(define-syntax def-insert-template
  (syntax-rules ()
    ((_ name template cursor-offset)
     (def (name app)
       (with-qt-editor app (ed pos)
         (qt-plain-text-edit-insert-text! ed template)
         (qt-plain-text-edit-set-cursor-position! ed (+ pos cursor-offset)))))))

;;; ============================================================================
;;; Pattern 13: def-sci-wrapper — Scintilla wrapper macros
;;; ============================================================================

(define-syntax def-sci-getter
  (syntax-rules ()
    ((_ name msg)
     (def (name sci) (sci-send sci msg)))))

(define-syntax def-sci-cmd!
  (syntax-rules ()
    ((_ name msg)
     (def (name sci) (sci-send sci msg)))))

(define-syntax def-sci-setter!
  (syntax-rules ()
    ((_ name msg)
     (def (name sci val) (sci-send sci msg val)))))

;;; ============================================================================
;;; Pattern 14: def-word-transform — Word case commands
;;; ============================================================================

(define-syntax def-word-transform
  (syntax-rules ()
    ((_ name transform-fn)
     (def (name app)
       (let ((ed (current-qt-editor app)))
         (let-values (((start end) (qt-word-at-point ed)))
           (when start
             (let* ((text (qt-plain-text-edit-text ed))
                    (word (substring text start end))
                    (transformed (transform-fn word))
                    (new-text (string-append (substring text 0 start) transformed
                                             (substring text end (string-length text)))))
               (qt-plain-text-edit-set-text! ed new-text)
               (qt-plain-text-edit-set-cursor-position! ed end)))))))))

;;; ============================================================================
;;; Pattern 15: def-insert-pair — Delimiter pair insertion
;;; ============================================================================

;; TUI version
(define-syntax def-insert-pair
  (syntax-rules ()
    ((_ name open-close)
     (def (name app)
       (with-editor app (ed pos)
         (editor-insert-text ed pos open-close)
         (editor-goto-pos ed (+ pos 1)))))))
