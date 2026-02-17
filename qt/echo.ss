;;; -*- Gerbil -*-
;;; Qt echo area / minibuffer for gemacs
;;;
;;; Uses a QLabel for displaying messages and an inline QLineEdit
;;; (in the same echo-area row) for minibuffer prompts — no popup dialog.

(export qt-echo-draw!
        qt-echo-read-string
        qt-echo-read-string-with-completion
        qt-echo-read-file-with-completion
        qt-minibuffer-init!)

(import :std/sugar
        :std/sort
        :std/srfi/1
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/qt/window)

;;;============================================================================
;;; Draw the echo area (QLabel)
;;;============================================================================

(def (qt-echo-draw! echo label)
  "Update the echo QLabel with current message."
  (let ((msg (echo-state-message echo)))
    (if msg
      (begin
        (qt-label-set-text! label msg)
        ;; Red text for errors, normal for messages
        (if (echo-state-error? echo)
          (qt-widget-set-style-sheet!
            label "color: #ff4040; background: #282828; font-family: monospace; font-size: 10pt; padding: 2px 4px;")
          (qt-widget-set-style-sheet!
            label "color: #d8d8d8; background: #282828; font-family: monospace; font-size: 10pt; padding: 2px 4px;")))
      (qt-label-set-text! label ""))))

;;;============================================================================
;;; Inline minibuffer state
;;;============================================================================

;; Persistent widgets — created once during app init
(def *mb-container* #f)   ; QWidget wrapper for the prompt + line-edit row
(def *mb-prompt* #f)      ; QLabel showing prompt text
(def *mb-input* #f)       ; QLineEdit for user input
(def *mb-echo-label* #f)  ; Reference to the echo QLabel (to hide/restore)
(def *mb-qt-app* #f)      ; Reference to the Qt application for process-events
(def *mb-editor* #f)      ; Reference to the editor widget (to restore focus)
(def *mb-result* #f)      ; Box: #f = still running, (list text) = accepted, (list) = cancelled
(def *mb-completions* []) ; Stored completions for Tab cycling
(def *mb-tab-idx* 0)      ; Current Tab cycle index
(def *mb-file-mode* #f)   ; When #t, Tab does directory-aware file completion
(def *mb-last-tab-input* "")  ; Track input at last Tab press (for cycle vs new-match detection)

(def *mb-style*
  "QWidget { background: #1e1e1e; border-top: 1px solid #484848; }
   QLabel { color: #b0b0b0; background: transparent; font-family: monospace; font-size: 10pt; padding: 0 4px; }
   QLineEdit { color: #d8d8d8; background: #1e1e1e; border: none; font-family: monospace; font-size: 10pt; padding: 2px 4px; }
   QListView { color: #d8d8d8; background: #282828; border: 1px solid #484848; font-family: monospace; font-size: 10pt; }")

;;;============================================================================
;;; Fuzzy matching and Tab completion logic
;;;============================================================================

(def (fuzzy-file-match? pattern name)
  "Fuzzy match: each character of pattern must appear in order in name.
   Also supports substring matching. Case-insensitive."
  (let ((pl (string-downcase pattern))
        (nl (string-downcase name)))
    (or (string-contains nl pl)  ;; substring match
        ;; fuzzy: chars in order
        (let loop ((pi 0) (ni 0))
          (cond
            ((>= pi (string-length pl)) #t)
            ((>= ni (string-length nl)) #f)
            ((char=? (string-ref pl pi) (string-ref nl ni))
             (loop (+ pi 1) (+ ni 1)))
            (else (loop pi (+ ni 1))))))))

(def (common-prefix strings)
  "Find the longest common prefix of a list of strings (case-insensitive for matching,
   returns the casing from the first string)."
  (if (or (null? strings) (null? (cdr strings)))
    (if (null? strings) "" (car strings))
    (let* ((first (car strings))
           (len (string-length first)))
      (let loop ((i 0))
        (if (>= i len) first
          (let ((ch (char-downcase (string-ref first i))))
            (if (every (lambda (s)
                         (and (> (string-length s) i)
                              (char=? (char-downcase (string-ref s i)) ch)))
                       (cdr strings))
              (loop (+ i 1))
              (substring first 0 i))))))))

(def (list-directory-safe dir)
  "List files in directory, returning empty list on error."
  (with-catch (lambda (e) [])
    (lambda ()
      (sort (directory-files dir) string<?))))

(def (mb-handle-tab! input)
  "Handle Tab press in minibuffer. Supports both regular and file-mode completion."
  (if *mb-file-mode*
    (mb-handle-file-tab! input)
    (mb-handle-regular-tab! input)))

(def (mb-handle-regular-tab! input)
  "Regular Tab completion: fuzzy match and cycle through completions."
  (when (pair? *mb-completions*)
    (let* ((current (qt-line-edit-text input))
           (current-lower (string-downcase current))
           (matches (filter
                      (lambda (c)
                        (fuzzy-file-match? current c))
                      *mb-completions*)))
      (when (pair? matches)
        (if (string=? current *mb-last-tab-input*)
          ;; Same input as last Tab — cycle
          (let ((idx (modulo *mb-tab-idx* (length matches))))
            (qt-line-edit-set-text! input (list-ref matches idx))
            (set! *mb-tab-idx* (+ *mb-tab-idx* 1))
            (set! *mb-last-tab-input* (list-ref matches idx)))
          ;; New input — complete common prefix first
          (let ((prefix (common-prefix matches)))
            (when (> (string-length prefix) (string-length current))
              (qt-line-edit-set-text! input prefix))
            (set! *mb-tab-idx* 0)
            (set! *mb-last-tab-input* (qt-line-edit-text input))))))))

(def (mb-handle-file-tab! input)
  "File-mode Tab completion: directory-aware with fuzzy matching."
  (let* ((current (qt-line-edit-text input))
         (expanded (cond
                     ((and (> (string-length current) 0)
                           (char=? (string-ref current 0) #\~))
                      (string-append (getenv "HOME" "/")
                                     (substring current 1 (string-length current))))
                     (else current)))
         ;; Split EXPANDED path for directory listing
         (last-slash (let loop ((i (- (string-length expanded) 1)))
                       (cond ((< i 0) #f)
                             ((char=? (string-ref expanded i) #\/) i)
                             (else (loop (- i 1))))))
         (dir (if last-slash
                (substring expanded 0 (+ last-slash 1))
                (current-directory)))
         (partial (if last-slash
                    (substring expanded (+ last-slash 1) (string-length expanded))
                    expanded))
         ;; Split ORIGINAL input for display prefix (avoids substring crash with ~)
         (orig-last-slash (let loop ((i (- (string-length current) 1)))
                            (cond ((< i 0) #f)
                                  ((char=? (string-ref current i) #\/) i)
                                  (else (loop (- i 1))))))
         (display-prefix (if orig-last-slash
                           (substring current 0 (+ orig-last-slash 1))
                           ""))
         ;; List files in the directory
         (files (list-directory-safe dir))
         ;; Filter with fuzzy matching
         (matches (if (string=? partial "")
                    files
                    (filter (lambda (f) (fuzzy-file-match? partial f)) files))))
    (when (pair? matches)
      (if (string=? current *mb-last-tab-input*)
        ;; Same input — cycle through matches
        (let* ((idx (modulo *mb-tab-idx* (length matches)))
               (match (list-ref matches idx))
               (full-path (if orig-last-slash
                            (string-append display-prefix match)
                            match)))
          (qt-line-edit-set-text! input full-path)
          (set! *mb-tab-idx* (+ *mb-tab-idx* 1))
          (set! *mb-last-tab-input* full-path))
        ;; New input — complete common prefix
        (let* ((prefix (common-prefix matches))
               (full-prefix (if orig-last-slash
                              (string-append display-prefix prefix)
                              prefix)))
          (when (> (string-length full-prefix) (string-length current))
            (qt-line-edit-set-text! input full-prefix))
          (set! *mb-tab-idx* 0)
          (set! *mb-last-tab-input* (qt-line-edit-text input))
          ;; If exactly one match and it's a directory, append /
          (when (and (= (length matches) 1)
                     (let ((p (string-append dir (car matches))))
                       (and (file-exists? p)
                            (eq? 'directory (file-info-type (file-info p))))))
            (let ((text (qt-line-edit-text input)))
              (unless (string-suffix? "/" text)
                (qt-line-edit-set-text! input (string-append text "/"))
                (set! *mb-last-tab-input* (qt-line-edit-text input))))))))))

;;;============================================================================
;;; Initialize inline minibuffer (called once during app startup)
;;;============================================================================

(def (qt-minibuffer-init! echo-label qt-app parent-layout)
  "Create the inline minibuffer widgets. Call once during app init.
   parent-layout is the main VBox layout that already contains echo-label."
  (let* ((container (qt-widget-create))
         (hlayout (qt-hbox-layout-create container))
         (prompt (qt-label-create ""))
         (input (qt-line-edit-create)))
    (qt-widget-set-style-sheet! container *mb-style*)
    (qt-widget-set-minimum-height! container 28)
    (qt-widget-set-size-policy! container QT_SIZE_PREFERRED QT_SIZE_FIXED)
    (qt-layout-set-margins! hlayout 0 0 0 0)
    (qt-layout-set-spacing! hlayout 0)
    (qt-layout-add-widget! hlayout prompt)
    (qt-layout-add-widget! hlayout input)
    (qt-layout-set-stretch-factor! hlayout prompt 0)
    (qt-layout-set-stretch-factor! hlayout input 1)
    ;; Add after the echo-label in the parent layout
    (qt-layout-add-widget! parent-layout container)
    (qt-layout-set-stretch-factor! parent-layout container 0)
    ;; Initially hidden
    (qt-widget-hide! container)
    ;; Connect Enter signal
    (qt-on-return-pressed! input
      (lambda ()
        (set! *mb-result* (list (qt-line-edit-text input)))))
    ;; Connect key handler for Escape and Tab
    (qt-on-key-press! input
      (lambda ()
        (let ((key (qt-last-key-code)))
          (cond
            ((= key QT_KEY_ESCAPE)
             (set! *mb-result* (list)))
            ((= key QT_KEY_TAB)
             (mb-handle-tab! input))
            (else (void))))))
    ;; Store references
    (set! *mb-container* container)
    (set! *mb-prompt* prompt)
    (set! *mb-input* input)
    (set! *mb-echo-label* echo-label)
    (set! *mb-qt-app* qt-app)))

;;;============================================================================
;;; Read a string via inline minibuffer
;;;============================================================================

(def (qt-echo-read-string app prompt)
  "Show inline minibuffer for input. Returns string or #f if cancelled."
  (let ((fr (app-state-frame app)))
    ;; Set up the minibuffer
    (qt-label-set-text! *mb-prompt* prompt)
    (qt-line-edit-set-text! *mb-input* "")
    ;; Remove any old completer
    (qt-line-edit-set-completer! *mb-input* #f)
    ;; Hide echo label, show minibuffer
    (qt-widget-hide! *mb-echo-label*)
    (qt-widget-show! *mb-container*)
    (qt-widget-set-focus! *mb-input*)
    ;; Blocking event loop
    (set! *mb-result* #f)
    (let loop ()
      (qt-app-process-events! *mb-qt-app*)
      (thread-sleep! 0.01)
      (if *mb-result*
        ;; Done — extract result
        (let ((text (if (pair? *mb-result*)
                      (if (null? *mb-result*) #f   ; Escape → cancelled
                        (let ((t (car *mb-result*)))
                          (if (string=? t "") #f t)))
                      #f)))
          ;; Restore: hide minibuffer, show echo label, refocus editor
          (qt-widget-hide! *mb-container*)
          (qt-widget-show! *mb-echo-label*)
          (let ((ed (qt-current-editor fr)))
            (when ed (qt-widget-set-focus! ed)))
          text)
        (loop)))))

;;;============================================================================
;;; Read a string with completion via inline minibuffer + QCompleter
;;;============================================================================

(def (qt-echo-read-string-with-completion app prompt completions)
  "Show inline minibuffer with QCompleter. Returns string or #f if cancelled."
  (let ((fr (app-state-frame app)))
    ;; Set up the minibuffer
    (qt-label-set-text! *mb-prompt* prompt)
    (qt-line-edit-set-text! *mb-input* "")
    ;; Store completions for Tab cycling
    (set! *mb-completions* completions)
    (set! *mb-tab-idx* 0)
    ;; Attach completer
    (let ((completer (qt-completer-create completions)))
      ;; Note: QT_CASE_INSENSITIVE is 0, which is truthy in Gerbil.
      ;; qt-completer-set-case-sensitivity! takes a boolean (sensitive?),
      ;; so we must pass #f for case-insensitive matching.
      (qt-completer-set-case-sensitivity! completer #f)
      (qt-completer-set-filter-mode! completer QT_MATCH_CONTAINS)
      (qt-completer-set-max-visible-items! completer 15)
      (qt-completer-set-widget! completer *mb-input*)
      (qt-line-edit-set-completer! *mb-input* completer)
      ;; Hide echo label, show minibuffer
      (qt-widget-hide! *mb-echo-label*)
      (qt-widget-show! *mb-container*)
      (qt-widget-set-focus! *mb-input*)
      ;; Blocking event loop
      (set! *mb-result* #f)
      (let loop ()
        (qt-app-process-events! *mb-qt-app*)
        (thread-sleep! 0.01)
        (if *mb-result*
          ;; Done — extract result
          (let ((text (if (pair? *mb-result*)
                        (if (null? *mb-result*) #f   ; Escape → cancelled
                          (let ((t (car *mb-result*)))
                            (if (string=? t "") #f t)))
                        #f)))
            ;; Clean up completer and tab state
            (set! *mb-completions* [])
            (set! *mb-tab-idx* 0)
            (qt-line-edit-set-completer! *mb-input* #f)
            (qt-completer-destroy! completer)
            ;; Restore: hide minibuffer, show echo label, refocus editor
            (qt-widget-hide! *mb-container*)
            (qt-widget-show! *mb-echo-label*)
            (let ((ed (qt-current-editor fr)))
              (when ed (qt-widget-set-focus! ed)))
            text)
          (loop))))))

;;;============================================================================
;;; Read a file path with directory-aware fuzzy completion
;;;============================================================================

(def (qt-echo-read-file-with-completion app prompt)
  "Show inline minibuffer for file path input with directory-aware Tab completion.
   Supports fuzzy matching, directory traversal, and ~ expansion."
  (let ((fr (app-state-frame app)))
    ;; Set up the minibuffer
    (qt-label-set-text! *mb-prompt* prompt)
    (qt-line-edit-set-text! *mb-input* "")
    ;; Enable file-mode Tab completion (no static completions/QCompleter)
    (set! *mb-file-mode* #t)
    (set! *mb-completions* [])
    (set! *mb-tab-idx* 0)
    (set! *mb-last-tab-input* "")
    (qt-line-edit-set-completer! *mb-input* #f)
    ;; Hide echo label, show minibuffer
    (qt-widget-hide! *mb-echo-label*)
    (qt-widget-show! *mb-container*)
    (qt-widget-set-focus! *mb-input*)
    ;; Blocking event loop
    (set! *mb-result* #f)
    (let loop ()
      (qt-app-process-events! *mb-qt-app*)
      (thread-sleep! 0.01)
      (if *mb-result*
        ;; Done — extract result
        (let ((text (if (pair? *mb-result*)
                      (if (null? *mb-result*) #f
                        (let ((t (car *mb-result*)))
                          (if (string=? t "") #f t)))
                      #f)))
          ;; Clean up file-mode state
          (set! *mb-file-mode* #f)
          (set! *mb-completions* [])
          (set! *mb-tab-idx* 0)
          (set! *mb-last-tab-input* "")
          ;; Restore
          (qt-widget-hide! *mb-container*)
          (qt-widget-show! *mb-echo-label*)
          (let ((ed (qt-current-editor fr)))
            (when ed (qt-widget-set-focus! ed)))
          text)
        (loop)))))
