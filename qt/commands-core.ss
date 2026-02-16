;;; -*- Gerbil -*-
;;; Qt commands core - helpers, navigation, editing, window management
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gerbil-emacs/qt/sci-shim
        :gerbil-emacs/core
        :gerbil-emacs/editor
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/terminal
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/persist
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/highlight
        :gerbil-emacs/qt/modeline)

;;; ========================================================================
;;; Winner mode — undo/redo window configuration changes
;;; ========================================================================

(def *winner-history* [])   ; list of (buffer-names . current-idx)
(def *winner-future* [])    ; redo stack
(def *winner-max-history* 50)

(def (winner-current-config fr)
  "Capture current window configuration as list of buffer names + index."
  (cons (map (lambda (w) (buffer-name (qt-edit-window-buffer w)))
             (qt-frame-windows fr))
        (qt-frame-current-idx fr)))

(def (winner-save! fr)
  "Save current window configuration to history."
  (let ((config (winner-current-config fr)))
    (set! *winner-history* (cons config *winner-history*))
    (when (> (length *winner-history*) *winner-max-history*)
      (set! *winner-history* (take *winner-history* *winner-max-history*)))
    (set! *winner-future* [])))

(def (winner-restore-config! app config)
  "Restore a saved window configuration by switching buffers in windows."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (names (car config))
         (saved-idx (cdr config))
         (n (min (length wins) (length names))))
    ;; Restore buffer assignments for existing windows
    (let loop ((ws wins) (ns names) (i 0))
      (when (and (pair? ws) (pair? ns))
        (let* ((w (car ws))
               (target-name (car ns))
               (target-buf (buffer-by-name target-name)))
          (when (and target-buf
                     (not (string=? (buffer-name (qt-edit-window-buffer w))
                                    target-name)))
            (qt-buffer-attach! (qt-edit-window-editor w) target-buf)
            (set! (qt-edit-window-buffer w) target-buf)))
        (loop (cdr ws) (cdr ns) (+ i 1))))
    ;; Restore active window index
    (when (< saved-idx (length wins))
      (set! (qt-frame-current-idx fr) saved-idx))))

(def (cmd-winner-undo app)
  "Undo the last window configuration change."
  (if (null? *winner-history*)
    (echo-error! (app-state-echo app) "No further window configuration to undo")
    (let* ((fr (app-state-frame app))
           (current (winner-current-config fr))
           (prev (car *winner-history*)))
      (set! *winner-future* (cons current *winner-future*))
      (set! *winner-history* (cdr *winner-history*))
      (winner-restore-config! app prev)
      (echo-message! (app-state-echo app) "Window configuration restored"))))

(def (cmd-winner-redo app)
  "Redo a window configuration change."
  (if (null? *winner-future*)
    (echo-error! (app-state-echo app) "No further window configuration to redo")
    (let* ((fr (app-state-frame app))
           (current (winner-current-config fr))
           (next (car *winner-future*)))
      (set! *winner-history* (cons current *winner-history*))
      (set! *winner-future* (cdr *winner-future*))
      (winner-restore-config! app next)
      (echo-message! (app-state-echo app) "Window configuration redone"))))

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (current-qt-editor app)
  (qt-edit-window-editor (qt-current-window (app-state-frame app))))

(def (current-qt-buffer app)
  (qt-edit-window-buffer (qt-current-window (app-state-frame app))))

;; Qt application pointer for clipboard access (set by qt/app.ss at startup)
(def *qt-app-ptr* #f)

;; Tab bar visibility (used by qt/app.ss for the tab bar widget)
(def *tab-bar-visible* #t)

;; Push text to kill ring AND system clipboard
(def (qt-kill-ring-push! app text)
  "Push text onto the kill ring and sync to system clipboard."
  (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
  (when *qt-app-ptr*
    (qt-clipboard-set-text! *qt-app-ptr* text)))

;; Get text from system clipboard (fallback to kill ring top)
(def (qt-clipboard-or-kill-ring app)
  "Get clipboard text, or top of kill ring if clipboard is empty."
  (let ((clip (and *qt-app-ptr*
                   (let ((t (qt-clipboard-text *qt-app-ptr*)))
                     (and (string? t) (> (string-length t) 0) t)))))
    (or clip
        (let ((kr (app-state-kill-ring app)))
          (and (pair? kr) (car kr))))))

;;;============================================================================
;;; Theme system
;;;============================================================================

;; Theme: an alist of named colors
(def *themes* (make-hash-table))
(def *current-theme* 'dark)

(def (define-theme! name colors)
  "Register a theme. COLORS is an alist of (key . value) pairs."
  (hash-put! *themes* name colors))

(def (theme-color key)
  "Get a color value from the current theme."
  (let ((theme (hash-get *themes* *current-theme*)))
    (and theme (let ((pair (assoc key theme)))
                 (and pair (cdr pair))))))

;; Built-in themes
(define-theme! 'dark
  '((bg . "#181818") (fg . "#d8d8d8") (selection . "#404060")
    (modeline-bg . "#282828") (modeline-fg . "#d8d8d8")
    (echo-bg . "#282828") (echo-fg . "#d8d8d8")
    (gutter-bg . "#202020") (gutter-fg . "#8c8c8c")
    (split . "#383838") (tab-bg . "#1e1e1e") (tab-border . "#383838")
    (tab-active-bg . "#404060") (tab-active-fg . "#ffffff")
    (tab-inactive-bg . "#252525") (tab-inactive-fg . "#a0a0a0")))

(define-theme! 'solarized-dark
  '((bg . "#002b36") (fg . "#839496") (selection . "#073642")
    (modeline-bg . "#073642") (modeline-fg . "#93a1a1")
    (echo-bg . "#073642") (echo-fg . "#93a1a1")
    (gutter-bg . "#002b36") (gutter-fg . "#586e75")
    (split . "#073642") (tab-bg . "#002b36") (tab-border . "#073642")
    (tab-active-bg . "#073642") (tab-active-fg . "#fdf6e3")
    (tab-inactive-bg . "#002b36") (tab-inactive-fg . "#586e75")))

(define-theme! 'light
  '((bg . "#fafafa") (fg . "#383838") (selection . "#c0d0e8")
    (modeline-bg . "#e8e8e8") (modeline-fg . "#383838")
    (echo-bg . "#e8e8e8") (echo-fg . "#383838")
    (gutter-bg . "#f0f0f0") (gutter-fg . "#a0a0a0")
    (split . "#d0d0d0") (tab-bg . "#f0f0f0") (tab-border . "#d0d0d0")
    (tab-active-bg . "#c0d0e8") (tab-active-fg . "#000000")
    (tab-inactive-bg . "#f0f0f0") (tab-inactive-fg . "#808080")))

(define-theme! 'monokai
  '((bg . "#272822") (fg . "#f8f8f2") (selection . "#49483e")
    (modeline-bg . "#3e3d32") (modeline-fg . "#f8f8f2")
    (echo-bg . "#3e3d32") (echo-fg . "#f8f8f2")
    (gutter-bg . "#272822") (gutter-fg . "#75715e")
    (split . "#3e3d32") (tab-bg . "#272822") (tab-border . "#3e3d32")
    (tab-active-bg . "#49483e") (tab-active-fg . "#f8f8f2")
    (tab-inactive-bg . "#272822") (tab-inactive-fg . "#75715e")))

(def (theme-stylesheet)
  "Generate a Qt stylesheet from the current theme."
  (let ((bg (or (theme-color 'bg) "#181818"))
        (fg (or (theme-color 'fg) "#d8d8d8"))
        (sel (or (theme-color 'selection) "#404060"))
        (ml-bg (or (theme-color 'modeline-bg) "#282828"))
        (ml-fg (or (theme-color 'modeline-fg) "#d8d8d8"))
        (echo-bg (or (theme-color 'echo-bg) "#282828"))
        (echo-fg (or (theme-color 'echo-fg) "#d8d8d8"))
        (split (or (theme-color 'split) "#383838")))
    (string-append
      "QPlainTextEdit { background-color: " bg "; color: " fg ";"
      " font-family: monospace; font-size: 10pt;"
      " selection-background-color: " sel "; }"
      " QLabel { color: " echo-fg "; background: " echo-bg ";"
      " font-family: monospace; font-size: 10pt; }"
      " QMainWindow { background: " bg "; }"
      " QStatusBar { color: " ml-fg "; background: " ml-bg ";"
      " font-family: monospace; font-size: 10pt; }"
      " QLineEdit { background: " bg "; color: " fg "; border: none;"
      " font-family: monospace; font-size: 10pt; }"
      " QSplitter::handle { background: " split "; }")))

(def (apply-theme! app)
  "Apply the current theme to the Qt application."
  (when *qt-app-ptr*
    (qt-app-set-style-sheet! *qt-app-ptr* (theme-stylesheet))
    ;; Update line number area colors
    (let* ((fr (app-state-frame app))
           (g-bg (theme-color 'gutter-bg))
           (g-fg (theme-color 'gutter-fg)))
      (when (and g-bg g-fg)
        (let ((parse-color (lambda (hex)
                (let ((r (string->number (substring hex 1 3) 16))
                      (g (string->number (substring hex 3 5) 16))
                      (b (string->number (substring hex 5 7) 16)))
                  (values r g b)))))
          (for-each
            (lambda (win)
              (let ((lna (qt-edit-window-line-number-area win)))
                (when lna
                  (let-values (((r g b) (parse-color g-bg)))
                    (qt-line-number-area-set-bg-color! lna r g b))
                  (let-values (((r g b) (parse-color g-fg)))
                    (qt-line-number-area-set-fg-color! lna r g b)))))
            (qt-frame-windows fr))))
    ;; Echo area label styling is handled by the Qt stylesheet above
    )))

;; Auto-save path: #filename# (Emacs convention)
(def (make-auto-save-path path)
  (let* ((dir (path-directory path))
         (name (path-strip-directory path)))
    (path-expand (string-append "#" name "#") dir)))

;; Buffer recency tracking (MRU order for buffer switching)
(def *buffer-recent* [])  ; list of buffer names, most recent first

(def (buffer-touch! buf)
  "Record buffer as most recently used."
  (let ((name (buffer-name buf)))
    (set! *buffer-recent*
      (cons name (filter (lambda (n) (not (string=? n name))) *buffer-recent*)))))

(def (buffer-names-mru)
  "Return buffer names sorted by most recently used, excluding current."
  (let* ((all-names (map buffer-name (buffer-list)))
         ;; Start with MRU order, then append any not yet tracked
         (mru (filter (lambda (n) (member n all-names)) *buffer-recent*))
         (rest (filter (lambda (n) (not (member n mru))) all-names)))
    (append mru rest)))

;; File modification tracking for auto-revert
(def *auto-revert-mode* #t)   ; enabled by default
(def *file-mtimes* (make-hash-table)) ; file-path -> mtime (seconds)
(def *auto-revert-tail-buffers* (make-hash-table)) ; buffer-name -> #t for tail-follow mode

(def (file-mtime path)
  "Get file modification time as seconds, or #f if file doesn't exist."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (time->seconds (file-info-last-modification-time (file-info path))))))

(def (file-mtime-record! path)
  "Record current modification time for a file."
  (when path
    (let ((mt (file-mtime path)))
      (when mt
        (hash-put! *file-mtimes* path mt)))))

(def (file-mtime-changed? path)
  "Check if file has been modified externally since we last recorded it.
Returns #t if changed, #f if not or if no record exists."
  (and path
       (let ((recorded (hash-get *file-mtimes* path))
             (current (file-mtime path)))
         (and recorded current
              (> current recorded)))))

;;;============================================================================
;;; Directory-local variables (.gerbil-emacs-config)
;;;============================================================================

(def *dir-locals-cache* (make-hash-table))  ; dir -> (mtime . alist)

(def (find-dir-locals-file dir)
  "Search DIR and parent directories for .gerbil-emacs-config file."
  (let loop ((d dir))
    (let ((config-path (path-expand ".gerbil-emacs-config" d)))
      (cond
        ((file-exists? config-path) config-path)
        ((string=? d "/") #f)
        (else (loop (path-directory (string-append d "/"))))))))

(def (read-dir-locals file)
  "Read directory-local settings from FILE. Returns alist or #f."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-input-file file
        (lambda (port) (read port))))))


;;;============================================================================
;;; Navigation commands
;;;============================================================================

(def (cmd-forward-char app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_NEXT_CHAR QT_CURSOR_PREVIOUS_CHAR))
        (loop (+ i 1))))))

(def (cmd-backward-char app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_PREVIOUS_CHAR QT_CURSOR_NEXT_CHAR))
        (loop (+ i 1))))))

(def (cmd-next-line app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_DOWN QT_CURSOR_UP))
        (loop (+ i 1))))))

(def (cmd-previous-line app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_UP QT_CURSOR_DOWN))
        (loop (+ i 1))))))

(def (cmd-beginning-of-line app)
  "Smart beginning of line: toggle between first non-whitespace and column 0."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (col (qt-plain-text-edit-cursor-column ed))
         (line (qt-plain-text-edit-cursor-line ed))
         ;; Find start of current line
         (line-start (let loop ((p pos))
                       (if (or (<= p 0)
                               (and (> p 0)
                                    (char=? (string-ref text (- p 1)) #\newline)))
                         p
                         (loop (- p 1)))))
         ;; Find first non-whitespace on line
         (indent-pos (let loop ((p line-start))
                       (if (or (>= p (string-length text))
                               (char=? (string-ref text p) #\newline))
                         line-start  ; all whitespace
                         (if (or (char=? (string-ref text p) #\space)
                                 (char=? (string-ref text p) #\tab))
                           (loop (+ p 1))
                           p)))))
    ;; Toggle: if at indentation, go to column 0; otherwise go to indentation
    (if (= pos indent-pos)
      (qt-plain-text-edit-set-cursor-position! ed line-start)
      (qt-plain-text-edit-set-cursor-position! ed indent-pos))))

(def (cmd-end-of-line app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_END_OF_BLOCK))

(def (cmd-forward-word app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_NEXT_WORD QT_CURSOR_PREVIOUS_WORD))
        (loop (+ i 1))))))

(def (cmd-backward-word app)
  (let ((n (get-prefix-arg app)) (ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i (abs n))
        (qt-plain-text-edit-move-cursor! ed (if (>= n 0) QT_CURSOR_PREVIOUS_WORD QT_CURSOR_NEXT_WORD))
        (loop (+ i 1))))))

;;; Subword movement (camelCase / snake_case boundaries)
(def (subword-boundary? text i direction)
  "Check if position i is a subword boundary in the given direction (1=forward, -1=backward)."
  (let ((len (string-length text)))
    (and (> i 0) (< i len)
         (let ((prev (string-ref text (- i 1)))
               (cur (string-ref text i)))
           (or ;; underscore/hyphen boundary
               (and (= direction 1) (or (char=? cur #\_) (char=? cur #\-)))
               (and (= direction -1) (or (char=? prev #\_) (char=? prev #\-)))
               ;; lowercase -> uppercase (camelCase boundary)
               (and (char-lower-case? prev) (char-upper-case? cur))
               ;; letter -> non-letter or non-letter -> letter
               (and (char-alphabetic? prev) (not (or (char-alphabetic? cur) (char-numeric? cur))))
               (and (not (or (char-alphabetic? prev) (char-numeric? prev))) (char-alphabetic? cur)))))))

(def (cmd-forward-subword app)
  "Move forward by subword (camelCase/snake_case boundary)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i (+ pos 1)))
      (cond
        ((>= i len) (qt-plain-text-edit-set-cursor-position! ed len))
        ((subword-boundary? text i 1) (qt-plain-text-edit-set-cursor-position! ed i))
        (else (loop (+ i 1)))))))

(def (cmd-backward-subword app)
  "Move backward by subword (camelCase/snake_case boundary)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)))
      (cond
        ((<= i 0) (qt-plain-text-edit-set-cursor-position! ed 0))
        ((subword-boundary? text i -1) (qt-plain-text-edit-set-cursor-position! ed i))
        (else (loop (- i 1)))))))

(def (cmd-kill-subword app)
  "Kill forward to the next subword boundary."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i (+ pos 1)))
      (let ((end (cond
                   ((>= i len) len)
                   ((subword-boundary? text i 1) i)
                   (else #f))))
        (if end
          (let ((killed (substring text pos end))
                (new-text (string-append
                            (substring text 0 pos)
                            (substring text end len))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-kill-ring-push! app killed))
          (loop (+ i 1)))))))

(def (cmd-beginning-of-buffer app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_START))

(def (cmd-end-of-buffer app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_END))

(def (cmd-scroll-down app)
  ;; Move down 20 lines to simulate page down
  (let ((ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i 20)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
        (loop (+ i 1))))
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-scroll-up app)
  ;; Move up 20 lines to simulate page up
  (let ((ed (current-qt-editor app)))
    (let loop ((i 0))
      (when (< i 20)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_UP)
        (loop (+ i 1))))
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-recenter app)
  (qt-plain-text-edit-center-cursor! (current-qt-editor app)))

;;;============================================================================
;;; Editing commands
;;;============================================================================

(def (cmd-delete-char app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_NEXT_CHAR
                                     mode: QT_KEEP_ANCHOR)
    (qt-plain-text-edit-remove-selected-text! ed)))

(def (cmd-backward-delete-char app)
  (let ((buf (current-qt-buffer app)))
    (cond
      ;; Terminal buffers: send DEL to PTY (PTY handles echo)
      ((terminal-buffer? buf)
       (let ((ts (hash-get *terminal-state* buf)))
         (when ts (terminal-send-raw! ts "\x7f;"))))
      ;; In REPL buffers, don't delete past the prompt.
      ((repl-buffer? buf)
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed))
              (rs (hash-get *repl-state* buf)))
         (when (and rs (> pos (repl-state-prompt-pos rs)))
           (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                            mode: QT_KEEP_ANCHOR)
           (qt-plain-text-edit-remove-selected-text! ed))))
      ;; Shell: don't delete past the prompt
      ((shell-buffer? buf)
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed))
              (ss (hash-get *shell-state* buf)))
         (when (and ss (> pos (shell-state-prompt-pos ss)))
           (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                            mode: QT_KEEP_ANCHOR)
           (qt-plain-text-edit-remove-selected-text! ed))))
      (else
       (let ((ed (current-qt-editor app)))
         (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                          mode: QT_KEEP_ANCHOR)
         (qt-plain-text-edit-remove-selected-text! ed))))))

(def (cmd-backward-delete-char-untabify app)
  "Delete backward, converting tabs to spaces if in leading whitespace."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (> pos 0)
      (let* ((text (qt-plain-text-edit-text ed))
             (line (qt-plain-text-edit-cursor-line ed))
             ;; Find line start
             (line-start (let loop ((i 0) (ln 0))
                           (if (>= ln line) i
                             (let ((nl (string-index text #\newline i)))
                               (if nl (loop (+ nl 1) (+ ln 1)) i)))))
             (ch-before (if (> pos 0)
                          (string-ref text (- pos 1))
                          #\nul)))
        ;; If char before is tab and we're in leading whitespace
        (if (and (char=? ch-before #\tab)
                 (let loop ((p line-start))
                   (or (>= p pos)
                       (let ((c (string-ref text p)))
                         (and (or (char=? c #\space) (char=? c #\tab))
                              (loop (+ p 1)))))))
          ;; Delete the tab
          (begin
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                              mode: QT_KEEP_ANCHOR)
            (qt-plain-text-edit-remove-selected-text! ed))
          ;; Normal backspace
          (begin
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                              mode: QT_KEEP_ANCHOR)
            (qt-plain-text-edit-remove-selected-text! ed)))))))

(def (cmd-buffer-list-select app)
  "Switch to the buffer named on the current line in *Buffer List*."
  (let* ((ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      "")))
    ;; Line format: "  CM NNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMPATH"
    ;; Name field is 24 chars starting at column 5
    (let* ((name (if (>= (string-length line-text) 29)
                   (string-trim-both (substring line-text 5 29))
                   "")))
      (if (and (> (string-length name) 0)
               (not (string=? name "Buffer"))
               (not (string=? name "------")))
        (let ((buf (buffer-by-name name)))
          (if buf
            (let ((fr (app-state-frame app)))
              ;; Clear read-only before switching — QScintilla read-only
              ;; is document-level but clear explicitly to be safe
              (qt-plain-text-edit-set-read-only! ed #f)
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              ;; Restore default caret line background
              (sci-send ed SCI_SETCARETLINEBACK (rgb->sci #x22 #x22 #x28))
              (echo-message! (app-state-echo app) (buffer-name buf)))
            (echo-error! (app-state-echo app) (string-append "No buffer: " name))))
        (echo-message! (app-state-echo app) "No buffer on this line")))))

(def (current-line-indent ed)
  "Get leading whitespace of the current line."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         ;; Find start of current line
         (line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1) (loop (- i 1))))))
    ;; Extract leading whitespace
    (let loop ((i line-start) (acc []))
      (if (and (< i (string-length text))
               (let ((ch (string-ref text i)))
                 (or (char=? ch #\space) (char=? ch #\tab))))
        (loop (+ i 1) (cons (string-ref text i) acc))
        (list->string (reverse acc))))))


(def (cmd-open-line app)
  (let ((ed (current-qt-editor app)))
    (let ((pos (qt-plain-text-edit-cursor-position ed)))
      (qt-plain-text-edit-insert-text! ed "\n")
      (qt-plain-text-edit-set-cursor-position! ed pos))))

(def (cmd-undo app)
  (let ((ed (current-qt-editor app)))
    (if (qt-plain-text-edit-can-undo? ed)
      (qt-plain-text-edit-undo! ed)
      (echo-message! (app-state-echo app) "No further undo information"))))

(def (cmd-redo app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-redo! ed)))

;;;============================================================================
;;; Kill / Yank
;;;============================================================================

(def (cmd-kill-line app)
  "Kill from point to end of line, or kill newline if at end."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line (qt-plain-text-edit-line-from-position ed pos))
         (line-end (qt-plain-text-edit-line-end-position ed line)))
    (if (= pos line-end)
      ;; At end of line: kill the newline
      (let ((killed (qt-plain-text-edit-text-range ed pos (+ pos 1))))
        (qt-plain-text-edit-set-selection! ed pos (+ pos 1))
        (qt-plain-text-edit-remove-selected-text! ed)
        (when (and (string? killed) (> (string-length killed) 0))
          (qt-kill-ring-push! app killed)))
      ;; Kill to end of line
      (let ((killed (qt-plain-text-edit-text-range ed pos line-end)))
        (qt-plain-text-edit-set-selection! ed pos line-end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (when (and (string? killed) (> (string-length killed) 0))
          (qt-kill-ring-push! app killed))))))

(def (cmd-yank app)
  (let* ((ed (current-qt-editor app))
         (pos-before (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-paste! ed)
    (let ((pos-after (qt-plain-text-edit-cursor-position ed)))
      (set! (app-state-last-yank-pos app) pos-before)
      (set! (app-state-last-yank-len app) (- pos-after pos-before))
      (set! (app-state-kill-ring-idx app) 0))))

;;;============================================================================
;;; Mark and region
;;;============================================================================

(def (cmd-set-mark app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (buf (current-qt-buffer app)))
    ;; Push previous mark to mark ring
    (when (buffer-mark buf)
      (set! (app-state-mark-ring app)
        (cons (cons (buffer-name buf) (buffer-mark buf))
              (app-state-mark-ring app))))
    (set! (buffer-mark buf) pos)
    (echo-message! (app-state-echo app) "Mark set")))

(def (cmd-kill-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (killed (qt-plain-text-edit-text-range ed start end)))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (when (and (string? killed) (> (string-length killed) 0))
          (qt-kill-ring-push! app killed))
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-copy-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text-range ed start end)))
        ;; Push to kill ring + clipboard
        (when (and (string? text) (> (string-length text) 0))
          (qt-kill-ring-push! app text))
        ;; Deselect
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region copied"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; File operations
;;;============================================================================

(def (path-char-delimiter? ch)
  "Check if character is a path delimiter (space, tab, newline, quotes, parens)."
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\newline)
      (char=? ch (integer->char 34))  ; double quote
      (char=? ch (integer->char 39))  ; single quote
      (char=? ch #\()
      (char=? ch #\))))

(def (file-path-at-point ed)
  "Extract a file-path-like string at the cursor position.
Returns (path . line) or #f. Handles file:line format."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (and (< pos len)
         ;; Expand backward to find start of path
         (let* ((start (let scan ((i pos))
                         (if (and (> i 0)
                                  (not (path-char-delimiter? (string-ref text (- i 1)))))
                           (scan (- i 1)) i)))
                ;; Expand forward to find end of path
                (end (let scan ((i pos))
                       (if (and (< i len)
                                (not (path-char-delimiter? (string-ref text i))))
                         (scan (+ i 1)) i)))
                (raw (substring text start end)))
           (and (> (string-length raw) 0)
                ;; Check for file:line format
                (let ((colon-pos (let scan ((i (- (string-length raw) 1)))
                                   (cond
                                     ((< i 0) #f)
                                     ((char=? (string-ref raw i) #\:) i)
                                     ((char-numeric? (string-ref raw i)) (scan (- i 1)))
                                     (else #f)))))
                  (if colon-pos
                    (let* ((path (substring raw 0 colon-pos))
                           (num-str (substring raw (+ colon-pos 1) (string-length raw)))
                           (line-num (string->number num-str)))
                      (if (and line-num (> (string-length path) 0))
                        (cons path line-num)
                        (cons raw #f)))
                    (cons raw #f))))))))


;;;============================================================================
;;; Buffer commands
;;;============================================================================

(def (cmd-switch-buffer app)
  (let* ((echo (app-state-echo app))
         (names (buffer-names-mru))
         (name (qt-echo-read-string-with-completion app "Switch to buffer: " names)))
    (when name
      (let ((buf (buffer-by-name name)))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (buffer-touch! buf)
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf))
          ;; Create new buffer if name doesn't match existing
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (new-buf (qt-buffer-create! name ed #f)))
            (buffer-touch! new-buf)
            (qt-buffer-attach! ed new-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) new-buf)
            (echo-message! echo (string-append "New buffer: " name))))))))


;;;============================================================================
;;; Window commands
;;;============================================================================

(def (cmd-split-window app)
  (winner-save! (app-state-frame app))
  (let ((new-ed (qt-frame-split! (app-state-frame app))))
    ;; Install key handler on the new editor
    (when (app-state-key-handler app)
      ((app-state-key-handler app) new-ed))))

(def (cmd-split-window-right app)
  (winner-save! (app-state-frame app))
  (let ((new-ed (qt-frame-split-right! (app-state-frame app))))
    ;; Install key handler on the new editor
    (when (app-state-key-handler app)
      ((app-state-key-handler app) new-ed))))

(def (cmd-other-window app)
  (qt-frame-other-window! (app-state-frame app)))

(def (cmd-delete-window app)
  (let ((fr (app-state-frame app)))
    (if (> (length (qt-frame-windows fr)) 1)
      (begin
        (winner-save! fr)
        (qt-frame-delete-window! fr))
      (echo-error! (app-state-echo app) "Can't delete sole window"))))

(def (cmd-delete-other-windows app)
  (winner-save! (app-state-frame app))
  (qt-frame-delete-other-windows! (app-state-frame app)))

;;; ace-window — quick window switching by number
(def (cmd-ace-window app)
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (if (<= n 1)
      (echo-message! (app-state-echo app) "Only one window")
      (if (= n 2)
        ;; With only 2 windows, just switch to the other one
        (qt-frame-other-window! fr)
        ;; Show numbered window list and prompt
        (let* ((labels
                (let loop ((ws wins) (i 0) (acc []))
                  (if (null? ws) (reverse acc)
                    (let* ((w (car ws))
                           (bname (buffer-name (qt-edit-window-buffer w)))
                           (marker (if (= i (qt-frame-current-idx fr)) "*" " "))
                           (label (string-append (number->string (+ i 1)) marker ": " bname)))
                      (loop (cdr ws) (+ i 1) (cons label acc))))))
               (prompt-str (string-append "Window [" (string-join labels " | ") "]: "))
               (input (qt-echo-read-string app prompt-str))
               (num (string->number (string-trim input))))
          (cond
            ((not num)
             (echo-error! (app-state-echo app) "Not a number"))
            ((or (< num 1) (> num n))
             (echo-error! (app-state-echo app)
                          (string-append "Window " (number->string num) " does not exist")))
            (else
             (set! (qt-frame-current-idx fr) (- num 1))
             (echo-message! (app-state-echo app)
                            (string-append "Switched to window "
                                           (number->string num))))))))))

;;; Swap window contents
(def (cmd-swap-window app)
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (if (<= n 1)
      (echo-error! (app-state-echo app) "Only one window")
      (let* ((cur-idx (qt-frame-current-idx fr))
             (next-idx (modulo (+ cur-idx 1) n))
             (cur-win (list-ref wins cur-idx))
             (next-win (list-ref wins next-idx))
             (cur-buf (qt-edit-window-buffer cur-win))
             (next-buf (qt-edit-window-buffer next-win)))
        ;; Swap buffers between the two windows
        (set! (qt-edit-window-buffer cur-win) next-buf)
        (set! (qt-edit-window-buffer next-win) cur-buf)
        (qt-buffer-attach! (qt-edit-window-editor cur-win) next-buf)
        (qt-buffer-attach! (qt-edit-window-editor next-win) cur-buf)
        (echo-message! (app-state-echo app) "Windows swapped")))))

;;;============================================================================
;;; Write file (save as)
;;;============================================================================

(def (cmd-write-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Write file: ")))
    (when (and filename (> (string-length filename) 0))
      (let* ((filename (expand-filename filename))
             (buf (current-qt-buffer app))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)))
        (set! (buffer-file-path buf) filename)
        (set! (buffer-name buf) (path-strip-directory filename))
        (write-string-to-file filename text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (echo-message! echo (string-append "Wrote " filename))))))

;;;============================================================================
;;; Revert buffer
;;;============================================================================

(def (cmd-revert-buffer app)
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (and path (file-exists? path))
      (let* ((ed (current-qt-editor app))
             (text (read-file-as-string path)))
        (when text
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (file-mtime-record! path)
          (echo-message! echo (string-append "Reverted " path))))
      (echo-error! echo "Buffer is not visiting a file"))))

;;;============================================================================
;;; Select all
;;;============================================================================

(def (cmd-select-all app)
  (qt-plain-text-edit-select-all! (current-qt-editor app))
  (echo-message! (app-state-echo app) "Mark set (whole buffer)"))

;;;============================================================================
;;; Goto line
;;;============================================================================

(def (cmd-goto-line app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Goto line: ")))
    (when (and input (> (string-length input) 0))
      (let ((line-num (string->number input)))
        (if (and line-num (> line-num 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 ;; Find position of the Nth newline
                 (target-line (- line-num 1))
                 (pos (let loop ((i 0) (line 0))
                        (cond
                          ((= line target-line) i)
                          ((>= i (string-length text)) i)
                          ((char=? (string-ref text i) #\newline)
                           (loop (+ i 1) (+ line 1)))
                          (else (loop (+ i 1) line))))))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            (echo-message! echo (string-append "Line " input)))
          (echo-error! echo "Invalid line number"))))))

;;;============================================================================
;;; M-x (execute extended command)
;;;============================================================================

(def *mx-command-history* [])
(def *mx-history-max* 50)

(def (mx-history-add! name)
  "Add a command name to M-x history (most recent first, no duplicates)."
  (set! *mx-command-history*
    (cons name
      (let loop ((h *mx-command-history*) (acc []))
        (cond
          ((null? h) (reverse acc))
          ((string=? (car h) name) (loop (cdr h) acc))
          (else (loop (cdr h) (cons (car h) acc)))))))
  (when (> (length *mx-command-history*) *mx-history-max*)
    (set! *mx-command-history*
      (let loop ((h *mx-command-history*) (n 0) (acc []))
        (if (or (null? h) (>= n *mx-history-max*))
          (reverse acc)
          (loop (cdr h) (+ n 1) (cons (car h) acc)))))))

(def (cmd-execute-extended-command app)
  (let* ((all-names (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         ;; Put recent commands first, then rest alphabetically
         (recent-set (let loop ((h *mx-command-history*) (s (make-hash-table)))
                       (if (null? h) s
                         (begin (hash-put! s (car h) #t)
                                (loop (cdr h) s)))))
         (non-recent (filter (lambda (n) (not (hash-get recent-set n))) all-names))
         (ordered (append *mx-command-history* non-recent))
         (input (qt-echo-read-string-with-completion app "M-x " ordered)))
    (when (and input (> (string-length input) 0))
      (mx-history-add! input)
      (execute-command! app (string->symbol input)))))

(def (cmd-helm-buffers-list app)
  "Fuzzy buffer switcher — like helm-buffers-list."
  (let* ((names (buffer-names-mru))
         (name (qt-echo-read-string-with-completion app "Buffer: " names)))
    (when (and name (> (string-length name) 0))
      (let ((buf (buffer-by-name name)))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (buffer-touch! buf)
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-modeline-update! app))
          ;; Create new buffer if not found
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (new-buf (qt-buffer-create! name ed #f)))
            (buffer-touch! new-buf)
            (qt-buffer-attach! ed new-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) new-buf)
            (echo-message! (app-state-echo app)
              (string-append "New buffer: " name))))))))

;;;============================================================================
;;; Help commands
;;;============================================================================

(def (cmd-list-bindings app)
  "Display all keybindings in a *Help* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (lines '()))
    (for-each
      (lambda (entry)
        (let ((key (car entry))
              (val (cdr entry)))
          (cond
            ((symbol? val)
             (set! lines (cons (string-append "  " key "\t" (symbol->string val))
                               lines)))
            ((hash-table? val)
             (for-each
               (lambda (sub-entry)
                 (let ((sub-key (car sub-entry))
                       (sub-val (cdr sub-entry)))
                   (when (symbol? sub-val)
                     (set! lines
                       (cons (string-append "  " key " " sub-key "\t"
                                            (symbol->string sub-val))
                             lines)))))
               (keymap-entries val))))))
      (keymap-entries *global-keymap*))
    (let* ((sorted (sort lines string<?))
           (text (string-append "Key Bindings:\n\n"
                                (string-join sorted "\n")
                                "\n")))
      (let ((buf (or (buffer-by-name "*Help*")
                     (qt-buffer-create! "*Help*" ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app) "*Help*")))))

;;;============================================================================
;;; Buffer list
;;;============================================================================

(def (cmd-list-buffers app)
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (cur-buf (current-qt-buffer app))
         (bufs (buffer-list))
         (header "  MR  Buffer                    Mode         File\n  --  ------                    ----         ----\n")
         (lines (map (lambda (buf)
                       (let* ((name (buffer-name buf))
                              (path (or (buffer-file-path buf) ""))
                              (mod (if (qt-text-document-modified?
                                        (buffer-doc-pointer buf)) "*" " "))
                              (cur (if (eq? buf cur-buf) "." " "))
                              (lang (or (buffer-lexer-lang buf) 'fundamental))
                              (mode-str (let ((s (if (symbol? lang)
                                                   (symbol->string lang)
                                                   (if (string? lang) lang "fundamental"))))
                                          (if (> (string-length s) 12)
                                            (substring s 0 12)
                                            s)))
                              ;; Pad name to 24 chars
                              (padded-name (if (>= (string-length name) 24)
                                             (substring name 0 24)
                                             (string-append name
                                               (make-string (- 24 (string-length name)) #\space))))
                              ;; Pad mode to 13 chars
                              (padded-mode (if (>= (string-length mode-str) 13) mode-str
                                             (string-append mode-str
                                               (make-string (- 13 (string-length mode-str)) #\space)))))
                         (string-append "  " cur mod " " padded-name padded-mode path)))
                     bufs))
         (text (string-append header (string-join lines "\n") "\n")))
    (let ((buf (or (buffer-by-name "*Buffer List*")
                   (qt-buffer-create! "*Buffer List*" ed #f))))
      (set! (buffer-lexer-lang buf) 'buffer-list)
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      ;; Clear read-only before setting text — Scintilla's SCI_SETTEXT
      ;; silently fails on read-only documents (from previous invocation)
      (qt-plain-text-edit-set-read-only! ed #f)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (qt-plain-text-edit-set-read-only! ed #t)
      ;; Brighter caret line for buffer-list row selection
      (sci-send ed SCI_SETCARETLINEBACK (rgb->sci #x2a #x2a #x4a))
      (echo-message! (app-state-echo app) "*Buffer List*"))))

;;;============================================================================
;;; What line
;;;============================================================================

(def (cmd-what-line app)
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (total (qt-plain-text-edit-line-count ed)))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string line)
                     " of " (number->string total)))))

;;;============================================================================
;;; Duplicate line
;;;============================================================================

(def (cmd-duplicate-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      "")))
    ;; Insert duplicate after current line
    (let* ((new-lines (let loop ((ls lines) (i 0) (acc []))
                        (if (null? ls)
                          (reverse acc)
                          (if (= i line)
                            (loop (cdr ls) (+ i 1) (cons (car ls) (cons (car ls) acc)))
                            (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
           (new-text (string-join new-lines "\n")))
      (qt-plain-text-edit-set-text! ed new-text)
      ;; Position cursor on the duplicated line by computing position
      (let ((pos (let loop ((i 0) (ln 0))
                   (cond
                     ((= ln (+ line 1)) i)
                     ((>= i (string-length new-text)) i)
                     ((char=? (string-ref new-text i) #\newline)
                      (loop (+ i 1) (+ ln 1)))
                     (else (loop (+ i 1) ln))))))
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

;;;============================================================================
;;; Beginning/end of defun
;;;============================================================================

(def (cmd-beginning-of-defun app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (let loop ((i (- pos 1)))
      (cond
        ((< i 0)
         (qt-plain-text-edit-set-cursor-position! ed 0)
         (echo-message! (app-state-echo app) "Beginning of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (qt-plain-text-edit-set-cursor-position! ed i)
         (qt-plain-text-edit-ensure-cursor-visible! ed))
        (else (loop (- i 1)))))))

(def (cmd-end-of-defun app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (let find-start ((i pos))
      (cond
        ((>= i len)
         (qt-plain-text-edit-set-cursor-position! ed len)
         (echo-message! (app-state-echo app) "End of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (let match ((j (+ i 1)) (depth 1))
           (cond
             ((>= j len)
              (qt-plain-text-edit-set-cursor-position! ed len))
             ((= depth 0)
              (qt-plain-text-edit-set-cursor-position! ed j)
              (qt-plain-text-edit-ensure-cursor-visible! ed))
             ((char=? (string-ref text j) #\() (match (+ j 1) (+ depth 1)))
             ((char=? (string-ref text j) #\)) (match (+ j 1) (- depth 1)))
             (else (match (+ j 1) depth)))))
        (else (find-start (+ i 1)))))))

;;;============================================================================
;;; Tab / indent
;;;============================================================================

;;;----------------------------------------------------------------------------
;;; Autocomplete support
;;;----------------------------------------------------------------------------

;; Per-editor completer
(def *editor-completers* (make-hash-table))

(def (word-char-for-complete? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)
      (char=? ch #\_) (char=? ch #\-) (char=? ch #\!)
      (char=? ch #\?) (char=? ch #\*) (char=? ch #\>)))

(def (get-word-prefix ed)
  "Get the word prefix before the cursor."
  (let* ((pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (let loop ((i (- pos 1)))
      (if (or (< i 0) (not (word-char-for-complete? (string-ref text i))))
        (if (< (+ i 1) pos)
          (substring text (+ i 1) pos)
          "")
        (loop (- i 1))))))

(def (collect-buffer-words text)
  "Collect unique words from buffer text."
  (let ((words (make-hash-table))
        (len (string-length text)))
    (let loop ((i 0))
      (if (>= i len) (hash-keys words)
        (if (word-char-for-complete? (string-ref text i))
          ;; Start of a word
          (let find-end ((j (+ i 1)))
            (if (or (>= j len) (not (word-char-for-complete? (string-ref text j))))
              (begin
                (when (> (- j i) 1) ;; skip single-char words
                  (hash-put! words (substring text i j) #t))
                (loop j))
              (find-end (+ j 1))))
          (loop (+ i 1)))))))

(def (get-or-create-completer! ed app)
  "Get or create a completer for an editor."
  (or (hash-get *editor-completers* ed)
      (let ((c (qt-completer-create [])))
        (qt-completer-set-case-sensitivity! c #f)
        (qt-completer-set-widget! c ed)
        ;; When completion accepted, insert the remaining text
        (qt-on-completer-activated! c
          (lambda (text)
            (let ((prefix (get-word-prefix ed)))
              (when (> (string-length text) (string-length prefix))
                (qt-plain-text-edit-insert-text! ed
                  (substring text (string-length prefix) (string-length text)))))))
        (hash-put! *editor-completers* ed c)
        c)))

;;;============================================================================
;;; Misc commands
;;;============================================================================

(def (text-line-position text line-num)
  "Find the character position of the start of LINE-NUM (1-based) in TEXT."
  (if (<= line-num 1) 0
    (let loop ((i 0) (line 1))
      (cond
        ((>= i (string-length text)) i)
        ((char=? (string-ref text i) #\newline)
         (if (= (+ line 1) line-num)
           (+ i 1)
           (loop (+ i 1) (+ line 1))))
        (else (loop (+ i 1) line))))))

;;;============================================================================

(def (cmd-keyboard-quit app)
  (echo-message! (app-state-echo app) "Quit")
  (set! (app-state-key-state app) (make-initial-key-state)))

;;;============================================================================
;;; Scroll margin commands (Qt)
;;;============================================================================

(def (cmd-set-scroll-margin app)
  "Set the scroll margin (lines to keep visible above/below cursor)."
  (let* ((input (qt-echo-read-string app
                  (string-append "Scroll margin (current "
                                 (number->string *scroll-margin*) "): "))))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (when (and n (>= n 0) (<= n 20))
          (set! *scroll-margin* n)
          (echo-message! (app-state-echo app)
            (string-append "Scroll margin set to " (number->string n))))))))

(def (cmd-toggle-scroll-margin app)
  "Toggle scroll margin between 0 and 3."
  (if (> *scroll-margin* 0)
    (set! *scroll-margin* 0)
    (set! *scroll-margin* 3))
  (echo-message! (app-state-echo app)
    (if (> *scroll-margin* 0)
      (string-append "Scroll margin: " (number->string *scroll-margin*))
      "Scroll margin: off")))

;;;============================================================================
;;; Save-place mode (Qt)
;;;============================================================================

(def (cmd-toggle-save-place-mode app)
  "Toggle save-place mode — remembers cursor position in files."
  (set! *save-place-enabled* (not *save-place-enabled*))
  (echo-message! (app-state-echo app)
    (if *save-place-enabled* "Save-place mode ON" "Save-place mode OFF")))

;;;============================================================================
;;; Require-final-newline (Qt)
;;;============================================================================

(def (cmd-toggle-require-final-newline app)
  "Toggle requiring files to end with a newline on save."
  (set! *require-final-newline* (not *require-final-newline*))
  (echo-message! (app-state-echo app)
    (if *require-final-newline*
      "Require final newline: ON"
      "Require final newline: OFF")))

;;;============================================================================
;;; Centered cursor mode (Qt)
;;;============================================================================

(def (cmd-toggle-centered-cursor-mode app)
  "Toggle centered cursor mode — keeps cursor vertically centered."
  (set! *centered-cursor-mode* (not *centered-cursor-mode*))
  (echo-message! (app-state-echo app)
    (if *centered-cursor-mode*
      "Centered cursor mode ON"
      "Centered cursor mode OFF")))

;;;============================================================================
;;; Tab insertion (Qt)
;;;============================================================================

(def *qt-tab-width* 4)

(def (cmd-tab-to-tab-stop app)
  "Insert spaces (or tab) to the next tab stop."
  (let* ((ed (current-qt-editor app))
         (col (qt-plain-text-edit-cursor-column ed))
         (next-stop (* (+ 1 (quotient col *qt-tab-width*)) *qt-tab-width*))
         (spaces (- next-stop col))
         (str (make-string spaces #\space)))
    (qt-plain-text-edit-insert-text! ed str)))

(def (cmd-set-tab-width app)
  "Set the tab width for the current buffer."
  (let* ((input (qt-echo-read-string app
                  (string-append "Tab width (" (number->string *qt-tab-width*) "): "))))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (if (and n (exact? n) (> n 0) (<= n 16))
          (begin
            (set! *qt-tab-width* n)
            (echo-message! (app-state-echo app)
              (string-append "Tab width: " (number->string n))))
          (echo-error! (app-state-echo app) "Invalid tab width (1-16)"))))))


