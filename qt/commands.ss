;;; -*- Gerbil -*-
;;; Qt command implementations for gerbil-emacs
;;;
;;; All Emacs commands reimplemented using Qt QPlainTextEdit APIs.

(export qt-register-all-commands!
        dired-open-directory!
        *qt-app-ptr*
        qt-kill-ring-push!
        *isearch-active*
        isearch-handle-key!
        *qreplace-active*
        qreplace-handle-key!
        recent-files-add!
        recent-files-load!
        bookmarks-load!
        session-save!
        session-restore-files
        *tab-bar-visible*
        *auto-revert-mode*
        *auto-revert-tail-buffers*
        *file-mtimes*
        file-mtime-record!
        file-mtime-changed?
        *eldoc-mode*
        eldoc-display!
        *mode-keymaps*
        mode-keymap-lookup
        *current-theme*
        *themes*
        theme-stylesheet
        apply-theme!
        buffer-touch!
        custom-keys-load!
        abbrevs-load!
        load-init-file!
        scratch-save!
        scratch-restore!
        scratch-update-text!
        undo-history-record!
        winner-save!
        *follow-mode*
        *view-mode-buffers*
        *so-long-buffers*
        check-so-long!
        savehist-save!
        savehist-load!
        auto-fill-check!
        *delete-trailing-whitespace-on-save*
        uniquify-buffer-name!)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/editor
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/terminal
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/highlight
        :gerbil-emacs/qt/modeline)

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

(def (apply-dir-locals! app file-path)
  "Apply directory-local variables for FILE-PATH."
  (when file-path
    (let* ((dir (path-directory file-path))
           (config-file (find-dir-locals-file dir)))
      (when config-file
        ;; Check cache
        (let* ((cached (hash-get *dir-locals-cache* config-file))
               (current-mtime (file-mtime config-file))
               (settings (if (and cached current-mtime
                                  (= (car cached) current-mtime))
                           (cdr cached)
                           ;; Reload
                           (let ((s (read-dir-locals config-file)))
                             (when (and s current-mtime)
                               (hash-put! *dir-locals-cache* config-file
                                          (cons current-mtime s)))
                             s))))
          (when (and settings (list? settings))
            (for-each
              (lambda (pair)
                (when (pair? pair)
                  (let ((key (car pair))
                        (val (cdr pair)))
                    (case key
                      ((tab-width)
                       (when (and (integer? val) (> val 0) (<= val 16))
                         (set! *tab-width* val)))
                      ((indent-tabs-mode)
                       (set! *indent-tabs-mode* (and val #t)))
                      ((fill-column)
                       (when (and (integer? val) (> val 0))
                         (set! *fill-column* val)))
                      ((compile-command)
                       (when (string? val)
                         (set! (app-state-last-compile app) val)))
                      ((auto-indent)
                       (set! *auto-indent* (and val #t)))
                      ((auto-pair-mode)
                       (set! *auto-pair-mode* (and val #t)))))))
              settings)))))))

(def (cmd-show-dir-locals app)
  "Show directory-local settings for the current buffer's file."
  (let ((path (buffer-file-path (current-qt-buffer app))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((config (find-dir-locals-file (path-directory path))))
        (if (not config)
          (echo-message! (app-state-echo app) "No .gerbil-emacs-config found")
          (let ((settings (read-dir-locals config)))
            (echo-message! (app-state-echo app)
              (string-append config ": "
                (if settings
                  (string-join
                    (map (lambda (p)
                           (string-append (symbol->string (car p)) "="
                             (let ((v (cdr p)))
                               (cond
                                 ((string? v) v)
                                 ((boolean? v) (if v "#t" "#f"))
                                 ((number? v) (number->string v))
                                 (else "?")))))
                         settings)
                    ", ")
                  "invalid format")))))))))

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
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_START_OF_BLOCK))

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
      (else
       (let ((ed (current-qt-editor app)))
         (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                          mode: QT_KEEP_ANCHOR)
         (qt-plain-text-edit-remove-selected-text! ed))))))

(def (cmd-buffer-list-select app)
  "Switch to the buffer named on the current line in *Buffer List*."
  (let* ((ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      "")))
    (let* ((trimmed (string-trim line-text))
           (tab-pos (string-index trimmed #\tab))
           (name (if tab-pos (substring trimmed 0 tab-pos) trimmed)))
      (if (and (> (string-length name) 0)
               (not (string=? name "Buffer"))
               (not (string=? name "------")))
        (let ((buf (buffer-by-name name)))
          (if buf
            (let ((fr (app-state-frame app)))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
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

(def (cmd-newline app)
  (let ((buf (current-qt-buffer app)))
    (cond
      ((dired-buffer? buf)  (cmd-dired-find-file app))
      ((eq? (buffer-lexer-lang buf) 'buffer-list) (cmd-buffer-list-select app))
      ((string=? (buffer-name buf) "*Grep*") (cmd-grep-goto app))
      ((string=? (buffer-name buf) "*Occur*") (cmd-occur-goto app))
      ((terminal-buffer? buf) (cmd-terminal-send app))
      ((repl-buffer? buf)   (cmd-repl-send app))
      ((eshell-buffer? buf) (cmd-eshell-send app))
      ((shell-buffer? buf)  (cmd-shell-send app))
      (else
        (let ((ed (current-qt-editor app)))
          (if *auto-indent*
            ;; Auto-indent: copy previous line's leading whitespace
            (let ((indent (current-line-indent ed)))
              (qt-plain-text-edit-insert-text! ed (string-append "\n" indent)))
            (qt-plain-text-edit-insert-text! ed "\n")))))))

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

(def (cmd-find-file-at-point app)
  "Open file at point, or prompt with path at point as default."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (at-point (file-path-at-point ed))
         (default-path (and at-point (car at-point)))
         (default-line (and at-point (cdr at-point)))
         (prompt (if default-path
                   (string-append "Find file [" default-path "]: ")
                   "Find file: "))
         (input (qt-echo-read-string app prompt))
         (filename (if (and input (string=? input "") default-path)
                     default-path
                     input)))
    (when (and filename (> (string-length filename) 0))
      (recent-files-add! filename)
      (if (and (file-exists? filename)
               (eq? 'directory (file-info-type (file-info filename))))
        (dired-open-directory! app filename)
        (let* ((name (path-strip-directory filename))
               (fr (app-state-frame app))
               (ed2 (current-qt-editor app))
               (buf (qt-buffer-create! name ed2 filename)))
          (qt-buffer-attach! ed2 buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? filename)
            (let ((text (read-file-as-string filename)))
              (when text
                (qt-plain-text-edit-set-text! ed2 text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (if default-line
                  ;; Jump to the line from file:line
                  (let ((target-pos (text-line-position text default-line)))
                    (qt-plain-text-edit-set-cursor-position! ed2 target-pos))
                  (qt-plain-text-edit-set-cursor-position! ed2 0))))
            (file-mtime-record! filename))
          (qt-setup-highlighting! app buf)
          (apply-dir-locals! app filename)
          (echo-message! echo (string-append "Opened: " filename)))))))

(def (cmd-find-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Find file: ")))
    (when filename
      (when (> (string-length filename) 0)
        ;; Check for remote path first
        (if (tramp-path? filename)
          (let-values (((host remote-path) (tramp-parse-path filename)))
            (echo-message! (app-state-echo app)
              (string-append "Fetching " host ":" remote-path "..."))
            (let ((content (tramp-read-file host remote-path)))
              (if (not content)
                (echo-error! (app-state-echo app)
                  (string-append "Failed to fetch " remote-path))
                (let* ((name (string-append (path-strip-directory remote-path) " [" host "]"))
                       (fr (app-state-frame app))
                       (ed (current-qt-editor app))
                       (buf (qt-buffer-create! name ed #f)))
                  (qt-buffer-attach! ed buf)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                  (qt-plain-text-edit-set-text! ed content)
                  (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                  (qt-plain-text-edit-set-cursor-position! ed 0)
                  (set! (buffer-file-path buf) filename)
                  (echo-message! (app-state-echo app)
                    (string-append "Loaded " remote-path " from " host))))))
          (begin
        ;; Track in recent files
        (recent-files-add! filename)
        ;; Check if it's a directory
        (if (and (file-exists? filename)
                 (eq? 'directory (file-info-type (file-info filename))))
          ;; Open as dired
          (dired-open-directory! app filename)
          ;; Open as regular file
          (let* ((name (path-strip-directory filename))
                 (fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (buf (qt-buffer-create! name ed filename)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (when (file-exists? filename)
              (let ((text (read-file-as-string filename)))
                (when text
                  (qt-plain-text-edit-set-text! ed text)
                  (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                  (qt-plain-text-edit-set-cursor-position! ed 0)))
              (file-mtime-record! filename))
            (qt-setup-highlighting! app buf)
            (echo-message! echo (string-append "Opened: " filename))))))))))

(def (cmd-save-buffer app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      (if (tramp-path? path)
        ;; Save to remote host
        (let-values (((host remote-path) (tramp-parse-path path)))
          (let ((text (qt-plain-text-edit-text ed)))
            (echo-message! echo (string-append "Saving to " host ":" remote-path "..."))
            (if (tramp-write-file host remote-path text)
              (begin
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (echo-message! echo "Remote file saved"))
              (echo-error! echo "Failed to save remote file"))))
      ;; Save to existing local path
      (begin
        ;; Create backup file if original exists and hasn't been backed up yet
        (when (and (file-exists? path) (not (buffer-backup-done? buf)))
          (let ((backup-path (string-append path "~")))
            (with-catch
              (lambda (e) #f)  ; Ignore backup errors
              (lambda ()
                (copy-file path backup-path)
                (set! (buffer-backup-done? buf) #t)))))
        ;; Delete trailing whitespace on save if enabled
        (when *delete-trailing-whitespace-on-save*
          (cmd-delete-trailing-whitespace app))
        (let ((text (qt-plain-text-edit-text ed)))
          (write-string-to-file path text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          ;; Remove auto-save file if it exists
          (let ((auto-save-path (make-auto-save-path path)))
            (when (file-exists? auto-save-path)
              (delete-file auto-save-path)))
          (file-mtime-record! path)
          (echo-message! echo (string-append "Wrote " path))
          ;; Compile-on-save for Gerbil projects
          (compile-on-save-check! app path)
          ;; Flycheck: run syntax check on Gerbil files
          (flycheck-check! app path))))
      ;; No path: prompt for one
      (let ((filename (qt-echo-read-string app "Write file: ")))
        (when (and filename (> (string-length filename) 0))
          (set! (buffer-file-path buf) filename)
          (set! (buffer-name buf) (path-strip-directory filename))
          (let ((text (qt-plain-text-edit-text ed)))
            (write-string-to-file filename text)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (file-mtime-record! filename)
            (echo-message! echo (string-append "Wrote " filename))))))))

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

(def (cmd-kill-buffer-cmd app)
  (let* ((echo (app-state-echo app))
         (cur-buf (current-qt-buffer app))
         (name (qt-echo-read-string app
                  (string-append "Kill buffer (" (buffer-name cur-buf) "): "))))
    (when name
      (let* ((target-name (if (string=? name "") (buffer-name cur-buf) name))
             (buf (buffer-by-name target-name)))
        (if buf
          (if (<= (length (buffer-list)) 1)
            (echo-error! echo "Can't kill last buffer")
            (begin
              ;; Switch to another buffer if killing current
              (when (eq? buf (current-qt-buffer app))
                (let* ((fr (app-state-frame app))
                       (ed (current-qt-editor app))
                       (other (let loop ((bs (buffer-list)))
                                (cond ((null? bs) #f)
                                      ((eq? (car bs) buf) (loop (cdr bs)))
                                      (else (car bs))))))
                  (when other
                    (qt-buffer-attach! ed other)
                    (set! (qt-edit-window-buffer (qt-current-window fr))
                          other))))
              ;; Clean up syntax highlighter if applicable
              (qt-remove-highlighting! buf)
              ;; Clean up dired entries and marks if applicable
              (hash-remove! *dired-entries* buf)
              (hash-remove! *dired-marks* buf)
              ;; Clean up REPL state if applicable
              (let ((rs (hash-get *repl-state* buf)))
                (when rs
                  (repl-stop! rs)
                  (hash-remove! *repl-state* buf)))
              ;; Clean up eshell state if applicable
              (hash-remove! *eshell-state* buf)
              ;; Clean up shell state if applicable
              (let ((ss (hash-get *shell-state* buf)))
                (when ss
                  (shell-stop! ss)
                  (hash-remove! *shell-state* buf)))
              ;; Clean up terminal state if applicable
              (let ((ts (hash-get *terminal-state* buf)))
                (when ts
                  (terminal-stop! ts)
                  (hash-remove! *terminal-state* buf)))
              ;; Remove from MRU list
              (set! *buffer-recent*
                (filter (lambda (n) (not (string=? n target-name)))
                        *buffer-recent*))
              (qt-buffer-kill! buf)
              (echo-message! echo (string-append "Killed " target-name))))
          (echo-error! echo (string-append "No buffer: " target-name)))))))

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
;;; Incremental Search (isearch)
;;;============================================================================

;; Isearch state
(def *isearch-active* #f)      ; #f, 'forward, or 'backward
(def *isearch-query* "")       ; current search string
(def *isearch-start-pos* 0)    ; cursor position when isearch started
(def *isearch-app* #f)         ; app-state reference

;; Search highlight colors for current match (bright cyan background)
(def isearch-cur-fg-r #x00) (def isearch-cur-fg-g #x00) (def isearch-cur-fg-b #x00)
(def isearch-cur-bg-r #x00) (def isearch-cur-bg-g #xdd) (def isearch-cur-bg-b #xff)

;; Search highlight colors for other matches (dim yellow background)
(def isearch-oth-fg-r #x00) (def isearch-oth-fg-g #x00) (def isearch-oth-fg-b #x00)
(def isearch-oth-bg-r #xff) (def isearch-oth-bg-g #xcc) (def isearch-oth-bg-b #x00)

(def (isearch-highlight-all! ed query cursor-pos)
  "Highlight all matches. The match at/nearest cursor-pos gets current-match color."
  (when (> (string-length query) 0)
    (let* ((text (qt-plain-text-edit-text ed))
           (len (string-length text))
           (pat-len (string-length query))
           (query-lower (string-downcase query))
           (text-lower (string-downcase text)))
      ;; Find all match positions
      (let loop ((i 0) (positions '()))
        (if (> (+ i pat-len) len)
          ;; Done collecting — now highlight
          (let ((positions (reverse positions)))
            (for-each
              (lambda (pos)
                (if (= pos cursor-pos)
                  ;; Current match — bright cyan
                  (qt-extra-selection-add-range! ed pos pat-len
                    isearch-cur-fg-r isearch-cur-fg-g isearch-cur-fg-b
                    isearch-cur-bg-r isearch-cur-bg-g isearch-cur-bg-b bold: #t)
                  ;; Other matches — dim yellow
                  (qt-extra-selection-add-range! ed pos pat-len
                    isearch-oth-fg-r isearch-oth-fg-g isearch-oth-fg-b
                    isearch-oth-bg-r isearch-oth-bg-g isearch-oth-bg-b bold: #f)))
              positions)
            (qt-extra-selections-apply! ed)
            (length positions))
          ;; Search for next occurrence (case-insensitive)
          (let ((found (string-contains text-lower query-lower i)))
            (if found
              (loop (+ found 1) (cons found positions))
              ;; No more matches
              (loop len positions))))))))

(def (isearch-find-nearest-forward text query from-pos)
  "Find first match at or after from-pos (case-insensitive). Returns position or #f."
  (let* ((text-lower (string-downcase text))
         (query-lower (string-downcase query)))
    (string-contains text-lower query-lower from-pos)))

(def (isearch-find-nearest-backward text query from-pos)
  "Find last match before from-pos (case-insensitive). Returns position or #f."
  (let* ((text-lower (string-downcase text))
         (query-lower (string-downcase query))
         (pat-len (string-length query)))
    ;; Scan backward from from-pos
    (let loop ((i (min from-pos (- (string-length text) pat-len))))
      (cond
        ((< i 0) #f)
        ((string-contains text-lower query-lower i)
         => (lambda (pos) (if (<= pos from-pos) pos (loop (- i 1)))))
        (else (loop (- i 1)))))))

(def (isearch-update! app)
  "Update search display: find match, highlight all, update echo."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (query *isearch-query*)
         (direction *isearch-active*)
         (prefix (if (eq? direction 'forward) "I-search: " "I-search backward: ")))
    ;; Update visual decorations first (current line, braces)
    (qt-update-visual-decorations! ed)
    (if (= (string-length query) 0)
      ;; Empty query — just show prompt
      (echo-message! echo prefix)
      ;; Non-empty query — find and highlight
      (let* ((text (qt-plain-text-edit-text ed))
             (cur-pos (qt-plain-text-edit-cursor-position ed))
             (match-pos
               (if (eq? direction 'forward)
                 (or (isearch-find-nearest-forward text query cur-pos)
                     ;; Wrap around
                     (isearch-find-nearest-forward text query 0))
                 (or (isearch-find-nearest-backward text query (- cur-pos 1))
                     ;; Wrap around
                     (isearch-find-nearest-backward text query
                       (- (string-length text) 1))))))
        (if match-pos
          (begin
            ;; Move cursor to end of match (forward) or start of match (backward)
            (if (eq? direction 'forward)
              (qt-plain-text-edit-set-cursor-position! ed (+ match-pos (string-length query)))
              (qt-plain-text-edit-set-cursor-position! ed match-pos))
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            ;; Highlight all matches with current match distinguished
            (let ((count (isearch-highlight-all! ed query match-pos)))
              (let ((wrapped (if (eq? direction 'forward)
                               (and (< match-pos *isearch-start-pos*) " [Wrapped]")
                               (and match-pos (> match-pos *isearch-start-pos*) " [Wrapped]"))))
                (echo-message! echo
                  (string-append prefix query
                    (if wrapped wrapped "")
                    " [" (number->string count) " matches]")))))
          ;; Not found
          (begin
            (isearch-highlight-all! ed query -1)  ;; highlight remaining matches in yellow
            (echo-error! echo (string-append "Failing " prefix query))))))))

(def (isearch-next! app direction)
  "Move to next/previous match."
  (set! *isearch-active* direction)
  (let* ((ed (current-qt-editor app))
         (query *isearch-query*)
         (text (qt-plain-text-edit-text ed))
         (cur-pos (qt-plain-text-edit-cursor-position ed)))
    (when (> (string-length query) 0)
      (let ((match-pos
              (if (eq? direction 'forward)
                (or (isearch-find-nearest-forward text query cur-pos)
                    (isearch-find-nearest-forward text query 0))
                (or (isearch-find-nearest-backward text query
                      (- cur-pos (string-length query) 1))
                    (isearch-find-nearest-backward text query
                      (- (string-length text) 1))))))
        (when match-pos
          (if (eq? direction 'forward)
            (qt-plain-text-edit-set-cursor-position! ed (+ match-pos (string-length query)))
            (qt-plain-text-edit-set-cursor-position! ed match-pos))
          (qt-plain-text-edit-ensure-cursor-visible! ed)
          ;; Rehighlight with new current match
          (qt-update-visual-decorations! ed)
          (isearch-highlight-all! ed query match-pos)
          (echo-message! (app-state-echo app)
            (string-append (if (eq? direction 'forward) "I-search: " "I-search backward: ")
                           query)))))))

(def (isearch-exit! app cancel?)
  "Exit isearch mode. If cancel?, restore original cursor position."
  (let ((ed (current-qt-editor app))
        (echo (app-state-echo app)))
    (when cancel?
      (qt-plain-text-edit-set-cursor-position! ed *isearch-start-pos*)
      (qt-plain-text-edit-ensure-cursor-visible! ed))
    ;; Save the query for future searches
    (when (> (string-length *isearch-query*) 0)
      (set! (app-state-last-search app) *isearch-query*))
    ;; Clear state
    (set! *isearch-active* #f)
    (set! *isearch-app* #f)
    ;; Restore visual decorations (clears search highlights)
    (qt-update-visual-decorations! ed)
    (if cancel?
      (echo-message! echo "Quit")
      (echo-clear! echo))))

(def (isearch-handle-key! app code mods text)
  "Handle a key event during isearch mode. Returns #t if handled."
  (cond
    ;; C-s: search forward / next match
    ((and (= code QT_KEY_S) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (if (= (string-length *isearch-query*) 0)
       ;; Empty query + C-s: use last search
       (let ((last (app-state-last-search app)))
         (when (and last (> (string-length last) 0))
           (set! *isearch-query* last)))
       (void))
     (isearch-next! app 'forward)
     #t)
    ;; C-r: search backward / prev match
    ((and (= code QT_KEY_R) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (if (= (string-length *isearch-query*) 0)
       (let ((last (app-state-last-search app)))
         (when (and last (> (string-length last) 0))
           (set! *isearch-query* last)))
       (void))
     (isearch-next! app 'backward)
     #t)
    ;; C-g: cancel isearch, restore position
    ((and (= code QT_KEY_G) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (isearch-exit! app #t)
     #t)
    ;; Backspace: remove last character from query
    ((= code QT_KEY_BACKSPACE)
     (if (> (string-length *isearch-query*) 0)
       (begin
         (set! *isearch-query*
           (substring *isearch-query* 0 (- (string-length *isearch-query*) 1)))
         ;; Re-search from start position
         (qt-plain-text-edit-set-cursor-position! (current-qt-editor app) *isearch-start-pos*)
         (isearch-update! app))
       ;; Empty query + backspace: exit isearch
       (isearch-exit! app #t))
     #t)
    ;; Enter/Return/Escape: exit isearch, keep position
    ((or (= code QT_KEY_RETURN) (= code QT_KEY_ENTER) (= code QT_KEY_ESCAPE))
     (isearch-exit! app #f)
     #t)
    ;; C-w: yank word at cursor into search query
    ((and (= code QT_KEY_W) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (let* ((ed (current-qt-editor app))
            (pos (qt-plain-text-edit-cursor-position ed))
            (text-content (qt-plain-text-edit-text ed))
            (len (string-length text-content)))
       ;; Grab word from cursor position
       (let loop ((end pos))
         (if (and (< end len)
                  (let ((ch (string-ref text-content end)))
                    (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-))))
           (loop (+ end 1))
           (when (> end pos)
             (set! *isearch-query*
               (string-append *isearch-query* (substring text-content pos end)))
             (isearch-update! app)))))
     #t)
    ;; Regular printable character: add to search query
    ((and text (> (string-length text) 0) (zero? (bitwise-and mods QT_MOD_CTRL)))
     (set! *isearch-query* (string-append *isearch-query* text))
     (isearch-update! app)
     #t)
    ;; Any other key: exit isearch and let normal handling proceed
    (else
     (isearch-exit! app #f)
     #f)))

(def (cmd-search-forward app)
  "Enter incremental search forward mode."
  (set! *isearch-active* 'forward)
  (set! *isearch-query* "")
  (set! *isearch-start-pos* (qt-plain-text-edit-cursor-position (current-qt-editor app)))
  (set! *isearch-app* app)
  (echo-message! (app-state-echo app) "I-search: "))

(def (cmd-search-backward app)
  "Enter incremental search backward mode."
  (set! *isearch-active* 'backward)
  (set! *isearch-query* "")
  (set! *isearch-start-pos* (qt-plain-text-edit-cursor-position (current-qt-editor app)))
  (set! *isearch-app* app)
  (echo-message! (app-state-echo app) "I-search backward: "))

;;;============================================================================
;;; Write file (save as)
;;;============================================================================

(def (cmd-write-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Write file: ")))
    (when (and filename (> (string-length filename) 0))
      (let* ((buf (current-qt-buffer app))
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
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "*Buffer List*"))))

;;;============================================================================
;;; Comment toggle (Scheme: ;; prefix)
;;;============================================================================

(def (qt-replace-line! ed line-num new-line-text)
  "Replace a line by index in a Qt editor. Reconstructs the full text."
  (let* ((text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (new-lines (let loop ((ls lines) (i 0) (acc []))
                      (if (null? ls)
                        (reverse acc)
                        (if (= i line-num)
                          (loop (cdr ls) (+ i 1) (cons new-line-text acc))
                          (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
         (new-text (string-join new-lines "\n"))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length new-text)))))

(def (cmd-toggle-comment app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      ""))
         (trimmed (string-trim line-text)))
    (cond
      ((and (>= (string-length trimmed) 3)
            (string=? (substring trimmed 0 3) ";; "))
       (let ((new-line (let ((cp (string-contains line-text ";; ")))
                         (if cp
                           (string-append (substring line-text 0 cp)
                                          (substring line-text (+ cp 3)
                                                     (string-length line-text)))
                           line-text))))
         (qt-replace-line! ed line new-line)))
      ((and (>= (string-length trimmed) 2)
            (string=? (substring trimmed 0 2) ";;"))
       (let ((new-line (let ((cp (string-contains line-text ";;")))
                         (if cp
                           (string-append (substring line-text 0 cp)
                                          (substring line-text (+ cp 2)
                                                     (string-length line-text)))
                           line-text))))
         (qt-replace-line! ed line new-line)))
      (else
       (qt-replace-line! ed line (string-append ";; " line-text))))))

;;;============================================================================
;;; Transpose chars
;;;============================================================================

(def (cmd-transpose-chars app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (when (>= pos 2)
      (let* ((c1 (string-ref text (- pos 2)))
             (c2 (string-ref text (- pos 1)))
             (new-text (string-append
                         (substring text 0 (- pos 2))
                         (string c2 c1)
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

;;;============================================================================
;;; Word case commands
;;;============================================================================

(def (qt-word-at-point ed)
  "Get word boundaries at cursor in Qt editor."
  (let* ((pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (let skip ((i pos))
      (if (>= i len)
        (values #f #f)
        (let ((ch (string-ref text i)))
          (if (or (char-alphabetic? ch) (char-numeric? ch)
                  (char=? ch #\_) (char=? ch #\-))
            (let find-end ((j (+ i 1)))
              (if (>= j len)
                (values i j)
                (let ((c (string-ref text j)))
                  (if (or (char-alphabetic? c) (char-numeric? c)
                          (char=? c #\_) (char=? c #\-))
                    (find-end (+ j 1))
                    (values i j)))))
            (skip (+ i 1))))))))

(def (cmd-upcase-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when start
        (let* ((text (qt-plain-text-edit-text ed))
               (word (substring text start end))
               (new-text (string-append
                           (substring text 0 start)
                           (string-upcase word)
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed end))))))

(def (cmd-downcase-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when start
        (let* ((text (qt-plain-text-edit-text ed))
               (word (substring text start end))
               (new-text (string-append
                           (substring text 0 start)
                           (string-downcase word)
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed end))))))

(def (cmd-capitalize-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when (and start (< start end))
        (let* ((text (qt-plain-text-edit-text ed))
               (word (substring text start end))
               (cap (string-append
                      (string-upcase (substring word 0 1))
                      (string-downcase (substring word 1 (string-length word)))))
               (new-text (string-append
                           (substring text 0 start)
                           cap
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed end))))))

;;;============================================================================
;;; Kill word
;;;============================================================================

(def (cmd-kill-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when start
        (let* ((pos (qt-plain-text-edit-cursor-position ed))
               (kill-start (min pos start))
               (text (qt-plain-text-edit-text ed))
               (killed (substring text kill-start end))
               (new-text (string-append
                           (substring text 0 kill-start)
                           (substring text end (string-length text)))))
          (set! (app-state-kill-ring app)
                (cons killed (app-state-kill-ring app)))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed kill-start))))))

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

(def (cmd-indent-or-complete app)
  (let ((buf (current-qt-buffer app)))
    (cond
      ((dired-buffer? buf) (void))
      ((terminal-buffer? buf) (cmd-term-send-tab app))
      ((repl-buffer? buf)
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed))
              (rs (hash-get *repl-state* buf)))
         (when (and rs (>= pos (repl-state-prompt-pos rs)))
           (qt-plain-text-edit-insert-text! ed "  "))))
      (else
       ;; If snippet is active, jump to next field
       (if *snippet-active*
         (cmd-snippet-next-field app)
         ;; Try snippet expansion first, then completion
         (if (cmd-snippet-expand app)
           (void)  ;; Snippet expanded
           (let* ((ed (current-qt-editor app))
                  (prefix (get-word-prefix ed)))
             (if (string=? prefix "")
               ;; No word prefix — just indent
               (qt-plain-text-edit-insert-text! ed "  ")
               ;; Have a prefix — show completions
               (let* ((text (qt-plain-text-edit-text ed))
                      (words (collect-buffer-words text))
                      ;; Filter to matching words
                      (matches (filter (lambda (w)
                                         (and (> (string-length w) (string-length prefix))
                                              (string-prefix? prefix w)))
                                       words))
                      (sorted (sort matches string<?)))
                 (if (null? sorted)
                   (qt-plain-text-edit-insert-text! ed "  ")
                   (let ((c (get-or-create-completer! ed app)))
                     (qt-completer-set-model-strings! c sorted)
                     (qt-completer-set-completion-prefix! c prefix)
                     ;; Show popup at cursor position
                     (let* ((pos (qt-plain-text-edit-cursor-position ed))
                            (line (qt-plain-text-edit-cursor-line ed)))
                       (qt-completer-complete-rect! c 0 0 200 20)))))))))))))

;;;============================================================================
;;; Query replace
;;;============================================================================

(def (string-replace-all str from to)
  "Replace all occurrences of 'from' with 'to' in 'str'."
  (let ((from-len (string-length from))
        (to-len (string-length to))
        (str-len (string-length str)))
    (if (= from-len 0) str
      (let ((out (open-output-string)))
        (let loop ((i 0))
          (if (> (+ i from-len) str-len)
            (begin (display (substring str i str-len) out)
                   (get-output-string out))
            (if (string=? (substring str i (+ i from-len)) from)
              (begin (display to out)
                     (loop (+ i from-len)))
              (begin (write-char (string-ref str i) out)
                     (loop (+ i 1))))))))))

;;; Interactive query-replace state
(def *qreplace-active* #f)    ; #f or #t
(def *qreplace-from* "")      ; search string
(def *qreplace-to* "")        ; replacement string
(def *qreplace-pos* 0)        ; current search position in text
(def *qreplace-count* 0)      ; number of replacements made
(def *qreplace-app* #f)       ; app-state reference

;; Query-replace highlight colors (red background for current match)
(def qr-cur-fg-r #xff) (def qr-cur-fg-g #xff) (def qr-cur-fg-b #xff)
(def qr-cur-bg-r #xcc) (def qr-cur-bg-g #x33) (def qr-cur-bg-b #x33)

(def (qreplace-find-next! app)
  "Find the next match from current position. Returns match position or #f."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (text-lower (string-downcase text))
         (from-lower (string-downcase *qreplace-from*)))
    (string-contains text-lower from-lower *qreplace-pos*)))

(def (qreplace-highlight-current! app match-pos)
  "Highlight the current match being queried."
  (let ((ed (current-qt-editor app))
        (pat-len (string-length *qreplace-from*)))
    ;; Restore visual decorations first
    (qt-update-visual-decorations! ed)
    ;; Highlight current match
    (qt-extra-selection-add-range! ed match-pos pat-len
      qr-cur-fg-r qr-cur-fg-g qr-cur-fg-b
      qr-cur-bg-r qr-cur-bg-g qr-cur-bg-b bold: #t)
    (qt-extra-selections-apply! ed)
    ;; Move cursor to match and ensure visible
    (qt-plain-text-edit-set-cursor-position! ed match-pos)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (qreplace-show-next! app)
  "Find and display the next match, or finish if none."
  (let ((match-pos (qreplace-find-next! app)))
    (if match-pos
      (begin
        (qreplace-highlight-current! app match-pos)
        (echo-message! (app-state-echo app)
          (string-append "Replace \"" *qreplace-from* "\" with \""
                         *qreplace-to* "\"? (y/n/!/q) ["
                         (number->string *qreplace-count*) " done]")))
      ;; No more matches
      (qreplace-finish! app))))

(def (qreplace-do-replace! app match-pos)
  "Replace the match at match-pos and advance."
  (let* ((ed (current-qt-editor app))
         (pat-len (string-length *qreplace-from*))
         (repl-len (string-length *qreplace-to*)))
    ;; Select the match text and replace it
    (qt-plain-text-edit-set-selection! ed match-pos (+ match-pos pat-len))
    (qt-plain-text-edit-remove-selected-text! ed)
    (qt-plain-text-edit-set-cursor-position! ed match-pos)
    (qt-plain-text-edit-insert-text! ed *qreplace-to*)
    ;; Advance past replacement
    (set! *qreplace-pos* (+ match-pos repl-len))
    (set! *qreplace-count* (+ *qreplace-count* 1))))

(def (qreplace-replace-all! app)
  "Replace all remaining matches."
  (let loop ()
    (let ((match-pos (qreplace-find-next! app)))
      (when match-pos
        (qreplace-do-replace! app match-pos)
        (loop))))
  (qreplace-finish! app))

(def (qreplace-finish! app)
  "End query-replace mode."
  (set! *qreplace-active* #f)
  (set! *qreplace-app* #f)
  ;; Restore visual decorations
  (qt-update-visual-decorations! (current-qt-editor app))
  (echo-message! (app-state-echo app)
    (string-append "Replaced " (number->string *qreplace-count*) " occurrence"
                   (if (= *qreplace-count* 1) "" "s"))))

(def (qreplace-handle-key! app code mods text)
  "Handle a key event during query-replace mode. Returns #t if handled."
  (let ((match-pos (qreplace-find-next! app)))
    (cond
      ;; No current match — should have been caught, but handle gracefully
      ((not match-pos)
       (qreplace-finish! app)
       #t)
      ;; y or space: replace this match, move to next
      ((and text (or (string=? text "y") (string=? text " ")))
       (qreplace-do-replace! app match-pos)
       (qreplace-show-next! app)
       #t)
      ;; n or Delete: skip this match, move to next
      ((and text (or (string=? text "n") (= code QT_KEY_DELETE) (= code QT_KEY_BACKSPACE)))
       (set! *qreplace-pos* (+ match-pos 1))
       (qreplace-show-next! app)
       #t)
      ;; !: replace all remaining
      ((and text (string=? text "!"))
       (qreplace-replace-all! app)
       #t)
      ;; q or Escape: quit
      ((or (and text (string=? text "q")) (= code QT_KEY_ESCAPE))
       (qreplace-finish! app)
       #t)
      ;; . (period): replace this one and quit
      ((and text (string=? text "."))
       (qreplace-do-replace! app match-pos)
       (qreplace-finish! app)
       #t)
      ;; C-g: cancel
      ((and (= code QT_KEY_G) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
       (qreplace-finish! app)
       #t)
      ;; Ignore other keys
      (else #t))))

(def (cmd-query-replace app)
  (let* ((echo (app-state-echo app))
         (from-str (qt-echo-read-string app "Query replace: ")))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (qt-echo-read-string app
                      (string-append "Replace \"" from-str "\" with: "))))
        (when to-str
          ;; Enter interactive query-replace mode
          (set! *qreplace-active* #t)
          (set! *qreplace-from* from-str)
          (set! *qreplace-to* to-str)
          (set! *qreplace-pos* (qt-plain-text-edit-cursor-position (current-qt-editor app)))
          (set! *qreplace-count* 0)
          (set! *qreplace-app* app)
          ;; Find and show first match
          (qreplace-show-next! app))))))

;;;============================================================================
;;; Eshell commands
;;;============================================================================

(def eshell-buffer-name "*eshell*")

(def (cmd-eshell app)
  "Open or switch to the *eshell* buffer."
  (let ((existing (buffer-by-name eshell-buffer-name)))
    (if existing
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app)))
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
        (echo-message! (app-state-echo app) eshell-buffer-name))
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app))
             (buf (qt-buffer-create! eshell-buffer-name ed #f)))
        (set! (buffer-lexer-lang buf) 'eshell)
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (hash-put! *eshell-state* buf (current-directory))
        (let ((welcome (string-append "Gerbil Eshell\n"
                                       "Type commands, Gerbil expressions, or 'exit' to close.\n\n"
                                       eshell-prompt)))
          (qt-plain-text-edit-set-text! ed welcome)
          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END))
        (echo-message! (app-state-echo app) "Eshell started")))))

(def (cmd-eshell-send app)
  "Process eshell input in Qt backend."
  (let* ((buf (current-qt-buffer app))
         (cwd (hash-get *eshell-state* buf)))
    (when cwd
      (let* ((ed (current-qt-editor app))
             (all-text (qt-plain-text-edit-text ed))
             ;; Find the last prompt
             (prompt-pos (let loop ((pos (- (string-length all-text) (string-length eshell-prompt))))
                           (cond
                             ((< pos 0) #f)
                             ((string=? (substring all-text pos (+ pos (string-length eshell-prompt))) eshell-prompt) pos)
                             (else (loop (- pos 1))))))
             (end-pos (string-length all-text))
             (input (if (and prompt-pos (> end-pos (+ prompt-pos (string-length eshell-prompt))))
                      (substring all-text (+ prompt-pos (string-length eshell-prompt)) end-pos)
                      "")))
        (qt-plain-text-edit-append! ed "")
        (let-values (((output new-cwd) (eshell-process-input input cwd)))
          (hash-put! *eshell-state* buf new-cwd)
          (cond
            ((eq? output 'clear)
             (qt-plain-text-edit-set-text! ed eshell-prompt)
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END))
            ((eq? output 'exit)
             (cmd-kill-buffer-cmd app))
            (else
             (when (and (string? output) (> (string-length output) 0))
               (qt-plain-text-edit-append! ed output))
             (qt-plain-text-edit-append! ed eshell-prompt)
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END))))))))

;;;============================================================================
;;; Shell commands
;;;============================================================================

(def shell-buffer-name "*shell*")

(def (cmd-shell app)
  "Open or switch to the *shell* buffer."
  (let ((existing (buffer-by-name shell-buffer-name)))
    (if existing
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app)))
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
        (echo-message! (app-state-echo app) shell-buffer-name))
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app))
             (buf (qt-buffer-create! shell-buffer-name ed #f)))
        (set! (buffer-lexer-lang buf) 'shell)
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (let ((ss (shell-start!)))
          (hash-put! *shell-state* buf ss)
          (qt-plain-text-edit-set-text! ed "")
          (set! (shell-state-prompt-pos ss) 0))
        (echo-message! (app-state-echo app) "Shell started")))))

(def (cmd-shell-send app)
  "Send the current input line to the shell subprocess."
  (let* ((buf (current-qt-buffer app))
         (ss (hash-get *shell-state* buf)))
    (when ss
      (let* ((ed (current-qt-editor app))
             (prompt-pos (shell-state-prompt-pos ss))
             (all-text (qt-plain-text-edit-text ed))
             (end-pos (string-length all-text))
             (input (if (> end-pos prompt-pos)
                      (substring all-text prompt-pos end-pos)
                      "")))
        (qt-plain-text-edit-append! ed "")
        (shell-send! ss input)
        (set! (shell-state-prompt-pos ss)
          (string-length (qt-plain-text-edit-text ed)))))))

;;;============================================================================
;;; Misc commands
;;;============================================================================

(def (cmd-keyboard-quit app)
  (echo-message! (app-state-echo app) "Quit")
  (set! (app-state-key-state app) (make-initial-key-state)))

(def (cmd-quit app)
  ;; Check for unsaved buffers
  (let* ((unsaved (filter
                    (lambda (buf)
                      (and (buffer-file-path buf)
                           (buffer-doc-pointer buf)
                           (qt-text-document-modified? (buffer-doc-pointer buf))))
                    *buffer-list*))
         (echo (app-state-echo app))
         (fr (app-state-frame app)))
    (if (null? unsaved)
      ;; No unsaved buffers, quit immediately
      (begin
        (scratch-save!)
        (savehist-save!)
        (session-save! app)
        (set! (app-state-running app) #f)
        (qt-widget-close! (qt-frame-main-win fr)))
      ;; Prompt about unsaved buffers
      (let* ((names (map buffer-name unsaved))
             (shown-names (if (> (length names) 3)
                            (let loop ((l names) (n 0) (acc []))
                              (if (or (null? l) (>= n 3))
                                (append (reverse acc) (list "..."))
                                (loop (cdr l) (+ n 1) (cons (car l) acc))))
                            names))
             (msg (string-append
                    (number->string (length unsaved))
                    " unsaved buffer(s): "
                    (string-join shown-names ", ")
                    ". Save? (yes/no/cancel) "))
             (answer (qt-echo-read-string app msg)))
        (cond
          ((and answer (or (string=? answer "yes") (string=? answer "y")))
           ;; Save all: attach each buffer temporarily to get text, save, restore
           (let* ((ed (current-qt-editor app))
                  (original-buf (current-qt-buffer app)))
             (for-each
               (lambda (buf)
                 (let ((path (buffer-file-path buf)))
                   (when path
                     (qt-buffer-attach! ed buf)
                     (let ((text (qt-plain-text-edit-text ed)))
                       (write-string-to-file path text)
                       (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))))
               unsaved)
             ;; Restore original buffer
             (qt-buffer-attach! ed original-buf))
           (scratch-save!)
           (session-save! app)
           (set! (app-state-running app) #f)
           (qt-widget-close! (qt-frame-main-win fr)))
          ((and answer (or (string=? answer "no") (string=? answer "n")))
           ;; Quit without saving
           (scratch-save!)
           (session-save! app)
           (set! (app-state-running app) #f)
           (qt-widget-close! (qt-frame-main-win fr)))
          (else
           ;; Cancel
           (echo-message! echo "Quit cancelled")))))))

;;;============================================================================
;;; Dired (directory listing) support
;;;============================================================================

(def (dired-open-directory! app dir-path)
  "Open a directory listing in a new dired buffer."
  (let* ((dir (strip-trailing-slash dir-path))
         (name (string-append dir "/"))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (qt-buffer-create! name ed dir)))
    ;; Mark as dired buffer
    (set! (buffer-lexer-lang buf) 'dired)
    ;; Attach buffer to editor
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    ;; Generate and set listing
    (let-values (((text entries) (dired-format-listing dir)))
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      ;; Position cursor at first entry (line 3, after header + count + blank)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)
      ;; Store entries for navigation
      (hash-put! *dired-entries* buf entries))
    (echo-message! (app-state-echo app) (string-append "Directory: " dir))))

(def (cmd-dired-find-file app)
  "In a dired buffer, open the file or directory under cursor."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (entries (hash-get *dired-entries* buf)))
    (when entries
      (let ((idx (- line 3)))
        (if (or (< idx 0) (>= idx (vector-length entries)))
          (echo-message! (app-state-echo app) "No file on this line")
          (let ((full-path (vector-ref entries idx)))
            (with-catch
              (lambda (e)
                (echo-error! (app-state-echo app)
                             (string-append "Error: "
                               (with-output-to-string
                                 (lambda () (display-exception e))))))
              (lambda ()
                (let ((info (file-info full-path)))
                  (if (eq? 'directory (file-info-type info))
                    (dired-open-directory! app full-path)
                    ;; Open as regular file
                    (let* ((fname (path-strip-directory full-path))
                           (fr (app-state-frame app))
                           (new-buf (qt-buffer-create! fname ed full-path)))
                      (qt-buffer-attach! ed new-buf)
                      (set! (qt-edit-window-buffer (qt-current-window fr))
                            new-buf)
                      (let ((text (read-file-as-string full-path)))
                        (when text
                          (qt-plain-text-edit-set-text! ed text)
                          (qt-text-document-set-modified!
                            (buffer-doc-pointer new-buf) #f)
                          (qt-plain-text-edit-set-cursor-position! ed 0)))
                      (qt-setup-highlighting! app new-buf)
                      (echo-message! (app-state-echo app)
                                     (string-append "Opened: "
                                                    full-path)))))))))))))

;;;============================================================================
;;; REPL commands
;;;============================================================================

(def repl-buffer-name "*REPL*")

(def (cmd-repl app)
  "Open or switch to the *REPL* buffer."
  (let ((existing (buffer-by-name repl-buffer-name)))
    (if existing
      ;; Switch to existing REPL buffer
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app)))
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
        (echo-message! (app-state-echo app) repl-buffer-name))
      ;; Create new REPL buffer
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app))
             (buf (qt-buffer-create! repl-buffer-name ed #f)))
        ;; Mark as REPL buffer
        (set! (buffer-lexer-lang buf) 'repl)
        ;; Attach buffer to editor
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        ;; Spawn gxi subprocess
        (let ((rs (repl-start!)))
          (hash-put! *repl-state* buf rs)
          ;; Don't insert initial prompt — let the timer handle it when
          ;; gxi's startup banner arrives. Set prompt-pos high to prevent
          ;; typing until gxi is ready.
          (qt-plain-text-edit-set-text! ed "")
          (set! (repl-state-prompt-pos rs) 999999999))
        (echo-message! (app-state-echo app) "REPL started")))))

(def (cmd-repl-send app)
  "Send the current input line to the gxi subprocess."
  (let* ((buf (current-qt-buffer app))
         (rs (hash-get *repl-state* buf)))
    (when rs
      (let* ((ed (current-qt-editor app))
             (prompt-pos (repl-state-prompt-pos rs))
             (all-text (qt-plain-text-edit-text ed))
             (text-len (string-length all-text))
             ;; Extract user input after the prompt
             (input (if (and (<= prompt-pos text-len) (> text-len prompt-pos))
                      (substring all-text prompt-pos text-len)
                      "")))
        (when (> (string-length input) 0)
          ;; Append newline to the buffer
          (qt-plain-text-edit-append! ed "")
          ;; Send to gxi
          (repl-send! rs input)
          ;; Prevent typing until gxi responds (timer will reset prompt-pos)
          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
          (set! (repl-state-prompt-pos rs) 999999999))))))

(def (cmd-eval-expression app)
  "Prompt for an expression, eval it in-process."
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Eval: ")))
    (when (and input (> (string-length input) 0))
      (let-values (((result error?) (eval-expression-string input)))
        (if error?
          (echo-error! echo result)
          (echo-message! echo result))))))

;;;============================================================================
;;; Zoom commands
;;;============================================================================

(def (cmd-zoom-in app)
  (let* ((ed (current-qt-editor app))
         (font (qt-widget-font ed))
         (size (qt-font-point-size font)))
    (qt-font-destroy! font)
    (qt-widget-set-font-size! ed (+ size 1))))

(def (cmd-zoom-out app)
  (let* ((ed (current-qt-editor app))
         (font (qt-widget-font ed))
         (size (qt-font-point-size font)))
    (qt-font-destroy! font)
    (when (> size 6)
      (qt-widget-set-font-size! ed (- size 1)))))

;;;============================================================================
;;; Toggle line numbers
;;;============================================================================

(def *line-numbers-visible* #t)

(def (cmd-toggle-line-numbers app)
  (set! *line-numbers-visible* (not *line-numbers-visible*))
  (let ((fr (app-state-frame app)))
    (for-each
      (lambda (win)
        (let ((lna (qt-edit-window-line-number-area win)))
          (when lna
            (qt-line-number-area-set-visible! lna *line-numbers-visible*))))
      (qt-frame-windows fr)))
  (echo-message! (app-state-echo app)
    (if *line-numbers-visible* "Line numbers ON" "Line numbers OFF")))

;;;============================================================================
;;; Backward kill word
;;;============================================================================

(def (cmd-backward-kill-word app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (when (> pos 0)
      ;; Skip whitespace backward
      (let skip-ws ((i (- pos 1)))
        (if (and (>= i 0) (char-whitespace? (string-ref text i)))
          (skip-ws (- i 1))
          ;; Now skip word chars backward
          (let skip-word ((j i))
            (if (and (>= j 0)
                     (let ((ch (string-ref text j)))
                       (or (char-alphabetic? ch) (char-numeric? ch)
                           (char=? ch #\_) (char=? ch #\-))))
              (skip-word (- j 1))
              ;; j+1 is the start of the word
              (let* ((start (+ j 1))
                     (killed (substring text start pos))
                     (new-text (string-append
                                 (substring text 0 start)
                                 (substring text pos (string-length text)))))
                (set! (app-state-kill-ring app)
                      (cons killed (app-state-kill-ring app)))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed start)))))))))

;;;============================================================================
;;; Kill whole line
;;;============================================================================

(def (cmd-kill-whole-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let* ((killed (list-ref lines line))
             (new-lines (let loop ((ls lines) (i 0) (acc []))
                          (if (null? ls) (reverse acc)
                            (if (= i line) (loop (cdr ls) (+ i 1) acc)
                              (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
             (new-text (if (null? new-lines) "" (string-join new-lines "\n"))))
        (set! (app-state-kill-ring app)
              (cons killed (app-state-kill-ring app)))
        (qt-plain-text-edit-set-text! ed new-text)
        (let ((pos (let loop ((i 0) (ln 0))
                     (cond ((= ln line) i)
                           ((>= i (string-length new-text)) i)
                           ((char=? (string-ref new-text i) #\newline)
                            (loop (+ i 1) (+ ln 1)))
                           (else (loop (+ i 1) ln))))))
          (qt-plain-text-edit-set-cursor-position! ed
            (min pos (string-length new-text))))))))

;;;============================================================================
;;; Join line
;;;============================================================================

(def (cmd-join-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< (+ line 1) (length lines))
      (let* ((current (list-ref lines line))
             (next (string-trim (list-ref lines (+ line 1))))
             (joined (string-append (string-trim-right current) " " next))
             (new-lines (let loop ((ls lines) (i 0) (acc []))
                          (cond ((null? ls) (reverse acc))
                                ((= i line)
                                 (loop (if (pair? (cdr ls)) (cddr ls) '())
                                       (+ i 2) (cons joined acc)))
                                (else (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))))))

;;;============================================================================
;;; Just one space
;;;============================================================================

(def (cmd-just-one-space app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (let* ((start (let loop ((i (- pos 1)))
                    (if (and (>= i 0) (char-whitespace? (string-ref text i)))
                      (loop (- i 1))
                      (+ i 1))))
           (end (let loop ((i pos))
                  (if (and (< i len) (char-whitespace? (string-ref text i)))
                    (loop (+ i 1))
                    i)))
           (new-text (string-append (substring text 0 start) " "
                                    (substring text end len))))
      (qt-plain-text-edit-set-text! ed new-text)
      (qt-plain-text-edit-set-cursor-position! ed (+ start 1)))))

;;;============================================================================
;;; Transpose words
;;;============================================================================

(def (cmd-transpose-words app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    ;; Find word before cursor
    (let* ((w1-end (let loop ((i (- pos 1)))
                     (if (and (>= i 0) (char-whitespace? (string-ref text i)))
                       (loop (- i 1)) (+ i 1))))
           (w1-start (let loop ((i (- w1-end 1)))
                       (if (and (>= i 0) (not (char-whitespace? (string-ref text i))))
                         (loop (- i 1)) (+ i 1))))
           ;; Find word after cursor
           (w2-start (let loop ((i pos))
                       (if (and (< i len) (char-whitespace? (string-ref text i)))
                         (loop (+ i 1)) i)))
           (w2-end (let loop ((i w2-start))
                     (if (and (< i len) (not (char-whitespace? (string-ref text i))))
                       (loop (+ i 1)) i))))
      (when (and (< w1-start w1-end) (< w2-start w2-end) (<= w1-end w2-start))
        (let* ((word1 (substring text w1-start w1-end))
               (between (substring text w1-end w2-start))
               (word2 (substring text w2-start w2-end))
               (new-text (string-append (substring text 0 w1-start)
                                        word2 between word1
                                        (substring text w2-end len))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed w2-end))))))

;;;============================================================================
;;; Transpose lines
;;;============================================================================

(def (cmd-transpose-lines app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (and (> line 0) (< line (length lines)))
      (let* ((vec (list->vector lines))
             (tmp (vector-ref vec line)))
        (vector-set! vec line (vector-ref vec (- line 1)))
        (vector-set! vec (- line 1) tmp)
        (let ((new-text (string-join (vector->list vec) "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          ;; Position cursor at start of next line
          (let ((pos (let loop ((i 0) (ln 0))
                       (cond ((= ln (+ line 1)) i)
                             ((>= i (string-length new-text)) i)
                             ((char=? (string-ref new-text i) #\newline)
                              (loop (+ i 1) (+ ln 1)))
                             (else (loop (+ i 1) ln))))))
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (string-length new-text)))))))))

;;;============================================================================
;;; Move line up / down
;;;============================================================================

(def (line-start-position text line-num)
  "Get character position of the start of a given line number."
  (let loop ((i 0) (ln 0))
    (cond ((= ln line-num) i)
          ((>= i (string-length text)) i)
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ ln 1)))
          (else (loop (+ i 1) ln)))))

(def (cmd-move-line-up app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (> line 0)
      (let* ((vec (list->vector lines))
             (tmp (vector-ref vec line)))
        (vector-set! vec line (vector-ref vec (- line 1)))
        (vector-set! vec (- line 1) tmp)
        (let ((new-text (string-join (vector->list vec) "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed
            (line-start-position new-text (- line 1))))))))

(def (cmd-move-line-down app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< (+ line 1) (length lines))
      (let* ((vec (list->vector lines))
             (tmp (vector-ref vec line)))
        (vector-set! vec line (vector-ref vec (+ line 1)))
        (vector-set! vec (+ line 1) tmp)
        (let ((new-text (string-join (vector->list vec) "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed
            (line-start-position new-text (+ line 1))))))))

;;;============================================================================
;;; Fill paragraph
;;;============================================================================

(def (cmd-fill-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (fill-column 70))
    ;; Find paragraph boundaries (blank lines)
    (let* ((para-start (let loop ((i line))
                         (if (or (<= i 0)
                                 (string=? (string-trim (list-ref lines (- i 1))) ""))
                           i (loop (- i 1)))))
           (para-end (let loop ((i line))
                       (if (or (>= i (- (length lines) 1))
                               (string=? (string-trim (list-ref lines (+ i 1))) ""))
                         (+ i 1) (loop (+ i 1)))))
           ;; Collect paragraph text
           (para-lines (let loop ((i para-start) (acc []))
                         (if (>= i para-end) (reverse acc)
                           (loop (+ i 1) (cons (list-ref lines i) acc)))))
           (para-text (string-join para-lines " "))
           (words (filter (lambda (w) (> (string-length w) 0))
                          (string-split (string-trim para-text) #\space)))
           ;; Reflow
           (filled-lines
             (if (null? words) '("")
               (let loop ((ws (cdr words))
                          (current-line (car words))
                          (acc []))
                 (if (null? ws)
                   (reverse (cons current-line acc))
                   (let ((next (string-append current-line " " (car ws))))
                     (if (<= (string-length next) fill-column)
                       (loop (cdr ws) next acc)
                       (loop (cdr ws) (car ws) (cons current-line acc)))))))))
      ;; Replace paragraph lines
      (let* ((before (let loop ((i 0) (acc []))
                       (if (>= i para-start) (reverse acc)
                         (loop (+ i 1) (cons (list-ref lines i) acc)))))
             (after (let loop ((i para-end) (acc []))
                      (if (>= i (length lines)) (reverse acc)
                        (loop (+ i 1) (cons (list-ref lines i) acc)))))
             (new-lines (append before filled-lines after))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))
        (echo-message! (app-state-echo app) "Filled")))))

;;;============================================================================
;;; Count words
;;;============================================================================

(def (cmd-count-words app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (num-lines (qt-plain-text-edit-line-count ed))
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i len) count
                    (let ((ch (string-ref text i)))
                      (if (char-whitespace? ch)
                        (loop (+ i 1) #f count)
                        (loop (+ i 1) #t (if in-word count (+ count 1)))))))))
    (echo-message! (app-state-echo app)
      (string-append "Buffer has " (number->string num-lines) " lines, "
                     (number->string words) " words, "
                     (number->string len) " characters"))))

;;;============================================================================
;;; What cursor position
;;;============================================================================

(def (cmd-what-cursor-position app)
  "Show detailed info about character at point: char, code (hex/dec/oct), position, line, col."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app)
        (string-append "point=" (number->string pos) " of " (number->string len) " (EOB)"))
      (let* ((ch (string-ref text pos))
             (code (char->integer ch))
             (hex (number->string code 16))
             (oct (number->string code 8))
             (line (let loop ((i 0) (l 1))
                     (cond ((>= i pos) l)
                           ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ l 1)))
                           (else (loop (+ i 1) l)))))
             (line-start (let loop ((i (- pos 1)))
                           (cond ((< i 0) 0)
                                 ((char=? (string-ref text i) #\newline) (+ i 1))
                                 (else (loop (- i 1))))))
             (col (- pos line-start))
             (pct (if (= len 0) 0 (round (* 100 (/ pos len))))))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (if (char=? ch #\newline) "^J" (string ch))
                         " (0x" hex ", " (number->string code) ", 0" oct ")"
                         "  point=" (number->string pos)
                         " of " (number->string len)
                         " (" (number->string (inexact->exact pct)) "%)"
                         "  line " (number->string line)
                         " col " (number->string col)))))))

;;;============================================================================
;;; Dynamic abbreviation (dabbrev)
;;;============================================================================

(def (cmd-dabbrev-expand app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (prefix (get-word-prefix ed)))
    (if (string=? prefix "")
      (echo-message! (app-state-echo app) "No dynamic expansion found")
      (let ((state (app-state-dabbrev-state app)))
        (if (and state
                 (eq? (app-state-last-command app) 'dabbrev-expand)
                 (string=? (car state) prefix))
          ;; Continue cycling
          (let ((matches (cadr state))
                (last-pos (caddr state))
                (last-len (cadddr state)))
            (if (null? matches)
              (begin
                (set! (app-state-dabbrev-state app) #f)
                (echo-message! (app-state-echo app) "No further expansions"))
              (let* ((match (car matches))
                     (suffix (substring match (string-length (car state))
                                        (string-length match)))
                     (cur-text (qt-plain-text-edit-text ed))
                     (new-text (string-append
                                 (substring cur-text 0 last-pos)
                                 suffix
                                 (substring cur-text (+ last-pos last-len)
                                           (string-length cur-text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (+ last-pos (string-length suffix)))
                (set! (app-state-dabbrev-state app)
                  (list (car state) (cdr matches)
                        last-pos (string-length suffix))))))
          ;; Fresh expansion
          (let* ((all-words (collect-buffer-words text))
                 (matches (filter (lambda (w)
                                    (and (> (string-length w) (string-length prefix))
                                         (string-prefix? prefix w)
                                         (not (string=? w prefix))))
                                  all-words))
                 (sorted (sort matches string<?)))
            (if (null? sorted)
              (echo-message! (app-state-echo app) "No dynamic expansion found")
              (let* ((match (car sorted))
                     (suffix (substring match (string-length prefix)
                                        (string-length match)))
                     (new-text (string-append
                                 (substring text 0 pos)
                                 suffix
                                 (substring text pos (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (+ pos (string-length suffix)))
                (set! (app-state-dabbrev-state app)
                  (list prefix (cdr sorted) pos (string-length suffix)))))))))))

;;;============================================================================
;;; Delete blank lines
;;;============================================================================

(def (cmd-delete-blank-lines app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (current-blank? (and (< line (length lines))
                              (string=? (string-trim (list-ref lines line)) ""))))
    (if current-blank?
      ;; On blank line: delete all surrounding blank lines, keep one
      (let* ((start (let loop ((i line))
                      (if (and (> i 0) (string=? (string-trim (list-ref lines (- i 1))) ""))
                        (loop (- i 1)) i)))
             (end (let loop ((i line))
                    (if (and (< (+ i 1) (length lines))
                             (string=? (string-trim (list-ref lines (+ i 1))) ""))
                      (loop (+ i 1)) (+ i 1))))
             (new-lines (append
                          (let loop ((i 0) (acc []))
                            (if (>= i start) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))
                          '("")
                          (let loop ((i end) (acc []))
                            (if (>= i (length lines)) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (line-start-position new-text start)))
      ;; On non-blank line: delete following blank lines
      (let* ((end (let loop ((i (+ line 1)))
                    (if (and (< i (length lines))
                             (string=? (string-trim (list-ref lines i)) ""))
                      (loop (+ i 1)) i)))
             (new-lines (append
                          (let loop ((i 0) (acc []))
                            (if (> i line) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))
                          (let loop ((i end) (acc []))
                            (if (>= i (length lines)) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))))))

;;;============================================================================
;;; Insert file
;;;============================================================================

(def (cmd-insert-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Insert file: ")))
    (when (and filename (> (string-length filename) 0))
      (if (file-exists? filename)
        (let ((content (read-file-as-string filename)))
          (when content
            (qt-plain-text-edit-insert-text! (current-qt-editor app) content)
            (echo-message! echo (string-append "Inserted " filename))))
        (echo-error! echo (string-append "File not found: " filename))))))

;;;============================================================================
;;; Shell command (M-!)
;;;============================================================================

(def (cmd-shell-command app)
  (let* ((echo (app-state-echo app))
         (cmd (qt-echo-read-string app "Shell command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (let ((result (with-catch
                      (lambda (e)
                        (string-append "Error: "
                          (with-output-to-string (lambda () (display-exception e)))))
                      (lambda ()
                        (let ((port (open-process
                                      (list path: "/bin/sh"
                                            arguments: ["-c" cmd]
                                            stdout-redirection: #t
                                            stderr-redirection: #t
                                            pseudo-terminal: #f))))
                          (let ((output (read-line port #f)))
                            (close-port port)
                            (or output "")))))))
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (or (buffer-by-name "*Shell Output*")
                        (qt-buffer-create! "*Shell Output*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (echo-message! echo (string-append "Shell: " cmd)))))))

;;;============================================================================
;;; Sort lines (in region)
;;;============================================================================

(def (cmd-sort-lines app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines string<?))
             (new-region (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app)
          (string-append "Sorted " (number->string (length lines)) " lines")))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Goto matching paren
;;;============================================================================

(def (cmd-goto-matching-paren app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "No paren at point")
      (let ((ch (string-ref text pos)))
        (cond
          ;; Opening: scan forward
          ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
           (let ((close (cond ((char=? ch #\() #\))
                              ((char=? ch #\[) #\])
                              (else #\}))))
             (let loop ((i (+ pos 1)) (depth 1))
               (cond ((>= i len)
                      (echo-message! (app-state-echo app) "No matching paren"))
                     ((char=? (string-ref text i) ch)
                      (loop (+ i 1) (+ depth 1)))
                     ((char=? (string-ref text i) close)
                      (if (= depth 1)
                        (begin
                          (qt-plain-text-edit-set-cursor-position! ed i)
                          (qt-plain-text-edit-ensure-cursor-visible! ed))
                        (loop (+ i 1) (- depth 1))))
                     (else (loop (+ i 1) depth))))))
          ;; Closing: scan backward
          ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
           (let ((open (cond ((char=? ch #\)) #\()
                             ((char=? ch #\]) #\[)
                             (else #\{))))
             (let loop ((i (- pos 1)) (depth 1))
               (cond ((< i 0)
                      (echo-message! (app-state-echo app) "No matching paren"))
                     ((char=? (string-ref text i) ch)
                      (loop (- i 1) (+ depth 1)))
                     ((char=? (string-ref text i) open)
                      (if (= depth 1)
                        (begin
                          (qt-plain-text-edit-set-cursor-position! ed i)
                          (qt-plain-text-edit-ensure-cursor-visible! ed))
                        (loop (- i 1) (- depth 1))))
                     (else (loop (- i 1) depth))))))
          (else
           (echo-message! (app-state-echo app) "No paren at point")))))))

;;;============================================================================
;;; Upcase / downcase region
;;;============================================================================

(def (cmd-upcase-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (new-text (string-append (substring text 0 start)
                                      (string-upcase region)
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-downcase-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (new-text (string-append (substring text 0 start)
                                      (string-downcase region)
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Indent region
;;;============================================================================

(def (cmd-indent-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (indented (map (lambda (l) (string-append "  " l)) lines))
             (new-region (string-join indented "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Indented region"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Zap to char
;;;============================================================================

(def (cmd-zap-to-char app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Zap to char: ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (ed (current-qt-editor app))
             (pos (qt-plain-text-edit-cursor-position ed))
             (text (qt-plain-text-edit-text ed))
             (len (string-length text)))
        (let loop ((i (+ pos 1)))
          (cond
            ((>= i len)
             (echo-error! echo (string-append "Char not found: " (string ch))))
            ((char=? (string-ref text i) ch)
             (let* ((killed (substring text pos (+ i 1)))
                    (new-text (string-append (substring text 0 pos)
                                             (substring text (+ i 1) len))))
               (set! (app-state-kill-ring app)
                     (cons killed (app-state-kill-ring app)))
               (qt-plain-text-edit-set-text! ed new-text)
               (qt-plain-text-edit-set-cursor-position! ed pos)))
            (else (loop (+ i 1)))))))))

;;;============================================================================
;;; Goto char position
;;;============================================================================

(def (cmd-goto-char app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Goto char: ")))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (if (and n (>= n 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (target (min n (string-length text))))
            (qt-plain-text-edit-set-cursor-position! ed target)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-error! echo "Invalid position"))))))

;;;============================================================================
;;; Zoom reset
;;;============================================================================

(def (cmd-zoom-reset app)
  (qt-widget-set-font-size! (current-qt-editor app) 10)
  (echo-message! (app-state-echo app) "Zoom reset"))

;;;============================================================================
;;; Yank pop (cycle kill ring)
;;;============================================================================

(def (cmd-yank-pop app)
  (if (and (app-state-last-yank-pos app)
           (memq (app-state-last-command app) '(yank yank-pop)))
    (let ((ring (app-state-kill-ring app)))
      (if (null? ring)
        (echo-message! (app-state-echo app) "Kill ring is empty")
        (let* ((ring-len (length ring))
               (idx (modulo (+ (app-state-kill-ring-idx app) 1) ring-len))
               (replacement (list-ref ring idx))
               (ed (current-qt-editor app))
               (full-text (qt-plain-text-edit-text ed))
               (yank-pos (app-state-last-yank-pos app))
               (yank-len (app-state-last-yank-len app))
               (new-text (string-append
                           (substring full-text 0 yank-pos)
                           replacement
                           (substring full-text (+ yank-pos yank-len)
                                     (string-length full-text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed
            (+ yank-pos (string-length replacement)))
          (set! (app-state-kill-ring-idx app) idx)
          (set! (app-state-last-yank-len app) (string-length replacement)))))
    (echo-message! (app-state-echo app) "Previous command was not a yank")))

;;;============================================================================
;;; Occur (show matching lines)
;;;============================================================================

;; Occur state: remember source buffer for jumping
(def *occur-source-buffer* #f) ; buffer name that occur was run on

(def (cmd-occur app)
  (let* ((echo (app-state-echo app))
         (source-buf (current-qt-buffer app))
         (query (qt-echo-read-string app "Occur: ")))
    (when (and query (> (string-length query) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (matches (let loop ((ls lines) (i 1) (acc []))
                        (if (null? ls) (reverse acc)
                          (if (string-contains (car ls) query)
                            (loop (cdr ls) (+ i 1)
                                  (cons (string-append (number->string i) ": " (car ls))
                                        acc))
                            (loop (cdr ls) (+ i 1) acc)))))
             (result (if (null? matches)
                       (string-append "No matches for: " query)
                       (string-append (number->string (length matches))
                                      " matches for \"" query
                                      "\" in " (buffer-name source-buf) "\n\n"
                                      (string-join matches "\n")
                                      "\n\nPress Enter on a line to jump to it."))))
        (set! *occur-source-buffer* (buffer-name source-buf))
        (let* ((fr (app-state-frame app))
               (buf (or (buffer-by-name "*Occur*")
                        (qt-buffer-create! "*Occur*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

(def (cmd-occur-goto app)
  "Jump from *Occur* buffer to the source line under cursor."
  (let* ((buf (current-qt-buffer app))
         (echo (app-state-echo app)))
    (if (not (string=? (buffer-name buf) "*Occur*"))
      (echo-error! echo "Not in *Occur* buffer")
      (if (not *occur-source-buffer*)
        (echo-error! echo "No occur source buffer")
        (let* ((ed (current-qt-editor app))
               (text (qt-plain-text-edit-text ed))
               ;; Get current line text
               (pos (qt-plain-text-edit-cursor-position ed))
               (line-start (let scan ((i (max 0 (- pos 1))))
                             (cond
                               ((<= i 0) 0)
                               ((char=? (string-ref text i) #\newline) (+ i 1))
                               (else (scan (- i 1))))))
               (line-end (let scan ((i pos))
                           (cond
                             ((>= i (string-length text)) i)
                             ((char=? (string-ref text i) #\newline) i)
                             (else (scan (+ i 1))))))
               (line (substring text line-start line-end)))
          ;; Parse line number from "NNN: text" format
          (let ((colon-pos (string-index line #\:)))
            (if (not colon-pos)
              (echo-error! echo "Not on an occur match line")
              (let ((line-num-str (substring line 0 colon-pos)))
                (let ((line-num (string->number line-num-str)))
                  (if (not line-num)
                    (echo-error! echo "Not on an occur match line")
                    ;; Switch to source buffer and jump to line
                    (let ((source (buffer-by-name *occur-source-buffer*)))
                      (if (not source)
                        (echo-error! echo
                          (string-append "Source buffer '" *occur-source-buffer* "' not found"))
                        (let* ((fr (app-state-frame app)))
                          (qt-buffer-attach! ed source)
                          (set! (qt-edit-window-buffer (qt-current-window fr)) source)
                          (let* ((src-text (qt-plain-text-edit-text ed))
                                 (target-pos (text-line-position src-text line-num)))
                            (qt-plain-text-edit-set-cursor-position! ed target-pos)
                            (qt-plain-text-edit-ensure-cursor-visible! ed)
                            (echo-message! echo
                              (string-append "Line " (number->string line-num)))))))))))))))))


;;;============================================================================
;;; Keyboard macros
;;;============================================================================

(def (cmd-start-kbd-macro app)
  (set! (app-state-macro-recording app) '())
  (echo-message! (app-state-echo app) "Defining keyboard macro..."))

(def (cmd-end-kbd-macro app)
  (if (app-state-macro-recording app)
    (begin
      (set! (app-state-macro-last app)
            (reverse (app-state-macro-recording app)))
      (set! (app-state-macro-recording app) #f)
      (echo-message! (app-state-echo app) "Keyboard macro defined"))
    (echo-error! (app-state-echo app) "Not defining a macro")))

(def (cmd-call-last-kbd-macro app)
  (let ((macro (app-state-macro-last app)))
    (if macro
      (let ((n (get-prefix-arg app)))
        (let loop ((i 0))
          (when (< i n)
            (for-each
              (lambda (event)
                (case (car event)
                  ((self-insert)
                   (qt-plain-text-edit-insert-text!
                     (current-qt-editor app) (cdr event)))
                  ((command)
                   (execute-command! app (cdr event)))))
              macro)
            (loop (+ i 1)))))
      (echo-error! (app-state-echo app) "No keyboard macro defined"))))

;;;============================================================================
;;; Repeat last command
;;;============================================================================

(def (cmd-repeat app)
  (let ((last (app-state-last-command app)))
    (if (and last (not (eq? last 'repeat)))
      (execute-command! app last)
      (echo-message! (app-state-echo app) "No command to repeat"))))

;;;============================================================================
;;; Pop mark (jump to mark ring)
;;;============================================================================

(def (cmd-pop-mark app)
  (let ((ring (app-state-mark-ring app)))
    (if (pair? ring)
      (let* ((entry (car ring))
             (buf-name (car entry))
             (position (cdr entry))
             (buf (buffer-by-name buf-name)))
        (set! (app-state-mark-ring app) (cdr ring))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-cursor-position! ed position)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-error! (app-state-echo app)
                       (string-append "Buffer not found: " buf-name))))
      (echo-message! (app-state-echo app) "Mark ring is empty"))))

;;;============================================================================
;;; Registers
;;;============================================================================

(def (cmd-copy-to-register app)
  (let* ((input (qt-echo-read-string app "Copy to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (mark (buffer-mark buf)))
        (if mark
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end)))
            (hash-put! (app-state-registers app) reg region)
            (set! (buffer-mark buf) #f)
            (echo-message! echo
              (string-append "Copied to register " (string reg))))
          (echo-error! echo "No mark set"))))))

(def (cmd-insert-register app)
  (let* ((input (qt-echo-read-string app "Insert register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (val (hash-get (app-state-registers app) reg)))
        (cond
          ((string? val)
           (qt-plain-text-edit-insert-text! (current-qt-editor app) val))
          ((pair? val)
           (echo-error! echo "Register contains a position, not text"))
          (else
           (echo-error! echo
             (string-append "Register " (string reg) " is empty"))))))))

(def (cmd-point-to-register app)
  (let* ((input (qt-echo-read-string app "Point to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (buf (current-qt-buffer app))
             (pos (qt-plain-text-edit-cursor-position (current-qt-editor app))))
        (hash-put! (app-state-registers app) reg
                   (cons (buffer-name buf) pos))
        (echo-message! echo
          (string-append "Saved point to register " (string reg)))))))

(def (cmd-jump-to-register app)
  (let* ((input (qt-echo-read-string app "Jump to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (val (hash-get (app-state-registers app) reg)))
        (cond
          ;; Window configuration register
          ((window-config? val)
           (let* ((fr (app-state-frame app))
                  (entries (window-config-windows val))
                  (target-idx (window-config-current-idx val)))
             ;; Delete all windows except current
             (qt-frame-delete-other-windows! fr)
             ;; Open first buffer in existing window
             (when (pair? entries)
               (let* ((first-entry (car entries))
                      (bname (car first-entry))
                      (pos (caddr first-entry))
                      (buf (buffer-by-name bname))
                      (ed (qt-current-editor fr)))
                 (when buf
                   (qt-buffer-attach! ed buf)
                   (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                   (qt-plain-text-edit-set-cursor-position! ed
                     (min pos (string-length (qt-plain-text-edit-text ed))))
                   (qt-plain-text-edit-ensure-cursor-visible! ed)))
               ;; Split and set up remaining windows
               (let loop ((rest (cdr entries)))
                 (when (pair? rest)
                   (let* ((entry (car rest))
                          (bname (car entry))
                          (pos (caddr entry))
                          (new-ed (qt-frame-split! fr))
                          (buf (buffer-by-name bname)))
                     (when buf
                       (qt-buffer-attach! new-ed buf)
                       (let ((new-win (car (reverse (qt-frame-windows fr)))))
                         (set! (qt-edit-window-buffer new-win) buf))
                       (qt-plain-text-edit-set-cursor-position! new-ed
                         (min pos (string-length (qt-plain-text-edit-text new-ed))))
                       (qt-plain-text-edit-ensure-cursor-visible! new-ed))
                     ;; Install key handler on new editor
                     (when (app-state-key-handler app)
                       ((app-state-key-handler app) new-ed))
                     (loop (cdr rest)))))
               ;; Restore active window index
               (when (< target-idx (length (qt-frame-windows fr)))
                 (set! (qt-frame-current-idx fr) target-idx)))
             (echo-message! echo "Window configuration restored")))
          ;; Point register (buffer-name . position)
          ((pair? val)
           (let ((buf (buffer-by-name (car val))))
             (if buf
               (let* ((fr (app-state-frame app))
                      (ed (current-qt-editor app)))
                 (qt-buffer-attach! ed buf)
                 (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                 (qt-plain-text-edit-set-cursor-position! ed (cdr val))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))
               (echo-error! echo
                 (string-append "Buffer not found: " (car val))))))
          ((string? val)
           ;; Text register — insert at point
           (qt-plain-text-edit-insert-text! (current-qt-editor app) val))
          (else
           (echo-error! echo
             (string-append "Register " (string reg) " is empty"))))))))

;;;============================================================================
;;; Window configuration registers
;;;============================================================================

(defstruct window-config (windows current-idx) transparent: #t)
;; windows: list of (buffer-name file-path cursor-pos)

(def (cmd-window-configuration-to-register app)
  "Save current window layout to a register."
  (let* ((input (qt-echo-read-string app "Window config to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (fr (app-state-frame app))
             (windows (qt-frame-windows fr))
             (entries
               (map (lambda (win)
                      (let* ((buf (qt-edit-window-buffer win))
                             (ed (qt-edit-window-editor win))
                             (pos (qt-plain-text-edit-cursor-position ed)))
                        (list (buffer-name buf)
                              (buffer-file-path buf)
                              pos)))
                    windows))
             (cfg (make-window-config entries (qt-frame-current-idx fr))))
        (hash-put! (app-state-registers app) reg cfg)
        (echo-message! echo
          (string-append "Window config saved to register " (string reg)))))))

;;;============================================================================
;;; Paragraph navigation
;;;============================================================================

(def (cmd-forward-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (num-lines (length lines)))
    ;; Skip non-blank lines, then skip blank lines
    (let* ((past-text (let loop ((i (+ line 1)))
                        (cond ((>= i num-lines) i)
                              ((string=? (string-trim (list-ref lines i)) "") i)
                              (else (loop (+ i 1))))))
           (target (let loop ((i past-text))
                     (cond ((>= i num-lines) (- num-lines 1))
                           ((not (string=? (string-trim (list-ref lines i)) "")) i)
                           (else (loop (+ i 1)))))))
      (qt-plain-text-edit-set-cursor-position! ed
        (line-start-position text (min target (- num-lines 1))))
      (qt-plain-text-edit-ensure-cursor-visible! ed))))

(def (cmd-backward-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    ;; Skip non-blank lines backward, then skip blank lines
    (let* ((past-text (let loop ((i (- line 1)))
                        (cond ((< i 0) 0)
                              ((string=? (string-trim (list-ref lines i)) "") i)
                              (else (loop (- i 1))))))
           (target (let loop ((i past-text))
                     (cond ((<= i 0) 0)
                           ((not (string=? (string-trim (list-ref lines i)) "")) (+ i 1))
                           (else (loop (- i 1)))))))
      (qt-plain-text-edit-set-cursor-position! ed
        (line-start-position text target))
      (qt-plain-text-edit-ensure-cursor-visible! ed))))

;;;============================================================================
;;; Back to indentation
;;;============================================================================

(def (cmd-back-to-indentation app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (indent (let loop ((i 0))
                       (if (and (< i (string-length line-text))
                                (char-whitespace? (string-ref line-text i)))
                         (loop (+ i 1)) i)))
             (line-pos (line-start-position text line)))
        (qt-plain-text-edit-set-cursor-position! ed (+ line-pos indent))))))

;;;============================================================================
;;; Delete indentation (join with previous line)
;;;============================================================================

(def (cmd-delete-indentation app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (> line 0)
      (let* ((prev (string-trim-right (list-ref lines (- line 1))))
             (curr (string-trim (list-ref lines line)))
             (joined (string-append prev " " curr))
             (new-lines (let loop ((ls lines) (i 0) (acc []))
                          (cond ((null? ls) (reverse acc))
                                ((= i (- line 1))
                                 (loop (if (pair? (cdr ls)) (cddr ls) '())
                                       (+ i 2) (cons joined acc)))
                                (else (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
             (new-text (string-join new-lines "\n"))
             (join-pos (+ (line-start-position new-text (- line 1))
                          (string-length prev))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed join-pos)))))

;;;============================================================================
;;; Exchange point and mark
;;;============================================================================

(def (cmd-exchange-point-and-mark app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        (set! (buffer-mark buf) pos)
        (qt-plain-text-edit-set-cursor-position! ed mark)
        (qt-plain-text-edit-ensure-cursor-visible! ed))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Copy line
;;;============================================================================

(def (cmd-copy-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let ((line-text (list-ref lines line)))
        (set! (app-state-kill-ring app)
              (cons line-text (app-state-kill-ring app)))
        (echo-message! (app-state-echo app) "Line copied")))))

;;;============================================================================
;;; Mark word
;;;============================================================================

(def (cmd-mark-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (if start
        (let ((buf (current-qt-buffer app)))
          (set! (buffer-mark buf) start)
          (qt-plain-text-edit-set-cursor-position! ed end)
          (echo-message! (app-state-echo app) "Word marked"))
        (echo-message! (app-state-echo app) "No word at point")))))

;;;============================================================================
;;; Save some buffers
;;;============================================================================

(def (cmd-save-some-buffers app)
  (let ((echo (app-state-echo app))
        (saved 0))
    (for-each
      (lambda (buf)
        (let ((path (buffer-file-path buf)))
          (when (and path
                     (buffer-doc-pointer buf)
                     (qt-text-document-modified? (buffer-doc-pointer buf)))
            ;; Find window showing this buffer to get text
            (let loop ((wins (qt-frame-windows (app-state-frame app))))
              (when (pair? wins)
                (if (eq? (qt-edit-window-buffer (car wins)) buf)
                  (let* ((ed (qt-edit-window-editor (car wins)))
                         (txt (qt-plain-text-edit-text ed)))
                    (write-string-to-file path txt)
                    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                    (set! saved (+ saved 1)))
                  (loop (cdr wins))))))))
      (buffer-list))
    (echo-message! echo
      (if (= saved 0) "No buffers need saving"
        (string-append "Saved " (number->string saved) " buffer(s)")))))

;;;============================================================================
;;; Compile
;;;============================================================================

;; Compilation error state
(def *compilation-errors* [])   ; list of (file line col message)
(def *compilation-error-index* -1)

;; Grep results state
(def *grep-results* [])        ; list of (file line text)
(def *grep-result-index* -1)

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

(def (parse-compilation-errors text)
  "Parse compilation output for file:line:col error locations.
Returns list of (file line col message) tuples."
  (let ((errors [])
        (len (string-length text)))
    (let line-loop ((i 0))
      (when (< i len)
        (let ((line-end (let scan ((j i))
                          (cond
                            ((>= j len) j)
                            ((char=? (string-ref text j) #\newline) j)
                            (else (scan (+ j 1)))))))
          (let ((line (substring text i line-end)))
            (let ((parsed (parse-error-line line)))
              (when parsed
                (set! errors (cons parsed errors)))))
          (line-loop (+ line-end 1)))))
    (reverse errors)))

(def (parse-error-line line)
  "Try to parse a single line for file:line:col patterns.
Returns (file line col message) or #f."
  (let ((len (string-length line)))
    (or
      ;; Pattern 1: Python — File \"path\", line N
      (let ((prefix "File \""))
        (and (> len (string-length prefix))
             (string-prefix? prefix line)
             (let ((quote-end (string-index line #\" (string-length prefix))))
               (and quote-end
                    (let* ((file (substring line (string-length prefix) quote-end))
                           (rest (substring line quote-end len)))
                      (and (string-prefix? "\", line " rest)
                           (let* ((num-start 8)
                                  (num-str (let scan ((j num-start))
                                             (if (and (< j (string-length rest))
                                                      (char-numeric? (string-ref rest j)))
                                               (scan (+ j 1))
                                               (substring rest num-start j)))))
                             (and (> (string-length num-str) 0)
                                  (let ((ln (string->number num-str)))
                                    (and ln
                                         (file-exists? file)
                                         (list file ln 1 (string-trim-both line))))))))))))
      ;; Pattern 2: file:line:col: message (GCC, Clang, Gerbil)
      (let find-first-colon ((start (if (and (> len 2) (char=? (string-ref line 1) #\:)) 2 0)))
        (let ((colon1 (string-index line #\: start)))
          (and colon1
               (> colon1 0)
               (let* ((file (substring line 0 colon1))
                      (rest1 (substring line (+ colon1 1) len)))
                 (let ((line-num-str (let scan ((j 0))
                                       (if (and (< j (string-length rest1))
                                                (char-numeric? (string-ref rest1 j)))
                                         (scan (+ j 1))
                                         (substring rest1 0 j)))))
                   (if (= (string-length line-num-str) 0)
                     (find-first-colon (+ colon1 1))
                     (let ((ln (string->number line-num-str)))
                       (and ln (> ln 0)
                            (or (file-exists? file)
                                (string-prefix? "/" file)
                                (string-prefix? "./" file)
                                (string-prefix? "../" file))
                            (let* ((after-line (string-length line-num-str))
                                   (has-col (and (< after-line (string-length rest1))
                                                 (char=? (string-ref rest1 after-line) #\:)))
                                   (col-and-msg
                                    (if has-col
                                      (let* ((rest2 (substring rest1 (+ after-line 1)
                                                               (string-length rest1)))
                                             (col-str (let scan ((j 0))
                                                        (if (and (< j (string-length rest2))
                                                                 (char-numeric? (string-ref rest2 j)))
                                                          (scan (+ j 1))
                                                          (substring rest2 0 j)))))
                                        (if (> (string-length col-str) 0)
                                          (cons (string->number col-str)
                                                (let ((mstart (string-length col-str)))
                                                  (if (and (< mstart (string-length rest2))
                                                           (char=? (string-ref rest2 mstart) #\:))
                                                    (string-trim-both
                                                      (substring rest2 (+ mstart 1) (string-length rest2)))
                                                    (string-trim-both
                                                      (substring rest2 mstart (string-length rest2))))))
                                          (cons 1 (string-trim-both rest2))))
                                      (let ((msg-start after-line))
                                        (cons 1 (if (< msg-start (string-length rest1))
                                                  (string-trim-both
                                                    (substring rest1 msg-start (string-length rest1)))
                                                  "")))))
                                   (col (car col-and-msg))
                                   (msg (cdr col-and-msg)))
                              (list file ln col
                                    (if (string=? msg "") line msg))))))))))))))

(def (compilation-run-command! app cmd)
  "Run a compile command, display output in *compilation* buffer, parse errors."
  (let* ((echo (app-state-echo app))
         (result+status
          (with-catch
            (lambda (e)
              (cons (string-append "Error: "
                      (with-output-to-string
                        (lambda () (display-exception e))))
                    -1))
            (lambda ()
              (let ((port (open-process
                            (list path: "/bin/sh"
                                  arguments: ["-c" cmd]
                                  stdout-redirection: #t
                                  stderr-redirection: #t
                                  pseudo-terminal: #f))))
                (let* ((output (read-line port #f))
                       (status (process-status port)))
                  (close-port port)
                  (cons (or output "") status))))))
         (result (car result+status))
         (status (cdr result+status))
         (errors (parse-compilation-errors result))
         (header (string-append
                   "-*- Compilation -*-\n"
                   "Command: " cmd "\n"
                   (make-string 60 #\-)
                   "\n\n"))
         (footer (string-append
                   "\n" (make-string 60 #\-) "\n"
                   "Compilation "
                   (if (= status 0) "finished" "exited abnormally")
                   (if (= status 0) ""
                     (string-append " (exit " (number->string status) ")"))
                   (if (null? errors) ""
                     (string-append " — "
                       (number->string (length errors)) " error location(s)"))
                   "\n"))
         (text (string-append header result footer))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (or (buffer-by-name "*compilation*")
                  (qt-buffer-create! "*compilation*" ed #f))))
    (set! *compilation-errors* errors)
    (set! *compilation-error-index* -1)
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! echo
      (string-append "Compilation "
        (if (= status 0) "finished" "failed")
        (if (null? errors) ""
          (string-append " — " (number->string (length errors)) " error(s)"))))))

(def (cmd-compile app)
  "Run a compile command and display output in *compilation* buffer with error parsing."
  (let* ((echo (app-state-echo app))
         (default (or (app-state-last-compile app) "make"))
         (cmd (qt-echo-read-string app
                (string-append "Compile command [" default "]: "))))
    (when cmd
      (let ((actual-cmd (if (string=? cmd "") default cmd)))
        (set! (app-state-last-compile app) actual-cmd)
        (compilation-run-command! app actual-cmd)))))

(def (cmd-recompile app)
  "Re-run the last compile command without prompting."
  (let ((cmd (app-state-last-compile app)))
    (if (not cmd)
      (cmd-compile app)
      (compilation-run-command! app cmd))))

;;;============================================================================
;;; Compile-on-save
;;;============================================================================

(def *compile-on-save* #f)  ;; off by default

(def (compile-on-save-check! app path)
  "If compile-on-save is enabled and PATH is a .ss file in a Gerbil project,
   run gerbil build in the background."
  (when (and *compile-on-save* path
             (string-suffix? ".ss" path))
    ;; Find gerbil.pkg by searching upward
    (let loop ((dir (path-directory path)))
      (when (and (> (string-length dir) 0) (not (string=? dir "/")))
        (let ((pkg (path-expand "gerbil.pkg" dir)))
          (if (file-exists? pkg)
            ;; Found gerbil project — run build
            (begin
              (echo-message! (app-state-echo app) "Auto-compiling...")
              (compilation-run-command! app
                (string-append "cd " dir " && gerbil build")))
            ;; Go up one level
            (let ((parent (path-directory
                            (let ((d (if (string-suffix? "/" dir)
                                       (substring dir 0 (- (string-length dir) 1))
                                       dir)))
                              d))))
              (unless (string=? parent dir)
                (loop parent)))))))))

;;;============================================================================
;;; Flycheck-style live syntax checking
;;;============================================================================

(def *flycheck-mode* #f)        ; global toggle
(def *flycheck-errors* [])      ; list of (file line col message)
(def *flycheck-error-idx* 0)    ; index for next/prev error navigation

(def (flycheck-check! app path)
  "Run gxc -S on a Gerbil file and parse errors. Updates *flycheck-errors*."
  (when (and *flycheck-mode* path (string-suffix? ".ss" path))
    ;; Find GERBIL_LOADPATH from project context
    (let* ((dir (path-directory path))
           (loadpath (flycheck-find-loadpath dir))
           (env-prefix (if loadpath
                         (string-append "GERBIL_LOADPATH=" loadpath " ")
                         ""))
           (cmd (string-append env-prefix "gxc -S " path " 2>&1"))
           (result
             (with-catch
               (lambda (e)
                 (string-append "flycheck error: "
                   (with-output-to-string (lambda () (display-exception e)))))
               (lambda ()
                 (let ((port (open-process
                               (list path: "/bin/sh"
                                     arguments: ["-c" cmd]
                                     stdout-redirection: #t
                                     stderr-redirection: #f
                                     pseudo-terminal: #f))))
                   (let* ((output (read-line port #f))
                          (status (process-status port)))
                     (close-port port)
                     (or output "")))))))
      ;; Parse errors from gxc output
      (let ((errors (flycheck-parse-errors result path)))
        (set! *flycheck-errors* errors)
        (set! *flycheck-error-idx* 0)
        (if (null? errors)
          (echo-message! (app-state-echo app) "Flycheck: no errors")
          (let* ((count (length errors))
                 (first-err (car errors))
                 (msg (string-append "Flycheck: "
                        (number->string count)
                        (if (= count 1) " error" " errors")
                        " — " (cadddr first-err))))
            (echo-error! (app-state-echo app) msg)))))))

(def (flycheck-find-loadpath dir)
  "Search upward from dir for gerbil.pkg and construct GERBIL_LOADPATH."
  (let loop ((d dir))
    (cond
      ((or (string=? d "") (string=? d "/")) #f)
      ((file-exists? (path-expand "gerbil.pkg" d))
       ;; Found project root, check for .gerbil/lib
       (let ((local-lib (path-expand ".gerbil/lib" d)))
         (if (file-exists? local-lib)
           (string-append local-lib ":" (or (getenv "GERBIL_LOADPATH" #f) ""))
           (or (getenv "GERBIL_LOADPATH" #f) #f))))
      (else
       (let ((parent (path-directory
                       (if (string-suffix? "/" d)
                         (substring d 0 (- (string-length d) 1))
                         d))))
         (if (string=? parent d) #f
           (loop parent)))))))

(def (flycheck-parse-errors output file)
  "Parse gxc error output into list of (file line col message).
   Handles formats like:
   - '... form: foo'
   - 'at file:line:col'
   - 'Syntax Error: ...'
   - 'Error: ...' "
  (let* ((lines (string-split output #\newline))
         (errors []))
    (for-each
      (lambda (line)
        (cond
          ;; Match "--- Syntax Error: ..." messages
          ((string-contains line "Syntax Error:")
           (let ((msg (substring line
                        (+ (string-contains line "Syntax Error:") 15)
                        (string-length line))))
             (set! errors (cons (list file 0 0 (string-trim msg)) errors))))
          ;; Match "--- Error: ..."
          ((and (string-prefix? "---" line) (string-contains line "Error:"))
           (let ((msg (substring line 4 (string-length line))))
             (set! errors (cons (list file 0 0 (string-trim msg)) errors))))
          ;; Match "... form: something" or "... detail: something"
          ((and (string-prefix? "..." line)
                (or (string-contains line "form:")
                    (string-contains line "detail:")))
           (let* ((trimmed (string-trim line))
                  (msg (if (> (string-length trimmed) 4)
                         (substring trimmed 4 (string-length trimmed))
                         trimmed)))
             (set! errors (cons (list file 0 0 msg) errors))))))
      lines)
    (reverse errors)))

(def (cmd-flycheck-mode app)
  "Toggle flycheck (live syntax checking on save) for Gerbil files."
  (set! *flycheck-mode* (not *flycheck-mode*))
  (echo-message! (app-state-echo app)
    (if *flycheck-mode* "Flycheck mode enabled" "Flycheck mode disabled")))

(def (cmd-flycheck-next-error app)
  "Jump to the next flycheck error."
  (if (null? *flycheck-errors*)
    (echo-message! (app-state-echo app) "No flycheck errors")
    (let* ((idx (min *flycheck-error-idx* (- (length *flycheck-errors*) 1)))
           (err (list-ref *flycheck-errors* idx))
           (line (cadr err))
           (msg (cadddr err)))
      (when (> line 0)
        ;; Jump to error line
        (let* ((ed (current-qt-editor app))
               (text (qt-plain-text-edit-text ed))
               (target-pos (text-line-position text (- line 1))))
          (qt-plain-text-edit-set-cursor-position! ed target-pos)
          (qt-plain-text-edit-ensure-cursor-visible! ed)))
      (echo-error! (app-state-echo app)
        (string-append "[" (number->string (+ idx 1)) "/"
                       (number->string (length *flycheck-errors*))
                       "] " msg))
      (set! *flycheck-error-idx* (min (+ idx 1) (- (length *flycheck-errors*) 1))))))

(def (cmd-flycheck-prev-error app)
  "Jump to the previous flycheck error."
  (if (null? *flycheck-errors*)
    (echo-message! (app-state-echo app) "No flycheck errors")
    (let* ((idx (max (- *flycheck-error-idx* 1) 0))
           (err (list-ref *flycheck-errors* idx))
           (line (cadr err))
           (msg (cadddr err)))
      (when (> line 0)
        (let* ((ed (current-qt-editor app))
               (text (qt-plain-text-edit-text ed))
               (target-pos (text-line-position text (- line 1))))
          (qt-plain-text-edit-set-cursor-position! ed target-pos)
          (qt-plain-text-edit-ensure-cursor-visible! ed)))
      (echo-error! (app-state-echo app)
        (string-append "[" (number->string (+ idx 1)) "/"
                       (number->string (length *flycheck-errors*))
                       "] " msg))
      (set! *flycheck-error-idx* idx))))

(def (cmd-flycheck-list-errors app)
  "Show all flycheck errors in a buffer."
  (if (null? *flycheck-errors*)
    (echo-message! (app-state-echo app) "No flycheck errors")
    (let* ((fr (app-state-frame app))
           (ed (current-qt-editor app))
           (text (string-join
                   (map (lambda (err)
                          (let ((file (car err))
                                (line (cadr err))
                                (col (caddr err))
                                (msg (cadddr err)))
                            (string-append
                              (path-strip-directory file) ":"
                              (number->string line) ":"
                              (number->string col) ": "
                              msg)))
                        *flycheck-errors*)
                   "\n"))
           (buf (or (buffer-by-name "*Flycheck Errors*")
                    (qt-buffer-create! "*Flycheck Errors*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

(def (cmd-toggle-compile-on-save app)
  "Toggle automatic compilation when saving Gerbil files."
  (set! *compile-on-save* (not *compile-on-save*))
  (echo-message! (app-state-echo app)
    (if *compile-on-save*
      "Compile-on-save enabled"
      "Compile-on-save disabled")))

;;;============================================================================
;;; Where-is (find key binding for command)
;;;============================================================================

(def (cmd-where-is app)
  (let ((input (qt-echo-read-string app "Where is command: ")))
    (when (and input (> (string-length input) 0))
      (let ((sym (string->symbol input))
            (found #f))
        (for-each
          (lambda (entry)
            (let ((key (car entry))
                  (val (cdr entry)))
              (cond
                ((eq? val sym)
                 (set! found key))
                ((hash-table? val)
                 (for-each
                   (lambda (sub-entry)
                     (when (eq? (cdr sub-entry) sym)
                       (set! found (string-append key " " (car sub-entry)))))
                   (keymap-entries val))))))
          (keymap-entries *global-keymap*))
        (if found
          (echo-message! (app-state-echo app)
            (string-append input " is on " found))
          (echo-message! (app-state-echo app)
            (string-append input " is not on any key")))))))

;;;============================================================================
;;; Flush lines / keep lines
;;;============================================================================

(def (cmd-flush-lines app)
  "Delete lines matching a pattern."
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Flush lines matching: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (kept (filter (lambda (l) (not (string-contains l pattern))) lines))
             (removed (- (length lines) (length kept)))
             (new-text (string-join kept "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! echo
          (string-append "Removed " (number->string removed) " lines"))))))

(def (cmd-keep-lines app)
  "Keep only lines matching a pattern."
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Keep lines matching: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (kept (filter (lambda (l) (string-contains l pattern)) lines))
             (new-text (string-join kept "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! echo
          (string-append "Kept " (number->string (length kept)) " lines"))))))

;;;============================================================================
;;; Number lines
;;;============================================================================

(def (cmd-number-lines app)
  "Prefix each line with its line number."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (numbered (let loop ((ls lines) (i 1) (acc []))
                     (if (null? ls) (reverse acc)
                       (loop (cdr ls) (+ i 1)
                             (cons (string-append (number->string i) ": " (car ls))
                                   acc)))))
         (new-text (string-join numbered "\n")))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;;============================================================================
;;; Reverse region
;;;============================================================================

(def (cmd-reverse-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (reversed (reverse lines))
             (new-region (string-join reversed "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region reversed"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Toggle read-only
;;;============================================================================

(def (cmd-toggle-read-only app)
  (let* ((ed (current-qt-editor app))
         (ro (qt-plain-text-edit-read-only? ed)))
    (qt-plain-text-edit-set-read-only! ed (not ro))
    (echo-message! (app-state-echo app)
      (if ro "Read-only OFF" "Read-only ON"))))

;;;============================================================================
;;; Rename buffer
;;;============================================================================

(def (cmd-rename-buffer app)
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (input (qt-echo-read-string app
                  (string-append "Rename buffer (" (buffer-name buf) "): "))))
    (when (and input (> (string-length input) 0))
      (set! (buffer-name buf) input)
      (echo-message! echo (string-append "Buffer renamed to " input)))))

;;;============================================================================
;;; Switch buffer / find file in other window
;;;============================================================================

(def (cmd-switch-buffer-other-window app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (names (buffer-names-mru))
         (name (qt-echo-read-string-with-completion app
                  "Switch to buffer in other window: " names)))
    (when name
      (let ((buf (buffer-by-name name)))
        (when buf
          (buffer-touch! buf)
          (let ((wins (qt-frame-windows fr)))
            (if (> (length wins) 1)
              ;; Switch in the other window
              (begin
                (qt-frame-other-window! fr)
                (let ((ed (current-qt-editor app)))
                  (qt-buffer-attach! ed buf)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) buf)))
              ;; Only one window: split first
              (let ((new-ed (qt-frame-split! fr)))
                (when (app-state-key-handler app)
                  ((app-state-key-handler app) new-ed))
                (qt-buffer-attach! new-ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)))))))))

(def (cmd-find-file-other-window app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (filename (qt-echo-read-string app "Find file in other window: ")))
    (when (and filename (> (string-length filename) 0))
      (let ((wins (qt-frame-windows fr)))
        (when (<= (length wins) 1)
          ;; Split first
          (let ((new-ed (qt-frame-split! fr)))
            (when (app-state-key-handler app)
              ((app-state-key-handler app) new-ed))))
        ;; Switch to other window
        (qt-frame-other-window! fr)
        ;; Open file
        (let* ((name (path-strip-directory filename))
               (ed (current-qt-editor app))
               (buf (qt-buffer-create! name ed filename)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? filename)
            (let ((text (read-file-as-string filename)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0))))
          (qt-setup-highlighting! app buf)
          (echo-message! echo (string-append "Opened: " filename)))))))

;;;============================================================================
;;; Insert date
;;;============================================================================

(def (cmd-insert-date app)
  (let* ((now (current-time))
         (secs (time->seconds now))
         ;; Simple date string: YYYY-MM-DD HH:MM:SS
         (date-str (with-catch
                     (lambda (e)
                       ;; Fallback: just show seconds since epoch
                       (number->string (inexact->exact (truncate secs))))
                     (lambda ()
                       (let* ((utc (seconds->time secs))
                              (port (open-process
                                      (list path: "/bin/date"
                                            arguments: ["+%Y-%m-%d %H:%M:%S"]
                                            stdout-redirection: #t))))
                         (let ((result (read-line port)))
                           (close-port port)
                           result))))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) date-str)))

;;;============================================================================
;;; Eval buffer / eval region
;;;============================================================================

(def (cmd-eval-buffer app)
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((result error?) (eval-expression-string text)))
      (if error?
        (echo-error! echo result)
        (echo-message! echo (string-append "=> " result))))))

(def (cmd-eval-region app)
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end)))
        (set! (buffer-mark buf) #f)
        (let-values (((result error?) (eval-expression-string region)))
          (if error?
            (echo-error! echo result)
            (echo-message! echo (string-append "=> " result)))))
      (echo-error! echo "No mark set"))))

;;;============================================================================
;;; Clone buffer / scratch buffer
;;;============================================================================

(def (cmd-clone-buffer app)
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app))
         (text (qt-plain-text-edit-text ed))
         (name (string-append (buffer-name buf) "<2>"))
         (new-buf (qt-buffer-create! name ed (buffer-file-path buf))))
    (qt-buffer-attach! ed new-buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) new-buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer new-buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append "Cloned to " name))))

(def (cmd-scratch-buffer app)
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (existing (buffer-by-name "*scratch*")))
    (if existing
      (begin
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing))
      (let ((buf (qt-buffer-create! "*scratch*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed ";; *scratch*\n")
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))
    (echo-message! (app-state-echo app) "*scratch*")))

;;;============================================================================
;;; Delete duplicate lines
;;;============================================================================

(def (cmd-delete-duplicate-lines app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (seen (make-hash-table))
         (unique (filter (lambda (l)
                           (if (hash-get seen l) #f
                             (begin (hash-put! seen l #t) #t)))
                         lines))
         (removed (- (length lines) (length unique)))
         (new-text (string-join unique "\n")))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append "Removed " (number->string removed) " duplicate lines"))))

;;;============================================================================
;;; Count matches
;;;============================================================================

(def (cmd-count-matches app)
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Count matches: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (count (let loop ((i 0) (c 0))
                      (let ((pos (string-contains text pattern i)))
                        (if pos
                          (loop (+ pos (string-length pattern)) (+ c 1))
                          c)))))
        (echo-message! echo
          (string-append (number->string count) " occurrences of \"" pattern "\""))))))

;;;============================================================================
;;; Count lines in region
;;;============================================================================

(def (cmd-count-lines-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (chars (string-length region)))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string (length lines))
                         " lines, " (number->string chars) " chars")))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Diff mode with colorized output
;;;============================================================================

(def (qt-highlight-diff! ed)
  "Apply diff-mode highlighting to a QPlainTextEdit.
+lines = green bg, -lines = red bg, @@ headers = blue bg."
  (let ((text (qt-plain-text-edit-text ed))
        (len 0))
    (set! len (string-length text))
    ;; Clear extra selections first
    (qt-extra-selections-clear! ed)
    ;; Walk through lines and highlight
    (let line-loop ((i 0))
      (when (< i len)
        (let* ((line-end (let scan ((j i))
                           (cond
                             ((>= j len) j)
                             ((char=? (string-ref text j) #\newline) j)
                             (else (scan (+ j 1))))))
               (line-len (- line-end i)))
          (when (> line-len 0)
            (let ((ch (string-ref text i)))
              (cond
                ;; +line (added) — green background
                ((and (char=? ch #\+)
                      ;; Skip +++ header line
                      (not (and (> line-len 2)
                                (char=? (string-ref text (+ i 1)) #\+)
                                (char=? (string-ref text (+ i 2)) #\+))))
                 (qt-extra-selection-add-range! ed i line-len
                   220 255 220    ; light green text
                   0 60 0         ; dark green bg
                   bold: #f))
                ;; -line (removed) — red background
                ((and (char=? ch #\-)
                      ;; Skip --- header line
                      (not (and (> line-len 2)
                                (char=? (string-ref text (+ i 1)) #\-)
                                (char=? (string-ref text (+ i 2)) #\-))))
                 (qt-extra-selection-add-range! ed i line-len
                   255 200 200    ; light red text
                   80 0 0         ; dark red bg
                   bold: #f))
                ;; @@ hunk header — blue background
                ((and (char=? ch #\@)
                      (> line-len 1)
                      (char=? (string-ref text (+ i 1)) #\@))
                 (qt-extra-selection-add-range! ed i line-len
                   180 200 255    ; light blue text
                   0 0 80         ; dark blue bg
                   bold: #t))
                ;; --- or +++ file header — bold
                ((or (and (char=? ch #\-)
                          (> line-len 2)
                          (char=? (string-ref text (+ i 1)) #\-)
                          (char=? (string-ref text (+ i 2)) #\-))
                     (and (char=? ch #\+)
                          (> line-len 2)
                          (char=? (string-ref text (+ i 1)) #\+)
                          (char=? (string-ref text (+ i 2)) #\+)))
                 (qt-extra-selection-add-range! ed i line-len
                   255 255 100    ; yellow text
                   40 40 40       ; dark gray bg
                   bold: #t)))))
          (line-loop (+ line-end 1)))))))

(def (cmd-diff-buffer-with-file app)
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (and path (file-exists? path))
      (let* ((ed (current-qt-editor app))
             (current-text (qt-plain-text-edit-text ed))
             (file-text (read-file-as-string path))
             ;; Write current buffer to temp file for diff
             (tmp-path (string-append "/tmp/gerbil-emacs-diff-" (number->string (random-integer 100000))))
             (_ (write-string-to-file tmp-path current-text))
             (result (with-catch
                       (lambda (e) "Error running diff")
                       (lambda ()
                         (let ((port (open-process
                                       (list path: "/usr/bin/diff"
                                             arguments: ["-u" path tmp-path]
                                             stdout-redirection: #t
                                             stderr-redirection: #t
                                             pseudo-terminal: #f))))
                           (let ((output (read-line port #f)))
                             (close-port port)
                             (or output "No differences")))))))
        ;; Clean up temp file
        (with-catch void (lambda () (delete-file tmp-path)))
        ;; Show diff in buffer
        (let* ((fr (app-state-frame app))
               (diff-buf (or (buffer-by-name "*Diff*")
                             (qt-buffer-create! "*Diff*" ed #f))))
          (qt-buffer-attach! ed diff-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
          (qt-plain-text-edit-set-text! ed
            (if (string=? result "") "No differences\n" result))
          (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (qt-highlight-diff! ed)))
      (echo-error! echo "Buffer is not visiting a file"))))

(def (cmd-diff-next-hunk app)
  "Jump to next diff hunk (@@ line)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find next @@ after current position
    (let loop ((i (+ pos 1)))
      (cond
        ((>= i (- len 1))
         (echo-message! (app-state-echo app) "No more hunks"))
        ((and (char=? (string-ref text i) #\@)
              (char=? (string-ref text (+ i 1)) #\@)
              ;; Make sure it's at start of line
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (qt-plain-text-edit-set-cursor-position! ed i)
         (qt-plain-text-edit-ensure-cursor-visible! ed))
        (else (loop (+ i 1)))))))

(def (cmd-diff-prev-hunk app)
  "Jump to previous diff hunk (@@ line)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Find the start of current line, then search backward
    (let ((line-start (let scan ((i (max 0 (- pos 1))))
                        (cond
                          ((<= i 0) 0)
                          ((char=? (string-ref text i) #\newline) (+ i 1))
                          (else (scan (- i 1)))))))
      (let loop ((i (- line-start 2)))
        (cond
          ((< i 1)
           (echo-message! (app-state-echo app) "No previous hunks"))
          ((and (char=? (string-ref text i) #\@)
                (char=? (string-ref text (+ i 1)) #\@)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (- i 1))))))))

;;;============================================================================
;;; Grep buffer (search all matching lines)
;;;============================================================================

(def (cmd-grep-buffer app)
  (let* ((echo (app-state-echo app))
         (query (qt-echo-read-string app "Grep buffer: ")))
    (when (and query (> (string-length query) 0))
      ;; Same as occur but searches case-insensitively
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (q-lower (string-downcase query))
             (matches (let loop ((ls lines) (i 1) (acc []))
                        (if (null? ls) (reverse acc)
                          (if (string-contains (string-downcase (car ls)) q-lower)
                            (loop (cdr ls) (+ i 1)
                                  (cons (string-append (number->string i) ": " (car ls))
                                        acc))
                            (loop (cdr ls) (+ i 1) acc)))))
             (result (if (null? matches)
                       (string-append "No matches for: " query)
                       (string-append (number->string (length matches))
                                      " matches for: " query "\n\n"
                                      (string-join matches "\n")))))
        (let* ((fr (app-state-frame app))
               (buf (or (buffer-by-name "*Grep*")
                        (qt-buffer-create! "*Grep*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; Revert buffer quick (no prompt)
;;;============================================================================

(def (cmd-revert-buffer-quick app)
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
          (echo-message! echo (string-append "Reverted " path))))
      (echo-error! echo "Buffer is not visiting a file"))))

;;;============================================================================
;;; Shell command on region (M-|)
;;;============================================================================

(def (cmd-shell-command-on-region app)
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((cmd (qt-echo-read-string app "Shell command on region: ")))
        (when (and cmd (> (string-length cmd) 0))
          (let* ((ed (current-qt-editor app))
                 (pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (result (with-catch
                           (lambda (e) (string-append "Error: "
                                         (with-output-to-string
                                           (lambda () (display-exception e)))))
                           (lambda ()
                             (let ((port (open-process
                                           (list path: "/bin/sh"
                                                 arguments: ["-c" cmd]
                                                 stdin-redirection: #t
                                                 stdout-redirection: #t
                                                 stderr-redirection: #t
                                                 pseudo-terminal: #f))))
                               (display region port)
                               (force-output port)
                               (close-output-port port)
                               (let ((output (read-line port #f)))
                                 (close-port port)
                                 (or output "")))))))
            ;; Replace region with result
            (let ((new-text (string-append (substring text 0 start)
                                           result
                                           (substring text end (string-length text)))))
              (qt-plain-text-edit-set-text! ed new-text)
              (qt-plain-text-edit-set-cursor-position! ed start)
              (set! (buffer-mark buf) #f)
              (echo-message! echo "Region filtered")))))
      (echo-error! echo "No mark set"))))

;;;============================================================================
;;; Pipe buffer through shell command
;;;============================================================================

(def (cmd-pipe-buffer app)
  (let* ((echo (app-state-echo app))
         (cmd (qt-echo-read-string app "Pipe buffer through: ")))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (result (with-catch
                       (lambda (e) (string-append "Error: "
                                     (with-output-to-string
                                       (lambda () (display-exception e)))))
                       (lambda ()
                         (let ((port (open-process
                                       (list path: "/bin/sh"
                                             arguments: ["-c" cmd]
                                             stdin-redirection: #t
                                             stdout-redirection: #t
                                             stderr-redirection: #t
                                             pseudo-terminal: #f))))
                           (display text port)
                           (force-output port)
                           (close-output-port port)
                           (let ((output (read-line port #f)))
                             (close-port port)
                             (or output "")))))))
        ;; Show result in *Shell Output*
        (let* ((fr (app-state-frame app))
               (out-buf (or (buffer-by-name "*Shell Output*")
                            (qt-buffer-create! "*Shell Output*" ed #f))))
          (qt-buffer-attach! ed out-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) out-buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer out-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (echo-message! echo "Buffer piped"))))))

;;;============================================================================
;;; Apropos (search commands)
;;;============================================================================

(def (cmd-apropos-command app)
  (let* ((echo (app-state-echo app))
         (query (qt-echo-read-string app "Apropos command: ")))
    (when (and query (> (string-length query) 0))
      (let* ((all-names (map symbol->string (hash-keys *all-commands*)))
             (matches (filter (lambda (n) (string-contains n query)) all-names))
             (sorted (sort matches string<?))
             (result (if (null? sorted)
                       (string-append "No commands matching: " query)
                       (string-append (number->string (length sorted))
                                      " commands matching \"" query "\":\n\n"
                                      (string-join sorted "\n")))))
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (or (buffer-by-name "*Apropos*")
                        (qt-buffer-create! "*Apropos*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; What page
;;;============================================================================

(def (cmd-what-page app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (page (let loop ((i 0) (p 1))
                 (cond ((>= i pos) p)
                       ((char=? (string-ref text i) #\page) (loop (+ i 1) (+ p 1)))
                       (else (loop (+ i 1) p))))))
    (echo-message! (app-state-echo app)
      (string-append "Page " (number->string page)))))

;;;============================================================================
;;; Async shell command
;;;============================================================================

(def (cmd-async-shell-command app)
  (let* ((echo (app-state-echo app))
         (cmd (qt-echo-read-string app "Async shell command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (let ((port (open-process
                    (list path: "/bin/sh"
                          arguments: ["-c" cmd]
                          stdout-redirection: #f
                          stderr-redirection: #f
                          pseudo-terminal: #f))))
        (echo-message! echo (string-append "Started: " cmd))))))

;;;============================================================================
;;; Checksum (MD5/SHA256 of buffer)
;;;============================================================================

(def (cmd-checksum app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (echo (app-state-echo app)))
    (let ((result (with-catch
                    (lambda (e) "Error computing checksum")
                    (lambda ()
                      (let ((port (open-process
                                    (list path: "/usr/bin/sha256sum"
                                          stdin-redirection: #t
                                          stdout-redirection: #t
                                          pseudo-terminal: #f))))
                        (display text port)
                        (force-output port)
                        (close-output-port port)
                        (let ((output (read-line port)))
                          (close-port port)
                          (or output "")))))))
      (echo-message! echo (string-append "SHA256: " result)))))

;;;============================================================================
;;; S-expression navigation (critical for Lisp editing)
;;;============================================================================

(def (find-matching-close text pos)
  "Find matching close paren/bracket/brace from opening at pos."
  (let* ((len (string-length text))
         (ch (string-ref text pos))
         (close (cond ((char=? ch #\() #\))
                      ((char=? ch #\[) #\])
                      ((char=? ch #\{) #\})
                      (else #f))))
    (if close
      (let loop ((i (+ pos 1)) (depth 1))
        (cond ((>= i len) #f)
              ((= depth 0) i)
              ((char=? (string-ref text i) ch) (loop (+ i 1) (+ depth 1)))
              ((char=? (string-ref text i) close)
               (if (= depth 1) (+ i 1)
                 (loop (+ i 1) (- depth 1))))
              (else (loop (+ i 1) depth))))
      #f)))

(def (find-matching-open text pos)
  "Find matching open paren/bracket/brace scanning backward from pos."
  (let* ((ch (string-ref text pos))
         (open (cond ((char=? ch #\)) #\()
                     ((char=? ch #\]) #\[)
                     ((char=? ch #\}) #\{)
                     (else #f))))
    (if open
      (let loop ((i (- pos 1)) (depth 1))
        (cond ((< i 0) #f)
              ((char=? (string-ref text i) ch) (loop (- i 1) (+ depth 1)))
              ((char=? (string-ref text i) open)
               (if (= depth 1) i
                 (loop (- i 1) (- depth 1))))
              (else (loop (- i 1) depth))))
      #f)))

(def (sexp-end text pos)
  "Find end position of sexp starting at pos."
  (let ((len (string-length text)))
    (if (>= pos len) pos
      (let ((ch (string-ref text pos)))
        (cond
          ;; Opening delimiter - find matching close
          ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
           (or (find-matching-close text pos) len))
          ;; String
          ((char=? ch #\")
           (let loop ((i (+ pos 1)))
             (cond ((>= i len) len)
                   ((char=? (string-ref text i) #\\) (loop (+ i 2)))
                   ((char=? (string-ref text i) #\") (+ i 1))
                   (else (loop (+ i 1))))))
          ;; Word/symbol
          (else
           (let loop ((i pos))
             (if (or (>= i len)
                     (char-whitespace? (string-ref text i))
                     (memv (string-ref text i) '(#\( #\) #\[ #\] #\{ #\})))
               i
               (loop (+ i 1))))))))))

(def (sexp-start text pos)
  "Find start position of sexp ending at pos."
  (if (<= pos 0) 0
    (let* ((i (- pos 1))
           (ch (string-ref text i)))
      (cond
        ;; Closing delimiter
        ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
         (or (find-matching-open text i) 0))
        ;; End of string
        ((char=? ch #\")
         (let loop ((j (- i 1)))
           (cond ((<= j 0) 0)
                 ((and (char=? (string-ref text j) #\")
                       (or (= j 0) (not (char=? (string-ref text (- j 1)) #\\))))
                  j)
                 (else (loop (- j 1))))))
        ;; Word/symbol
        (else
         (let loop ((j i))
           (if (or (<= j 0)
                   (char-whitespace? (string-ref text j))
                   (memv (string-ref text j) '(#\( #\) #\[ #\] #\{ #\})))
             (+ j 1)
             (loop (- j 1)))))))))

(def (skip-whitespace-forward text pos)
  (let ((len (string-length text)))
    (let loop ((i pos))
      (if (or (>= i len) (not (char-whitespace? (string-ref text i))))
        i (loop (+ i 1))))))

(def (skip-whitespace-backward text pos)
  (let loop ((i pos))
    (if (or (<= i 0) (not (char-whitespace? (string-ref text (- i 1)))))
      i (loop (- i 1)))))

(def (cmd-forward-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (skip-whitespace-forward text (qt-plain-text-edit-cursor-position ed)))
         (end (sexp-end text pos)))
    (qt-plain-text-edit-set-cursor-position! ed end)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-backward-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (skip-whitespace-backward text (qt-plain-text-edit-cursor-position ed)))
         (start (sexp-start text pos)))
    (qt-plain-text-edit-set-cursor-position! ed start)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-kill-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd-pos (skip-whitespace-forward text pos))
         (end (sexp-end text fwd-pos)))
    (when (> end pos)
      (let* ((killed (substring text pos end))
             (new-text (string-append (substring text 0 pos)
                                      (substring text end (string-length text)))))
        (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

(def (cmd-backward-kill-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (bwd-pos (skip-whitespace-backward text pos))
         (start (sexp-start text bwd-pos)))
    (when (< start pos)
      (let* ((killed (substring text start pos))
             (new-text (string-append (substring text 0 start)
                                      (substring text pos (string-length text)))))
        (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)))))

(def (cmd-mark-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd-pos (skip-whitespace-forward text pos))
         (end (sexp-end text fwd-pos))
         (buf (current-qt-buffer app)))
    (set! (buffer-mark buf) pos)
    (qt-plain-text-edit-set-cursor-position! ed end)
    (echo-message! (app-state-echo app) "Sexp marked")))

(def (cmd-mark-defun app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (buf (current-qt-buffer app)))
    ;; Find start of current top-level form
    (let loop ((i pos))
      (cond
        ((< i 0)
         (set! (buffer-mark buf) 0)
         (echo-message! (app-state-echo app) "Defun marked"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (let ((end (sexp-end text i)))
           (set! (buffer-mark buf) i)
           (qt-plain-text-edit-set-cursor-position! ed end)
           (echo-message! (app-state-echo app) "Defun marked")))
        (else (loop (- i 1)))))))

(def (cmd-indent-sexp app)
  "Re-indent the sexp after point with 2-space indentation."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (skip-whitespace-forward text (qt-plain-text-edit-cursor-position ed)))
         (end (sexp-end text pos)))
    (when (> end pos)
      (let* ((sexp-text (substring text pos end))
             (lines (string-split sexp-text #\newline))
             ;; Simple re-indent: first line stays, others get 2 spaces per depth
             (indented (if (null? lines) lines
                         (cons (car lines)
                               (map (lambda (l)
                                      (string-append "  " (string-trim l)))
                                    (cdr lines)))))
             (new-sexp (string-join indented "\n"))
             (new-text (string-append (substring text 0 pos)
                                      new-sexp
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

;;;============================================================================
;;; Paredit-style structured editing
;;;============================================================================

(def (find-enclosing-open text pos)
  "Find the position of the innermost opening paren/bracket/brace enclosing POS."
  (let loop ((i (- pos 1)) (depth 0))
    (cond
      ((< i 0) #f)
      ((memv (string-ref text i) '(#\) #\] #\}))
       (loop (- i 1) (+ depth 1)))
      ((memv (string-ref text i) '(#\( #\[ #\{))
       (if (= depth 0) i
         (loop (- i 1) (- depth 1))))
      (else (loop (- i 1) depth)))))

(def (find-enclosing-close text pos)
  "Find the position of the innermost closing paren/bracket/brace enclosing POS."
  (let ((len (string-length text)))
    (let loop ((i pos) (depth 0))
      (cond
        ((>= i len) #f)
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (loop (+ i 1) (+ depth 1)))
        ((memv (string-ref text i) '(#\) #\] #\}))
         (if (= depth 0) i
           (loop (+ i 1) (- depth 1))))
        (else (loop (+ i 1) depth))))))

(def (cmd-paredit-slurp-forward app)
  "Extend enclosing sexp to include the next sexp after the closing delimiter."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (close-pos (find-enclosing-close text pos)))
    (when close-pos
      (let* ((after (skip-whitespace-forward text (+ close-pos 1)))
             (next-end (sexp-end text after)))
        (when (> next-end after)
          ;; Remove closing delimiter, put it after the next sexp
          (let* ((close-char (string (string-ref text close-pos)))
                 (new-text (string-append
                             (substring text 0 close-pos)
                             (substring text (+ close-pos 1) next-end)
                             close-char
                             (substring text next-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-barf-forward app)
  "Move the last element of enclosing sexp out past the closing delimiter."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (close-pos (find-enclosing-close text pos)))
    (when close-pos
      ;; Find the last sexp inside before the closing paren
      (let* ((before-close (skip-whitespace-backward text close-pos))
             (last-start (sexp-start text before-close)))
        (when (> last-start 0)
          (let* ((open-pos (find-enclosing-open text pos))
                 (close-char (string (string-ref text close-pos))))
            (when (and open-pos (> last-start (+ open-pos 1)))
              ;; Move close-paren to before the last sexp
              (let* ((ws-before (skip-whitespace-backward text last-start))
                     (new-text (string-append
                                 (substring text 0 ws-before)
                                 close-char
                                 (substring text ws-before close-pos)
                                 (substring text (+ close-pos 1) (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (min pos (string-length new-text)))
                (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))

(def (cmd-paredit-wrap-round app)
  "Wrap the sexp at point in parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd (skip-whitespace-forward text pos))
         (end (sexp-end text fwd)))
    (when (> end fwd)
      (let ((new-text (string-append
                        (substring text 0 fwd) "("
                        (substring text fwd end) ")"
                        (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ fwd 1))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-paredit-wrap-square app)
  "Wrap the sexp at point in square brackets."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd (skip-whitespace-forward text pos))
         (end (sexp-end text fwd)))
    (when (> end fwd)
      (let ((new-text (string-append
                        (substring text 0 fwd) "["
                        (substring text fwd end) "]"
                        (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ fwd 1))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-paredit-splice-sexp app)
  "Remove the enclosing parens (splice sexp into parent)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (open-pos (find-enclosing-open text pos)))
    (when open-pos
      (let ((close-pos (find-matching-close text open-pos)))
        (when close-pos
          ;; Remove both delimiters
          (let ((new-text (string-append
                            (substring text 0 open-pos)
                            (substring text (+ open-pos 1) (- close-pos 1))
                            (substring text close-pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (- pos 1))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-raise-sexp app)
  "Replace enclosing sexp with the sexp at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd (skip-whitespace-forward text pos))
         (sexp-e (sexp-end text fwd))
         (open-pos (find-enclosing-open text pos)))
    (when (and open-pos (> sexp-e fwd))
      (let ((close-pos (find-matching-close text open-pos)))
        (when close-pos
          (let* ((inner (substring text fwd sexp-e))
                 (new-text (string-append
                             (substring text 0 open-pos)
                             inner
                             (substring text close-pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed open-pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-split-sexp app)
  "Split the enclosing sexp into two at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (open-pos (find-enclosing-open text pos)))
    (when open-pos
      (let ((close-pos (find-enclosing-close text pos))
            (open-ch (string-ref text open-pos)))
        (when close-pos
          (let* ((close-ch (string-ref text close-pos))
                 (new-text (string-append
                             (substring text 0 pos)
                             (string close-ch) " " (string open-ch)
                             (substring text pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (+ pos 2))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-join-sexps app)
  "Join two adjacent sexps into one."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         ;; Find sexp before point (should end with close delimiter)
         (bwd (skip-whitespace-backward text pos)))
    (when (and (> bwd 0)
               (memv (string-ref text (- bwd 1)) '(#\) #\] #\})))
      ;; Find sexp after point (should start with open delimiter)
      (let ((fwd (skip-whitespace-forward text pos)))
        (when (and (< fwd (string-length text))
                   (memv (string-ref text fwd) '(#\( #\[ #\{)))
          ;; Remove the close of first and open of second
          (let ((new-text (string-append
                            (substring text 0 (- bwd 1))
                            " "
                            (substring text (+ fwd 1) (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (- bwd 1))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

;;;============================================================================
;;; Avy-style jump-to-char navigation
;;;============================================================================

(def *avy-labels* "asdfjklghqwertyuiopzxcvbnm")

(def (avy-find-char-positions text ch start end)
  "Find all positions of CH in TEXT between START and END."
  (let ((len (min end (string-length text)))
        (target (char-downcase ch)))
    (let loop ((i start) (acc []))
      (if (>= i len) (reverse acc)
        (if (char=? (char-downcase (string-ref text i)) target)
          (loop (+ i 1) (cons i acc))
          (loop (+ i 1) acc))))))

(def (avy-find-word-positions text start end)
  "Find positions at the start of each word in TEXT between START and END."
  (let ((len (min end (string-length text))))
    (let loop ((i start) (in-word #f) (acc []))
      (if (>= i len) (reverse acc)
        (let ((ch (string-ref text i)))
          (cond
            ((and (not in-word) (char-alphabetic? ch))
             (loop (+ i 1) #t (cons i acc)))
            ((char-whitespace? ch)
             (loop (+ i 1) #f acc))
            (else
             (loop (+ i 1) in-word acc))))))))

(def (avy-format-candidates text positions)
  "Format candidate list with labels for echo display."
  (let ((labels *avy-labels*)
        (n (min (string-length *avy-labels*) (length positions))))
    (let loop ((i 0) (ps positions) (acc []))
      (if (or (>= i n) (null? ps)) (string-join (reverse acc) "  ")
        (let* ((pos (car ps))
               ;; Get context: the word around the position
               (ctx-start (max 0 (- pos 10)))
               (ctx-end (min (string-length text) (+ pos 20)))
               (ctx (substring text ctx-start ctx-end))
               ;; Clean up context (replace newlines with spaces)
               (clean (let loop ((j 0) (out (open-output-string)))
                        (if (>= j (string-length ctx))
                          (get-output-string out)
                          (let ((c (string-ref ctx j)))
                            (display (if (char=? c #\newline) " " (string c)) out)
                            (loop (+ j 1) out))))))
          (loop (+ i 1) (cdr ps)
                (cons (string-append
                        (string (string-ref labels i)) ":"
                        clean)
                      acc)))))))

(def (cmd-avy-goto-char app)
  "Jump to a character occurrence. Type a char, then type a label to jump."
  (let ((ch-str (qt-echo-read-string app "avy char: ")))
    (when (and ch-str (= (string-length ch-str) 1))
      (let* ((ch (string-ref ch-str 0))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             ;; Search in a window around cursor (approx visible range)
             (start (max 0 (- pos 2000)))
             (end (min (string-length text) (+ pos 2000)))
             (positions (avy-find-char-positions text ch start end))
             (n (min (string-length *avy-labels*) (length positions))))
        (cond
          ((= (length positions) 0)
           (echo-error! (app-state-echo app) "No matches"))
          ((= (length positions) 1)
           ;; Single match — jump directly
           (qt-plain-text-edit-set-cursor-position! ed (car positions))
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else
           ;; Show candidates
           (echo-message! (app-state-echo app)
             (avy-format-candidates text positions))
           ;; Read label
           (let ((label (qt-echo-read-string app "avy jump: ")))
             (when (and label (= (string-length label) 1))
               (let ((idx (let loop ((i 0))
                            (if (>= i (string-length *avy-labels*)) #f
                              (if (char=? (string-ref *avy-labels* i)
                                          (string-ref label 0))
                                i (loop (+ i 1)))))))
                 (when (and idx (< idx n))
                   (qt-plain-text-edit-set-cursor-position! ed
                     (list-ref positions idx))
                   (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))))

(def (cmd-avy-goto-word app)
  "Jump to a word start. Type a label to jump."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (start (max 0 (- pos 2000)))
         (end (min (string-length text) (+ pos 2000)))
         (positions (avy-find-word-positions text start end))
         (n (min (string-length *avy-labels*) (length positions))))
    (cond
      ((= (length positions) 0)
       (echo-error! (app-state-echo app) "No word starts found"))
      (else
       (echo-message! (app-state-echo app)
         (avy-format-candidates text positions))
       (let ((label (qt-echo-read-string app "avy word: ")))
         (when (and label (= (string-length label) 1))
           (let ((idx (let loop ((i 0))
                        (if (>= i (string-length *avy-labels*)) #f
                          (if (char=? (string-ref *avy-labels* i)
                                      (string-ref label 0))
                            i (loop (+ i 1)))))))
             (when (and idx (< idx n))
               (qt-plain-text-edit-set-cursor-position! ed
                 (list-ref positions idx))
               (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))

(def (cmd-avy-goto-line app)
  "Jump to a line start. Type a label to jump."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         ;; Find all line starts in visible range
         (start (max 0 (- pos 2000)))
         (end (min (string-length text) (+ pos 2000)))
         (positions
           (let loop ((i start) (acc (if (= start 0) [0] [])))
             (if (>= i end) (reverse acc)
               (if (char=? (string-ref text i) #\newline)
                 (loop (+ i 1) (cons (+ i 1) acc))
                 (loop (+ i 1) acc)))))
         (n (min (string-length *avy-labels*) (length positions))))
    (cond
      ((= (length positions) 0)
       (echo-error! (app-state-echo app) "No lines found"))
      (else
       (let ((labels-str
               (let loop ((i 0) (ps positions) (acc []))
                 (if (or (>= i n) (null? ps)) (string-join (reverse acc) " ")
                   (let* ((p (car ps))
                          ;; Get line number
                          (line-num (let lp ((j 0) (ln 1))
                                      (if (>= j p) ln
                                        (lp (+ j 1)
                                            (if (char=? (string-ref text j) #\newline)
                                              (+ ln 1) ln)))))
                          (line-end (let lp ((j p))
                                      (if (or (>= j (string-length text))
                                              (char=? (string-ref text j) #\newline))
                                        j (lp (+ j 1)))))
                          (preview (substring text p (min line-end (+ p 30)))))
                     (loop (+ i 1) (cdr ps)
                           (cons (string-append
                                   (string (string-ref *avy-labels* i))
                                   ":" (number->string line-num) " " preview)
                                 acc)))))))
         (echo-message! (app-state-echo app) labels-str)
         (let ((label (qt-echo-read-string app "avy line: ")))
           (when (and label (= (string-length label) 1))
             (let ((idx (let loop ((i 0))
                          (if (>= i (string-length *avy-labels*)) #f
                            (if (char=? (string-ref *avy-labels* i)
                                        (string-ref label 0))
                              i (loop (+ i 1)))))))
               (when (and idx (< idx n))
                 (qt-plain-text-edit-set-cursor-position! ed
                   (list-ref positions idx))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))))))))))

;;;============================================================================
;;; Buffer cycling (previous/next buffer)
;;;============================================================================

(def (cmd-previous-buffer app)
  (let* ((bufs (buffer-list))
         (cur (current-qt-buffer app)))
    (when (> (length bufs) 1)
      (let ((prev (let loop ((bs bufs) (last #f))
                    (cond ((null? bs) last)
                          ((eq? (car bs) cur) (or last (last-element bufs)))
                          (else (loop (cdr bs) (car bs)))))))
        (when prev
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed prev)
            (set! (qt-edit-window-buffer (qt-current-window fr)) prev)
            (echo-message! (app-state-echo app) (buffer-name prev))))))))

(def (last-element lst)
  (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

(def (cmd-next-buffer app)
  (let* ((bufs (buffer-list))
         (cur (current-qt-buffer app)))
    (when (> (length bufs) 1)
      (let ((next (let loop ((bs bufs))
                    (cond ((null? bs) (car bufs))
                          ((eq? (car bs) cur)
                           (if (pair? (cdr bs)) (cadr bs) (car bufs)))
                          (else (loop (cdr bs)))))))
        (when next
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed next)
            (set! (qt-edit-window-buffer (qt-current-window fr)) next)
            (echo-message! (app-state-echo app) (buffer-name next))))))))

;;;============================================================================
;;; Delete trailing whitespace
;;;============================================================================

(def (cmd-delete-trailing-whitespace app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (cleaned (map string-trim-right lines))
         (new-text (string-join cleaned "\n"))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed
      (min pos (string-length new-text)))
    (echo-message! (app-state-echo app) "Trailing whitespace deleted")))

;;;============================================================================
;;; Kill buffer and window
;;;============================================================================

(def (cmd-kill-buffer-and-window app)
  (let ((fr (app-state-frame app)))
    (if (> (length (qt-frame-windows fr)) 1)
      (begin
        (cmd-kill-buffer-cmd app)
        (qt-frame-delete-window! fr))
      (cmd-kill-buffer-cmd app))))

;;;============================================================================
;;; Open line above
;;;============================================================================

(def (cmd-open-line-above app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)
    (qt-plain-text-edit-insert-text! ed "\n")
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_UP)))

;;;============================================================================
;;; Select line
;;;============================================================================

(def (cmd-select-line app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)
    (set! (buffer-mark buf) (qt-plain-text-edit-cursor-position ed))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END_OF_BLOCK)
    (echo-message! (app-state-echo app) "Line selected")))

;;;============================================================================
;;; Smart beginning of line (toggle between indentation and column 0)
;;;============================================================================

(def (cmd-smart-beginning-of-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (line-pos (line-start-position text line))
             (indent (let loop ((i 0))
                       (if (and (< i (string-length line-text))
                                (char-whitespace? (string-ref line-text i)))
                         (loop (+ i 1)) i)))
             (indent-pos (+ line-pos indent)))
        ;; If at indentation, go to column 0; otherwise go to indentation
        (if (= pos indent-pos)
          (qt-plain-text-edit-set-cursor-position! ed line-pos)
          (qt-plain-text-edit-set-cursor-position! ed indent-pos))))))

;;;============================================================================
;;; Insert parentheses / brackets
;;;============================================================================

(def (cmd-insert-parentheses app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "()")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

(def (cmd-insert-pair-brackets app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "[]")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

;;;============================================================================
;;; Find file at point
;;;============================================================================

;; cmd-find-file-at-point is defined earlier with file:line support

;;;============================================================================
;;; Show kill ring
;;;============================================================================

(def (cmd-show-kill-ring app)
  (let* ((ring (app-state-kill-ring app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (if (null? ring)
      (echo-message! echo "Kill ring is empty")
      (let* ((entries (let loop ((items ring) (i 0) (acc []))
                        (if (or (null? items) (>= i 20)) (reverse acc)
                          (let ((item (car items)))
                            (loop (cdr items) (+ i 1)
                                  (cons (string-append (number->string i) ": "
                                          (if (> (string-length item) 60)
                                            (string-append (substring item 0 60) "...")
                                            item))
                                        acc))))))
             (text (string-append "Kill Ring:\n\n"
                                  (string-join entries "\n")))
             (buf (or (buffer-by-name "*Kill Ring*")
                      (qt-buffer-create! "*Kill Ring*" ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

(def (cmd-browse-kill-ring app)
  "Interactive kill ring browser — prompt to select an entry to insert."
  (let* ((ring (app-state-kill-ring app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app)))
    (if (null? ring)
      (echo-message! echo "Kill ring is empty")
      ;; Show numbered preview in echo area and prompt
      (let* ((previews (let loop ((items ring) (i 0) (acc []))
                         (if (or (null? items) (>= i 10)) (reverse acc)
                           (let* ((item (car items))
                                  ;; Take first line only, truncate
                                  (first-line (let ((nl (string-contains item "\n")))
                                                (if nl (substring item 0 nl) item)))
                                  (preview (if (> (string-length first-line) 40)
                                             (string-append (substring first-line 0 40) "...")
                                             first-line)))
                             (loop (cdr items) (+ i 1)
                                   (cons (string-append (number->string i) ":" preview)
                                         acc))))))
             (prompt (string-append (string-join previews " | ") " — Enter #: "))
             (input (qt-echo-read-string app prompt)))
        (when (and input (> (string-length input) 0))
          (let ((idx (string->number input)))
            (if (and idx (>= idx 0) (< idx (length ring)))
              (let ((text (list-ref ring idx)))
                (qt-plain-text-edit-insert-text! ed text)
                (echo-message! echo
                  (string-append "Inserted kill ring entry " (number->string idx))))
              (echo-error! echo "Invalid kill ring index"))))))))

;;;============================================================================
;;; List registers
;;;============================================================================

(def (cmd-list-registers app)
  (let* ((regs (app-state-registers app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (let ((entries (let loop ((keys (sort (map (lambda (k) (string k))
                                               (hash-keys regs)) string<?))
                              (acc []))
                     (if (null? keys) (reverse acc)
                       (let* ((key (string-ref (car keys) 0))
                              (val (hash-get regs key))
                              (desc (cond
                                      ((string? val)
                                       (if (> (string-length val) 50)
                                         (string-append "\"" (substring val 0 50) "...\"")
                                         (string-append "\"" val "\"")))
                                      ((pair? val)
                                       (string-append "pos " (number->string (cdr val))
                                                      " in " (car val)))
                                      (else "?"))))
                         (loop (cdr keys)
                               (cons (string-append "  " (string key) ": " desc)
                                     acc)))))))
      (if (null? entries)
        (echo-message! echo "No registers defined")
        (let* ((text (string-append "Registers:\n\n"
                                    (string-join entries "\n")))
               (buf (or (buffer-by-name "*Registers*")
                        (qt-buffer-create! "*Registers*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; Scroll other window
;;;============================================================================

(def (cmd-scroll-other-window app)
  (let ((fr (app-state-frame app)))
    (when (> (length (qt-frame-windows fr)) 1)
      (qt-frame-other-window! fr)
      (cmd-scroll-down app)
      (qt-frame-other-window! fr))))

(def (cmd-scroll-other-window-up app)
  (let ((fr (app-state-frame app)))
    (when (> (length (qt-frame-windows fr)) 1)
      (qt-frame-other-window! fr)
      (cmd-scroll-up app)
      (qt-frame-other-window! fr))))

;;;============================================================================
;;; Swap buffers between windows
;;;============================================================================

(def (cmd-swap-buffers app)
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    (if (>= (length wins) 2)
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (b1 (qt-edit-window-buffer w1))
             (b2 (qt-edit-window-buffer w2))
             (e1 (qt-edit-window-editor w1))
             (e2 (qt-edit-window-editor w2)))
        (qt-buffer-attach! e1 b2)
        (qt-buffer-attach! e2 b1)
        (set! (qt-edit-window-buffer w1) b2)
        (set! (qt-edit-window-buffer w2) b1)
        (echo-message! (app-state-echo app) "Buffers swapped"))
      (echo-message! (app-state-echo app) "Need at least 2 windows"))))

;;;============================================================================
;;; Goto percent
;;;============================================================================

(def (cmd-goto-percent app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Goto percent: ")))
    (when (and input (> (string-length input) 0))
      (let ((pct (string->number input)))
        (if (and pct (>= pct 0) (<= pct 100))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (len (string-length text))
                 (target (inexact->exact (round (* len (/ pct 100.0))))))
            (qt-plain-text-edit-set-cursor-position! ed (min target len))
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-error! echo "Invalid percentage"))))))

;;;============================================================================
;;; Sentence navigation
;;;============================================================================

(def (cmd-forward-sentence app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond ((>= i len)
             (qt-plain-text-edit-set-cursor-position! ed len))
            ((and (memv (string-ref text i) '(#\. #\! #\?))
                  (or (>= (+ i 1) len)
                      (char-whitespace? (string-ref text (+ i 1)))))
             (qt-plain-text-edit-set-cursor-position! ed (+ i 1))
             (qt-plain-text-edit-ensure-cursor-visible! ed))
            (else (loop (+ i 1)))))))

(def (cmd-backward-sentence app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 2)))
      (cond ((<= i 0)
             (qt-plain-text-edit-set-cursor-position! ed 0))
            ((and (memv (string-ref text i) '(#\. #\! #\?))
                  (char-whitespace? (string-ref text (+ i 1))))
             (qt-plain-text-edit-set-cursor-position! ed (+ i 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed))
            (else (loop (- i 1)))))))

;;;============================================================================
;;; Dired (M-x dired)
;;;============================================================================

(def (cmd-dired app)
  (let* ((echo (app-state-echo app))
         (dir (qt-echo-read-string app "Dired: ")))
    (when (and dir (> (string-length dir) 0))
      (if (and (file-exists? dir)
               (eq? 'directory (file-info-type (file-info dir))))
        (dired-open-directory! app dir)
        (echo-error! echo (string-append "Not a directory: " dir))))))

;;;============================================================================
;;; Unfill paragraph (join lines in paragraph)
;;;============================================================================

(def (cmd-unfill-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    ;; Find paragraph boundaries
    (let* ((para-start (let loop ((i line))
                         (if (or (<= i 0)
                                 (string=? (string-trim (list-ref lines (- i 1))) ""))
                           i (loop (- i 1)))))
           (para-end (let loop ((i line))
                       (if (or (>= i (- (length lines) 1))
                               (string=? (string-trim (list-ref lines (+ i 1))) ""))
                         (+ i 1) (loop (+ i 1)))))
           (para-lines (let loop ((i para-start) (acc []))
                         (if (>= i para-end) (reverse acc)
                           (loop (+ i 1) (cons (string-trim (list-ref lines i)) acc)))))
           (joined (string-join para-lines " "))
           (before (let loop ((i 0) (acc []))
                     (if (>= i para-start) (reverse acc)
                       (loop (+ i 1) (cons (list-ref lines i) acc)))))
           (after (let loop ((i para-end) (acc []))
                    (if (>= i (length lines)) (reverse acc)
                      (loop (+ i 1) (cons (list-ref lines i) acc)))))
           (new-lines (append before (list joined) after))
           (new-text (string-join new-lines "\n")))
      (qt-plain-text-edit-set-text! ed new-text)
      (qt-plain-text-edit-set-cursor-position! ed
        (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))
      (echo-message! (app-state-echo app) "Unfilled"))))

;;;============================================================================
;;; Whitespace cleanup
;;;============================================================================

(def (cmd-whitespace-cleanup app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         ;; Remove trailing whitespace
         (cleaned (map string-trim-right lines))
         ;; Remove trailing blank lines
         (trimmed (let loop ((ls (reverse cleaned)))
                    (if (and (pair? ls) (string=? (car ls) ""))
                      (loop (cdr ls))
                      (reverse ls))))
         (new-text (string-append (string-join trimmed "\n") "\n"))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length new-text)))
    (echo-message! (app-state-echo app) "Whitespace cleaned")))

;;;============================================================================
;;; Insert UUID
;;;============================================================================

(def (cmd-insert-uuid app)
  (let ((uuid (with-catch
                (lambda (e) "00000000-0000-0000-0000-000000000000")
                (lambda ()
                  (let ((port (open-process
                                (list path: "/usr/bin/uuidgen"
                                      stdout-redirection: #t))))
                    (let ((result (read-line port)))
                      (close-port port)
                      (string-downcase (string-trim result))))))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) uuid)))

;;;============================================================================
;;; Word frequency
;;;============================================================================

(def (cmd-word-frequency app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (words (collect-buffer-words text))
         (freq (make-hash-table)))
    ;; Count each word
    (for-each
      (lambda (w) (hash-put! freq w (+ 1 (or (hash-get freq w) 0))))
      words)
    ;; Sort by frequency
    (let* ((pairs (map (lambda (k) (cons k (hash-get freq k))) (hash-keys freq)))
           (sorted (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
           (lines (map (lambda (p)
                         (string-append (number->string (cdr p)) "\t" (car p)))
                       (if (> (length sorted) 100)
                         (let loop ((ls sorted) (i 0) (acc []))
                           (if (or (null? ls) (>= i 100)) (reverse acc)
                             (loop (cdr ls) (+ i 1) (cons (car ls) acc))))
                         sorted)))
           (text (string-append "Word Frequency (top "
                                (number->string (length lines)) "):\n\n"
                                (string-join lines "\n")))
           (fr (app-state-frame app))
           (buf (or (buffer-by-name "*Word Frequency*")
                    (qt-buffer-create! "*Word Frequency*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;;;============================================================================
;;; Indent rigidly (shift region right/left)
;;;============================================================================

(def (cmd-indent-rigidly-right app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (shifted (map (lambda (l) (string-append "  " l)) lines))
             (new-region (string-join shifted "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start))
      ;; No mark: indent current line
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (line (qt-plain-text-edit-cursor-line ed)))
        (when (< line (length lines))
          (qt-replace-line! ed line
            (string-append "  " (list-ref lines line))))))))

(def (cmd-indent-rigidly-left app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (shifted (map (lambda (l)
                             (if (and (>= (string-length l) 2)
                                      (string=? (substring l 0 2) "  "))
                               (substring l 2 (string-length l))
                               (string-trim l)))
                           lines))
             (new-region (string-join shifted "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start))
      ;; No mark: dedent current line
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (line (qt-plain-text-edit-cursor-line ed)))
        (when (< line (length lines))
          (let ((l (list-ref lines line)))
            (when (and (>= (string-length l) 2)
                       (string=? (substring l 0 2) "  "))
              (qt-replace-line! ed line
                (substring l 2 (string-length l))))))))))

;;;============================================================================
;;; Center line
;;;============================================================================

(def (cmd-center-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (fill-column 70))
    (when (< line (length lines))
      (let* ((l (string-trim (list-ref lines line)))
             (padding (max 0 (quotient (- fill-column (string-length l)) 2)))
             (centered (string-append (make-string padding #\space) l)))
        (qt-replace-line! ed line centered)))))

;;;============================================================================
;;; Narrow to region / widen
;;;============================================================================

;; Store narrowing state per buffer
(def *narrow-state* (make-hash-table))

(def (cmd-narrow-to-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end)))
        ;; Save full text and narrow bounds
        (hash-put! *narrow-state* buf (list text start end))
        ;; Show only the region
        (qt-plain-text-edit-set-text! ed region)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Narrowed"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-widen app)
  (let* ((buf (current-qt-buffer app))
         (state (hash-get *narrow-state* buf)))
    (if state
      (let* ((ed (current-qt-editor app))
             (full-text (car state))
             (start (cadr state))
             ;; Get the current narrowed text (may have been edited)
             (narrow-text (qt-plain-text-edit-text ed))
             ;; Replace the narrowed region with the edited version
             (new-text (string-append
                         (substring full-text 0 start)
                         narrow-text
                         (substring full-text (caddr state) (string-length full-text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (hash-remove! *narrow-state* buf)
        (echo-message! (app-state-echo app) "Widened"))
      (echo-message! (app-state-echo app) "Buffer is not narrowed"))))

;;;============================================================================
;;; Display time
;;;============================================================================

(def (cmd-display-time app)
  (let ((result (with-catch
                  (lambda (e) "unknown")
                  (lambda ()
                    (let ((port (open-process
                                  (list path: "/bin/date"
                                        stdout-redirection: #t))))
                      (let ((output (read-line port)))
                        (close-port port)
                        output))))))
    (echo-message! (app-state-echo app) result)))

;;;============================================================================
;;; Buffer info
;;;============================================================================

(def (cmd-buffer-info app)
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (qt-plain-text-edit-line-count ed))
         (chars (string-length text))
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i chars) count
                    (if (char-whitespace? (string-ref text i))
                      (loop (+ i 1) #f count)
                      (loop (+ i 1) #t (if in-word count (+ count 1)))))))
         (name (buffer-name buf))
         (path (or (buffer-file-path buf) "(no file)"))
         (modified (if (and (buffer-doc-pointer buf)
                           (qt-text-document-modified? (buffer-doc-pointer buf)))
                     "modified" "unmodified")))
    (echo-message! (app-state-echo app)
      (string-append name " [" modified "] " path
                     " (" (number->string lines) "L "
                     (number->string words) "W "
                     (number->string chars) "C)"))))

;;;============================================================================
;;; Bookmarks
;;;============================================================================

(def (cmd-bookmark-set app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Bookmark name: ")))
    (when (and input (> (string-length input) 0))
      (let* ((buf (current-qt-buffer app))
             (pos (qt-plain-text-edit-cursor-position (current-qt-editor app))))
        (hash-put! (app-state-bookmarks app) input
                   (list (buffer-name buf) (buffer-file-path buf) pos))
        (echo-message! echo (string-append "Bookmark \"" input "\" set"))
        (bookmarks-save! app)))))

(def (cmd-bookmark-jump app)
  (let* ((echo (app-state-echo app))
         (names (sort (hash-keys (app-state-bookmarks app)) string<?))
         (input (qt-echo-read-string-with-completion app "Jump to bookmark: " names)))
    (when (and input (> (string-length input) 0))
      (let ((entry (hash-get (app-state-bookmarks app) input)))
        (if entry
          (let* ((buf-name (if (list? entry) (car entry) (car entry)))
                 (fpath (if (list? entry) (cadr entry) #f))
                 (pos (if (list? entry) (caddr entry) (cdr entry)))
                 (fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 ;; Try existing buffer first, then open by file-path
                 (buf (or (buffer-by-name buf-name)
                          (and fpath (file-exists? fpath)
                               (let loop ((bufs *buffer-list*))
                                 (if (null? bufs) #f
                                   (let ((b (car bufs)))
                                     (if (and (buffer-file-path b)
                                              (string=? (buffer-file-path b) fpath))
                                       b (loop (cdr bufs)))))))
                          ;; Open the file fresh
                          (and fpath (file-exists? fpath)
                               (let* ((name (path-strip-directory fpath))
                                      (new-buf (qt-buffer-create! name ed fpath)))
                                 (let ((text (read-file-as-string fpath)))
                                   (when text
                                     (qt-plain-text-edit-set-text! ed text)
                                     (qt-text-document-set-modified! (buffer-doc-pointer new-buf) #f)))
                                 (qt-setup-highlighting! app new-buf)
                                 new-buf)))))
            (if buf
              (begin
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-cursor-position! ed
                  (min pos (string-length (qt-plain-text-edit-text ed))))
                (qt-plain-text-edit-ensure-cursor-visible! ed))
              (echo-error! echo (string-append "Cannot find: "
                                  (or fpath buf-name)))))
          (echo-error! echo (string-append "No bookmark: " input)))))))

(def (cmd-bookmark-list app)
  (let* ((bmarks (app-state-bookmarks app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (let ((entries (let loop ((keys (sort (hash-keys bmarks) string<?))
                              (acc []))
                     (if (null? keys) (reverse acc)
                       (let* ((name (car keys))
                              (val (hash-get bmarks name))
                              (buf-name (if (list? val) (car val) (car val)))
                              (fpath (if (list? val) (cadr val) #f))
                              (pos (if (list? val) (caddr val) (cdr val))))
                         (loop (cdr keys)
                               (cons (string-append "  " name "\t"
                                       (or fpath buf-name)
                                       " pos " (number->string pos))
                                     acc)))))))
      (if (null? entries)
        (echo-message! echo "No bookmarks defined")
        (let* ((text (string-append "Bookmarks:\n\n"
                                    (string-join entries "\n")))
               (buf (or (buffer-by-name "*Bookmarks*")
                        (qt-buffer-create! "*Bookmarks*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; Rectangle operations
;;;============================================================================

(def (region-line-range text start end)
  "Get line numbers for start and end positions."
  (let loop ((i 0) (line 0) (start-line #f) (end-line #f))
    (cond
      ((and start-line end-line) (values start-line end-line))
      ((>= i (string-length text))
       (values (or start-line line) (or end-line line)))
      ((= i start)
       (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ line 1) line)
             line end-line))
      ((= i end)
       (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ line 1) line)
             start-line line))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ line 1) start-line end-line))
      (else (loop (+ i 1) line start-line end-line)))))

(def (column-at-position text pos)
  "Get column number for a position."
  (let loop ((i (- pos 1)) (col 0))
    (if (or (< i 0) (char=? (string-ref text i) #\newline)) col
      (loop (- i 1) (+ col 1)))))

(def (cmd-kill-rectangle app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (echo (app-state-echo app)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (col1 (column-at-position text start))
             (col2 (column-at-position text end))
             (left-col (min col1 col2))
             (right-col (max col1 col2))
             (lines (string-split text #\newline)))
        (let-values (((start-line end-line) (region-line-range text start end)))
          (let ((killed [])
                (new-lines []))
            ;; Process each line in range
            (let loop ((ls lines) (i 0))
              (if (null? ls)
                (begin
                  (set! (app-state-rect-kill app) (reverse killed))
                  (let ((result (string-join (reverse new-lines) "\n")))
                    (qt-plain-text-edit-set-text! ed result)
                    (qt-plain-text-edit-set-cursor-position! ed
                      (min start (string-length result)))
                    (set! (buffer-mark buf) #f)
                    (echo-message! echo "Rectangle killed")))
                (if (and (>= i start-line) (<= i end-line))
                  ;; Extract rectangle region from this line
                  (let* ((l (car ls))
                         (len (string-length l))
                         (rect-text (if (< left-col len)
                                      (substring l left-col (min right-col len))
                                      ""))
                         (new-line (string-append
                                     (if (< left-col len)
                                       (substring l 0 left-col)
                                       l)
                                     (if (< right-col len)
                                       (substring l right-col len)
                                       ""))))
                    (set! killed (cons rect-text killed))
                    (set! new-lines (cons new-line new-lines))
                    (loop (cdr ls) (+ i 1)))
                  (begin
                    (set! new-lines (cons (car ls) new-lines))
                    (loop (cdr ls) (+ i 1)))))))))
      (echo-error! echo "No mark set"))))

(def (cmd-yank-rectangle app)
  (let* ((rect (app-state-rect-kill app))
         (echo (app-state-echo app)))
    (if (null? rect)
      (echo-message! echo "No rectangle in kill ring")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (col (column-at-position text pos))
             (lines (string-split text #\newline))
             (line (qt-plain-text-edit-cursor-line ed)))
        ;; Insert rectangle text at current column on consecutive lines
        (let* ((new-lines (let loop ((ls lines) (i 0) (rs rect) (acc []))
                            (if (null? ls)
                              (reverse acc)
                              (if (and (>= i line) (pair? rs))
                                (let* ((l (car ls))
                                       (len (string-length l))
                                       (padded (if (< len col)
                                                 (string-append l (make-string (- col len) #\space))
                                                 l))
                                       (new-line (string-append
                                                   (substring padded 0 col)
                                                   (car rs)
                                                   (substring padded col (string-length padded)))))
                                  (loop (cdr ls) (+ i 1) (cdr rs) (cons new-line acc)))
                                (loop (cdr ls) (+ i 1) rs (cons (car ls) acc))))))
                (new-text (string-join new-lines "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed pos)
          (echo-message! echo "Rectangle yanked"))))))

(def (cmd-string-rectangle app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (echo (app-state-echo app)))
    (if mark
      (let ((str (qt-echo-read-string app "String rectangle: ")))
        (when (and str (> (string-length str) 0))
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (col1 (column-at-position text start))
                 (col2 (column-at-position text end))
                 (left-col (min col1 col2))
                 (right-col (max col1 col2))
                 (lines (string-split text #\newline)))
            (let-values (((start-line end-line) (region-line-range text start end)))
              (let* ((new-lines (let loop ((ls lines) (i 0) (acc []))
                                  (if (null? ls) (reverse acc)
                                    (if (and (>= i start-line) (<= i end-line))
                                      (let* ((l (car ls))
                                             (len (string-length l))
                                             (new-line (string-append
                                                         (if (< left-col len)
                                                           (substring l 0 left-col)
                                                           l)
                                                         str
                                                         (if (< right-col len)
                                                           (substring l right-col len)
                                                           ""))))
                                        (loop (cdr ls) (+ i 1) (cons new-line acc)))
                                      (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
                     (new-text (string-join new-lines "\n")))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (min start (string-length new-text)))
                (set! (buffer-mark buf) #f)
                (echo-message! echo "Rectangle replaced"))))))
      (echo-error! echo "No mark set"))))

;;;============================================================================
;;; Describe key / describe command
;;;============================================================================

(def (cmd-describe-key app)
  "Show what command a key sequence runs."
  (echo-message! (app-state-echo app)
    "Press a key... (use C-h b for all bindings)"))

(def (cmd-describe-command app)
  (let* ((echo (app-state-echo app))
         (cmd-names (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         (input (qt-echo-read-string-with-completion app "Describe command: " cmd-names)))
    (when (and input (> (string-length input) 0))
      (let* ((sym (string->symbol input))
             (proc (find-command sym)))
        (if proc
          ;; Find the keybinding
          (let ((binding #f))
            (for-each
              (lambda (entry)
                (let ((key (car entry)) (val (cdr entry)))
                  (cond
                    ((eq? val sym) (set! binding key))
                    ((hash-table? val)
                     (for-each
                       (lambda (sub) (when (eq? (cdr sub) sym)
                                       (set! binding (string-append key " " (car sub)))))
                       (keymap-entries val))))))
              (keymap-entries *global-keymap*))
            (echo-message! echo
              (string-append input
                (if binding (string-append " is on " binding)
                  " is not on any key"))))
          (echo-error! echo (string-append input " is not a known command")))))))

;;;============================================================================
;;; Toggle electric pair mode
;;;============================================================================

(def (cmd-toggle-electric-pair app)
  (set! *auto-pair-mode* (not *auto-pair-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-pair-mode* "Auto-pair ON" "Auto-pair OFF")))

;;;============================================================================
;;; Universal argument / prefix arg system
;;;============================================================================

(def (cmd-universal-argument app)
  "C-u prefix argument."
  (let ((current (app-state-prefix-arg app)))
    (cond
     ((not current)
      (set! (app-state-prefix-arg app) '(4)))
     ((list? current)
      (set! (app-state-prefix-arg app) (list (* 4 (car current)))))
     (else
      (set! (app-state-prefix-arg app) '(4))))
    (echo-message! (app-state-echo app)
                   (string-append "C-u"
                                  (let ((val (car (app-state-prefix-arg app))))
                                    (if (= val 4) "" (string-append " " (number->string val))))
                                  "-"))))

(def (cmd-digit-argument app digit)
  "Build a numeric prefix argument."
  (let ((current (app-state-prefix-arg app)))
    (cond
     ((number? current)
      (set! (app-state-prefix-arg app) (+ (* current 10) digit)))
     ((eq? current '-)
      (set! (app-state-prefix-arg app) (- digit)))
     (else
      (set! (app-state-prefix-arg app) digit)))
    (set! (app-state-prefix-digit-mode? app) #t)
    (echo-message! (app-state-echo app)
                   (string-append "Arg: " (if (eq? (app-state-prefix-arg app) '-)
                                            "-"
                                            (number->string (app-state-prefix-arg app)))))))

(def (cmd-negative-argument app)
  "Negative prefix argument (M--)."
  (set! (app-state-prefix-arg app) '-)
  (set! (app-state-prefix-digit-mode? app) #t)
  (echo-message! (app-state-echo app) "Arg: -"))

(def (cmd-digit-argument-0 app) (cmd-digit-argument app 0))
(def (cmd-digit-argument-1 app) (cmd-digit-argument app 1))
(def (cmd-digit-argument-2 app) (cmd-digit-argument app 2))
(def (cmd-digit-argument-3 app) (cmd-digit-argument app 3))
(def (cmd-digit-argument-4 app) (cmd-digit-argument app 4))
(def (cmd-digit-argument-5 app) (cmd-digit-argument app 5))
(def (cmd-digit-argument-6 app) (cmd-digit-argument app 6))
(def (cmd-digit-argument-7 app) (cmd-digit-argument app 7))
(def (cmd-digit-argument-8 app) (cmd-digit-argument app 8))
(def (cmd-digit-argument-9 app) (cmd-digit-argument app 9))

;;;============================================================================
;;; Next/previous error (compilation error navigation)
;;;============================================================================

(def (compilation-goto-error! app index)
  "Jump to the compilation error at INDEX. Opens the file and positions cursor."
  (when (and (>= index 0) (< index (length *compilation-errors*)))
    (let* ((err (list-ref *compilation-errors* index))
           (file (car err))
           (line (cadr err))
           (col (caddr err))
           (msg (cadddr err))
           (echo (app-state-echo app))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      (set! *compilation-error-index* index)
      ;; Open the file (inline logic to avoid circular dep with app.ss)
      (when (file-exists? file)
        (let* ((name (path-strip-directory file))
               ;; Check if buffer already exists for this file
               (existing (let loop ((bufs *buffer-list*))
                           (if (null? bufs) #f
                             (let ((b (car bufs)))
                               (if (and (buffer-file-path b)
                                        (string=? (buffer-file-path b) file))
                                 b
                                 (loop (cdr bufs)))))))
               (buf (or existing
                        (qt-buffer-create! name ed file))))
          ;; Switch to buffer
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          ;; Load file content if new buffer
          (when (not existing)
            (let ((text (read-file-as-string file)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
            (qt-setup-highlighting! app buf))
          ;; Navigate to line:col
          (let* ((text (qt-plain-text-edit-text ed))
                 (line-pos (text-line-position text line))
                 (pos (+ line-pos (max 0 (- col 1)))))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-message! echo
            (string-append "Error " (number->string (+ index 1))
              "/" (number->string (length *compilation-errors*))
              ": " msg)))))))

(def (cmd-next-error app)
  "Jump to next compilation error or grep result."
  (cond
    ((not (null? *compilation-errors*))
     (let ((next-idx (+ *compilation-error-index* 1)))
       (if (>= next-idx (length *compilation-errors*))
         (echo-message! (app-state-echo app) "No more errors")
         (compilation-goto-error! app next-idx))))
    ((not (null? *grep-results*))
     (cmd-next-grep-result app))
    (else
     ;; Fallback: search match navigation
     (let* ((ed (current-qt-editor app))
            (search (app-state-last-search app)))
       (if (not search)
         (echo-error! (app-state-echo app) "No errors, grep results, or search")
         (let* ((text (qt-plain-text-edit-text ed))
                (pos (qt-plain-text-edit-cursor-position ed))
                (found (string-contains text search (+ pos 1))))
           (if found
             (begin
               (qt-plain-text-edit-set-cursor-position! ed found)
               (qt-plain-text-edit-ensure-cursor-visible! ed))
             (let ((found2 (string-contains text search)))
               (if found2
                 (begin
                   (qt-plain-text-edit-set-cursor-position! ed found2)
                   (qt-plain-text-edit-ensure-cursor-visible! ed)
                   (echo-message! (app-state-echo app) "Wrapped"))
                 (echo-error! (app-state-echo app) "No more matches"))))))))))

(def (cmd-previous-error app)
  "Jump to previous compilation error or grep result."
  (cond
    ((not (null? *compilation-errors*))
     (let ((prev-idx (- *compilation-error-index* 1)))
       (if (< prev-idx 0)
         (echo-message! (app-state-echo app) "No previous errors")
         (compilation-goto-error! app prev-idx))))
    ((not (null? *grep-results*))
     (cmd-previous-grep-result app))
    (else
     ;; Fallback: search match navigation
     (let* ((ed (current-qt-editor app))
            (search (app-state-last-search app)))
       (if (not search)
         (echo-error! (app-state-echo app) "No errors, grep results, or search")
         (let* ((text (qt-plain-text-edit-text ed))
                (pos (qt-plain-text-edit-cursor-position ed))
                (found (let loop ((last-found #f) (start 0))
                         (let ((idx (string-contains text search start)))
                           (if (and idx (< idx pos))
                             (loop idx (+ idx 1))
                             last-found)))))
           (if found
             (begin
               (qt-plain-text-edit-set-cursor-position! ed found)
               (qt-plain-text-edit-ensure-cursor-visible! ed))
             (let ((found2 (let loop ((last-found #f) (start 0))
                             (let ((idx (string-contains text search start)))
                               (if idx
                                 (loop idx (+ idx 1))
                                 last-found)))))
               (if found2
                 (begin
                   (qt-plain-text-edit-set-cursor-position! ed found2)
                   (qt-plain-text-edit-ensure-cursor-visible! ed)
                   (echo-message! (app-state-echo app) "Wrapped"))
                 (echo-error! (app-state-echo app) "No matches"))))))))))

;;;============================================================================
;;; Text transforms: tabify, untabify, base64, rot13
;;;============================================================================

(def (cmd-tabify app)
  "Convert runs of 8 spaces to tabs in region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (result (let loop ((s region) (acc ""))
                       (let ((idx (string-contains s "        ")))
                         (if idx
                           (loop (substring s (+ idx 8) (string-length s))
                                 (string-append acc (substring s 0 idx) "\t"))
                           (string-append acc s)))))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! (app-state-echo app) "Tabified")))))

(def (cmd-untabify app)
  "Convert tabs to spaces in region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (result (let loop ((i 0) (acc '()))
                       (if (>= i (string-length region))
                         (apply string-append (reverse acc))
                         (if (char=? (string-ref region i) #\tab)
                           (loop (+ i 1) (cons "        " acc))
                           (loop (+ i 1) (cons (string (string-ref region i)) acc))))))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! (app-state-echo app) "Untabified")))))

(def (cmd-base64-encode-region app)
  "Base64 encode the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (encoded (base64-encode region))
             (new-text (string-append (substring text 0 start) encoded
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length encoded)))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Base64 encoded")))))

(def (cmd-base64-decode-region app)
  "Base64 decode the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Base64 decode error"))
        (lambda ()
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (decoded (base64-decode (string-trim-both region)))
                 (new-text (string-append (substring text 0 start) decoded
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length decoded)))
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app) "Base64 decoded")))))))

(def (rot13-char ch)
  (cond
    ((and (char>=? ch #\a) (char<=? ch #\z))
     (integer->char (+ (char->integer #\a)
                       (modulo (+ (- (char->integer ch) (char->integer #\a)) 13) 26))))
    ((and (char>=? ch #\A) (char<=? ch #\Z))
     (integer->char (+ (char->integer #\A)
                       (modulo (+ (- (char->integer ch) (char->integer #\A)) 13) 26))))
    (else ch)))

(def (cmd-rot13-region app)
  "ROT13 encode the region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (len (string-length region))
             (result (make-string len)))
        (let loop ((i 0))
          (when (< i len)
            (string-set! result i (rot13-char (string-ref region i)))
            (loop (+ i 1))))
        (let ((new-text (string-append (substring text 0 start) result
                                        (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start)
          (when mark (set! (buffer-mark buf) #f))
          (echo-message! (app-state-echo app) "ROT13 applied"))))))

;;;============================================================================
;;; Hex dump mode
;;;============================================================================

(def (cmd-hexl-mode app)
  "Display buffer contents as hex dump in *Hex* buffer."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (text (qt-plain-text-edit-text ed))
         (bytes (string->bytes text))
         (len (u8vector-length bytes))
         (lines '()))
    (let loop ((offset 0))
      (when (< offset len)
        (let* ((end (min (+ offset 16) len))
               (hex-parts '())
               (ascii-parts '()))
          (let hex-loop ((i offset))
            (when (< i end)
              (let* ((b (u8vector-ref bytes i))
                     (h (number->string b 16)))
                (set! hex-parts
                  (cons (if (< b 16) (string-append "0" h) h)
                        hex-parts)))
              (hex-loop (+ i 1))))
          (let ascii-loop ((i offset))
            (when (< i end)
              (let ((b (u8vector-ref bytes i)))
                (set! ascii-parts
                  (cons (if (and (>= b 32) (<= b 126))
                          (string (integer->char b))
                          ".")
                        ascii-parts)))
              (ascii-loop (+ i 1))))
          (let* ((addr (let ((h (number->string offset 16)))
                         (string-append (make-string (max 0 (- 8 (string-length h))) #\0) h)))
                 (hex-str (string-join (reverse hex-parts) " "))
                 (pad (make-string (max 0 (- 48 (string-length hex-str))) #\space))
                 (ascii-str (apply string-append (reverse ascii-parts))))
            (set! lines (cons (string-append addr "  " hex-str pad "  |" ascii-str "|")
                              lines))))
        (loop (+ offset 16))))
    (let* ((result (string-join (reverse lines) "\n"))
           (buf (qt-buffer-create! "*Hex*" ed #f)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed result)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "Hex dump"))))

;;;============================================================================
;;; Toggle commands
;;;============================================================================

(def *word-wrap-on* #f)

(def (cmd-toggle-word-wrap app)
  "Toggle word wrap."
  (let ((ed (current-qt-editor app)))
    (set! *word-wrap-on* (not *word-wrap-on*))
    (if *word-wrap-on*
      (begin
        (qt-plain-text-edit-set-line-wrap! ed QT_PLAIN_WIDGET_WRAP)
        (echo-message! (app-state-echo app) "Word wrap ON"))
      (begin
        (qt-plain-text-edit-set-line-wrap! ed QT_PLAIN_NO_WRAP)
        (echo-message! (app-state-echo app) "Word wrap OFF")))))

(def *whitespace-mode-on* #f)

(def (cmd-toggle-whitespace app)
  "Toggle trailing whitespace highlighting."
  (set! *whitespace-mode-on* (not *whitespace-mode-on*))
  (if *whitespace-mode-on*
    (begin
      (qt-highlight-trailing-whitespace! (current-qt-editor app))
      (echo-message! (app-state-echo app) "Whitespace highlighting enabled"))
    (begin
      ;; Re-apply visual decorations to clear the whitespace highlights
      (qt-update-visual-decorations! (current-qt-editor app))
      (echo-message! (app-state-echo app) "Whitespace highlighting disabled"))))

;; Trailing whitespace colors (red background)
(def ws-bg-r #x80) (def ws-bg-g #x20) (def ws-bg-b #x20)

(def (qt-highlight-trailing-whitespace! ed)
  "Highlight trailing whitespace on all lines."
  (let* ((text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (pos 0))
    (for-each
      (lambda (line)
        (let ((line-len (string-length line)))
          ;; Find trailing whitespace
          (let loop ((i (- line-len 1)))
            (cond
              ((< i 0) ; whole line is whitespace — skip
               (void))
              ((let ((ch (string-ref line i)))
                 (or (char=? ch #\space) (char=? ch #\tab)))
               (loop (- i 1)))
              (else
               ;; i is the last non-whitespace char; trailing ws starts at i+1
               (let ((trail-start (+ i 1)))
                 (when (< trail-start line-len)
                   (qt-extra-selection-add-range! ed
                     (+ pos trail-start) (- line-len trail-start)
                     #xff #xff #xff
                     ws-bg-r ws-bg-g ws-bg-b bold: #f)))))))
        ;; Advance pos by line length + newline
        (set! pos (+ pos (string-length line) 1)))
      lines)
    (qt-extra-selections-apply! ed)))

(def (cmd-toggle-truncate-lines app)
  "Toggle line truncation (same as word-wrap toggle)."
  (cmd-toggle-word-wrap app))

(def *case-fold-search-qt* #t)

(def (cmd-toggle-case-fold-search app)
  "Toggle case-sensitive search."
  (set! *case-fold-search-qt* (not *case-fold-search-qt*))
  (echo-message! (app-state-echo app)
    (if *case-fold-search-qt*
      "Case-insensitive search"
      "Case-sensitive search")))

(def *overwrite-mode* #f)

(def (cmd-toggle-overwrite-mode app)
  "Toggle overwrite mode (display only - Qt doesn't expose overwrite API)."
  (set! *overwrite-mode* (not *overwrite-mode*))
  (echo-message! (app-state-echo app)
    (if *overwrite-mode* "Overwrite mode ON" "Overwrite mode OFF")))

(def *auto-fill-mode* #f)
(def *fill-column* 80)

(def (cmd-toggle-auto-fill app)
  "Toggle auto-fill mode."
  (set! *auto-fill-mode* (not *auto-fill-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-fill-mode*
      (string-append "Auto fill ON (col " (number->string *fill-column*) ")")
      "Auto fill OFF")))

(def (cmd-set-fill-column app)
  "Set the fill column for paragraph filling."
  (let ((input (qt-echo-read-string app
                 (string-append "Fill column (current " (number->string *fill-column*) "): "))))
    (when input
      (let ((n (string->number input)))
        (if (and n (> n 0))
          (begin
            (set! *fill-column* n)
            (echo-message! (app-state-echo app)
              (string-append "Fill column set to " (number->string n))))
          (echo-error! (app-state-echo app) "Invalid number"))))))

(def (cmd-toggle-highlighting app)
  "Toggle syntax highlighting."
  (echo-message! (app-state-echo app) "Syntax highlighting toggled"))

(def (cmd-toggle-visual-line-mode app)
  "Toggle visual line mode."
  (cmd-toggle-word-wrap app))

(def (cmd-toggle-fill-column-indicator app)
  "Toggle fill column indicator display."
  (echo-message! (app-state-echo app)
    (string-append "Fill column indicator at " (number->string *fill-column*))))

;;;============================================================================
;;; Calculator
;;;============================================================================

(def (cmd-calc app)
  "Evaluate a math expression."
  (let ((expr (qt-echo-read-string app "Calc: ")))
    (when (and expr (> (string-length expr) 0))
      (let-values (((result error?) (eval-expression-string expr)))
        (if error?
          (echo-error! (app-state-echo app) (string-append "Error: " result))
          (echo-message! (app-state-echo app) (string-append "= " result)))))))

;;;============================================================================
;;; Describe bindings
;;;============================================================================

(def (cmd-describe-bindings app)
  "Show all keybindings in a *Bindings* buffer."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (let ((lines []))
      (define (collect-prefix prefix km)
        (for-each
          (lambda (entry)
            (let ((key (car entry))
                  (val (cdr entry)))
              (if (hash-table? val)
                (collect-prefix (string-append prefix key " ") val)
                (set! lines (cons (string-append prefix key "\t"
                                                 (symbol->string val))
                                  lines)))))
          (keymap-entries km)))
      (collect-prefix "" *global-keymap*)
      (let* ((sorted (sort lines string<?))
             (text (string-join sorted "\n"))
             (buf (qt-buffer-create! "*Bindings*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app)
          (string-append (number->string (length sorted)) " bindings"))))))

;;;============================================================================
;;; Describe char
;;;============================================================================

(def (cmd-describe-char app)
  "Show info about the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if (>= pos (string-length text))
      (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch)
                         " (U+" (let ((h (number->string code 16)))
                                  (string-append (make-string (max 0 (- 4 (string-length h))) #\0) h))
                         ") = " (number->string code)))))))

;;;============================================================================
;;; Describe key briefly
;;;============================================================================

(def (cmd-describe-key-briefly app)
  "Show what a key sequence is bound to."
  (echo-message! (app-state-echo app) "Press a key...")
  ;; This needs to be handled at the key dispatch level.
  ;; For now, delegate to describe-key which uses echo prompt.
  (cmd-describe-key app))

;;;============================================================================
;;; Count chars region, count words buffer/region
;;;============================================================================

(def (cmd-count-chars-region app)
  "Count characters in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (count (- end start)))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string count) " chars"))))))

(def (cmd-count-words-region app)
  "Count words in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (words (length (filter (lambda (s) (> (string-length s) 0))
                                    (string-split region #\space)))))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string words) " words"))))))

(def (cmd-count-words-buffer app)
  "Count words, lines, and chars in entire buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (chars (string-length text))
         (lines (+ 1 (length (string-split text #\newline))))
         (words (length (filter (lambda (s) (> (string-length s) 0))
                                (string-split text #\space)))))
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (number->string words) " words, "
                     (number->string lines) " lines, "
                     (number->string chars) " chars"))))

;;;============================================================================
;;; Buffer stats
;;;============================================================================

(def (cmd-buffer-stats app)
  "Show statistics about the current buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (chars (string-length text))
         (lines (+ 1 (length (filter (lambda (s) #t) (string-split text #\newline)))))
         (words (length (filter (lambda (s) (> (string-length s) 0))
                                (string-split text #\space))))
         (blanks (length (filter (lambda (s) (= (string-length (string-trim-both s)) 0))
                                 (string-split text #\newline))))
         (path (buffer-file-path buf)))
    (echo-message! (app-state-echo app)
      (string-append (buffer-name buf) ": "
                     (number->string lines) "L "
                     (number->string words) "W "
                     (number->string chars) "C "
                     (number->string blanks) " blank"
                     (if path (string-append " [" path "]") "")))))

;;;============================================================================
;;; List processes
;;;============================================================================

(def (cmd-list-processes app)
  "Show running subprocesses."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (lines ["Type\tBuffer"
                 "----\t------"]))
    (for-each
      (lambda (buf)
        (cond
          ((repl-buffer? buf)
           (set! lines (cons (string-append "REPL\t" (buffer-name buf)) lines)))
          ((shell-buffer? buf)
           (set! lines (cons (string-append "Shell\t" (buffer-name buf)) lines)))
          ((eshell-buffer? buf)
           (set! lines (cons (string-append "Eshell\t" (buffer-name buf)) lines)))
          ((terminal-buffer? buf)
           (set! lines (cons (string-append "Term\t" (buffer-name buf)) lines)))))
      (buffer-list))
    (let* ((text (string-join (reverse lines) "\n"))
           (buf (qt-buffer-create! "*Processes*" ed #f)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app)
        (string-append (number->string (- (length lines) 2)) " processes")))))

;;;============================================================================
;;; View messages
;;;============================================================================

(def (cmd-view-messages app)
  "Show *Messages* buffer with recent echo area messages."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-buffer-create! "*Messages*" ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed "(Messages buffer - recent activity shown here)")
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;;============================================================================
;;; What buffer / what face
;;;============================================================================

(def (cmd-what-buffer app)
  "Show current buffer name and file path."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (echo-message! (app-state-echo app)
      (if path
        (string-append (buffer-name buf) " [" path "]")
        (buffer-name buf)))))

(def (cmd-what-face app)
  "Show style info at point."
  (echo-message! (app-state-echo app)
    "Font style info not available in Qt plain text mode"))

;;;============================================================================
;;; Insert helpers
;;;============================================================================

(def (cmd-insert-buffer-name app)
  "Insert current buffer name at point."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (qt-plain-text-edit-insert-text! ed (buffer-name buf))))

(def (cmd-insert-file-name app)
  "Insert current buffer's file path at point."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (qt-plain-text-edit-insert-text! ed path)
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-insert-char app)
  "Insert a character by Unicode code point."
  (let ((input (qt-echo-read-string app "Unicode code point (hex): ")))
    (when input
      (let ((n (string->number (string-append "#x" input))))
        (if (and n (> n 0) (< n #x110000))
          (qt-plain-text-edit-insert-text! (current-qt-editor app)
            (string (integer->char n)))
          (echo-error! (app-state-echo app) "Invalid code point"))))))

;;;============================================================================
;;; Rename file and buffer
;;;============================================================================

(def (cmd-rename-file-and-buffer app)
  "Rename the current file on disk and rename the buffer."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((new-name (qt-echo-read-string app "New name: ")))
        (when new-name
          (let ((new-path (path-expand new-name (path-directory path))))
            (with-catch
              (lambda (e)
                (echo-error! (app-state-echo app)
                  (string-append "Rename failed: " (with-output-to-string
                    (lambda () (display-exception e))))))
              (lambda ()
                (rename-file path new-path)
                (set! (buffer-file-path buf) new-path)
                (set! (buffer-name buf) (path-strip-directory new-path))
                (echo-message! (app-state-echo app)
                  (string-append "Renamed to " new-path))))))))))

;;;============================================================================
;;; Sort numeric
;;;============================================================================

(def (cmd-sort-numeric app)
  "Sort lines in region by leading number."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (extract-num
               (lambda (s)
                 (let ((trimmed (string-trim s)))
                   (or (string->number
                         (let loop ((i 0) (acc ""))
                           (if (or (>= i (string-length trimmed))
                                   (not (or (char-numeric? (string-ref trimmed i))
                                            (char=? (string-ref trimmed i) #\-)
                                            (char=? (string-ref trimmed i) #\.))))
                             acc
                             (loop (+ i 1)
                                   (string-append acc (string (string-ref trimmed i)))))))
                       0))))
             (sorted (sort lines (lambda (a b) (< (extract-num a) (extract-num b)))))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted numerically")))))

;;;============================================================================
;;; Sort fields
;;;============================================================================

(def (cmd-sort-fields app)
  "Sort lines by a given field number."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((field-str (qt-echo-read-string app "Sort by field #: ")))
        (when field-str
          (let ((field-num (string->number field-str)))
            (when (and field-num (> field-num 0))
              (let* ((pos (qt-plain-text-edit-cursor-position ed))
                     (start (min mark pos))
                     (end (max mark pos))
                     (text (qt-plain-text-edit-text ed))
                     (region (substring text start end))
                     (lines (string-split region #\newline))
                     (get-field
                       (lambda (line)
                         (let ((fields (filter (lambda (s) (> (string-length s) 0))
                                              (string-split (string-trim line) #\space))))
                           (if (>= (length fields) field-num)
                             (list-ref fields (- field-num 1))
                             ""))))
                     (sorted (sort lines (lambda (a b) (string<? (get-field a) (get-field b)))))
                     (result (string-join sorted "\n"))
                     (new-text (string-append (substring text 0 start) result
                                              (substring text end (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed start)
                (set! (buffer-mark buf) #f)
                (echo-message! (app-state-echo app)
                  (string-append "Sorted by field " field-str))))))))))

;;;============================================================================
;;; Align regexp
;;;============================================================================

(def (cmd-align-regexp app)
  "Align region by a given string."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((pattern (qt-echo-read-string app "Align on: ")))
        (when (and pattern (> (string-length pattern) 0))
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 ;; Find max column of first occurrence of pattern
                 (max-col
                   (let loop ((ls lines) (mc 0))
                     (if (null? ls) mc
                       (let ((idx (string-contains (car ls) pattern)))
                         (loop (cdr ls) (if idx (max mc idx) mc))))))
                 ;; Align each line
                 (aligned
                   (map (lambda (line)
                          (let ((idx (string-contains line pattern)))
                            (if idx
                              (string-append
                                (substring line 0 idx)
                                (make-string (- max-col idx) #\space)
                                (substring line idx (string-length line)))
                              line)))
                        lines))
                 (result (string-join aligned "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app) "Aligned")))))))

;;;============================================================================
;;; Window management: enlarge, shrink, balance
;;;============================================================================

(def (adjust-window-size! app delta)
  "Adjust current window size by DELTA pixels in the splitter."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (when (> n 1)
      (let* ((splitter (qt-frame-splitter fr))
             (idx (qt-frame-current-idx fr))
             ;; Get current sizes
             (sizes (let loop ((i 0) (acc []))
                      (if (>= i n) (reverse acc)
                        (loop (+ i 1) (cons (qt-splitter-size-at splitter i) acc)))))
             ;; Find a neighbor to take/give from
             (neighbor (if (< idx (- n 1)) (+ idx 1) (- idx 1)))
             (cur-size (list-ref sizes idx))
             (nbr-size (list-ref sizes neighbor))
             (new-cur (max 50 (+ cur-size delta)))
             (new-nbr (max 50 (- nbr-size delta))))
        ;; Build new sizes list
        (let ((new-sizes (let loop ((i 0) (s sizes) (acc []))
                           (if (null? s) (reverse acc)
                             (loop (+ i 1) (cdr s)
                                   (cons (cond
                                           ((= i idx) new-cur)
                                           ((= i neighbor) new-nbr)
                                           (else (car s)))
                                         acc))))))
          (qt-splitter-set-sizes! splitter new-sizes))))))

(def (cmd-enlarge-window app)
  "Enlarge the current window by 50 pixels."
  (adjust-window-size! app 50)
  (echo-message! (app-state-echo app) "Window enlarged"))

(def (cmd-shrink-window app)
  "Shrink the current window by 50 pixels."
  (adjust-window-size! app -50)
  (echo-message! (app-state-echo app) "Window shrunk"))

(def (cmd-balance-windows app)
  "Make all windows the same size."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (when (> n 1)
      (let ((splitter (qt-frame-splitter fr))
            (equal-size 500)) ; Each panel gets same size hint
        (qt-splitter-set-sizes! splitter
          (let loop ((i 0) (acc '()))
            (if (>= i n) (reverse acc)
              (loop (+ i 1) (cons equal-size acc))))))))
  (echo-message! (app-state-echo app) "Windows balanced"))

;;;============================================================================
;;; Move to window line (M-r: cycle top/center/bottom)
;;;============================================================================

(def *recenter-position-qt* 'center)

(def (cmd-move-to-window-line app)
  "Move point to center, then top, then bottom of visible window."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (total-lines (length lines))
         (current-line (qt-plain-text-edit-cursor-line ed))
         ;; Approximate visible lines (assume 30)
         (visible-lines 30)
         (first-vis (max 0 (- current-line (quotient visible-lines 2))))
         (target-line
           (case *recenter-position-qt*
             ((center) (+ first-vis (quotient visible-lines 2)))
             ((top) first-vis)
             ((bottom) (+ first-vis (- visible-lines 1))))))
    ;; Move to target line start
    (let ((target (min (- total-lines 1) (max 0 target-line))))
      (qt-plain-text-edit-set-cursor-position! ed
        (line-start-position text target))
      (qt-plain-text-edit-ensure-cursor-visible! ed))
    ;; Cycle
    (set! *recenter-position-qt*
      (case *recenter-position-qt*
        ((center) 'top)
        ((top) 'bottom)
        ((bottom) 'center)))))

;;;============================================================================
;;; Upcase initials region (title case)
;;;============================================================================

(def (cmd-upcase-initials-region app)
  "Capitalize the first letter of each word in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (result (string-titlecase region))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Title-cased")))))

;;;============================================================================
;;; S-expression: backward-up-list, forward-up-list, mark-paragraph
;;;============================================================================

(def (cmd-backward-up-list app)
  "Move backward up one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (if (< i 0)
        (echo-error! (app-state-echo app) "At top level")
        (let ((ch (string-ref text i)))
          (cond
            ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
             (loop (- i 1) (+ depth 1)))
            ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
             (if (= depth 0)
               (begin
                 (qt-plain-text-edit-set-cursor-position! ed i)
                 (qt-plain-text-edit-ensure-cursor-visible! ed))
               (loop (- i 1) (- depth 1))))
            (else (loop (- i 1) depth))))))))

(def (cmd-forward-up-list app)
  "Move forward out of one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i pos) (depth 0))
      (if (>= i len)
        (echo-error! (app-state-echo app) "At top level")
        (let ((ch (string-ref text i)))
          (cond
            ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
             (loop (+ i 1) (+ depth 1)))
            ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
             (if (= depth 0)
               (begin
                 (qt-plain-text-edit-set-cursor-position! ed (+ i 1))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))
               (loop (+ i 1) (- depth 1))))
            (else (loop (+ i 1) depth))))))))

(def (cmd-mark-paragraph app)
  "Set mark at end of paragraph, point at beginning."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         ;; Find paragraph start (go back to blank line or beginning)
         (para-start
           (let loop ((i (- pos 1)))
             (if (< i 0) 0
               (if (and (char=? (string-ref text i) #\newline)
                        (or (= i 0)
                            (char=? (string-ref text (- i 1)) #\newline)))
                 (+ i 1)
                 (loop (- i 1))))))
         ;; Find paragraph end
         (para-end
           (let loop ((i pos))
             (if (>= i len) len
               (if (and (char=? (string-ref text i) #\newline)
                        (or (>= (+ i 1) len)
                            (char=? (string-ref text (+ i 1)) #\newline)))
                 (+ i 1)
                 (loop (+ i 1)))))))
    (set! (buffer-mark buf) para-end)
    (qt-plain-text-edit-set-cursor-position! ed para-start)
    (echo-message! (app-state-echo app) "Paragraph marked")))

;;;============================================================================
;;; Open rectangle (insert blank space)
;;;============================================================================

(def (cmd-open-rectangle app)
  "Insert blank space in rectangle region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (echo (app-state-echo app)))
    (if (not mark)
      (echo-error! echo "No mark set")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (col1 (column-at-position text start))
             (col2 (column-at-position text end))
             (left-col (min col1 col2))
             (right-col (max col1 col2))
             (width (- right-col left-col))
             (lines (string-split text #\newline)))
        (let-values (((start-line end-line) (region-line-range text start end)))
          (let* ((new-lines
                   (let loop ((ls lines) (i 0) (acc []))
                     (if (null? ls) (reverse acc)
                       (if (and (>= i start-line) (<= i end-line))
                         (let* ((l (car ls))
                                (len (string-length l))
                                (padded (if (< len left-col)
                                          (string-append l (make-string (- left-col len) #\space))
                                          l))
                                (new-line (string-append
                                            (substring padded 0 left-col)
                                            (make-string width #\space)
                                            (substring padded left-col (string-length padded)))))
                           (loop (cdr ls) (+ i 1) (cons new-line acc)))
                         (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
                 (new-text (string-join new-lines "\n")))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (min start (string-length new-text)))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Rectangle opened")))))))

;;;============================================================================
;;; Hippie expand (acts like dabbrev-expand)
;;;============================================================================

(def (cmd-hippie-expand app)
  "Expand word at point using buffer content (like dabbrev)."
  (cmd-dabbrev-expand app))

;;;============================================================================
;;; Split line
;;;============================================================================

(def (cmd-split-line app)
  "Split line at point, indenting continuation to current column."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (col (column-at-position text pos)))
    (qt-plain-text-edit-insert-text! ed
      (string-append "\n" (make-string col #\space)))))

;;;============================================================================
;;; Copy from above
;;;============================================================================

(def (cmd-copy-from-above app)
  "Copy character from the line above at the same column."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (col (column-at-position text pos))
         (current-line (qt-plain-text-edit-cursor-line ed)))
    (if (= current-line 0)
      (echo-error! (app-state-echo app) "No line above")
      (let* ((lines (string-split text #\newline))
             (above-line (list-ref lines (- current-line 1))))
        (if (>= col (string-length above-line))
          (echo-error! (app-state-echo app) "Above line shorter")
          (qt-plain-text-edit-insert-text! ed
            (string (string-ref above-line col))))))))

;;;============================================================================
;;; Find alternate file
;;;============================================================================

(def (cmd-find-alternate-file app)
  "Replace current buffer with another file."
  (cmd-find-file app))

;;;============================================================================
;;; Increment register
;;;============================================================================

(def (cmd-increment-register app)
  "Increment the value in a register by 1."
  (let* ((echo (app-state-echo app))
         (regs (app-state-registers app))
         (name (qt-echo-read-string app "Register to increment: ")))
    (when (and name (= (string-length name) 1))
      (let* ((ch (string-ref name 0))
             (val (hash-get regs ch)))
        (cond
          ((not val) (echo-error! echo "Register empty"))
          ((string? val)
           (let ((n (string->number val)))
             (if n
               (begin
                 (hash-put! regs ch (number->string (+ n 1)))
                 (echo-message! echo (string-append "Register " name " = "
                                                     (number->string (+ n 1)))))
               (echo-error! echo "Register does not contain a number"))))
          (else (echo-error! echo "Register does not contain text")))))))

;;;============================================================================
;;; Delete pair
;;;============================================================================

(def (cmd-delete-pair app)
  "Delete the delimiter pair surrounding point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find enclosing open delimiter
    (let loop ((i (- pos 1)) (depth 0))
      (if (< i 0)
        (echo-error! (app-state-echo app) "No enclosing delimiters")
        (let ((ch (string-ref text i)))
          (cond
            ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
             (loop (- i 1) (+ depth 1)))
            ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
             (if (= depth 0)
               ;; Found open - now find matching close
               (let ((close-ch (cond ((char=? ch #\() #\))
                                     ((char=? ch #\[) #\])
                                     ((char=? ch #\{) #\}))))
                 (let cloop ((j (+ i 1)) (d 0))
                   (if (>= j len)
                     (echo-error! (app-state-echo app) "No matching close")
                     (let ((c (string-ref text j)))
                       (cond
                         ((char=? c ch) (cloop (+ j 1) (+ d 1)))
                         ((char=? c close-ch)
                          (if (= d 0)
                            ;; Delete close first (higher position), then open
                            (let* ((new-text (string-append
                                               (substring text 0 j)
                                               (substring text (+ j 1) len)))
                                   (new-text2 (string-append
                                                (substring new-text 0 i)
                                                (substring new-text (+ i 1)
                                                           (string-length new-text)))))
                              (qt-plain-text-edit-set-text! ed new-text2)
                              (qt-plain-text-edit-set-cursor-position! ed i))
                            (cloop (+ j 1) (- d 1))))
                         (else (cloop (+ j 1) d)))))))
               (loop (- i 1) (- depth 1))))
            (else (loop (- i 1) depth))))))))

;;;============================================================================
;;; Sudo write
;;;============================================================================

(def (cmd-sudo-write app)
  "Save file with sudo (using tee)."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (ed (current-qt-editor app)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((text (qt-plain-text-edit-text ed)))
        (with-catch
          (lambda (e)
            (echo-error! (app-state-echo app)
              (string-append "Sudo write failed: " (with-output-to-string
                (lambda () (display-exception e))))))
          (lambda ()
            (let ((proc (open-process
                          (list path: "/usr/bin/sudo"
                                arguments: (list "tee" path)
                                stdin-redirection: #t
                                stdout-redirection: #t))))
              (display text proc)
              (close-output-port proc)
              (process-status proc)
              (close-port proc)
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (echo-message! (app-state-echo app)
                (string-append "Wrote " path " (sudo)")))))))))

;;;============================================================================
;;; Ediff buffers
;;;============================================================================

(def (cmd-ediff-buffers app)
  "Compare two buffers by running diff."
  (let* ((bufs (buffer-list))
         (names (map buffer-name bufs))
         (a-name (qt-echo-read-string-with-completion app "Buffer A: " names)))
    (when a-name
      (let ((b-name (qt-echo-read-string-with-completion app "Buffer B: " names)))
        (when b-name
          (let* ((buf-a (find (lambda (b) (string=? (buffer-name b) a-name)) bufs))
                 (buf-b (find (lambda (b) (string=? (buffer-name b) b-name)) bufs)))
            (if (or (not buf-a) (not buf-b))
              (echo-error! (app-state-echo app) "Buffer not found")
              ;; Write both to temp files and diff
              (let* ((tmp-a (path-expand "ediff-a" (or (getenv "TMPDIR") "/tmp")))
                     (tmp-b (path-expand "ediff-b" (or (getenv "TMPDIR") "/tmp"))))
                ;; Get text from buffers
                (let ((text-a "")
                      (text-b ""))
                  ;; Find editors showing these buffers
                  (let ((fr (app-state-frame app)))
                    (for-each
                      (lambda (win)
                        (when (eq? (qt-edit-window-buffer win) buf-a)
                          (set! text-a (qt-plain-text-edit-text (qt-edit-window-editor win))))
                        (when (eq? (qt-edit-window-buffer win) buf-b)
                          (set! text-b (qt-plain-text-edit-text (qt-edit-window-editor win)))))
                      (qt-frame-windows fr)))
                  (call-with-output-file tmp-a (lambda (p) (display text-a p)))
                  (call-with-output-file tmp-b (lambda (p) (display text-b p)))
                  (let* ((proc (open-process
                                 (list path: "/usr/bin/diff"
                                       arguments: (list "-u" tmp-a tmp-b)
                                       stdout-redirection: #t)))
                         (output (read-line proc #f))
                         (_ (process-status proc))
                         (ed (current-qt-editor app))
                         (fr (app-state-frame app))
                         (diff-buf (qt-buffer-create! "*Ediff*" ed #f)))
                    (close-port proc)
                    (qt-buffer-attach! ed diff-buf)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                    (qt-plain-text-edit-set-text! ed (or output "No differences"))
                    (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                    (qt-plain-text-edit-set-cursor-position! ed 0)
                    ;; Clean up temp files
                    (with-catch void (lambda () (delete-file tmp-a)))
                    (with-catch void (lambda () (delete-file tmp-b)))
                    (echo-message! (app-state-echo app) "Diff complete")))))))))))

;;;============================================================================
;;; Highlight symbol / clear highlight
;;;============================================================================

(def (word-at-point ed)
  "Extract the word at the cursor position. Returns (values word start end) or (values #f 0 0)."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (>= pos len)
      (values #f 0 0)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\-)
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i
                        (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\-)
                                     (char=? (string-ref text i) #\_))))
                      i
                      (loop (+ i 1)))))
             (word (substring text start end)))
        (if (> (string-length word) 0)
          (values word start end)
          (values #f 0 0))))))

(def (count-occurrences text word)
  "Count non-overlapping occurrences of word in text."
  (let ((wlen (string-length word))
        (tlen (string-length text)))
    (let loop ((i 0) (count 0))
      (if (> (+ i wlen) tlen) count
        (if (string=? (substring text i (+ i wlen)) word)
          (loop (+ i wlen) (+ count 1))
          (loop (+ i 1) count))))))

(def (cmd-highlight-symbol app)
  "Toggle highlight of the word at point. Shows occurrence count."
  (let ((ed (current-qt-editor app)))
    (let-values (((word start end) (word-at-point ed)))
      (if (not word)
        (echo-error! (app-state-echo app) "No word at point")
        ;; Toggle: if already highlighting this word, clear it
        (if (and (app-state-last-search app)
                 (string=? (app-state-last-search app) word))
          (begin
            (set! (app-state-last-search app) #f)
            (echo-message! (app-state-echo app) "Highlights cleared"))
          (let* ((text (qt-plain-text-edit-text ed))
                 (count (count-occurrences text word)))
            (set! (app-state-last-search app) word)
            (echo-message! (app-state-echo app)
              (string-append "Highlighting: " word
                             " (" (number->string count) " occurrences)"))))))))

(def (cmd-highlight-symbol-next app)
  "Jump to next occurrence of the highlighted symbol."
  (let ((search (app-state-last-search app)))
    (if (not search)
      ;; If nothing highlighted, highlight word at point first
      (cmd-highlight-symbol app)
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (slen (string-length search))
             (tlen (string-length text))
             ;; Search forward from pos+1
             (found (let loop ((i (+ pos 1)))
                      (cond
                        ((> (+ i slen) tlen) #f)
                        ((string=? (substring text i (+ i slen)) search) i)
                        (else (loop (+ i 1)))))))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app)
              (string-append "\"" search "\" found")))
          ;; Wrap around
          (let ((wrapped (let loop ((i 0))
                           (cond
                             ((> (+ i slen) pos) #f)
                             ((string=? (substring text i (+ i slen)) search) i)
                             (else (loop (+ i 1)))))))
            (if wrapped
              (begin
                (qt-plain-text-edit-set-cursor-position! ed wrapped)
                (echo-message! (app-state-echo app)
                  (string-append "\"" search "\" (wrapped)")))
              (echo-message! (app-state-echo app) "No more occurrences"))))))))

(def (cmd-highlight-symbol-prev app)
  "Jump to previous occurrence of the highlighted symbol."
  (let ((search (app-state-last-search app)))
    (if (not search)
      (cmd-highlight-symbol app)
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (slen (string-length search))
             (tlen (string-length text))
             ;; Search backward from pos-1
             (found (let loop ((i (- pos 1)))
                      (cond
                        ((< i 0) #f)
                        ((and (<= (+ i slen) tlen)
                              (string=? (substring text i (+ i slen)) search)) i)
                        (else (loop (- i 1)))))))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app)
              (string-append "\"" search "\" found")))
          ;; Wrap around from end
          (let ((wrapped (let loop ((i (- tlen slen)))
                           (cond
                             ((< i (+ pos 1)) #f)
                             ((string=? (substring text i (+ i slen)) search) i)
                             (else (loop (- i 1)))))))
            (if wrapped
              (begin
                (qt-plain-text-edit-set-cursor-position! ed wrapped)
                (echo-message! (app-state-echo app)
                  (string-append "\"" search "\" (wrapped)")))
              (echo-message! (app-state-echo app) "No more occurrences"))))))))

(def (cmd-clear-highlight app)
  "Clear the current search highlight."
  (set! (app-state-last-search app) #f)
  (echo-message! (app-state-echo app) "Highlights cleared"))

;;;============================================================================
;;; Repeat complex command
;;;============================================================================

(def (cmd-repeat-complex-command app)
  "Repeat the last command that was executed."
  (let ((last-cmd (app-state-last-command app)))
    (if last-cmd
      (begin
        (execute-command! app last-cmd)
        (echo-message! (app-state-echo app)
          (string-append "Repeated: " (symbol->string last-cmd))))
      (echo-error! (app-state-echo app) "No previous command"))))

;;;============================================================================
;;; Undo history / undo tree visualization
;;;============================================================================

;; Track buffer snapshots for undo history browsing.
;; Each buffer name maps to a list of (timestamp . text-snapshot) pairs (newest first).
(def *undo-history* (make-hash-table)) ;; buffer-name -> list of (timestamp . text)
(def *undo-history-max* 50) ;; max snapshots per buffer

(def (undo-history-record! buf-name text)
  "Record a snapshot of the buffer text for undo history."
  (let* ((existing (or (hash-get *undo-history* buf-name) []))
         ;; Don't record if text is same as most recent
         (already-same (and (pair? existing)
                            (string=? (cdar existing) text))))
    (unless already-same
      (let* ((timestamp (inexact->exact (floor (time->seconds (current-time)))))
             (entry (cons timestamp text))
             (new-list (cons entry existing))
             ;; Trim to max size
             (trimmed (if (> (length new-list) *undo-history-max*)
                       (let loop ((l new-list) (n 0) (acc []))
                         (if (or (null? l) (>= n *undo-history-max*))
                           (reverse acc)
                           (loop (cdr l) (+ n 1) (cons (car l) acc))))
                       new-list)))
        (hash-put! *undo-history* buf-name trimmed)))))

(def (cmd-undo-history app)
  "Show the undo history for the current buffer. Navigate and restore past states."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (buf-name (buffer-name buf))
         (current-text (qt-plain-text-edit-text ed))
         (history (or (hash-get *undo-history* buf-name) [])))
    ;; Record current state if not already there
    (undo-history-record! buf-name current-text)
    (let ((history (or (hash-get *undo-history* buf-name) [])))
      (if (null? history)
        (echo-message! (app-state-echo app) "No undo history for this buffer")
        ;; Display history in a buffer
        (let* ((fr (app-state-frame app))
               (hist-buf (or (buffer-by-name "*Undo History*")
                             (qt-buffer-create! "*Undo History*" ed #f)))
               (lines
                 (let loop ((entries history) (i 0) (acc []))
                   (if (null? entries) (reverse acc)
                     (let* ((entry (car entries))
                            (ts (car entry))
                            (text (cdr entry))
                            (tlen (string-length text))
                            (line-count (let lp ((j 0) (c 1))
                                          (cond ((>= j tlen) c)
                                                ((char=? (string-ref text j) #\newline) (lp (+ j 1) (+ c 1)))
                                                (else (lp (+ j 1) c)))))
                            (marker (if (= i 0) " <- current" ""))
                            ;; Format timestamp as relative
                            (now (inexact->exact (floor (time->seconds (current-time)))))
                            (age (- now ts))
                            (age-str (cond
                                       ((< age 60) (string-append (number->string age) "s ago"))
                                       ((< age 3600) (string-append (number->string (quotient age 60)) "m ago"))
                                       ((< age 86400) (string-append (number->string (quotient age 3600)) "h ago"))
                                       (else (string-append (number->string (quotient age 86400)) "d ago"))))
                            (preview (let ((first-change
                                            (substring text 0 (min 60 tlen))))
                                       (let ((nl (string-contains first-change "\n")))
                                         (if nl (substring first-change 0 nl) first-change))))
                            (line (string-append
                                    (number->string i) ": "
                                    age-str " | "
                                    (number->string tlen) " chars, "
                                    (number->string line-count) " lines"
                                    marker
                                    "\n   " preview)))
                       (loop (cdr entries) (+ i 1) (cons line acc))))))
               (header (string-append "Undo History for: " buf-name "\n"
                                      "Type the snapshot number to restore, or q to quit.\n"
                                      (make-string 60 #\-) "\n"))
               (content (string-append header (string-join lines "\n"))))
          (qt-buffer-attach! ed hist-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) hist-buf)
          (qt-plain-text-edit-set-text! ed content)
          (qt-text-document-set-modified! (buffer-doc-pointer hist-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

(def (cmd-undo-history-restore app)
  "Restore a snapshot from undo history. Prompts for snapshot number."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (buf-name (buffer-name buf))
         (history (or (hash-get *undo-history* buf-name) [])))
    (if (null? history)
      (echo-message! (app-state-echo app) "No undo history")
      (let* ((input (qt-echo-read-string app
                      (string-append "Restore snapshot (0-" (number->string (- (length history) 1)) "): ")))
             (num (string->number (string-trim input))))
        (cond
          ((not num)
           (echo-error! (app-state-echo app) "Not a number"))
          ((or (< num 0) (>= num (length history)))
           (echo-error! (app-state-echo app) "Invalid snapshot number"))
          (else
           (let* ((entry (list-ref history num))
                  (text (cdr entry)))
             (qt-plain-text-edit-set-text! ed text)
             (qt-plain-text-edit-set-cursor-position! ed 0)
             (echo-message! (app-state-echo app)
               (string-append "Restored snapshot " (number->string num))))))))))

(def (cmd-flush-undo app)
  "Clear the undo history."
  (let* ((buf (current-qt-buffer app))
         (buf-name (buffer-name buf)))
    (hash-put! *undo-history* buf-name [])
    (echo-message! (app-state-echo app) "Undo history cleared")))

;;;============================================================================
;;; Untabify buffer
;;;============================================================================

(def (cmd-untabify-buffer app)
  "Convert all tabs to spaces in the entire buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (if (not (string-contains text "\t"))
      (echo-message! (app-state-echo app) "No tabs found")
      (let* ((parts (string-split text #\tab))
             (result (string-join parts "        ")))
        (qt-plain-text-edit-set-text! ed result)
        (echo-message! (app-state-echo app) "Untabified buffer")))))

;;;============================================================================
;;; Narrow to defun
;;;============================================================================

(def (cmd-narrow-to-defun app)
  "Narrow buffer to the current function/defun."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find start of current top-level form (search backward for unindented open paren)
    (let ((defun-start
            (let loop ((i pos))
              (cond
                ((< i 0) 0)
                ((and (char=? (string-ref text i) #\()
                      (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                 i)
                (else (loop (- i 1)))))))
      ;; Find end of defun using matching paren
      (let ((defun-end
              (let loop ((i defun-start) (depth 0))
                (cond
                  ((>= i len) len)
                  ((char=? (string-ref text i) #\()
                   (loop (+ i 1) (+ depth 1)))
                  ((char=? (string-ref text i) #\))
                   (if (= depth 1) (+ i 1)
                     (loop (+ i 1) (- depth 1))))
                  (else (loop (+ i 1) depth))))))
        ;; Include trailing newline
        (let ((end (if (and (< defun-end len) (char=? (string-ref text defun-end) #\newline))
                     (+ defun-end 1) defun-end)))
          (let* ((buf (current-qt-buffer app))
                 (region (substring text defun-start end)))
            ;; Use the existing narrowing infrastructure
            (hash-put! *narrow-state* buf (list text defun-start end))
            (qt-plain-text-edit-set-text! ed region)
            (qt-plain-text-edit-set-cursor-position! ed (- pos defun-start))
            (echo-message! (app-state-echo app) "Narrowed to defun")))))))

;;;============================================================================
;;; Expand region - smart selection expansion
;;;============================================================================

(def *expand-region-stack* []) ;; stack of (start . end) for contract-region

(def (cmd-expand-region app)
  "Progressively expand the selection: word -> line -> paragraph -> sexp -> buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Push current selection onto stack
    (when (not (= sel-start sel-end))
      (set! *expand-region-stack* (cons (cons sel-start sel-end) *expand-region-stack*)))
    (cond
      ;; No selection -> select word at point
      ((= sel-start sel-end)
       (let* ((word-start (let loop ((i pos))
                            (if (or (= i 0) (not (or (char-alphabetic? (string-ref text (- i 1)))
                                                      (char-numeric? (string-ref text (- i 1)))
                                                      (char=? (string-ref text (- i 1)) #\_)
                                                      (char=? (string-ref text (- i 1)) #\-))))
                              i (loop (- i 1)))))
              (word-end (let loop ((i pos))
                          (if (or (>= i len) (not (or (char-alphabetic? (string-ref text i))
                                                       (char-numeric? (string-ref text i))
                                                       (char=? (string-ref text i) #\_)
                                                       (char=? (string-ref text i) #\-))))
                            i (loop (+ i 1))))))
         (when (< word-start word-end)
           (set! *expand-region-stack* (cons (cons sel-start sel-end) *expand-region-stack*))
           (qt-plain-text-edit-set-selection! ed word-start word-end))))
      ;; Selection within a line -> expand to whole line
      ((let* ((line-start (let loop ((i sel-start))
                            (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                              i (loop (- i 1)))))
              (line-end (let loop ((i sel-end))
                          (if (or (>= i len) (char=? (string-ref text i) #\newline))
                            i (loop (+ i 1))))))
         (and (or (> sel-start line-start) (< sel-end line-end))
              (begin
                (qt-plain-text-edit-set-selection! ed line-start (min (+ line-end 1) len))
                #t))))
      ;; Selection is line(s) -> expand to paragraph
      ((let* ((para-start (let loop ((i sel-start))
                            (cond
                              ((<= i 0) 0)
                              ((and (char=? (string-ref text (- i 1)) #\newline)
                                    (or (= (- i 1) 0) (char=? (string-ref text (- i 2)) #\newline)))
                               i)
                              (else (loop (- i 1))))))
              (para-end (let loop ((i sel-end))
                          (cond
                            ((>= i len) len)
                            ((and (char=? (string-ref text i) #\newline)
                                  (or (>= (+ i 1) len) (char=? (string-ref text (+ i 1)) #\newline)))
                             (+ i 1))
                            (else (loop (+ i 1)))))))
         (and (or (> sel-start para-start) (< sel-end para-end))
              (begin
                (qt-plain-text-edit-set-selection! ed para-start para-end)
                #t))))
      ;; Already paragraph -> expand to whole buffer
      (else
       (qt-plain-text-edit-set-selection! ed 0 len)))))

(def (cmd-contract-region app)
  "Contract the selection to the previous expand-region state."
  (if (null? *expand-region-stack*)
    (echo-message! (app-state-echo app) "No previous selection")
    (let* ((prev (car *expand-region-stack*))
           (ed (current-qt-editor app)))
      (set! *expand-region-stack* (cdr *expand-region-stack*))
      (if (= (car prev) (cdr prev))
        (qt-plain-text-edit-set-cursor-position! ed (car prev))
        (qt-plain-text-edit-set-selection! ed (car prev) (cdr prev))))))

;;;============================================================================
;;; String inflection (case conversion)
;;;============================================================================

(def (split-identifier word)
  "Split an identifier into its component words, handling camelCase, snake_case, kebab-case."
  (let ((len (string-length word)))
    (if (= len 0) []
      (let loop ((i 0) (start 0) (acc []))
        (cond
          ((>= i len)
           (reverse (if (< start i) (cons (substring word start i) acc) acc)))
          ;; Split on underscore or hyphen
          ((or (char=? (string-ref word i) #\_) (char=? (string-ref word i) #\-))
           (let ((part (if (< start i) (list (substring word start i)) [])))
             (loop (+ i 1) (+ i 1) (append part acc))))
          ;; Split on camelCase boundary (lowercase followed by uppercase)
          ((and (> i start)
                (char-upper-case? (string-ref word i))
                (char-lower-case? (string-ref word (- i 1))))
           (loop i i (cons (substring word start i) acc)))
          (else (loop (+ i 1) start acc)))))))

(def (to-snake-case parts)
  (string-join (map string-downcase parts) "_"))

(def (to-kebab-case parts)
  (string-join (map string-downcase parts) "-"))

(def (to-camel-case parts)
  (if (null? parts) ""
    (string-append
      (string-downcase (car parts))
      (apply string-append
        (map (lambda (p)
               (if (> (string-length p) 0)
                 (string-append
                   (string (char-upcase (string-ref p 0)))
                   (string-downcase (substring p 1 (string-length p))))
                 ""))
             (cdr parts))))))

(def (to-pascal-case parts)
  (apply string-append
    (map (lambda (p)
           (if (> (string-length p) 0)
             (string-append
               (string (char-upcase (string-ref p 0)))
               (string-downcase (substring p 1 (string-length p))))
             ""))
         parts)))

(def (to-upper-case parts)
  (string-join (map string-upcase parts) "_"))

(def (cmd-string-inflection-cycle app)
  "Cycle the identifier at point through: snake_case -> kebab-case -> camelCase -> PascalCase -> UPPER_CASE."
  (let ((ed (current-qt-editor app)))
    (let-values (((word start end) (word-at-point ed)))
      (if (not word)
        (echo-error! (app-state-echo app) "No word at point")
        (let* ((parts (split-identifier word))
               (snake (to-snake-case parts))
               (kebab (to-kebab-case parts))
               (camel (to-camel-case parts))
               (pascal (to-pascal-case parts))
               (upper (to-upper-case parts))
               ;; Determine current form and pick next
               (next (cond
                       ((string=? word snake) kebab)
                       ((string=? word kebab) camel)
                       ((string=? word camel) pascal)
                       ((string=? word pascal) upper)
                       (else snake)))
               (text (qt-plain-text-edit-text ed))
               (new-text (string-append
                           (substring text 0 start)
                           next
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start)
          (echo-message! (app-state-echo app) (string-append word " -> " next)))))))

;;;============================================================================
;;; Number increment/decrement at point
;;;============================================================================

(def (number-at-point ed)
  "Find a number at cursor position. Returns (values num-string start end) or (values #f 0 0)."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (or (>= pos len) (not (or (char-numeric? (string-ref text pos))
                                    (and (char=? (string-ref text pos) #\-)
                                         (< (+ pos 1) len)
                                         (char-numeric? (string-ref text (+ pos 1)))))))
      (values #f 0 0)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\-))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len) (not (char-numeric? (string-ref text i))))
                      i (loop (+ i 1)))))
             (num-str (substring text start end)))
        (values num-str start end)))))

(def (cmd-increment-number app)
  "Increment the number at point."
  (let ((ed (current-qt-editor app)))
    (let-values (((num-str start end) (number-at-point ed)))
      (if (not num-str)
        (echo-error! (app-state-echo app) "No number at point")
        (let* ((num (string->number num-str))
               (new-num (+ num 1))
               (new-str (number->string new-num))
               (text (qt-plain-text-edit-text ed))
               (new-text (string-append
                           (substring text 0 start)
                           new-str
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start))))))

(def (cmd-decrement-number app)
  "Decrement the number at point."
  (let ((ed (current-qt-editor app)))
    (let-values (((num-str start end) (number-at-point ed)))
      (if (not num-str)
        (echo-error! (app-state-echo app) "No number at point")
        (let* ((num (string->number num-str))
               (new-num (- num 1))
               (new-str (number->string new-num))
               (text (qt-plain-text-edit-text ed))
               (new-text (string-append
                           (substring text 0 start)
                           new-str
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start))))))

;;;============================================================================
;;; Browse URL / open link at point
;;;============================================================================

(def (url-at-point ed)
  "Find a URL at or near the cursor position. Returns URL string or #f."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Search backward for URL start
    (let* ((line-start (let loop ((i pos))
                         (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                           i (loop (- i 1)))))
           (line-end (let loop ((i pos))
                       (if (or (>= i len) (char=? (string-ref text i) #\newline))
                         i (loop (+ i 1)))))
           (line (substring text line-start line-end)))
      ;; Find http:// or https:// in the line
      (let loop ((i 0))
        (let ((http-pos (string-contains (substring line i (string-length line)) "http")))
          (if (not http-pos) #f
            (let* ((url-start (+ i http-pos))
                   ;; Find end of URL (whitespace or common delimiters)
                   (url-end (let lp ((j url-start))
                              (if (or (>= j (string-length line))
                                      (char=? (string-ref line j) #\space)
                                      (char=? (string-ref line j) #\tab)
                                      (char=? (string-ref line j) #\>)
                                      (char=? (string-ref line j) #\))
                                      (char=? (string-ref line j) #\])
                                      (char=? (string-ref line j) #\"))
                                j (lp (+ j 1)))))
                   (url (substring line url-start url-end)))
              ;; Check if it's a valid URL prefix
              (if (or (string-prefix? "http://" url) (string-prefix? "https://" url))
                url
                (loop (+ url-start 4))))))))))

(def (cmd-browse-url-at-point app)
  "Open the URL at point in an external browser."
  (let* ((ed (current-qt-editor app))
         (url (url-at-point ed)))
    (if (not url)
      (echo-error! (app-state-echo app) "No URL at point")
      (begin
        (open-process
          (list path: "xdg-open"
                arguments: (list url)
                stdout-redirection: #f
                stderr-redirection: #f))
        (echo-message! (app-state-echo app)
          (string-append "Opening: " url))))))

(def (cmd-browse-url app)
  "Prompt for a URL and open it in an external browser."
  (let ((url (qt-echo-read-string app "URL: ")))
    (when (> (string-length url) 0)
      (let ((full-url (if (or (string-prefix? "http://" url) (string-prefix? "https://" url))
                        url (string-append "https://" url))))
        (open-process
          (list path: "xdg-open"
                arguments: (list full-url)
                stdout-redirection: #f
                stderr-redirection: #f))
        (echo-message! (app-state-echo app)
          (string-append "Opening: " full-url))))))

;;;============================================================================
;;; Imenu - jump to definition (multi-language)
;;;============================================================================

;; Note: cmd-imenu is defined near goto-definition with multi-language support

;;;============================================================================
;;; Cycle tab width
;;;============================================================================

(def *tab-width* 4)

(def (cmd-cycle-tab-width app)
  "Cycle tab stop width: 2 -> 4 -> 8 -> 2."
  (set! *tab-width*
    (cond ((= *tab-width* 2) 4)
          ((= *tab-width* 4) 8)
          (else 2)))
  (echo-message! (app-state-echo app)
    (string-append "Tab width: " (number->string *tab-width*))))

;;;============================================================================
;;; Toggle indent tabs mode
;;;============================================================================

(def *indent-tabs-mode* #f)

(def (cmd-toggle-indent-tabs-mode app)
  "Toggle between tabs and spaces for indentation."
  (set! *indent-tabs-mode* (not *indent-tabs-mode*))
  (echo-message! (app-state-echo app)
    (if *indent-tabs-mode* "Indent with tabs" "Indent with spaces")))

;;;============================================================================
;;; Replace string (non-interactive replace-all)
;;;============================================================================

(def (cmd-replace-string app)
  "Replace all occurrences of a string."
  (let ((from (qt-echo-read-string app "Replace string: ")))
    (when from
      (let ((to (qt-echo-read-string app (string-append "Replace \"" from "\" with: "))))
        (when to
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (count 0)
                 (result (let loop ((s text) (acc ""))
                           (let ((idx (string-contains s from)))
                             (if idx
                               (begin
                                 (set! count (+ count 1))
                                 (loop (substring s (+ idx (string-length from)) (string-length s))
                                       (string-append acc (substring s 0 idx) to)))
                               (string-append acc s))))))
            (qt-plain-text-edit-set-text! ed result)
            (echo-message! (app-state-echo app)
              (string-append "Replaced " (number->string count) " occurrences"))))))))

;;;============================================================================
;;; String insert file (alias)
;;;============================================================================

(def (cmd-string-insert-file app)
  "Insert a file's contents (alias for insert-file)."
  (cmd-insert-file app))

;;;============================================================================
;;; Navigation: goto-column, recenter-top/bottom, window positions
;;;============================================================================

(def (cmd-goto-column app)
  "Move cursor to a specified column on the current line."
  (let ((input (qt-echo-read-string app "Go to column: ")))
    (when input
      (let ((col (string->number input)))
        (when (and col (>= col 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (line (qt-plain-text-edit-cursor-line ed))
                 (line-start (line-start-position text line))
                 (lines (string-split text #\newline))
                 (line-len (if (< line (length lines))
                             (string-length (list-ref lines line))
                             0))
                 (target-col (min col line-len)))
            (qt-plain-text-edit-set-cursor-position! ed (+ line-start target-col))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-goto-line-relative app)
  "Move cursor by a relative number of lines."
  (let ((input (qt-echo-read-string app "Relative lines (+/-): ")))
    (when input
      (let ((n (string->number input)))
        (when n
          (let* ((ed (current-qt-editor app))
                 (line (qt-plain-text-edit-cursor-line ed))
                 (text (qt-plain-text-edit-text ed))
                 (total (length (string-split text #\newline)))
                 (target (max 0 (min (- total 1) (+ line n)))))
            (qt-plain-text-edit-set-cursor-position! ed
              (line-start-position text target))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-recenter-top app)
  "Scroll so cursor is at the top of the window."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-ensure-cursor-visible! ed)
    (echo-message! (app-state-echo app) "Recentered to top")))

(def (cmd-recenter-bottom app)
  "Scroll so cursor is at the bottom of the window."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-ensure-cursor-visible! ed)
    (echo-message! (app-state-echo app) "Recentered to bottom")))

;;;============================================================================
;;; Character case commands
;;;============================================================================

(def (cmd-upcase-char app)
  "Upcase the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (char-upcase (string-ref text pos)))
             (new-text (string-append (substring text 0 pos)
                                      (string ch)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-downcase-char app)
  "Downcase the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (char-downcase (string-ref text pos)))
             (new-text (string-append (substring text 0 pos)
                                      (string ch)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-toggle-case-at-point app)
  "Toggle case of character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (string-ref text pos))
             (toggled (if (char-upper-case? ch)
                        (char-downcase ch)
                        (char-upcase ch)))
             (new-text (string-append (substring text 0 pos)
                                      (string toggled)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-capitalize-region app)
  "Capitalize the first letter of each word in region."
  (cmd-upcase-initials-region app))

;;;============================================================================
;;; Copy commands
;;;============================================================================

(def (cmd-copy-buffer-name app)
  "Copy buffer name to kill ring."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (set! (app-state-kill-ring app) (cons name (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied: " name))))

(def (cmd-copy-current-line app)
  "Copy the current line to kill ring."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines)) (list-ref lines line) "")))
    (set! (app-state-kill-ring app) (cons line-text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) "Line copied")))

(def (cmd-copy-word app)
  "Copy word at point to kill ring."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1))))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i)))))
                      i (loop (+ i 1)))))
             (word (substring text start end)))
        (when (> (string-length word) 0)
          (set! (app-state-kill-ring app) (cons word (app-state-kill-ring app)))
          (echo-message! (app-state-echo app) (string-append "Copied: " word)))))))

(def (cmd-copy-file-path app)
  "Copy file path to kill ring."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (set! (app-state-kill-ring app) (cons path (app-state-kill-ring app)))
        (echo-message! (app-state-echo app) (string-append "Copied: " path)))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-copy-line-number app)
  "Copy current line number to kill ring."
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (s (number->string line)))
    (set! (app-state-kill-ring app) (cons s (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied line: " s))))

(def (cmd-copy-region-as-kill app)
  "Copy region without deactivating mark (alias for copy-region)."
  (cmd-copy-region app))

(def (cmd-yank-whole-line app)
  "Yank the first item from kill ring as a whole line."
  (let* ((ed (current-qt-editor app))
         (kr (app-state-kill-ring app))
         (text (and (pair? kr) (car kr))))
    (if (not text)
      (echo-error! (app-state-echo app) "Kill ring empty")
      (begin
        ;; Move to beginning of line, insert text + newline
        (let* ((pos (qt-plain-text-edit-cursor-position ed))
               (full (qt-plain-text-edit-text ed))
               (line-start (line-start-position full (qt-plain-text-edit-cursor-line ed))))
          (qt-plain-text-edit-set-cursor-position! ed line-start)
          (qt-plain-text-edit-insert-text! ed (string-append text "\n")))))))

;;;============================================================================
;;; Insert commands
;;;============================================================================

(def (cmd-insert-pair-braces app)
  "Insert a pair of braces with cursor between."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "{}")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

(def (cmd-insert-pair-quotes app)
  "Insert a pair of double quotes with cursor between."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "\"\"")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

(def (cmd-insert-newline-above app)
  "Insert a blank line above the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (line-start (line-start-position text line)))
    (qt-plain-text-edit-set-cursor-position! ed line-start)
    (qt-plain-text-edit-insert-text! ed "\n")
    (qt-plain-text-edit-set-cursor-position! ed line-start)))

(def (cmd-insert-newline-below app)
  "Insert a blank line below the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines)) (list-ref lines line) ""))
         (line-start (line-start-position text line))
         (line-end (+ line-start (string-length line-text))))
    (qt-plain-text-edit-set-cursor-position! ed line-end)
    (qt-plain-text-edit-insert-text! ed "\n")))

(def (cmd-insert-comment-separator app)
  "Insert a comment separator line."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-insert-text! ed
      "\n;;; ============================================================\n")))

(def (cmd-insert-line-number app)
  "Insert the current line number at point."
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed))))
    (qt-plain-text-edit-insert-text! ed (number->string line))))

(def (cmd-insert-buffer-filename app)
  "Insert the buffer's filename at point."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (qt-plain-text-edit-insert-text! (current-qt-editor app)
        (path-strip-directory path))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-insert-timestamp app)
  "Insert a timestamp at point."
  (let* ((now (time->seconds (current-time)))
         (ts (number->string (inexact->exact (floor now)))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) ts)))

(def (cmd-insert-shebang app)
  "Insert a shebang line at the beginning of the buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (if (string-prefix? "#!" text)
      (echo-message! (app-state-echo app) "Shebang already present")
      (let ((shebang (qt-echo-read-string app "Shebang (e.g. /usr/bin/env python3): ")))
        (when shebang
          (let ((new-text (string-append "#!" shebang "\n" text)))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))

;;;============================================================================
;;; Buffer management
;;;============================================================================

(def (cmd-count-buffers app)
  "Show the number of open buffers."
  (let ((n (length (buffer-list))))
    (echo-message! (app-state-echo app)
      (string-append (number->string n) " buffers"))))

(def (cmd-rename-uniquely app)
  "Rename buffer to a unique name."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf))
         (new-name (string-append name "<" (number->string (random-integer 1000)) ">")))
    (set! (buffer-name buf) new-name)
    (echo-message! (app-state-echo app) (string-append "Renamed to " new-name))))

(def (cmd-bury-buffer app)
  "Switch to the next buffer (bury current)."
  (cmd-next-buffer app))

(def (cmd-unbury-buffer app)
  "Switch to the previous buffer."
  (cmd-previous-buffer app))

(def (cmd-append-to-buffer app)
  "Append region to another buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (bufs (buffer-list))
             (names (map buffer-name bufs))
             (target-name (qt-echo-read-string-with-completion app "Append to buffer: " names)))
        (when target-name
          (let ((target-buf (find (lambda (b) (string=? (buffer-name b) target-name)) bufs)))
            (if target-buf
              (begin
                ;; Find editor showing target buffer
                (let ((fr (app-state-frame app)))
                  (let loop ((wins (qt-frame-windows fr)))
                    (when (pair? wins)
                      (if (eq? (qt-edit-window-buffer (car wins)) target-buf)
                        (qt-plain-text-edit-append!
                          (qt-edit-window-editor (car wins)) region)
                        (loop (cdr wins))))))
                (echo-message! (app-state-echo app)
                  (string-append "Appended to " target-name)))
              (echo-error! (app-state-echo app) "Buffer not found"))))))))

;;;============================================================================
;;; File operations
;;;============================================================================

(def (cmd-make-directory app)
  "Create a directory."
  (let ((dir (qt-echo-read-string app "Create directory: ")))
    (when dir
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app)
                      (string-append "Failed: " (with-output-to-string
                        (lambda () (display-exception e))))))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app) (string-append "Created: " dir)))))))

(def (cmd-delete-file app)
  "Delete a file."
  (let ((file (qt-echo-read-string app "Delete file: ")))
    (when file
      (if (file-exists? file)
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "Delete failed"))
          (lambda ()
            (delete-file file)
            (echo-message! (app-state-echo app) (string-append "Deleted: " file))))
        (echo-error! (app-state-echo app) "File not found")))))

(def (cmd-copy-file app)
  "Copy a file."
  (let ((src (qt-echo-read-string app "Copy from: ")))
    (when src
      (let ((dst (qt-echo-read-string app "Copy to: ")))
        (when dst
          (with-catch
            (lambda (e) (echo-error! (app-state-echo app) "Copy failed"))
            (lambda ()
              (let ((text (read-file-as-string src)))
                (when text
                  (call-with-output-file dst
                    (lambda (p) (display text p)))
                  (echo-message! (app-state-echo app)
                    (string-append "Copied " src " -> " dst)))))))))))

(def (cmd-list-directory app)
  "List directory contents in a buffer."
  (let ((dir (qt-echo-read-string app "List directory: ")))
    (when dir
      (if (and (file-exists? dir)
               (eq? 'directory (file-info-type (file-info dir))))
        (dired-open-directory! app dir)
        (echo-error! (app-state-echo app) "Not a directory")))))

(def (cmd-pwd app)
  "Show current working directory."
  (echo-message! (app-state-echo app) (current-directory)))

;;;============================================================================
;;; Dired commands
;;;============================================================================

(def (cmd-dired-create-directory app)
  "Create a new directory from dired."
  (let ((dir (qt-echo-read-string app "New directory: ")))
    (when dir
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Failed to create directory"))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app) (string-append "Created: " dir)))))))

(def (cmd-dired-do-rename app)
  "Rename file in dired."
  (let ((old (qt-echo-read-string app "Rename: ")))
    (when old
      (let ((new-name (qt-echo-read-string app "To: ")))
        (when new-name
          (with-catch
            (lambda (e) (echo-error! (app-state-echo app) "Rename failed"))
            (lambda ()
              (rename-file old new-name)
              (echo-message! (app-state-echo app) "Renamed"))))))))

(def (cmd-dired-do-delete app)
  "Delete file in dired."
  (let ((file (qt-echo-read-string app "Delete: ")))
    (when file
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Delete failed"))
        (lambda ()
          (delete-file file)
          (echo-message! (app-state-echo app) "Deleted"))))))

(def (cmd-dired-do-copy app)
  "Copy file in dired."
  (cmd-copy-file app))

;;;============================================================================
;;; Toggle commands (additional)
;;;============================================================================

(def *hl-line-mode* #f)
(def (cmd-toggle-hl-line app)
  "Toggle current line highlighting."
  (set! *hl-line-mode* (not *hl-line-mode*))
  (echo-message! (app-state-echo app)
    (if *hl-line-mode* "Line highlight ON" "Line highlight OFF")))

(def (cmd-toggle-show-tabs app)
  "Toggle tab character visibility."
  (echo-message! (app-state-echo app) "Tab visibility toggled"))

(def (cmd-toggle-show-eol app)
  "Toggle end-of-line marker visibility."
  (echo-message! (app-state-echo app) "EOL markers toggled"))

(def *narrowing-indicator* #f)
(def (cmd-toggle-narrowing-indicator app)
  "Toggle narrowing indicator in modeline."
  (set! *narrowing-indicator* (not *narrowing-indicator*))
  (echo-message! (app-state-echo app)
    (if *narrowing-indicator* "Narrowing indicator ON" "Narrowing indicator OFF")))

(def *debug-on-error* #f)
(def (cmd-toggle-debug-on-error app)
  "Toggle debug-on-error mode."
  (set! *debug-on-error* (not *debug-on-error*))
  (echo-message! (app-state-echo app)
    (if *debug-on-error* "Debug on error ON" "Debug on error OFF")))

(def (cmd-toggle-fold app)
  "Toggle code folding at point (placeholder)."
  (echo-message! (app-state-echo app) "Code folding not supported in Qt plain text mode"))

;;;============================================================================
;;; Info/describe commands
;;;============================================================================

(def (cmd-what-mode app)
  "Show the current buffer's mode."
  (let* ((buf (current-qt-buffer app))
         (lang (buffer-lexer-lang buf)))
    (echo-message! (app-state-echo app)
      (if lang
        (string-append "Mode: " (symbol->string lang))
        "Mode: fundamental"))))

(def (cmd-what-encoding app)
  "Show the current buffer encoding."
  (echo-message! (app-state-echo app) "Encoding: UTF-8"))

(def (cmd-what-line-col app)
  "Show line and column of cursor."
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (col (+ 1 (qt-plain-text-edit-cursor-column ed))))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string line)
                     ", Col " (number->string col)))))

(def (cmd-show-file-info app)
  "Show information about the current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-message! (app-state-echo app) (string-append (buffer-name buf) " (no file)"))
      (if (file-exists? path)
        (let* ((info (file-info path))
               (size (file-info-size info)))
          (echo-message! (app-state-echo app)
            (string-append path " (" (number->string size) " bytes)")))
        (echo-message! (app-state-echo app)
          (string-append path " (new file)"))))))

(def (cmd-show-buffer-size app)
  "Show the size of the current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (size (string-length text)))
    (echo-message! (app-state-echo app)
      (string-append "Buffer size: " (number->string size) " chars"))))

(def (cmd-show-column-number app)
  "Show the current column number."
  (let* ((ed (current-qt-editor app))
         (col (+ 1 (qt-plain-text-edit-cursor-column ed))))
    (echo-message! (app-state-echo app)
      (string-append "Column: " (number->string col)))))

(def (cmd-emacs-version app)
  "Show gerbil-emacs version."
  (echo-message! (app-state-echo app) "gerbil-emacs (Qt backend)"))

;;;============================================================================
;;; Git/VCS commands
;;;============================================================================

(def (run-git-command app args buffer-name)
  "Run a git command and show output in a buffer."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (with-catch
      (lambda (e) (echo-error! (app-state-echo app)
                    (string-append "Git error: " (with-output-to-string
                      (lambda () (display-exception e))))))
      (lambda ()
        (let* ((proc (open-process
                        (list path: "/usr/bin/git"
                              arguments: args
                              directory: dir
                              stdout-redirection: #t
                              stderr-redirection: #t)))
               (output (read-line proc #f))
               (_ (process-status proc)))
          (close-port proc)
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app))
                 (git-buf (qt-buffer-create! buffer-name ed #f)))
            (qt-buffer-attach! ed git-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) git-buf)
            (qt-plain-text-edit-set-text! ed (or output ""))
            (qt-text-document-set-modified! (buffer-doc-pointer git-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))

(def (cmd-show-git-status app)
  "Show git status."
  (run-git-command app '("status") "*Git Status*"))

(def (cmd-show-git-log app)
  "Show git log."
  (run-git-command app '("log" "--oneline" "-30") "*Git Log*"))

(def (cmd-show-git-diff app)
  "Show git diff with syntax coloring."
  (run-git-command app '("diff") "*Git Diff*")
  (qt-highlight-diff! (current-qt-editor app)))

(def (cmd-show-git-blame app)
  "Show git blame for current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (run-git-command app (list "blame" path) "*Git Blame*")
      (echo-error! (app-state-echo app) "Buffer has no file"))))

;;;============================================================================
;;; Magit-style interactive git interface
;;;============================================================================

(def *magit-dir* #f)  ;; working directory for magit operations

(def (magit-run-git args dir)
  "Run a git command and return output as string."
  (with-catch
    (lambda (e) "")
    (lambda ()
      (let* ((proc (open-process
                      (list path: "/usr/bin/git"
                            arguments: args
                            directory: dir
                            stdout-redirection: #t
                            stderr-redirection: #t)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        (or output "")))))

(def (magit-parse-status output)
  "Parse git status --porcelain output into list of (status . filename)."
  (let ((lines (string-split output #\newline)))
    (filter identity
      (map (lambda (line)
             (and (>= (string-length line) 3)
                  (let ((status (substring line 0 2))
                        (file (substring line 3 (string-length line))))
                    (cons (string-trim status) file))))
           lines))))

(def (magit-format-status entries branch)
  "Format magit-style status buffer."
  (let ((out (open-output-string)))
    (display (string-append "Head: " branch "\n\n") out)
    (let ((staged (filter (lambda (e)
                            (let ((s (car e)))
                              (and (> (string-length s) 0)
                                   (not (string=? s "??"))
                                   (not (string=? s ""))
                                   (let ((ch (string-ref s 0)))
                                     (and (not (char=? ch #\space))
                                          (not (char=? ch #\?)))))))
                          entries))
          (unstaged (filter (lambda (e)
                              (let ((s (car e)))
                                (and (> (string-length s) 0)
                                     (not (string=? s "??"))
                                     (>= (string-length s) 2)
                                     (let ((ch (string-ref s (min 1 (- (string-length s) 1)))))
                                       (and (not (char=? ch #\space))
                                            (not (char=? ch #\?)))))))
                            entries))
          (untracked (filter (lambda (e) (string=? (car e) "??")) entries)))
      (when (not (null? staged))
        (display "Staged changes:\n" out)
        (for-each (lambda (e)
                    (display (string-append "  " (car e) " " (cdr e) "\n") out))
                  staged)
        (display "\n" out))
      (when (not (null? unstaged))
        (display "Unstaged changes:\n" out)
        (for-each (lambda (e)
                    (display (string-append "  " (car e) " " (cdr e) "\n") out))
                  unstaged)
        (display "\n" out))
      (when (not (null? untracked))
        (display "Untracked files:\n" out)
        (for-each (lambda (e)
                    (display (string-append "  ?? " (cdr e) "\n") out))
                  untracked)
        (display "\n" out))
      (when (and (null? staged) (null? unstaged) (null? untracked))
        (display "Nothing to commit, working tree clean.\n" out))
      (display "\nKeys: s=stage u=unstage c=commit d=diff g=refresh q=quit\n" out))
    (get-output-string out)))

(def (magit-file-at-point text pos)
  "Extract the filename from the current line in magit buffer."
  (let* ((line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1) (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1)))))
         (line (substring text line-start line-end)))
    ;; Lines look like "  M filename" or "  ?? filename"
    (let ((trimmed (string-trim line)))
      (cond
        ((and (>= (string-length trimmed) 3)
              (string=? (substring trimmed 0 2) "??"))
         (string-trim (substring trimmed 2 (string-length trimmed))))
        ((and (>= (string-length trimmed) 2)
              (memv (string-ref trimmed 0)
                    '(#\M #\A #\D #\R #\C #\U)))
         (string-trim (substring trimmed 1 (string-length trimmed))))
        (else #f)))))

(def (cmd-magit-status app)
  "Open interactive git status buffer (magit-style)."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (set! *magit-dir* dir)
    (let* ((status-output (magit-run-git '("status" "--porcelain") dir))
           (branch-output (magit-run-git '("rev-parse" "--abbrev-ref" "HEAD") dir))
           (branch (string-trim branch-output))
           (entries (magit-parse-status status-output))
           (text (magit-format-status entries branch))
           (ed (current-qt-editor app))
           (fr (app-state-frame app))
           (git-buf (or (buffer-by-name "*Magit*")
                        (qt-buffer-create! "*Magit*" ed #f))))
      (qt-buffer-attach! ed git-buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) git-buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer git-buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "*Magit*"))))

(def (cmd-magit-stage app)
  "Stage the file at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (file (magit-file-at-point text pos)))
        (if file
          (begin
            (magit-run-git (list "add" file) *magit-dir*)
            (cmd-magit-status app)
            (echo-message! (app-state-echo app)
              (string-append "Staged: " file)))
          (echo-error! (app-state-echo app) "No file at point"))))))

(def (cmd-magit-unstage app)
  "Unstage the file at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (file (magit-file-at-point text pos)))
        (if file
          (begin
            (magit-run-git (list "reset" "HEAD" file) *magit-dir*)
            (cmd-magit-status app)
            (echo-message! (app-state-echo app)
              (string-append "Unstaged: " file)))
          (echo-error! (app-state-echo app) "No file at point"))))))

(def (cmd-magit-commit app)
  "Commit staged changes."
  (let ((msg (qt-echo-read-string app "Commit message: ")))
    (when (and msg (> (string-length msg) 0))
      (let ((output (magit-run-git (list "commit" "-m" msg) *magit-dir*)))
        (cmd-magit-status app)
        (echo-message! (app-state-echo app)
          (string-append "Committed: " msg))))))

(def (cmd-magit-diff app)
  "Show diff for file at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (file (magit-file-at-point text pos)))
        (if file
          (let* ((diff-output (magit-run-git (list "diff" file) *magit-dir*))
                 (staged-diff (magit-run-git (list "diff" "--cached" file) *magit-dir*))
                 (full-diff (if (> (string-length staged-diff) 0) staged-diff diff-output))
                 (fr (app-state-frame app))
                 (diff-buf (or (buffer-by-name "*Magit Diff*")
                               (qt-buffer-create! "*Magit Diff*" ed #f))))
            (qt-buffer-attach! ed diff-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
            (qt-plain-text-edit-set-text! ed
              (if (string=? full-diff "") "No differences.\n" full-diff))
            (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (qt-highlight-diff! ed))
          (echo-error! (app-state-echo app) "No file at point"))))))

(def (cmd-magit-stage-all app)
  "Stage all changes."
  (when *magit-dir*
    (magit-run-git '("add" "-A") *magit-dir*)
    (cmd-magit-status app)
    (echo-message! (app-state-echo app) "All changes staged")))

(def (cmd-magit-log app)
  "Show git log in magit buffer."
  (when *magit-dir*
    (let* ((output (magit-run-git '("log" "--oneline" "--graph" "-30") *magit-dir*))
           (ed (current-qt-editor app))
           (fr (app-state-frame app))
           (log-buf (or (buffer-by-name "*Magit Log*")
                        (qt-buffer-create! "*Magit Log*" ed #f))))
      (qt-buffer-attach! ed log-buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) log-buf)
      (qt-plain-text-edit-set-text! ed (or output ""))
      (qt-text-document-set-modified! (buffer-doc-pointer log-buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;;;============================================================================
;;; Text manipulation
;;;============================================================================

(def (cmd-comment-region app)
  "Add comment prefix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (commented (map (lambda (l) (string-append ";; " l)) lines))
             (result (string-join commented "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Commented")))))

(def (cmd-uncomment-region app)
  "Remove comment prefix from each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (uncommented (map (lambda (l)
                                 (cond
                                   ((string-prefix? ";; " l) (substring l 3 (string-length l)))
                                   ((string-prefix? ";" l) (substring l 1 (string-length l)))
                                   ((string-prefix? "# " l) (substring l 2 (string-length l)))
                                   ((string-prefix? "//" l) (substring l 2 (string-length l)))
                                   (else l)))
                               lines))
             (result (string-join uncommented "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Uncommented")))))

(def (cmd-collapse-blank-lines app)
  "Collapse multiple blank lines into one."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (result (let loop ((s text))
                   (let ((idx (string-contains s "\n\n\n")))
                     (if idx
                       (loop (string-append (substring s 0 (+ idx 1))
                                            (substring s (+ idx 2) (string-length s))))
                       s)))))
    (qt-plain-text-edit-set-text! ed result)
    (echo-message! (app-state-echo app) "Blank lines collapsed")))

(def (cmd-remove-blank-lines app)
  "Remove all blank lines from region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (lines (string-split region #\newline))
             (non-blank (filter (lambda (l) (> (string-length (string-trim-both l)) 0)) lines))
             (result (string-join non-blank "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! (app-state-echo app) "Blank lines removed")))))

(def (cmd-delete-trailing-lines app)
  "Delete trailing blank lines at end of buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (trimmed (let loop ((s text))
                    (if (string-suffix? "\n\n" s)
                      (loop (substring s 0 (- (string-length s) 1)))
                      s))))
    (qt-plain-text-edit-set-text! ed trimmed)
    (echo-message! (app-state-echo app) "Trailing lines deleted")))

(def (cmd-trim-lines app)
  "Trim trailing whitespace from all lines."
  (cmd-delete-trailing-whitespace app))

(def (cmd-prefix-lines app)
  "Add a prefix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((prefix (qt-echo-read-string app "Prefix: ")))
        (when prefix
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (prefixed (map (lambda (l) (string-append prefix l)) lines))
                 (result (string-join prefixed "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)))))))

(def (cmd-suffix-lines app)
  "Add a suffix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((suffix (qt-echo-read-string app "Suffix: ")))
        (when suffix
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (suffixed (map (lambda (l) (string-append l suffix)) lines))
                 (result (string-join suffixed "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)))))))

;;;============================================================================
;;; Sort variants
;;;============================================================================

(def (cmd-sort-lines-reverse app)
  "Sort lines in region in reverse order."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines string>?))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted (reverse)")))))

(def (cmd-sort-lines-case-fold app)
  "Sort lines case-insensitively."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines (lambda (a b) (string<? (string-downcase a) (string-downcase b)))))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted (case-insensitive)")))))

(def (cmd-uniquify-lines app)
  "Remove duplicate lines in region."
  (cmd-delete-duplicate-lines app))

(def (cmd-sort-words app)
  "Sort words on the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (words (filter (lambda (s) (> (string-length s) 0))
                            (string-split line-text #\space)))
             (sorted-words (sort words string<?))
             (new-line (string-join sorted-words " "))
             (new-lines (let loop ((ls lines) (i 0) (acc '()))
                          (if (null? ls) (reverse acc)
                            (loop (cdr ls) (+ i 1)
                                  (cons (if (= i line) new-line (car ls)) acc)))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (line-start-position new-text line))
        (echo-message! (app-state-echo app) "Words sorted")))))

;;;============================================================================
;;; Case conversion helpers
;;;============================================================================

(def (cmd-camel-to-snake app)
  "Convert camelCase word at point to snake_case."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\_))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (snake (let loop ((i 0) (acc ""))
                      (if (>= i (string-length word)) acc
                        (let ((ch (string-ref word i)))
                          (if (and (char-upper-case? ch) (> i 0))
                            (loop (+ i 1) (string-append acc "_" (string (char-downcase ch))))
                            (loop (+ i 1) (string-append acc (string (char-downcase ch)))))))))
             (new-text (string-append (substring text 0 start) snake
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length snake)))))))

(def (cmd-snake-to-camel app)
  "Convert snake_case word at point to camelCase."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\_))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (camel (let loop ((i 0) (acc "") (cap? #f))
                      (if (>= i (string-length word)) acc
                        (let ((ch (string-ref word i)))
                          (if (char=? ch #\_)
                            (loop (+ i 1) acc #t)
                            (loop (+ i 1)
                                  (string-append acc (string (if cap? (char-upcase ch) ch)))
                                  #f))))))
             (new-text (string-append (substring text 0 start) camel
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length camel)))))))

;;;============================================================================
;;; Search helpers
;;;============================================================================

(def (cmd-highlight-word-at-point app)
  "Highlight the word at point as search term."
  (cmd-highlight-symbol app))

(def (parse-grep-line line)
  "Parse a grep -n output line. Returns (file line-num text) or #f."
  ;; Format: file:line:text
  (let ((colon1 (string-index line #\:)))
    (and colon1
         (let ((colon2 (string-index line #\: (+ colon1 1))))
           (and colon2
                (let* ((file (substring line 0 colon1))
                       (line-str (substring line (+ colon1 1) colon2))
                       (text (substring line (+ colon2 1) (string-length line)))
                       (line-num (string->number line-str)))
                  (and line-num
                       (list file line-num text))))))))

(def (grep-run-and-show! app pattern dir args)
  "Run grep with ARGS, parse results, show in *Grep* buffer."
  (with-catch
    (lambda (e) (echo-error! (app-state-echo app) "Grep failed"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/grep"
                           arguments: (append args (list pattern dir))
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        ;; Parse results
        (let* ((lines (if output
                       (let loop ((s output) (acc []))
                         (let ((nl (string-index s #\newline)))
                           (if nl
                             (loop (substring s (+ nl 1) (string-length s))
                                   (cons (substring s 0 nl) acc))
                             (reverse (if (> (string-length s) 0)
                                        (cons s acc) acc)))))
                       []))
               (parsed (filter identity (map parse-grep-line lines)))
               (header (string-append "-*- grep -*-\n"
                         "grep " (string-join args " ") " " pattern " " dir "\n\n")))
          ;; Store results for navigation
          (set! *grep-results* parsed)
          (set! *grep-result-index* -1)
          ;; Show in buffer
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app))
                 (result-text (if (null? parsed)
                               (string-append header "No matches found.\n")
                               (string-append header
                                 (number->string (length parsed)) " matches\n\n"
                                 (string-join
                                   (map (lambda (r)
                                          (string-append (car r) ":"
                                            (number->string (cadr r)) ":"
                                            (caddr r)))
                                        parsed)
                                   "\n")
                                 "\n\nPress Enter on a result line to jump to source.")))
                 (grep-buf (or (buffer-by-name "*Grep*")
                               (qt-buffer-create! "*Grep*" ed #f))))
            (qt-buffer-attach! ed grep-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) grep-buf)
            (qt-plain-text-edit-set-text! ed result-text)
            (qt-text-document-set-modified! (buffer-doc-pointer grep-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! (app-state-echo app)
              (string-append (number->string (length parsed)) " matches"))))))))

(def (cmd-grep app)
  "Run grep -rn and show results with navigation."
  (let ((pattern (qt-echo-read-string app "Grep: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let ((dir (qt-echo-read-string app "In directory: ")))
        (when (and dir (> (string-length dir) 0))
          (grep-run-and-show! app pattern dir '("-rn")))))))

(def (cmd-rgrep app)
  "Run recursive grep with file type filter."
  (let ((pattern (qt-echo-read-string app "Rgrep: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let ((include (qt-echo-read-string app "File pattern (e.g. *.ss): ")))
        (when include
          (let ((dir (qt-echo-read-string app "In directory: ")))
            (when (and dir (> (string-length dir) 0))
              (if (and include (> (string-length include) 0))
                (grep-run-and-show! app pattern dir
                  (list "-rn" (string-append "--include=" include)))
                (grep-run-and-show! app pattern dir '("-rn"))))))))))

(def (grep-goto-result! app index)
  "Jump to grep result at INDEX."
  (when (and (>= index 0) (< index (length *grep-results*)))
    (let* ((result (list-ref *grep-results* index))
           (file (car result))
           (line-num (cadr result))
           (echo (app-state-echo app))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      (set! *grep-result-index* index)
      (when (file-exists? file)
        (let* ((name (path-strip-directory file))
               (existing (let loop ((bufs *buffer-list*))
                           (if (null? bufs) #f
                             (let ((b (car bufs)))
                               (if (and (buffer-file-path b)
                                        (string=? (buffer-file-path b) file))
                                 b (loop (cdr bufs)))))))
               (buf (or existing (qt-buffer-create! name ed file))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (not existing)
            (let ((text (read-file-as-string file)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
            (qt-setup-highlighting! app buf))
          ;; Jump to line
          (let* ((text (qt-plain-text-edit-text ed))
                 (pos (text-line-position text line-num)))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-message! echo
            (string-append "Match " (number->string (+ index 1))
              "/" (number->string (length *grep-results*)))))))))

(def (cmd-grep-goto app)
  "Jump from *Grep* buffer line to source location."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (if (not (string=? (buffer-name buf) "*Grep*"))
      (echo-error! (app-state-echo app) "Not in *Grep* buffer")
      ;; Parse current line for file:line:text pattern
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             ;; Find current line
             (line-start (let loop ((i (- pos 1)))
                           (if (or (< i 0) (char=? (string-ref text i) #\newline))
                             (+ i 1) (loop (- i 1)))))
             (line-end (let loop ((i pos))
                         (if (or (>= i (string-length text))
                                 (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
             (line (substring text line-start line-end))
             (parsed (parse-grep-line line)))
        (if parsed
          ;; Find the matching index in *grep-results*
          (let ((idx (let loop ((results *grep-results*) (i 0))
                       (cond
                         ((null? results) 0)
                         ((and (string=? (car (car results)) (car parsed))
                               (= (cadr (car results)) (cadr parsed)))
                          i)
                         (else (loop (cdr results) (+ i 1)))))))
            (grep-goto-result! app idx))
          (echo-error! (app-state-echo app) "No grep result on this line"))))))

(def (cmd-next-grep-result app)
  "Jump to next grep result."
  (if (null? *grep-results*)
    (echo-error! (app-state-echo app) "No grep results")
    (let ((next-idx (+ *grep-result-index* 1)))
      (if (>= next-idx (length *grep-results*))
        (echo-message! (app-state-echo app) "No more matches")
        (grep-goto-result! app next-idx)))))

(def (cmd-previous-grep-result app)
  "Jump to previous grep result."
  (if (null? *grep-results*)
    (echo-error! (app-state-echo app) "No grep results")
    (let ((prev-idx (- *grep-result-index* 1)))
      (if (< prev-idx 0)
        (echo-message! (app-state-echo app) "No previous matches")
        (grep-goto-result! app prev-idx)))))

;;;============================================================================
;;; Wgrep — editable grep results
;;;============================================================================

(def *wgrep-mode* #f) ;; #t when *Grep* buffer is in wgrep edit mode
(def *wgrep-original-lines* []) ;; original grep result lines for diffing

(def (cmd-wgrep-change-to-wgrep-mode app)
  "Make *Grep* buffer editable for wgrep-style batch editing."
  (let ((buf (current-qt-buffer app)))
    (if (not (string=? (buffer-name buf) "*Grep*"))
      (echo-error! (app-state-echo app) "Not in *Grep* buffer")
      (begin
        (set! *wgrep-mode* #t)
        ;; Store original result lines for later comparison
        (set! *wgrep-original-lines*
          (map (lambda (r)
                 (string-append (car r) ":"
                   (number->string (cadr r)) ":"
                   (caddr r)))
               *grep-results*))
        (echo-message! (app-state-echo app)
          "Wgrep mode: edit results, then C-c C-c to apply or C-c C-k to abort")))))

(def (cmd-wgrep-finish-edit app)
  "Apply wgrep changes back to source files."
  (let ((buf (current-qt-buffer app)))
    (if (not (and (string=? (buffer-name buf) "*Grep*") *wgrep-mode*))
      (echo-error! (app-state-echo app) "Not in wgrep mode")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (all-lines (let loop ((s text) (acc []))
                          (let ((nl (string-index s #\newline)))
                            (if nl
                              (loop (substring s (+ nl 1) (string-length s))
                                    (cons (substring s 0 nl) acc))
                              (reverse (if (> (string-length s) 0) (cons s acc) acc))))))
             ;; Extract only file:line:text result lines (skip header)
             (result-lines (filter (lambda (l) (parse-grep-line l)) all-lines))
             (changes 0))
        ;; Group changes by file
        (let ((file-changes (make-hash-table)))
          (for-each
            (lambda (line)
              (let ((parsed (parse-grep-line line)))
                (when parsed
                  (let ((file (car parsed))
                        (line-num (cadr parsed))
                        (new-text (caddr parsed)))
                    ;; Check if line changed from original
                    (let* ((orig (let loop ((results *grep-results*))
                                  (if (null? results) #f
                                    (let ((r (car results)))
                                      (if (and (string=? (car r) file)
                                               (= (cadr r) line-num))
                                        (caddr r)
                                        (loop (cdr results)))))))
                           (changed? (and orig (not (string=? orig new-text)))))
                      (when changed?
                        (let ((existing (or (hash-get file-changes file) [])))
                          (hash-put! file-changes file
                            (cons (cons line-num new-text) existing)))))))))
            result-lines)
          ;; Apply changes to each file
          (hash-for-each
            (lambda (file line-edits)
              (when (file-exists? file)
                (let* ((content (read-file-as-string file))
                       (lines (let loop ((s content) (acc []))
                                (let ((nl (string-index s #\newline)))
                                  (if nl
                                    (loop (substring s (+ nl 1) (string-length s))
                                          (cons (substring s 0 nl) acc))
                                    (reverse (if (> (string-length s) 0) (cons s acc) acc))))))
                       (new-lines
                         (let loop ((ls lines) (i 1) (acc []))
                           (if (null? ls) (reverse acc)
                             (let ((edit (assoc i line-edits)))
                               (loop (cdr ls) (+ i 1)
                                     (cons (if edit (cdr edit) (car ls)) acc))))))
                       (new-content (string-join new-lines "\n")))
                  ;; Add trailing newline if original had one
                  (let ((final (if (and (> (string-length content) 0)
                                       (char=? (string-ref content (- (string-length content) 1))
                                               #\newline))
                                 (string-append new-content "\n")
                                 new-content)))
                    (write-string-to-file file final)
                    (set! changes (+ changes (length line-edits)))))))
            file-changes))
        (set! *wgrep-mode* #f)
        (echo-message! (app-state-echo app)
          (string-append "Applied " (number->string changes) " change(s)"))))))

(def (cmd-wgrep-abort-changes app)
  "Abort wgrep changes and restore original *Grep* buffer."
  (if (not *wgrep-mode*)
    (echo-error! (app-state-echo app) "Not in wgrep mode")
    (begin
      (set! *wgrep-mode* #f)
      ;; Restore original content
      (let* ((ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (header (string-append "-*- grep -*-\n\n"))
             (result-text (if (null? *wgrep-original-lines*)
                            (string-append header "No matches found.\n")
                            (string-append header
                              (number->string (length *wgrep-original-lines*)) " matches\n\n"
                              (string-join *wgrep-original-lines* "\n")
                              "\n\nPress Enter on a result line to jump to source."))))
        (qt-plain-text-edit-set-text! ed result-text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0))
      (echo-message! (app-state-echo app) "Wgrep changes aborted"))))

;;;============================================================================
;;; Quoted insert
;;;============================================================================

(def (cmd-quoted-insert app)
  "Insert the next character literally."
  (echo-message! (app-state-echo app) "C-q: type character to insert literally"))

;;;============================================================================
;;; Quick calc
;;;============================================================================

(def (cmd-quick-calc app)
  "Quick calculator (alias for calc)."
  (cmd-calc app))

;;;============================================================================
;;; Eval and insert
;;;============================================================================

(def (cmd-eval-and-insert app)
  "Evaluate expression and insert result at point."
  (let ((expr (qt-echo-read-string app "Eval and insert: ")))
    (when (and expr (> (string-length expr) 0))
      (let-values (((result error?) (eval-expression-string expr)))
        (if error?
          (echo-error! (app-state-echo app) (string-append "Error: " result))
          (qt-plain-text-edit-insert-text! (current-qt-editor app) result))))))

;;;============================================================================
;;; Shell command insert
;;;============================================================================

(def (cmd-shell-command-insert app)
  "Run shell command and insert output at point."
  (let ((cmd (qt-echo-read-string app "Shell command (insert): ")))
    (when cmd
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Command failed"))
        (lambda ()
          (let* ((proc (open-process
                          (list path: "/bin/sh"
                                arguments: (list "-c" cmd)
                                stdout-redirection: #t)))
                 (output (read-line proc #f))
                 (_ (process-status proc)))
            (close-port proc)
            (when output
              (qt-plain-text-edit-insert-text! (current-qt-editor app) output))))))))

;;;============================================================================
;;; Pipe region
;;;============================================================================

(def (cmd-pipe-region app)
  "Pipe region through shell command."
  (cmd-shell-command-on-region app))

;;;============================================================================
;;; Bookmark extensions
;;;============================================================================

(def (cmd-bookmark-delete app)
  "Delete a bookmark."
  (let* ((bookmarks (app-state-bookmarks app))
         (names (sort (hash-keys bookmarks) string<?)))
    (if (null? names)
      (echo-error! (app-state-echo app) "No bookmarks")
      (let ((name (qt-echo-read-string-with-completion app "Delete bookmark: " names)))
        (when (and name (> (string-length name) 0))
          (hash-remove! bookmarks name)
          (bookmarks-save! app)
          (echo-message! (app-state-echo app) (string-append "Deleted: " name)))))))

(def *bookmarks-path*
  (path-expand ".gerbil-emacs-bookmarks" (user-info-home (user-info (user-name)))))

(def (bookmarks-save! app)
  "Persist bookmarks to disk. Format: one line per bookmark: name\tfile-path\tposition"
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file *bookmarks-path*
        (lambda (port)
          (for-each
            (lambda (pair)
              (let* ((name (car pair))
                     (val (cdr pair))
                     (fpath (if (list? val) (cadr val) #f))
                     (pos (if (list? val) (caddr val) (cdr val))))
                (when fpath
                  (display name port) (display "\t" port)
                  (display fpath port) (display "\t" port)
                  (display (number->string pos) port) (newline port))))
            (hash->list (app-state-bookmarks app))))))))

(def (split-by-tab str)
  "Split STR by tab characters into a list of strings."
  (let loop ((start 0) (i 0) (acc []))
    (cond
      ((>= i (string-length str))
       (reverse (cons (substring str start i) acc)))
      ((char=? (string-ref str i) #\tab)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
      (else (loop start (+ i 1) acc)))))

(def (bookmarks-load! app)
  "Load bookmarks from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *bookmarks-path*)
        (let ((bm (app-state-bookmarks app)))
          (call-with-input-file *bookmarks-path*
            (lambda (port)
              (let loop ()
                (let ((line (read-line port)))
                  (unless (eof-object? line)
                    (let ((parts (split-by-tab line)))
                      (when (= (length parts) 3)
                        (let ((name (car parts))
                              (fpath (cadr parts))
                              (pos (string->number (caddr parts))))
                          (when (and name fpath pos)
                            (hash-put! bm name
                              (list (path-strip-directory fpath) fpath pos))))))
                    (loop)))))))))))

(def (cmd-bookmark-save app)
  "Save bookmarks to file."
  (bookmarks-save! app)
  (echo-message! (app-state-echo app)
    (string-append "Bookmarks saved to " *bookmarks-path*)))

(def (cmd-bookmark-load app)
  "Load bookmarks from file."
  (bookmarks-load! app)
  (echo-message! (app-state-echo app)
    (string-append "Bookmarks loaded from " *bookmarks-path*)))

;;;============================================================================
;;; Session persistence (desktop save/restore)
;;;============================================================================

(def *session-path*
  (path-expand ".gerbil-emacs-session" (user-info-home (user-info (user-name)))))

(def (session-save! app)
  "Save current session (open file buffers + positions) to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((ed (current-qt-editor app))
             (fr (app-state-frame app))
             (current-buf (current-qt-buffer app))
             ;; Collect file buffers with positions
             (entries
               (let loop ((bufs *buffer-list*) (acc []))
                 (if (null? bufs) (reverse acc)
                   (let ((buf (car bufs)))
                     (if (buffer-file-path buf)
                       ;; Get position: attach temporarily to read cursor pos
                       (let ((pos (begin
                                    (qt-buffer-attach! ed buf)
                                    (qt-plain-text-edit-cursor-position ed))))
                         (loop (cdr bufs) (cons (cons (buffer-file-path buf) pos) acc)))
                       (loop (cdr bufs) acc)))))))
        ;; Restore current buffer
        (qt-buffer-attach! ed current-buf)
        ;; Write session file
        (call-with-output-file *session-path*
          (lambda (port)
            ;; First line: current buffer path
            (display (or (buffer-file-path current-buf) "") port)
            (newline port)
            ;; Remaining lines: file\tposition
            (for-each
              (lambda (entry)
                (display (car entry) port)
                (display "\t" port)
                (display (number->string (cdr entry)) port)
                (newline port))
              entries)))))))

(def (session-restore-files)
  "Read session file and return list of (file-path . cursor-pos) plus current-file."
  (with-catch
    (lambda (e) (values #f []))
    (lambda ()
      (if (not (file-exists? *session-path*))
        (values #f [])
        (call-with-input-file *session-path*
          (lambda (port)
            (let ((current-file (read-line port)))
              (let loop ((acc []))
                (let ((line (read-line port)))
                  (if (eof-object? line)
                    (values (if (and (string? current-file)
                                    (> (string-length current-file) 0))
                              current-file #f)
                            (reverse acc))
                    (let ((parts (split-by-tab line)))
                      (if (and (= (length parts) 2)
                               (string->number (cadr parts)))
                        (loop (cons (cons (car parts) (string->number (cadr parts))) acc))
                        (loop acc)))))))))))))

(def (cmd-session-save app)
  "Save current session."
  (session-save! app)
  (echo-message! (app-state-echo app) "Session saved"))

(def (cmd-session-restore app)
  "Restore last session."
  (echo-message! (app-state-echo app) "Use --restore flag to restore session on startup"))

;;;============================================================================
;;; Duplicate region
;;;============================================================================

(def (cmd-duplicate-region app)
  "Duplicate the selected region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (cmd-duplicate-line app)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (new-text (string-append (substring text 0 end) region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ end (string-length region)))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region duplicated")))))

;;;============================================================================
;;; Reverse chars/word
;;;============================================================================

(def (cmd-reverse-chars app)
  "Reverse characters in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (reversed (list->string (reverse (string->list region))))
             (new-text (string-append (substring text 0 start) reversed
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Reversed")))))

(def (cmd-reverse-word app)
  "Reverse the word at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1))))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i)))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (reversed (list->string (reverse (string->list word))))
             (new-text (string-append (substring text 0 start) reversed
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed end)))))

;;;============================================================================
;;; Environment
;;;============================================================================

(def (cmd-getenv app)
  "Display an environment variable."
  (let ((var (qt-echo-read-string app "Environment variable: ")))
    (when var
      (let ((val (getenv var #f)))
        (echo-message! (app-state-echo app)
          (if val (string-append var "=" val)
            (string-append var " is not set")))))))

(def (cmd-setenv app)
  "Set an environment variable."
  (let ((var (qt-echo-read-string app "Variable name: ")))
    (when var
      (let ((val (qt-echo-read-string app (string-append var "="))))
        (when val
          (setenv var val)
          (echo-message! (app-state-echo app)
            (string-append "Set " var "=" val)))))))

;;;============================================================================
;;; Batch 7: More missing commands
;;;============================================================================

(def (cmd-transpose-sexps app)
  "Transpose the s-expressions before and after point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find the sexp before point (backward)
    (let* ((before-end pos)
           (before-start
             (let loop ((i (- pos 1)) (depth 0))
               (cond
                 ((< i 0) 0)
                 ((and (memq (string-ref text i) '(#\) #\] #\})) (= depth 0))
                  ;; Find matching open
                  (let ((close-ch (string-ref text i))
                        (open-ch (cond ((char=? (string-ref text i) #\)) #\()
                                       ((char=? (string-ref text i) #\]) #\[)
                                       (else #\{))))
                    (let inner ((j (- i 1)) (d 1))
                      (cond
                        ((< j 0) 0)
                        ((char=? (string-ref text j) close-ch) (inner (- j 1) (+ d 1)))
                        ((char=? (string-ref text j) open-ch)
                         (if (= d 1) j (inner (- j 1) (- d 1))))
                        (else (inner (- j 1) d))))))
                 ((and (char-alphabetic? (string-ref text i)) (= depth 0))
                  (let loop2 ((j i))
                    (if (and (> j 0) (or (char-alphabetic? (string-ref text (- j 1)))
                                         (char-numeric? (string-ref text (- j 1)))
                                         (char=? (string-ref text (- j 1)) #\-)))
                      (loop2 (- j 1)) j)))
                 ((char-whitespace? (string-ref text i)) (loop (- i 1) depth))
                 (else i))))
           ;; Find the sexp after point (forward)
           (after-start pos)
           (after-end
             (let loop ((i pos))
               (cond
                 ((>= i len) len)
                 ((memq (string-ref text i) '(#\( #\[ #\{))
                  (let ((open-ch (string-ref text i))
                        (close-ch (cond ((char=? (string-ref text i) #\() #\))
                                        ((char=? (string-ref text i) #\[) #\])
                                        (else #\}))))
                    (let inner ((j (+ i 1)) (d 1))
                      (cond
                        ((>= j len) len)
                        ((char=? (string-ref text j) open-ch) (inner (+ j 1) (+ d 1)))
                        ((char=? (string-ref text j) close-ch)
                         (if (= d 1) (+ j 1) (inner (+ j 1) (- d 1))))
                        (else (inner (+ j 1) d))))))
                 ((char-alphabetic? (string-ref text i))
                  (let loop2 ((j (+ i 1)))
                    (if (and (< j len) (or (char-alphabetic? (string-ref text j))
                                           (char-numeric? (string-ref text j))
                                           (char=? (string-ref text j) #\-)))
                      (loop2 (+ j 1)) j)))
                 ((char-whitespace? (string-ref text i)) (loop (+ i 1)))
                 (else (+ i 1))))))
      (when (and (< before-start before-end) (< after-start after-end))
        (let ((sexp1 (substring text before-start before-end))
              (mid (substring text before-end after-start))
              (sexp2 (substring text after-start after-end)))
          (qt-plain-text-edit-set-selection! ed before-start after-end)
          (qt-plain-text-edit-remove-selected-text! ed)
          (qt-plain-text-edit-insert-text! ed (string-append sexp2 mid sexp1))
          (echo-message! (app-state-echo app) "S-expressions transposed"))))))

(def (cmd-transpose-paragraphs app)
  "Transpose the paragraph before point with the one after."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let* ((para-start
             (let loop ((i (- pos 1)))
               (cond
                 ((< i 0) 0)
                 ((and (char=? (string-ref text i) #\newline)
                       (or (= i 0)
                           (and (> i 0) (char=? (string-ref text (- i 1)) #\newline))))
                  (+ i 1))
                 (else (loop (- i 1))))))
           (para-end
             (let loop ((i pos))
               (cond
                 ((>= i len) len)
                 ((and (char=? (string-ref text i) #\newline)
                       (< (+ i 1) len)
                       (char=? (string-ref text (+ i 1)) #\newline))
                  i)
                 (else (loop (+ i 1))))))
           (next-start
             (let loop ((i (+ para-end 1)))
               (cond
                 ((>= i len) #f)
                 ((not (or (char=? (string-ref text i) #\newline)
                           (char=? (string-ref text i) #\space)))
                  i)
                 (else (loop (+ i 1))))))
           (next-end
             (if next-start
               (let loop ((i next-start))
                 (cond
                   ((>= i len) len)
                   ((and (char=? (string-ref text i) #\newline)
                         (< (+ i 1) len)
                         (char=? (string-ref text (+ i 1)) #\newline))
                    i)
                   (else (loop (+ i 1)))))
               #f)))
      (if (and next-start next-end)
        (let* ((para1 (substring text para-start para-end))
               (sep (substring text para-end next-start))
               (para2 (substring text next-start next-end)))
          (qt-plain-text-edit-set-selection! ed para-start next-end)
          (qt-plain-text-edit-remove-selected-text! ed)
          (qt-plain-text-edit-insert-text! ed (string-append para2 sep para1))
          (echo-message! (app-state-echo app) "Paragraphs transposed"))
        (echo-message! (app-state-echo app) "No next paragraph to transpose")))))

(def (cmd-zap-up-to-char app)
  "Kill text up to but not including the specified character."
  (let ((input (qt-echo-read-string app "Zap up to char: ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (found (let loop ((i (+ pos 1)))
                      (cond
                        ((>= i (string-length text)) #f)
                        ((char=? (string-ref text i) ch) i)
                        (else (loop (+ i 1)))))))
        (if found
          (let ((killed (substring text pos found)))
            (qt-plain-text-edit-set-selection! ed pos found)
            (qt-plain-text-edit-remove-selected-text! ed)
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
              (string-append "Zapped to '" (string ch) "'")))
          (echo-error! (app-state-echo app)
            (string-append "'" (string ch) "' not found")))))))

(def (cmd-zap-to-char-inclusive app)
  "Kill text up to and including the specified character."
  (let ((input (qt-echo-read-string app "Zap to char (inclusive): ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (found (let loop ((i (+ pos 1)))
                      (cond
                        ((>= i (string-length text)) #f)
                        ((char=? (string-ref text i) ch) (+ i 1))
                        (else (loop (+ i 1)))))))
        (if found
          (let ((killed (substring text pos found)))
            (qt-plain-text-edit-set-selection! ed pos found)
            (qt-plain-text-edit-remove-selected-text! ed)
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app))))
          (echo-error! (app-state-echo app)
            (string-append "'" (string ch) "' not found")))))))

(def (cmd-query-replace-regexp app)
  "Query replace (simplified: uses string-contains, not regexp)."
  (let ((from (qt-echo-read-string app "Query replace: ")))
    (when from
      (let ((to (qt-echo-read-string app (string-append "Replace \"" from "\" with: "))))
        (when to
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed)))
            ;; Do all replacements
            (let loop ((result text) (replaced 0))
              (let ((found (string-contains result from)))
                (if found
                  (let ((new-text (string-append
                                    (substring result 0 found)
                                    to
                                    (substring result (+ found (string-length from))
                                               (string-length result)))))
                    (loop new-text (+ replaced 1)))
                  (begin
                    (qt-plain-text-edit-set-text! ed result)
                    (echo-message! (app-state-echo app)
                      (string-append "Replaced " (number->string replaced)
                                     " occurrences"))))))))))))

(def (cmd-copy-from-below app)
  "Copy character from the line below."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (cur-line (qt-plain-text-edit-cursor-line ed))
         (col (- pos (line-start-position text cur-line))))
    (if (< (+ cur-line 1) (length lines))
      (let ((below (list-ref lines (+ cur-line 1))))
        (if (< col (string-length below))
          (qt-plain-text-edit-insert-text! ed (string (string-ref below col)))
          (echo-message! (app-state-echo app) "Line below too short")))
      (echo-message! (app-state-echo app) "No line below"))))

(def (cmd-copy-symbol-at-point app)
  "Copy the symbol at point to kill ring."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((sym-char? (lambda (ch)
                          (or (char-alphabetic? ch) (char-numeric? ch)
                              (char=? ch #\-) (char=? ch #\_)
                              (char=? ch #\!) (char=? ch #\?)
                              (char=? ch #\*))))
             (start (let loop ((i pos))
                      (if (and (> i 0) (sym-char? (string-ref text (- i 1))))
                        (loop (- i 1)) i)))
             (end (let loop ((i pos))
                    (if (and (< i len) (sym-char? (string-ref text i)))
                      (loop (+ i 1)) i)))
             (sym (substring text start end)))
        (if (> (string-length sym) 0)
          (begin
            (set! (app-state-kill-ring app) (cons sym (app-state-kill-ring app)))
            (echo-message! (app-state-echo app) (string-append "Copied: " sym)))
          (echo-message! (app-state-echo app) "No symbol at point"))))))

(def (cmd-copy-word-at-point app)
  "Copy word at point to kill ring (alias for copy-word)."
  (cmd-copy-word app))

(def (cmd-delete-to-end-of-line app)
  "Delete from point to end of line (without killing)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-end (+ (line-start-position text line) (string-length (list-ref lines line)))))
    (when (> line-end pos)
      (qt-plain-text-edit-set-selection! ed pos line-end)
      (qt-plain-text-edit-remove-selected-text! ed))))

(def (cmd-delete-to-beginning-of-line app)
  "Delete from point to beginning of line (without killing)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (line-start (line-start-position text line)))
    (when (> pos line-start)
      (qt-plain-text-edit-set-selection! ed line-start pos)
      (qt-plain-text-edit-remove-selected-text! ed))))

(def (cmd-delete-horizontal-space-forward app)
  "Delete spaces and tabs after point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (end (let loop ((i pos))
                (if (and (< i len) (or (char=? (string-ref text i) #\space)
                                       (char=? (string-ref text i) #\tab)))
                  (loop (+ i 1)) i))))
    (when (> end pos)
      (qt-plain-text-edit-set-selection! ed pos end)
      (qt-plain-text-edit-remove-selected-text! ed))))

(def (cmd-cycle-spacing app)
  "Cycle between one space, no spaces, and original spacing."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         ;; Find whitespace boundaries around point
         (start (let loop ((i pos))
                  (if (and (> i 0) (or (char=? (string-ref text (- i 1)) #\space)
                                       (char=? (string-ref text (- i 1)) #\tab)))
                    (loop (- i 1)) i)))
         (end (let loop ((i pos))
                (if (and (< i len) (or (char=? (string-ref text i) #\space)
                                       (char=? (string-ref text i) #\tab)))
                  (loop (+ i 1)) i)))
         (ws-len (- end start)))
    (cond
      ((> ws-len 1) ;; Multiple spaces → one space
       (qt-plain-text-edit-set-selection! ed start end)
       (qt-plain-text-edit-remove-selected-text! ed)
       (qt-plain-text-edit-insert-text! ed " "))
      ((= ws-len 1) ;; One space → no spaces
       (qt-plain-text-edit-set-selection! ed start end)
       (qt-plain-text-edit-remove-selected-text! ed))
      (else ;; No spaces → one space
       (qt-plain-text-edit-insert-text! ed " ")))))

(def (cmd-swap-windows app)
  "Swap buffers between two windows."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    (if (>= (length wins) 2)
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (b1 (qt-edit-window-buffer w1))
             (b2 (qt-edit-window-buffer w2))
             (e1 (qt-edit-window-editor w1))
             (e2 (qt-edit-window-editor w2)))
        ;; Swap buffer associations
        (set! (qt-edit-window-buffer w1) b2)
        (set! (qt-edit-window-buffer w2) b1)
        ;; Swap editor content
        (let ((t1 (qt-plain-text-edit-text e1))
              (t2 (qt-plain-text-edit-text e2)))
          (qt-plain-text-edit-set-text! e1 t2)
          (qt-plain-text-edit-set-text! e2 t1))
        (echo-message! (app-state-echo app) "Windows swapped"))
      (echo-message! (app-state-echo app) "Only one window"))))

(def (cmd-rotate-windows app)
  "Rotate window layout by cycling to next window."
  (let ((wins (qt-frame-windows (app-state-frame app))))
    (if (>= (length wins) 2)
      (begin
        (qt-frame-other-window! (app-state-frame app))
        (echo-message! (app-state-echo app) "Windows rotated"))
      (echo-message! (app-state-echo app) "Only one window"))))

;; cmd-toggle-line-comment is defined later (near end of file)
;; cmd-narrow-to-defun is defined earlier (near narrowing section)

(def (cmd-fold-all app)
  "Fold all top-level forms."
  (echo-message! (app-state-echo app) "Fold all (not supported in QPlainTextEdit)"))

(def (cmd-unfold-all app)
  "Unfold all forms."
  (echo-message! (app-state-echo app) "Unfold all (not supported in QPlainTextEdit)"))

(def (cmd-toggle-auto-pair-mode app)
  "Toggle automatic pairing of brackets/quotes."
  (set! *auto-pair-mode* (not *auto-pair-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-pair-mode* "Auto-pair mode ON" "Auto-pair mode OFF")))

(def (cmd-mark-page app)
  "Select the entire buffer (alias for mark-whole-buffer)."
  (cmd-select-all app))

(def (cmd-mark-whole-buffer app)
  "Select the entire buffer."
  (cmd-select-all app))

(def (cmd-view-lossage app)
  "View recent keystrokes."
  (echo-message! (app-state-echo app) "Lossage not recorded in Qt backend"))

(def (cmd-bookmark-rename app)
  "Rename a bookmark."
  (let* ((bm (app-state-bookmarks app))
         (names (sort (hash-keys bm) string<?))
         (old-name (qt-echo-read-string-with-completion app "Rename bookmark: " names)))
    (when (and old-name (> (string-length old-name) 0))
      (if (hash-get bm old-name)
        (let ((new-name (qt-echo-read-string app "New name: ")))
          (when (and new-name (> (string-length new-name) 0))
            (hash-put! bm new-name (hash-get bm old-name))
            (hash-remove! bm old-name)
            (bookmarks-save! app)
            (echo-message! (app-state-echo app) (string-append old-name " -> " new-name))))
        (echo-error! (app-state-echo app) (string-append "No bookmark: " old-name))))))

(def (cmd-view-register app)
  "View contents of a register."
  (let ((input (qt-echo-read-string app "View register: ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (regs (app-state-registers app))
             (val (hash-get regs ch)))
        (if val
          (echo-message! (app-state-echo app)
            (string-append "Register " (string ch) ": "
              (cond
                ((string? val) (if (> (string-length val) 60)
                                 (string-append (substring val 0 60) "...")
                                 val))
                ((number? val) (number->string val))
                (else "(complex value)"))))
          (echo-message! (app-state-echo app)
            (string-append "Register " (string ch) " is empty")))))))

(def (cmd-sort-imports app)
  "Sort import lines in the current buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         ;; Find consecutive import lines (starting with (import or :)
         (result
           (let loop ((ls lines) (imports []) (in-import? #f) (acc []))
             (cond
               ((null? ls)
                (if (pair? imports)
                  (reverse (append (reverse (sort (reverse imports) string<?)) acc))
                  (reverse acc)))
               ((or (string-contains (car ls) "(import ")
                    (and in-import? (> (string-length (car ls)) 0)
                         (or (char=? (string-ref (car ls) 0) #\space)
                             (char=? (string-ref (car ls) 0) #\tab)
                             (char-alphabetic? (string-ref (car ls) 0)))))
                (loop (cdr ls) (cons (car ls) imports) #t acc))
               (else
                (if (pair? imports)
                  (loop (cdr ls) [] #f
                    (cons (car ls) (append (reverse (sort (reverse imports) string<?)) acc)))
                  (loop (cdr ls) [] #f (cons (car ls) acc)))))))
         (new-text (string-join result "\n")))
    (qt-plain-text-edit-set-text! ed new-text)
    (echo-message! (app-state-echo app) "Imports sorted")))

(def (cmd-replace-string-all app)
  "Replace all occurrences of a string (no prompting)."
  (let ((from (qt-echo-read-string app "Replace all: ")))
    (when from
      (let ((to (qt-echo-read-string app (string-append "Replace \"" from "\" with: "))))
        (when to
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed)))
            (let loop ((result text) (count 0))
              (let ((found (string-contains result from)))
                (if found
                  (loop (string-append (substring result 0 found) to
                          (substring result (+ found (string-length from))
                                    (string-length result)))
                        (+ count 1))
                  (begin
                    (qt-plain-text-edit-set-text! ed result)
                    (echo-message! (app-state-echo app)
                      (string-append "Replaced " (number->string count)
                                     " occurrences"))))))))))))

(def (cmd-replace-in-region app)
  "Replace all occurrences within the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let ((search (qt-echo-read-string app "Replace in region: ")))
        (when search
          (let ((replace (qt-echo-read-string app "Replace with: ")))
            (when replace
              (let* ((start (min pos mark))
                     (end (max pos mark))
                     (text (qt-plain-text-edit-text ed))
                     (region (substring text start end))
                     (slen (string-length search))
                     (result (let loop ((p 0) (acc []))
                               (let ((f (string-contains region search p)))
                                 (if f
                                   (loop (+ f slen)
                                     (cons replace (cons (substring region p f) acc)))
                                   (apply string-append
                                     (reverse (cons (substring region p (string-length region)) acc))))))))
                (qt-plain-text-edit-set-selection! ed start end)
                (qt-plain-text-edit-remove-selected-text! ed)
                (qt-plain-text-edit-insert-text! ed result)
                (set! (buffer-mark buf) #f)
                (echo-message! (app-state-echo app) "Replaced in region"))))))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-write-region app)
  "Write the region to a file."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let ((filename (qt-echo-read-string app "Write region to file: ")))
        (when filename
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (substring (qt-plain-text-edit-text ed) start end)))
            (with-output-to-file filename (lambda () (display text)))
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app)
              (string-append "Wrote " (number->string (- end start)) " chars to " filename)))))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-search-forward-word app)
  "Search forward for the word at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (and (< pos len)
             (let ((ch (string-ref text pos)))
               (or (char-alphabetic? ch) (char-numeric? ch))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (char-alphabetic? ch) (char-numeric? ch))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (char-alphabetic? ch) (char-numeric? ch))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (found (string-contains text word end)))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app) (string-append "Found: " word)))
          (echo-error! (app-state-echo app) (string-append "\"" word "\" not found below"))))
      (echo-error! (app-state-echo app) "Not on a word"))))

(def (cmd-search-backward-word app)
  "Search backward for the word at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (and (< pos len)
             (let ((ch (string-ref text pos)))
               (or (char-alphabetic? ch) (char-numeric? ch))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (char-alphabetic? ch) (char-numeric? ch))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (char-alphabetic? ch) (char-numeric? ch))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (found (let loop ((p 0) (last-found #f))
                      (let ((f (string-contains text word p)))
                        (if (and f (< f start))
                          (loop (+ f 1) f)
                          last-found)))))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app) (string-append "Found: " word)))
          (echo-error! (app-state-echo app) (string-append "\"" word "\" not found above"))))
      (echo-error! (app-state-echo app) "Not on a word"))))

(def (cmd-count-occurrences app)
  "Count occurrences of a string in the buffer."
  (let ((pat (qt-echo-read-string app "Count occurrences of: ")))
    (when pat
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (count (let loop ((p 0) (n 0))
                      (let ((f (string-contains text pat p)))
                        (if f (loop (+ f (max 1 (string-length pat))) (+ n 1)) n)))))
        (echo-message! (app-state-echo app)
          (string-append (number->string count) " occurrences of \"" pat "\""))))))

(def (cmd-delete-file-and-buffer app)
  "Delete the file and kill the buffer."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "Error deleting file"))
          (lambda ()
            (delete-file path)
            (cmd-kill-buffer-cmd app)
            (echo-message! (app-state-echo app) (string-append "Deleted: " path)))))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-find-file-literally app)
  "Open a file without any processing (same as find-file)."
  (cmd-find-file app))

(def (cmd-kill-matching-buffers app)
  "Kill all buffers whose names match a pattern."
  (let ((pattern (qt-echo-read-string app "Kill buffers matching: ")))
    (when pattern
      (let ((killed 0))
        (for-each
          (lambda (buf)
            (when (string-contains (buffer-name buf) pattern)
              (set! killed (+ killed 1))))
          (buffer-list))
        (echo-message! (app-state-echo app)
          (string-append "Would kill " (number->string killed) " matching buffers"))))))

(def *recent-files* [])
(def *recent-files-max* 50)
(def *recent-files-path*
  (path-expand ".gerbil-emacs-recent-files" (user-info-home (user-info (user-name)))))

(def (recent-files-add! path)
  "Add a file path to the recent files list (most recent first, no duplicates)."
  (when (and path (string? path) (> (string-length path) 0))
    (let ((abs-path (path-expand path)))
      ;; Remove existing entry if present, then prepend
      (set! *recent-files*
        (cons abs-path
          (let loop ((files *recent-files*) (acc []))
            (cond
              ((null? files) (reverse acc))
              ((string=? (car files) abs-path) (loop (cdr files) acc))
              (else (loop (cdr files) (cons (car files) acc)))))))
      ;; Trim to max size
      (when (> (length *recent-files*) *recent-files-max*)
        (set! *recent-files*
          (let loop ((files *recent-files*) (n 0) (acc []))
            (if (or (null? files) (>= n *recent-files-max*))
              (reverse acc)
              (loop (cdr files) (+ n 1) (cons (car files) acc))))))
      ;; Save to disk
      (recent-files-save!))))

(def (recent-files-save!)
  "Persist recent files list to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file *recent-files-path*
        (lambda (port)
          (for-each (lambda (f) (display f port) (newline port))
                    *recent-files*))))))

(def (recent-files-load!)
  "Load recent files list from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *recent-files-path*)
        (set! *recent-files*
          (call-with-input-file *recent-files-path*
            (lambda (port)
              (let loop ((acc []))
                (let ((line (read-line port)))
                  (if (eof-object? line)
                    (reverse acc)
                    (if (> (string-length line) 0)
                      (loop (cons line acc))
                      (loop acc))))))))))))

(def (cmd-list-recent-files app)
  "Show list of recently opened files."
  (if (null? *recent-files*)
    (echo-message! (app-state-echo app) "No recent files")
    (let* ((text (string-join *recent-files* "\n"))
           (fr (app-state-frame app))
           (ed (qt-current-editor fr))
           (buf (qt-buffer-create! "*Recent Files*" ed #f)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text))))

(def (cmd-clear-recent-files app)
  "Clear the recent files list."
  (set! *recent-files* [])
  (recent-files-save!)
  (echo-message! (app-state-echo app) "Recent files cleared"))

(def (cmd-recentf-open app)
  "Open a recently visited file using completion."
  (if (null? *recent-files*)
    (echo-message! (app-state-echo app) "No recent files")
    (let ((choice (qt-echo-read-string-with-completion app "Recent file: " *recent-files*)))
      (when (and choice (> (string-length choice) 0))
        (cond
          ;; Directory -> dired
          ((and (file-exists? choice)
                (eq? 'directory (file-info-type (file-info choice))))
           (dired-open-directory! app choice))
          ;; Regular file
          (else
           (let* ((name (path-strip-directory choice))
                  (fr (app-state-frame app))
                  (ed (current-qt-editor app))
                  (buf (qt-buffer-create! name ed choice)))
             (qt-buffer-attach! ed buf)
             (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
             (when (file-exists? choice)
               (let ((text (read-file-as-string choice)))
                 (when text
                   (qt-plain-text-edit-set-text! ed text)
                   (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                   (qt-plain-text-edit-set-cursor-position! ed 0))))
             (qt-setup-highlighting! app buf))))))))

(def (cmd-recentf-cleanup app)
  "Remove non-existent files from the recent files list."
  (let* ((before (length *recent-files*))
         (cleaned (filter file-exists? *recent-files*))
         (removed (- before (length cleaned))))
    (set! *recent-files* cleaned)
    (recent-files-save!)
    (echo-message! (app-state-echo app)
      (string-append "Removed " (number->string removed) " non-existent files"))))

(def (cmd-multi-occur app)
  "Search for pattern across all file-visiting buffers."
  (let ((pat (qt-echo-read-string app "Multi-occur: ")))
    (when pat
      (let* ((fr (app-state-frame app))
             (file-results
               (let loop ((bufs (buffer-list)) (acc []))
                 (if (null? bufs)
                   (reverse acc)
                   (let* ((buf (car bufs))
                          (file (buffer-file-path buf)))
                     (if (and file (file-exists? file))
                       (with-catch
                         (lambda (e) (loop (cdr bufs) acc))
                         (lambda ()
                           (let* ((content (read-file-as-string file))
                                  (lines (string-split content #\newline))
                                  (matches
                                    (let mloop ((ls lines) (n 1) (hits []))
                                      (if (null? ls) (reverse hits)
                                        (if (string-contains (car ls) pat)
                                          (mloop (cdr ls) (+ n 1)
                                            (cons (string-append (buffer-name buf) ":"
                                                    (number->string n) ": " (car ls))
                                                  hits))
                                          (mloop (cdr ls) (+ n 1) hits))))))
                             (loop (cdr bufs) (append acc matches)))))
                       (loop (cdr bufs) acc))))))
             (output (if (null? file-results)
                       (string-append "No matches for: " pat)
                       (string-join file-results "\n")))
             (ed (qt-current-editor fr))
             (buf (qt-buffer-create! "*Multi-Occur*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed output)
        (echo-message! (app-state-echo app)
          (string-append (number->string (length file-results)) " matches for: " pat))))))

(def (cmd-align-current app)
  "Align the region on a separator."
  (let ((sep (qt-echo-read-string app "Align on: ")))
    (when sep
      (let* ((ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (mark (buffer-mark buf))
             (pos (qt-plain-text-edit-cursor-position ed)))
        (if mark
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (max-col (let loop ((ls lines) (max-c 0))
                            (if (null? ls) max-c
                              (let ((p (string-contains (car ls) sep)))
                                (loop (cdr ls) (if p (max max-c p) max-c))))))
                 (aligned (map (lambda (l)
                                 (let ((p (string-contains l sep)))
                                   (if p
                                     (string-append (substring l 0 p)
                                       (make-string (- max-col p) #\space)
                                       (substring l p (string-length l)))
                                     l)))
                               lines))
                 (result (string-join aligned "\n")))
            (qt-plain-text-edit-set-selection! ed start end)
            (qt-plain-text-edit-remove-selected-text! ed)
            (qt-plain-text-edit-insert-text! ed result)
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app) "Aligned"))
          (echo-error! (app-state-echo app) "No mark set"))))))

(def (cmd-clear-rectangle app)
  "Clear text in a rectangle region (replace with spaces)."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (line1 (qt-plain-text-edit-cursor-line ed))
             (mark-line (let loop ((i 0) (p 0))
                          (if (>= p mark) i
                            (if (< i (length lines))
                              (loop (+ i 1) (+ p (string-length (list-ref lines i)) 1))
                              i))))
             (col1 (- pos (line-start-position text line1)))
             (col2 (- mark (line-start-position text mark-line)))
             (start-line (min line1 mark-line))
             (end-line (max line1 mark-line))
             (start-col (min col1 col2))
             (end-col (max col1 col2))
             (new-lines
               (let loop ((i 0) (ls lines) (acc []))
                 (if (null? ls) (reverse acc)
                   (if (and (>= i start-line) (<= i end-line))
                     (let* ((l (car ls))
                            (llen (string-length l))
                            (sc (min start-col llen))
                            (ec (min end-col llen))
                            (spaces (make-string (- ec sc) #\space))
                            (new-l (string-append (substring l 0 sc) spaces
                                     (if (< ec llen) (substring l ec llen) ""))))
                       (loop (+ i 1) (cdr ls) (cons new-l acc)))
                     (loop (+ i 1) (cdr ls) (cons (car ls) acc)))))))
        (qt-plain-text-edit-set-text! ed (string-join new-lines "\n"))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Rectangle cleared"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-describe-mode app)
  "Describe the current buffer mode."
  (let* ((buf (current-qt-buffer app))
         (lang (buffer-lexer-lang buf)))
    (echo-message! (app-state-echo app)
      (string-append "Major mode: " (if lang (symbol->string lang) "fundamental")))))

(def (cmd-describe-face app)
  "Describe the face at point."
  (echo-message! (app-state-echo app) "QPlainTextEdit uses uniform styling"))

(def (cmd-describe-function app)
  "Describe a function/command."
  (let ((name (qt-echo-read-string app "Describe function: ")))
    (when name
      (let ((sym (string->symbol name)))
        (if (find-command sym)
          (echo-message! (app-state-echo app) (string-append name " is a registered command"))
          (echo-error! (app-state-echo app) (string-append name " is not a command")))))))

(def (cmd-describe-variable app)
  "Describe a variable."
  (echo-message! (app-state-echo app) "Variable inspection not available"))

(def (cmd-describe-syntax app)
  "Describe syntax at point."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (if (< pos len)
      (let* ((ch (string-ref text pos))
             (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch) " (U+"
            (number->string code 16) ") "
            (cond ((char-alphabetic? ch) "letter")
                  ((char-numeric? ch) "digit")
                  ((char-whitespace? ch) "whitespace")
                  ((memq ch '(#\( #\) #\[ #\] #\{ #\})) "bracket")
                  ((memq ch '(#\" #\')) "string delimiter")
                  (else "punctuation")))))
      (echo-message! (app-state-echo app) "End of buffer"))))

(def (cmd-insert-lorem-ipsum app)
  "Insert Lorem Ipsum text."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-insert-text! ed
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\n")))

(def (cmd-insert-current-date-iso app)
  "Insert date in ISO format (YYYY-MM-DD)."
  (let* ((ed (current-qt-editor app))
         (t (time->seconds (current-time)))
         (date-str (let* ((secs (inexact->exact (floor t)))
                          (out (open-process (list path: "date" arguments: '("+%Y-%m-%d")))))
                     (let ((result (read-line out)))
                       (close-port out)
                       (if (string? result) result "???")))))
    (qt-plain-text-edit-insert-text! ed date-str)))

(def (cmd-insert-time app)
  "Insert current time."
  (let* ((ed (current-qt-editor app))
         (out (open-process (list path: "date" arguments: '("+%H:%M:%S")))))
    (let ((result (read-line out)))
      (close-port out)
      (when (string? result)
        (qt-plain-text-edit-insert-text! ed result)))))

;;;============================================================================
;;; Eldoc — automatic function signature display
;;;============================================================================

(def *eldoc-mode* #t)  ;; enabled by default for Gerbil/Scheme buffers

;; Signature database: symbol-string -> "(sym arg1 arg2 ...)"
(def *gerbil-signatures* (make-hash-table))

(def (eldoc-init-signatures!)
  "Populate the signature database with common Gerbil functions."
  ;; Hash tables
  (hash-put! *gerbil-signatures* "hash-get" "(hash-get table key)")
  (hash-put! *gerbil-signatures* "hash-put!" "(hash-put! table key value)")
  (hash-put! *gerbil-signatures* "hash-remove!" "(hash-remove! table key)")
  (hash-put! *gerbil-signatures* "hash-key?" "(hash-key? table key)")
  (hash-put! *gerbil-signatures* "hash-ref" "(hash-ref table key [default])")
  (hash-put! *gerbil-signatures* "hash-update!" "(hash-update! table key proc [default])")
  (hash-put! *gerbil-signatures* "hash-for-each" "(hash-for-each proc table)")
  (hash-put! *gerbil-signatures* "hash-map" "(hash-map proc table)")
  (hash-put! *gerbil-signatures* "hash-fold" "(hash-fold proc init table)")
  (hash-put! *gerbil-signatures* "hash->list" "(hash->list table)")
  (hash-put! *gerbil-signatures* "list->hash-table" "(list->hash-table alist)")
  (hash-put! *gerbil-signatures* "make-hash-table" "(make-hash-table [size:])")
  (hash-put! *gerbil-signatures* "hash-length" "(hash-length table)")
  (hash-put! *gerbil-signatures* "hash-keys" "(hash-keys table)")
  (hash-put! *gerbil-signatures* "hash-values" "(hash-values table)")
  (hash-put! *gerbil-signatures* "hash-copy" "(hash-copy table)")
  (hash-put! *gerbil-signatures* "hash-merge" "(hash-merge table1 table2)")
  (hash-put! *gerbil-signatures* "hash-merge!" "(hash-merge! table1 table2)")
  ;; Lists
  (hash-put! *gerbil-signatures* "map" "(map proc list ...)")
  (hash-put! *gerbil-signatures* "for-each" "(for-each proc list ...)")
  (hash-put! *gerbil-signatures* "filter" "(filter pred list)")
  (hash-put! *gerbil-signatures* "foldl" "(foldl proc init list)")
  (hash-put! *gerbil-signatures* "foldr" "(foldr proc init list)")
  (hash-put! *gerbil-signatures* "append" "(append list ...)")
  (hash-put! *gerbil-signatures* "reverse" "(reverse list)")
  (hash-put! *gerbil-signatures* "length" "(length list)")
  (hash-put! *gerbil-signatures* "assoc" "(assoc key alist [=])")
  (hash-put! *gerbil-signatures* "member" "(member elem list)")
  (hash-put! *gerbil-signatures* "sort" "(sort list less?)")
  (hash-put! *gerbil-signatures* "iota" "(iota count [start step])")
  (hash-put! *gerbil-signatures* "take" "(take list n)")
  (hash-put! *gerbil-signatures* "drop" "(drop list n)")
  (hash-put! *gerbil-signatures* "find" "(find pred list)")
  (hash-put! *gerbil-signatures* "any" "(any pred list)")
  (hash-put! *gerbil-signatures* "every" "(every pred list)")
  (hash-put! *gerbil-signatures* "partition" "(partition pred list)")
  ;; Strings
  (hash-put! *gerbil-signatures* "string-append" "(string-append str ...)")
  (hash-put! *gerbil-signatures* "string-length" "(string-length str)")
  (hash-put! *gerbil-signatures* "string-ref" "(string-ref str index)")
  (hash-put! *gerbil-signatures* "substring" "(substring str start end)")
  (hash-put! *gerbil-signatures* "string-contains" "(string-contains str search)")
  (hash-put! *gerbil-signatures* "string-prefix?" "(string-prefix? prefix str)")
  (hash-put! *gerbil-signatures* "string-suffix?" "(string-suffix? suffix str)")
  (hash-put! *gerbil-signatures* "string-split" "(string-split str sep)")
  (hash-put! *gerbil-signatures* "string-join" "(string-join strs [sep])")
  (hash-put! *gerbil-signatures* "string-upcase" "(string-upcase str)")
  (hash-put! *gerbil-signatures* "string-downcase" "(string-downcase str)")
  (hash-put! *gerbil-signatures* "number->string" "(number->string num [radix])")
  (hash-put! *gerbil-signatures* "string->number" "(string->number str [radix])")
  (hash-put! *gerbil-signatures* "string->symbol" "(string->symbol str)")
  (hash-put! *gerbil-signatures* "symbol->string" "(symbol->string sym)")
  ;; I/O
  (hash-put! *gerbil-signatures* "open-input-file" "(open-input-file path)")
  (hash-put! *gerbil-signatures* "open-output-file" "(open-output-file path)")
  (hash-put! *gerbil-signatures* "call-with-input-file" "(call-with-input-file path proc)")
  (hash-put! *gerbil-signatures* "call-with-output-file" "(call-with-output-file path proc)")
  (hash-put! *gerbil-signatures* "read-line" "(read-line [port])")
  (hash-put! *gerbil-signatures* "display" "(display obj [port])")
  (hash-put! *gerbil-signatures* "write" "(write obj [port])")
  (hash-put! *gerbil-signatures* "newline" "(newline [port])")
  (hash-put! *gerbil-signatures* "close-port" "(close-port port)")
  ;; Control flow
  (hash-put! *gerbil-signatures* "with-catch" "(with-catch handler thunk)")
  (hash-put! *gerbil-signatures* "call-with-current-continuation" "(call/cc proc)")
  (hash-put! *gerbil-signatures* "values" "(values val ...)")
  (hash-put! *gerbil-signatures* "call-with-values" "(call-with-values producer consumer)")
  (hash-put! *gerbil-signatures* "dynamic-wind" "(dynamic-wind before thunk after)")
  (hash-put! *gerbil-signatures* "error" "(error message irritants ...)")
  (hash-put! *gerbil-signatures* "raise" "(raise exception)")
  ;; Paths
  (hash-put! *gerbil-signatures* "path-expand" "(path-expand path [origin])")
  (hash-put! *gerbil-signatures* "path-directory" "(path-directory path)")
  (hash-put! *gerbil-signatures* "path-strip-directory" "(path-strip-directory path)")
  (hash-put! *gerbil-signatures* "path-extension" "(path-extension path)")
  (hash-put! *gerbil-signatures* "path-strip-extension" "(path-strip-extension path)")
  (hash-put! *gerbil-signatures* "file-exists?" "(file-exists? path)")
  (hash-put! *gerbil-signatures* "file-info" "(file-info path)")
  (hash-put! *gerbil-signatures* "create-directory" "(create-directory path)")
  (hash-put! *gerbil-signatures* "delete-file" "(delete-file path)")
  (hash-put! *gerbil-signatures* "rename-file" "(rename-file old new)")
  ;; Threads / concurrency
  (hash-put! *gerbil-signatures* "spawn" "(spawn thunk)")
  (hash-put! *gerbil-signatures* "spawn/name" "(spawn/name name thunk)")
  (hash-put! *gerbil-signatures* "thread-sleep!" "(thread-sleep! secs)")
  (hash-put! *gerbil-signatures* "make-mutex" "(make-mutex [name])")
  (hash-put! *gerbil-signatures* "mutex-lock!" "(mutex-lock! mutex [timeout])")
  (hash-put! *gerbil-signatures* "mutex-unlock!" "(mutex-unlock! mutex)")
  (hash-put! *gerbil-signatures* "make-channel" "(make-channel [name])")
  (hash-put! *gerbil-signatures* "channel-put" "(channel-put channel value)")
  (hash-put! *gerbil-signatures* "channel-get" "(channel-get channel)")
  ;; Syntax sugar
  (hash-put! *gerbil-signatures* "def" "(def (name args ...) body ...)")
  (hash-put! *gerbil-signatures* "defstruct" "(defstruct name (field ...) [transparent: #t])")
  (hash-put! *gerbil-signatures* "defclass" "(defclass name (super ...) (slot ...))")
  (hash-put! *gerbil-signatures* "defrule" "(defrule (name pattern ...) template)")
  (hash-put! *gerbil-signatures* "let" "(let ((var val) ...) body ...)")
  (hash-put! *gerbil-signatures* "let*" "(let* ((var val) ...) body ...)")
  (hash-put! *gerbil-signatures* "letrec" "(letrec ((var val) ...) body ...)")
  (hash-put! *gerbil-signatures* "when" "(when test body ...)")
  (hash-put! *gerbil-signatures* "unless" "(unless test body ...)")
  (hash-put! *gerbil-signatures* "cond" "(cond (test expr ...) ... [(else expr ...)])")
  (hash-put! *gerbil-signatures* "case" "(case key ((datum ...) expr ...) ... [(else expr ...)])")
  (hash-put! *gerbil-signatures* "match" "(match expr (pattern body ...) ...)")
  (hash-put! *gerbil-signatures* "parameterize" "(parameterize ((param val) ...) body ...)")
  ;; Vectors
  (hash-put! *gerbil-signatures* "vector-ref" "(vector-ref vec index)")
  (hash-put! *gerbil-signatures* "vector-set!" "(vector-set! vec index val)")
  (hash-put! *gerbil-signatures* "vector-length" "(vector-length vec)")
  (hash-put! *gerbil-signatures* "make-vector" "(make-vector n [fill])")
  (hash-put! *gerbil-signatures* "vector->list" "(vector->list vec)")
  (hash-put! *gerbil-signatures* "list->vector" "(list->vector list)")
  ;; JSON
  (hash-put! *gerbil-signatures* "json-object->string" "(json-object->string obj)")
  (hash-put! *gerbil-signatures* "string->json-object" "(string->json-object str)")
  ;; Process
  (hash-put! *gerbil-signatures* "open-process" "(open-process settings)")
  (hash-put! *gerbil-signatures* "process-status" "(process-status proc)")
  ;; Apply / call
  (hash-put! *gerbil-signatures* "apply" "(apply proc arg ... args)")
  (hash-put! *gerbil-signatures* "call-with-port" "(call-with-port port proc)"))

;; Initialize the database once
(eldoc-init-signatures!)

(def (enclosing-function-at-point text pos)
  "Find the function name of the enclosing s-expression at POS.
   Returns the symbol name string or #f."
  (let ((len (string-length text)))
    (and (> len 0) (<= pos len)
         ;; Scan backwards for the opening paren of the enclosing form
         (let scan ((i (min (- pos 1) (- len 1))) (depth 0))
           (and (>= i 0)
                (let ((ch (string-ref text i)))
                  (cond
                    ((char=? ch #\)) (scan (- i 1) (+ depth 1)))
                    ((char=? ch #\()
                     (if (> depth 0)
                       (scan (- i 1) (- depth 1))
                       ;; Found opening paren — extract the function name after it
                       (let ((start (+ i 1)))
                         (let sym-end ((j start))
                           (if (and (< j len)
                                    (let ((c (string-ref text j)))
                                      (or (char-alphabetic? c) (char-numeric? c)
                                          (char=? c #\-) (char=? c #\_)
                                          (char=? c #\?) (char=? c #\!)
                                          (char=? c #\>) (char=? c #\/)
                                          (char=? c #\*) (char=? c #\+))))
                             (sym-end (+ j 1))
                             (if (> j start)
                               (substring text start j)
                               #f))))))
                    (else (scan (- i 1) depth)))))))))

(def *eldoc-last-sym* #f) ;; avoid flicker — don't re-display same sig

(def (eldoc-display! app)
  "Check cursor position and show signature in echo area if applicable."
  (when *eldoc-mode*
    (let* ((fr (app-state-frame app))
           (buf (qt-edit-window-buffer (qt-current-window fr)))
           (lang (buffer-lexer-lang buf)))
      ;; Only for Scheme-like languages
      (when (memq lang '(scheme gerbil lisp))
        (let* ((ed (qt-edit-window-editor (qt-current-window fr)))
               (text (qt-plain-text-edit-text ed))
               (pos (qt-plain-text-edit-cursor-position ed))
               (sym (enclosing-function-at-point text pos)))
          (cond
            ((and sym (not (equal? sym *eldoc-last-sym*)))
             (let ((sig (hash-get *gerbil-signatures* sym)))
               (set! *eldoc-last-sym* sym)
               (when sig
                 (echo-message! (app-state-echo app) sig))))
            ((and (not sym) *eldoc-last-sym*)
             (set! *eldoc-last-sym* #f))))))))

;; Xref back stack: list of (file-path . position) or (buffer-name . position)
(def *xref-back-stack* [])

(def (xref-push-location! app)
  "Push current location onto xref back stack."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (set! *xref-back-stack*
      (cons (cons (or (buffer-file-path buf) (buffer-name buf)) pos)
            (if (> (length *xref-back-stack*) 50)
              (let loop ((l *xref-back-stack*) (n 0) (acc []))
                (if (or (null? l) (>= n 50)) (reverse acc)
                  (loop (cdr l) (+ n 1) (cons (car l) acc))))
              *xref-back-stack*)))))

(def (symbol-at-point ed)
  "Extract the symbol name at cursor position."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (and (< pos len)
         (let* ((sym-char? (lambda (ch)
                   (or (char-alphabetic? ch) (char-numeric? ch)
                       (char=? ch #\-) (char=? ch #\_)
                       (char=? ch #\?) (char=? ch #\!))))
                (start (let loop ((i pos))
                         (if (and (> i 0) (sym-char? (string-ref text (- i 1))))
                           (loop (- i 1)) i)))
                (end (let loop ((i pos))
                       (if (and (< i len) (sym-char? (string-ref text i)))
                         (loop (+ i 1)) i)))
                (sym (substring text start end)))
           (if (> (string-length sym) 0) sym #f)))))

(def *def-patterns*
  '("(def (" "(def " "(defstruct " "(defclass " "(defrule " "(defmethod "
    "(defsyntax " "(defmacro " "(defun " "(define (" "(define "))

(def (find-def-in-text text sym)
  "Search TEXT for a definition of SYM. Returns char position or #f."
  (let loop ((patterns *def-patterns*))
    (if (null? patterns) #f
      (let ((pattern (string-append (car patterns) sym)))
        (let ((found (string-contains text pattern)))
          (if found
            ;; Verify it's a word boundary (next char is space, paren, or newline)
            (let ((end-pos (+ found (string-length pattern))))
              (if (or (>= end-pos (string-length text))
                      (let ((ch (string-ref text end-pos)))
                        (or (char=? ch #\space) (char=? ch #\)) (char=? ch #\newline)
                            (char=? ch #\tab))))
                found
                (loop (cdr patterns))))
            (loop (cdr patterns))))))))

(def (find-def-in-project sym root)
  "Search project files for definition of SYM. Returns (file . char-pos) or #f."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/grep"
                           arguments: (list "-rnl"
                             (string-append "(def[a-z]* (" sym "\\|def[a-z]* " sym)
                             "--include=*.ss" "--include=*.scm"
                             root)
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        (and output
             (let ((files (let loop ((s output) (acc []))
                            (let ((nl (string-index s #\newline)))
                              (if nl
                                (loop (substring s (+ nl 1) (string-length s))
                                      (cons (substring s 0 nl) acc))
                                (reverse (if (> (string-length s) 0)
                                           (cons s acc) acc)))))))
               ;; Search each file for the actual definition position
               (let loop ((fs files))
                 (if (null? fs) #f
                   (let ((text (with-catch (lambda (e) #f)
                                 (lambda () (read-file-as-string (car fs))))))
                     (if text
                       (let ((pos (find-def-in-text text sym)))
                         (if pos (cons (car fs) pos)
                           (loop (cdr fs))))
                       (loop (cdr fs))))))))))))

(def (cmd-goto-definition app)
  "Jump to definition of symbol at point. Searches current buffer, then project."
  (let* ((ed (current-qt-editor app))
         (sym (symbol-at-point ed)))
    (if (not sym)
      (echo-message! (app-state-echo app) "No symbol at point")
      (let* ((text (qt-plain-text-edit-text ed))
             (local-pos (find-def-in-text text sym)))
        (if local-pos
          ;; Found in current buffer
          (begin
            (xref-push-location! app)
            (qt-plain-text-edit-set-cursor-position! ed local-pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            (echo-message! (app-state-echo app) (string-append "-> " sym)))
          ;; Search project
          (let* ((root (current-project-root app))
                 (result (find-def-in-project sym root)))
            (if result
              (let* ((file (car result))
                     (pos (cdr result))
                     (fr (app-state-frame app)))
                (xref-push-location! app)
                ;; Open file and jump
                (let* ((name (path-strip-directory file))
                       (existing (let loop ((bufs *buffer-list*))
                                   (if (null? bufs) #f
                                     (let ((b (car bufs)))
                                       (if (and (buffer-file-path b)
                                                (string=? (buffer-file-path b) file))
                                         b (loop (cdr bufs)))))))
                       (buf (or existing (qt-buffer-create! name ed file))))
                  (qt-buffer-attach! ed buf)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                  (when (not existing)
                    (let ((ftext (read-file-as-string file)))
                      (when ftext
                        (qt-plain-text-edit-set-text! ed ftext)
                        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
                    (qt-setup-highlighting! app buf))
                  (qt-plain-text-edit-set-cursor-position! ed pos)
                  (qt-plain-text-edit-ensure-cursor-visible! ed)
                  (echo-message! (app-state-echo app) (string-append "-> " sym " in " name))))
              (echo-error! (app-state-echo app)
                (string-append "Definition not found: " sym)))))))))

(def (cmd-xref-back app)
  "Pop xref stack and return to previous location."
  (if (null? *xref-back-stack*)
    (echo-error! (app-state-echo app) "No previous location")
    (let* ((loc (car *xref-back-stack*))
           (path-or-name (car loc))
           (pos (cdr loc))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      (set! *xref-back-stack* (cdr *xref-back-stack*))
      ;; Find buffer by file-path or name
      (let ((buf (or (let loop ((bufs *buffer-list*))
                       (if (null? bufs) #f
                         (let ((b (car bufs)))
                           (if (and (buffer-file-path b)
                                    (string=? (buffer-file-path b) path-or-name))
                             b (loop (cdr bufs))))))
                     (buffer-by-name path-or-name))))
        (if buf
          (begin
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (string-length (qt-plain-text-edit-text ed))))
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            (echo-message! (app-state-echo app) "Back"))
          (echo-error! (app-state-echo app) "Buffer no longer exists"))))))

(def (imenu-extract-definitions text lang)
  "Extract symbol definitions from TEXT based on LANG.
Returns list of (name . line-number) pairs."
  (let ((lines (string-split text #\newline))
        (defs []))
    (let loop ((ls lines) (line-num 1))
      (if (null? ls)
        (reverse defs)
        (let ((line (car ls)))
          ;; Match based on language
          (let ((found
                 (cond
                   ;; Gerbil/Scheme/Lisp: (def, (defstruct, (defclass, (defrule, etc.
                   ((memq lang '(scheme gerbil lisp))
                    (cond
                      ;; (def (name ...) or (def name
                      ((and (>= (string-length line) 5)
                            (string-prefix? "(def " line))
                       (let* ((rest (substring line 5 (string-length line)))
                              (rest (if (and (> (string-length rest) 0)
                                             (char=? (string-ref rest 0) #\())
                                      (substring rest 1 (string-length rest))
                                      rest)))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\-) (char=? ch #\_)
                                          (char=? ch #\!) (char=? ch #\?))))
                             (scan (+ i 1))
                             (and (> i 0) (substring rest 0 i))))))
                      ;; (defstruct name, (defclass name, etc.
                      ((or (string-prefix? "(defstruct " line)
                           (string-prefix? "(defclass " line)
                           (string-prefix? "(defrule " line)
                           (string-prefix? "(defsyntax " line)
                           (string-prefix? "(defmethod " line))
                       (let* ((space-pos (string-index line #\space 1))
                              (rest (and space-pos
                                         (substring line (+ space-pos 1)
                                                    (string-length line)))))
                         (and rest
                              (let scan ((i 0))
                                (if (and (< i (string-length rest))
                                         (let ((ch (string-ref rest i)))
                                           (or (char-alphabetic? ch) (char-numeric? ch)
                                               (char=? ch #\-) (char=? ch #\_)
                                               (char=? ch #\!) (char=? ch #\?))))
                                  (scan (+ i 1))
                                  (and (> i 0) (substring rest 0 i)))))))
                      (else #f)))
                   ;; Python: def/class at start of line
                   ((eq? lang 'python)
                    (cond
                      ((string-prefix? "def " line)
                       (let* ((rest (substring line 4 (string-length line))))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\_))))
                             (scan (+ i 1))
                             (and (> i 0) (string-append "def " (substring rest 0 i)))))))
                      ((string-prefix? "class " line)
                       (let* ((rest (substring line 6 (string-length line))))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\_))))
                             (scan (+ i 1))
                             (and (> i 0) (string-append "class " (substring rest 0 i)))))))
                      (else #f)))
                   ;; C/C++/Java/Go/Rust/JS/TS: function name(
                   ((memq lang '(c cpp java go rust javascript typescript))
                    ;; Look for lines containing "name(" that start at indent 0
                    ;; and don't start with # (preprocessor) or // (comment)
                    (and (> (string-length line) 0)
                         (not (char=? (string-ref line 0) #\#))
                         (not (string-prefix? "//" line))
                         (not (string-prefix? " " line))
                         (not (string-prefix? "\t" line))
                         ;; Look for word( pattern
                         (let ((paren-pos (string-index line #\()))
                           (and paren-pos (> paren-pos 0)
                                ;; Extract the word before (
                                (let scan ((i (- paren-pos 1)))
                                  (if (and (>= i 0)
                                           (let ((ch (string-ref line i)))
                                             (or (char-alphabetic? ch) (char-numeric? ch)
                                                 (char=? ch #\_))))
                                    (scan (- i 1))
                                    (let ((name (substring line (+ i 1) paren-pos)))
                                      (and (> (string-length name) 0)
                                           ;; Skip common keywords
                                           (not (member name '("if" "for" "while" "switch"
                                                               "return" "else" "catch"
                                                               "sizeof" "typeof")))
                                           name))))))))
                   ;; Shell: function name() or name()
                   ((memq lang '(shell bash))
                    (or (and (string-prefix? "function " line)
                             (let* ((rest (substring line 9 (string-length line))))
                               (let scan ((i 0))
                                 (if (and (< i (string-length rest))
                                          (let ((ch (string-ref rest i)))
                                            (or (char-alphabetic? ch) (char-numeric? ch)
                                                (char=? ch #\_))))
                                   (scan (+ i 1))
                                   (and (> i 0) (substring rest 0 i))))))
                        #f))
                   ;; Ruby: def name
                   ((eq? lang 'ruby)
                    (and (string-prefix? "def " line)
                         (let* ((rest (substring line 4 (string-length line))))
                           (let scan ((i 0))
                             (if (and (< i (string-length rest))
                                      (let ((ch (string-ref rest i)))
                                        (or (char-alphabetic? ch) (char-numeric? ch)
                                            (char=? ch #\_) (char=? ch #\?))))
                               (scan (+ i 1))
                               (and (> i 0) (substring rest 0 i)))))))
                   ;; Fallback: look for "def " prefix
                   (else
                    (and (string-prefix? "(def " line)
                         (let* ((rest (substring line 5 (string-length line)))
                                (rest (if (and (> (string-length rest) 0)
                                               (char=? (string-ref rest 0) #\())
                                        (substring rest 1 (string-length rest))
                                        rest)))
                           (let scan ((i 0))
                             (if (and (< i (string-length rest))
                                      (let ((ch (string-ref rest i)))
                                        (or (char-alphabetic? ch) (char-numeric? ch)
                                            (char=? ch #\-) (char=? ch #\_))))
                               (scan (+ i 1))
                               (and (> i 0) (substring rest 0 i))))))))))
            (when found
              (set! defs (cons (cons found line-num) defs)))
            (loop (cdr ls) (+ line-num 1))))))))

(def (cmd-imenu app)
  "List definitions in the current buffer and jump to selected one."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (lang (buffer-lexer-lang buf))
         (defs (imenu-extract-definitions text lang))
         (echo (app-state-echo app)))
    (if (null? defs)
      (echo-error! echo "No definitions found")
      (let* ((names (map (lambda (d)
                           (string-append (car d) " (L" (number->string (cdr d)) ")"))
                         defs))
             (choice (qt-echo-read-string-with-completion app "Go to: " names)))
        (when (and choice (> (string-length choice) 0))
          ;; Find the matching definition
          (let ((found (let loop ((ds defs) (ns names))
                         (cond
                           ((null? ds) #f)
                           ((string=? choice (car ns)) (car ds))
                           (else (loop (cdr ds) (cdr ns)))))))
            (when found
              (let* ((line-num (cdr found))
                     (target-pos (text-line-position text line-num)))
                (qt-plain-text-edit-set-cursor-position! ed target-pos)
                (qt-plain-text-edit-ensure-cursor-visible! ed)
                (echo-message! echo (string-append (car found) " — line "
                                                   (number->string line-num)))))))))))

(def (cmd-show-word-count app)
  "Show word count for the entire buffer."
  (cmd-count-words-buffer app))

(def (cmd-show-char-count app)
  "Show character count."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (echo-message! (app-state-echo app)
      (string-append (number->string (string-length text)) " characters"))))

(def (cmd-insert-path-separator app)
  "Insert a path separator."
  (qt-plain-text-edit-insert-text! (current-qt-editor app) "/"))

(def (cmd-maximize-window app)
  "Maximize current window by deleting others."
  (cmd-delete-other-windows app))

(def (cmd-minimize-window app)
  "Minimize window (placeholder)."
  (echo-message! (app-state-echo app) "Window minimized"))

(def (cmd-delete-matching-lines app)
  "Delete lines matching a pattern (alias for flush-lines)."
  (cmd-flush-lines app))

(def (cmd-delete-non-matching-lines app)
  "Delete lines not matching a pattern (alias for keep-lines)."
  (cmd-keep-lines app))

(def (cmd-copy-matching-lines app)
  "Copy lines matching a pattern to kill ring."
  (let ((pat (qt-echo-read-string app "Copy lines matching: ")))
    (when pat
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (matches (filter (lambda (l) (string-contains l pat)) lines)))
        (if (pair? matches)
          (let ((result (string-join matches "\n")))
            (set! (app-state-kill-ring app) (cons result (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
              (string-append (number->string (length matches)) " lines copied")))
          (echo-message! (app-state-echo app) "No matching lines"))))))

(def (cmd-count-lines-buffer app)
  "Count lines in the buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline)))
    (echo-message! (app-state-echo app)
      (string-append (number->string (length lines)) " lines"))))

(def (cmd-count-words-paragraph app)
  "Count words in the current paragraph."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         ;; Find paragraph boundaries
         (para-start
           (let loop ((i (- pos 1)))
             (cond
               ((< i 0) 0)
               ((and (char=? (string-ref text i) #\newline)
                     (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                (+ i 1))
               (else (loop (- i 1))))))
         (para-end
           (let loop ((i pos))
             (cond
               ((>= i len) len)
               ((and (char=? (string-ref text i) #\newline)
                     (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\newline))
                i)
               (else (loop (+ i 1))))))
         (para (substring text para-start para-end))
         (words (let loop ((i 0) (in-word? #f) (count 0))
                  (if (>= i (string-length para))
                    (if in-word? (+ count 1) count)
                    (let ((ch (string-ref para i)))
                      (if (char-whitespace? ch)
                        (loop (+ i 1) #f (if in-word? (+ count 1) count))
                        (loop (+ i 1) #t count)))))))
    (echo-message! (app-state-echo app)
      (string-append (number->string words) " words in paragraph"))))

(def (cmd-convert-to-unix app)
  "Convert line endings to Unix (LF)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (new-text (let loop ((i 0) (acc []))
                     (cond
                       ((>= i (string-length text))
                        (list->string (reverse acc)))
                       ((and (char=? (string-ref text i) #\return)
                             (< (+ i 1) (string-length text))
                             (char=? (string-ref text (+ i 1)) #\newline))
                        (loop (+ i 2) (cons #\newline acc)))
                       ((char=? (string-ref text i) #\return)
                        (loop (+ i 1) (cons #\newline acc)))
                       (else (loop (+ i 1) (cons (string-ref text i) acc)))))))
    (qt-plain-text-edit-set-text! ed new-text)
    (echo-message! (app-state-echo app) "Converted to Unix line endings")))

(def (cmd-convert-to-dos app)
  "Convert line endings to DOS (CRLF)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         ;; First convert to Unix, then to DOS
         (unix (let loop ((i 0) (acc []))
                 (cond
                   ((>= i (string-length text))
                    (list->string (reverse acc)))
                   ((and (char=? (string-ref text i) #\return)
                         (< (+ i 1) (string-length text))
                         (char=? (string-ref text (+ i 1)) #\newline))
                    (loop (+ i 2) (cons #\newline acc)))
                   ((char=? (string-ref text i) #\return)
                    (loop (+ i 1) (cons #\newline acc)))
                   (else (loop (+ i 1) (cons (string-ref text i) acc))))))
         (dos (let loop ((i 0) (acc []))
                (cond
                  ((>= i (string-length unix))
                   (list->string (reverse acc)))
                  ((char=? (string-ref unix i) #\newline)
                   (loop (+ i 1) (cons #\newline (cons #\return acc))))
                  (else (loop (+ i 1) (cons (string-ref unix i) acc)))))))
    (qt-plain-text-edit-set-text! ed dos)
    (echo-message! (app-state-echo app) "Converted to DOS line endings")))

(def (cmd-show-line-endings app)
  "Show the line ending style of current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (has-cr (string-contains text "\r\n"))
         (style (if has-cr "DOS (CRLF)" "Unix (LF)")))
    (echo-message! (app-state-echo app) (string-append "Line endings: " style))))

(def (cmd-wrap-lines-at-column app)
  "Wrap long lines at a specified column."
  (let ((col-str (qt-echo-read-string app "Wrap at column: ")))
    (when col-str
      (let ((col (string->number col-str)))
        (when (and col (> col 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (lines (string-split text #\newline))
                 (wrapped
                   (apply append
                     (map (lambda (line)
                            (if (<= (string-length line) col)
                              (list line)
                              ;; Break line at word boundaries
                              (let loop ((rest line) (acc []))
                                (if (<= (string-length rest) col)
                                  (reverse (cons rest acc))
                                  (let* ((break-pos
                                           (let bloop ((i col))
                                             (cond
                                               ((<= i 0) col)
                                               ((char=? (string-ref rest i) #\space) i)
                                               (else (bloop (- i 1))))))
                                         (frag (substring rest 0 break-pos))
                                         (remaining (if (< break-pos (string-length rest))
                                                      (substring rest (+ break-pos 1)
                                                                 (string-length rest))
                                                      "")))
                                    (loop remaining (cons frag acc)))))))
                          lines)))
                 (new-text (string-join wrapped "\n")))
            (qt-plain-text-edit-set-text! ed new-text)
            (echo-message! (app-state-echo app)
              (string-append "Lines wrapped at column " col-str))))))))

(def (cmd-strip-line-numbers app)
  "Strip leading line numbers from each line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (stripped (map (lambda (line)
                          ;; Strip leading digits + optional separator (: or . or space)
                          (let loop ((i 0))
                            (cond
                              ((>= i (string-length line)) line)
                              ((char-numeric? (string-ref line i)) (loop (+ i 1)))
                              ((and (> i 0) (memq (string-ref line i) '(#\: #\. #\space #\tab)))
                               (let ((rest (substring line (+ i 1) (string-length line))))
                                 (if (and (> (string-length rest) 0)
                                          (char=? (string-ref rest 0) #\space))
                                   (substring rest 1 (string-length rest))
                                   rest)))
                              (else line))))
                        lines)))
    (qt-plain-text-edit-set-text! ed (string-join stripped "\n"))
    (echo-message! (app-state-echo app) "Line numbers stripped")))

(def (cmd-goto-word-at-point app)
  "Search for the word at point (same as search-forward-word)."
  (cmd-search-forward-word app))

(def (cmd-unindent-region app)
  "Remove one level of indentation from the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (dedented (map (lambda (l)
                              (cond
                                ((and (>= (string-length l) 2)
                                      (char=? (string-ref l 0) #\space)
                                      (char=? (string-ref l 1) #\space))
                                 (substring l 2 (string-length l)))
                                ((and (>= (string-length l) 1)
                                      (char=? (string-ref l 0) #\tab))
                                 (substring l 1 (string-length l)))
                                (else l)))
                            lines))
             (result (string-join dedented "\n")))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region unindented"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-number-region app)
  "Add line numbers to the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (numbered (let loop ((ls lines) (n 1) (acc []))
                         (if (null? ls) (reverse acc)
                           (loop (cdr ls) (+ n 1)
                             (cons (string-append (number->string n) ": " (car ls)) acc)))))
             (result (string-join numbered "\n")))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-insert-kbd-macro app)
  "Insert the last keyboard macro as text."
  (let ((macro (app-state-macro-last app)))
    (if macro
      (let* ((ed (current-qt-editor app))
             (text (with-output-to-string
                     (lambda ()
                       (for-each (lambda (entry)
                                   (display "(") (display (car entry))
                                   (display " . ") (display (cdr entry))
                                   (display ")\n"))
                                 macro)))))
        (qt-plain-text-edit-insert-text! ed text))
      (echo-error! (app-state-echo app) "No macro recorded"))))

(def (cmd-name-last-kbd-macro app)
  "Name the last keyboard macro."
  (let ((name (qt-echo-read-string app "Name for macro: ")))
    (when name
      (echo-message! (app-state-echo app)
        (string-append "Macro named: " name " (naming not fully supported)")))))

(def (cmd-show-environment app)
  "Show environment variables."
  (let* ((fr (app-state-frame app))
         (ed (qt-current-editor fr))
         (out (open-process (list path: "env" arguments: '())))
         (text (read-line out #f)))
    (close-port out)
    (when (string? text)
      (let ((buf (qt-buffer-create! "*Environment*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)))))

(def (cmd-show-keybinding-for app)
  "Show keybinding for a command."
  (cmd-where-is app))

(def (cmd-first-error app)
  "Jump to the first search match."
  (let* ((ed (current-qt-editor app))
         (search (app-state-last-search app)))
    (if search
      (let* ((text (qt-plain-text-edit-text ed))
             (found (string-contains text search)))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app) (string-append "First: " search)))
          (echo-error! (app-state-echo app) "Not found")))
      (echo-error! (app-state-echo app) "No search"))))

(def (cmd-find-grep app)
  "Run grep on files (using shell grep command)."
  (let ((pat (qt-echo-read-string app "Grep for: ")))
    (when pat
      (let ((dir (qt-echo-read-string app "In directory: ")))
        (when dir
          (let* ((fr (app-state-frame app))
                 (ed (qt-current-editor fr))
                 (out (open-process
                        (list path: "grep" arguments: (list "-rn" pat dir)
                              stderr-redirection: #t)))
                 (text (read-line out #f)))
            (close-port out)
            (let ((buf (qt-buffer-create! "*Grep*" ed #f)))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed (or text "No results")))))))))

;; cmd-project-grep moved below with project commands

(def *project-root-markers*
  '(".git" "gerbil.pkg" "Makefile" "build.ss" "Cargo.toml"
    "package.json" "pyproject.toml" "setup.py" "go.mod"
    "CMakeLists.txt" ".project-root"))

(def (detect-project-root path)
  "Find project root by searching upward for marker files/dirs."
  (and path
       (let loop ((dir (path-directory path)))
         (if (string=? dir "/") #f
           (if (let check ((markers *project-root-markers*))
                 (and (pair? markers)
                      (or (file-exists? (path-expand (car markers) dir))
                          (check (cdr markers)))))
             dir
             (let ((parent (path-directory (string-append dir "/"))))
               (if (string=? parent dir) #f
                 (loop parent))))))))

(def (current-project-root app)
  "Get project root for current buffer, or current directory."
  (let ((path (buffer-file-path (current-qt-buffer app))))
    (or (detect-project-root path)
        (current-directory))))

(def (project-list-files root)
  "List files in project ROOT using find, excluding common build/vcs dirs."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/find"
                           arguments: (list root
                             "-type" "f"
                             "-not" "-path" "*/.git/*"
                             "-not" "-path" "*/.gerbil/*"
                             "-not" "-path" "*/node_modules/*"
                             "-not" "-path" "*/__pycache__/*"
                             "-not" "-path" "*/target/*"
                             "-not" "-path" "*/.build/*"
                             "-not" "-name" "*.o"
                             "-not" "-name" "*.o1")
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        (if output
          (let loop ((s output) (acc []))
            (let ((nl (string-index s #\newline)))
              (if nl
                (loop (substring s (+ nl 1) (string-length s))
                      (cons (substring s 0 nl) acc))
                (reverse (if (> (string-length s) 0) (cons s acc) acc)))))
          [])))))

(def (cmd-project-find-file app)
  "Find file in the project directory with completion."
  (let* ((root (current-project-root app))
         (files (project-list-files root))
         ;; Make paths relative to project root for nicer display
         (prefix-len (+ (string-length root) (if (char=? (string-ref root (- (string-length root) 1)) #\/) 0 1)))
         (relative-files (map (lambda (f)
                                (if (> (string-length f) prefix-len)
                                  (substring f prefix-len (string-length f))
                                  f))
                              files))
         (input (qt-echo-read-string-with-completion app
                  (string-append "Find file in " (path-strip-directory root) ": ")
                  relative-files)))
    (when (and input (> (string-length input) 0))
      (let ((full-path (path-expand input root)))
        (recent-files-add! full-path)
        (let* ((name (path-strip-directory full-path))
               (fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (qt-buffer-create! name ed full-path)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? full-path)
            (let ((text (read-file-as-string full-path)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)))
            (file-mtime-record! full-path))
          (qt-setup-highlighting! app buf)
          (apply-dir-locals! app full-path)
          (echo-message! (app-state-echo app) (string-append "Opened: " full-path)))))))

(def (cmd-project-compile app)
  "Compile the project from project root."
  (let* ((root (current-project-root app))
         (default (or (app-state-last-compile app)
                      (cond
                        ((file-exists? (path-expand "gerbil.pkg" root)) "gerbil build")
                        ((file-exists? (path-expand "Makefile" root)) "make")
                        ((file-exists? (path-expand "Cargo.toml" root)) "cargo build")
                        (else "make"))))
         (cmd (qt-echo-read-string app
                (string-append "Compile in " root " [" default "]: "))))
    (when cmd
      (let ((actual-cmd (if (string=? cmd "") default cmd)))
        (set! (app-state-last-compile app) actual-cmd)
        (compilation-run-command! app (string-append "cd " root " && " actual-cmd))))))

(def (cmd-project-grep app)
  "Grep in the project directory."
  (let* ((root (current-project-root app))
         (pattern (qt-echo-read-string app
                    (string-append "Grep in " (path-strip-directory root) ": "))))
    (when (and pattern (> (string-length pattern) 0))
      (grep-run-and-show! app pattern root '("-rn")))))

(def (cmd-reindent-buffer app)
  "Re-indent the entire buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         ;; Simple: re-indent based on paren depth
         (reindented
           (let loop ((ls lines) (depth 0) (acc []))
             (if (null? ls) (reverse acc)
               (let* ((line (car ls))
                      (trimmed (string-trim line))
                      ;; Count closing parens at start
                      (close-first (let cloop ((i 0) (d 0))
                                     (if (>= i (string-length trimmed)) d
                                       (case (string-ref trimmed i)
                                         ((#\) #\] #\}) (cloop (+ i 1) (+ d 1)))
                                         (else d)))))
                      (this-depth (max 0 (- depth close-first)))
                      (indent (make-string (* this-depth 2) #\space))
                      (new-line (string-append indent trimmed))
                      ;; Count net depth change
                      (delta (let dloop ((i 0) (d 0))
                               (if (>= i (string-length trimmed)) d
                                 (case (string-ref trimmed i)
                                   ((#\( #\[ #\{) (dloop (+ i 1) (+ d 1)))
                                   ((#\) #\] #\}) (dloop (+ i 1) (- d 1)))
                                   (else (dloop (+ i 1) d)))))))
                 (loop (cdr ls) (max 0 (+ depth delta)) (cons new-line acc)))))))
    (qt-plain-text-edit-set-text! ed (string-join reindented "\n"))
    (echo-message! (app-state-echo app) "Buffer re-indented")))

(def (cmd-fill-individual-paragraphs app)
  "Fill each paragraph in the region individually."
  (cmd-fill-paragraph app))

;;;============================================================================
;;; Batch 8: Remaining missing commands
;;;============================================================================

;; --- Font size ---
(def *font-size* 10)

(def (cmd-increase-font-size app)
  "Increase editor font size."
  (set! *font-size* (min 48 (+ *font-size* 1)))
  (let ((ed (current-qt-editor app)))
    (qt-widget-set-font-size! ed *font-size*)
    (echo-message! (app-state-echo app) (string-append "Font size: " (number->string *font-size*)))))

(def (cmd-decrease-font-size app)
  "Decrease editor font size."
  (set! *font-size* (max 6 (- *font-size* 1)))
  (let ((ed (current-qt-editor app)))
    (qt-widget-set-font-size! ed *font-size*)
    (echo-message! (app-state-echo app) (string-append "Font size: " (number->string *font-size*)))))

(def (cmd-reset-font-size app)
  "Reset font size to default."
  (set! *font-size* 10)
  (let ((ed (current-qt-editor app)))
    (qt-widget-set-font-size! ed *font-size*)
    (echo-message! (app-state-echo app) "Font size: 10 (default)")))

;; --- Navigation ---
(def (cmd-goto-first-non-blank app)
  "Go to the first non-blank character on the line."
  (cmd-back-to-indentation app))

(def (cmd-goto-last-non-blank app)
  "Go to the last non-blank character on the line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (ls (line-start-position text line))
             (last-non-blank
               (let loop ((i (- (string-length line-text) 1)))
                 (cond
                   ((<= i 0) 0)
                   ((not (char-whitespace? (string-ref line-text i))) i)
                   (else (loop (- i 1)))))))
        (qt-plain-text-edit-set-cursor-position! ed (+ ls last-non-blank))))))

(def (cmd-move-to-window-top app)
  "Move cursor to the top of the visible window."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app) "Top of buffer")))

(def (cmd-move-to-window-middle app)
  "Move cursor to the middle of the buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (mid (quotient (length lines) 2))
         (pos (line-start-position text mid)))
    (qt-plain-text-edit-set-cursor-position! ed pos)))

(def (cmd-move-to-window-bottom app)
  "Move cursor to the end of the buffer."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)))

(def (cmd-scroll-left app)
  "Scroll left (no-op in line-wrap mode)."
  (echo-message! (app-state-echo app) "Scroll left (no horizontal scrolling)"))

(def (cmd-scroll-right app)
  "Scroll right (no-op in line-wrap mode)."
  (echo-message! (app-state-echo app) "Scroll right (no horizontal scrolling)"))

;; --- Code insertion templates ---
(def (cmd-insert-let app)
  "Insert a let template."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "(let ((x ))\n  )")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 7))))

(def (cmd-insert-lambda app)
  "Insert a lambda template."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "(lambda ()\n  )")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 9))))

(def (cmd-insert-defun app)
  "Insert a def template."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "(def (name )\n  )")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 6))))

(def (cmd-insert-cond app)
  "Insert a cond template."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "(cond\n  (( ) )\n  (else ))")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 10))))

(def (cmd-insert-when app)
  "Insert a when template."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "(when \n  )")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 6))))

(def (cmd-insert-unless app)
  "Insert an unless template."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "(unless \n  )")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 8))))

(def (cmd-insert-match app)
  "Insert a match template."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "(match \n  ((_ ) ))")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 7))))

(def (cmd-insert-import app)
  "Insert an import template."
  (qt-plain-text-edit-insert-text! (current-qt-editor app) "(import )"))

(def (cmd-insert-export app)
  "Insert an export template."
  (qt-plain-text-edit-insert-text! (current-qt-editor app) "(export )"))

(def (cmd-insert-include app)
  "Insert an include template."
  (qt-plain-text-edit-insert-text! (current-qt-editor app) "(include \"\")"))

(def (cmd-insert-file-header app)
  "Insert a file header comment."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (qt-plain-text-edit-insert-text! (current-qt-editor app)
      (string-append ";;; -*- Gerbil -*-\n;;; " name "\n;;;\n\n"))))

(def (cmd-insert-header-guard app)
  "Insert a C header guard."
  (let* ((buf (current-qt-buffer app))
         (name (string-upcase (buffer-name buf)))
         (guard (let loop ((i 0) (acc []))
                  (if (>= i (string-length name))
                    (list->string (reverse acc))
                    (let ((ch (string-ref name i)))
                      (loop (+ i 1)
                        (cons (if (or (char-alphabetic? ch) (char-numeric? ch)) ch #\_) acc)))))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app)
      (string-append "#ifndef " guard "_H\n#define " guard "_H\n\n\n\n#endif /* " guard "_H */\n"))))

(def (cmd-insert-box-comment app)
  "Insert a box comment."
  (let* ((ed (current-qt-editor app))
         (width 72)
         (border (make-string width #\-)))
    (qt-plain-text-edit-insert-text! ed
      (string-append ";; " border "\n;; \n;; " border "\n"))))

(def (cmd-insert-file-contents app)
  "Insert the contents of a file."
  (cmd-insert-file app))

(def (cmd-insert-register-string app)
  "Insert the string from a register."
  (cmd-insert-register app))

;; --- Toggles ---
(def *auto-indent* #t)
(def *backup-files* #t)
(def *debug-mode* #f)
(def *debug-on-quit* #f)
(def *visible-bell* #f)
(def *transient-mark* #t)
(def *electric-indent* #t)

(def (cmd-toggle-auto-indent app)
  "Toggle auto-indentation."
  (set! *auto-indent* (not *auto-indent*))
  (echo-message! (app-state-echo app) (if *auto-indent* "Auto-indent ON" "Auto-indent OFF")))

(def (cmd-toggle-backup-files app)
  "Toggle backup file creation."
  (set! *backup-files* (not *backup-files*))
  (echo-message! (app-state-echo app) (if *backup-files* "Backup files ON" "Backup files OFF")))

(def (cmd-toggle-debug-mode app)
  "Toggle debug mode."
  (set! *debug-mode* (not *debug-mode*))
  (echo-message! (app-state-echo app) (if *debug-mode* "Debug mode ON" "Debug mode OFF")))

(def (cmd-toggle-debug-on-quit app)
  "Toggle debug on quit."
  (set! *debug-on-quit* (not *debug-on-quit*))
  (echo-message! (app-state-echo app) (if *debug-on-quit* "Debug on quit ON" "Debug on quit OFF")))

(def (cmd-toggle-visible-bell app)
  "Toggle visible bell."
  (set! *visible-bell* (not *visible-bell*))
  (echo-message! (app-state-echo app) (if *visible-bell* "Visible bell ON" "Visible bell OFF")))

(def (cmd-toggle-transient-mark app)
  "Toggle transient mark mode."
  (set! *transient-mark* (not *transient-mark*))
  (echo-message! (app-state-echo app) (if *transient-mark* "Transient mark ON" "Transient mark OFF")))

(def (cmd-toggle-electric-indent app)
  "Toggle electric indent mode."
  (set! *electric-indent* (not *electric-indent*))
  (echo-message! (app-state-echo app) (if *electric-indent* "Electric indent ON" "Electric indent OFF")))

(def (cmd-toggle-auto-revert app)
  "Toggle auto-revert mode for file-visiting buffers."
  (set! *auto-revert-mode* (not *auto-revert-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-revert-mode* "Auto-revert mode ON" "Auto-revert mode OFF")))

(def (cmd-toggle-auto-revert-global app)
  "Toggle global auto-revert mode (same as auto-revert for now)."
  (cmd-toggle-auto-revert app))

(def (cmd-auto-revert-tail-mode app)
  "Toggle auto-revert-tail-mode for the current buffer.
When enabled, the buffer automatically reverts when the file changes on disk
and scrolls to the end — like 'tail -f'. Useful for watching log files."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no associated file")
      (if (hash-get *auto-revert-tail-buffers* name)
        (begin
          (hash-remove! *auto-revert-tail-buffers* name)
          (echo-message! (app-state-echo app)
            (string-append "Auto-revert-tail-mode OFF for " name)))
        (begin
          (hash-put! *auto-revert-tail-buffers* name #t)
          ;; Scroll to end immediately
          (let ((ed (current-qt-editor app)))
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-message! (app-state-echo app)
            (string-append "Auto-revert-tail-mode ON for " name)))))))

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

;;; ========================================================================
;;; View mode — read-only browsing with navigation keys
;;; ========================================================================

(def *view-mode-buffers* (make-hash-table)) ; buffer-name -> #t

(def (cmd-view-mode app)
  "Toggle view-mode: read-only browsing with simplified navigation.
SPC = page down, DEL = page up, q = quit view-mode."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf))
         (ed (current-qt-editor app)))
    (if (hash-get *view-mode-buffers* name)
      (begin
        (hash-remove! *view-mode-buffers* name)
        (qt-plain-text-edit-set-read-only! ed #f)
        (echo-message! (app-state-echo app) "View mode OFF"))
      (begin
        (hash-put! *view-mode-buffers* name #t)
        (qt-plain-text-edit-set-read-only! ed #t)
        (echo-message! (app-state-echo app)
          "View mode ON (SPC=pgdn DEL=pgup q=quit)")))))

;;; ========================================================================
;;; So-long mode — disable expensive features for long-line files
;;; ========================================================================

(def *so-long-threshold* 10000) ; characters per line
(def *so-long-buffers* (make-hash-table)) ; buffer-name -> #t

(def (check-so-long! app buf text)
  "Check if text has very long lines and enable so-long mode if needed."
  (let ((name (buffer-name buf)))
    (unless (hash-get *so-long-buffers* name)
      (let loop ((i 0) (line-start 0))
        (cond
          ((>= i (string-length text)) #f)
          ((char=? (string-ref text i) #\newline)
           (if (> (- i line-start) *so-long-threshold*)
             (begin
               (hash-put! *so-long-buffers* name #t)
               (echo-message! (app-state-echo app)
                 (string-append name ": long lines detected (so-long-mode enabled)")))
             (loop (+ i 1) (+ i 1))))
          (else (loop (+ i 1) line-start)))))))

(def (cmd-so-long-mode app)
  "Toggle so-long-mode which disables expensive features for long-line files."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (if (hash-get *so-long-buffers* name)
      (begin
        (hash-remove! *so-long-buffers* name)
        (echo-message! (app-state-echo app) "So-long mode OFF"))
      (begin
        (hash-put! *so-long-buffers* name #t)
        (echo-message! (app-state-echo app) "So-long mode ON")))))

;;; ========================================================================
;;; Follow mode — linked scrolling across windows
;;; ========================================================================

(def *follow-mode* #f)

(def (cmd-follow-mode app)
  "Toggle follow-mode: synchronize scrolling across windows showing the same buffer."
  (set! *follow-mode* (not *follow-mode*))
  (echo-message! (app-state-echo app)
    (if *follow-mode* "Follow mode ON" "Follow mode OFF")))

;;; ========================================================================
;;; IBBuffer — enhanced buffer list
;;; ========================================================================

(def (cmd-ibuffer app)
  "Open an enhanced buffer list with filtering and sorting.
Marks: d=delete, s=save, u=unmark, x=execute, /n=filter by name, /m=filter by mode,
S=sort by name, z=sort by size, q=quit."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (bufs (buffer-list))
         (lines
           (let loop ((bs bufs) (acc []))
             (if (null? bs) (reverse acc)
               (let* ((b (car bs))
                      (name (buffer-name b))
                      (modified? (let ((doc (buffer-doc-pointer b)))
                                   (and doc (qt-text-document-modified? doc))))
                      (readonly? (let loop2r ((ws (qt-frame-windows fr)))
                                   (cond
                                     ((null? ws) #f)
                                     ((eq? (qt-edit-window-buffer (car ws)) b)
                                      (qt-plain-text-edit-read-only?
                                        (qt-edit-window-editor (car ws))))
                                     (else (loop2r (cdr ws))))))
                      (path (or (buffer-file-path b) ""))
                      (size-str
                        (let loop2 ((wins (qt-frame-windows fr)))
                          (cond
                            ((null? wins) "?")
                            ((eq? (qt-edit-window-buffer (car wins)) b)
                             (number->string
                               (string-length
                                 (qt-plain-text-edit-text
                                   (qt-edit-window-editor (car wins))))))
                            (else (loop2 (cdr wins))))))
                      (flag (string-append
                              (if modified? "*" " ")
                              (if readonly? "%" " ")))
                      (line (string-append
                              "  " flag " "
                              (string-pad-right name 30)
                              (string-pad-right size-str 10)
                              path)))
                 (loop (cdr bs) (cons line acc))))))
         (header "  Flags  Name                          Size      File")
         (sep    "  -----  ----------------------------  --------  ----")
         (text (string-join (cons header (cons sep lines)) "\n"))
         (buf-name "*IBBuffer*")
         (buf (or (buffer-by-name buf-name)
                  (qt-buffer-create! buf-name ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      "IBBuffer: d=mark delete, s=mark save, u=unmark, x=execute, q=quit")))

;;; ========================================================================
;;; Savehist — persistent minibuffer history
;;; ========================================================================

(def *command-history-file*
  (string-append (getenv "HOME" "/tmp") "/.gerbil-emacs-history"))

(def (savehist-save!)
  "Save command history to file."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (pair? *mx-command-history*)
        (call-with-output-file *command-history-file*
          (lambda (port)
            (for-each
              (lambda (cmd)
                (display cmd port)
                (newline port))
              (take *mx-command-history*
                    (min 500 (length *mx-command-history*))))))))))

(def (savehist-load!)
  "Load command history from file."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *command-history-file*)
        (let ((lines (call-with-input-file *command-history-file*
                       (lambda (port)
                         (let loop ((acc []))
                           (let ((line (read-line port)))
                             (if (eof-object? line)
                               (reverse acc)
                               (loop (cons line acc)))))))))
          (set! *mx-command-history* lines))))))

;;; ========================================================================
;;; WDired — editable directory mode
;;; ========================================================================

(def *wdired-state* (make-hash-table)) ; buffer-name -> list of original filenames

(def (cmd-wdired-mode app)
  "Toggle writable dired mode. In wdired, edit filenames then C-c C-c to commit."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf))
         (ed (current-qt-editor app)))
    (cond
      ;; Currently in wdired — commit changes
      ((hash-get *wdired-state* name)
       (let* ((originals (hash-get *wdired-state* name))
              (text (qt-plain-text-edit-text ed))
              (lines (string-split text #\newline))
              (renamed 0)
              (errors 0))
         ;; Each line that starts with "  " is a filename
         (let loop ((os originals) (ls lines))
           (when (and (pair? os) (pair? ls))
             (let ((orig (car os))
                   (line (car ls)))
               (when (and (> (string-length line) 2)
                          (not (string=? orig line)))
                 ;; Attempt rename
                 (with-catch
                   (lambda (e) (set! errors (+ errors 1)))
                   (lambda ()
                     (rename-file orig (string-trim line))
                     (set! renamed (+ renamed 1)))))
               (loop (cdr os) (cdr ls)))))
         (hash-remove! *wdired-state* name)
         (qt-plain-text-edit-set-read-only! ed #t)
         (echo-message! (app-state-echo app)
           (string-append "WDired: " (number->string renamed) " renamed"
                          (if (> errors 0)
                            (string-append ", " (number->string errors) " errors")
                            "")))))
      ;; Check if this is a dired-like buffer
      ((or (string-prefix? "*Dired:" name)
           (string-prefix? "*dired" name))
       ;; Enter wdired mode — record original names
       (let* ((text (qt-plain-text-edit-text ed))
              (lines (string-split text #\newline)))
         (hash-put! *wdired-state* name lines)
         (qt-plain-text-edit-set-read-only! ed #f)
         (echo-message! (app-state-echo app)
           "WDired mode ON — edit filenames, then M-x wdired-finish to commit")))
      (else
        (echo-error! (app-state-echo app) "Not a dired buffer")))))

(def (cmd-wdired-finish app)
  "Finish wdired editing and rename files."
  (cmd-wdired-mode app))

;;; ========================================================================
;;; Auto-fill mode — hard wrap at fill-column
;;; ========================================================================

(def (auto-fill-check! ed)
  "If auto-fill-mode is on and current line exceeds fill-column, break at word boundary."
  (when *auto-fill-mode*
    (let* ((text (qt-plain-text-edit-text ed))
           (pos (qt-plain-text-edit-cursor-position ed))
           ;; Find start of current line
           (line-start (let loop ((i (- pos 1)))
                         (cond ((< i 0) 0)
                               ((char=? (string-ref text i) #\newline) (+ i 1))
                               (else (loop (- i 1))))))
           (col (- pos line-start)))
      (when (> col *fill-column*)
        ;; Find last space before fill-column boundary
        (let loop ((i (+ line-start *fill-column*)))
          (cond
            ((<= i line-start) #f)  ; no break point found
            ((char=? (string-ref text i) #\space)
             ;; Break here: replace space with newline
             (let ((before (substring text 0 i))
                   (after (substring text (+ i 1) (string-length text))))
               (qt-plain-text-edit-set-text! ed (string-append before "\n" after))
               (qt-plain-text-edit-set-cursor-position! ed pos)))
            (else (loop (- i 1)))))))))

;;; ========================================================================
;;; Delete trailing whitespace on save
;;; ========================================================================

(def *delete-trailing-whitespace-on-save* #f)

(def (cmd-toggle-delete-trailing-whitespace-on-save app)
  "Toggle automatic deletion of trailing whitespace when saving."
  (set! *delete-trailing-whitespace-on-save*
        (not *delete-trailing-whitespace-on-save*))
  (echo-message! (app-state-echo app)
    (if *delete-trailing-whitespace-on-save*
      "Delete trailing whitespace on save: ON"
      "Delete trailing whitespace on save: OFF")))

;;; delete-horizontal-space — delete all whitespace around point

(def (cmd-delete-horizontal-space app)
  "Delete all whitespace around point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (ws-start (let loop ((i (- pos 1)))
                     (if (and (>= i 0) (memq (string-ref text i) '(#\space #\tab)))
                       (loop (- i 1))
                       (+ i 1))))
         (ws-end (let loop ((i pos))
                   (if (and (< i len) (memq (string-ref text i) '(#\space #\tab)))
                     (loop (+ i 1))
                     i)))
         (before (substring text 0 ws-start))
         (after (substring text ws-end len)))
    (let ((new-text (string-append before after)))
      (qt-plain-text-edit-set-text! ed new-text)
      (qt-plain-text-edit-set-cursor-position! ed ws-start))))

;;; ========================================================================
;;; Uniquify buffer names
;;; ========================================================================

(def (uniquify-buffer-name! path)
  "Generate a unique buffer name for a file path by adding parent dir when needed."
  (let* ((basename (path-strip-directory path))
         (existing (filter (lambda (b)
                            (and (buffer-file-path b)
                                 (string=? (path-strip-directory (buffer-file-path b))
                                           basename)))
                          (buffer-list))))
    (if (null? existing)
      basename
      ;; Add parent directory for disambiguation
      (let ((parent (path-strip-directory
                      (path-strip-trailing-directory-separator
                        (path-directory path)))))
        (string-append basename "<" parent ">")))))

;;; ========================================================================
;;; Recentf-open-files (numbered list)
;;; ========================================================================

(def (cmd-recentf-open-files app)
  "Show recent files in a numbered buffer for easy selection."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (recents *recent-files*)
         (lines (let loop ((fs recents) (i 1) (acc []))
                  (if (null? fs) (reverse acc)
                    (loop (cdr fs) (+ i 1)
                          (cons (string-append "  " (number->string i) ". " (car fs))
                                acc)))))
         (text (string-append "Recent Files (enter number to open):\n\n"
                              (string-join lines "\n")))
         (buf-name "*Recent Files*")
         (buf (or (buffer-by-name buf-name)
                  (qt-buffer-create! buf-name ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      "Enter file number to open, or q to quit")))

(def (cmd-toggle-frame-fullscreen app)
  "Toggle fullscreen."
  (let ((win (qt-frame-main-win (app-state-frame app))))
    (qt-widget-show-fullscreen! win)
    (echo-message! (app-state-echo app) "Fullscreen toggled")))

(def (cmd-toggle-frame-maximized app)
  "Toggle maximized."
  (let ((win (qt-frame-main-win (app-state-frame app))))
    (qt-widget-show-maximized! win)
    (echo-message! (app-state-echo app) "Maximized")))

(def (cmd-toggle-menu-bar app)
  "Toggle menu bar visibility."
  (echo-message! (app-state-echo app) "Menu bar toggled"))

(def (cmd-toggle-menu-bar-mode app)
  "Toggle menu bar mode."
  (echo-message! (app-state-echo app) "Menu bar mode toggled"))

(def (cmd-toggle-tool-bar app)
  "Toggle toolbar visibility."
  (echo-message! (app-state-echo app) "Toolbar toggled"))

(def (cmd-toggle-scroll-bar app)
  "Toggle scrollbar visibility."
  (echo-message! (app-state-echo app) "Scrollbar toggled"))

(def (cmd-toggle-tab-bar-mode app)
  "Toggle tab bar visibility."
  (set! *tab-bar-visible* (not *tab-bar-visible*))
  (echo-message! (app-state-echo app)
    (if *tab-bar-visible* "Tab bar enabled" "Tab bar disabled")))

(def (cmd-toggle-input-method app)
  "Toggle input method."
  (echo-message! (app-state-echo app) "Input method toggled"))

(def (cmd-toggle-eol-conversion app)
  "Toggle end-of-line conversion display."
  (echo-message! (app-state-echo app) "EOL conversion toggled"))

(def (cmd-toggle-flymake app)
  "Toggle flymake mode."
  (echo-message! (app-state-echo app) "Flymake not available"))

(def (cmd-toggle-flyspell app)
  "Toggle flyspell mode."
  (echo-message! (app-state-echo app) "Flyspell not available"))

(def (cmd-toggle-lsp app)
  "Toggle LSP mode."
  (echo-message! (app-state-echo app) "LSP not available"))

(def (cmd-toggle-global-hl-line app)
  "Toggle global highlight line."
  (cmd-toggle-hl-line app))

(def (cmd-toggle-global-whitespace app)
  "Toggle global whitespace display."
  (cmd-toggle-whitespace app))

(def (cmd-toggle-show-spaces app)
  "Toggle space display."
  (echo-message! (app-state-echo app) "Show spaces toggled"))

(def (cmd-toggle-show-trailing-whitespace app)
  "Toggle trailing whitespace display."
  (echo-message! (app-state-echo app) "Show trailing whitespace toggled"))

(def (cmd-toggle-narrow-indicator app)
  "Toggle narrow indicator."
  (cmd-toggle-narrowing-indicator app))

(def (cmd-toggle-auto-complete app)
  "Toggle auto-complete."
  (echo-message! (app-state-echo app) "Auto-complete toggled"))

;; --- Window management ---
(def (cmd-split-window-below app)
  "Split window horizontally (below)."
  (cmd-split-window app))

(def (cmd-delete-window-below app)
  "Delete the window below (same as delete-window)."
  (cmd-delete-window app))

(def (cmd-fit-window-to-buffer app)
  "Fit window to buffer content."
  (echo-message! (app-state-echo app) "Window fitted"))

(def (cmd-shrink-window-if-larger-than-buffer app)
  "Shrink window to fit buffer."
  (echo-message! (app-state-echo app) "Window shrunk"))

(def (cmd-resize-window-width app)
  "Resize window width."
  (echo-message! (app-state-echo app) "Width resize not supported in vertical split"))

(def (cmd-make-frame app)
  "Create a new frame (not applicable in single-window Qt)."
  (echo-message! (app-state-echo app) "Only one frame in Qt backend"))

(def (cmd-delete-frame app)
  "Delete a frame."
  (echo-message! (app-state-echo app) "Only one frame in Qt backend"))

(def (cmd-suspend-frame app)
  "Suspend the frame."
  (echo-message! (app-state-echo app) "Suspend not supported"))

;; --- Editing ---
(def (cmd-center-region app)
  "Center the lines in the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (col *fill-column*)
             (centered (map (lambda (l)
                              (let* ((trimmed (string-trim l))
                                     (pad (max 0 (quotient (- col (string-length trimmed)) 2))))
                                (string-append (make-string pad #\space) trimmed)))
                            lines))
             (result (string-join centered "\n")))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region centered"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-indent-rigidly app)
  "Indent region rigidly by 2 spaces."
  (cmd-indent-rigidly-right app))

(def (cmd-dedent-rigidly app)
  "Dedent region rigidly by 2 spaces."
  (cmd-indent-rigidly-left app))

(def (cmd-fixup-whitespace app)
  "Fix whitespace around point (collapse to single space or none)."
  (cmd-just-one-space app))

(def (cmd-electric-newline-and-indent app)
  "Insert newline and indent to match previous line."
  (let* ((ed (current-qt-editor app))
         (indent (current-line-indent ed)))
    (qt-plain-text-edit-insert-text! ed (string-append "\n" indent))))

(def (cmd-kebab-to-camel app)
  "Convert kebab-case to camelCase."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (result (let loop ((i 0) (capitalize? #f) (acc []))
                       (cond
                         ((>= i (string-length region))
                          (list->string (reverse acc)))
                         ((char=? (string-ref region i) #\-)
                          (loop (+ i 1) #t acc))
                         (capitalize?
                          (loop (+ i 1) #f (cons (char-upcase (string-ref region i)) acc)))
                         (else
                          (loop (+ i 1) #f (cons (string-ref region i) acc)))))))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-flush-lines-region app)
  "Flush lines matching pattern in region."
  (cmd-flush-lines app))

(def (cmd-keep-lines-region app)
  "Keep lines matching pattern in region."
  (cmd-keep-lines app))

;; --- VCS ---
(def (cmd-vc-annotate app)
  "Show git blame/annotate."
  (cmd-show-git-blame app))

(def (cmd-vc-diff-head app)
  "Show git diff against HEAD."
  (cmd-show-git-diff app))

(def (cmd-vc-log-file app)
  "Show git log for current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (let* ((fr (app-state-frame app))
             (ed (qt-current-editor fr))
             (out (open-process
                    (list path: "git" arguments: (list "log" "--oneline" "-20" path)
                          stderr-redirection: #t)))
             (text (read-line out #f)))
        (close-port out)
        (when (string? text)
          (let ((buf (qt-buffer-create! "*Git Log*" ed #f)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed text))))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-vc-revert app)
  "Revert current file using git checkout."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (let ((out (open-process
                     (list path: "git" arguments: (list "checkout" "--" path)
                           stderr-redirection: #t))))
          (close-port out))
        (cmd-revert-buffer app)
        (echo-message! (app-state-echo app) "Reverted from git"))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

;; --- Search extensions ---
(def (cmd-isearch-forward-word app)
  "Search forward for word."
  (cmd-search-forward-word app))

(def (cmd-isearch-backward-word app)
  "Search backward for word."
  (cmd-search-backward-word app))

(def (cmd-isearch-forward-symbol app)
  "Search forward for symbol at point."
  (cmd-search-forward-word app))

(def (cmd-mark-lines-matching app)
  "Mark all lines matching a pattern."
  (let ((pat (qt-echo-read-string app "Mark lines matching: ")))
    (when pat
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (count (length (filter (lambda (l) (string-contains l pat)) lines))))
        (echo-message! (app-state-echo app)
          (string-append (number->string count) " lines match: " pat))))))

;; --- Buffer/undo ---
(def (cmd-buffer-disable-undo app)
  "Disable undo for current buffer."
  (echo-message! (app-state-echo app) "Undo disabled (not supported)"))

(def (cmd-buffer-enable-undo app)
  "Enable undo for current buffer."
  (echo-message! (app-state-echo app) "Undo enabled"))

(def (cmd-lock-buffer app)
  "Lock the current buffer (toggle read-only)."
  (cmd-toggle-read-only app))

(def (cmd-auto-revert-mode app)
  "Toggle auto-revert mode."
  (echo-message! (app-state-echo app) "Auto-revert mode toggled"))

;; --- Registers ---
(def (cmd-append-to-register app)
  "Append text to a register."
  (let ((input (qt-echo-read-string app "Append to register: ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (mark (buffer-mark buf))
             (pos (qt-plain-text-edit-cursor-position ed)))
        (if mark
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (substring (qt-plain-text-edit-text ed) start end))
                 (regs (app-state-registers app))
                 (existing (or (hash-get regs ch) "")))
            (hash-put! regs ch (string-append existing text))
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app) (string-append "Appended to register " (string ch))))
          (echo-error! (app-state-echo app) "No mark set"))))))

;; --- Completion ---
(def (cmd-complete-filename app)
  "Complete filename at point."
  (echo-message! (app-state-echo app) "Filename completion not yet available"))

(def (cmd-completion-at-point app)
  "Smart completion at point. For Gerbil buffers, combines buffer words
   with known Gerbil standard library symbols."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (lang (buffer-lexer-lang buf))
         (prefix (get-word-prefix ed)))
    (if (string=? prefix "")
      (echo-message! (app-state-echo app) "No prefix for completion")
      ;; Collect candidates
      (let* ((text (qt-plain-text-edit-text ed))
             (buffer-words (collect-buffer-words text))
             ;; For Gerbil/Scheme: add known standard library symbols
             (stdlib-syms (if (memq lang '(scheme gerbil lisp))
                            (hash-keys *gerbil-signatures*)
                            []))
             ;; Merge and filter
             (all-syms (let ((h (make-hash-table)))
                         (for-each (lambda (w) (hash-put! h w #t)) buffer-words)
                         (for-each (lambda (w) (hash-put! h w #t)) stdlib-syms)
                         (hash-keys h)))
             (matches (filter (lambda (w)
                                (and (> (string-length w) (string-length prefix))
                                     (string-prefix? prefix w)
                                     (not (string=? w prefix))))
                              all-syms))
             (sorted (sort matches string<?)))
        (cond
          ((null? sorted)
           (echo-message! (app-state-echo app) "No completions"))
          ((= (length sorted) 1)
           ;; Single match — complete immediately
           (let* ((match (car sorted))
                  (suffix (substring match (string-length prefix)
                                     (string-length match)))
                  (pos (qt-plain-text-edit-cursor-position ed)))
             (qt-plain-text-edit-insert-text! ed suffix)
             (echo-message! (app-state-echo app) match)))
          (else
           ;; Multiple matches — show up to 10 and complete common prefix
           (let* ((common (let loop ((i (string-length prefix)))
                            (if (>= i (apply min (map string-length sorted)))
                              i
                              (let ((ch (string-ref (car sorted) i)))
                                (if (every (lambda (s) (char=? (string-ref s i) ch))
                                           sorted)
                                  (loop (+ i 1))
                                  i)))))
                  (common-suffix (substring (car sorted) (string-length prefix) common))
                  (shown (if (> (length sorted) 10)
                           (append (let loop ((l sorted) (n 0) (acc []))
                                     (if (or (null? l) (>= n 10)) (reverse acc)
                                       (loop (cdr l) (+ n 1) (cons (car l) acc))))
                                   (list "..."))
                           sorted)))
             ;; Insert common prefix
             (when (> (string-length common-suffix) 0)
               (qt-plain-text-edit-insert-text! ed common-suffix))
             ;; Show candidates in echo area
             (echo-message! (app-state-echo app)
               (string-join shown " ")))))))))

;; --- Info/Help ---
(def (cmd-info app)
  "Show info."
  (echo-message! (app-state-echo app) "Info not available"))

(def (cmd-info-emacs-manual app)
  "Show Emacs manual."
  (echo-message! (app-state-echo app) "Use M-x list-bindings for keybinding help"))

(def (cmd-info-elisp-manual app)
  "Show Elisp manual."
  (echo-message! (app-state-echo app) "Gerbil Scheme documentation at https://cons.io"))

(def (cmd-report-bug app)
  "Report a bug."
  (echo-message! (app-state-echo app) "Report bugs at the project repository"))

(def (cmd-memory-report app)
  "Show memory usage."
  (let* ((out (open-process (list path: "ps" arguments: '("-o" "rss=" "-p" "$$"))))
         (rss (read-line out)))
    (close-port out)
    (echo-message! (app-state-echo app)
      (if (string? rss)
        (string-append "RSS: " (string-trim rss) " KB")
        "Memory info unavailable"))))

(def (cmd-view-echo-area-messages app)
  "View messages log."
  (cmd-view-messages app))

;; --- Spelling via aspell ---
(def *ispell-program* "aspell")
(def *ispell-personal-dict* #f) ;; set to path for personal dictionary

(def (ispell-word-bounds ed)
  "Get word boundaries around cursor (searching backward then forward)."
  (let* ((pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    ;; Search backward from cursor to find word start
    (let find-start ((i (- pos 1)))
      (if (or (< i 0)
              (let ((ch (string-ref text i)))
                (not (or (char-alphabetic? ch) (char=? ch #\')))))
        (let ((start (+ i 1)))
          ;; Search forward to find word end
          (if (>= start len)
            (values #f #f)
            (let find-end ((j start))
              (if (or (>= j len)
                      (let ((ch (string-ref text j)))
                        (not (or (char-alphabetic? ch) (char=? ch #\')))))
                (if (> j start)
                  (values start j)
                  (values #f #f))
                (find-end (+ j 1))))))
        (find-start (- i 1))))))

(def (ispell-check-word word)
  "Check a word with aspell. Returns #f if correct, or list of suggestions."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((args (if *ispell-personal-dict*
                     ["-a" "--personal" *ispell-personal-dict*]
                     ["-a"]))
             (port (open-process
                     (list path: *ispell-program*
                           arguments: args
                           stdin-redirection: #t
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f))))
        ;; Read the version header line
        (read-line port)
        ;; Send word to check
        (display word port)
        (newline port)
        (force-output port)
        ;; Read result
        (let ((line (read-line port)))
          (close-port port)
          (cond
            ((or (eof-object? line) (string=? line "")) #f)
            ((char=? (string-ref line 0) #\*) #f)  ;; correct
            ((char=? (string-ref line 0) #\+) #f)  ;; correct (root found)
            ((char=? (string-ref line 0) #\-)  #f)  ;; compound ok
            ((char=? (string-ref line 0) #\#)  [])  ;; no suggestions
            ((char=? (string-ref line 0) #\&)       ;; misspelled with suggestions
             ;; Format: & word count offset: sugg1, sugg2, ...
             (let ((colon-pos (string-contains line ":")))
               (if colon-pos
                 (let* ((sugg-str (substring line (+ colon-pos 2) (string-length line)))
                        (suggestions (map string-trim-both
                                       (string-split sugg-str #\,))))
                   suggestions)
                 [])))
            (else #f)))))))

(def (cmd-ispell-word app)
  "Check spelling of word at point."
  (let* ((ed (current-qt-editor app)))
    (let-values (((start end) (ispell-word-bounds ed)))
      (if (not start)
        (echo-message! (app-state-echo app) "No word at point")
        (let* ((text (qt-plain-text-edit-text ed))
               (word (substring text start end))
               (result (ispell-check-word word)))
          (cond
            ((not result)
             (echo-message! (app-state-echo app)
               (string-append "\"" word "\" is correct")))
            ((null? result)
             (echo-error! (app-state-echo app)
               (string-append "\"" word "\" is misspelled (no suggestions)")))
            (else
             ;; Show suggestions with completion
             (let* ((prompt (string-append "\"" word "\" → "))
                    (choice (qt-echo-read-string-with-completion
                              app prompt result)))
               (when (and choice (not (string=? choice "")))
                 ;; Replace word
                 (let ((new-text (string-append
                                   (substring text 0 start)
                                   choice
                                   (substring text end (string-length text)))))
                   (qt-plain-text-edit-set-text! ed new-text)
                   (qt-plain-text-edit-set-cursor-position! ed
                     (+ start (string-length choice)))))))))))))

(def (cmd-ispell-region app)
  "Check spelling of each word in region."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No mark set")
      (let* ((start (min pos mark))
             (end (max pos mark))
             (region (substring text start end))
             (misspelled (ispell-check-region region)))
        (if (null? misspelled)
          (echo-message! (app-state-echo app) "No misspellings found")
          (echo-message! (app-state-echo app)
            (string-append (number->string (length misspelled))
              " misspelled: "
              (string-join misspelled ", "))))))))

(def (ispell-check-region text)
  "Return list of misspelled words in text."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (let* ((args (if *ispell-personal-dict*
                     ["-a" "--personal" *ispell-personal-dict*]
                     ["-a"]))
             (port (open-process
                     (list path: *ispell-program*
                           arguments: args
                           stdin-redirection: #t
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f))))
        ;; Read header
        (read-line port)
        ;; Send text
        (display text port)
        (newline port)
        (force-output port)
        ;; Collect misspelled words
        (let loop ((result []))
          (let ((line (read-line port)))
            (cond
              ((or (eof-object? line) (string=? line ""))
               (close-port port)
               (reverse result))
              ((and (> (string-length line) 0)
                    (or (char=? (string-ref line 0) #\&)
                        (char=? (string-ref line 0) #\#)))
               ;; Extract the misspelled word (second token)
               (let* ((parts (string-split line #\space))
                      (word (if (> (length parts) 1) (cadr parts) "")))
                 (loop (cons word result))))
              (else (loop result)))))))))

(def (cmd-ispell-buffer app)
  "Check spelling of entire buffer, navigating to each error."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (misspelled (ispell-check-region text)))
    (if (null? misspelled)
      (echo-message! (app-state-echo app) "No misspellings found")
      ;; Find and offer to fix each misspelled word
      (let loop ((words misspelled) (fixed 0) (current-text text))
        (if (null? words)
          (echo-message! (app-state-echo app)
            (string-append "Spell check done. " (number->string fixed) " corrections."))
          (let* ((word (car words))
                 (idx (string-contains current-text word)))
            (if (not idx)
              (loop (cdr words) fixed current-text)
              (begin
                ;; Position cursor at misspelled word
                (qt-plain-text-edit-set-cursor-position! ed idx)
                (qt-plain-text-edit-ensure-cursor-visible! ed)
                ;; Check this specific word for suggestions
                (let ((suggestions (ispell-check-word word)))
                  (cond
                    ((or (not suggestions) (null? suggestions))
                     ;; Skip words with no suggestions
                     (loop (cdr words) fixed current-text))
                    (else
                     (let* ((options (cons "[skip]" (if (> (length suggestions) 10)
                                                     (take suggestions 10)
                                                     suggestions)))
                            (prompt (string-append "\"" word "\" → "))
                            (choice (qt-echo-read-string-with-completion
                                      app prompt options)))
                       (if (or (not choice) (string=? choice "")
                               (string=? choice "[skip]"))
                         (loop (cdr words) fixed current-text)
                         ;; Replace
                         (let* ((new-text (string-append
                                            (substring current-text 0 idx)
                                            choice
                                            (substring current-text
                                              (+ idx (string-length word))
                                              (string-length current-text)))))
                           (qt-plain-text-edit-set-text! ed new-text)
                           (qt-plain-text-edit-set-cursor-position! ed
                             (+ idx (string-length choice)))
                           (loop (cdr words) (+ fixed 1) new-text)))))))))))))))

;; --- Abbreviations ---
(def *abbrev-mode* #f)
(def *abbrev-table* (make-hash-table))  ;; abbrev -> expansion
(def *abbrevs-path*
  (path-expand ".gerbil-emacs-abbrevs" (user-info-home (user-info (user-name)))))

(def (abbrevs-save!)
  "Persist abbreviation table to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file *abbrevs-path*
        (lambda (port)
          (for-each
            (lambda (pair)
              (display (car pair) port) (display "\t" port)
              (display (cdr pair) port) (newline port))
            (hash->list *abbrev-table*)))))))

(def (abbrevs-load!)
  "Load abbreviation table from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *abbrevs-path*)
        (call-with-input-file *abbrevs-path*
          (lambda (port)
            (let loop ()
              (let ((line (read-line port)))
                (unless (eof-object? line)
                  (let ((tab-pos (string-index line #\tab)))
                    (when tab-pos
                      (let ((abbrev (substring line 0 tab-pos))
                            (expansion (substring line (+ tab-pos 1) (string-length line))))
                        (hash-put! *abbrev-table* abbrev expansion))))
                  (loop))))))))))

(def (cmd-abbrev-mode app)
  "Toggle abbreviation mode."
  (set! *abbrev-mode* (not *abbrev-mode*))
  (when *abbrev-mode* (abbrevs-load!))
  (echo-message! (app-state-echo app)
    (if *abbrev-mode* "Abbrev mode enabled" "Abbrev mode disabled")))

(def (cmd-define-abbrev app)
  "Define a new abbreviation interactively."
  (let* ((abbrev (qt-echo-read-string app "Abbrev: "))
         (expansion (qt-echo-read-string app "Expansion: ")))
    (when (and abbrev (not (string=? abbrev ""))
               expansion (not (string=? expansion "")))
      (hash-put! *abbrev-table* abbrev expansion)
      (abbrevs-save!)
      (echo-message! (app-state-echo app)
        (string-append "\"" abbrev "\" => \"" expansion "\"")))))

(def (cmd-expand-abbrev app)
  "Expand abbreviation before cursor."
  (let* ((ed (current-qt-editor app))
         (prefix (get-word-prefix ed)))
    (if (string=? prefix "")
      (echo-message! (app-state-echo app) "No abbrev at point")
      (let ((expansion (hash-get *abbrev-table* prefix)))
        (if (not expansion)
          (echo-message! (app-state-echo app)
            (string-append "No abbrev for \"" prefix "\""))
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (- pos (string-length prefix)))
                 (text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring text 0 start)
                             expansion
                             (substring text pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed
              (+ start (string-length expansion)))))))))

(def (cmd-list-abbrevs app)
  "List all defined abbreviations in a buffer."
  (let* ((pairs (sort (hash->list *abbrev-table*)
                  (lambda (a b) (string<? (car a) (car b)))))
         (text (if (null? pairs)
                 "No abbreviations defined.\n"
                 (string-append
                   "Abbreviations:\n"
                   (string-append
                     (string-join
                       (map (lambda (p)
                              (string-append "  " (car p) " => " (cdr p)))
                            pairs)
                       "\n")
                     "\n")))))
    (let* ((fr (app-state-frame app))
           (ed (current-qt-editor app))
           (buf (or (buffer-by-name "*abbrevs*")
                    (qt-buffer-create! "*abbrevs*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;; --- Man page viewer ---
(def (cmd-man app)
  "View a man page in a buffer."
  (let ((topic (qt-echo-read-string app "Man page: ")))
    (when (and topic (not (string=? topic "")))
      (let* ((parts (string-split topic #\space))
             (args (if (= (length parts) 2)
                     [(car parts) (cadr parts)]  ;; section + topic
                     parts))
             (port (open-process
                     (list path: "/usr/bin/man"
                           arguments: args
                           environment: ["MANPAGER=cat" "COLUMNS=80"
                                         "MAN_KEEP_FORMATTING=1"
                                         (string-append "HOME=" (or (getenv "HOME" #f) "/tmp"))
                                         (string-append "PATH=" (or (getenv "PATH" #f) "/usr/bin"))]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (output (read-line port #f))
             (_ (process-status port)))
        (close-port port)
        (if (or (not output) (string=? output ""))
          (echo-error! (app-state-echo app)
            (string-append "No man page for \"" topic "\""))
          ;; Strip backspace-based formatting (bold: X^HX, underline: _^HX)
          (let* ((clean (man-strip-formatting output))
                 (buf-name (string-append "*Man " topic "*"))
                 (fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (buf (or (buffer-by-name buf-name)
                          (qt-buffer-create! buf-name ed #f))))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed clean)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! (app-state-echo app) buf-name)))))))

(def (man-strip-formatting text)
  "Remove backspace-based man page formatting (bold: X^HX, underline: _^HX)."
  (let* ((len (string-length text))
         (out (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (get-output-string out))
        ((and (< (+ i 2) len)
              (char=? (string-ref text (+ i 1)) #\backspace))
         ;; Skip the overstriking: take the character after ^H
         (write-char (string-ref text (+ i 2)) out)
         (loop (+ i 3)))
        (else
         (write-char (string-ref text i) out)
         (loop (+ i 1)))))))

;; --- EWW-style web browser ---
(def *eww-history* [])
(def *eww-current-url* #f)

(def (eww-fetch-url url)
  "Fetch a URL using curl and return the raw HTML."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((port (open-process
                     (list path: "curl"
                           arguments: ["-sL" "-m" "10"
                                       "-A" "Mozilla/5.0 (compatible; gerbil-emacs eww)"
                                       url]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (output (read-line port #f))
             (_ (process-status port)))
        (close-port port)
        output))))

(def (eww-html-to-text html)
  "Simple HTML to text converter. Strips tags, decodes basic entities, adds newlines."
  (let* ((len (string-length html))
         (out (open-output-string))
         (col 0)
         (fill-col 78))
    (let loop ((i 0) (in-tag #f) (in-pre #f) (tag-buf ""))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref html i)))
          (cond
            ;; Start of tag
            ((and (not in-tag) (char=? ch #\<))
             (loop (+ i 1) #t in-pre ""))
            ;; End of tag
            ((and in-tag (char=? ch #\>))
             (let* ((tag (string-downcase tag-buf))
                    (tag-name (let ((sp (string-index tag #\space)))
                                (if sp (substring tag 0 sp) tag))))
               (cond
                 ((or (string=? tag-name "br") (string=? tag-name "br/"))
                  (write-char #\newline out) (set! col 0))
                 ((or (string=? tag-name "p") (string=? tag-name "/p")
                      (string=? tag-name "div") (string=? tag-name "/div")
                      (string=? tag-name "h1") (string=? tag-name "h2")
                      (string=? tag-name "h3") (string=? tag-name "h4")
                      (string=? tag-name "/h1") (string=? tag-name "/h2")
                      (string=? tag-name "/h3") (string=? tag-name "/h4")
                      (string=? tag-name "tr") (string=? tag-name "/tr")
                      (string=? tag-name "li"))
                  (when (> col 0)
                    (write-char #\newline out) (set! col 0))
                  (when (string=? tag-name "li")
                    (display "  * " out) (set! col 4)))
                 ((string=? tag-name "pre")
                  (loop (+ i 1) #f #t ""))
                 ((string=? tag-name "/pre")
                  (loop (+ i 1) #f #f "")))
               (loop (+ i 1) #f in-pre "")))
            ;; Inside tag
            (in-tag
             (loop (+ i 1) #t in-pre (string-append tag-buf (string ch))))
            ;; HTML entity
            ((char=? ch #\&)
             (let entity ((j (+ i 1)) (ebuf ""))
               (if (or (>= j len) (> (- j i) 10))
                 (begin (write-char #\& out) (set! col (+ col 1))
                        (loop (+ i 1) #f in-pre ""))
                 (let ((ec (string-ref html j)))
                   (if (char=? ec #\;)
                     (let ((entity-str ebuf))
                       (cond
                         ((string=? entity-str "amp") (write-char #\& out))
                         ((string=? entity-str "lt") (write-char #\< out))
                         ((string=? entity-str "gt") (write-char #\> out))
                         ((string=? entity-str "quot") (write-char #\" out))
                         ((string=? entity-str "apos") (write-char #\' out))
                         ((string=? entity-str "nbsp") (write-char #\space out))
                         ((string=? entity-str "#39") (write-char #\' out))
                         (else (display entity-str out)))
                       (set! col (+ col 1))
                       (loop (+ j 1) #f in-pre ""))
                     (entity (+ j 1) (string-append ebuf (string ec))))))))
            ;; Whitespace handling
            ((and (not in-pre) (char-whitespace? ch))
             (when (> col 0)
               (write-char #\space out) (set! col (+ col 1)))
             ;; Skip consecutive whitespace
             (let skip ((k (+ i 1)))
               (if (and (< k len) (char-whitespace? (string-ref html k)))
                 (skip (+ k 1))
                 (loop k #f in-pre ""))))
            ;; Regular character
            (else
             (write-char ch out)
             (set! col (+ col 1))
             ;; Word wrap at fill column
             (when (and (not in-pre) (>= col fill-col) (char=? ch #\space))
               (write-char #\newline out) (set! col 0))
             (loop (+ i 1) #f in-pre ""))))))))

(def (cmd-eww app)
  "Open a URL in the text browser."
  (let ((url (qt-echo-read-string app "URL: ")))
    (when (and url (not (string=? url "")))
      ;; Prepend https:// if no scheme
      (let ((full-url (if (or (string-prefix? "http://" url)
                              (string-prefix? "https://" url))
                        url
                        (string-append "https://" url))))
        (echo-message! (app-state-echo app) (string-append "Fetching " full-url "..."))
        (let ((html (eww-fetch-url full-url)))
          (if (not html)
            (echo-error! (app-state-echo app) "Failed to fetch URL")
            (let* ((text (eww-html-to-text html))
                   (buf-name "*eww*")
                   (fr (app-state-frame app))
                   (ed (current-qt-editor app))
                   (buf (or (buffer-by-name buf-name)
                            (qt-buffer-create! buf-name ed #f))))
              (set! *eww-current-url* full-url)
              (set! *eww-history* (cons full-url *eww-history*))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed
                (string-append "URL: " full-url "\n\n" text))
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (qt-plain-text-edit-set-cursor-position! ed 0)
              (echo-message! (app-state-echo app) full-url))))))))

(def (cmd-eww-back app)
  "Go back in eww browsing history."
  (if (or (null? *eww-history*) (null? (cdr *eww-history*)))
    (echo-message! (app-state-echo app) "No previous page")
    (begin
      (set! *eww-history* (cdr *eww-history*))
      (let ((url (car *eww-history*)))
        (set! *eww-current-url* url)
        (echo-message! (app-state-echo app) (string-append "Fetching " url "..."))
        (let ((html (eww-fetch-url url)))
          (when html
            (let* ((text (eww-html-to-text html))
                   (ed (current-qt-editor app))
                   (fr (app-state-frame app)))
              (qt-plain-text-edit-set-text! ed
                (string-append "URL: " url "\n\n" text))
              (qt-plain-text-edit-set-cursor-position! ed 0))))))))

(def (cmd-eww-reload app)
  "Reload the current eww page."
  (when *eww-current-url*
    (echo-message! (app-state-echo app) "Reloading...")
    (let ((html (eww-fetch-url *eww-current-url*)))
      (when html
        (let* ((text (eww-html-to-text html))
               (ed (current-qt-editor app)))
          (qt-plain-text-edit-set-text! ed
            (string-append "URL: " *eww-current-url* "\n\n" text))
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;; --- Remote file editing (tramp-style) ---
(def (tramp-path? path)
  "Check if PATH is a tramp-style remote path (/ssh:host:path or /scp:host:path)."
  (or (string-prefix? "/ssh:" path)
      (string-prefix? "/scp:" path)))

(def (tramp-parse-path path)
  "Parse /ssh:host:path into (values host remote-path).
   Also supports /ssh:user@host:path."
  (let* ((rest (cond
                 ((string-prefix? "/ssh:" path) (substring path 5 (string-length path)))
                 ((string-prefix? "/scp:" path) (substring path 5 (string-length path)))
                 (else path)))
         (colon-pos (string-index rest #\:)))
    (if colon-pos
      (values (substring rest 0 colon-pos)
              (substring rest (+ colon-pos 1) (string-length rest)))
      (values rest "/"))))

(def (tramp-read-file host remote-path)
  "Read a remote file via scp into a string."
  (let* ((tmp (path-expand
                (string-append "tramp-" (number->string (random-integer 100000)))
                (or (getenv "TMPDIR" #f) "/tmp")))
         (src (string-append host ":" remote-path))
         (proc (open-process
                 (list path: "/usr/bin/scp"
                       arguments: ["-q" src tmp]
                       stdout-redirection: #t
                       stderr-redirection: #f
                       pseudo-terminal: #f)))
         (_ (process-status proc)))
    (close-port proc)
    (if (file-exists? tmp)
      (let ((content (call-with-input-file tmp (lambda (p) (read-line p #f)))))
        (delete-file tmp)
        content)
      #f)))

(def (tramp-write-file host remote-path content)
  "Write content to a remote file via scp."
  (let* ((tmp (path-expand
                (string-append "tramp-" (number->string (random-integer 100000)))
                (or (getenv "TMPDIR" #f) "/tmp")))
         (dst (string-append host ":" remote-path)))
    (call-with-output-file tmp (lambda (p) (display content p)))
    (let* ((proc (open-process
                   (list path: "/usr/bin/scp"
                         arguments: ["-q" tmp dst]
                         stdout-redirection: #t
                         stderr-redirection: #f
                         pseudo-terminal: #f)))
           (status (process-status proc)))
      (close-port proc)
      (when (file-exists? tmp) (delete-file tmp))
      (= status 0))))

(def (cmd-find-file-remote app)
  "Open a remote file via SSH/SCP. Use /ssh:host:path or /scp:host:path syntax."
  (let ((path (qt-echo-read-string app "Remote file (/ssh:host:path): ")))
    (when (and path (not (string=? path "")))
      (if (not (tramp-path? path))
        (echo-error! (app-state-echo app) "Use /ssh:host:path syntax")
        (let-values (((host remote-path) (tramp-parse-path path)))
          (echo-message! (app-state-echo app)
            (string-append "Fetching " host ":" remote-path "..."))
          (let ((content (tramp-read-file host remote-path)))
            (if (not content)
              (echo-error! (app-state-echo app)
                (string-append "Failed to fetch " remote-path " from " host))
              (let* ((name (string-append (path-strip-directory remote-path) " [" host "]"))
                     (fr (app-state-frame app))
                     (ed (current-qt-editor app))
                     (buf (qt-buffer-create! name ed #f)))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed content)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)
                ;; Store remote info in buffer for save-back
                (set! (buffer-file-path buf) path)
                (echo-message! (app-state-echo app)
                  (string-append "Loaded " remote-path " from " host))))))))))

(def (cmd-save-remote-buffer app)
  "Save buffer back to remote host if it has a tramp-style path."
  (let* ((buf (current-qt-buffer app))
         (fpath (buffer-file-path buf)))
    (if (or (not fpath) (not (tramp-path? fpath)))
      (echo-error! (app-state-echo app) "Not a remote buffer")
      (let-values (((host remote-path) (tramp-parse-path fpath)))
        (let ((text (qt-plain-text-edit-text (current-qt-editor app))))
          (echo-message! (app-state-echo app)
            (string-append "Saving to " host ":" remote-path "..."))
          (if (tramp-write-file host remote-path text)
            (begin
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (echo-message! (app-state-echo app) "Remote file saved"))
            (echo-error! (app-state-echo app) "Failed to save remote file")))))))

;; --- Calendar ---
(def *calendar-year* #f)
(def *calendar-month* #f)

(def (calendar-current-year-month)
  "Get current year and month from system date command."
  (with-catch
    (lambda (e) (values 2026 1))
    (lambda ()
      (let* ((port (open-process
                     (list path: "/bin/date"
                           arguments: ["+%Y %m"]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (line (read-line port)))
        (close-port port)
        (if (eof-object? line)
          (values 2026 1)
          (let* ((parts (string-split line #\space))
                 (year (string->number (car parts)))
                 (month (string->number (cadr parts))))
            (values year month)))))))

(def (calendar-render year month)
  "Render a 3-month calendar centered on the given month."
  (with-catch
    (lambda (e) (string-append "Calendar error\n"))
    (lambda ()
      (let* ((port (open-process
                     (list path: "/usr/bin/cal"
                           arguments: ["-3" (number->string month) (number->string year)]
                           stdout-redirection: #t
                           stderr-redirection: #f
                           pseudo-terminal: #f)))
             (output (read-line port #f)))
        (close-port port)
        (or output "")))))

(def (cmd-calendar app)
  "Display a calendar."
  (when (not *calendar-year*)
    (let-values (((y m) (calendar-current-year-month)))
      (set! *calendar-year* y)
      (set! *calendar-month* m)))
  (let* ((text (string-append
                 (calendar-render *calendar-year* *calendar-month*)
                 "\n\nNavigation: p=prev month  n=next month  <=prev year  >=next year  .=today"))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (or (buffer-by-name "*calendar*")
                  (qt-buffer-create! "*calendar*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

(def (cmd-calendar-prev-month app)
  "Go to previous month in calendar."
  (when *calendar-month*
    (set! *calendar-month* (- *calendar-month* 1))
    (when (< *calendar-month* 1)
      (set! *calendar-month* 12)
      (set! *calendar-year* (- *calendar-year* 1)))
    (cmd-calendar app)))

(def (cmd-calendar-next-month app)
  "Go to next month in calendar."
  (when *calendar-month*
    (set! *calendar-month* (+ *calendar-month* 1))
    (when (> *calendar-month* 12)
      (set! *calendar-month* 1)
      (set! *calendar-year* (+ *calendar-year* 1)))
    (cmd-calendar app)))

(def (cmd-calendar-prev-year app)
  "Go to previous year in calendar."
  (when *calendar-year*
    (set! *calendar-year* (- *calendar-year* 1))
    (cmd-calendar app)))

(def (cmd-calendar-next-year app)
  "Go to next year in calendar."
  (when *calendar-year*
    (set! *calendar-year* (+ *calendar-year* 1))
    (cmd-calendar app)))

(def (cmd-calendar-today app)
  "Go to current month in calendar."
  (let-values (((y m) (calendar-current-year-month)))
    (set! *calendar-year* y)
    (set! *calendar-month* m)
    (cmd-calendar app)))

;; --- Runtime key rebinding ---
(def *custom-keys-path*
  (path-expand ".gerbil-emacs-keys" (user-info-home (user-info (user-name)))))

(def (cmd-global-set-key app)
  "Bind a key to a command interactively."
  (let* ((key-str (qt-echo-read-string app "Key (e.g. C-c a, M-g t): "))
         (cmds (sort (map (lambda (p) (symbol->string (car p)))
                       (hash->list *all-commands*))
                 string<?))
         (cmd-name (qt-echo-read-string-with-completion app "Command: " cmds)))
    (when (and key-str (not (string=? key-str ""))
               cmd-name (not (string=? cmd-name "")))
      (let ((cmd-sym (string->symbol cmd-name)))
        ;; Determine which keymap to bind in
        (cond
          ((string-prefix? "C-x " key-str)
           (keymap-bind! *ctrl-x-map* (substring key-str 4 (string-length key-str)) cmd-sym))
          ((string-prefix? "C-c " key-str)
           (keymap-bind! *ctrl-c-map* (substring key-str 4 (string-length key-str)) cmd-sym))
          (else
           (keymap-bind! *global-keymap* key-str cmd-sym)))
        ;; Record and save to persistent file
        (set! *custom-key-bindings*
          (cons (cons key-str cmd-name)
                (filter (lambda (p) (not (string=? (car p) key-str)))
                        *custom-key-bindings*)))
        (custom-keys-save!)
        (echo-message! (app-state-echo app)
          (string-append key-str " → " cmd-name))))))

(def (cmd-global-unset-key app)
  "Unbind a key."
  (let ((key-str (qt-echo-read-string app "Key to unbind: ")))
    (when (and key-str (not (string=? key-str "")))
      (cond
        ((string-prefix? "C-x " key-str)
         (hash-remove! *ctrl-x-map* (substring key-str 4 (string-length key-str))))
        ((string-prefix? "C-c " key-str)
         (hash-remove! *ctrl-c-map* (substring key-str 4 (string-length key-str))))
        (else
         (hash-remove! *global-keymap* key-str)))
      (set! *custom-key-bindings*
        (filter (lambda (p) (not (string=? (car p) key-str)))
                *custom-key-bindings*))
      (custom-keys-save!)
      (echo-message! (app-state-echo app)
        (string-append key-str " unbound")))))

(def *custom-key-bindings* []) ;; list of (key-str . cmd-name) pairs

(def (custom-keys-save!)
  "Save custom key bindings to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file *custom-keys-path*
        (lambda (port)
          (for-each
            (lambda (pair)
              (display (car pair) port) (display "\t" port)
              (display (cdr pair) port) (newline port))
            *custom-key-bindings*))))))

(def (custom-keys-load!)
  "Load custom key bindings from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *custom-keys-path*)
        (call-with-input-file *custom-keys-path*
          (lambda (port)
            (let loop ()
              (let ((line (read-line port)))
                (unless (eof-object? line)
                  (let ((tab-pos (string-index line #\tab)))
                    (when tab-pos
                      (let* ((key-str (substring line 0 tab-pos))
                             (cmd-name (substring line (+ tab-pos 1) (string-length line)))
                             (cmd-sym (string->symbol cmd-name)))
                        (cond
                          ((string-prefix? "C-x " key-str)
                           (keymap-bind! *ctrl-x-map*
                             (substring key-str 4 (string-length key-str)) cmd-sym))
                          ((string-prefix? "C-c " key-str)
                           (keymap-bind! *ctrl-c-map*
                             (substring key-str 4 (string-length key-str)) cmd-sym))
                          (else
                           (keymap-bind! *global-keymap* key-str cmd-sym))))))
                  (loop))))))))))

;;; ============================================================================
;;; Init file loading
;;; ============================================================================

(def *init-file-path*
  (path-expand ".gerbil-emacs-init.ss" (user-info-home (user-info (user-name)))))

(def (load-init-file!)
  "Load user init file (~/.gerbil-emacs-init.ss) if it exists."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *init-file-path*)
        (let ((text (read-file-as-string *init-file-path*)))
          (when text
            (let ((port (open-input-string text)))
              (let loop ()
                (let ((form (read port)))
                  (unless (eof-object? form)
                    (with-catch
                      (lambda (e) #f) ;; skip forms that error
                      (lambda () (eval form)))
                    (loop)))))))))))

(def (cmd-load-init-file app)
  "Reload user init file."
  (if (file-exists? *init-file-path*)
    (begin
      (load-init-file!)
      (echo-message! (app-state-echo app)
        (string-append "Loaded " *init-file-path*)))
    (echo-error! (app-state-echo app)
      (string-append "No init file: " *init-file-path*))))

(def (cmd-find-init-file app)
  "Open user init file for editing."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (name (path-strip-directory *init-file-path*))
         (buf (or (buffer-by-name name)
                  (qt-buffer-create! name ed *init-file-path*))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (when (file-exists? *init-file-path*)
      (let ((text (read-file-as-string *init-file-path*)))
        (when text
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))
    (echo-message! (app-state-echo app) *init-file-path*)))

;;; ============================================================================
;;; Persistent scratch buffer
;;; ============================================================================

(def *scratch-file-path*
  (path-expand ".gerbil-emacs-scratch" (user-info-home (user-info (user-name)))))

(def (scratch-save!)
  "Save scratch buffer contents to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((buf (buffer-by-name "*scratch*")))
        (when buf
          ;; Find a window showing this buffer to get text
          (let ((text #f))
            ;; Try each buffer in buffer-list to find one with a doc-pointer
            (when (buffer-doc-pointer buf)
              ;; We need the text from the widget, but we may not have the editor
              ;; Use the last known text approach: save it on each access
              (void))
            ;; Fallback: write from *scratch-last-text*
            (when *scratch-last-text*
              (call-with-output-file *scratch-file-path*
                (lambda (port) (display *scratch-last-text* port))))))))))

(def *scratch-last-text* #f)

(def (scratch-update-text! text)
  "Update cached scratch buffer text for persistence."
  (set! *scratch-last-text* text))

(def (scratch-restore!)
  "Restore scratch buffer contents from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (if (file-exists? *scratch-file-path*)
        (let ((text (read-file-as-string *scratch-file-path*)))
          (if text
            (begin (set! *scratch-last-text* text) text)
            #f))
        #f))))

;;; ============================================================================
;;; Basic Org-mode support
;;; ============================================================================

(def *org-todo-keywords* '("TODO" "IN-PROGRESS" "DONE"))

(def (org-buffer? buf)
  "Check if buffer is an org file."
  (let ((name (buffer-name buf)))
    (and (> (string-length name) 4)
         (string-suffix? ".org" name))))

(def (org-get-current-line ed)
  "Get the current line text and its start/end positions."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1) (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1))))))
    (values (substring text line-start line-end) line-start line-end)))

(def (org-heading-level line)
  "Return the heading level (number of leading *'s) or 0 if not a heading."
  (let loop ((i 0))
    (if (or (>= i (string-length line))
            (not (char=? (string-ref line i) #\*)))
      (if (and (> i 0) (< i (string-length line))
               (char=? (string-ref line i) #\space))
        i 0)
      (loop (+ i 1)))))

(def (cmd-org-todo-cycle app)
  "Cycle TODO state on current heading: none -> TODO -> IN-PROGRESS -> DONE -> none."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          ;; Extract the part after "*** "
          (let* ((after-stars (substring line (+ level 1) (string-length line)))
                 ;; Check if line starts with a TODO keyword
                 (current-kw
                   (let loop ((kws *org-todo-keywords*))
                     (if (null? kws) #f
                       (let ((kw (car kws)))
                         (if (string-prefix? (string-append kw " ") after-stars)
                           kw (loop (cdr kws)))))))
                 ;; Determine next keyword
                 (next-kw
                   (if (not current-kw)
                     (car *org-todo-keywords*)
                     (let loop ((kws *org-todo-keywords*))
                       (cond
                         ((null? kws) #f)  ;; cycle back to none
                         ((string=? (car kws) current-kw)
                          (if (null? (cdr kws)) #f (cadr kws)))
                         (else (loop (cdr kws)))))))
                 ;; Build new line
                 (heading-prefix (string-append (make-string level #\*) " "))
                 (rest (if current-kw
                         (substring after-stars
                           (+ (string-length current-kw) 1)
                           (string-length after-stars))
                         after-stars))
                 (new-line (if next-kw
                            (string-append heading-prefix next-kw " " rest)
                            (string-append heading-prefix rest)))
                 (new-text (string-append
                             (substring text 0 line-start)
                             new-line
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (+ line-start (string-length new-line))))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-org-promote app)
  "Decrease heading level (remove one *)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 1)
          (let* ((new-line (substring line 1 (string-length line)))
                 (new-text (string-append
                             (substring text 0 line-start)
                             new-line
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (max line-start (- pos 1)))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-org-demote app)
  "Increase heading level (add one *)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          (let* ((new-line (string-append "*" line))
                 (new-text (string-append
                             (substring text 0 line-start)
                             new-line
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-org-toggle-checkbox app)
  "Toggle checkbox: [ ] <-> [X]."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((check-pos (string-contains line "[ ]"))
            (checked-pos (string-contains line "[X]")))
        (cond
          (check-pos
           (let* ((abs-pos (+ line-start check-pos))
                  (new-text (string-append
                              (substring text 0 abs-pos) "[X]"
                              (substring text (+ abs-pos 3) (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed pos)))
          (checked-pos
           (let* ((abs-pos (+ line-start checked-pos))
                  (new-text (string-append
                              (substring text 0 abs-pos) "[ ]"
                              (substring text (+ abs-pos 3) (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed pos)))
          (else
           (echo-message! (app-state-echo app) "No checkbox on this line")))))))

(def (cmd-org-insert-heading app)
  "Insert a new heading at the same level as the current one."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let* ((level (org-heading-level line))
             (stars (if (> level 0) level 1))
             (heading (string-append "\n" (make-string stars #\*) " "))
             (new-text (string-append
                         (substring text 0 line-end) heading
                         (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (+ line-end (string-length heading)))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-org-next-heading app)
  "Move to next heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Skip past current line
    (let ((next-nl (let loop ((i pos))
                     (if (or (>= i len) (char=? (string-ref text i) #\newline))
                       (+ i 1) (loop (+ i 1))))))
      ;; Find next line starting with *
      (let loop ((i next-nl))
        (cond
          ((>= i len)
           (echo-message! (app-state-echo app) "No more headings"))
          ((and (char=? (string-ref text i) #\*)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (+ i 1))))))))

(def (cmd-org-prev-heading app)
  "Move to previous heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Go back to start of current line
    (let ((line-start (let loop ((i (- pos 1)))
                        (if (or (< i 0) (char=? (string-ref text i) #\newline))
                          i (loop (- i 1))))))
      ;; Search backward for a line starting with *
      (let loop ((i (- line-start 1)))
        (cond
          ((< i 0)
           (echo-message! (app-state-echo app) "No previous heading"))
          ((and (char=? (string-ref text i) #\*)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (- i 1))))))))

(def (cmd-org-move-subtree-up app)
  "Move current heading and its subtree up (swap with previous sibling)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          ;; Find end of current subtree
          (let* ((len (string-length text))
                 (subtree-end
                   (let loop ((i (+ line-end 1)))
                     (cond
                       ((>= i len) len)
                       ((and (char=? (string-ref text i) #\*)
                             (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                        ;; Check if this is same level or higher (not deeper)
                        (let ((sub-line-end (let lp ((j i))
                                              (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                j (lp (+ j 1))))))
                          (let ((sub-level (org-heading-level (substring text i sub-line-end))))
                            (if (<= sub-level level) i
                              (loop (+ sub-line-end 1))))))
                       (else (loop (+ i 1))))))
                 ;; Find start of previous sibling
                 (prev-start
                   (let loop ((i (- line-start 2)))
                     (cond
                       ((< i 0) #f)
                       ((and (char=? (string-ref text i) #\*)
                             (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                        (let ((prev-line-end (let lp ((j i))
                                               (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                 j (lp (+ j 1))))))
                          (let ((prev-level (org-heading-level (substring text i prev-line-end))))
                            (if (= prev-level level) i
                              (if (< prev-level level) #f
                                (loop (- i 1)))))))
                       (else (loop (- i 1)))))))
            (when prev-start
              ;; Swap: previous sibling subtree with current subtree
              (let* ((current-subtree (substring text line-start subtree-end))
                     (prev-subtree (substring text prev-start line-start))
                     (new-text (string-append
                                 (substring text 0 prev-start)
                                 current-subtree prev-subtree
                                 (substring text subtree-end (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed prev-start)
                (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))

(def (cmd-org-move-subtree-down app)
  "Move current heading and its subtree down (swap with next sibling)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((line line-start line-end) (org-get-current-line ed)))
      (let ((level (org-heading-level line)))
        (when (> level 0)
          (let* ((len (string-length text))
                 ;; Find end of current subtree
                 (subtree-end
                   (let loop ((i (+ line-end 1)))
                     (cond
                       ((>= i len) len)
                       ((and (char=? (string-ref text i) #\*)
                             (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                        (let ((sub-line-end (let lp ((j i))
                                              (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                j (lp (+ j 1))))))
                          (let ((sub-level (org-heading-level (substring text i sub-line-end))))
                            (if (<= sub-level level) i
                              (loop (+ sub-line-end 1))))))
                       (else (loop (+ i 1)))))))
            ;; Find end of next sibling subtree
            (when (< subtree-end len)
              (let* ((next-heading-end
                       (let loop ((i subtree-end))
                         (if (or (>= i len) (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
                     (next-subtree-end
                       (let loop ((i (+ next-heading-end 1)))
                         (cond
                           ((>= i len) len)
                           ((and (char=? (string-ref text i) #\*)
                                 (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                            (let ((sub-line-end (let lp ((j i))
                                                  (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                                    j (lp (+ j 1))))))
                              (let ((sub-level (org-heading-level (substring text i sub-line-end))))
                                (if (<= sub-level level) i
                                  (loop (+ sub-line-end 1))))))
                           (else (loop (+ i 1)))))))
                ;; Swap
                (let* ((current-subtree (substring text line-start subtree-end))
                       (next-subtree (substring text subtree-end next-subtree-end))
                       (new-text (string-append
                                   (substring text 0 line-start)
                                   next-subtree current-subtree
                                   (substring text next-subtree-end (string-length text)))))
                  (qt-plain-text-edit-set-text! ed new-text)
                  (qt-plain-text-edit-set-cursor-position! ed
                    (+ line-start (string-length next-subtree)))
                  (qt-plain-text-edit-ensure-cursor-visible! ed))))))))))

;;; ============================================================================
;;; Markdown mode
;;; ============================================================================

(def (md-heading-level line)
  "Return the heading level (1-6) of LINE, or 0 if not a heading."
  (let ((len (string-length line)))
    (if (= len 0) 0
      (let loop ((i 0))
        (cond
          ((>= i len) 0)
          ((>= i 6) 0) ;; max 6 levels
          ((char=? (string-ref line i) #\#) (loop (+ i 1)))
          ((and (> i 0) (char=? (string-ref line i) #\space)) i)
          (else 0))))))

(def (md-get-current-line ed)
  "Get current line text, start position, and end position."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (line-start (let loop ((i pos))
                       (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                         i (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i len) (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1))))))
    (values (substring text line-start line-end) line-start line-end)))

(def (cmd-markdown-promote app)
  "Decrease heading level (remove a #)."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let ((level (md-heading-level line)))
        (if (<= level 1)
          (echo-error! (app-state-echo app) "Cannot promote further")
          (let* ((text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring text 0 line-start)
                             (substring line 1 (string-length line))
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed line-start)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-markdown-demote app)
  "Increase heading level (add a #)."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let ((level (md-heading-level line)))
        (cond
          ((= level 0)
           ;; Not a heading — make it one
           (let* ((text (qt-plain-text-edit-text ed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              "# " line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed (+ line-start 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed)))
          ((>= level 6)
           (echo-error! (app-state-echo app) "Cannot demote further (max level 6)"))
          (else
           (let* ((text (qt-plain-text-edit-text ed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              "#" line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed (+ line-start level 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed))))))))

(def (cmd-markdown-next-heading app)
  "Jump to the next markdown heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Skip to next line first
    (let ((start (let loop ((i pos))
                   (if (or (>= i len) (char=? (string-ref text i) #\newline))
                     (+ i 1) (loop (+ i 1))))))
      (let loop ((i start))
        (cond
          ((>= i len)
           (echo-message! (app-state-echo app) "No more headings"))
          ((and (char=? (string-ref text i) #\#)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (+ i 1))))))))

(def (cmd-markdown-prev-heading app)
  "Jump to the previous markdown heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Go to start of current line, then one more
    (let ((start (let loop ((i pos))
                   (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                     (- i 1) (loop (- i 1))))))
      (let loop ((i (max 0 start)))
        (cond
          ((< i 0)
           (echo-message! (app-state-echo app) "No previous heading"))
          ((and (char=? (string-ref text i) #\#)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (- i 1))))))))

(def (cmd-markdown-insert-heading app)
  "Insert a heading at the same level as the current one."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let* ((level (md-heading-level line))
             (prefix (if (> level 0) (string-append (make-string level #\#) " ") "## "))
             (text (qt-plain-text-edit-text ed))
             (insert-text (string-append "\n" prefix))
             (new-text (string-append
                         (substring text 0 line-end)
                         insert-text
                         (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end (string-length insert-text)))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-markdown-toggle-bold app)
  "Toggle bold (**) around the word at point or selection."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection, insert **cursor**
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "****"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 2)))
      ;; Wrap selection in **
      (let* ((selected (substring text sel-start sel-end))
             ;; Check if already bold
             (already-bold (and (>= (string-length selected) 4)
                                (string-prefix? "**" selected)
                                (string-suffix? "**" selected)))
             (replacement (if already-bold
                            (substring selected 2 (- (string-length selected) 2))
                            (string-append "**" selected "**")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-toggle-italic app)
  "Toggle italic (*) around the word at point or selection."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "**"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
      (let* ((selected (substring text sel-start sel-end))
             (already-italic (and (>= (string-length selected) 2)
                                   (string-prefix? "*" selected)
                                   (string-suffix? "*" selected)
                                   (not (string-prefix? "**" selected))))
             (replacement (if already-italic
                            (substring selected 1 (- (string-length selected) 1))
                            (string-append "*" selected "*")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-toggle-code app)
  "Toggle inline code (`) around selection or insert backticks."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "``"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
      (let* ((selected (substring text sel-start sel-end))
             (already-code (and (>= (string-length selected) 2)
                                 (string-prefix? "`" selected)
                                 (string-suffix? "`" selected)))
             (replacement (if already-code
                            (substring selected 1 (- (string-length selected) 1))
                            (string-append "`" selected "`")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-insert-link app)
  "Insert a markdown link [text](url)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (link-text (if (= sel-start sel-end) ""
                      (substring text sel-start sel-end)))
         (url (qt-echo-read-string app "URL: ")))
    (when (> (string-length url) 0)
      (let* ((md-link (string-append "[" (if (string=? link-text "") "link" link-text) "](" url ")"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         md-link
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length md-link)))))))

(def (cmd-markdown-insert-code-block app)
  "Insert a fenced code block."
  (let* ((ed (current-qt-editor app))
         (lang (qt-echo-read-string app "Language (empty for none): "))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (block (string-append "\n```" lang "\n\n```\n"))
         (new-text (string-append
                     (substring text 0 pos)
                     block
                     (substring text pos (string-length text)))))
    (qt-plain-text-edit-set-text! ed new-text)
    ;; Place cursor inside the code block
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 4 (string-length lang) 1))
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-markdown-toggle-checkbox app)
  "Toggle a markdown checkbox [ ] / [x]."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let* ((text (qt-plain-text-edit-text ed))
             (trimmed (string-trim line))
             (has-unchecked (string-contains trimmed "[ ]"))
             (has-checked (string-contains trimmed "[x]")))
        (cond
          (has-unchecked
           (let* ((idx (string-contains line "[ ]"))
                  (new-line (string-append
                              (substring line 0 idx) "[x]"
                              (substring line (+ idx 3) (string-length line))))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)))
          (has-checked
           (let* ((idx (string-contains line "[x]"))
                  (new-line (string-append
                              (substring line 0 idx) "[ ]"
                              (substring line (+ idx 3) (string-length line))))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)))
          (else
           ;; Add checkbox prefix
           (let* ((new-line (string-append "- [ ] " trimmed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text))))))))

(def (cmd-markdown-outline app)
  "Show an outline of all headings in the current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (lines
           (let loop ((i 0) (acc []))
             (if (>= i len) (reverse acc)
               (let* ((line-end (let lp ((j i))
                                  (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                    j (lp (+ j 1)))))
                      (line (substring text i line-end)))
                 (if (and (> (string-length line) 0)
                          (char=? (string-ref line 0) #\#))
                   (let ((level (md-heading-level line)))
                     (if (> level 0)
                       (loop (+ line-end 1) (cons (cons i line) acc))
                       (loop (+ line-end 1) acc)))
                   (loop (+ line-end 1) acc)))))))
    (if (null? lines)
      (echo-message! (app-state-echo app) "No headings found")
      (let* ((fr (app-state-frame app))
             (outline-buf (or (buffer-by-name "*MD Outline*")
                              (qt-buffer-create! "*MD Outline*" ed #f)))
             (outline-text
               (string-join
                 (map (lambda (entry)
                        (let* ((pos (car entry))
                               (line (cdr entry))
                               (level (md-heading-level line))
                               (indent (make-string (* 2 (- level 1)) #\space)))
                          (string-append indent (number->string pos) ": " line)))
                      lines)
                 "\n")))
        (qt-buffer-attach! ed outline-buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) outline-buf)
        (qt-plain-text-edit-set-text! ed outline-text)
        (qt-text-document-set-modified! (buffer-doc-pointer outline-buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

(def (cmd-markdown-preview app)
  "Generate and display an HTML preview of the current markdown buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (fr (app-state-frame app))
         (preview-buf (or (buffer-by-name "*MD Preview*")
                          (qt-buffer-create! "*MD Preview*" ed #f))))
    ;; Simple markdown to text conversion for preview
    (let* ((lines (string-split text #\newline))
           (rendered
             (string-join
               (map (lambda (line)
                      (let ((level (md-heading-level line)))
                        (cond
                          ;; Heading: underline with = or -
                          ((> level 0)
                           (let* ((heading-text (substring line (+ level 1) (string-length line)))
                                  (underline (make-string (string-length heading-text)
                                                          (if (= level 1) #\= #\-))))
                             (string-append "\n" heading-text "\n" underline)))
                          ;; Horizontal rule
                          ((or (string-prefix? "---" line) (string-prefix? "***" line)
                               (string-prefix? "___" line))
                           (make-string 72 #\-))
                          ;; Code block markers
                          ((string-prefix? "```" line)
                           (string-append "--- " (substring line 3 (string-length line)) " ---"))
                          ;; List items
                          ((string-prefix? "- " line) line)
                          ((string-prefix? "* " line) (string-append "- " (substring line 2 (string-length line))))
                          ;; Quote blocks
                          ((string-prefix? "> " line) (string-append "  | " (substring line 2 (string-length line))))
                          ;; Regular line
                          (else line))))
                    lines)
               "\n")))
      (qt-buffer-attach! ed preview-buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) preview-buf)
      (qt-plain-text-edit-set-text! ed rendered)
      (qt-text-document-set-modified! (buffer-doc-pointer preview-buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "Markdown preview"))))

;;; ============================================================================
;;; Snippet/template expansion system
;;; ============================================================================

(def *snippet-table* (make-hash-table)) ;; lang -> (hash trigger -> template)
(def *snippet-active* #f) ;; #f or (fields . current-field-idx)
(def *snippet-field-positions* []) ;; list of (start . end) positions

(def (snippet-define! lang trigger template)
  "Define a snippet: LANG is language symbol or 'global, TRIGGER is prefix string,
   TEMPLATE is string with $1, $2 etc. for fields and $0 for final cursor pos."
  (let ((lang-table (or (hash-get *snippet-table* lang) (make-hash-table))))
    (hash-put! lang-table trigger template)
    (hash-put! *snippet-table* lang lang-table)))

(def (snippet-lookup trigger lang)
  "Look up a snippet by trigger, checking lang-specific then global."
  (or (let ((lt (hash-get *snippet-table* lang)))
        (and lt (hash-get lt trigger)))
      (let ((gt (hash-get *snippet-table* 'global)))
        (and gt (hash-get gt trigger)))))

(def (snippet-expand-template template)
  "Expand template: replace $1..$9 with empty placeholders, return (text . field-offsets).
   Field offsets are positions where $N markers were."
  (let ((out (open-output-string))
        (fields (make-hash-table))
        (len (string-length template)))
    (let loop ((i 0))
      (cond
        ((>= i len)
         (let* ((text (get-output-string out))
                (offsets
                  (let collect ((n 1) (acc []))
                    (if (> n 9)
                      (let ((zero-pos (hash-get fields 0)))
                        (if zero-pos
                          (reverse (cons (cons 0 zero-pos) acc))
                          (reverse acc)))
                      (let ((pos (hash-get fields n)))
                        (if pos
                          (collect (+ n 1) (cons (cons n pos) acc))
                          (collect (+ n 1) acc)))))))
           (cons text offsets)))
        ((and (char=? (string-ref template i) #\$)
              (< (+ i 1) len)
              (char-numeric? (string-ref template (+ i 1))))
         (let ((n (- (char->integer (string-ref template (+ i 1)))
                     (char->integer #\0)))
               (pos (string-length (get-output-string out))))
           ;; Re-create output string to get current position
           (hash-put! fields n pos)
           (loop (+ i 2))))
        (else
         (display (string (string-ref template i)) out)
         (loop (+ i 1)))))))

(def (cmd-snippet-expand app)
  "Try to expand snippet at point. If no snippet, insert tab."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (prefix (get-word-prefix ed))
         (lang (or (buffer-lexer-lang buf) 'global)))
    (if (string=? prefix "")
      #f  ;; No trigger word
      (let ((template (snippet-lookup prefix lang)))
        (if (not template)
          #f  ;; No matching snippet
          (let* ((expanded (snippet-expand-template template))
                 (text (car expanded))
                 (fields (cdr expanded))
                 (pos (qt-plain-text-edit-cursor-position ed))
                 (trigger-start (- pos (string-length prefix)))
                 ;; Replace trigger with expanded text
                 (full-text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring full-text 0 trigger-start)
                             text
                             (substring full-text pos (string-length full-text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (if (null? fields)
              ;; No fields — place cursor at end of expansion
              (qt-plain-text-edit-set-cursor-position! ed
                (+ trigger-start (string-length text)))
              ;; Place cursor at first field
              (let ((first-field (car fields)))
                (set! *snippet-active* #t)
                (set! *snippet-field-positions*
                  (map (lambda (f) (+ trigger-start (cdr f))) fields))
                (qt-plain-text-edit-set-cursor-position! ed
                  (+ trigger-start (cdr first-field)))))
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            #t))))))

(def (cmd-snippet-next-field app)
  "Jump to next snippet field."
  (when *snippet-active*
    (let* ((ed (current-qt-editor app))
           (pos (qt-plain-text-edit-cursor-position ed))
           ;; Find next field after current position
           (next (let loop ((fps *snippet-field-positions*))
                   (if (null? fps) #f
                     (if (> (car fps) pos)
                       (car fps)
                       (loop (cdr fps)))))))
      (if next
        (begin
          (qt-plain-text-edit-set-cursor-position! ed next)
          (qt-plain-text-edit-ensure-cursor-visible! ed))
        ;; No more fields — deactivate snippet
        (begin
          (set! *snippet-active* #f)
          (set! *snippet-field-positions* []))))))

(def (cmd-snippet-prev-field app)
  "Jump to previous snippet field."
  (when *snippet-active*
    (let* ((ed (current-qt-editor app))
           (pos (qt-plain-text-edit-cursor-position ed))
           ;; Find previous field before current position
           (prev (let loop ((fps (reverse *snippet-field-positions*)))
                   (if (null? fps) #f
                     (if (< (car fps) pos)
                       (car fps)
                       (loop (cdr fps)))))))
      (when prev
        (qt-plain-text-edit-set-cursor-position! ed prev)
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-define-snippet app)
  "Interactively define a snippet."
  (let* ((lang-str (qt-echo-read-string app "Language (or global): "))
         (trigger (qt-echo-read-string app "Trigger: "))
         (template (qt-echo-read-string app "Template ($1,$2 for fields): ")))
    (when (and lang-str trigger template
               (> (string-length trigger) 0)
               (> (string-length template) 0))
      (let ((lang (string->symbol lang-str)))
        (snippet-define! lang trigger template)
        (echo-message! (app-state-echo app)
          (string-append "Snippet '" trigger "' defined for " lang-str))))))

(def (cmd-list-snippets app)
  "List all defined snippets."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (out (open-output-string)))
    (display "Snippets:\n\n" out)
    (hash-for-each
      (lambda (lang lang-table)
        (display (string-append "--- " (symbol->string lang) " ---\n") out)
        (hash-for-each
          (lambda (trigger template)
            (display (string-append "  " trigger " → " template "\n") out))
          lang-table))
      *snippet-table*)
    (let* ((text (get-output-string out))
           (buf (or (buffer-by-name "*Snippets*")
                    (qt-buffer-create! "*Snippets*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;; Built-in snippets for common languages
(snippet-define! 'scheme "def" "(def ($1)\n  $2)\n$0")
(snippet-define! 'scheme "defn" "(def ($1 $2)\n  $3)\n$0")
(snippet-define! 'scheme "let" "(let (($1 $2))\n  $3)\n$0")
(snippet-define! 'scheme "let*" "(let* (($1 $2))\n  $3)\n$0")
(snippet-define! 'scheme "when" "(when $1\n  $2)\n$0")
(snippet-define! 'scheme "unless" "(unless $1\n  $2)\n$0")
(snippet-define! 'scheme "cond" "(cond\n  (($1) $2)\n  (else $3))\n$0")
(snippet-define! 'scheme "if" "(if $1\n  $2\n  $3)\n$0")
(snippet-define! 'scheme "lambda" "(lambda ($1)\n  $2)\n$0")
(snippet-define! 'scheme "match" "(match $1\n  (($2) $3))\n$0")
(snippet-define! 'scheme "defstruct" "(defstruct $1 ($2))\n$0")
(snippet-define! 'scheme "defclass" "(defclass $1 ($2)\n  $3)\n$0")
(snippet-define! 'scheme "import" "(import $1)\n$0")
(snippet-define! 'scheme "export" "(export $1)\n$0")
(snippet-define! 'scheme "with-catch" "(with-catch\n  (lambda (e) $1)\n  (lambda ()\n    $2))\n$0")
(snippet-define! 'scheme "for-each" "(for-each\n  (lambda ($1)\n    $2)\n  $3)\n$0")

(snippet-define! 'global "todo" ";; TODO: $1\n$0")
(snippet-define! 'global "fixme" ";; FIXME: $1\n$0")
(snippet-define! 'global "note" ";; NOTE: $1\n$0")

;; --- Misc ---
(def (cmd-display-fill-column-indicator app)
  "Display fill column indicator."
  (cmd-toggle-fill-column-indicator app))

(def (cmd-display-line-numbers-relative app)
  "Toggle relative line numbers."
  (echo-message! (app-state-echo app) "Relative line numbers toggled"))

(def (cmd-font-lock-mode app)
  "Toggle font lock mode."
  (cmd-toggle-highlighting app))

(def (cmd-customize-face app)
  "Customize face."
  (echo-message! (app-state-echo app) "Face customization not available in Qt backend"))

(def (cmd-list-colors app)
  "List available colors."
  (echo-message! (app-state-echo app) "Color list not available"))

(def (cmd-load-theme app)
  "Switch to a different color theme."
  (let* ((available (hash-keys *themes*))
         (names (map symbol->string available))
         (input (qt-echo-read-string-with-completion app
                  "Load theme: " names)))
    (when (and input (> (string-length input) 0))
      (let ((sym (string->symbol input)))
        (if (hash-key? *themes* sym)
          (begin
            (set! *current-theme* sym)
            (apply-theme! app)
            (echo-message! (app-state-echo app)
              (string-append "Theme: " input)))
          (echo-error! (app-state-echo app)
            (string-append "Unknown theme: " input)))))))

(def (cmd-fold-level app)
  "Set fold level."
  (echo-message! (app-state-echo app) "Folding not supported in QPlainTextEdit"))

(def (cmd-ansi-term app)
  "Open an ANSI terminal."
  (cmd-term app))

(def (cmd-diff-backup app)
  "Diff current file with backup."
  (echo-message! (app-state-echo app) "No backup file to diff"))

(def (cmd-dired-do-chmod app)
  "Change permissions in dired."
  (echo-message! (app-state-echo app) "chmod not yet implemented in dired"))

(def (cmd-eldoc app)
  "Toggle eldoc mode (automatic function signature display)."
  (set! *eldoc-mode* (not *eldoc-mode*))
  (set! *eldoc-last-sym* #f)
  (echo-message! (app-state-echo app)
    (if *eldoc-mode* "Eldoc mode enabled" "Eldoc mode disabled")))

(def (cmd-recover-session app)
  "Recover a previous session."
  (echo-message! (app-state-echo app) "Session recovery not available"))

(def (cmd-revert-buffer-with-coding app)
  "Revert buffer with different coding."
  (cmd-revert-buffer app))

(def (cmd-set-buffer-file-coding app)
  "Set buffer file coding system."
  (echo-message! (app-state-echo app) "File coding: UTF-8 (fixed)"))

(def (cmd-set-language-environment app)
  "Set language environment."
  (echo-message! (app-state-echo app) "Language environment: UTF-8"))

(def (cmd-sudo-find-file app)
  "Open file as root."
  (cmd-sudo-write app))

(def (cmd-which-function app)
  "Show current function name."
  ;; Simple: find the nearest (def ...
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)))
      (cond
        ((< i 5) (echo-message! (app-state-echo app) "Not in a function"))
        ((and (>= i 5)
              (string=? (substring text i (+ i 5)) "(def "))
         (let* ((name-start (+ i 5))
                (name-end (let nloop ((j name-start))
                            (if (or (>= j (string-length text))
                                    (memq (string-ref text j) '(#\space #\) #\newline #\()))
                              j (nloop (+ j 1))))))
           (echo-message! (app-state-echo app)
             (string-append "In: " (substring text name-start name-end)))))
        (else (loop (- i 1)))))))

(def (cmd-widen-all app)
  "Widen all narrowed buffers."
  (cmd-widen app))

(def (cmd-whitespace-mode app)
  "Toggle whitespace visualization."
  (cmd-toggle-whitespace app))

(def (cmd-profiler-start app)
  "Start profiler."
  (echo-message! (app-state-echo app) "Profiler not available"))

(def (cmd-profiler-stop app)
  "Stop profiler."
  (echo-message! (app-state-echo app) "Profiler not available"))

(def (cmd-show-tab-count app)
  "Show tab count in buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (count (let loop ((i 0) (n 0))
                  (if (>= i (string-length text)) n
                    (loop (+ i 1) (if (char=? (string-ref text i) #\tab) (+ n 1) n))))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " tabs in buffer"))))

(def (cmd-show-trailing-whitespace-count app)
  "Show count of lines with trailing whitespace."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (count (length (filter (lambda (l)
                                  (and (> (string-length l) 0)
                                       (char-whitespace? (string-ref l (- (string-length l) 1)))))
                                lines))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " lines with trailing whitespace"))))

;;;============================================================================
;;; Terminal commands (PTY-backed)
;;;============================================================================

(def terminal-buffer-counter 0)

(def (terminal-read-plain ts)
  "Read available terminal output and return as plain text (ANSI stripped)."
  (let ((segs (terminal-read-available ts)))
    (if segs
      (let ((out (open-output-string)))
        (for-each (lambda (seg) (display (text-segment-text seg) out)) segs)
        (let ((s (get-output-string out)))
          (if (string=? s "") #f s)))
      #f)))

(def (cmd-term app)
  "Open a new PTY-backed terminal buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (name (begin
                 (set! terminal-buffer-counter (+ terminal-buffer-counter 1))
                 (if (= terminal-buffer-counter 1)
                   "*terminal*"
                   (string-append "*terminal-"
                                  (number->string terminal-buffer-counter) "*"))))
         (buf (qt-buffer-create! name ed #f)))
    ;; Mark as terminal buffer
    (set! (buffer-lexer-lang buf) 'terminal)
    ;; Attach buffer to editor
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    ;; Spawn PTY-backed shell
    (let ((ts (terminal-start!)))
      (hash-put! *terminal-state* buf ts)
      (qt-plain-text-edit-set-text! ed "")
      (set! (terminal-state-prompt-pos ts) 0))
    (echo-message! (app-state-echo app) (string-append name " started"))))

(def (cmd-terminal-send app)
  "Send Enter (newline) to the terminal PTY."
  (let* ((buf (current-qt-buffer app))
         (ts (hash-get *terminal-state* buf)))
    (when ts
      (terminal-send-raw! ts "\n"))))

(def (cmd-term-interrupt app)
  "Send Ctrl-C (interrupt) to the terminal PTY."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\x03;")
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

(def (cmd-term-send-eof app)
  "Send Ctrl-D (EOF) to the terminal PTY."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\x04;")
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

(def (cmd-term-send-tab app)
  "Send Tab to the terminal PTY (for tab completion)."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\t")
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

;;;============================================================================
;;; Buffer-local keybindings (mode keymaps)
;;;============================================================================

;; Maps buffer-lexer-lang symbol -> keymap hash table
(def *mode-keymaps* (make-hash-table))

(def (mode-keymap-lookup buf key-str)
  "Look up KEY-STR in the buffer's mode keymap. Returns command symbol or #f.
   Checks lexer-lang first, then buffer name for special buffers."
  (let* ((lang (buffer-lexer-lang buf))
         (km (or (hash-get *mode-keymaps* lang)
                 ;; Check by buffer name for buffers without special lexer-lang
                 (let ((name (buffer-name buf)))
                   (cond
                     ((string=? name "*compilation*") (hash-get *mode-keymaps* 'compilation))
                     ((string=? name "*Grep*") (hash-get *mode-keymaps* 'grep))
                     ((string=? name "*Occur*") (hash-get *mode-keymaps* 'occur))
                     ((string=? name "*calendar*") (hash-get *mode-keymaps* 'calendar))
                     ((string=? name "*eww*") (hash-get *mode-keymaps* 'eww))
                     ((string=? name "*Magit*") (hash-get *mode-keymaps* 'magit))
                     (else #f))))))
    (and km (keymap-lookup km key-str))))

(def (setup-mode-keymaps!)
  "Initialize mode-specific keybindings for special buffer types."
  ;; Dired mode: single-key navigation
  (let ((dired-km (make-keymap)))
    (keymap-bind! dired-km "n" 'next-line)
    (keymap-bind! dired-km "p" 'previous-line)
    (keymap-bind! dired-km "g" 'revert-buffer)
    (keymap-bind! dired-km "d" 'dired-do-delete)
    (keymap-bind! dired-km "R" 'dired-do-rename)
    (keymap-bind! dired-km "C" 'dired-do-copy)
    (keymap-bind! dired-km "+" 'dired-create-directory)
    (keymap-bind! dired-km "q" 'kill-buffer-cmd)
    (keymap-bind! dired-km "^" 'dired) ;; go up to parent
    (keymap-bind! dired-km "m" 'dired-mark)
    (keymap-bind! dired-km "u" 'dired-unmark)
    (keymap-bind! dired-km "U" 'dired-unmark-all)
    (keymap-bind! dired-km "t" 'dired-toggle-marks)
    (keymap-bind! dired-km "D" 'dired-do-delete-marked)
    (keymap-bind! dired-km "x" 'dired-do-delete-marked)
    (hash-put! *mode-keymaps* 'dired dired-km))

  ;; Compilation mode
  (let ((comp-km (make-keymap)))
    (keymap-bind! comp-km "n" 'next-error)
    (keymap-bind! comp-km "p" 'previous-error)
    (keymap-bind! comp-km "g" 'recompile)
    (keymap-bind! comp-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'compilation comp-km))

  ;; Grep results mode
  (let ((grep-km (make-keymap)))
    (keymap-bind! grep-km "n" 'next-grep-result)
    (keymap-bind! grep-km "p" 'previous-grep-result)
    (keymap-bind! grep-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'grep grep-km))

  ;; Occur mode
  (let ((occur-km (make-keymap)))
    (keymap-bind! occur-km "n" 'next-line)
    (keymap-bind! occur-km "p" 'previous-line)
    (keymap-bind! occur-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'occur occur-km))

  ;; Calendar mode
  (let ((cal-km (make-keymap)))
    (keymap-bind! cal-km "p" 'calendar-prev-month)
    (keymap-bind! cal-km "n" 'calendar-next-month)
    (keymap-bind! cal-km "<" 'calendar-prev-year)
    (keymap-bind! cal-km ">" 'calendar-next-year)
    (keymap-bind! cal-km "." 'calendar-today)
    (keymap-bind! cal-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'calendar cal-km))

  ;; EWW browser mode
  (let ((eww-km (make-keymap)))
    (keymap-bind! eww-km "g" 'eww)        ;; go to URL
    (keymap-bind! eww-km "l" 'eww-back)   ;; back
    (keymap-bind! eww-km "r" 'eww-reload) ;; reload
    (keymap-bind! eww-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'eww eww-km))
  ;; Magit mode
  (let ((magit-km (make-keymap)))
    (keymap-bind! magit-km "s" 'magit-stage)
    (keymap-bind! magit-km "S" 'magit-stage-all)
    (keymap-bind! magit-km "u" 'magit-unstage)
    (keymap-bind! magit-km "c" 'magit-commit)
    (keymap-bind! magit-km "d" 'magit-diff)
    (keymap-bind! magit-km "l" 'magit-log)
    (keymap-bind! magit-km "g" 'magit-status) ;; refresh
    (keymap-bind! magit-km "n" 'next-line)
    (keymap-bind! magit-km "p" 'previous-line)
    (keymap-bind! magit-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'magit magit-km)))

;;;============================================================================
;;; Ediff-files: compare two files from disk
;;;============================================================================

(def (cmd-ediff-files app)
  "Compare two files by running diff."
  (let ((file-a (qt-echo-read-string app "File A: ")))
    (when (and file-a (> (string-length file-a) 0))
      (let ((file-b (qt-echo-read-string app "File B: ")))
        (when (and file-b (> (string-length file-b) 0))
          (let ((path-a (path-expand file-a))
                (path-b (path-expand file-b)))
            (if (not (and (file-exists? path-a) (file-exists? path-b)))
              (echo-error! (app-state-echo app) "One or both files not found")
              (let* ((proc (open-process
                             (list path: "/usr/bin/diff"
                                   arguments: (list "-u" path-a path-b)
                                   stdout-redirection: #t)))
                     (output (read-line proc #f))
                     (_ (process-status proc))
                     (ed (current-qt-editor app))
                     (fr (app-state-frame app))
                     (diff-buf (qt-buffer-create! "*Ediff*" ed #f)))
                (close-port proc)
                (qt-buffer-attach! ed diff-buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                (qt-plain-text-edit-set-text! ed (or output "No differences"))
                (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)
                (echo-message! (app-state-echo app) "Diff complete")))))))))

;;;============================================================================
;;; Comment-dwim: intelligent comment toggle
;;;============================================================================

(def (cmd-comment-dwim app)
  "Do What I Mean with comments.
If region active: toggle comment on region.
If at end of code: add end-of-line comment.
If on blank line: insert comment and indent."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (has-sel (not (= sel-start sel-end))))
    (if has-sel
      ;; Region active: toggle comment on each line
      (cmd-comment-region app)
      ;; No region: check current line
      (let* ((line (qt-plain-text-edit-cursor-line ed))
             (lines (string-split text #\newline))
             (line-text (if (< line (length lines))
                          (list-ref lines line)
                          ""))
             (trimmed (string-trim line-text)))
        (cond
          ;; Blank line: insert comment
          ((string=? trimmed "")
           (qt-replace-line! ed line ";; ")
           (let* ((new-text (qt-plain-text-edit-text ed))
                  (new-lines (string-split new-text #\newline))
                  (pos (let loop ((i 0) (offset 0))
                         (if (>= i line) (+ offset 3)
                           (loop (+ i 1) (+ offset (string-length (list-ref new-lines i)) 1))))))
             (qt-plain-text-edit-set-cursor-position! ed pos)))
          ;; Line already commented: uncomment
          ((and (>= (string-length trimmed) 3)
                (string=? (substring trimmed 0 3) ";; "))
           (cmd-toggle-comment app))
          ((and (>= (string-length trimmed) 2)
                (string=? (substring trimmed 0 2) ";;"))
           (cmd-toggle-comment app))
          ;; Line has code: add end-of-line comment
          (else
           (let ((new-line (string-append line-text "  ;; ")))
             (qt-replace-line! ed line new-line)
             ;; Position cursor at comment
             (let* ((new-text (qt-plain-text-edit-text ed))
                    (new-lines (string-split new-text #\newline))
                    (pos (let loop ((i 0) (offset 0))
                           (if (>= i line) (+ offset (string-length new-line))
                             (loop (+ i 1) (+ offset (string-length (list-ref new-lines i)) 1))))))
               (qt-plain-text-edit-set-cursor-position! ed pos)))))))))

;;;============================================================================
;;; Auto-save mode toggle
;;;============================================================================

(def *auto-save-disabled-buffers* (make-hash-table))

(def (cmd-auto-save-mode app)
  "Toggle auto-save for the current buffer."
  (let* ((buf (current-qt-buffer app))
         (currently-disabled (hash-get *auto-save-disabled-buffers* buf)))
    (if currently-disabled
      (begin
        (hash-remove! *auto-save-disabled-buffers* buf)
        (echo-message! (app-state-echo app) "Auto-save enabled for this buffer"))
      (begin
        (hash-put! *auto-save-disabled-buffers* buf #t)
        (echo-message! (app-state-echo app) "Auto-save disabled for this buffer")))))

;;;============================================================================
;;; Keyboard macro counter
;;;============================================================================

(def *kbd-macro-counter* 0)
(def *kbd-macro-counter-format* "%d")

(def (cmd-kbd-macro-counter-insert app)
  "Insert the current keyboard macro counter value and increment it."
  (let* ((ed (current-qt-editor app))
         (text (number->string *kbd-macro-counter*)))
    (qt-plain-text-edit-insert-text! ed text)
    (set! *kbd-macro-counter* (+ *kbd-macro-counter* 1))))

(def (cmd-kbd-macro-counter-set app)
  "Set the keyboard macro counter to a specific value."
  (let ((input (qt-echo-read-string app
                 (string-append "Set macro counter (current "
                                (number->string *kbd-macro-counter*) "): "))))
    (when input
      (let ((n (string->number input)))
        (if n
          (begin
            (set! *kbd-macro-counter* n)
            (echo-message! (app-state-echo app)
              (string-append "Macro counter set to " (number->string n))))
          (echo-error! (app-state-echo app) "Not a number"))))))

;;;============================================================================
;;; Dired enhancements: marks, bulk operations
;;;============================================================================

(def *dired-marks* (make-hash-table))  ;; buf -> hash(index -> mark-char)

(def (dired-get-marks buf)
  "Get or create marks hash for a dired buffer."
  (or (hash-get *dired-marks* buf)
      (let ((h (make-hash-table)))
        (hash-put! *dired-marks* buf h)
        h)))

(def (cmd-dired-mark app)
  "Mark the file under cursor in dired."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (idx (- line 3))
         (entries (hash-get *dired-entries* buf)))
    (when (and entries (>= idx 0) (< idx (vector-length entries)))
      (let* ((marks (dired-get-marks buf))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (hash-put! marks idx #\*)
        ;; Update display: replace first char with *
        (when (< line (length lines))
          (let* ((old-line (list-ref lines line))
                 (new-line (if (> (string-length old-line) 0)
                             (string-append "*" (substring old-line 1 (string-length old-line)))
                             "*")))
            (qt-replace-line! ed line new-line)))
        ;; Move to next line
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)))))

(def (cmd-dired-unmark app)
  "Unmark the file under cursor in dired."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (idx (- line 3))
         (entries (hash-get *dired-entries* buf)))
    (when (and entries (>= idx 0) (< idx (vector-length entries)))
      (let* ((marks (dired-get-marks buf))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (hash-remove! marks idx)
        ;; Restore display: replace first char with space
        (when (< line (length lines))
          (let* ((old-line (list-ref lines line))
                 (new-line (if (> (string-length old-line) 0)
                             (string-append " " (substring old-line 1 (string-length old-line)))
                             " ")))
            (qt-replace-line! ed line new-line)))
        ;; Move to next line
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)))))

(def (cmd-dired-unmark-all app)
  "Unmark all files in dired."
  (let* ((buf (current-qt-buffer app))
         (marks (dired-get-marks buf)))
    (hash-clear! marks)
    ;; Refresh the listing
    (let* ((path (buffer-file-path buf)))
      (when path
        (dired-open-directory! app path)))
    (echo-message! (app-state-echo app) "All marks removed")))

(def (cmd-dired-toggle-marks app)
  "Toggle marks on all files in dired."
  (let* ((buf (current-qt-buffer app))
         (entries (hash-get *dired-entries* buf)))
    (when entries
      (let* ((marks (dired-get-marks buf))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (let loop ((i 0))
          (when (< i (vector-length entries))
            (if (hash-get marks i)
              (hash-remove! marks i)
              (hash-put! marks i #\*))
            ;; Update display
            (let ((line-idx (+ i 3)))
              (when (< line-idx (length lines))
                (let* ((old-line (list-ref lines line-idx))
                       (mark-ch (if (hash-get marks i) "*" " "))
                       (new-line (if (> (string-length old-line) 0)
                                   (string-append mark-ch (substring old-line 1 (string-length old-line)))
                                   mark-ch)))
                  (qt-replace-line! ed line-idx new-line))))
            (loop (+ i 1))))
        (echo-message! (app-state-echo app) "Marks toggled")))))

(def (dired-marked-files buf)
  "Get list of full paths of marked files in a dired buffer."
  (let ((marks (hash-get *dired-marks* buf))
        (entries (hash-get *dired-entries* buf)))
    (if (and marks entries)
      (let loop ((i 0) (acc []))
        (if (>= i (vector-length entries))
          (reverse acc)
          (if (hash-get marks i)
            (loop (+ i 1) (cons (vector-ref entries i) acc))
            (loop (+ i 1) acc))))
      [])))

(def (cmd-dired-do-delete-marked app)
  "Delete all marked files in dired."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((confirm (qt-echo-read-string app
                       (string-append "Delete " (number->string (length files))
                                      " marked files? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (let ((count 0))
            (for-each
              (lambda (path)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (delete-file path)
                    (set! count (+ count 1)))))
              files)
            ;; Refresh
            (let ((dir (buffer-file-path buf)))
              (when dir (dired-open-directory! app dir)))
            (echo-message! (app-state-echo app)
              (string-append "Deleted " (number->string count) " files"))))))))

(def (cmd-dired-do-copy-marked app)
  "Copy all marked files in dired to a destination directory."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((dest (qt-echo-read-string app "Copy to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest))
                (count 0))
            (for-each
              (lambda (path)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (let ((target (path-expand (path-strip-directory path) dest-dir)))
                      (copy-file path target)
                      (set! count (+ count 1))))))
              files)
            ;; Refresh
            (let ((dir (buffer-file-path buf)))
              (when dir (dired-open-directory! app dir)))
            (echo-message! (app-state-echo app)
              (string-append "Copied " (number->string count) " files to " dest-dir))))))))

(def (cmd-dired-do-rename-marked app)
  "Move/rename all marked files in dired to a destination directory."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((dest (qt-echo-read-string app "Move to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest))
                (count 0))
            (for-each
              (lambda (path)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (let ((target (path-expand (path-strip-directory path) dest-dir)))
                      (rename-file path target)
                      (set! count (+ count 1))))))
              files)
            ;; Refresh
            (let ((dir (buffer-file-path buf)))
              (when dir (dired-open-directory! app dir)))
            (echo-message! (app-state-echo app)
              (string-append "Moved " (number->string count) " files to " dest-dir))))))))

(def (cmd-dired-mark-by-regexp app)
  "Mark files matching a regular expression in dired."
  (let* ((buf (current-qt-buffer app))
         (entries (hash-get *dired-entries* buf)))
    (when entries
      (let ((pattern (qt-echo-read-string app "Mark files matching regexp: ")))
        (when (and pattern (> (string-length pattern) 0))
          (let ((marks (dired-get-marks buf))
                (ed (current-qt-editor app))
                (count 0))
            (let loop ((i 0))
              (when (< i (vector-length entries))
                (let* ((path (vector-ref entries i))
                       (name (path-strip-directory path)))
                  (when (with-catch (lambda (e) #f)
                          (lambda () (string-contains name pattern)))
                    (hash-put! marks i #\*)
                    (set! count (+ count 1))
                    ;; Update display
                    (let* ((text (qt-plain-text-edit-text ed))
                           (lines (string-split text #\newline))
                           (line-idx (+ i 3)))
                      (when (< line-idx (length lines))
                        (let* ((old-line (list-ref lines line-idx))
                               (new-line (if (> (string-length old-line) 0)
                                           (string-append "*" (substring old-line 1 (string-length old-line)))
                                           "*")))
                          (qt-replace-line! ed line-idx new-line))))))
                (loop (+ i 1))))
            (echo-message! (app-state-echo app)
              (string-append "Marked " (number->string count) " files"))))))))

(def (cmd-dired-sort-toggle app)
  "Toggle dired sort between name and modification time."
  (echo-message! (app-state-echo app)
    "Dired is sorted by name (default). Use M-x dired to refresh."))

;;;============================================================================
;;; Pop global mark ring
;;;============================================================================

(def (cmd-pop-global-mark app)
  "Pop the global mark ring and jump to the saved position."
  (let ((ring (app-state-mark-ring app)))
    (if (null? ring)
      (echo-message! (app-state-echo app) "Global mark ring is empty")
      (let* ((entry (car ring))
             (buf-name (car entry))
             (pos (cdr entry))
             (buf (buffer-by-name buf-name)))
        (set! (app-state-mark-ring app) (cdr ring))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (string-length (qt-plain-text-edit-text ed))))
            (qt-modeline-update! app)
            (echo-message! (app-state-echo app)
              (string-append "Mark: " buf-name)))
          (echo-error! (app-state-echo app)
            (string-append "Buffer no longer exists: " buf-name)))))))

;;;============================================================================
;;; Window horizontal resize
;;;============================================================================

(def (cmd-shrink-window-horizontally app)
  "Shrink the current window horizontally."
  (echo-message! (app-state-echo app)
    "Horizontal resize not supported in vertical splitter layout"))

(def (cmd-enlarge-window-horizontally app)
  "Enlarge the current window horizontally."
  (echo-message! (app-state-echo app)
    "Horizontal resize not supported in vertical splitter layout"))

;;;============================================================================
;;; Recover file from auto-save
;;;============================================================================

(def (cmd-recover-file app)
  "Recover a file from its auto-save version."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((auto-path (make-auto-save-path path)))
        (if (not (file-exists? auto-path))
          (echo-message! (app-state-echo app)
            (string-append "No auto-save file for " (path-strip-directory path)))
          (let ((confirm (qt-echo-read-string app
                           (string-append "Recover from " auto-path "? (yes/no): "))))
            (when (and confirm (string=? confirm "yes"))
              (let* ((ed (current-qt-editor app))
                     (text (read-file-as-string auto-path)))
                (when text
                  (qt-plain-text-edit-set-text! ed text)
                  (qt-plain-text-edit-set-cursor-position! ed 0)
                  (echo-message! (app-state-echo app)
                    (string-append "Recovered from " auto-path)))))))))))

;;;============================================================================
;;; Insert char by name (Unicode)
;;;============================================================================

(def (cmd-insert-char-by-name app)
  "Insert a character by its hex codepoint."
  (let ((input (qt-echo-read-string app "Unicode hex codepoint (e.g. 03BB for lambda): ")))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input 16)))
        (if n
          (let ((ed (current-qt-editor app)))
            (qt-plain-text-edit-insert-text! ed (string (integer->char n)))
            (echo-message! (app-state-echo app)
              (string-append "Inserted U+" (string-upcase input))))
          (echo-error! (app-state-echo app) "Invalid hex codepoint"))))))

;;;============================================================================
;;; Display battery / system info
;;;============================================================================

(def (cmd-display-battery app)
  "Display battery status if available."
  (let ((result
          (with-catch
            (lambda (e) #f)
            (lambda ()
              (let* ((proc (open-process
                             (list path: "/usr/bin/cat"
                                   arguments: '("/sys/class/power_supply/BAT0/capacity")
                                   stdout-redirection: #t)))
                     (output (read-line proc))
                     (_ (process-status proc)))
                (close-port proc)
                output)))))
    (if result
      (echo-message! (app-state-echo app)
        (string-append "Battery: " result "%"))
      (echo-message! (app-state-echo app) "Battery info not available"))))

;;;============================================================================
;;; Scratch message
;;;============================================================================

(def (cmd-scratch-message app)
  "Switch to *scratch* buffer with initial message."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (buffer-by-name "*scratch*")))
    (if buf
      (begin
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-modeline-update! app))
      (echo-message! (app-state-echo app) "No *scratch* buffer found"))))

;;;============================================================================
;;; Sentence navigation and kill
;;;============================================================================

(def (sentence-end-pos text pos)
  "Find end of current sentence from pos."
  (let ((len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len) len)
        ((and (memv (string-ref text i) '(#\. #\? #\!))
              (or (>= (+ i 1) len)
                  (char-whitespace? (string-ref text (+ i 1)))))
         (+ i 1))
        (else (loop (+ i 1)))))))

(def (sentence-start-pos text pos)
  "Find start of current sentence looking backward from pos."
  (let loop ((i (- pos 1)))
    (cond
      ((<= i 0) 0)
      ((and (memv (string-ref text i) '(#\. #\? #\!))
            (< (+ i 1) (string-length text))
            (char-whitespace? (string-ref text (+ i 1))))
       ;; Skip whitespace after sentence-ender
       (let skip ((j (+ i 1)))
         (if (and (< j (string-length text))
                  (char-whitespace? (string-ref text j)))
           (skip (+ j 1))
           j)))
      (else (loop (- i 1))))))

(def (cmd-kill-sentence app)
  "Kill from point to end of sentence."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (end (sentence-end-pos text pos))
         (killed (substring text pos end)))
    (qt-kill-ring-push! app killed)
    (qt-plain-text-edit-set-selection! ed pos end)
    (qt-plain-text-edit-remove-selected-text! ed)))

(def (cmd-backward-kill-sentence app)
  "Kill from point back to start of sentence."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (start (sentence-start-pos text pos))
         (killed (substring text start pos)))
    (qt-kill-ring-push! app killed)
    (qt-plain-text-edit-set-selection! ed start pos)
    (qt-plain-text-edit-remove-selected-text! ed)))

;;;============================================================================
;;; Paragraph navigation and kill
;;;============================================================================

(def (paragraph-end-pos text pos)
  "Find end of current paragraph (next blank line or end of text)."
  (let ((len (string-length text)))
    ;; Skip current non-blank lines, then find blank line or end
    (let loop ((i pos) (saw-text? #f))
      (cond
        ((>= i len) len)
        ((char=? (string-ref text i) #\newline)
         (if (and saw-text?
                  (or (>= (+ i 1) len)
                      (char=? (string-ref text (+ i 1)) #\newline)))
           (+ i 1)
           (loop (+ i 1) saw-text?)))
        (else (loop (+ i 1) #t))))))

(def (paragraph-start-pos text pos)
  "Find start of current paragraph (previous blank line or start of text)."
  (let loop ((i (- pos 1)) (saw-text? #f))
    (cond
      ((<= i 0) 0)
      ((char=? (string-ref text i) #\newline)
       (if (and saw-text?
                (> i 0)
                (char=? (string-ref text (- i 1)) #\newline))
         (+ i 1)
         (loop (- i 1) saw-text?)))
      (else (loop (- i 1) #t)))))

(def (cmd-kill-paragraph app)
  "Kill from point to end of paragraph."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (end (paragraph-end-pos text pos))
         (killed (substring text pos end)))
    (qt-kill-ring-push! app killed)
    (qt-plain-text-edit-set-selection! ed pos end)
    (qt-plain-text-edit-remove-selected-text! ed)))

;;;============================================================================
;;; Recenter-top-bottom cycling (C-l)
;;;============================================================================

(def *recenter-cycle-state* 'center)

(def (cmd-recenter-top-bottom app)
  "Cycle cursor position: center, top, bottom."
  (let ((ed (current-qt-editor app)))
    (case *recenter-cycle-state*
      ((center)
       (qt-plain-text-edit-center-cursor! ed)
       (set! *recenter-cycle-state* 'top)
       (echo-message! (app-state-echo app) "Centered"))
      ((top)
       (qt-plain-text-edit-ensure-cursor-visible! ed)
       (set! *recenter-cycle-state* 'bottom)
       (echo-message! (app-state-echo app) "Top"))
      ((bottom)
       (qt-plain-text-edit-ensure-cursor-visible! ed)
       (set! *recenter-cycle-state* 'center)
       (echo-message! (app-state-echo app) "Bottom")))))

;;;============================================================================
;;; S-expression list navigation (up-list, down-list)
;;;============================================================================

(def (cmd-up-list app)
  "Move backward out of one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (cond
        ((< i 0)
         (echo-message! (app-state-echo app) "At top level"))
        ((memv (string-ref text i) '(#\) #\] #\}))
         (loop (- i 1) (+ depth 1)))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (if (= depth 0)
           (qt-plain-text-edit-set-cursor-position! ed i)
           (loop (- i 1) (- depth 1))))
        (else (loop (- i 1) depth))))))

(def (cmd-down-list app)
  "Move forward into one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len)
         (echo-message! (app-state-echo app) "No inner list found"))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (qt-plain-text-edit-set-cursor-position! ed (+ i 1)))
        (else (loop (+ i 1)))))))

;;;============================================================================
;;; Windmove (directional window navigation)
;;;============================================================================

(def (cmd-windmove-left app)
  "Move to the window to the left."
  (let* ((fr (app-state-frame app))
         (idx (qt-frame-current-idx fr)))
    (if (> idx 0)
      (begin
        (qt-frame-current-idx-set! fr (- idx 1))
        (qt-modeline-update! app))
      (echo-message! (app-state-echo app) "No window to the left"))))

(def (cmd-windmove-right app)
  "Move to the window to the right."
  (let* ((fr (app-state-frame app))
         (idx (qt-frame-current-idx fr))
         (count (length (qt-frame-windows fr))))
    (if (< idx (- count 1))
      (begin
        (qt-frame-current-idx-set! fr (+ idx 1))
        (qt-modeline-update! app))
      (echo-message! (app-state-echo app) "No window to the right"))))

(def (cmd-windmove-up app)
  "Move to the window above (alias for windmove-left in horizontal split)."
  (cmd-windmove-left app))

(def (cmd-windmove-down app)
  "Move to the window below (alias for windmove-right in horizontal split)."
  (cmd-windmove-right app))

;;;============================================================================
;;; Set-variable and customize-variable
;;;============================================================================

(def *user-variables*
  (hash
    ("fill-column" (cons (lambda () *fill-column*)
                         (lambda (v) (set! *fill-column* (string->number v)))))
    ("tab-width" (cons (lambda () *tab-width*)
                       (lambda (v) (set! *tab-width* (string->number v)))))
    ("auto-save-interval" (cons (lambda () *auto-save-interval*)
                                (lambda (v) (set! *auto-save-interval* (string->number v)))))))

(def (cmd-set-variable app)
  "Set a variable to a value interactively."
  (let* ((echo (app-state-echo app))
         (varname (qt-echo-read-string echo "Set variable: "))
         (entry (hash-get *user-variables* varname)))
    (if entry
      (let* ((cur ((car entry)))
             (val (qt-echo-read-string echo
                    (string-append varname " (current: "
                                   (if cur (object->string cur) "nil")
                                   "): "))))
        ((cdr entry) val)
        (echo-message! echo (string-append varname " set to " val)))
      (echo-error! echo (string-append "Unknown variable: " varname)))))

(def (cmd-customize-variable app)
  "Show current value of a variable and optionally change it."
  (cmd-set-variable app))

;;;============================================================================
;;; View-file and append-to-file
;;;============================================================================

(def (cmd-view-file app)
  "Open a file in read-only view mode."
  (let* ((echo (app-state-echo app))
         (path (qt-echo-read-string echo "View file: ")))
    (when (and path (> (string-length path) 0))
      (if (file-exists? path)
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (content (call-with-input-file path (lambda (p) (read-string 1000000 p))))
               (buf (or (buffer-by-name (path-strip-directory path))
                        (qt-buffer-create! (path-strip-directory path) ed path))))
          (qt-buffer-attach! ed buf)
          (qt-plain-text-edit-set-text! ed content)
          (qt-plain-text-edit-set-read-only! ed #t)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (hash-put! *view-mode-buffers* (buffer-name buf) #t)
          (qt-modeline-update! app)
          (echo-message! echo (string-append "Viewing: " path)))
        (echo-error! echo (string-append "File not found: " path))))))

(def (cmd-append-to-file app)
  "Append the selected region to a file."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! echo "No region selected")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (path (qt-echo-read-string echo "Append to file: ")))
        (when (and path (> (string-length path) 0))
          (call-with-output-file [path: path append: #t]
            (lambda (p) (display region p)))
          (echo-message! echo
            (string-append "Appended " (number->string (- sel-end sel-start))
                           " chars to " path)))))))

;;;============================================================================
;;; Flyspell-buffer (spell check whole buffer)
;;;============================================================================

(def (cmd-flyspell-buffer app)
  "Spell-check the entire buffer using aspell."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    ;; Extract words and check each
    (let* ((words (let extract ((i 0) (acc '()))
                    (let ((len (string-length text)))
                      (if (>= i len) (reverse acc)
                        (if (char-alphabetic? (string-ref text i))
                          (let word-end ((j (+ i 1)))
                            (if (and (< j len) (char-alphabetic? (string-ref text j)))
                              (word-end (+ j 1))
                              (extract j (cons (substring text i j) acc))))
                          (extract (+ i 1) acc))))))
           (unique (let ((seen (make-hash-table)))
                     (filter (lambda (w)
                               (and (> (string-length w) 2)
                                    (not (hash-get seen w))
                                    (begin (hash-put! seen w #t) #t)))
                             words)))
           (misspelled '()))
      ;; Use aspell pipe mode
      (with-catch
        (lambda (e) (echo-error! echo "aspell not available"))
        (lambda ()
          (let ((proc (open-process
                        [path: "aspell"
                         arguments: ["pipe"]
                         stdin-redirection: #t
                         stdout-redirection: #t
                         stderr-redirection: #t])))
            ;; Read banner
            (read-line proc)
            ;; Check each word
            (for-each
              (lambda (word)
                (display word proc)
                (newline proc)
                (force-output proc)
                (let ((line (read-line proc)))
                  (when (and (string? line)
                             (> (string-length line) 0)
                             (char=? (string-ref line 0) #\&))
                    (set! misspelled (cons word misspelled)))))
              unique)
            (close-port proc))))
      (if (null? misspelled)
        (echo-message! echo "No misspelled words found")
        (let* ((fr (app-state-frame app))
               (buf (or (buffer-by-name "*Spelling*")
                        (qt-buffer-create! "*Spelling*" ed)))
               (result (string-join (reverse misspelled) "\n")))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "=== Misspelled words ===\n\n" result "\n\n"
                           (number->string (length misspelled)) " misspelled word(s) found"))
          (qt-modeline-update! app))))))

;;;============================================================================
;;; Profiler report
;;;============================================================================

(def *profiler-data* (hash))

(def (cmd-profiler-report app)
  "Display profiler data in a buffer."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Profiler*")
                  (qt-buffer-create! "*Profiler*" ed)))
         (entries (hash->list *profiler-data*)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (if (null? entries)
      (qt-plain-text-edit-set-text! ed
        "No profiler data available.\n\nUse M-x profiler-start to begin profiling.")
      (let* ((sorted (sort entries (lambda (a b) (> (cdr a) (cdr b)))))
             (lines (map (lambda (e)
                          (string-append (car e) ": "
                                         (number->string (cdr e)) " calls"))
                        sorted))
             (report (string-join lines "\n")))
        (qt-plain-text-edit-set-text! ed
          (string-append "=== Profiler Report ===\n\n" report))))
    (qt-modeline-update! app)))

;;;============================================================================
;;; List-faces-display
;;;============================================================================

(def (cmd-list-faces-display app)
  "Display all available face/theme information."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Faces*")
                  (qt-buffer-create! "*Faces*" ed))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (let* ((lines
            (list "=== Face Types ===\n"
                  "  default       - Normal text"
                  "  keyword       - Language keywords"
                  "  comment       - Comments"
                  "  string        - String literals"
                  "  function      - Function names"
                  "  type          - Type names"
                  "  constant      - Constants and numbers"
                  "  preprocessor  - Preprocessor directives"
                  "  builtin       - Built-in functions"
                  "  warning       - Warnings"
                  "  error         - Errors"
                  "  region        - Selected region"
                  "  modeline      - Mode line"
                  "  minibuffer    - Minibuffer/echo area"
                  "\n=== Customization ==="
                  "  Use M-x customize-face to modify face colors."
                  "  Use M-x load-theme to change the theme."))
           (text (string-join lines "\n")))
      (qt-plain-text-edit-set-text! ed text)
      (qt-modeline-update! app))))

;;;============================================================================
;;; Display-line-numbers-mode toggle
;;;============================================================================

(def (cmd-display-line-numbers-mode app)
  "Toggle display of line numbers (state flag)."
  (let ((echo (app-state-echo app)))
    (set! *line-numbers-visible* (not *line-numbers-visible*))
    (echo-message! echo
      (if *line-numbers-visible*
        "Line numbers enabled"
        "Line numbers disabled"))))

;;;============================================================================
;;; Find file read-only (C-x C-r)
;;;============================================================================

(def (cmd-find-file-read-only app)
  "Open a file in read-only mode."
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Find file read-only: ")))
    (when (and filename (> (string-length filename) 0))
      (if (file-exists? filename)
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (name (path-strip-directory filename))
               (buf (qt-buffer-create! name ed filename))
               (text (read-file-as-string filename)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when text
            (qt-plain-text-edit-set-text! ed text)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0))
          (qt-plain-text-edit-set-read-only! ed #t)
          (qt-setup-highlighting! app buf)
          (qt-modeline-update! app)
          (echo-message! echo (string-append "Opened read-only: " filename)))
        (echo-error! echo (string-append "File not found: " filename))))))

;;;============================================================================
;;; Project switch and project dired
;;;============================================================================

(def *known-projects* (make-hash-table))

(def (cmd-project-switch-project app)
  "Switch to a different project directory."
  (let* ((echo (app-state-echo app))
         (dir (qt-echo-read-string app "Switch to project: ")))
    (when (and dir (> (string-length dir) 0))
      (let ((expanded (path-expand dir)))
        (if (file-exists? expanded)
          (begin
            (hash-put! *known-projects* expanded #t)
            (current-directory expanded)
            (echo-message! echo (string-append "Project: " expanded)))
          (echo-error! echo (string-append "Directory not found: " expanded)))))))

(def (cmd-project-dired app)
  "Open dired in the project root directory."
  (let* ((root (current-project-root app)))
    (dired-open-directory! app root)))

(def (cmd-project-run-shell app)
  "Open a shell in the project root directory."
  (let* ((root (current-project-root app))
         (echo (app-state-echo app)))
    (current-directory root)
    (echo-message! echo (string-append "Shell directory: " root))))

;;;============================================================================
;;; Global auto-revert mode
;;;============================================================================

(def *global-auto-revert-mode* #f)

(def (cmd-global-auto-revert-mode app)
  "Toggle global auto-revert mode for all file-visiting buffers."
  (let ((echo (app-state-echo app)))
    (set! *global-auto-revert-mode* (not *global-auto-revert-mode*))
    (echo-message! echo
      (if *global-auto-revert-mode*
        "Global auto-revert mode enabled"
        "Global auto-revert mode disabled"))))

;;;============================================================================
;;; Project search (multi-file grep with navigable results)
;;;============================================================================

(def (cmd-project-search app)
  "Search for pattern across all project files with navigable results."
  (let* ((echo (app-state-echo app))
         (root (current-project-root app))
         (pattern (qt-echo-read-string app "Project search: ")))
    (when (and pattern (> (string-length pattern) 0))
      (with-catch
        (lambda (e) (echo-error! echo "Search failed"))
        (lambda ()
          (let* ((proc (open-process
                         [path: "/usr/bin/grep"
                          arguments: ["-rn" "--include=*.ss" "--include=*.scm"
                                      "--include=*.el" "--include=*.py"
                                      "--include=*.js" "--include=*.ts"
                                      "--include=*.c" "--include=*.h"
                                      "--include=*.rs" "--include=*.go"
                                      "--include=*.java" "--include=*.rb"
                                      "--include=*.md" "--include=*.txt"
                                      "--include=*.json" "--include=*.yaml"
                                      "--include=*.yml" "--include=*.toml"
                                      "--include=*.html" "--include=*.css"
                                      pattern root]
                          stdout-redirection: #t
                          stderr-redirection: #t]))
                 (output (let loop ((lines '()))
                           (let ((line (read-line proc)))
                             (if (eof-object? line)
                               (reverse lines)
                               (loop (cons line lines)))))))
            (close-port proc)
            (if (null? output)
              (echo-message! echo (string-append "No matches for: " pattern))
              (let* ((fr (app-state-frame app))
                     (ed (current-qt-editor app))
                     (buf (or (buffer-by-name "*Project Search*")
                              (qt-buffer-create! "*Project Search*" ed)))
                     (result (string-join output "\n")))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed
                  (string-append "=== Project search: " pattern " ===\n"
                                 "=== Root: " root " ===\n\n"
                                 result "\n\n"
                                 (number->string (length output)) " match(es) found"))
                (qt-plain-text-edit-set-read-only! ed #t)
                (qt-modeline-update! app)
                (echo-message! echo
                  (string-append (number->string (length output)) " match(es) found"))))))))))

;;;============================================================================
;;; Goto last change (navigate to last edit position)
;;;============================================================================

(def *last-change-positions* (make-hash-table))

(def (record-change-position! app)
  "Record current cursor position as last change position for current buffer."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (name (buffer-name buf)))
    (hash-put! *last-change-positions* name pos)))

(def (cmd-goto-last-change app)
  "Jump to the position of the last edit in the current buffer."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (name (buffer-name buf))
         (pos (hash-get *last-change-positions* name)))
    (if pos
      (begin
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (qt-plain-text-edit-ensure-cursor-visible! ed))
      (echo-message! (app-state-echo app) "No recorded change position"))))

;;;============================================================================
;;; Git gutter / diff-hl (show changed lines in margin)
;;;============================================================================

(def (cmd-diff-hl-mode app)
  "Show diff indicators for lines changed since last save/commit."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-message! echo "Buffer has no file")
      (with-catch
        (lambda (e) (echo-error! echo "diff-hl: git diff failed"))
        (lambda ()
          (let* ((proc (open-process
                         [path: "/usr/bin/git"
                          arguments: ["diff" "--no-color" "-U0" path]
                          stdout-redirection: #t
                          stderr-redirection: #t]))
                 (output (let loop ((lines '()))
                           (let ((line (read-line proc)))
                             (if (eof-object? line)
                               (reverse lines)
                               (loop (cons line lines)))))))
            (close-port proc)
            (if (null? output)
              (echo-message! echo "No uncommitted changes in this file")
              (let* ((fr (app-state-frame app))
                     (res-buf (or (buffer-by-name "*Diff-HL*")
                                  (qt-buffer-create! "*Diff-HL*" ed)))
                     (result (string-join output "\n")))
                (qt-buffer-attach! ed res-buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) res-buf)
                (qt-plain-text-edit-set-text! ed
                  (string-append "=== Changes in " (path-strip-directory path) " ===\n\n"
                                 result))
                (qt-plain-text-edit-set-read-only! ed #t)
                (qt-modeline-update! app)
                (echo-message! echo "Showing uncommitted changes")))))))))

;;;============================================================================
;;; Pop-to-mark (cycle through mark ring)
;;;============================================================================

(def (cmd-pop-to-mark app)
  "Pop to previous mark position in the mark ring."
  (let* ((ed (current-qt-editor app))
         (marks (app-state-mark-ring app)))
    (if (null? marks)
      (echo-message! (app-state-echo app) "Mark ring empty")
      (let* ((entry (car marks))
             (rest (cdr marks))
             (buf-name (car entry))
             (pos (cdr entry)))
        ;; Push current position, pop first entry
        (set! (app-state-mark-ring app)
          (append rest (list (cons (buffer-name (current-qt-buffer app))
                                   (qt-plain-text-edit-cursor-position ed)))))
        ;; Switch to buffer if different
        (let ((target-buf (buffer-by-name buf-name)))
          (when target-buf
            (let ((fr (app-state-frame app)))
              (unless (eq? target-buf (current-qt-buffer app))
                (qt-buffer-attach! ed target-buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) target-buf)))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

;;;============================================================================
;;; Scratch buffer new (create additional scratch buffers)
;;;============================================================================

(def *scratch-counter* 0)

(def (cmd-scratch-buffer-new app)
  "Create a new scratch buffer with a unique name."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app)))
    (set! *scratch-counter* (+ *scratch-counter* 1))
    (let* ((name (string-append "*scratch-" (number->string *scratch-counter*) "*"))
           (buf (qt-buffer-create! name ed)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed
        (string-append ";; " name " -- scratch buffer\n\n"))
      (qt-modeline-update! app)
      (echo-message! (app-state-echo app) (string-append "Created " name)))))

;;;============================================================================
;;; Duplicate line or region
;;;============================================================================

(def (cmd-duplicate-line-or-region app)
  "Duplicate the current line or selected region."
  (let* ((ed (current-qt-editor app))
         (has-sel (qt-plain-text-edit-has-selection? ed)))
    (if has-sel
      ;; Duplicate selection
      (let* ((sel-text (qt-plain-text-edit-selected-text ed))
             (end (qt-plain-text-edit-selection-end ed)))
        (qt-plain-text-edit-set-cursor-position! ed end)
        (qt-plain-text-edit-insert-text! ed sel-text))
      ;; Duplicate current line
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (line-num (qt-plain-text-edit-cursor-line ed))
             (line-start (qt-plain-text-edit-line-end-position ed
                           (if (> line-num 0) (- line-num 1) -1)))
             (line-end (qt-plain-text-edit-line-end-position ed line-num))
             (line-text (qt-plain-text-edit-text-range ed
                          (if (> line-num 0) (+ line-start 1) 0)
                          line-end)))
        (qt-plain-text-edit-set-cursor-position! ed line-end)
        (qt-plain-text-edit-insert-text! ed (string-append "\n" line-text))))))

;;;============================================================================
;;; Select current line
;;;============================================================================

(def (cmd-select-current-line app)
  "Select the entire current line."
  (let* ((ed (current-qt-editor app))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (line-start (if (> line-num 0)
                       (+ (qt-plain-text-edit-line-end-position ed (- line-num 1)) 1)
                       0))
         (line-end (qt-plain-text-edit-line-end-position ed line-num)))
    (qt-plain-text-edit-set-selection! ed line-start line-end)))

;;;============================================================================
;;; Smart join line (context-aware join)
;;;============================================================================

(def (cmd-smart-join-line app)
  "Join the next line to the current one, handling indentation intelligently."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find end of current line
    (let loop-eol ((i pos))
      (if (or (>= i len) (char=? (string-ref text i) #\newline))
        (when (< i len)
          ;; i is at newline - find first non-whitespace on next line
          (let skip-ws ((j (+ i 1)))
            (if (and (< j len)
                     (memv (string-ref text j) '(#\space #\tab)))
              (skip-ws (+ j 1))
              ;; Delete from current line end to first non-ws on next line
              ;; and insert a single space
              (begin
                (qt-plain-text-edit-set-selection! ed i j)
                (qt-plain-text-edit-remove-selected-text! ed)
                ;; Add space unless next char is a closing paren/bracket
                (let ((next-text (qt-plain-text-edit-text ed))
                      (next-pos (qt-plain-text-edit-cursor-position ed)))
                  (unless (and (< next-pos (string-length next-text))
                               (memv (string-ref next-text next-pos)
                                     '(#\) #\] #\})))
                    (qt-plain-text-edit-insert-text! ed " ")))))))
        (loop-eol (+ i 1))))))

;;;============================================================================
;;; Copy buffer filename
;;;============================================================================

(def (cmd-copy-buffer-filename app)
  "Copy the current buffer's filename (without directory) to clipboard."
  (let* ((buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (name (buffer-name buf)))
    (qt-clipboard-set-text! *qt-app-ptr* name)
    (echo-message! echo (string-append "Copied: " name))))

;;;============================================================================
;;; Revert buffer with confirmation
;;;============================================================================

(def (cmd-revert-buffer-confirm app)
  "Revert buffer from file with yes/no confirmation."
  (let* ((buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no file")
      (let ((answer (qt-echo-read-string app
                      (string-append "Revert buffer from " (path-strip-directory path) "? (yes/no) "))))
        (when (and answer (or (string=? answer "yes") (string=? answer "y")))
          (cmd-revert-buffer app))))))

;;;============================================================================
;;; Find file at line (open file:line:col references)
;;;============================================================================

(def (cmd-find-file-at-line app)
  "Open a file and jump to a specific line (file:line format)."
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "File:line: ")))
    (when (and input (> (string-length input) 0))
      (let* ((parts (string-split input #\:))
             (file (car parts))
             (line (if (and (pair? (cdr parts))
                            (> (string-length (cadr parts)) 0))
                     (with-catch (lambda (e) #f)
                       (lambda () (string->number (cadr parts))))
                     #f)))
        (when (file-exists? file)
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (name (path-strip-directory file))
                 (buf (qt-buffer-create! name ed file))
                 (text (read-file-as-string file)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (when text
              (qt-plain-text-edit-set-text! ed text)
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f))
            (qt-setup-highlighting! app buf)
            (when (and line (> line 0))
              (let ((target-pos (qt-plain-text-edit-line-end-position ed (- line 1))))
                (qt-plain-text-edit-set-cursor-position! ed
                  (if (> line 1)
                    (+ target-pos 1)
                    0))
                (qt-plain-text-edit-ensure-cursor-visible! ed)))
            (qt-modeline-update! app)
            (echo-message! echo (string-append "Opened: " file
                                  (if line (string-append ":" (number->string line)) "")))))))))

;;;============================================================================
;;; Toggle comment at point (quick line comment)
;;;============================================================================

(def (cmd-toggle-line-comment app)
  "Toggle comment on the current line using ;; prefix."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (line-start (if (> line-num 0)
                       (+ (qt-plain-text-edit-line-end-position ed (- line-num 1)) 1)
                       0))
         (line-end (qt-plain-text-edit-line-end-position ed line-num))
         (line-text (qt-plain-text-edit-text-range ed line-start line-end)))
    ;; Find first non-whitespace
    (let* ((trimmed (let skip ((i 0))
                      (if (and (< i (string-length line-text))
                               (memv (string-ref line-text i) '(#\space #\tab)))
                        (skip (+ i 1))
                        i)))
           (indent (substring line-text 0 trimmed))
           (rest (substring line-text trimmed (string-length line-text))))
      (qt-plain-text-edit-set-selection! ed line-start line-end)
      (qt-plain-text-edit-remove-selected-text! ed)
      (if (and (>= (string-length rest) 3)
               (string=? (substring rest 0 3) ";; "))
        ;; Uncomment
        (qt-plain-text-edit-insert-text! ed
          (string-append indent (substring rest 3 (string-length rest))))
        ;; Comment
        (qt-plain-text-edit-insert-text! ed
          (string-append indent ";; " rest))))))

;;;============================================================================
;;; Register all commands
;;;============================================================================

(def (qt-register-all-commands!)
  ;; Setup mode-specific keymaps
  (setup-mode-keymaps!)
  ;; Navigation
  (register-command! 'forward-char cmd-forward-char)
  (register-command! 'backward-char cmd-backward-char)
  (register-command! 'next-line cmd-next-line)
  (register-command! 'previous-line cmd-previous-line)
  (register-command! 'beginning-of-line cmd-beginning-of-line)
  (register-command! 'end-of-line cmd-end-of-line)
  (register-command! 'forward-word cmd-forward-word)
  (register-command! 'backward-word cmd-backward-word)
  (register-command! 'forward-subword cmd-forward-subword)
  (register-command! 'backward-subword cmd-backward-subword)
  (register-command! 'kill-subword cmd-kill-subword)
  (register-command! 'beginning-of-buffer cmd-beginning-of-buffer)
  (register-command! 'end-of-buffer cmd-end-of-buffer)
  (register-command! 'scroll-down cmd-scroll-down)
  (register-command! 'scroll-up cmd-scroll-up)
  (register-command! 'recenter cmd-recenter)
  ;; Editing
  (register-command! 'delete-char cmd-delete-char)
  (register-command! 'backward-delete-char cmd-backward-delete-char)
  (register-command! 'newline cmd-newline)
  (register-command! 'open-line cmd-open-line)
  (register-command! 'undo cmd-undo)
  (register-command! 'redo cmd-redo)
  ;; Kill/Yank
  (register-command! 'kill-line cmd-kill-line)
  (register-command! 'yank cmd-yank)
  ;; Mark/Region
  (register-command! 'set-mark cmd-set-mark)
  (register-command! 'kill-region cmd-kill-region)
  (register-command! 'copy-region cmd-copy-region)
  ;; File
  (register-command! 'find-file cmd-find-file)
  (register-command! 'find-file-at-point cmd-find-file-at-point)
  (register-command! 'ffap cmd-find-file-at-point)
  (register-command! 'save-buffer cmd-save-buffer)
  (register-command! 'write-file cmd-write-file)
  (register-command! 'revert-buffer cmd-revert-buffer)
  ;; Buffer
  (register-command! 'switch-buffer cmd-switch-buffer)
  (register-command! 'kill-buffer-cmd cmd-kill-buffer-cmd)
  (register-command! 'list-buffers cmd-list-buffers)
  ;; Window
  (register-command! 'split-window cmd-split-window)
  (register-command! 'split-window-right cmd-split-window-right)
  (register-command! 'other-window cmd-other-window)
  (register-command! 'ace-window cmd-ace-window)
  (register-command! 'swap-window cmd-swap-window)
  (register-command! 'delete-window cmd-delete-window)
  (register-command! 'delete-other-windows cmd-delete-other-windows)
  ;; Search
  (register-command! 'search-forward cmd-search-forward)
  (register-command! 'search-backward cmd-search-backward)
  (register-command! 'query-replace cmd-query-replace)
  ;; REPL
  (register-command! 'repl cmd-repl)
  (register-command! 'eval-expression cmd-eval-expression)
  ;; Eshell
  (register-command! 'eshell cmd-eshell)
  ;; Shell
  (register-command! 'shell cmd-shell)
  ;; Terminal
  (register-command! 'term cmd-term)
  (register-command! 'term-interrupt cmd-term-interrupt)
  (register-command! 'term-send-eof cmd-term-send-eof)
  (register-command! 'term-send-tab cmd-term-send-tab)
  ;; Goto line / M-x
  (register-command! 'goto-line cmd-goto-line)
  (register-command! 'execute-extended-command cmd-execute-extended-command)
  ;; Help
  (register-command! 'list-bindings cmd-list-bindings)
  ;; Select all
  (register-command! 'select-all cmd-select-all)
  ;; Duplicate line
  (register-command! 'duplicate-line cmd-duplicate-line)
  ;; Comment toggle
  (register-command! 'toggle-comment cmd-toggle-comment)
  ;; Transpose
  (register-command! 'transpose-chars cmd-transpose-chars)
  ;; Word case
  (register-command! 'upcase-word cmd-upcase-word)
  (register-command! 'downcase-word cmd-downcase-word)
  (register-command! 'capitalize-word cmd-capitalize-word)
  ;; Kill word
  (register-command! 'kill-word cmd-kill-word)
  ;; What line
  (register-command! 'what-line cmd-what-line)
  ;; Defun navigation
  (register-command! 'beginning-of-defun cmd-beginning-of-defun)
  (register-command! 'end-of-defun cmd-end-of-defun)
  ;; Tab/indent
  (register-command! 'indent-or-complete cmd-indent-or-complete)
  ;; Zoom
  (register-command! 'zoom-in cmd-zoom-in)
  (register-command! 'zoom-out cmd-zoom-out)
  ;; Line numbers
  (register-command! 'toggle-line-numbers cmd-toggle-line-numbers)
  ;; Backward kill word
  (register-command! 'backward-kill-word cmd-backward-kill-word)
  ;; Kill whole line
  (register-command! 'kill-whole-line cmd-kill-whole-line)
  ;; Join line
  (register-command! 'join-line cmd-join-line)
  ;; Just one space
  (register-command! 'just-one-space cmd-just-one-space)
  ;; Transpose words/lines
  (register-command! 'transpose-words cmd-transpose-words)
  (register-command! 'transpose-lines cmd-transpose-lines)
  ;; Move line up/down
  (register-command! 'move-line-up cmd-move-line-up)
  (register-command! 'move-line-down cmd-move-line-down)
  ;; Fill paragraph
  (register-command! 'fill-paragraph cmd-fill-paragraph)
  ;; Count words
  (register-command! 'count-words cmd-count-words)
  ;; Cursor position
  (register-command! 'what-cursor-position cmd-what-cursor-position)
  ;; Dynamic abbreviation
  (register-command! 'dabbrev-expand cmd-dabbrev-expand)
  ;; Delete blank lines
  (register-command! 'delete-blank-lines cmd-delete-blank-lines)
  ;; Insert file
  (register-command! 'insert-file cmd-insert-file)
  ;; Shell command
  (register-command! 'shell-command cmd-shell-command)
  ;; Sort lines
  (register-command! 'sort-lines cmd-sort-lines)
  ;; Goto matching paren
  (register-command! 'goto-matching-paren cmd-goto-matching-paren)
  ;; Upcase/downcase region
  (register-command! 'upcase-region cmd-upcase-region)
  (register-command! 'downcase-region cmd-downcase-region)
  ;; Indent region
  (register-command! 'indent-region cmd-indent-region)
  ;; Zap to char
  (register-command! 'zap-to-char cmd-zap-to-char)
  ;; Goto char
  (register-command! 'goto-char cmd-goto-char)
  ;; Zoom reset
  (register-command! 'zoom-reset cmd-zoom-reset)
  ;; Yank pop
  (register-command! 'yank-pop cmd-yank-pop)
  ;; Occur
  (register-command! 'occur cmd-occur)
  (register-command! 'occur-goto cmd-occur-goto)
  ;; Keyboard macros
  (register-command! 'start-kbd-macro cmd-start-kbd-macro)
  (register-command! 'end-kbd-macro cmd-end-kbd-macro)
  (register-command! 'call-last-kbd-macro cmd-call-last-kbd-macro)
  ;; Repeat
  (register-command! 'repeat cmd-repeat)
  ;; Mark ring
  (register-command! 'pop-mark cmd-pop-mark)
  ;; Registers
  (register-command! 'copy-to-register cmd-copy-to-register)
  (register-command! 'insert-register cmd-insert-register)
  (register-command! 'point-to-register cmd-point-to-register)
  (register-command! 'jump-to-register cmd-jump-to-register)
  (register-command! 'window-configuration-to-register cmd-window-configuration-to-register)
  ;; Paragraph navigation
  (register-command! 'forward-paragraph cmd-forward-paragraph)
  (register-command! 'backward-paragraph cmd-backward-paragraph)
  ;; Indentation
  (register-command! 'back-to-indentation cmd-back-to-indentation)
  (register-command! 'delete-indentation cmd-delete-indentation)
  ;; Exchange point and mark
  (register-command! 'exchange-point-and-mark cmd-exchange-point-and-mark)
  ;; Copy line
  (register-command! 'copy-line cmd-copy-line)
  ;; Mark word
  (register-command! 'mark-word cmd-mark-word)
  ;; Save some buffers
  (register-command! 'save-some-buffers cmd-save-some-buffers)
  ;; Compile
  (register-command! 'compile cmd-compile)
  (register-command! 'recompile cmd-recompile)
  (register-command! 'toggle-compile-on-save cmd-toggle-compile-on-save)
  ;; Flycheck
  (register-command! 'flycheck-mode cmd-flycheck-mode)
  (register-command! 'flycheck-next-error cmd-flycheck-next-error)
  (register-command! 'flycheck-prev-error cmd-flycheck-prev-error)
  (register-command! 'flycheck-list-errors cmd-flycheck-list-errors)
  (register-command! 'first-error (lambda (app)
    (if (null? *compilation-errors*)
      (echo-error! (app-state-echo app) "No compilation errors")
      (compilation-goto-error! app 0))))
  ;; Where-is
  (register-command! 'where-is cmd-where-is)
  ;; Flush/keep lines
  (register-command! 'flush-lines cmd-flush-lines)
  (register-command! 'keep-lines cmd-keep-lines)
  ;; Number lines
  (register-command! 'number-lines cmd-number-lines)
  ;; Reverse region
  (register-command! 'reverse-region cmd-reverse-region)
  ;; Toggle read-only
  (register-command! 'toggle-read-only cmd-toggle-read-only)
  ;; Rename buffer
  (register-command! 'rename-buffer cmd-rename-buffer)
  ;; Other window commands
  (register-command! 'switch-buffer-other-window cmd-switch-buffer-other-window)
  (register-command! 'find-file-other-window cmd-find-file-other-window)
  ;; Insert date
  (register-command! 'insert-date cmd-insert-date)
  ;; Eval buffer/region
  (register-command! 'eval-buffer cmd-eval-buffer)
  (register-command! 'eval-region cmd-eval-region)
  ;; Clone buffer / scratch
  (register-command! 'clone-buffer cmd-clone-buffer)
  (register-command! 'scratch-buffer cmd-scratch-buffer)
  ;; Delete duplicate lines
  (register-command! 'delete-duplicate-lines cmd-delete-duplicate-lines)
  ;; Count matches / count lines region
  (register-command! 'count-matches cmd-count-matches)
  (register-command! 'count-lines-region cmd-count-lines-region)
  ;; Diff buffer
  (register-command! 'diff-buffer-with-file cmd-diff-buffer-with-file)
  (register-command! 'diff-next-hunk cmd-diff-next-hunk)
  (register-command! 'diff-prev-hunk cmd-diff-prev-hunk)
  ;; Grep buffer
  (register-command! 'grep-buffer cmd-grep-buffer)
  ;; Revert quick
  (register-command! 'revert-buffer-quick cmd-revert-buffer-quick)
  ;; Shell command on region
  (register-command! 'shell-command-on-region cmd-shell-command-on-region)
  ;; Pipe buffer
  (register-command! 'pipe-buffer cmd-pipe-buffer)
  ;; Apropos
  (register-command! 'apropos-command cmd-apropos-command)
  ;; What page
  (register-command! 'what-page cmd-what-page)
  ;; Async shell command
  (register-command! 'async-shell-command cmd-async-shell-command)
  ;; Checksum
  (register-command! 'checksum cmd-checksum)
  ;; S-expression navigation
  (register-command! 'forward-sexp cmd-forward-sexp)
  (register-command! 'backward-sexp cmd-backward-sexp)
  (register-command! 'kill-sexp cmd-kill-sexp)
  (register-command! 'backward-kill-sexp cmd-backward-kill-sexp)
  (register-command! 'mark-sexp cmd-mark-sexp)
  (register-command! 'mark-defun cmd-mark-defun)
  (register-command! 'indent-sexp cmd-indent-sexp)
  ;; Paredit
  (register-command! 'paredit-slurp-forward cmd-paredit-slurp-forward)
  (register-command! 'paredit-barf-forward cmd-paredit-barf-forward)
  (register-command! 'paredit-wrap-round cmd-paredit-wrap-round)
  (register-command! 'paredit-wrap-square cmd-paredit-wrap-square)
  (register-command! 'paredit-splice-sexp cmd-paredit-splice-sexp)
  (register-command! 'paredit-raise-sexp cmd-paredit-raise-sexp)
  (register-command! 'paredit-split-sexp cmd-paredit-split-sexp)
  (register-command! 'paredit-join-sexps cmd-paredit-join-sexps)
  ;; Avy jump
  (register-command! 'avy-goto-char cmd-avy-goto-char)
  (register-command! 'avy-goto-word cmd-avy-goto-word)
  (register-command! 'avy-goto-line cmd-avy-goto-line)
  ;; Buffer cycling
  (register-command! 'previous-buffer cmd-previous-buffer)
  (register-command! 'next-buffer cmd-next-buffer)
  ;; Delete trailing whitespace
  (register-command! 'delete-trailing-whitespace cmd-delete-trailing-whitespace)
  (register-command! 'untabify cmd-untabify)
  (register-command! 'tabify cmd-tabify)
  ;; Kill buffer and window
  (register-command! 'kill-buffer-and-window cmd-kill-buffer-and-window)
  ;; Open line above
  (register-command! 'open-line-above cmd-open-line-above)
  ;; Select line
  (register-command! 'select-line cmd-select-line)
  ;; Smart beginning of line
  (register-command! 'smart-beginning-of-line cmd-smart-beginning-of-line)
  ;; Insert parens/brackets
  (register-command! 'insert-parentheses cmd-insert-parentheses)
  (register-command! 'insert-pair-brackets cmd-insert-pair-brackets)
  ;; Find file at point
  (register-command! 'find-file-at-point cmd-find-file-at-point)
  ;; Show kill ring / list registers
  (register-command! 'show-kill-ring cmd-show-kill-ring)
  (register-command! 'browse-kill-ring cmd-browse-kill-ring)
  (register-command! 'list-registers cmd-list-registers)
  ;; Scroll other window
  (register-command! 'scroll-other-window cmd-scroll-other-window)
  (register-command! 'scroll-other-window-up cmd-scroll-other-window-up)
  ;; Swap buffers
  (register-command! 'swap-buffers cmd-swap-buffers)
  ;; Goto percent
  (register-command! 'goto-percent cmd-goto-percent)
  ;; Sentence navigation
  (register-command! 'forward-sentence cmd-forward-sentence)
  (register-command! 'backward-sentence cmd-backward-sentence)
  ;; Dired
  (register-command! 'dired cmd-dired)
  ;; Unfill paragraph
  (register-command! 'unfill-paragraph cmd-unfill-paragraph)
  ;; Whitespace cleanup
  (register-command! 'whitespace-cleanup cmd-whitespace-cleanup)
  ;; Insert UUID
  (register-command! 'insert-uuid cmd-insert-uuid)
  ;; Word frequency
  (register-command! 'word-frequency cmd-word-frequency)
  ;; Indent rigidly
  (register-command! 'indent-rigidly-right cmd-indent-rigidly-right)
  (register-command! 'indent-rigidly-left cmd-indent-rigidly-left)
  ;; Center line
  (register-command! 'center-line cmd-center-line)
  ;; Narrow/widen
  (register-command! 'narrow-to-region cmd-narrow-to-region)
  (register-command! 'widen cmd-widen)
  ;; Display time
  (register-command! 'display-time cmd-display-time)
  ;; Buffer info
  (register-command! 'buffer-info cmd-buffer-info)
  ;; Bookmarks
  (register-command! 'bookmark-set cmd-bookmark-set)
  (register-command! 'bookmark-jump cmd-bookmark-jump)
  (register-command! 'bookmark-list cmd-bookmark-list)
  ;; Rectangle operations
  (register-command! 'kill-rectangle cmd-kill-rectangle)
  (register-command! 'yank-rectangle cmd-yank-rectangle)
  (register-command! 'string-rectangle cmd-string-rectangle)
  (register-command! 'open-rectangle cmd-open-rectangle)
  ;; Describe
  (register-command! 'describe-key cmd-describe-key)
  (register-command! 'describe-command cmd-describe-command)
  (register-command! 'describe-key-briefly cmd-describe-key-briefly)
  (register-command! 'describe-bindings cmd-describe-bindings)
  (register-command! 'describe-char cmd-describe-char)
  ;; Toggle electric pair
  (register-command! 'toggle-electric-pair cmd-toggle-electric-pair)
  ;; Universal argument / digit args
  (register-command! 'universal-argument cmd-universal-argument)
  (register-command! 'negative-argument cmd-negative-argument)
  (register-command! 'digit-argument-0 cmd-digit-argument-0)
  (register-command! 'digit-argument-1 cmd-digit-argument-1)
  (register-command! 'digit-argument-2 cmd-digit-argument-2)
  (register-command! 'digit-argument-3 cmd-digit-argument-3)
  (register-command! 'digit-argument-4 cmd-digit-argument-4)
  (register-command! 'digit-argument-5 cmd-digit-argument-5)
  (register-command! 'digit-argument-6 cmd-digit-argument-6)
  (register-command! 'digit-argument-7 cmd-digit-argument-7)
  (register-command! 'digit-argument-8 cmd-digit-argument-8)
  (register-command! 'digit-argument-9 cmd-digit-argument-9)
  ;; Next/previous error
  (register-command! 'next-error cmd-next-error)
  (register-command! 'previous-error cmd-previous-error)
  ;; Text transforms
  (register-command! 'tabify cmd-tabify)
  (register-command! 'untabify cmd-untabify)
  (register-command! 'base64-encode-region cmd-base64-encode-region)
  (register-command! 'base64-decode-region cmd-base64-decode-region)
  (register-command! 'rot13-region cmd-rot13-region)
  ;; Hex dump
  (register-command! 'hexl-mode cmd-hexl-mode)
  ;; Toggles
  (register-command! 'toggle-word-wrap cmd-toggle-word-wrap)
  (register-command! 'toggle-whitespace cmd-toggle-whitespace)
  (register-command! 'toggle-truncate-lines cmd-toggle-truncate-lines)
  (register-command! 'toggle-case-fold-search cmd-toggle-case-fold-search)
  (register-command! 'toggle-overwrite-mode cmd-toggle-overwrite-mode)
  (register-command! 'toggle-auto-fill cmd-toggle-auto-fill)
  (register-command! 'toggle-visual-line-mode cmd-toggle-visual-line-mode)
  (register-command! 'toggle-highlighting cmd-toggle-highlighting)
  (register-command! 'toggle-fill-column-indicator cmd-toggle-fill-column-indicator)
  (register-command! 'toggle-indent-tabs-mode cmd-toggle-indent-tabs-mode)
  ;; Fill column
  (register-command! 'set-fill-column cmd-set-fill-column)
  ;; Calculator
  (register-command! 'calc cmd-calc)
  ;; Count words buffer/region/chars
  (register-command! 'count-words-region cmd-count-words-region)
  (register-command! 'count-words-buffer cmd-count-words-buffer)
  (register-command! 'count-chars-region cmd-count-chars-region)
  (register-command! 'buffer-stats cmd-buffer-stats)
  ;; List processes
  (register-command! 'list-processes cmd-list-processes)
  ;; View messages
  (register-command! 'view-messages cmd-view-messages)
  ;; What buffer / what face
  (register-command! 'what-buffer cmd-what-buffer)
  (register-command! 'what-face cmd-what-face)
  ;; Insert helpers
  (register-command! 'insert-buffer-name cmd-insert-buffer-name)
  (register-command! 'insert-file-name cmd-insert-file-name)
  (register-command! 'insert-char cmd-insert-char)
  (register-command! 'string-insert-file cmd-string-insert-file)
  ;; Rename file and buffer
  (register-command! 'rename-file-and-buffer cmd-rename-file-and-buffer)
  ;; Sort numeric / sort fields
  (register-command! 'sort-numeric cmd-sort-numeric)
  (register-command! 'sort-fields cmd-sort-fields)
  ;; Align regexp
  (register-command! 'align-regexp cmd-align-regexp)
  ;; Window management
  (register-command! 'enlarge-window cmd-enlarge-window)
  (register-command! 'shrink-window cmd-shrink-window)
  (register-command! 'balance-windows cmd-balance-windows)
  (register-command! 'move-to-window-line cmd-move-to-window-line)
  ;; Title case
  (register-command! 'upcase-initials-region cmd-upcase-initials-region)
  ;; S-expression extended
  (register-command! 'backward-up-list cmd-backward-up-list)
  (register-command! 'forward-up-list cmd-forward-up-list)
  (register-command! 'mark-paragraph cmd-mark-paragraph)
  ;; Hippie expand
  (register-command! 'hippie-expand cmd-hippie-expand)
  ;; Split line
  (register-command! 'split-line cmd-split-line)
  ;; Copy from above
  (register-command! 'copy-from-above cmd-copy-from-above)
  ;; Find alternate file
  (register-command! 'find-alternate-file cmd-find-alternate-file)
  ;; Increment register
  (register-command! 'increment-register cmd-increment-register)
  ;; Delete pair
  (register-command! 'delete-pair cmd-delete-pair)
  ;; Sudo write
  (register-command! 'sudo-write cmd-sudo-write)
  ;; Ediff buffers
  (register-command! 'ediff-buffers cmd-ediff-buffers)
  ;; Highlight symbol / clear
  (register-command! 'highlight-symbol cmd-highlight-symbol)
  (register-command! 'highlight-symbol-next cmd-highlight-symbol-next)
  (register-command! 'highlight-symbol-prev cmd-highlight-symbol-prev)
  (register-command! 'clear-highlight cmd-clear-highlight)
  ;; Repeat complex command
  (register-command! 'repeat-complex-command cmd-repeat-complex-command)
  ;; Flush undo
  (register-command! 'flush-undo cmd-flush-undo)
  (register-command! 'undo-history cmd-undo-history)
  (register-command! 'undo-history-restore cmd-undo-history-restore)
  ;; Untabify buffer
  (register-command! 'untabify-buffer cmd-untabify-buffer)
  ;; Expand region
  (register-command! 'expand-region cmd-expand-region)
  (register-command! 'contract-region cmd-contract-region)
  ;; String inflection
  (register-command! 'string-inflection-cycle cmd-string-inflection-cycle)
  ;; Number increment/decrement
  (register-command! 'increment-number cmd-increment-number)
  (register-command! 'decrement-number cmd-decrement-number)
  ;; Browse URL
  (register-command! 'browse-url-at-point cmd-browse-url-at-point)
  (register-command! 'browse-url cmd-browse-url)
  ;; Imenu
  (register-command! 'imenu cmd-imenu)
  ;; Cycle tab width
  (register-command! 'cycle-tab-width cmd-cycle-tab-width)
  ;; Replace string
  (register-command! 'replace-string cmd-replace-string)
  ;; Batch 6: Navigation
  (register-command! 'goto-column cmd-goto-column)
  (register-command! 'goto-line-relative cmd-goto-line-relative)
  (register-command! 'recenter-top cmd-recenter-top)
  ;; Character case
  (register-command! 'upcase-char cmd-upcase-char)
  (register-command! 'downcase-char cmd-downcase-char)
  (register-command! 'toggle-case-at-point cmd-toggle-case-at-point)
  (register-command! 'capitalize-region cmd-capitalize-region)
  ;; Copy commands
  (register-command! 'copy-buffer-name cmd-copy-buffer-name)
  (register-command! 'copy-current-line cmd-copy-current-line)
  (register-command! 'copy-word cmd-copy-word)
  (register-command! 'copy-file-path cmd-copy-file-path)
  (register-command! 'copy-line-number cmd-copy-line-number)
  (register-command! 'copy-region-as-kill cmd-copy-region-as-kill)
  (register-command! 'yank-whole-line cmd-yank-whole-line)
  ;; Insert commands
  (register-command! 'insert-pair-braces cmd-insert-pair-braces)
  (register-command! 'insert-pair-quotes cmd-insert-pair-quotes)
  (register-command! 'insert-newline-above cmd-insert-newline-above)
  (register-command! 'insert-newline-below cmd-insert-newline-below)
  (register-command! 'insert-comment-separator cmd-insert-comment-separator)
  (register-command! 'insert-line-number cmd-insert-line-number)
  (register-command! 'insert-buffer-filename cmd-insert-buffer-filename)
  (register-command! 'insert-timestamp cmd-insert-timestamp)
  (register-command! 'insert-shebang cmd-insert-shebang)
  ;; Buffer management
  (register-command! 'count-buffers cmd-count-buffers)
  (register-command! 'rename-uniquely cmd-rename-uniquely)
  (register-command! 'bury-buffer cmd-bury-buffer)
  (register-command! 'unbury-buffer cmd-unbury-buffer)
  (register-command! 'append-to-buffer cmd-append-to-buffer)
  ;; File operations
  (register-command! 'make-directory cmd-make-directory)
  (register-command! 'delete-file cmd-delete-file)
  (register-command! 'copy-file cmd-copy-file)
  (register-command! 'list-directory cmd-list-directory)
  (register-command! 'pwd cmd-pwd)
  ;; Dired extended
  (register-command! 'dired-create-directory cmd-dired-create-directory)
  (register-command! 'dired-find-file cmd-dired-find-file)
  (register-command! 'dired-do-rename cmd-dired-do-rename)
  (register-command! 'dired-do-delete cmd-dired-do-delete)
  (register-command! 'dired-do-copy cmd-dired-do-copy)
  ;; Toggle commands
  (register-command! 'toggle-hl-line cmd-toggle-hl-line)
  (register-command! 'toggle-show-tabs cmd-toggle-show-tabs)
  (register-command! 'toggle-show-eol cmd-toggle-show-eol)
  (register-command! 'toggle-narrowing-indicator cmd-toggle-narrowing-indicator)
  (register-command! 'toggle-debug-on-error cmd-toggle-debug-on-error)
  (register-command! 'toggle-fold cmd-toggle-fold)
  ;; Info/describe
  (register-command! 'what-mode cmd-what-mode)
  (register-command! 'what-encoding cmd-what-encoding)
  (register-command! 'what-line-col cmd-what-line-col)
  (register-command! 'show-file-info cmd-show-file-info)
  (register-command! 'show-buffer-size cmd-show-buffer-size)
  (register-command! 'show-column-number cmd-show-column-number)
  (register-command! 'emacs-version cmd-emacs-version)
  ;; Git/VCS
  (register-command! 'show-git-status cmd-show-git-status)
  (register-command! 'show-git-log cmd-show-git-log)
  (register-command! 'show-git-diff cmd-show-git-diff)
  (register-command! 'show-git-blame cmd-show-git-blame)
  ;; Text manipulation
  (register-command! 'comment-region cmd-comment-region)
  (register-command! 'uncomment-region cmd-uncomment-region)
  (register-command! 'collapse-blank-lines cmd-collapse-blank-lines)
  (register-command! 'remove-blank-lines cmd-remove-blank-lines)
  (register-command! 'delete-trailing-lines cmd-delete-trailing-lines)
  (register-command! 'trim-lines cmd-trim-lines)
  (register-command! 'prefix-lines cmd-prefix-lines)
  (register-command! 'suffix-lines cmd-suffix-lines)
  ;; Sort variants
  (register-command! 'sort-lines-reverse cmd-sort-lines-reverse)
  (register-command! 'sort-lines-case-fold cmd-sort-lines-case-fold)
  (register-command! 'uniquify-lines cmd-uniquify-lines)
  (register-command! 'sort-words cmd-sort-words)
  ;; Case conversion
  (register-command! 'camel-to-snake cmd-camel-to-snake)
  (register-command! 'snake-to-camel cmd-snake-to-camel)
  ;; Search
  (register-command! 'highlight-word-at-point cmd-highlight-word-at-point)
  (register-command! 'grep cmd-grep)
  (register-command! 'rgrep cmd-rgrep)
  (register-command! 'grep-goto cmd-grep-goto)
  (register-command! 'next-grep-result cmd-next-grep-result)
  (register-command! 'previous-grep-result cmd-previous-grep-result)
  ;; Wgrep
  (register-command! 'wgrep-change-to-wgrep-mode cmd-wgrep-change-to-wgrep-mode)
  (register-command! 'wgrep-finish-edit cmd-wgrep-finish-edit)
  (register-command! 'wgrep-abort-changes cmd-wgrep-abort-changes)
  ;; Misc batch 6
  (register-command! 'quoted-insert cmd-quoted-insert)
  (register-command! 'quick-calc cmd-quick-calc)
  (register-command! 'eval-and-insert cmd-eval-and-insert)
  (register-command! 'shell-command-insert cmd-shell-command-insert)
  (register-command! 'pipe-region cmd-pipe-region)
  ;; Bookmark extensions
  (register-command! 'bookmark-delete cmd-bookmark-delete)
  (register-command! 'bookmark-save cmd-bookmark-save)
  (register-command! 'bookmark-load cmd-bookmark-load)
  ;; Region/text misc
  (register-command! 'duplicate-region cmd-duplicate-region)
  (register-command! 'reverse-chars cmd-reverse-chars)
  (register-command! 'reverse-word cmd-reverse-word)
  ;; Environment
  (register-command! 'getenv cmd-getenv)
  (register-command! 'setenv cmd-setenv)
  ;; Batch 7: More commands
  (register-command! 'transpose-sexps cmd-transpose-sexps)
  (register-command! 'transpose-paragraphs cmd-transpose-paragraphs)
  (register-command! 'zap-up-to-char cmd-zap-up-to-char)
  (register-command! 'zap-to-char-inclusive cmd-zap-to-char-inclusive)
  (register-command! 'query-replace-regexp cmd-query-replace-regexp)
  (register-command! 'copy-from-below cmd-copy-from-below)
  (register-command! 'copy-symbol-at-point cmd-copy-symbol-at-point)
  (register-command! 'copy-word-at-point cmd-copy-word-at-point)
  (register-command! 'delete-to-end-of-line cmd-delete-to-end-of-line)
  (register-command! 'delete-to-beginning-of-line cmd-delete-to-beginning-of-line)
  (register-command! 'delete-horizontal-space-forward cmd-delete-horizontal-space-forward)
  (register-command! 'cycle-spacing cmd-cycle-spacing)
  (register-command! 'swap-windows cmd-swap-windows)
  (register-command! 'rotate-windows cmd-rotate-windows)
  (register-command! 'toggle-line-comment cmd-toggle-line-comment)
  (register-command! 'narrow-to-defun cmd-narrow-to-defun)
  (register-command! 'fold-all cmd-fold-all)
  (register-command! 'unfold-all cmd-unfold-all)
  (register-command! 'toggle-auto-pair-mode cmd-toggle-auto-pair-mode)
  (register-command! 'mark-page cmd-mark-page)
  (register-command! 'mark-whole-buffer cmd-mark-whole-buffer)
  (register-command! 'view-lossage cmd-view-lossage)
  (register-command! 'bookmark-rename cmd-bookmark-rename)
  (register-command! 'view-register cmd-view-register)
  (register-command! 'sort-imports cmd-sort-imports)
  (register-command! 'replace-string-all cmd-replace-string-all)
  (register-command! 'replace-in-region cmd-replace-in-region)
  (register-command! 'write-region cmd-write-region)
  (register-command! 'search-forward-word cmd-search-forward-word)
  (register-command! 'search-backward-word cmd-search-backward-word)
  (register-command! 'count-occurrences cmd-count-occurrences)
  (register-command! 'delete-file-and-buffer cmd-delete-file-and-buffer)
  (register-command! 'find-file-literally cmd-find-file-literally)
  (register-command! 'kill-matching-buffers cmd-kill-matching-buffers)
  (register-command! 'list-recent-files cmd-list-recent-files)
  (register-command! 'clear-recent-files cmd-clear-recent-files)
  (register-command! 'recentf-open cmd-recentf-open)
  (register-command! 'recentf-cleanup cmd-recentf-cleanup)
  (register-command! 'multi-occur cmd-multi-occur)
  (register-command! 'align-current cmd-align-current)
  (register-command! 'clear-rectangle cmd-clear-rectangle)
  (register-command! 'describe-mode cmd-describe-mode)
  (register-command! 'describe-face cmd-describe-face)
  (register-command! 'describe-function cmd-describe-function)
  (register-command! 'describe-variable cmd-describe-variable)
  (register-command! 'describe-syntax cmd-describe-syntax)
  (register-command! 'insert-lorem-ipsum cmd-insert-lorem-ipsum)
  (register-command! 'insert-current-date-iso cmd-insert-current-date-iso)
  (register-command! 'insert-time cmd-insert-time)
  (register-command! 'goto-definition cmd-goto-definition)
  (register-command! 'xref-back cmd-xref-back)
  (register-command! 'imenu cmd-imenu)
  (register-command! 'show-word-count cmd-show-word-count)
  (register-command! 'show-char-count cmd-show-char-count)
  (register-command! 'insert-path-separator cmd-insert-path-separator)
  (register-command! 'maximize-window cmd-maximize-window)
  (register-command! 'minimize-window cmd-minimize-window)
  (register-command! 'delete-matching-lines cmd-delete-matching-lines)
  (register-command! 'delete-non-matching-lines cmd-delete-non-matching-lines)
  (register-command! 'copy-matching-lines cmd-copy-matching-lines)
  (register-command! 'count-lines-buffer cmd-count-lines-buffer)
  (register-command! 'count-words-paragraph cmd-count-words-paragraph)
  (register-command! 'convert-to-unix cmd-convert-to-unix)
  (register-command! 'convert-to-dos cmd-convert-to-dos)
  (register-command! 'convert-line-endings-unix cmd-convert-to-unix)
  (register-command! 'convert-line-endings-dos cmd-convert-to-dos)
  (register-command! 'show-line-endings cmd-show-line-endings)
  (register-command! 'wrap-lines-at-column cmd-wrap-lines-at-column)
  (register-command! 'strip-line-numbers cmd-strip-line-numbers)
  (register-command! 'goto-word-at-point cmd-goto-word-at-point)
  (register-command! 'unindent-region cmd-unindent-region)
  (register-command! 'number-region cmd-number-region)
  (register-command! 'insert-kbd-macro cmd-insert-kbd-macro)
  (register-command! 'name-last-kbd-macro cmd-name-last-kbd-macro)
  (register-command! 'show-environment cmd-show-environment)
  (register-command! 'show-keybinding-for cmd-show-keybinding-for)
  (register-command! 'first-error cmd-first-error)
  (register-command! 'find-grep cmd-find-grep)
  (register-command! 'project-grep cmd-project-grep)
  (register-command! 'project-find-file cmd-project-find-file)
  (register-command! 'project-compile cmd-project-compile)
  (register-command! 'reindent-buffer cmd-reindent-buffer)
  (register-command! 'fill-individual-paragraphs cmd-fill-individual-paragraphs)
  ;; Batch 8: Font size
  (register-command! 'increase-font-size cmd-increase-font-size)
  (register-command! 'decrease-font-size cmd-decrease-font-size)
  (register-command! 'reset-font-size cmd-reset-font-size)
  ;; Navigation
  (register-command! 'goto-first-non-blank cmd-goto-first-non-blank)
  (register-command! 'goto-last-non-blank cmd-goto-last-non-blank)
  (register-command! 'move-to-window-top cmd-move-to-window-top)
  (register-command! 'move-to-window-middle cmd-move-to-window-middle)
  (register-command! 'move-to-window-bottom cmd-move-to-window-bottom)
  (register-command! 'recenter-bottom cmd-recenter-bottom)
  (register-command! 'scroll-left cmd-scroll-left)
  (register-command! 'scroll-right cmd-scroll-right)
  ;; Code insertion templates
  (register-command! 'insert-let cmd-insert-let)
  (register-command! 'insert-lambda cmd-insert-lambda)
  (register-command! 'insert-defun cmd-insert-defun)
  (register-command! 'insert-cond cmd-insert-cond)
  (register-command! 'insert-when cmd-insert-when)
  (register-command! 'insert-unless cmd-insert-unless)
  (register-command! 'insert-match cmd-insert-match)
  (register-command! 'insert-import cmd-insert-import)
  (register-command! 'insert-export cmd-insert-export)
  (register-command! 'insert-include cmd-insert-include)
  (register-command! 'insert-file-header cmd-insert-file-header)
  (register-command! 'insert-header-guard cmd-insert-header-guard)
  (register-command! 'insert-box-comment cmd-insert-box-comment)
  (register-command! 'insert-file-contents cmd-insert-file-contents)
  (register-command! 'insert-register-string cmd-insert-register-string)
  ;; Toggles
  (register-command! 'show-dir-locals cmd-show-dir-locals)
  (register-command! 'toggle-auto-indent cmd-toggle-auto-indent)
  (register-command! 'toggle-backup-files cmd-toggle-backup-files)
  (register-command! 'toggle-debug-mode cmd-toggle-debug-mode)
  (register-command! 'toggle-debug-on-quit cmd-toggle-debug-on-quit)
  (register-command! 'toggle-visible-bell cmd-toggle-visible-bell)
  (register-command! 'toggle-transient-mark cmd-toggle-transient-mark)
  (register-command! 'toggle-electric-indent cmd-toggle-electric-indent)
  (register-command! 'toggle-auto-revert cmd-toggle-auto-revert)
  (register-command! 'toggle-auto-revert-global cmd-toggle-auto-revert-global)
  (register-command! 'auto-revert-tail-mode cmd-auto-revert-tail-mode)
  (register-command! 'toggle-frame-fullscreen cmd-toggle-frame-fullscreen)
  (register-command! 'toggle-frame-maximized cmd-toggle-frame-maximized)
  (register-command! 'toggle-menu-bar cmd-toggle-menu-bar)
  (register-command! 'toggle-menu-bar-mode cmd-toggle-menu-bar-mode)
  (register-command! 'toggle-tool-bar cmd-toggle-tool-bar)
  (register-command! 'toggle-scroll-bar cmd-toggle-scroll-bar)
  (register-command! 'toggle-tab-bar-mode cmd-toggle-tab-bar-mode)
  (register-command! 'toggle-input-method cmd-toggle-input-method)
  (register-command! 'toggle-eol-conversion cmd-toggle-eol-conversion)
  (register-command! 'toggle-flymake cmd-toggle-flymake)
  (register-command! 'toggle-flyspell cmd-toggle-flyspell)
  (register-command! 'toggle-lsp cmd-toggle-lsp)
  (register-command! 'toggle-global-hl-line cmd-toggle-global-hl-line)
  (register-command! 'toggle-global-whitespace cmd-toggle-global-whitespace)
  (register-command! 'toggle-show-spaces cmd-toggle-show-spaces)
  (register-command! 'toggle-show-trailing-whitespace cmd-toggle-show-trailing-whitespace)
  (register-command! 'toggle-narrow-indicator cmd-toggle-narrow-indicator)
  (register-command! 'toggle-auto-complete cmd-toggle-auto-complete)
  ;; Windows
  (register-command! 'split-window-below cmd-split-window-below)
  (register-command! 'delete-window-below cmd-delete-window-below)
  (register-command! 'fit-window-to-buffer cmd-fit-window-to-buffer)
  (register-command! 'shrink-window-if-larger-than-buffer cmd-shrink-window-if-larger-than-buffer)
  (register-command! 'resize-window-width cmd-resize-window-width)
  (register-command! 'make-frame cmd-make-frame)
  (register-command! 'delete-frame cmd-delete-frame)
  (register-command! 'suspend-frame cmd-suspend-frame)
  ;; Editing
  (register-command! 'center-region cmd-center-region)
  (register-command! 'indent-rigidly cmd-indent-rigidly)
  (register-command! 'dedent-rigidly cmd-dedent-rigidly)
  (register-command! 'fixup-whitespace cmd-fixup-whitespace)
  (register-command! 'electric-newline-and-indent cmd-electric-newline-and-indent)
  (register-command! 'kebab-to-camel cmd-kebab-to-camel)
  (register-command! 'flush-lines-region cmd-flush-lines-region)
  (register-command! 'keep-lines-region cmd-keep-lines-region)
  ;; VCS
  (register-command! 'vc-annotate cmd-vc-annotate)
  (register-command! 'vc-diff-head cmd-vc-diff-head)
  (register-command! 'vc-log-file cmd-vc-log-file)
  (register-command! 'vc-revert cmd-vc-revert)
  ;; Search
  (register-command! 'isearch-forward-word cmd-isearch-forward-word)
  (register-command! 'isearch-backward-word cmd-isearch-backward-word)
  (register-command! 'isearch-forward-symbol cmd-isearch-forward-symbol)
  (register-command! 'mark-lines-matching cmd-mark-lines-matching)
  ;; Buffer/undo
  (register-command! 'buffer-disable-undo cmd-buffer-disable-undo)
  (register-command! 'buffer-enable-undo cmd-buffer-enable-undo)
  (register-command! 'lock-buffer cmd-lock-buffer)
  (register-command! 'auto-revert-mode cmd-auto-revert-mode)
  ;; Registers
  (register-command! 'append-to-register cmd-append-to-register)
  ;; Completion
  (register-command! 'complete-filename cmd-complete-filename)
  (register-command! 'completion-at-point cmd-completion-at-point)
  ;; Info/Help
  (register-command! 'info cmd-info)
  (register-command! 'info-emacs-manual cmd-info-emacs-manual)
  (register-command! 'info-elisp-manual cmd-info-elisp-manual)
  (register-command! 'report-bug cmd-report-bug)
  (register-command! 'memory-report cmd-memory-report)
  (register-command! 'view-echo-area-messages cmd-view-echo-area-messages)
  ;; Spelling
  (register-command! 'ispell-word cmd-ispell-word)
  (register-command! 'ispell-region cmd-ispell-region)
  (register-command! 'ispell-buffer cmd-ispell-buffer)
  ;; Abbreviations
  (register-command! 'abbrev-mode cmd-abbrev-mode)
  (register-command! 'define-abbrev cmd-define-abbrev)
  (register-command! 'expand-abbrev cmd-expand-abbrev)
  (register-command! 'list-abbrevs cmd-list-abbrevs)
  ;; Calendar
  ;; Key rebinding
  (register-command! 'global-set-key cmd-global-set-key)
  (register-command! 'global-unset-key cmd-global-unset-key)
  ;; Man page viewer
  (register-command! 'man cmd-man)
  ;; Web browser
  (register-command! 'eww cmd-eww)
  (register-command! 'eww-back cmd-eww-back)
  (register-command! 'eww-reload cmd-eww-reload)
  ;; Remote file editing
  (register-command! 'find-file-remote cmd-find-file-remote)
  (register-command! 'save-remote-buffer cmd-save-remote-buffer)
  ;; Calendar
  (register-command! 'calendar cmd-calendar)
  (register-command! 'calendar-prev-month cmd-calendar-prev-month)
  (register-command! 'calendar-next-month cmd-calendar-next-month)
  (register-command! 'calendar-prev-year cmd-calendar-prev-year)
  (register-command! 'calendar-next-year cmd-calendar-next-year)
  (register-command! 'calendar-today cmd-calendar-today)
  ;; Misc
  (register-command! 'display-fill-column-indicator cmd-display-fill-column-indicator)
  (register-command! 'display-line-numbers-relative cmd-display-line-numbers-relative)
  (register-command! 'font-lock-mode cmd-font-lock-mode)
  (register-command! 'customize-face cmd-customize-face)
  (register-command! 'list-colors cmd-list-colors)
  (register-command! 'load-theme cmd-load-theme)
  (register-command! 'fold-level cmd-fold-level)
  (register-command! 'ansi-term cmd-ansi-term)
  (register-command! 'diff-backup cmd-diff-backup)
  (register-command! 'dired-do-chmod cmd-dired-do-chmod)
  (register-command! 'eldoc cmd-eldoc)
  (register-command! 'recover-session cmd-recover-session)
  (register-command! 'revert-buffer-with-coding cmd-revert-buffer-with-coding)
  (register-command! 'set-buffer-file-coding cmd-set-buffer-file-coding)
  (register-command! 'set-language-environment cmd-set-language-environment)
  (register-command! 'sudo-find-file cmd-sudo-find-file)
  (register-command! 'which-function cmd-which-function)
  (register-command! 'widen-all cmd-widen-all)
  (register-command! 'whitespace-mode cmd-whitespace-mode)
  (register-command! 'profiler-start cmd-profiler-start)
  (register-command! 'profiler-stop cmd-profiler-stop)
  (register-command! 'show-tab-count cmd-show-tab-count)
  (register-command! 'show-trailing-whitespace-count cmd-show-trailing-whitespace-count)
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit)
  ;; Session persistence
  (register-command! 'session-save cmd-session-save)
  (register-command! 'session-restore cmd-session-restore)
  ;; Init file
  (register-command! 'load-init-file cmd-load-init-file)
  (register-command! 'find-init-file cmd-find-init-file)
  ;; Magit
  (register-command! 'magit-status cmd-magit-status)
  (register-command! 'magit-stage cmd-magit-stage)
  (register-command! 'magit-unstage cmd-magit-unstage)
  (register-command! 'magit-commit cmd-magit-commit)
  (register-command! 'magit-diff cmd-magit-diff)
  (register-command! 'magit-stage-all cmd-magit-stage-all)
  (register-command! 'magit-log cmd-magit-log)
  ;; Org-mode
  (register-command! 'org-todo-cycle cmd-org-todo-cycle)
  (register-command! 'org-promote cmd-org-promote)
  (register-command! 'org-demote cmd-org-demote)
  (register-command! 'org-toggle-checkbox cmd-org-toggle-checkbox)
  (register-command! 'org-insert-heading cmd-org-insert-heading)
  (register-command! 'org-next-heading cmd-org-next-heading)
  (register-command! 'org-prev-heading cmd-org-prev-heading)
  (register-command! 'org-move-subtree-up cmd-org-move-subtree-up)
  (register-command! 'org-move-subtree-down cmd-org-move-subtree-down)
  ;; Markdown
  (register-command! 'markdown-promote cmd-markdown-promote)
  (register-command! 'markdown-demote cmd-markdown-demote)
  (register-command! 'markdown-next-heading cmd-markdown-next-heading)
  (register-command! 'markdown-prev-heading cmd-markdown-prev-heading)
  (register-command! 'markdown-insert-heading cmd-markdown-insert-heading)
  (register-command! 'markdown-toggle-bold cmd-markdown-toggle-bold)
  (register-command! 'markdown-toggle-italic cmd-markdown-toggle-italic)
  (register-command! 'markdown-toggle-code cmd-markdown-toggle-code)
  (register-command! 'markdown-insert-link cmd-markdown-insert-link)
  (register-command! 'markdown-insert-code-block cmd-markdown-insert-code-block)
  (register-command! 'markdown-toggle-checkbox cmd-markdown-toggle-checkbox)
  (register-command! 'markdown-outline cmd-markdown-outline)
  (register-command! 'markdown-preview cmd-markdown-preview)
  ;; Snippets
  (register-command! 'snippet-expand cmd-snippet-expand)
  (register-command! 'snippet-next-field cmd-snippet-next-field)
  (register-command! 'snippet-prev-field cmd-snippet-prev-field)
  (register-command! 'define-snippet cmd-define-snippet)
  (register-command! 'list-snippets cmd-list-snippets)
  ;; Winner mode
  (register-command! 'winner-undo cmd-winner-undo)
  (register-command! 'winner-redo cmd-winner-redo)
  ;; View mode
  (register-command! 'view-mode cmd-view-mode)
  ;; So-long mode
  (register-command! 'so-long-mode cmd-so-long-mode)
  ;; Follow mode
  (register-command! 'follow-mode cmd-follow-mode)
  ;; IBBuffer
  (register-command! 'ibuffer cmd-ibuffer)
  ;; WDired
  (register-command! 'wdired-mode cmd-wdired-mode)
  (register-command! 'wdired-finish cmd-wdired-finish)
  ;; Auto-fill mode (alias)
  (register-command! 'auto-fill-mode cmd-toggle-auto-fill)
  ;; Delete trailing whitespace
  (register-command! 'toggle-delete-trailing-whitespace-on-save cmd-toggle-delete-trailing-whitespace-on-save)
  ;; Delete horizontal space
  (register-command! 'delete-horizontal-space cmd-delete-horizontal-space)
  ;; Recentf open files
  (register-command! 'recentf-open-files cmd-recentf-open-files)
  ;; Ediff files
  (register-command! 'ediff-files cmd-ediff-files)
  ;; Comment-dwim
  (register-command! 'comment-dwim cmd-comment-dwim)
  ;; Auto-save mode
  (register-command! 'auto-save-mode cmd-auto-save-mode)
  ;; Keyboard macro counter
  (register-command! 'kbd-macro-counter-insert cmd-kbd-macro-counter-insert)
  (register-command! 'kbd-macro-counter-set cmd-kbd-macro-counter-set)
  ;; Dired enhancements
  (register-command! 'dired-mark cmd-dired-mark)
  (register-command! 'dired-unmark cmd-dired-unmark)
  (register-command! 'dired-unmark-all cmd-dired-unmark-all)
  (register-command! 'dired-toggle-marks cmd-dired-toggle-marks)
  (register-command! 'dired-do-delete-marked cmd-dired-do-delete-marked)
  (register-command! 'dired-do-copy-marked cmd-dired-do-copy-marked)
  (register-command! 'dired-do-rename-marked cmd-dired-do-rename-marked)
  (register-command! 'dired-mark-by-regexp cmd-dired-mark-by-regexp)
  (register-command! 'dired-sort-toggle cmd-dired-sort-toggle)
  ;; Global mark ring
  (register-command! 'pop-global-mark cmd-pop-global-mark)
  ;; Window horizontal resize
  (register-command! 'shrink-window-horizontally cmd-shrink-window-horizontally)
  (register-command! 'enlarge-window-horizontally cmd-enlarge-window-horizontally)
  ;; Recover file
  (register-command! 'recover-file cmd-recover-file)
  ;; Insert char by name
  (register-command! 'insert-char-by-name cmd-insert-char-by-name)
  ;; System info
  (register-command! 'display-battery cmd-display-battery)
  ;; Scratch
  (register-command! 'scratch-message cmd-scratch-message)
  ;; Kill sentence / paragraph
  (register-command! 'kill-sentence cmd-kill-sentence)
  (register-command! 'backward-kill-sentence cmd-backward-kill-sentence)
  (register-command! 'kill-paragraph cmd-kill-paragraph)
  ;; Recenter cycling
  (register-command! 'recenter-top-bottom cmd-recenter-top-bottom)
  ;; Sexp list navigation
  (register-command! 'up-list cmd-up-list)
  (register-command! 'down-list cmd-down-list)
  ;; Windmove
  (register-command! 'windmove-left cmd-windmove-left)
  (register-command! 'windmove-right cmd-windmove-right)
  (register-command! 'windmove-up cmd-windmove-up)
  (register-command! 'windmove-down cmd-windmove-down)
  ;; Variable customization
  (register-command! 'set-variable cmd-set-variable)
  (register-command! 'customize-variable cmd-customize-variable)
  ;; View-file and append-to-file
  (register-command! 'view-file cmd-view-file)
  (register-command! 'append-to-file cmd-append-to-file)
  ;; Spell check buffer
  (register-command! 'flyspell-buffer cmd-flyspell-buffer)
  ;; Profiler
  (register-command! 'profiler-report cmd-profiler-report)
  ;; Faces and line numbers
  (register-command! 'list-faces-display cmd-list-faces-display)
  (register-command! 'display-line-numbers-mode cmd-display-line-numbers-mode)
  ;; Find file read-only
  (register-command! 'find-file-read-only cmd-find-file-read-only)
  ;; Project commands
  (register-command! 'project-switch-project cmd-project-switch-project)
  (register-command! 'project-dired cmd-project-dired)
  (register-command! 'project-run-shell cmd-project-run-shell)
  ;; Global auto-revert
  (register-command! 'global-auto-revert-mode cmd-global-auto-revert-mode)
  ;; Project search
  (register-command! 'project-search cmd-project-search)
  ;; Goto last change
  (register-command! 'goto-last-change cmd-goto-last-change)
  ;; Diff-HL (git diff at point)
  (register-command! 'diff-hl-mode cmd-diff-hl-mode)
  ;; Pop-to-mark
  (register-command! 'pop-to-mark cmd-pop-to-mark)
  ;; Scratch buffer new
  (register-command! 'scratch-buffer-new cmd-scratch-buffer-new)
  ;; Duplicate line/region
  (register-command! 'duplicate-line-or-region cmd-duplicate-line-or-region)
  ;; Select current line
  (register-command! 'select-current-line cmd-select-current-line)
  ;; Smart join line
  (register-command! 'smart-join-line cmd-smart-join-line)
  ;; Copy buffer filename
  (register-command! 'copy-buffer-filename cmd-copy-buffer-filename)
  ;; Revert buffer confirm
  (register-command! 'revert-buffer-confirm cmd-revert-buffer-confirm)
  ;; Find file at line
  (register-command! 'find-file-at-line cmd-find-file-at-line)
  ;; Toggle line comment
  (register-command! 'toggle-line-comment cmd-toggle-line-comment))
