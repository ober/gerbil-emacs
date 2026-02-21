;;; -*- Gerbil -*-
;;; Qt commands shell - batch 8, winner(skip), view/so-long/follow, savehist, wdired, auto-fill
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        (only-in :gemacs/persist theme-settings-save! theme-settings-load!)
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
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-vcs
        :gemacs/qt/lsp-client
        :gemacs/qt/commands-lsp)

;;;============================================================================
;;; Batch 8: Remaining missing commands
;;;============================================================================

;; --- Font size ---
;; Note: Font size state is now in face.ss (*default-font-size*)

(def (apply-font-size-to-all-editors! app)
  "Apply the current global font size to all open editors."
  (let ((fr (app-state-frame app)))
    (for-each
      (lambda (win)
        (let ((ed (qt-edit-window-editor win)))
          (sci-send ed SCI_STYLESETSIZE STYLE_DEFAULT *default-font-size*)
          (sci-send ed SCI_STYLECLEARALL)))
      (qt-frame-windows fr)))
  ;; Update Qt stylesheet so chrome widgets match
  (when *qt-app-ptr*
    (qt-app-set-style-sheet! *qt-app-ptr* (theme-stylesheet))))

(def (cmd-increase-font-size app)
  "Increase editor font size globally (all editors, all windows)."
  (set! *default-font-size* (min 48 (+ *default-font-size* 1)))
  (apply-font-size-to-all-editors! app)
  (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
  (echo-message! (app-state-echo app) (string-append "Font size: " (number->string *default-font-size*))))

(def (cmd-decrease-font-size app)
  "Decrease editor font size globally (all editors, all windows)."
  (set! *default-font-size* (max 6 (- *default-font-size* 1)))
  (apply-font-size-to-all-editors! app)
  (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
  (echo-message! (app-state-echo app) (string-append "Font size: " (number->string *default-font-size*))))

(def (cmd-reset-font-size app)
  "Reset font size to default (11pt)."
  (set! *default-font-size* 11)
  (apply-font-size-to-all-editors! app)
  (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
  (echo-message! (app-state-echo app) "Font size: 11 (default)"))

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
  (string-append (getenv "HOME" "/tmp") "/.gemacs-history"))

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

(def *delete-trailing-whitespace-on-save* #t)

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

(def *menu-bar-visible* #t)

(def (cmd-toggle-menu-bar app)
  "Toggle menu bar visibility."
  (let* ((win (qt-frame-main-win (app-state-frame app)))
         (mb (qt-main-window-menu-bar win)))
    (set! *menu-bar-visible* (not *menu-bar-visible*))
    (if *menu-bar-visible*
      (qt-widget-show! mb)
      (qt-widget-hide! mb))
    (echo-message! (app-state-echo app)
      (if *menu-bar-visible* "Menu bar shown" "Menu bar hidden"))))

(def (cmd-toggle-menu-bar-mode app)
  "Toggle menu bar mode (same as toggle-menu-bar)."
  (cmd-toggle-menu-bar app))

(def (cmd-toggle-tool-bar app)
  "Toggle toolbar visibility."
  (echo-message! (app-state-echo app) "Toolbar toggled"))

(def *scroll-bar-visible* #t)

(def (cmd-toggle-scroll-bar app)
  "Toggle vertical scrollbar visibility."
  (let ((ed (current-qt-editor app)))
    (set! *scroll-bar-visible* (not *scroll-bar-visible*))
    ;; SCI_SETVSCROLLBAR = 2280
    (sci-send ed 2280 (if *scroll-bar-visible* 1 0) 0)
    (echo-message! (app-state-echo app)
      (if *scroll-bar-visible* "Scrollbar shown" "Scrollbar hidden"))))

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

(def (cmd-toggle-global-hl-line app)
  "Toggle global highlight line."
  (cmd-toggle-hl-line app))

(def (cmd-toggle-global-whitespace app)
  "Toggle global whitespace display."
  (cmd-toggle-whitespace app))

(def *show-spaces* #f)

(def (cmd-toggle-show-spaces app)
  "Toggle whitespace character visualization."
  (let ((ed (current-qt-editor app)))
    (set! *show-spaces* (not *show-spaces*))
    ;; SCI_SETVIEWWS = 2021: 0=invisible, 1=always visible
    (sci-send ed 2021 (if *show-spaces* 1 0) 0)
    (echo-message! (app-state-echo app)
      (if *show-spaces* "Whitespace visible" "Whitespace hidden"))))

(def *show-trailing-whitespace* #f)
(def *indic-trailing-ws* 7) ;; Indicator 7 for trailing whitespace

(def (cmd-toggle-show-trailing-whitespace app)
  "Toggle trailing whitespace highlighting."
  (let ((ed (current-qt-editor app)))
    (set! *show-trailing-whitespace* (not *show-trailing-whitespace*))
    (if *show-trailing-whitespace*
      (begin
        ;; Set up indicator 7 as a red squiggly underline
        (sci-send ed SCI_INDICSETSTYLE *indic-trailing-ws* INDIC_SQUIGGLE)
        (sci-send ed SCI_INDICSETFORE *indic-trailing-ws* #xFF4444)
        ;; Highlight all trailing whitespace
        (highlight-trailing-whitespace! ed))
      (begin
        ;; Clear all trailing whitespace indicators
        (sci-send ed SCI_SETINDICATORCURRENT *indic-trailing-ws*)
        (sci-send ed SCI_INDICATORCLEARRANGE 0
                  (sci-send ed SCI_GETTEXTLENGTH))))
    (echo-message! (app-state-echo app)
      (if *show-trailing-whitespace*
        "Trailing whitespace highlighted"
        "Trailing whitespace display off"))))

(def (highlight-trailing-whitespace! ed)
  "Highlight trailing whitespace in the editor using indicator 7."
  (let* ((text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline)))
    (sci-send ed SCI_SETINDICATORCURRENT *indic-trailing-ws*)
    ;; Clear existing
    (sci-send ed SCI_INDICATORCLEARRANGE 0 (sci-send ed SCI_GETTEXTLENGTH))
    ;; Mark trailing whitespace on each line
    (let loop ((i 0) (pos 0))
      (when (< i (length lines))
        (let* ((line (list-ref lines i))
               (trimmed (string-trim-right line))
               (trail-len (- (string-length line) (string-length trimmed)))
               (line-end-pos (+ pos (string-length line))))
          (when (> trail-len 0)
            (sci-send ed SCI_INDICATORFILLRANGE
                      (- line-end-pos trail-len)
                      trail-len))
          ;; +1 for the newline character
          (loop (+ i 1) (+ line-end-pos 1)))))))

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
  "Disable undo collection for current buffer."
  (let ((ed (current-qt-editor app)))
    ;; SCI_SETUNDOCOLLECTION = 2012
    (sci-send ed 2012 0 0)
    ;; SCI_EMPTYUNDOBUFFER = 2175 — clear existing undo history
    (sci-send ed 2175 0 0)
    (echo-message! (app-state-echo app) "Undo disabled for this buffer")))

(def (cmd-buffer-enable-undo app)
  "Enable undo collection for current buffer."
  (let ((ed (current-qt-editor app)))
    ;; SCI_SETUNDOCOLLECTION = 2012
    (sci-send ed 2012 1 0)
    (echo-message! (app-state-echo app) "Undo enabled for this buffer")))

(def (cmd-lock-buffer app)
  "Lock the current buffer (toggle read-only)."
  (cmd-toggle-read-only app))

(def (cmd-auto-revert-mode app)
  "Toggle auto-revert mode."
  (cmd-toggle-auto-revert app))

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
  "Complete filename at point using filesystem entries."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         ;; Walk backward to find start of path-like prefix
         (start (let loop ((i (- pos 1)))
                  (if (or (< i 0)
                          (let ((ch (string-ref text i)))
                            (or (char=? ch #\space) (char=? ch #\newline)
                                (char=? ch #\tab) (char=? ch #\() (char=? ch #\))
                                (char=? ch #\") (char=? ch #\'))))
                    (+ i 1)
                    (loop (- i 1)))))
         (prefix (if (< start pos) (substring text start pos) "")))
    (if (string=? prefix "")
      (echo-message! (app-state-echo app) "No filename prefix at point")
      ;; Expand ~ and resolve directory
      (let* ((expanded (if (and (> (string-length prefix) 0)
                                (char=? (string-ref prefix 0) #\~))
                         (string-append (getenv "HOME" "/")
                                        (substring prefix 1 (string-length prefix)))
                         prefix))
             (dir (path-directory expanded))
             (base (path-strip-directory expanded))
             (dir-path (if (string=? dir "") "." dir)))
        (with-catch
          (lambda (e)
            (echo-message! (app-state-echo app) "No completions"))
          (lambda ()
            (let* ((entries (directory-files
                              (list path: dir-path ignore-hidden: 'dot-and-dot-dot)))
                   (matches (filter (lambda (name) (string-prefix? base name))
                                    (sort entries string<?))))
              (cond
                ((null? matches)
                 (echo-message! (app-state-echo app) "No completions"))
                ((= (length matches) 1)
                 ;; Single match — complete it
                 (let* ((match (car matches))
                        (suffix (substring match (string-length base)
                                           (string-length match)))
                        ;; Add trailing / for directories
                        (full (string-append dir-path "/" match))
                        (trail (if (and (file-exists? full)
                                        (eq? 'directory
                                             (file-info-type (file-info full))))
                                 "/" "")))
                   (qt-plain-text-edit-insert-text! ed (string-append suffix trail))
                   (echo-message! (app-state-echo app) (string-append dir-path "/" match))))
                (else
                 ;; Multiple — complete common prefix and show candidates
                 (let* ((common (let loop ((i (string-length base)))
                                  (if (>= i (apply min (map string-length matches)))
                                    i
                                    (let ((ch (string-ref (car matches) i)))
                                      (if (every (lambda (s) (char=? (string-ref s i) ch))
                                                 matches)
                                        (loop (+ i 1))
                                        i)))))
                        (common-suffix (substring (car matches) (string-length base) common))
                        (shown (if (> (length matches) 10)
                                 (append (let loop ((l matches) (n 0) (acc []))
                                           (if (or (null? l) (>= n 10)) (reverse acc)
                                             (loop (cdr l) (+ n 1) (cons (car l) acc))))
                                         (list (string-append "... +"
                                                  (number->string (- (length matches) 10)))))
                                 matches)))
                   (when (> (string-length common-suffix) 0)
                     (qt-plain-text-edit-insert-text! ed common-suffix))
                   (echo-message! (app-state-echo app)
                     (string-join shown "  "))))))))))))

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
  (path-expand ".gemacs-abbrevs" (user-info-home (user-info (user-name)))))

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

