;;; -*- Gerbil -*-
;;; Qt command implementations for gerbil-emacs
;;;
;;; All Emacs commands reimplemented using Qt QPlainTextEdit APIs.

(export qt-register-all-commands!)

(import :std/sugar
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/echo)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (current-qt-editor app)
  (qt-edit-window-editor (qt-current-window (app-state-frame app))))

(def (current-qt-buffer app)
  (qt-edit-window-buffer (qt-current-window (app-state-frame app))))

;;;============================================================================
;;; Navigation commands
;;;============================================================================

(def (cmd-forward-char app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_NEXT_CHAR))

(def (cmd-backward-char app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_PREVIOUS_CHAR))

(def (cmd-next-line app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_DOWN))

(def (cmd-previous-line app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_UP))

(def (cmd-beginning-of-line app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_START_OF_BLOCK))

(def (cmd-end-of-line app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_END_OF_BLOCK))

(def (cmd-forward-word app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_NEXT_WORD))

(def (cmd-backward-word app)
  (qt-plain-text-edit-move-cursor! (current-qt-editor app)
                                   QT_CURSOR_PREVIOUS_WORD))

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
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                     mode: QT_KEEP_ANCHOR)
    (qt-plain-text-edit-remove-selected-text! ed)))

(def (cmd-newline app)
  (qt-plain-text-edit-insert-text! (current-qt-editor app) "\n"))

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
      ;; At end of line: delete the newline
      (begin
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_NEXT_CHAR
                                         mode: QT_KEEP_ANCHOR)
        (qt-plain-text-edit-remove-selected-text! ed))
      ;; Kill to end of line: select and cut
      (begin
        (qt-plain-text-edit-set-selection! ed pos line-end)
        (qt-plain-text-edit-cut! ed)
        ;; Store in kill ring
        ;; (clipboard already has the text from cut)
        ))))

(def (cmd-yank app)
  (qt-plain-text-edit-paste! (current-qt-editor app)))

;;;============================================================================
;;; Mark and region
;;;============================================================================

(def (cmd-set-mark app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (buf (current-qt-buffer app)))
    (set! (buffer-mark buf) pos)
    (echo-message! (app-state-echo app) "Mark set")))

(def (cmd-kill-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        (qt-plain-text-edit-set-selection! ed (min mark pos) (max mark pos))
        (qt-plain-text-edit-cut! ed)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-copy-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        (qt-plain-text-edit-set-selection! ed (min mark pos) (max mark pos))
        (qt-plain-text-edit-copy! ed)
        ;; Deselect
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region copied"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; File operations
;;;============================================================================

(def (cmd-find-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Find file: ")))
    (when filename
      (when (> (string-length filename) 0)
        (let* ((name (path-strip-directory filename))
               (fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (qt-buffer-create! name ed filename)))
          ;; Switch to new buffer
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          ;; Load file if it exists
          (when (file-exists? filename)
            (let ((text (read-file-as-string filename)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0))))
          (echo-message! echo (string-append "Opened: " filename)))))))

(def (cmd-save-buffer app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      ;; Save to existing path
      (begin
        (let ((text (qt-plain-text-edit-text ed)))
          (write-string-to-file path text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (echo-message! echo (string-append "Wrote " path))))
      ;; No path: prompt for one
      (let ((filename (qt-echo-read-string app "Write file: ")))
        (when (and filename (> (string-length filename) 0))
          (set! (buffer-file-path buf) filename)
          (set! (buffer-name buf) (path-strip-directory filename))
          (let ((text (qt-plain-text-edit-text ed)))
            (write-string-to-file filename text)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (echo-message! echo (string-append "Wrote " filename))))))))

;;;============================================================================
;;; Buffer commands
;;;============================================================================

(def (cmd-switch-buffer app)
  (let* ((echo (app-state-echo app))
         (name (qt-echo-read-string app "Switch to buffer: ")))
    (when name
      (let ((buf (buffer-by-name name)))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf))
          (echo-error! echo (string-append "No buffer: " name)))))))

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
              (qt-buffer-kill! buf)
              (echo-message! echo (string-append "Killed " target-name))))
          (echo-error! echo (string-append "No buffer: " target-name)))))))

;;;============================================================================
;;; Window commands
;;;============================================================================

(def (cmd-split-window app)
  (qt-frame-split! (app-state-frame app)))

(def (cmd-other-window app)
  (qt-frame-other-window! (app-state-frame app)))

(def (cmd-delete-window app)
  (let ((fr (app-state-frame app)))
    (if (> (length (qt-frame-windows fr)) 1)
      (qt-frame-delete-window! fr)
      (echo-error! (app-state-echo app) "Can't delete sole window"))))

(def (cmd-delete-other-windows app)
  (qt-frame-delete-other-windows! (app-state-frame app)))

;;;============================================================================
;;; Search
;;;============================================================================

(def (cmd-search-forward app)
  (let* ((echo (app-state-echo app))
         (default (or (app-state-last-search app) ""))
         (prompt (if (string=? default "")
                   "Search: "
                   (string-append "Search [" default "]: ")))
         (input (qt-echo-read-string app prompt)))
    (when input
      (let* ((query (if (string=? input "") default input))
             (ed (current-qt-editor app)))
        (when (> (string-length query) 0)
          (set! (app-state-last-search app) query)
          (let ((found (qt-plain-text-edit-find-text ed query)))
            (if (>= found 0)
              (qt-plain-text-edit-ensure-cursor-visible! ed)
              ;; Wrap around from beginning
              (begin
                (qt-plain-text-edit-set-cursor-position! ed 0)
                (let ((found2 (qt-plain-text-edit-find-text ed query)))
                  (if (>= found2 0)
                    (begin
                      (qt-plain-text-edit-ensure-cursor-visible! ed)
                      (echo-message! echo "Wrapped"))
                    (echo-error! echo
                                 (string-append "Not found: " query))))))))))))

(def (cmd-search-backward app)
  (let* ((echo (app-state-echo app))
         (default (or (app-state-last-search app) ""))
         (prompt (if (string=? default "")
                   "Search backward: "
                   (string-append "Search backward [" default "]: ")))
         (input (qt-echo-read-string app prompt)))
    (when input
      (let* ((query (if (string=? input "") default input))
             (ed (current-qt-editor app)))
        (when (> (string-length query) 0)
          (set! (app-state-last-search app) query)
          (let ((found (qt-plain-text-edit-find-text ed query
                          flags: QT_FIND_BACKWARD)))
            (if (>= found 0)
              (qt-plain-text-edit-ensure-cursor-visible! ed)
              (echo-error! echo
                           (string-append "Not found: " query)))))))))

;;;============================================================================
;;; Misc commands
;;;============================================================================

(def (cmd-keyboard-quit app)
  (echo-message! (app-state-echo app) "Quit")
  (set! (app-state-key-state app) (make-initial-key-state)))

(def (cmd-quit app)
  (set! (app-state-running app) #f)
  ;; Quit the Qt event loop
  (let ((fr (app-state-frame app)))
    (qt-widget-close! (qt-frame-main-win fr))))

;;;============================================================================
;;; Register all commands
;;;============================================================================

(def (qt-register-all-commands!)
  ;; Navigation
  (register-command! 'forward-char cmd-forward-char)
  (register-command! 'backward-char cmd-backward-char)
  (register-command! 'next-line cmd-next-line)
  (register-command! 'previous-line cmd-previous-line)
  (register-command! 'beginning-of-line cmd-beginning-of-line)
  (register-command! 'end-of-line cmd-end-of-line)
  (register-command! 'forward-word cmd-forward-word)
  (register-command! 'backward-word cmd-backward-word)
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
  ;; Kill/Yank
  (register-command! 'kill-line cmd-kill-line)
  (register-command! 'yank cmd-yank)
  ;; Mark/Region
  (register-command! 'set-mark cmd-set-mark)
  (register-command! 'kill-region cmd-kill-region)
  (register-command! 'copy-region cmd-copy-region)
  ;; File
  (register-command! 'find-file cmd-find-file)
  (register-command! 'save-buffer cmd-save-buffer)
  ;; Buffer
  (register-command! 'switch-buffer cmd-switch-buffer)
  (register-command! 'kill-buffer-cmd cmd-kill-buffer-cmd)
  ;; Window
  (register-command! 'split-window cmd-split-window)
  (register-command! 'other-window cmd-other-window)
  (register-command! 'delete-window cmd-delete-window)
  (register-command! 'delete-other-windows cmd-delete-other-windows)
  ;; Search
  (register-command! 'search-forward cmd-search-forward)
  (register-command! 'search-backward cmd-search-backward)
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit))
