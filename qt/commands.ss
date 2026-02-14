;;; -*- Gerbil -*-
;;; Qt command implementations for gerbil-emacs
;;;
;;; All Emacs commands reimplemented using Qt QPlainTextEdit APIs.

(export qt-register-all-commands!
        dired-open-directory!)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/highlight)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (current-qt-editor app)
  (qt-edit-window-editor (qt-current-window (app-state-frame app))))

(def (current-qt-buffer app)
  (qt-edit-window-buffer (qt-current-window (app-state-frame app))))

;; Auto-save path: #filename# (Emacs convention)
(def (make-auto-save-path path)
  (let* ((dir (path-directory path))
         (name (path-strip-directory path)))
    (path-expand (string-append "#" name "#") dir)))

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
    (if (repl-buffer? buf)
      ;; In REPL buffers, don't delete past the prompt.
      ;; Use cursor-position which is in the same units as the document model.
      ;; prompt-pos is set from string-length(toPlainText) which matches cursor units.
      (let* ((ed (current-qt-editor app))
             (pos (qt-plain-text-edit-cursor-position ed))
             (rs (hash-get *repl-state* buf)))
        (when (and rs (> pos (repl-state-prompt-pos rs)))
          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                           mode: QT_KEEP_ANCHOR)
          (qt-plain-text-edit-remove-selected-text! ed)))
      (let ((ed (current-qt-editor app)))
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_PREVIOUS_CHAR
                                         mode: QT_KEEP_ANCHOR)
        (qt-plain-text-edit-remove-selected-text! ed)))))

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

(def (cmd-newline app)
  (let ((buf (current-qt-buffer app)))
    (cond
      ((dired-buffer? buf)  (cmd-dired-find-file app))
      ((eq? (buffer-lexer-lang buf) 'buffer-list) (cmd-buffer-list-select app))
      ((repl-buffer? buf)   (cmd-repl-send app))
      ((eshell-buffer? buf) (cmd-eshell-send app))
      ((shell-buffer? buf)  (cmd-shell-send app))
      (else (qt-plain-text-edit-insert-text! (current-qt-editor app) "\n")))))

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
                  (qt-plain-text-edit-set-cursor-position! ed 0))))
            (qt-setup-highlighting! app buf)
            (echo-message! echo (string-append "Opened: " filename))))))))

(def (cmd-save-buffer app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      ;; Save to existing path
      (begin
        ;; Create backup file if original exists and hasn't been backed up yet
        (when (and (file-exists? path) (not (buffer-backup-done? buf)))
          (let ((backup-path (string-append path "~")))
            (with-catch
              (lambda (e) #f)  ; Ignore backup errors
              (lambda ()
                (copy-file path backup-path)
                (set! (buffer-backup-done? buf) #t)))))
        (let ((text (qt-plain-text-edit-text ed)))
          (write-string-to-file path text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          ;; Remove auto-save file if it exists
          (let ((auto-save-path (make-auto-save-path path)))
            (when (file-exists? auto-save-path)
              (delete-file auto-save-path)))
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
              ;; Clean up syntax highlighter if applicable
              (qt-remove-highlighting! buf)
              ;; Clean up dired entries if applicable
              (hash-remove! *dired-entries* buf)
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
              (qt-buffer-kill! buf)
              (echo-message! echo (string-append "Killed " target-name))))
          (echo-error! echo (string-append "No buffer: " target-name)))))))

;;;============================================================================
;;; Window commands
;;;============================================================================

(def (cmd-split-window app)
  (let ((new-ed (qt-frame-split! (app-state-frame app))))
    ;; Install key handler on the new editor
    (when (app-state-key-handler app)
      ((app-state-key-handler app) new-ed))))

(def (cmd-split-window-right app)
  (let ((new-ed (qt-frame-split-right! (app-state-frame app))))
    ;; Install key handler on the new editor
    (when (app-state-key-handler app)
      ((app-state-key-handler app) new-ed))))

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

(def (cmd-execute-extended-command app)
  (let* ((cmd-names (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         (input (qt-echo-read-string-with-completion app "M-x " cmd-names)))
    (when (and input (> (string-length input) 0))
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
         (bufs (buffer-list))
         (header "  Buffer\t\tFile\n  ------\t\t----\n")
         (lines (map (lambda (buf)
                       (let ((name (buffer-name buf))
                             (path (or (buffer-file-path buf) "")))
                         (string-append "  " name "\t\t" path)))
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
      ((repl-buffer? buf)
       (let* ((ed (current-qt-editor app))
              (pos (qt-plain-text-edit-cursor-position ed))
              (rs (hash-get *repl-state* buf)))
         (when (and rs (>= pos (repl-state-prompt-pos rs)))
           (qt-plain-text-edit-insert-text! ed "  "))))
      (else
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
                   (qt-completer-complete-rect! c 0 0 200 20)))))))))))

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

(def (cmd-query-replace app)
  (let* ((echo (app-state-echo app))
         (from-str (qt-echo-read-string app "Query replace: ")))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (qt-echo-read-string app
                      (string-append "Replace \"" from-str "\" with: "))))
        (when to-str
          ;; Simple replace-all for Qt (no interactive y/n since we lack peek-event)
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (new-text (string-replace-all text from-str to-str))
                 (pos (qt-plain-text-edit-cursor-position ed)))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length new-text)))
            (echo-message! echo "Replaced all occurrences")))))))

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
  (set! (app-state-running app) #f)
  ;; Quit the Qt event loop
  (let ((fr (app-state-frame app)))
    (qt-widget-close! (qt-frame-main-win fr))))

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
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit))
