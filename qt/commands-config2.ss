;;; -*- Gerbil -*-
;;; Qt commands config2 - auto-save, macros, dired marks, projects, windmove, misc
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/async
        :gemacs/editor
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-ide2
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-modes
        :gemacs/qt/commands-config)

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
  "Delete all marked files in dired (async)."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf))
         (dir (buffer-file-path buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((confirm (qt-echo-read-string app
                       (string-append "Delete " (number->string (length files))
                                      " marked files? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (echo-message! (app-state-echo app) "Deleting...")
          (spawn/name 'dired-delete
            (lambda ()
              (let ((count 0))
                (for-each
                  (lambda (path)
                    (with-catch
                      (lambda (e) #f)
                      (lambda ()
                        (delete-file path)
                        (set! count (+ count 1)))))
                  files)
                (ui-queue-push!
                  (lambda ()
                    (when dir (dired-open-directory! app dir))
                    (echo-message! (app-state-echo app)
                      (string-append "Deleted " (number->string count) " files"))))))))))))

(def (cmd-dired-do-copy-marked app)
  "Copy all marked files in dired to a destination directory (async)."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf))
         (dir (buffer-file-path buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((dest (qt-echo-read-string app "Copy to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest)))
            (echo-message! (app-state-echo app) "Copying...")
            (spawn/name 'dired-copy
              (lambda ()
                (let ((count 0))
                  (for-each
                    (lambda (path)
                      (with-catch
                        (lambda (e) #f)
                        (lambda ()
                          (let ((target (path-expand (path-strip-directory path) dest-dir)))
                            (copy-file path target)
                            (set! count (+ count 1))))))
                    files)
                  (ui-queue-push!
                    (lambda ()
                      (when dir (dired-open-directory! app dir))
                      (echo-message! (app-state-echo app)
                        (string-append "Copied " (number->string count)
                                       " files to " dest-dir)))))))))))))

(def (cmd-dired-do-rename-marked app)
  "Move/rename all marked files in dired to a destination directory (async)."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf))
         (dir (buffer-file-path buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((dest (qt-echo-read-string app "Move to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest)))
            (echo-message! (app-state-echo app) "Moving...")
            (spawn/name 'dired-rename
              (lambda ()
                (let ((count 0))
                  (for-each
                    (lambda (path)
                      (with-catch
                        (lambda (e) #f)
                        (lambda ()
                          (let ((target (path-expand (path-strip-directory path) dest-dir)))
                            (rename-file path target)
                            (set! count (+ count 1))))))
                    files)
                  (ui-queue-push!
                    (lambda ()
                      (when dir (dired-open-directory! app dir))
                      (echo-message! (app-state-echo app)
                        (string-append "Moved " (number->string count)
                                       " files to " dest-dir)))))))))))))

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

(def (cmd-dired-do-chmod app)
  "Change permissions of marked files (or file under cursor) in dired."
  (let* ((buf (current-qt-buffer app))
         (marked (dired-marked-files buf))
         (files (if (null? marked)
                  ;; Fall back to file under cursor
                  (let* ((ed (current-qt-editor app))
                         (line (qt-plain-text-edit-cursor-line ed))
                         (idx (- line 3))
                         (entries (hash-get *dired-entries* buf)))
                    (if (and entries (>= idx 0) (< idx (vector-length entries)))
                      (list (vector-ref entries idx))
                      []))
                  marked)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No file on this line")
      (let ((mode-str (qt-echo-read-string app
                        (string-append "chmod " (number->string (length files))
                                       " file(s) to mode (e.g. 755): "))))
        (when (and mode-str (> (string-length mode-str) 0))
          (let ((count 0))
            (for-each
              (lambda (path)
                (with-catch
                  (lambda (e)
                    (echo-error! (app-state-echo app)
                      (string-append "chmod failed: " (path-strip-directory path))))
                  (lambda ()
                    (let* ((proc (open-process
                                  (list path: "chmod"
                                        arguments: (list mode-str path)
                                        stdout-redirection: #f
                                        stderr-redirection: #f)))
                           (rc (process-status proc)))
                      (when (zero? rc)
                        (set! count (+ count 1)))))))
              files)
            ;; Refresh dired listing
            (let ((dir (buffer-file-path buf)))
              (when dir (dired-open-directory! app dir)))
            (echo-message! (app-state-echo app)
              (string-append "Changed mode of " (number->string count)
                             " file(s) to " mode-str))))))))

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
  (adjust-window-size! app -50)
  (echo-message! (app-state-echo app) "Window shrunk horizontally"))

(def (cmd-enlarge-window-horizontally app)
  "Enlarge the current window horizontally."
  (adjust-window-size! app 50)
  (echo-message! (app-state-echo app) "Window enlarged horizontally"))

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
                         (lambda (v) (set! *fill-column* (or (string->number v) 80)))))
    ("tab-width" (cons (lambda () *tab-width*)
                       (lambda (v) (set! *tab-width* (or (string->number v) 4)))))
    ("indent-tabs-mode" (cons (lambda () *indent-tabs-mode*)
                              (lambda (v) (set! *indent-tabs-mode*
                                            (member v '("t" "true" "yes" "1" "on"))))))
    ("undo-history-max" (cons (lambda () *undo-history-max*)
                              (lambda (v) (set! *undo-history-max* (or (string->number v) 50)))))
    ("hl-todo-keywords" (cons (lambda () *hl-todo-keywords*)
                              (lambda (v) (set! *hl-todo-keywords*
                                            (string-tokenize v (char-set-complement (char-set #\,)))))))
    ("so-long-threshold" (cons (lambda () *so-long-threshold*)
                               (lambda (v) (set! *so-long-threshold* (or (string->number v) 10000)))))))

(def (cmd-set-variable app)
  "Set a variable to a value interactively with narrowing selection."
  (let* ((echo (app-state-echo app))
         (var-names (sort (let ((acc [])) (hash-for-each (lambda (k v) (set! acc (cons k acc))) *user-variables*) acc) string<?))
         (varname (qt-echo-read-with-narrowing app "Set variable:" var-names))
         (entry (and varname (hash-get *user-variables* varname))))
    (if entry
      (let* ((cur ((car entry)))
             (val (qt-echo-read-string echo
                    (string-append varname " (current: "
                                   (if cur (object->string cur) "nil")
                                   "): "))))
        (when (and val (> (string-length val) 0))
          ((cdr entry) val)
          (echo-message! echo (string-append varname " = " val))))
      (when varname
        (echo-error! echo (string-append "Unknown variable: " varname))))))

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
         ;; Collect known projects from open buffers + explicit list
         (buf-roots (let loop ((bs *buffer-list*) (acc []))
                      (if (null? bs) acc
                        (let* ((b (car bs))
                               (fp (buffer-file-path b))
                               (root (and fp (detect-project-root (path-directory fp)))))
                          (loop (cdr bs) (if (and root (not (member root acc)))
                                           (cons root acc) acc))))))
         (all-projects (let loop ((ks (hash-keys *known-projects*)) (acc buf-roots))
                         (if (null? ks) (sort acc string<?)
                           (loop (cdr ks) (if (member (car ks) acc) acc (cons (car ks) acc))))))
         (dir (if (null? all-projects)
                (qt-echo-read-string app "Switch to project: ")
                (qt-echo-read-with-narrowing app "Switch to project: " all-projects))))
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
      (echo-message! echo "Searching...")
      (spawn/name 'project-search
        (lambda ()
          (let ((output
                  (with-catch
                    (lambda (e) [])
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
                             (lines (let loop ((acc []))
                                      (let ((line (read-line proc)))
                                        (if (eof-object? line)
                                          (reverse acc)
                                          (loop (cons line acc)))))))
                        (close-port proc)
                        lines)))))
            (ui-queue-push!
              (lambda ()
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
                      (string-append (number->string (length output)) " match(es) found"))))))))))))

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

(def *diff-hl-active* #f)

(def (parse-hunk-header line)
  "Parse @@ -old,count +new,count @@ to extract new-start and new-count.
   Returns (start . count) or #f."
  (let ((at-pos (string-contains line "+"))
        (end-pos (string-contains line " @@" 3)))
    (when (and at-pos end-pos)
      (let* ((plus-part (substring line at-pos end-pos))
             (comma (string-contains plus-part ",")))
        (if comma
          (let ((start (string->number (substring plus-part 1 comma)))
                (count (string->number (substring plus-part (+ comma 1)
                                         (string-length plus-part)))))
            (and start count (cons start count)))
          (let ((start (string->number (substring plus-part 1
                                         (string-length plus-part)))))
            (and start (cons start 1))))))))

(def (cmd-diff-hl-mode app)
  "Highlight lines changed since last git commit directly in the buffer.
   Green = added, yellow = modified. Toggle on/off."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (path (buffer-file-path buf)))
    (cond
      ((not path)
       (echo-message! echo "Buffer has no file"))
      ;; Toggle off
      (*diff-hl-active*
       (set! *diff-hl-active* #f)
       (qt-extra-selections-clear! ed)
       (qt-extra-selections-apply! ed)
       (echo-message! echo "diff-hl: off"))
      ;; Toggle on: run git diff and highlight changed lines
      (else
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
               (echo-message! echo "No uncommitted changes")
               ;; Parse hunks and highlight lines
               (let* ((hunks (filter identity
                               (map (lambda (line)
                                      (if (and (>= (string-length line) 3)
                                               (string=? (substring line 0 2) "@@"))
                                        (parse-hunk-header line)
                                        #f))
                                    output))))
                 (qt-extra-selections-clear! ed)
                 (for-each
                   (lambda (hunk)
                     (let* ((start-line (- (car hunk) 1))  ; 0-indexed
                            (count (cdr hunk)))
                       (let hloop ((i 0))
                         (when (< i count)
                           ;; Green (#2d5a2d) background for changed lines
                           (qt-extra-selection-add-line! ed (+ start-line i)
                             #x2d #x5a #x2d)
                           (hloop (+ i 1))))))
                   hunks)
                 (qt-extra-selections-apply! ed)
                 (set! *diff-hl-active* #t)
                 (echo-message! echo
                   (string-append "diff-hl: " (number->string (length hunks))
                     " changed region(s)")))))))))))


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

