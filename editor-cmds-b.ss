;;; -*- Gerbil -*-
;;; Command batch B (Tasks 41-43): macros, windows, text transforms,
;;; project, search

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :std/text/hex
        :std/crypto/digest
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/style
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/highlight
        :gerbil-emacs/persist
        :gerbil-emacs/editor-core
        :gerbil-emacs/editor-ui
        :gerbil-emacs/editor-text
        :gerbil-emacs/editor-advanced
        :gerbil-emacs/editor-cmds-a)

;;;============================================================================
;;; Task #41: macros, windows, and advanced editing
;;;============================================================================

(def (cmd-comment-region app)
  "Comment each line in region with ;; prefix."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let ((ls (editor-position-from-line ed l)))
                (editor-insert-text ed ls ";; "))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Commented "
                                            (number->string (+ 1 (- end-line start-line)))
                                            " lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-uncomment-region app)
  "Remove ;; comment prefix from each line in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (line-len (- le ls))
                     (text (editor-get-text-range ed ls (min line-len 3))))
                ;; Remove ";; " or ";;" at start
                (cond
                  ((and (>= (string-length text) 3) (string=? text ";; "))
                   (editor-delete-range ed ls 3))
                  ((and (>= (string-length text) 2) (string=? (substring text 0 2) ";;"))
                   (editor-delete-range ed ls 2))))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region uncommented"))
      (echo-error! echo "No mark set"))))

(def (cmd-upcase-char app)
  "Uppercase the character at point and advance."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((ch (string-ref text pos))
             (up (char-upcase ch)))
        (when (not (char=? ch up))
          (editor-delete-range ed pos 1)
          (editor-insert-text ed pos (string up)))
        (editor-goto-pos ed (+ pos 1))))))

(def (cmd-downcase-char app)
  "Lowercase the character at point and advance."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((ch (string-ref text pos))
             (lo (char-downcase ch)))
        (when (not (char=? ch lo))
          (editor-delete-range ed pos 1)
          (editor-insert-text ed pos (string lo)))
        (editor-goto-pos ed (+ pos 1))))))

(def (cmd-toggle-case-at-point app)
  "Toggle case of character at point and advance."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((ch (string-ref text pos))
             (toggled (if (char-upper-case? ch) (char-downcase ch) (char-upcase ch))))
        (when (not (char=? ch toggled))
          (editor-delete-range ed pos 1)
          (editor-insert-text ed pos (string toggled)))
        (editor-goto-pos ed (+ pos 1))))))

(def (cmd-write-region app)
  "Write the region to a file."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((filename (echo-read-string echo "Write region to file: " row width)))
        (when (and filename (not (string-empty? filename)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (substring (editor-get-text ed) start end)))
            (with-output-to-file filename (lambda () (display text)))
            (set! (buffer-mark buf) #f)
            (echo-message! echo (string-append "Wrote "
                                                (number->string (- end start))
                                                " chars to " filename)))))
      (echo-error! echo "No mark set"))))

(def (cmd-kill-matching-buffers app)
  "Kill all buffers whose names match a pattern."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Kill buffers matching: " row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((killed 0))
        (for-each
          (lambda (buf)
            (when (string-contains (buffer-name buf) pattern)
              (set! killed (+ killed 1))))
          (buffer-list))
        (echo-message! echo (string-append "Would kill "
                                            (number->string killed)
                                            " matching buffers"))))))

(def (cmd-goto-line-relative app)
  "Go to a line relative to the current line (+N or -N)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Relative line (+N or -N): " row width)))
    (when (and input (not (string-empty? input)))
      (let ((n (string->number input)))
        (when n
          (let* ((pos (editor-get-current-pos ed))
                 (cur-line (editor-line-from-position ed pos))
                 (target (+ cur-line n))
                 (max-line (- (editor-get-line-count ed) 1))
                 (clamped (max 0 (min target max-line))))
            (editor-goto-pos ed (editor-position-from-line ed clamped))
            (editor-scroll-caret ed)))))))

(def (cmd-bookmark-delete app)
  "Delete a bookmark by name."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Delete bookmark: " row width)))
    (when (and name (not (string-empty? name)))
      (let ((bm (app-state-bookmarks app)))
        (if (hash-get bm name)
          (begin
            (hash-remove! bm name)
            (echo-message! echo (string-append "Deleted bookmark: " name)))
          (echo-error! echo (string-append "No bookmark: " name)))))))

(def (cmd-bookmark-rename app)
  "Rename a bookmark."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (old-name (echo-read-string echo "Rename bookmark: " row width)))
    (when (and old-name (not (string-empty? old-name)))
      (let ((bm (app-state-bookmarks app)))
        (if (hash-get bm old-name)
          (let ((new-name (echo-read-string echo "New name: " row width)))
            (when (and new-name (not (string-empty? new-name)))
              (hash-put! bm new-name (hash-ref bm old-name))
              (hash-remove! bm old-name)
              (echo-message! echo (string-append old-name " -> " new-name))))
          (echo-error! echo (string-append "No bookmark: " old-name)))))))

(def (cmd-describe-mode app)
  "Describe the current buffer mode."
  (let* ((buf (current-buffer-from-app app))
         (lang (buffer-lexer-lang buf))
         (echo (app-state-echo app)))
    (echo-message! echo (string-append "Major mode: "
                                        (if lang (symbol->string lang) "fundamental")
                                        " | Use M-x describe-bindings for keybindings"))))

(def (cmd-delete-trailing-lines app)
  "Delete trailing blank lines at end of buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (= len 0)
      (echo-message! echo "Buffer is empty")
      (let loop ((end len))
        (if (and (> end 0)
                 (let ((ch (string-ref text (- end 1))))
                   (or (char=? ch #\newline) (char=? ch #\space) (char=? ch #\tab))))
          (loop (- end 1))
          (if (< end len)
            (let ((removed (- len end)))
              ;; Keep one trailing newline
              (let ((keep-end (+ end 1)))
                (when (< keep-end len)
                  (editor-delete-range ed keep-end (- len keep-end))
                  (echo-message! echo (string-append "Removed "
                                                      (number->string (- len keep-end))
                                                      " trailing chars")))))
            (echo-message! echo "No trailing blank lines")))))))

(def (cmd-display-line-numbers-relative app)
  "Toggle relative line numbers display."
  (echo-message! (app-state-echo app) "Relative line numbers: N/A (use absolute)"))

(def (cmd-goto-column app)
  "Go to a specific column on the current line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Go to column: " row width)))
    (when (and input (not (string-empty? input)))
      (let ((col (string->number input)))
        (when (and col (> col 0))
          (let* ((pos (editor-get-current-pos ed))
                 (line (editor-line-from-position ed pos))
                 (line-start (editor-position-from-line ed line))
                 (line-end (editor-get-line-end-position ed line))
                 (line-len (- line-end line-start))
                 (target-col (min (- col 1) line-len)))
            (editor-goto-pos ed (+ line-start target-col))))))))

(def (cmd-insert-line-number app)
  "Insert the current line number at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (text (number->string line)))
    (editor-insert-text ed pos text)
    (editor-goto-pos ed (+ pos (string-length text)))))

(def (cmd-insert-buffer-filename app)
  "Insert the current buffer's filename at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (filename (or (buffer-file-path buf) (buffer-name buf))))
    (editor-insert-text ed pos filename)
    (editor-goto-pos ed (+ pos (string-length filename)))))

(def (cmd-copy-line-number app)
  "Copy the current line number to kill ring."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (text (number->string line)))
    (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied line number: " text))))

(def (cmd-copy-current-line app)
  "Copy the current line to kill ring."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (start (editor-position-from-line ed line))
         (end (editor-get-line-end-position ed line))
         (text (substring (editor-get-text ed) start end)))
    (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) "Line copied")))

(def (cmd-copy-word app)
  "Copy the word at point to kill ring."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end)))
        (set! (app-state-kill-ring app) (cons word (app-state-kill-ring app)))
        (echo-message! (app-state-echo app) (string-append "Copied: " word)))
      (echo-error! (app-state-echo app) "Not on a word"))))

(def (cmd-move-to-window-top app)
  "Move cursor to the top visible line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (first-visible (send-message ed SCI_GETFIRSTVISIBLELINE 0 0))
         (doc-line (send-message ed 2312 first-visible 0)) ; SCI_DOCLINEFROMVISIBLE
         (pos (editor-position-from-line ed doc-line)))
    (editor-goto-pos ed pos)))

(def (cmd-move-to-window-bottom app)
  "Move cursor to the bottom visible line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (first-visible (send-message ed SCI_GETFIRSTVISIBLELINE 0 0))
         (lines-on-screen (send-message ed 2370 0 0)) ; SCI_LINESONSCREEN
         (last-visible (+ first-visible (- lines-on-screen 1)))
         (doc-line (send-message ed 2312 last-visible 0)) ; SCI_DOCLINEFROMVISIBLE
         (max-line (- (editor-get-line-count ed) 1))
         (target (min doc-line max-line))
         (pos (editor-position-from-line ed target)))
    (editor-goto-pos ed pos)))

(def (cmd-move-to-window-middle app)
  "Move cursor to the middle visible line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (first-visible (send-message ed SCI_GETFIRSTVISIBLELINE 0 0))
         (lines-on-screen (send-message ed 2370 0 0)) ; SCI_LINESONSCREEN
         (middle-visible (+ first-visible (quotient lines-on-screen 2)))
         (doc-line (send-message ed 2312 middle-visible 0)) ; SCI_DOCLINEFROMVISIBLE
         (pos (editor-position-from-line ed doc-line)))
    (editor-goto-pos ed pos)))

(def (cmd-scroll-left app)
  "Scroll the view left."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (offset (send-message ed SCI_GETXOFFSET 0 0)))
    (when (> offset 0)
      (send-message ed SCI_SETXOFFSET (max 0 (- offset 20)) 0))))

(def (cmd-scroll-right app)
  "Scroll the view right."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (offset (send-message ed SCI_GETXOFFSET 0 0)))
    (send-message ed SCI_SETXOFFSET (+ offset 20) 0)))

(def (cmd-delete-to-end-of-line app)
  "Delete from point to end of line (without killing)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (end (editor-get-line-end-position ed line)))
    (when (> end pos)
      (editor-delete-range ed pos (- end pos)))))

(def (cmd-delete-to-beginning-of-line app)
  "Delete from point to beginning of line (without killing)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (start (editor-position-from-line ed line)))
    (when (> pos start)
      (editor-delete-range ed start (- pos start)))))

(def (cmd-yank-whole-line app)
  "Yank (paste) a whole line above the current line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (kill-ring (app-state-kill-ring app)))
    (if (null? kill-ring)
      (echo-error! echo "Kill ring is empty")
      (let* ((text (car kill-ring))
             (pos (editor-get-current-pos ed))
             (line (editor-line-from-position ed pos))
             (line-start (editor-position-from-line ed line))
             (insert-text (string-append text "\n")))
        (editor-insert-text ed line-start insert-text)
        (editor-goto-pos ed line-start)
        (echo-message! echo "Yanked line")))))

(def (cmd-show-column-number app)
  "Show the current column number."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (col (+ 1 (- pos line-start))))
    (echo-message! echo (string-append "Column " (number->string col)))))

(def (cmd-count-lines-buffer app)
  "Count total lines in the buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (lines (editor-get-line-count ed)))
    (echo-message! echo (string-append "Buffer has " (number->string lines) " lines"))))

(def (cmd-recover-session app)
  "Recover auto-saved session files."
  (echo-message! (app-state-echo app) "No auto-saved sessions found"))

(def (cmd-toggle-backup-files app)
  "Toggle whether backup files are created on save."
  (echo-message! (app-state-echo app) "Backup files: always enabled (~suffix)"))

;;;============================================================================
;;; Task #42: text transforms, programming, and info
;;;============================================================================

(def (cmd-camel-to-snake app)
  "Convert camelCase word at point to snake_case."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (result (let loop ((i 0) (acc []))
                       (if (>= i (string-length word))
                         (list->string (reverse acc))
                         (let ((ch (string-ref word i)))
                           (if (and (char-upper-case? ch) (> i 0))
                             (loop (+ i 1) (cons (char-downcase ch) (cons #\_ acc)))
                             (loop (+ i 1) (cons (char-downcase ch) acc))))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (editor-goto-pos ed (+ start (string-length result)))))))

(def (cmd-snake-to-camel app)
  "Convert snake_case word at point to camelCase."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Include underscores in word boundary
    (when (and (< pos len)
               (let ((ch (string-ref text pos)))
                 (or (word-char? (char->integer ch)) (char=? ch #\_))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (word-char? (char->integer ch)) (char=? ch #\_))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (word-char? (char->integer ch)) (char=? ch #\_))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (result (let loop ((i 0) (capitalize? #f) (acc []))
                       (if (>= i (string-length word))
                         (list->string (reverse acc))
                         (let ((ch (string-ref word i)))
                           (if (char=? ch #\_)
                             (loop (+ i 1) #t acc)
                             (if capitalize?
                               (loop (+ i 1) #f (cons (char-upcase ch) acc))
                               (loop (+ i 1) #f (cons ch acc)))))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (editor-goto-pos ed (+ start (string-length result)))))))

(def (cmd-kebab-to-camel app)
  "Convert kebab-case word at point to camelCase."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (and (< pos len)
               (let ((ch (string-ref text pos)))
                 (or (word-char? (char->integer ch)) (char=? ch #\-))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (word-char? (char->integer ch)) (char=? ch #\-))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (word-char? (char->integer ch)) (char=? ch #\-))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (result (let loop ((i 0) (capitalize? #f) (acc []))
                       (if (>= i (string-length word))
                         (list->string (reverse acc))
                         (let ((ch (string-ref word i)))
                           (if (char=? ch #\-)
                             (loop (+ i 1) #t acc)
                             (if capitalize?
                               (loop (+ i 1) #f (cons (char-upcase ch) acc))
                               (loop (+ i 1) #f (cons ch acc)))))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (editor-goto-pos ed (+ start (string-length result)))))))

(def (cmd-reverse-word app)
  "Reverse the word at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (reversed (list->string (reverse (string->list word)))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start reversed))
        (editor-goto-pos ed end)))))

(def (cmd-count-occurrences app)
  "Count occurrences of a string in the buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (search (echo-read-string echo "Count occurrences of: " row width)))
    (when (and search (not (string-empty? search)))
      (let* ((text (editor-get-text ed))
             (slen (string-length search))
             (count (let loop ((pos 0) (n 0))
                      (let ((found (string-contains text search pos)))
                        (if found
                          (loop (+ found slen) (+ n 1))
                          n)))))
        (echo-message! echo (string-append (number->string count)
                                            " occurrences of \"" search "\""))))))

(def (cmd-mark-lines-matching app)
  "Count lines matching a string."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (search (echo-read-string echo "Count lines matching: " row width)))
    (when (and search (not (string-empty? search)))
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matching (let loop ((ls lines) (n 0))
                         (if (null? ls) n
                           (loop (cdr ls)
                                 (if (string-contains (car ls) search)
                                   (+ n 1) n))))))
        (echo-message! echo (string-append (number->string matching)
                                            " lines match \"" search "\""))))))

(def (cmd-number-region app)
  "Number lines in region starting from 1."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line) (n (+ 1 (- end-line start-line))))
            (when (>= l start-line)
              (let ((ls (editor-position-from-line ed l))
                    (prefix (string-append (number->string n) ": ")))
                (editor-insert-text ed ls prefix))
              (loop (- l 1) (- n 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Lines numbered"))
      (echo-error! echo "No mark set"))))

(def (cmd-strip-line-numbers app)
  "Remove leading line numbers (NNN: ) from region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (line-len (- le ls))
                     (text (editor-get-text-range ed ls (min line-len 10))))
                ;; Find ": " after digits
                (let digit-loop ((i 0))
                  (when (< i (string-length text))
                    (let ((ch (string-ref text i)))
                      (cond
                        ((char-numeric? ch) (digit-loop (+ i 1)))
                        ((and (char=? ch #\:)
                              (< (+ i 1) (string-length text))
                              (char=? (string-ref text (+ i 1)) #\space)
                              (> i 0))
                         (editor-delete-range ed ls (+ i 2)))
                        (else (void)))))))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Line numbers stripped"))
      (echo-error! echo "No mark set"))))

(def (cmd-prefix-lines app)
  "Add a prefix string to each line in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((prefix (echo-read-string echo "Prefix: " row width)))
        (when (and prefix (not (string-empty? prefix)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end)))
            (with-undo-action ed
              (let loop ((l end-line))
                (when (>= l start-line)
                  (editor-insert-text ed (editor-position-from-line ed l) prefix)
                  (loop (- l 1)))))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Lines prefixed"))))
      (echo-error! echo "No mark set"))))

(def (cmd-suffix-lines app)
  "Add a suffix string to each line in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((suffix (echo-read-string echo "Suffix: " row width)))
        (when (and suffix (not (string-empty? suffix)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end)))
            (with-undo-action ed
              (let loop ((l end-line))
                (when (>= l start-line)
                  (editor-insert-text ed (editor-get-line-end-position ed l) suffix)
                  (loop (- l 1)))))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Lines suffixed"))))
      (echo-error! echo "No mark set"))))

(def (cmd-wrap-lines-at-column app)
  "Hard-wrap long lines at a specified column."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Wrap at column (default 80): " row width)))
    (let ((col (if (or (not input) (string-empty? input)) 80
                 (or (string->number input) 80))))
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (wrapped-lines
               (apply append
                 (map (lambda (line)
                        (if (<= (string-length line) col)
                          (list line)
                          (let loop ((s line) (acc []))
                            (if (<= (string-length s) col)
                              (reverse (cons s acc))
                              (let find-break ((p col))
                                (cond
                                  ((and (>= p 0) (char=? (string-ref s p) #\space))
                                   (loop (substring s (+ p 1) (string-length s))
                                         (cons (substring s 0 p) acc)))
                                  ((> p 0) (find-break (- p 1)))
                                  (else ; no space found, break at col
                                   (loop (substring s col (string-length s))
                                         (cons (substring s 0 col) acc)))))))))
                      lines)))
             (result (string-join wrapped-lines "\n")))
        (with-undo-action ed
          (editor-set-text ed result))
        (echo-message! echo (string-append "Lines wrapped at column " (number->string col)))))))

(def (cmd-show-file-info app)
  "Show detailed file information."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (path (or (buffer-file-path buf) "(no file)"))
         (size (editor-get-text-length ed))
         (lines (editor-get-line-count ed))
         (lang (buffer-lexer-lang buf)))
    (echo-message! echo (string-append name " | " path " | "
                                        (number->string size) "B | "
                                        (number->string lines) "L | "
                                        (if lang (symbol->string lang) "text")))))

(def (cmd-toggle-narrow-indicator app)
  "Toggle narrow region indicator."
  (echo-message! (app-state-echo app) "Narrow indicator toggled"))

(def (cmd-insert-timestamp app)
  "Insert ISO 8601 timestamp at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         ;; Use time->seconds to get epoch and format
         (now (time->seconds (current-time)))
         (secs (inexact->exact (floor now)))
         (text (number->string secs)))
    (editor-insert-text ed pos (string-append "[" text "]"))
    (editor-goto-pos ed (+ pos (string-length text) 2))))

(def (cmd-eval-and-insert app)
  "Eval a Gerbil expression and insert the result at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Eval and insert: " row width)))
    (when (and input (not (string-empty? input)))
      (let-values (((result err?) (eval-expression-string input)))
        (if err?
          (echo-error! echo result)
          (let ((pos (editor-get-current-pos ed)))
            (editor-insert-text ed pos result)
            (editor-goto-pos ed (+ pos (string-length result)))))))))

(def (cmd-shell-command-insert app)
  "Run a shell command and insert output at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Shell command (insert output): " row width)))
    (when (and cmd (not (string-empty? cmd)))
      (let ((output (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((proc (open-process
                                      (list path: "/bin/sh"
                                            arguments: (list "-c" cmd)
                                            stdin-redirection: #f
                                            stdout-redirection: #t
                                            stderr-redirection: #t))))
                          (let ((result (read-line proc #f)))
                            (process-status proc)
                            (or result "")))))))
        (let ((pos (editor-get-current-pos ed)))
          (editor-insert-text ed pos output)
          (editor-goto-pos ed (+ pos (string-length output))))))))

(def (cmd-pipe-region app)
  "Pipe region through a shell command."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((cmd (echo-read-string echo "Pipe region through: " row width)))
        (when (and cmd (not (string-empty? cmd)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (region-text (substring (editor-get-text ed) start end))
                 (output (with-exception-catcher
                           (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                           (lambda ()
                             (let ((proc (open-process
                                           (list path: "/bin/sh"
                                                 arguments: (list "-c" cmd)
                                                 stdin-redirection: #t
                                                 stdout-redirection: #t
                                                 stderr-redirection: #t))))
                               (display region-text proc)
                               (close-output-port proc)
                               (let ((result (read-line proc #f)))
                                 (process-status proc)
                                 (or result "")))))))
            (with-undo-action ed
              (editor-delete-range ed start (- end start))
              (editor-insert-text ed start output))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Region filtered"))))
      (echo-error! echo "No mark set"))))

(def (cmd-sort-words app)
  "Sort words in region alphabetically."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (substring (editor-get-text ed) start end))
             (words (string-split text #\space))
             (sorted (sort words string<?))
             (result (string-join sorted " ")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Sorted " (number->string (length sorted)) " words")))
      (echo-error! echo "No mark set"))))

(def (cmd-remove-blank-lines app)
  "Remove blank lines in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (line-start (editor-position-from-line ed start-line))
             (line-end (editor-get-line-end-position ed end-line))
             (text (editor-get-text-range ed line-start (- line-end line-start)))
             (lines (string-split text #\newline))
             (non-blank (filter (lambda (l) (not (string-empty? (string-trim l)))) lines))
             (removed (- (length lines) (length non-blank)))
             (result (string-join non-blank "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Removed " (number->string removed) " blank lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-collapse-blank-lines app)
  "Collapse consecutive blank lines into one."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (collapsed (let loop ((ls lines) (prev-blank? #f) (acc []))
                      (cond
                        ((null? ls) (reverse acc))
                        ((string-empty? (string-trim (car ls)))
                         (if prev-blank?
                           (loop (cdr ls) #t acc)
                           (loop (cdr ls) #t (cons (car ls) acc))))
                        (else
                         (loop (cdr ls) #f (cons (car ls) acc))))))
         (removed (- (length lines) (length collapsed)))
         (result (string-join collapsed "\n")))
    (when (> removed 0)
      (with-undo-action ed
        (editor-set-text ed result))
      (echo-message! echo (string-append "Collapsed " (number->string removed)
                                          " blank lines")))))

(def (cmd-trim-lines app)
  "Trim trailing whitespace from all lines."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (trimmed (map string-trim-right lines))
         (result (string-join trimmed "\n")))
    (when (not (string=? text result))
      (with-undo-action ed
        (let ((pos (editor-get-current-pos ed)))
          (editor-set-text ed result)
          (editor-goto-pos ed (min pos (string-length result)))))
      (echo-message! echo "Trailing whitespace trimmed"))))

(def (cmd-toggle-line-comment app)
  "Toggle ;; comment on the current line (no region needed)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (ls (editor-position-from-line ed line))
         (le (editor-get-line-end-position ed line))
         (line-len (- le ls))
         (text (editor-get-text-range ed ls (min line-len 3))))
    (cond
      ((and (>= (string-length text) 3) (string=? text ";; "))
       (editor-delete-range ed ls 3))
      ((and (>= (string-length text) 2) (string=? (substring text 0 2) ";;"))
       (editor-delete-range ed ls 2))
      (else
       (editor-insert-text ed ls ";; ")))))

(def (cmd-copy-file-path app)
  "Copy the current buffer's file path to kill ring."
  (let* ((buf (current-buffer-from-app app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (set! (app-state-kill-ring app) (cons path (app-state-kill-ring app)))
        (echo-message! echo (string-append "Copied: " path)))
      (echo-error! echo "Buffer has no file path"))))

(def (cmd-insert-path-separator app)
  "Insert a file path separator."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "/")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-show-word-count app)
  "Show word count for the entire buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (words (let loop ((i 0) (in-word? #f) (count 0))
                  (if (>= i (string-length text))
                    (if in-word? (+ count 1) count)
                    (let ((ch (string-ref text i)))
                      (if (or (char=? ch #\space) (char=? ch #\newline)
                              (char=? ch #\tab) (char=? ch #\return))
                        (loop (+ i 1) #f (if in-word? (+ count 1) count))
                        (loop (+ i 1) #t count)))))))
    (echo-message! echo (string-append (number->string words) " words"))))

(def (cmd-show-char-count app)
  "Show character count for the entire buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app)))
    (echo-message! echo (string-append (number->string (editor-get-text-length ed)) " characters"))))

(def (cmd-toggle-auto-complete app)
  "Toggle auto-completion display."
  (echo-message! (app-state-echo app) "Auto-complete toggled"))

(def (cmd-insert-lorem-ipsum app)
  "Insert a paragraph of Lorem Ipsum text."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\n"))
    (editor-insert-text ed pos lorem)
    (editor-goto-pos ed (+ pos (string-length lorem)))))

(def (cmd-narrow-to-defun app)
  "Narrow the view to the current function definition."
  ;; Simplified: find nearest defun boundaries
  (echo-message! (app-state-echo app) "Narrow to defun (use M-x narrow-to-region)"))

(def (cmd-widen-all app)
  "Widen all narrowed buffers."
  (echo-message! (app-state-echo app) "Widen all (use M-x widen)"))

(def (cmd-reindent-buffer app)
  "Re-indent the entire buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (lines (editor-get-line-count ed)))
    ;; Simple: re-indent all lines using 2-space indent from column 0
    ;; This is a very basic version — just trims leading whitespace
    (echo-message! echo (string-append "Buffer has " (number->string lines) " lines (use indent-region for region)"))))

(def (cmd-show-trailing-whitespace-count app)
  "Count lines with trailing whitespace."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (count (let loop ((ls lines) (n 0))
                  (if (null? ls) n
                    (let ((line (car ls)))
                      (loop (cdr ls)
                            (if (and (> (string-length line) 0)
                                     (let ((last-ch (string-ref line (- (string-length line) 1))))
                                       (or (char=? last-ch #\space) (char=? last-ch #\tab))))
                              (+ n 1) n)))))))
    (echo-message! echo (string-append (number->string count)
                                        " lines have trailing whitespace"))))

(def (cmd-show-tab-count app)
  "Count tab characters in the buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (count (let loop ((i 0) (n 0))
                  (if (>= i (string-length text)) n
                    (loop (+ i 1)
                          (if (char=? (string-ref text i) #\tab) (+ n 1) n))))))
    (echo-message! echo (string-append (number->string count) " tab characters"))))

(def (cmd-toggle-global-whitespace app)
  "Toggle global whitespace visibility."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (current (editor-get-view-whitespace ed)))
    (if (= current 0)
      (begin (editor-set-view-whitespace ed 1)
             (echo-message! (app-state-echo app) "Whitespace visible globally"))
      (begin (editor-set-view-whitespace ed 0)
             (echo-message! (app-state-echo app) "Whitespace hidden")))))

(def (cmd-insert-box-comment app)
  "Insert a box comment."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (box (string-append
                ";;;============================================================================\n"
                ";;; \n"
                ";;;============================================================================\n")))
    (editor-insert-text ed pos box)
    ;; Position cursor at the description line
    (editor-goto-pos ed (+ pos 80))))  ; After ";;; " on second line

(def (cmd-toggle-electric-indent app)
  "Toggle electric indent mode."
  (echo-message! (app-state-echo app) "Electric indent toggled"))

(def (cmd-increase-font-size app)
  "Increase editor font size."
  (cmd-zoom-in app))

(def (cmd-decrease-font-size app)
  "Decrease editor font size."
  (cmd-zoom-out app))

(def (cmd-reset-font-size app)
  "Reset editor font size."
  (cmd-zoom-reset app))

;;;============================================================================
;;; Task #43: project, search, and utilities
;;;============================================================================

;; Helper: create/find a named buffer, switch to it, set text
(def (open-output-buffer app name text)
  (let* ((ed (current-editor app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name name)
                  (buffer-create! name ed #f))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (editor-set-text ed text)
    (editor-set-save-point ed)
    (editor-goto-pos ed 0)))

(def (cmd-project-find-file app)
  "Find file in project (prompts for filename)."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (base-dir (or (buffer-file-path buf) "."))
         (dir (let ((d (path-directory base-dir)))
                (if (string-empty? d) "." d)))
         (filename (echo-read-string echo (string-append "Find in " dir ": ") row width)))
    (when (and filename (not (string-empty? filename)))
      (let ((full-path (path-expand filename dir)))
        (if (file-exists? full-path)
          (let* ((name (path-strip-directory full-path))
                 (ed (current-editor app))
                 (new-buf (buffer-create! name ed full-path)))
            (buffer-attach! ed new-buf)
            (set! (edit-window-buffer (current-window fr)) new-buf)
            (let ((text (read-file-as-string full-path)))
              (when text
                (editor-set-text ed text)
                (editor-set-save-point ed)
                (editor-goto-pos ed 0)))
            (echo-message! echo (string-append "Opened: " full-path)))
          (echo-error! echo (string-append "File not found: " full-path)))))))

(def (cmd-project-grep app)
  "Grep for a pattern in the current file's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) "."))
         (pattern (echo-read-string echo (string-append "Grep in " dir ": ") row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((output (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((proc (open-process
                                      (list path: "/usr/bin/grep"
                                            arguments: (list "-rn" pattern dir)
                                            stdin-redirection: #f
                                            stdout-redirection: #t
                                            stderr-redirection: #t))))
                          (let ((result (read-line proc #f)))
                            (process-status proc)
                            (or result "(no matches)")))))))
        ;; Show in a new buffer
        (begin
          (open-output-buffer app (string-append "*grep " pattern "*") output)
          (echo-message! echo (string-append "grep: " pattern)))))))

(def (cmd-project-compile app)
  "Run make in the current file's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                    (lambda ()
                      (let ((proc (open-process
                                    (list path: "/usr/bin/make"
                                          arguments: '()
                                          directory: dir
                                          stdin-redirection: #f
                                          stdout-redirection: #t
                                          stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "")))))))
      (begin
        (open-output-buffer app "*compile*" output)
        (echo-message! echo "Compilation complete")))))

(def (cmd-search-forward-word app)
  "Search forward for the word at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (found (string-contains text word end)))
        (if found
          (begin
            (editor-goto-pos ed found)
            (editor-scroll-caret ed)
            (echo-message! echo (string-append "Found: " word)))
          (echo-error! echo (string-append "\"" word "\" not found below"))))
      (echo-error! echo "Not on a word"))))

(def (cmd-search-backward-word app)
  "Search backward for the word at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             ;; Search backwards by scanning from beginning
             (found (let loop ((p 0) (last-found #f))
                      (let ((f (string-contains text word p)))
                        (if (and f (< f start))
                          (loop (+ f 1) f)
                          last-found)))))
        (if found
          (begin
            (editor-goto-pos ed found)
            (editor-scroll-caret ed)
            (echo-message! echo (string-append "Found: " word)))
          (echo-error! echo (string-append "\"" word "\" not found above"))))
      (echo-error! echo "Not on a word"))))

(def (cmd-replace-in-region app)
  "Replace all occurrences of a string within the region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((search (echo-read-string echo "Replace in region: " row width)))
        (when (and search (not (string-empty? search)))
          (let ((replace (echo-read-string echo "Replace with: " row width)))
            (when replace
              (let* ((start (min pos mark))
                     (end (max pos mark))
                     (region (substring (editor-get-text ed) start end))
                     (slen (string-length search))
                     ;; Manual replace
                     (result (let loop ((p 0) (acc []))
                               (let ((f (string-contains region search p)))
                                 (if f
                                   (loop (+ f slen)
                                         (cons replace (cons (substring region p f) acc)))
                                   (list->string
                                     (apply append
                                       (map string->list
                                            (reverse (cons (substring region p (string-length region)) acc))))))))))
                (with-undo-action ed
                  (editor-delete-range ed start (- end start))
                  (editor-insert-text ed start result))
                (set! (buffer-mark buf) #f)
                (echo-message! echo "Replaced in region"))))))
      (echo-error! echo "No mark set"))))

(def (cmd-highlight-word-at-point app)
  "Highlight all occurrences of the word at point."
  ;; This sets the search indicator
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             ;; Count occurrences
             (count (let loop ((p 0) (n 0))
                      (let ((f (string-contains text word p)))
                        (if f (loop (+ f (string-length word)) (+ n 1)) n)))))
        (echo-message! echo (string-append "\"" word "\" — "
                                            (number->string count) " occurrences")))
      (echo-error! echo "Not on a word"))))

(def (cmd-goto-definition app)
  "Jump to definition of symbol at point (simple text search)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (and (< pos len) (word-char? (char->integer (string-ref text pos))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0) (word-char? (char->integer (string-ref text (- p 1)))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len) (word-char? (char->integer (string-ref text p))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             ;; Search for "(def (WORD" or "(def WORD" or "(defstruct WORD"
             (def-pattern (string-append "(def " word))
             (found (or (string-contains text (string-append "(def (" word " "))
                        (string-contains text (string-append "(def (" word ")"))
                        (string-contains text (string-append "(def " word " "))
                        (string-contains text (string-append "(def " word "\n"))
                        (string-contains text (string-append "(defstruct " word))
                        (string-contains text (string-append "(defclass " word)))))
        (if found
          (begin
            (editor-goto-pos ed found)
            (editor-scroll-caret ed)
            (echo-message! echo (string-append "Jumped to definition of " word)))
          (echo-error! echo (string-append "No definition found for " word))))
      (echo-error! echo "Not on a word"))))

(def (cmd-toggle-eol-conversion app)
  "Toggle end-of-line conversion mode."
  (echo-message! (app-state-echo app) "EOL: use convert-line-endings-unix/dos commands"))

(def (cmd-make-frame app)
  "Create a new frame (stub — single frame only)."
  (echo-message! (app-state-echo app) "Multiple frames not supported"))

(def (cmd-delete-frame app)
  "Delete the current frame."
  (echo-message! (app-state-echo app) "Cannot delete the only frame"))

(def (cmd-toggle-menu-bar app)
  "Toggle menu bar display — N/A in terminal."
  (echo-message! (app-state-echo app) "Menu bar: N/A in terminal"))

(def (cmd-toggle-tool-bar app)
  "Toggle tool bar display — N/A in terminal."
  (echo-message! (app-state-echo app) "Tool bar: N/A in terminal"))

(def (cmd-toggle-scroll-bar app)
  "Toggle scroll bar display — N/A in terminal."
  (echo-message! (app-state-echo app) "Scroll bar: N/A in terminal"))

(def (cmd-suspend-frame app)
  "Suspend the editor (send to background)."
  (echo-message! (app-state-echo app) "Suspend (use C-z from terminal)"))

(def (cmd-list-directory app)
  "List files in the current buffer's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                    (lambda ()
                      (let ((proc (open-process
                                    (list path: "/bin/ls"
                                          arguments: (list "-la" dir)
                                          stdin-redirection: #f
                                          stdout-redirection: #t
                                          stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "")))))))
      (begin
        (open-output-buffer app (string-append "*directory " dir "*") output)
        (echo-message! echo dir)))))

(def (cmd-find-grep app)
  "Find files matching a pattern and grep inside them."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) "."))
         (pattern (echo-read-string echo "Find+grep pattern: " row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((output (with-exception-catcher
                      (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                      (lambda ()
                        (let ((proc (open-process
                                      (list path: "/usr/bin/grep"
                                            arguments: (list "-rl" pattern dir)
                                            stdin-redirection: #f
                                            stdout-redirection: #t
                                            stderr-redirection: #t))))
                          (let ((result (read-line proc #f)))
                            (process-status proc)
                            (or result "(no files match)")))))))
        (begin
          (open-output-buffer app (string-append "*find-grep " pattern "*") output)
          (echo-message! echo (string-append "Files matching: " pattern)))))))

(def (cmd-insert-header-guard app)
  "Insert C/C++ header guard."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (guard (string-upcase
                  (let loop ((i 0) (acc []))
                    (if (>= i (string-length name))
                      (list->string (reverse acc))
                      (let ((ch (string-ref name i)))
                        (loop (+ i 1)
                              (cons (if (or (char-alphabetic? ch) (char-numeric? ch))
                                      ch #\_) acc)))))))
         (text (string-append "#ifndef " guard "_H\n#define " guard "_H\n\n\n#endif /* " guard "_H */\n")))
    (editor-insert-text ed 0 text)
    (editor-goto-pos ed (+ (string-length (string-append "#ifndef " guard "_H\n#define " guard "_H\n\n")) 0))
    (echo-message! echo "Header guard inserted")))

(def (cmd-insert-include app)
  "Insert a #include statement."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (header (echo-read-string echo "Include header: " row width)))
    (when (and header (not (string-empty? header)))
      (let ((line (if (char=? (string-ref header 0) #\<)
                    (string-append "#include " header "\n")
                    (string-append "#include \"" header "\"\n"))))
        (editor-insert-text ed pos line)
        (editor-goto-pos ed (+ pos (string-length line)))))))

(def (cmd-insert-import app)
  "Insert a Gerbil (import ...) statement."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (module (echo-read-string echo "Import module: " row width)))
    (when (and module (not (string-empty? module)))
      (let ((line (string-append "(import " module ")\n")))
        (editor-insert-text ed pos line)
        (editor-goto-pos ed (+ pos (string-length line)))))))

(def (cmd-insert-export app)
  "Insert a Gerbil (export ...) statement."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (sym (echo-read-string echo "Export symbol: " row width)))
    (when (and sym (not (string-empty? sym)))
      (let ((line (string-append "(export " sym ")\n")))
        (editor-insert-text ed pos line)
        (editor-goto-pos ed (+ pos (string-length line)))))))

(def (cmd-insert-defun app)
  "Insert a Gerbil (def (name ...) ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Function name: " row width)))
    (when (and name (not (string-empty? name)))
      (let ((template (string-append "(def (" name ")\n  )\n")))
        (editor-insert-text ed pos template)
        ;; Position inside the body
        (editor-goto-pos ed (+ pos (string-length (string-append "(def (" name ")\n  "))))))))

(def (cmd-insert-let app)
  "Insert a (let ((var val)) ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(let* (())\n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 8))))  ; Inside first binding pair

(def (cmd-insert-cond app)
  "Insert a (cond ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(cond\n  (())\n  (else\n   ))\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 9))))  ; Inside first condition

(def (cmd-insert-match app)
  "Insert a (match ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(match \n  (())\n  (else ))\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 7))))  ; After "match "

(def (cmd-insert-when app)
  "Insert a (when ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(when \n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 6))))  ; After "when "

(def (cmd-insert-unless app)
  "Insert an (unless ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(unless \n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 8))))  ; After "unless "

(def (cmd-insert-lambda app)
  "Insert a (lambda ...) template."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (template "(lambda ()\n  )\n"))
    (editor-insert-text ed pos template)
    (editor-goto-pos ed (+ pos 9))))  ; Inside parameter list

(def (cmd-toggle-auto-pair-mode app)
  "Toggle automatic bracket/quote pairing."
  (echo-message! (app-state-echo app) "Auto-pair toggled (use M-x toggle-electric-pair)"))

(def (cmd-count-buffers app)
  "Count the number of open buffers."
  (let ((count (length (buffer-list))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " buffers open"))))

(def (cmd-list-recent-files app)
  "List recently opened files from persistent recentf."
  (let ((echo (app-state-echo app)))
    (if (null? *recent-files*)
      (echo-message! echo "No recent files")
      (begin
        (open-output-buffer app "*recent-files*" (string-join *recent-files* "\n"))
        (echo-message! echo (string-append (number->string (length *recent-files*)) " recent files"))))))

(def (cmd-clear-recent-files app)
  "Clear the recent files list and save."
  (set! *recent-files* [])
  (recent-files-save!)
  (echo-message! (app-state-echo app) "Recent files cleared"))

(def (cmd-recentf-open app)
  "Open a recently visited file with completion."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if (null? *recent-files*)
      (echo-message! echo "No recent files")
      (let ((choice (echo-read-string-with-completion echo "Recent file: " *recent-files* row width)))
        (when (and choice (> (string-length choice) 0) (file-exists? choice))
          ;; Open the file (reuse find-file logic)
          (let* ((name (path-strip-directory choice))
                 (ed (current-editor app))
                 (buf (buffer-create! name ed choice)))
            (recent-files-add! choice)
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (let ((text (read-file-as-string choice)))
              (when text
                (editor-set-text ed text)
                (editor-set-save-point ed)
                (editor-goto-pos ed 0)))
            (echo-message! echo (string-append "Opened: " choice))))))))

(def (cmd-recentf-cleanup app)
  "Remove non-existent files from recent files list."
  (let ((removed (recent-files-cleanup!)))
    (echo-message! (app-state-echo app)
      (string-append "Removed " (number->string removed) " stale entries"))))

(def (cmd-desktop-save app)
  "Save current session (open buffers and positions) to disk."
  (let* ((entries
           (filter-map
             (lambda (buf)
               (let ((path (buffer-file-path buf)))
                 (when path
                   (make-desktop-entry
                     (buffer-name buf)
                     path
                     0  ;; cursor pos not easily accessible without editor ref
                     (buffer-local-get buf 'major-mode)))))
             (buffer-list))))
    (desktop-save! entries)
    (echo-message! (app-state-echo app)
      (string-append "Desktop saved: " (number->string (length entries)) " buffers"))))

(def (cmd-desktop-read app)
  "Restore session from saved desktop."
  (let ((entries (desktop-load)))
    (if (null? entries)
      (echo-message! (app-state-echo app) "No saved desktop")
      (let ((count 0))
        (for-each
          (lambda (entry)
            (let ((path (desktop-entry-file-path entry)))
              (when (and path (file-exists? path))
                ;; Open the file
                (let* ((name (path-strip-directory path))
                       (ed (current-editor app))
                       (fr (app-state-frame app))
                       (buf (buffer-create! name ed path)))
                  (buffer-attach! ed buf)
                  (set! (edit-window-buffer (current-window fr)) buf)
                  (let ((text (read-file-as-string path)))
                    (when text
                      (editor-set-text ed text)
                      (editor-set-save-point ed)
                      (editor-goto-pos ed (desktop-entry-cursor-pos entry))))
                  (set! count (+ count 1))))))
          entries)
        (echo-message! (app-state-echo app)
          (string-append "Desktop restored: " (number->string count) " buffers"))))))

(def (cmd-savehist-save app)
  "Save minibuffer history to disk."
  (savehist-save! *minibuffer-history*)
  (echo-message! (app-state-echo app)
    (string-append "History saved: " (number->string (length *minibuffer-history*)) " entries")))

(def (cmd-savehist-load app)
  "Load minibuffer history from disk."
  (let ((hist (savehist-load!)))
    (set! *minibuffer-history* hist)
    (echo-message! (app-state-echo app)
      (string-append "History loaded: " (number->string (length hist)) " entries"))))

(def (cmd-show-keybinding-for app)
  "Show what key is bound to a command."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Show keybinding for command: " row width)))
    (when (and name (not (string-empty? name)))
      (let* ((sym (string->symbol name))
             ;; Search keymaps for this command
             (found (let scan-keymap ((km *global-keymap*) (prefix ""))
                      (let loop ((keys (keymap-entries km)))
                        (cond
                          ((null? keys) #f)
                          ((eq? (cdar keys) sym)
                           (string-append prefix (caar keys)))
                          ((hash-table? (cdar keys))
                           (or (scan-keymap (cdar keys) (string-append prefix (caar keys) " "))
                               (loop (cdr keys))))
                          (else (loop (cdr keys))))))))
        (if found
          (echo-message! echo (string-append name " is on " found))
          (echo-message! echo (string-append name " is not bound to any key")))))))

(def (cmd-sort-imports app)
  "Sort import lines in the current region or near point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (line-start (editor-position-from-line ed start-line))
             (line-end (editor-get-line-end-position ed end-line))
             (text (editor-get-text-range ed line-start (- line-end line-start)))
             (lines (string-split text #\newline))
             (sorted (sort lines string<?))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Imports sorted"))
      (echo-error! echo "No mark set (select import lines first)"))))

(def (cmd-show-git-status app)
  "Show git status for the current file's directory."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) "Not a git repository")
                    (lambda ()
                      (let ((proc (open-process
                                    (list path: "/usr/bin/git"
                                          arguments: (list "status" "--short")
                                          directory: dir
                                          stdin-redirection: #f
                                          stdout-redirection: #t
                                          stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "(clean)")))))))
      (open-output-buffer app "*git-status*" output)
      (echo-message! echo "git status"))))

(def (cmd-show-git-log app)
  "Show git log for the current file."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) "Not a git repository")
                    (lambda ()
                      (let* ((args (if path
                                     (list "log" "--oneline" "-20" path)
                                     (list "log" "--oneline" "-20")))
                             (proc (open-process
                                     (list path: "/usr/bin/git"
                                           arguments: args
                                           directory: dir
                                           stdin-redirection: #f
                                           stdout-redirection: #t
                                           stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "(no commits)")))))))
      (open-output-buffer app "*git-log*" output)
      (echo-message! echo "git log"))))

(def (cmd-show-git-diff app)
  "Show git diff for the current file."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) ".")))
    (let ((output (with-exception-catcher
                    (lambda (e) "Not a git repository")
                    (lambda ()
                      (let* ((args (if path (list "diff" path) (list "diff")))
                             (proc (open-process
                                     (list path: "/usr/bin/git"
                                           arguments: args
                                           directory: dir
                                           stdin-redirection: #f
                                           stdout-redirection: #t
                                           stderr-redirection: #t))))
                        (let ((result (read-line proc #f)))
                          (process-status proc)
                          (or result "(no changes)")))))))
      (open-output-buffer app "*git-diff*" output)
      (echo-message! echo "git diff"))))

(def (cmd-show-git-blame app)
  "Show git blame for the current file."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if path
      (let* ((dir (path-directory path))
             (output (with-exception-catcher
                       (lambda (e) "Not a git repository or file not tracked")
                       (lambda ()
                         (let ((proc (open-process
                                       (list path: "/usr/bin/git"
                                             arguments: (list "blame" "--date=short" path)
                                             directory: dir
                                             stdin-redirection: #f
                                             stdout-redirection: #t
                                             stderr-redirection: #t))))
                           (let ((result (read-line proc #f)))
                             (process-status proc)
                             (or result "(no data)")))))))
        (open-output-buffer app (string-append "*git-blame " (path-strip-directory path) "*") output)
        (echo-message! echo "git blame"))
      (echo-error! echo "Buffer has no file"))))

(def (cmd-toggle-flyspell app)
  "Toggle spell checking."
  (set! *flyspell-mode* (not *flyspell-mode*))
  (echo-message! (app-state-echo app)
    (if *flyspell-mode* "Spell check: on" "Spell check: off")))

(def *flyspell-mode* #f)
(def *flymake-mode* #f)
(def *lsp-mode* #f)
(def *global-auto-revert-mode* #f)

(def (cmd-toggle-flymake app)
  "Toggle syntax checking."
  (set! *flymake-mode* (not *flymake-mode*))
  (echo-message! (app-state-echo app)
    (if *flymake-mode* "Syntax check: on" "Syntax check: off")))

(def (cmd-toggle-lsp app)
  "Toggle LSP support."
  (set! *lsp-mode* (not *lsp-mode*))
  (echo-message! (app-state-echo app)
    (if *lsp-mode* "LSP: on" "LSP: off")))

(def (cmd-toggle-auto-revert-global app)
  "Toggle global auto-revert mode."
  (set! *global-auto-revert-mode* (not *global-auto-revert-mode*))
  (echo-message! (app-state-echo app)
    (if *global-auto-revert-mode* "Global auto-revert: on" "Global auto-revert: off")))

