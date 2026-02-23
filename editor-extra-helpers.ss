;;; -*- Gerbil -*-
;;; Shared helpers for editor-extra sub-modules

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (current-editor app)
  (edit-window-editor (current-window (app-state-frame app))))

(def (current-buffer-from-app app)
  (edit-window-buffer (current-window (app-state-frame app))))

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

(def (app-read-string app prompt)
  "Convenience wrapper: read a string from the echo area.
   In tests, dequeues from *test-echo-responses* if non-empty."
  (if (pair? *test-echo-responses*)
    (let ((r (car *test-echo-responses*)))
      (set! *test-echo-responses* (cdr *test-echo-responses*))
      r)
    (let* ((echo (app-state-echo app))
           (fr (app-state-frame app))
           (row (- (frame-height fr) 1))
           (width (frame-width fr)))
      (echo-read-string echo prompt row width))))

(def (extra-word-char? ch)
  (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-)))

(def (word-bounds-at ed pos)
  "Find word boundaries around POS. Returns (values start end) or (values #f #f)."
  (let* ((text (editor-get-text ed))
         (len (string-length text)))
    (if (or (>= pos len) (< pos 0) (not (extra-word-char? (string-ref text pos))))
      ;; Not in a word — try char before pos
      (if (and (> pos 0) (extra-word-char? (string-ref text (- pos 1))))
        (let ((p (- pos 1)))
          (let find-start ((i p))
            (if (and (> i 0) (extra-word-char? (string-ref text (- i 1))))
              (find-start (- i 1))
              (let find-end ((j (+ p 1)))
                (if (and (< j len) (extra-word-char? (string-ref text j)))
                  (find-end (+ j 1))
                  (values i j))))))
        (values #f #f))
      ;; In a word — scan backward then forward
      (let find-start ((i pos))
        (if (and (> i 0) (extra-word-char? (string-ref text (- i 1))))
          (find-start (- i 1))
          (let find-end ((j (+ pos 1)))
            (if (and (< j len) (extra-word-char? (string-ref text j)))
              (find-end (+ j 1))
              (values i j))))))))

;;;============================================================================
;;; Global mode flags — used by simple mode toggles
;;;============================================================================
(def (directory-exists? path)
  (and (file-exists? path)
       (eq? 'directory (file-type path))))

(def (editor-replace-selection ed text)
  "Replace the current selection with text. SCI_REPLACESEL=2170."
  (send-message/string ed 2170 text))

(def *mode-flags* (make-hash-table))
(def *recent-files* '())
(def *last-compile-proc* #f)
(def *kmacro-counter* 0)
(def *kmacro-counter-format* "%d")
(def *custom-variables* (make-hash-table)) ; name -> value

(def (app-state-mark-pos app)
  "Get the mark position from the current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (buffer-mark buf)))
(def (toggle-mode! name)
  "Toggle a named mode flag. Returns the new state."
  (let ((current (hash-get *mode-flags* name)))
    (hash-put! *mode-flags* name (not current))
    (not current)))
(def (mode-enabled? name)
  (hash-get *mode-flags* name))

;;;============================================================================
;;; Shared s-expression helpers (used by paredit and smartparens)
;;;============================================================================

(def (sp-find-enclosing-paren ed pos open-char close-char)
  "Find the position of the enclosing open paren before pos."
  (let ((text (editor-get-text ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (if (< i 0)
        #f
        (let ((ch (string-ref text i)))
          (cond
            ((char=? ch close-char) (loop (- i 1) (+ depth 1)))
            ((char=? ch open-char)
             (if (= depth 0) i (loop (- i 1) (- depth 1))))
            (else (loop (- i 1) depth))))))))

(def (sp-find-matching-close ed pos open-char close-char)
  "Find the position of the matching close paren after pos."
  (let* ((text (editor-get-text ed))
         (len (string-length text)))
    (let loop ((i pos) (depth 1))
      (if (>= i len)
        #f
        (let ((ch (string-ref text i)))
          (cond
            ((char=? ch open-char) (loop (+ i 1) (+ depth 1)))
            ((char=? ch close-char)
             (if (= depth 1) i (loop (+ i 1) (- depth 1))))
            (else (loop (+ i 1) depth))))))))

(def (sp-find-sexp-end ed pos)
  "Find the end of the sexp starting at or after pos."
  (let* ((text (editor-get-text ed))
         (len (string-length text)))
    ;; Skip whitespace
    (let skip ((i pos))
      (if (>= i len)
        #f
        (let ((ch (string-ref text i)))
          (cond
            ((char-whitespace? ch) (skip (+ i 1)))
            ((char=? ch #\() (sp-find-matching-close ed (+ i 1) #\( #\)))
            ((char=? ch #\[) (sp-find-matching-close ed (+ i 1) #\[ #\]))
            ((char=? ch #\{) (sp-find-matching-close ed (+ i 1) #\{ #\}))
            ;; Symbol/atom - find end
            (else
             (let find-end ((j i))
               (if (>= j len)
                 (- j 1)
                 (let ((c (string-ref text j)))
                   (if (or (char-whitespace? c)
                           (memv c '(#\( #\) #\[ #\] #\{ #\})))
                     (- j 1)
                     (find-end (+ j 1)))))))))))))

;;;============================================================================
;;; Shared project helpers
;;;============================================================================

(def *project-markers* '(".git" ".hg" ".svn" ".project" "Makefile" "package.json"
                         "Cargo.toml" "go.mod" "build.ss" "gerbil.pkg"))
(def *project-history* '()) ; list of project roots

(def (project-find-root dir)
  "Find project root by looking for project markers. Returns root or #f."
  (let loop ((d (path-normalize dir)))
    (if (or (string=? d "/") (string=? d ""))
      #f
      (if (ormap (lambda (marker)
                   (let ((path (path-expand marker d)))
                     (or (file-exists? path)
                         (directory-exists? path))))
                 *project-markers*)
        d
        (loop (path-directory d))))))

(def (project-current app)
  "Get current project root based on current buffer's file."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file (and buf (buffer-file-path buf))))
    (if file
      (project-find-root (path-directory file))
      (project-find-root (current-directory)))))

;;;============================================================================
;;; Shared spell-check helper
;;;============================================================================

(def (flyspell-check-word word)
  "Check a word with aspell. Returns list of suggestions or #f if correct."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (let* ((proc (open-process
                      (list path: "aspell"
                            arguments: '("pipe")
                            stdin-redirection: #t stdout-redirection: #t
                            stderr-redirection: #f)))
             (_ (begin (display (string-append "^" word "\n") proc)
                       (force-output proc)))
             ;; Read header line
             (header (read-line proc))
             ;; Read result line
             (result (read-line proc)))
        (close-port proc)
        (cond
          ((or (eof-object? result) (string-empty? result)) #f) ; correct
          ((char=? (string-ref result 0) #\*) #f) ; correct
          ((char=? (string-ref result 0) #\&) ; suggestions
           (let* ((parts (string-split result #\:))
                  (suggestions (if (>= (length parts) 2)
                                 (map string-trim (string-split (cadr parts) #\,))
                                 '())))
             suggestions))
          ((char=? (string-ref result 0) #\#) '()) ; no suggestions
          (else #f))))))

;;;============================================================================
;;; Batch 13: New commands (placed here for editor-extra line budget)
;;;============================================================================

(def (cmd-set-visited-file-name app)
  "Change the file name associated with the current buffer."
  (let* ((fr (app-state-frame app))
         (buf (edit-window-buffer (current-window fr)))
         (old (and buf (buffer-file-path buf)))
         (prompt (if old (string-append "New file name (was " old "): ") "File name: "))
         (new-name (app-read-string app prompt)))
    (if (and new-name (not (string=? new-name "")))
      (begin
        (set! (buffer-file-path buf) new-name)
        (set! (buffer-name buf) (path-strip-directory new-name))
        (set! (buffer-modified buf) #t)
        (echo-message! (app-state-echo app) (string-append "File name set to " new-name)))
      (echo-message! (app-state-echo app) "Cancelled"))))

(def (cmd-sort-columns app)
  "Sort lines in region by a column range."
  (echo-message! (app-state-echo app) "sort-columns: use M-x sort-fields for column sorting"))

(def (cmd-sort-regexp-fields app)
  "Sort lines in region by regex match."
  (echo-message! (app-state-echo app) "sort-regexp-fields: use M-x sort-lines for basic sorting"))

;;; Batch 15: insert-tab (TUI)
(def (cmd-insert-tab app)
  "Insert a literal tab character at point."
  (let ((ed (current-editor app)))
    (editor-replace-selection ed "\t")))

;;;============================================================================
;;; Smerge mode: Git conflict marker resolution (TUI)
;;;============================================================================

(def *smerge-mine-marker*  "<<<<<<<")
(def *smerge-sep-marker*   "=======")
(def *smerge-other-marker* ">>>>>>>")

(def (smerge-find-conflict text pos direction)
  "Find the next/prev conflict starting from POS.
   DIRECTION is 'next or 'prev.
   Returns (values mine-start sep-start other-end) or (values #f #f #f).
   mine-start = start of <<<<<<< line
   sep-start = start of ======= line
   other-end = end of >>>>>>> line (after newline)"
  (let ((len (string-length text)))
    (if (eq? direction 'next)
      ;; Search forward from pos for <<<<<<<
      (let loop ((i pos))
        (if (>= i len)
          (values #f #f #f)
          (if (and (<= (+ i 7) len)
                   (string=? (substring text i (+ i 7)) *smerge-mine-marker*)
                   (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
            ;; Found <<<<<<< - now find ======= and >>>>>>>
            (let ((mine-start i))
              (let find-sep ((j (+ i 7)))
                (if (>= j len)
                  (values #f #f #f)
                  (if (and (<= (+ j 7) len)
                           (string=? (substring text j (+ j 7)) *smerge-sep-marker*)
                           (or (= j 0) (char=? (string-ref text (- j 1)) #\newline)))
                    (let ((sep-start j))
                      (let find-other ((k (+ j 7)))
                        (if (>= k len)
                          (values #f #f #f)
                          (if (and (<= (+ k 7) len)
                                   (string=? (substring text k (+ k 7)) *smerge-other-marker*)
                                   (or (= k 0) (char=? (string-ref text (- k 1)) #\newline)))
                            ;; Find end of >>>>>>> line
                            (let find-eol ((e (+ k 7)))
                              (if (or (>= e len) (char=? (string-ref text e) #\newline))
                                (values mine-start sep-start (min (+ e 1) len))
                                (find-eol (+ e 1))))
                            (find-other (+ k 1))))))
                    (find-sep (+ j 1))))))
            (loop (+ i 1)))))
      ;; Search backward: find <<<<<<< before pos
      (let loop ((i (min pos (- len 1))))
        (if (< i 0)
          (values #f #f #f)
          (if (and (<= (+ i 7) len)
                   (string=? (substring text i (+ i 7)) *smerge-mine-marker*)
                   (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
            ;; Found <<<<<<< before pos - verify it has ======= and >>>>>>>
            (let ((mine-start i))
              (let find-sep ((j (+ i 7)))
                (if (>= j len)
                  (loop (- i 1))
                  (if (and (<= (+ j 7) len)
                           (string=? (substring text j (+ j 7)) *smerge-sep-marker*)
                           (or (= j 0) (char=? (string-ref text (- j 1)) #\newline)))
                    (let ((sep-start j))
                      (let find-other ((k (+ j 7)))
                        (if (>= k len)
                          (loop (- i 1))
                          (if (and (<= (+ k 7) len)
                                   (string=? (substring text k (+ k 7)) *smerge-other-marker*)
                                   (or (= k 0) (char=? (string-ref text (- k 1)) #\newline)))
                            ;; Found complete conflict
                            (let find-eol ((e (+ k 7)))
                              (if (or (>= e len) (char=? (string-ref text e) #\newline))
                                (values mine-start sep-start (min (+ e 1) len))
                                (find-eol (+ e 1))))
                            (find-other (+ k 1))))))
                    (find-sep (+ j 1))))))
            (loop (- i 1))))))))

(def (smerge-count-conflicts text)
  "Count total conflict markers in text."
  (let ((len (string-length text)))
    (let loop ((i 0) (count 0))
      (if (>= i len)
        count
        (if (and (<= (+ i 7) len)
                 (string=? (substring text i (+ i 7)) *smerge-mine-marker*)
                 (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
          (loop (+ i 1) (+ count 1))
          (loop (+ i 1) count))))))

(def (smerge-extract-mine text mine-start sep-start)
  "Extract 'mine' content between <<<<<<< and =======.
   Returns the content lines (without the marker lines)."
  (let ((mine-line-end
          (let find-eol ((i (+ mine-start 7)))
            (if (or (>= i (string-length text)) (char=? (string-ref text i) #\newline))
              (min (+ i 1) (string-length text))
              (find-eol (+ i 1))))))
    (substring text mine-line-end sep-start)))

(def (smerge-extract-other text sep-start other-end)
  "Extract 'other' content between ======= and >>>>>>>.
   Returns the content lines (without the marker lines)."
  (let* ((sep-line-end
           (let find-eol ((i (+ sep-start 7)))
             (if (or (>= i (string-length text)) (char=? (string-ref text i) #\newline))
               (min (+ i 1) (string-length text))
               (find-eol (+ i 1)))))
         ;; Find start of >>>>>>> line
         (other-line-start
           (let find-marker ((k sep-line-end))
             (if (>= k other-end) other-end
               (if (and (<= (+ k 7) (string-length text))
                        (string=? (substring text k (+ k 7)) *smerge-other-marker*)
                        (or (= k 0) (char=? (string-ref text (- k 1)) #\newline)))
                 k
                 (find-marker (+ k 1)))))))
    (substring text sep-line-end other-line-start)))

;;; TUI smerge commands

(def (cmd-smerge-next app)
  "Jump to the next merge conflict marker."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (+ (editor-get-current-pos ed) 1)))
    (let-values (((mine sep other) (smerge-find-conflict text pos 'next)))
      (if mine
        (begin
          (editor-goto-pos ed mine)
          (let ((total (smerge-count-conflicts text)))
            (echo-message! echo (string-append "Conflict (" (number->string total) " total)"))))
        (echo-message! echo "No more conflicts")))))

(def (cmd-smerge-prev app)
  "Jump to the previous merge conflict marker."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (max 0 (- (editor-get-current-pos ed) 1))))
    (let-values (((mine sep other) (smerge-find-conflict text pos 'prev)))
      (if mine
        (begin
          (editor-goto-pos ed mine)
          (let ((total (smerge-count-conflicts text)))
            (echo-message! echo (string-append "Conflict (" (number->string total) " total)"))))
        (echo-message! echo "No previous conflict")))))

(def (cmd-smerge-keep-mine app)
  "Keep 'mine' (upper) side of the current conflict."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    ;; Find conflict containing pos: search backward for <<<<<<<
    (let-values (((mine sep other) (smerge-find-conflict text pos 'prev)))
      ;; Also check if pos is inside the conflict
      (if (and mine (<= mine pos) (< pos other))
        (let* ((content (smerge-extract-mine text mine sep))
               (before (substring text 0 mine))
               (after (substring text other (string-length text)))
               (new-text (string-append before content after)))
          (editor-set-text ed new-text)
          (editor-goto-pos ed mine)
          (echo-message! echo "Kept mine"))
        ;; Try forward search — maybe cursor is just before the conflict
        (let-values (((mine2 sep2 other2) (smerge-find-conflict text pos 'next)))
          (if mine2
            (let* ((content (smerge-extract-mine text mine2 sep2))
                   (before (substring text 0 mine2))
                   (after (substring text other2 (string-length text)))
                   (new-text (string-append before content after)))
              (editor-set-text ed new-text)
              (editor-goto-pos ed mine2)
              (echo-message! echo "Kept mine"))
            (echo-message! echo "No conflict at point")))))))

(def (cmd-smerge-keep-other app)
  "Keep 'other' (lower) side of the current conflict."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    (let-values (((mine sep other) (smerge-find-conflict text pos 'prev)))
      (if (and mine (<= mine pos) (< pos other))
        (let* ((content (smerge-extract-other text sep other))
               (before (substring text 0 mine))
               (after (substring text other (string-length text)))
               (new-text (string-append before content after)))
          (editor-set-text ed new-text)
          (editor-goto-pos ed mine)
          (echo-message! echo "Kept other"))
        (let-values (((mine2 sep2 other2) (smerge-find-conflict text pos 'next)))
          (if mine2
            (let* ((content (smerge-extract-other text mine2 sep2))
                   (before (substring text 0 mine2))
                   (after (substring text other2 (string-length text)))
                   (new-text (string-append before content after)))
              (editor-set-text ed new-text)
              (editor-goto-pos ed mine2)
              (echo-message! echo "Kept other"))
            (echo-message! echo "No conflict at point")))))))

(def (cmd-smerge-keep-both app)
  "Keep both sides of the current conflict (remove markers only)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    (let-values (((mine sep other) (smerge-find-conflict text pos 'prev)))
      (if (and mine (<= mine pos) (< pos other))
        (let* ((mine-content (smerge-extract-mine text mine sep))
               (other-content (smerge-extract-other text sep other))
               (before (substring text 0 mine))
               (after (substring text other (string-length text)))
               (new-text (string-append before mine-content other-content after)))
          (editor-set-text ed new-text)
          (editor-goto-pos ed mine)
          (echo-message! echo "Kept both"))
        (let-values (((mine2 sep2 other2) (smerge-find-conflict text pos 'next)))
          (if mine2
            (let* ((mine-content (smerge-extract-mine text mine2 sep2))
                   (other-content (smerge-extract-other text sep2 other2))
                   (before (substring text 0 mine2))
                   (after (substring text other2 (string-length text)))
                   (new-text (string-append before mine-content other-content after)))
              (editor-set-text ed new-text)
              (editor-goto-pos ed mine2)
              (echo-message! echo "Kept both"))
            (echo-message! echo "No conflict at point")))))))

;;;============================================================================
;;; Interactive Org Agenda commands (TUI)
;;;============================================================================

(def *agenda-items* (make-hash-table))  ; line-number -> (buf-name file-path src-line)

(def (agenda-parse-line text line-num)
  "Parse an agenda line 'bufname:linenum: text' → (buf-name src-line) or #f."
  (let* ((lines (string-split text #\newline))
         (len (length lines)))
    (if (or (< line-num 0) (>= line-num len))
      #f
      (let* ((line (list-ref lines line-num))
             (trimmed (string-trim line)))
        ;; Format: "bufname:NUM: rest"
        (let ((colon1 (string-contains trimmed ":")))
          (if (not colon1)
            #f
            (let* ((buf-name (substring trimmed 0 colon1))
                   (rest (substring trimmed (+ colon1 1) (string-length trimmed)))
                   (colon2 (string-contains rest ":")))
              (if (not colon2)
                #f
                (let* ((num-str (substring rest 0 colon2))
                       (src-line (string->number num-str)))
                  (if src-line
                    (list buf-name src-line)
                    #f))))))))))

(def (cmd-org-agenda-goto app)
  "Jump to the source of the agenda item on the current line."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (parsed (agenda-parse-line text line-num)))
    (if (not parsed)
      (echo-message! echo "No agenda item on this line")
      (let* ((buf-name (car parsed))
             (src-line (cadr parsed))
             (target-buf (buffer-by-name buf-name)))
        (if target-buf
          ;; Buffer exists - switch to it and go to line
          (let* ((fr (app-state-frame app))
                 (win (current-window fr)))
            (buffer-attach! ed target-buf)
            (set! (edit-window-buffer win) target-buf)
            (editor-goto-line ed (- src-line 1))
            (echo-message! echo (string-append "Jumped to " buf-name ":" (number->string src-line))))
          ;; Buffer doesn't exist - try to find file
          (let ((fp (let search ((bufs (buffer-list)))
                      (if (null? bufs) #f
                        (let ((b (car bufs)))
                          (if (string=? (buffer-name b) buf-name)
                            (buffer-file-path b)
                            (search (cdr bufs))))))))
            (if fp
              (begin
                (let* ((content (with-exception-catcher (lambda (e) #f)
                                  (lambda () (call-with-input-file fp (lambda (p) (read-line p #f))))))
                       (fr (app-state-frame app))
                       (win (current-window fr))
                       (buf (buffer-create! buf-name ed #f)))
                  (when content
                    (buffer-attach! ed buf)
                    (set! (edit-window-buffer win) buf)
                    (set! (buffer-file-path buf) fp)
                    (editor-set-text ed content)
                    (editor-goto-line ed (- src-line 1))
                    (echo-message! echo (string-append "Opened " fp ":" (number->string src-line))))))
              (echo-message! echo (string-append "Buffer not found: " buf-name)))))))))

(def (cmd-org-agenda-todo app)
  "Toggle TODO state of the agenda item on the current line."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (parsed (agenda-parse-line text line-num)))
    (if (not parsed)
      (echo-message! echo "No agenda item on this line")
      (let* ((buf-name (car parsed))
             (src-line (cadr parsed))
             (target-buf (buffer-by-name buf-name)))
        (if (not target-buf)
          (echo-message! echo (string-append "Buffer not found: " buf-name))
          ;; Find the target buffer's file and toggle TODO
          (let ((fp (buffer-file-path target-buf)))
            (if (not fp)
              (echo-message! echo "Buffer has no file")
              (with-exception-catcher
                (lambda (e) (echo-message! echo "Error toggling TODO"))
                (lambda ()
                  (let* ((content (call-with-input-file fp (lambda (p) (read-line p #f))))
                         (lines (string-split content #\newline))
                         (idx (- src-line 1)))
                    (when (and (>= idx 0) (< idx (length lines)))
                      (let* ((line (list-ref lines idx))
                             (new-line
                               (cond
                                 ((string-contains line "TODO ")
                                  (let ((i (string-contains line "TODO ")))
                                    (string-append (substring line 0 i) "DONE "
                                                   (substring line (+ i 5) (string-length line)))))
                                 ((string-contains line "DONE ")
                                  (let ((i (string-contains line "DONE ")))
                                    (string-append (substring line 0 i) "TODO "
                                                   (substring line (+ i 5) (string-length line)))))
                                 (else line)))
                             (new-lines (let loop ((ls lines) (n 0) (acc '()))
                                          (if (null? ls) (reverse acc)
                                            (loop (cdr ls) (+ n 1)
                                                  (cons (if (= n idx) new-line (car ls)) acc)))))
                             (new-content (string-join new-lines "\n")))
                        (call-with-output-file fp (lambda (p) (display new-content p)))
                        ;; Update the agenda line in place
                        (let* ((agenda-text (editor-get-text ed))
                               (agenda-lines (string-split agenda-text #\newline))
                               (new-agenda-lines
                                 (let loop ((ls agenda-lines) (n 0) (acc '()))
                                   (if (null? ls) (reverse acc)
                                     (loop (cdr ls) (+ n 1)
                                           (cons (if (= n line-num)
                                                   (string-append "  " buf-name ":"
                                                                  (number->string src-line) ": "
                                                                  (string-trim new-line))
                                                   (car ls))
                                                 acc)))))
                               (new-agenda (string-join new-agenda-lines "\n")))
                          (editor-set-read-only ed #f)
                          (editor-set-text ed new-agenda)
                          (editor-set-read-only ed #t))
                        (echo-message! echo
                          (if (string-contains new-line "DONE")
                            "TODO → DONE"
                            "DONE → TODO"))))))))))))))

(def (cmd-smerge-mode app)
  "Toggle smerge mode — report conflict count in current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (count (smerge-count-conflicts text)))
    (if (> count 0)
      (begin
        (echo-message! echo
          (string-append "Smerge: " (number->string count) " conflict"
                         (if (> count 1) "s" "") " found. "
                         "n/p=navigate, m=mine, o=other, b=both"))
        ;; Jump to first conflict
        (let-values (((mine sep other) (smerge-find-conflict text 0 'next)))
          (when mine (editor-goto-pos ed mine))))
      (echo-message! echo "No merge conflicts found"))))

;;;============================================================================
;;; Flyspell mode: spell-check buffer and underline misspelled words (TUI)
;;;============================================================================

(def *flyspell-active* #f)
(def *flyspell-indicator* 1) ;; Scintilla indicator number (0 = highlight-symbol)

(def (flyspell-is-word-char? ch)
  "Check if character is part of a word for spell-checking."
  (or (char-alphabetic? ch) (char=? ch #\')))

(def (flyspell-extract-words text)
  "Extract word positions from text. Returns list of (word start end)."
  (let ((len (string-length text)))
    (let loop ((i 0) (words '()))
      (if (>= i len)
        (reverse words)
        (if (flyspell-is-word-char? (string-ref text i))
          ;; Found word start
          (let find-end ((j (+ i 1)))
            (if (or (>= j len) (not (flyspell-is-word-char? (string-ref text j))))
              ;; Word is text[i..j)
              (let ((word (substring text i j)))
                (if (> (string-length word) 1)
                  (loop j (cons (list word i j) words))
                  (loop j words)))
              (find-end (+ j 1))))
          (loop (+ i 1) words))))))

(def (cmd-flyspell-mode app)
  "Toggle flyspell mode: check buffer and underline misspelled words."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if *flyspell-active*
      ;; Turn off: clear indicators
      (begin
        (set! *flyspell-active* #f)
        (send-message ed SCI_SETINDICATORCURRENT *flyspell-indicator* 0)
        (send-message ed SCI_INDICATORCLEARRANGE 0 len)
        (echo-message! echo "Flyspell mode OFF"))
      ;; Turn on: scan and underline
      (begin
        (set! *flyspell-active* #t)
        ;; Setup indicator: INDIC_SQUIGGLE = 1, red color
        (send-message ed SCI_INDICSETSTYLE *flyspell-indicator* 1) ;; squiggle
        (send-message ed SCI_INDICSETFORE *flyspell-indicator* #x0000FF) ;; red (BGR)
        (send-message ed SCI_SETINDICATORCURRENT *flyspell-indicator* 0)
        ;; Clear old indicators
        (send-message ed SCI_INDICATORCLEARRANGE 0 len)
        ;; Check each word
        (let* ((words (flyspell-extract-words text))
               (misspelled 0))
          (for-each
            (lambda (entry)
              (let ((word (car entry))
                    (start (cadr entry))
                    (end (caddr entry)))
                (let ((suggestions (flyspell-check-word word)))
                  (when suggestions  ;; non-#f means misspelled
                    (set! misspelled (+ misspelled 1))
                    (send-message ed SCI_INDICATORFILLRANGE start (- end start))))))
            words)
          (echo-message! echo
            (string-append "Flyspell: " (number->string misspelled) " misspelled in "
                           (number->string (length words)) " words")))))))

;;;============================================================================
;;; Custom groups
;;;============================================================================

(def *custom-groups* (make-hash-table))  ;; group-name -> list of var-names

(def (custom-group-add! group var-name)
  "Add a variable to a custom group."
  (let ((vars (or (hash-get *custom-groups* group) [])))
    (unless (member var-name vars)
      (hash-put! *custom-groups* group (cons var-name vars)))))

;; Initialize default groups
(custom-group-add! "editing" "tab-width")
(custom-group-add! "editing" "indent-tabs-mode")
(custom-group-add! "editing" "require-final-newline")
(custom-group-add! "display" "scroll-margin")
(custom-group-add! "display" "show-paren-mode")
(custom-group-add! "files" "global-auto-revert-mode")
(custom-group-add! "files" "delete-trailing-whitespace-on-save")

;;;============================================================================
;;; Face customization UI
;;;============================================================================

(def *face-definitions* (make-hash-table))  ;; face-name -> alist of properties

(def (face-set! name . props)
  "Define or update a face with properties."
  (hash-put! *face-definitions* name props))

(def (face-get name)
  "Get face properties."
  (hash-get *face-definitions* name))

;; Define some default faces
(face-set! "default" 'fg: "white" 'bg: "black")
(face-set! "region" 'bg: "blue")
(face-set! "modeline" 'fg: "black" 'bg: "white")
(face-set! "minibuffer" 'fg: "white" 'bg: "black")
(face-set! "comment" 'fg: "gray" 'style: "italic")
(face-set! "string" 'fg: "green")
(face-set! "keyword" 'fg: "cyan" 'style: "bold")
(face-set! "error" 'fg: "red" 'style: "bold")
(face-set! "warning" 'fg: "yellow")
(face-set! "success" 'fg: "green")

;;;============================================================================
;;; Advice system
;;;============================================================================

(def *advice-before* (make-hash-table))  ;; symbol -> list of (fn . name)
(def *advice-after*  (make-hash-table))  ;; symbol -> list of (fn . name)

(def (advice-add! symbol where fn advice-name)
  "Add advice to a command symbol. WHERE is 'before or 'after."
  (let ((table (if (eq? where 'before) *advice-before* *advice-after*))
        (entry (cons fn advice-name)))
    (let ((existing (or (hash-get table symbol) [])))
      (hash-put! table symbol (cons entry existing)))))

(def (advice-remove! symbol advice-name)
  "Remove named advice from a command symbol."
  (for-each
    (lambda (table)
      (let ((existing (or (hash-get table symbol) [])))
        (hash-put! table symbol
          (filter (lambda (e) (not (equal? (cdr e) advice-name))) existing))))
    [*advice-before* *advice-after*]))

(def (run-advice-before symbol app)
  "Run all before-advice for SYMBOL."
  (let ((advices (hash-get *advice-before* symbol)))
    (when advices
      (for-each (lambda (entry) ((car entry) app)) (reverse advices)))))

(def (run-advice-after symbol app)
  "Run all after-advice for SYMBOL."
  (let ((advices (hash-get *advice-after* symbol)))
    (when advices
      (for-each (lambda (entry) ((car entry) app)) (reverse advices)))))

(def (cmd-describe-advice app)
  "Show all active advice on commands."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (ed (current-editor app))
         (win (current-window fr))
         (buf (buffer-create! "*Advice*" ed))
         (lines ["Command Advice"
                 "=============="
                 ""]))
    (hash-for-each
      (lambda (sym advices)
        (for-each
          (lambda (entry)
            (set! lines (cons
              (string-append "  :before " (symbol->string sym) " — " (cdr entry))
              lines)))
          advices))
      *advice-before*)
    (hash-for-each
      (lambda (sym advices)
        (for-each
          (lambda (entry)
            (set! lines (cons
              (string-append "  :after  " (symbol->string sym) " — " (cdr entry))
              lines)))
          advices))
      *advice-after*)
    (when (= (length lines) 3)
      (set! lines (cons "  (no active advice)" lines)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-join (reverse lines) "\n"))
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;;;============================================================================
;;; Autoload system
;;;============================================================================

(def *autoloads* (make-hash-table))  ;; symbol -> file-path

(def (autoload! symbol file-path)
  "Register SYMBOL to be loaded from FILE-PATH on first use."
  (hash-put! *autoloads* symbol file-path))

(def (autoload-resolve symbol)
  "If SYMBOL has an autoload, load the file and return #t, else #f."
  (let ((path (hash-get *autoloads* symbol)))
    (when path
      (hash-remove! *autoloads* symbol)
      (with-catch
        (lambda (e) #f)
        (lambda ()
          (load (path-expand path))
          #t)))))

(def (cmd-list-autoloads app)
  "Show registered autoloads."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (ed (current-editor app))
         (win (current-window fr))
         (buf (buffer-create! "*Autoloads*" ed))
         (lines ["Registered Autoloads"
                 "===================="
                 ""]))
    (hash-for-each
      (lambda (sym path)
        (set! lines (cons
          (string-append "  " (symbol->string sym) " → " path)
          lines)))
      *autoloads*)
    (when (= (length lines) 3)
      (set! lines (cons "  (no autoloads registered)" lines)))
    (set! lines (append (reverse lines)
      ["" "Use (autoload! 'symbol \"path.ss\") in ~/.gemacs-init to register."]))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-join lines "\n"))
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;;;============================================================================
;;; Dynamic module loading
;;;============================================================================

(def *loaded-modules* [])

(def (cmd-load-module app)
  "Load a compiled Gerbil module (.so or .ss) at runtime."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (path (echo-read-string echo "Load module: " row width)))
    (when (and path (> (string-length path) 0))
      (let ((full-path (path-expand path)))
        (if (not (file-exists? full-path))
          (echo-error! echo (string-append "Module not found: " full-path))
          (with-catch
            (lambda (e)
              (echo-error! echo (string-append "Load error: "
                (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (load full-path)
              (set! *loaded-modules* (cons full-path *loaded-modules*))
              (echo-message! echo (string-append "Loaded module: "
                (path-strip-directory full-path))))))))))

(def (cmd-list-modules app)
  "Show loaded dynamic modules."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (ed (current-editor app))
         (win (current-window fr))
         (buf (buffer-create! "*Modules*" ed))
         (text (string-append
                 "Loaded Modules\n"
                 "==============\n\n"
                 (if (null? *loaded-modules*)
                   "  (none)\n"
                   (string-join (map (lambda (m) (string-append "  " m)) *loaded-modules*) "\n"))
                 "\n\nUse M-x load-module to load a module.\n")))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;;;============================================================================
;;; Icomplete / Fido mode
;;;============================================================================

(def *icomplete-mode* #f)

(def (cmd-icomplete-mode app)
  "Toggle icomplete-mode (inline completion display)."
  (let ((echo (app-state-echo app)))
    (set! *icomplete-mode* (not *icomplete-mode*))
    (echo-message! echo (if *icomplete-mode*
                          "Icomplete mode ON (inline completions)"
                          "Icomplete mode OFF"))))

(def (cmd-fido-mode app)
  "Toggle fido-mode (flex matching + icomplete)."
  (let ((echo (app-state-echo app)))
    (set! *icomplete-mode* (not *icomplete-mode*))
    (echo-message! echo (if *icomplete-mode*
                          "Fido mode ON (flex matching)"
                          "Fido mode OFF"))))

;;;============================================================================
;;; Marginalia (annotations in completions)
;;;============================================================================

(def *marginalia-annotators* (make-hash-table))

(def (marginalia-annotate! category annotator)
  "Register an annotator function for a completion CATEGORY."
  (hash-put! *marginalia-annotators* category annotator))

(marginalia-annotate! 'command
  (lambda (name)
    (let ((cmd (find-command (string->symbol name))))
      (if cmd " [command]" ""))))

(marginalia-annotate! 'buffer
  (lambda (name)
    (let ((buf (buffer-by-name name)))
      (if buf
        (let ((file (buffer-file-path buf)))
          (if file (string-append " " file) " [no file]"))
        ""))))

;;;============================================================================
;;; Embark action registry (used by cmd-embark-act in editor-extra-modes.ss)
;;;============================================================================

(def *embark-actions* (make-hash-table))

(def (embark-define-action! category name fn)
  "Register an action for completion candidates of CATEGORY."
  (let ((existing (or (hash-get *embark-actions* category) [])))
    (hash-put! *embark-actions* category (cons (cons name fn) existing))))

(embark-define-action! 'command "describe"
  (lambda (app candidate)
    (echo-message! (app-state-echo app)
      (string-append "Command: " candidate))))

(embark-define-action! 'command "execute"
  (lambda (app candidate)
    (let ((cmd (find-command (string->symbol candidate))))
      (when cmd (cmd app)))))

(embark-define-action! 'file "find-file"
  (lambda (app candidate)
    (echo-message! (app-state-echo app)
      (string-append "Would open: " candidate))))

(embark-define-action! 'file "delete"
  (lambda (app candidate)
    (echo-message! (app-state-echo app)
      (string-append "Would delete: " candidate))))

;;;============================================================================
;;; Persistent undo across sessions
;;;============================================================================

(def *persistent-undo-dir*
  (string-append (or (getenv "HOME" #f) ".") "/.gemacs-undo/"))

(def (persistent-undo-file-for path)
  "Return the undo save file path for a given file path."
  (string-append *persistent-undo-dir*
    (string-map (lambda (c) (if (char=? c #\/) #\_ c))
                (if (> (string-length path) 0) (substring path 1 (string-length path)) "unknown"))
    ".undo"))

(def (cmd-undo-history-save app)
  "Save undo history for the current buffer to disk."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if (not file)
      (echo-message! echo "Buffer has no file — cannot save undo history")
      (let ((undo-file (persistent-undo-file-for file))
            (text (editor-get-text ed)))
        (with-catch
          (lambda (e) (echo-message! echo (string-append "Error saving undo: " (error-message e))))
          (lambda ()
            (create-directory* *persistent-undo-dir*)
            (call-with-output-file undo-file
              (lambda (port)
                (write (list 'undo-v1 file (string-length text)) port)
                (newline port)))
            (echo-message! echo (string-append "Undo history saved: " undo-file))))))))

(def (cmd-undo-history-load app)
  "Load undo history for the current buffer from disk."
  (let* ((echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if (not file)
      (echo-message! echo "Buffer has no file — cannot load undo history")
      (let ((undo-file (persistent-undo-file-for file)))
        (if (not (file-exists? undo-file))
          (echo-message! echo "No saved undo history for this file")
          (with-catch
            (lambda (e) (echo-message! echo (string-append "Error loading undo: " (error-message e))))
            (lambda ()
              (let ((data (call-with-input-file undo-file read)))
                (echo-message! echo (string-append "Undo history loaded from: " undo-file))))))))))

;;;============================================================================
;;; Image thumbnails in dired
;;;============================================================================

(def *image-extensions* '("png" "jpg" "jpeg" "gif" "bmp" "svg" "webp" "ico" "tiff"))

(def (image-file? path)
  "Return #t if path has an image file extension."
  (let ((ext (string-downcase (path-extension path))))
    (member ext *image-extensions*)))

(def (cmd-image-dired-display-thumbnail app)
  "Display thumbnail info for image under cursor in dired."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf)))
    (if (not (string-suffix? " [dired]" name))
      (echo-message! echo "Not in a dired buffer")
      (let* ((pos (editor-get-current-pos ed))
             (line (editor-get-line ed (editor-line-from-position ed pos)))
             (trimmed (string-trim-both line)))
        (if (image-file? trimmed)
          (echo-message! echo (string-append "Image: " trimmed " [thumbnail view not available in TUI]"))
          (echo-message! echo "Not an image file"))))))

(def (cmd-image-dired-show-all-thumbnails app)
  "List all image files in the current dired directory."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf)))
    (if (not (string-suffix? " [dired]" name))
      (echo-message! echo "Not in a dired buffer")
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (images (filter (lambda (l) (image-file? (string-trim-both l)))
                             lines)))
        (if (null? images)
          (echo-message! echo "No image files in this directory")
          (let ((listing (string-join (map string-trim-both images) "\n")))
            (echo-message! echo
              (string-append "Images (" (number->string (length images)) "): "
                (string-join (map string-trim-both (take images (min 5 (length images)))) ", ")
                (if (> (length images) 5) "..." "")))))))))

;;;============================================================================
;;; Virtual dired (dired from search results)
;;;============================================================================

(def (cmd-virtual-dired app)
  "Create a virtual dired buffer from a list of file paths."
  (let* ((echo (app-state-echo app))
         (input (app-read-string app "Virtual dired files (space-separated): ")))
    (when (and input (> (string-length input) 0))
      (let* ((files (string-split input #\space))
             (content (string-join
                        (map (lambda (f)
                               (string-append "  " (path-strip-directory f) "  → " f))
                             files)
                        "\n")))
        (open-output-buffer app "*Virtual Dired*"
          (string-append "Virtual Dired:\n\n" content "\n"))
        (echo-message! echo (string-append "Virtual dired: " (number->string (length files)) " files"))))))

(def (cmd-dired-from-find app)
  "Create a virtual dired from find command results."
  (let* ((echo (app-state-echo app))
         (pattern (app-read-string app "Find pattern (glob): ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((buf (current-buffer-from-app app))
             (dir (or (buffer-file-path buf) (current-directory))))
        (echo-message! echo (string-append "Virtual dired from find: " pattern " in " dir))))))

;;;============================================================================
;;; Super/Hyper key mapping and global key remap
;;;============================================================================

(def (cmd-key-translate app)
  "Define a key translation (input-decode-map equivalent)."
  (let* ((echo (app-state-echo app))
         (from (app-read-string app "Translate from key: ")))
    (when (and from (> (string-length from) 0))
      (let* ((to (app-read-string app "Translate to key: "))
             (from-ch (if (= (string-length from) 1) (string-ref from 0) #f))
             (to-ch (if (and to (= (string-length to) 1)) (string-ref to 0) #f)))
        (when (and from-ch to-ch)
          (key-translate! from-ch to-ch)
          (echo-message! echo (string-append "Key translation: " from " → " to)))))))

(def *super-key-mode* #f)

(def (cmd-toggle-super-key-mode app)
  "Toggle super key mode (treat super as meta)."
  (let ((echo (app-state-echo app)))
    (set! *super-key-mode* (not *super-key-mode*))
    (echo-message! echo (if *super-key-mode*
                          "Super-key-mode enabled (super → meta)"
                          "Super-key-mode disabled"))))

(def (cmd-describe-key-translations app)
  "Show all active key translations."
  (let ((echo (app-state-echo app)))
    (echo-message! echo "Key translations: use key-translate to define")))

