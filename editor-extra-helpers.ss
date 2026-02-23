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

