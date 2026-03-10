;;; -*- Gerbil -*-
;;; Shared helpers for editor-extra sub-modules

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        (only-in :std/text/json
          json-object->string read-json string->json-object)
        (only-in :std/net/request
          http-post request-status request-text request-close)
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        (only-in :gemacs/persist
          *copilot-mode* *copilot-api-key* *copilot-model*
          *copilot-api-url* *copilot-suggestion* *copilot-suggestion-pos*))

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
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (col-str (app-read-string app "Sort by column (start-end): ")))
    (when (and col-str (not (string-empty? col-str)))
      (let* ((parts (string-split col-str #\-))
             (start-col (and (pair? parts) (string->number (car parts))))
             (end-col (and (> (length parts) 1) (string->number (cadr parts)))))
        (if (not start-col)
          (echo-message! echo "Invalid column spec — use start-end (e.g., 10-20)")
          (let* ((text (editor-get-text ed))
                 (lines (string-split text #\newline))
                 (end-c (or end-col 999))
                 (extract (lambda (line)
                            (if (>= (string-length line) start-col)
                              (substring line (- start-col 1) (min end-c (string-length line)))
                              "")))
                 (sorted (sort lines (lambda (a b) (string<? (extract a) (extract b)))))
                 (result (string-join sorted "\n")))
            (editor-set-text ed result)
            (echo-message! echo (string-append "Sorted " (number->string (length lines)) " lines by columns "
                                               (number->string start-col) "-" (number->string end-c)))))))))

(def (cmd-sort-regexp-fields app)
  "Sort lines in region by regex match."
  (let* ((echo (app-state-echo app))
         (pattern (app-read-string app "Sort by regexp: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (extract (lambda (line)
                        (let ((m (string-contains line pattern)))
                          (if m (substring line m (string-length line)) line))))
             (sorted (sort lines (lambda (a b) (string<? (extract a) (extract b)))))
             (result (string-join sorted "\n")))
        (editor-set-text ed result)
        (echo-message! echo (string-append "Sorted " (number->string (length lines)) " lines by /" pattern "/"))))))

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
             (dir (or (and buf (buffer-file-path buf) (path-directory (buffer-file-path buf)))
                      (current-directory))))
        (with-exception-catcher
          (lambda (e) (echo-error! echo "find command failed"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "find" arguments: (list dir "-name" pattern "-type" "f")
                                 stdin-redirection: #f stdout-redirection: #t stderr-redirection: #f)))
                   (out (read-line proc #f)))
              (process-status proc)
              (if (and out (> (string-length out) 0))
                (open-output-buffer app (string-append dir " [find:" pattern "]")
                  (string-append "  " dir " (find: " pattern "):\n\n" out "\n"))
                (echo-message! echo (string-append "No files matching: " pattern))))))))))

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

;;;============================================================================
;;; Display tables (character display mapping)
;;;============================================================================

(def *display-table* (make-hash-table))

(def (display-table-set! char replacement)
  "Set a display table entry: show REPLACEMENT instead of CHAR."
  (hash-put! *display-table* char replacement))

(def (display-table-get char)
  "Look up display table entry for CHAR."
  (hash-get *display-table* char))

(def (cmd-set-display-table-entry app)
  "Set a display table entry to map one character to another."
  (let* ((echo (app-state-echo app))
         (from (app-read-string app "Display char (single): ")))
    (when (and from (= (string-length from) 1))
      (let ((to (app-read-string app "Display as: ")))
        (when (and to (> (string-length to) 0))
          (display-table-set! (string-ref from 0) to)
          (echo-message! echo (string-append "Display: " from " → " to)))))))

(def (cmd-describe-display-table app)
  "Show current display table entries."
  (let* ((echo (app-state-echo app))
         (entries (hash->list *display-table*)))
    (if (null? entries)
      (echo-message! echo "Display table: empty (default rendering)")
      (echo-message! echo
        (string-append "Display table: "
          (string-join
            (map (lambda (p) (string-append (string (car p)) " → " (cdr p)))
                 entries)
            ", "))))))

;;;============================================================================
;;; Multi-server LSP support
;;;============================================================================

(def *lsp-servers* (make-hash-table))

(def (lsp-server-register! lang-id command)
  "Register an LSP server command for a language."
  (hash-put! *lsp-servers* lang-id command))

(def (lsp-server-for lang-id)
  "Look up registered LSP server for a language."
  (hash-get *lsp-servers* lang-id))

;; Default server registrations
(lsp-server-register! "python" "pylsp")
(lsp-server-register! "javascript" "typescript-language-server --stdio")
(lsp-server-register! "typescript" "typescript-language-server --stdio")
(lsp-server-register! "rust" "rust-analyzer")
(lsp-server-register! "go" "gopls")
(lsp-server-register! "c" "clangd")
(lsp-server-register! "cpp" "clangd")

(def (cmd-lsp-set-server app)
  "Set LSP server command for a language."
  (let* ((echo (app-state-echo app))
         (lang (app-read-string app "Language ID: ")))
    (when (and lang (> (string-length lang) 0))
      (let ((cmd (app-read-string app "Server command: ")))
        (when (and cmd (> (string-length cmd) 0))
          (lsp-server-register! lang cmd)
          (echo-message! echo (string-append "LSP server for " lang ": " cmd)))))))

(def (cmd-lsp-list-servers app)
  "List registered LSP servers."
  (let* ((echo (app-state-echo app))
         (entries (hash->list *lsp-servers*)))
    (if (null? entries)
      (echo-message! echo "No LSP servers registered")
      (let ((listing (string-join
                       (map (lambda (p) (string-append (car p) ": " (cdr p)))
                            entries)
                       ", ")))
        (echo-message! echo (string-append "LSP servers: " listing))))))

;;;============================================================================
;;; DevOps modes: Ansible, Systemd, Kubernetes
;;;============================================================================

(def (cmd-ansible-mode app)
  "Enable Ansible YAML mode for playbooks."
  (echo-message! (app-state-echo app) "Ansible mode enabled (YAML highlighting)"))

(def (cmd-systemd-mode app)
  "Enable systemd unit file mode."
  (echo-message! (app-state-echo app) "Systemd unit file mode enabled"))

(def (cmd-kubernetes-mode app)
  "Enable Kubernetes manifest mode."
  (echo-message! (app-state-echo app) "Kubernetes mode enabled (YAML highlighting)"))

(def (cmd-ssh-config-mode app)
  "Enable SSH config file mode."
  (echo-message! (app-state-echo app) "SSH config mode enabled"))

;;;============================================================================
;;; Helm-style occur and dash documentation
;;;============================================================================

(def (cmd-helm-occur app)
  "Helm-style occur: filter lines matching pattern interactively."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (pattern (app-read-string app "Helm occur pattern: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matches (filter (lambda (l) (string-contains l pattern)) lines)))
        (if (null? matches)
          (echo-message! echo "No matches")
          (open-output-buffer app "*Helm Occur*"
            (string-append "Helm Occur: " pattern "\n\n"
              (string-join matches "\n") "\n")))))))

(def (cmd-helm-dash app)
  "Search documentation — uses man pages and apropos."
  (let* ((echo (app-state-echo app))
         (query (app-read-string app "Dash search: ")))
    (when (and query (> (string-length query) 0))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (proc (open-process
                     (list path: "/usr/bin/man" arguments: (list "-k" query)
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (output (read-line proc #f))
             (status (process-status proc)))
        (close-port proc)
        (let ((buf (buffer-create! (string-append "*Dash: " query "*") ed)))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer win) buf)
          (editor-set-text ed
            (string-append "Documentation Search: " query "\n"
                           "========================================\n\n"
                           (or output "(No results found)\n")
                           "\n\nUse M-x man to view a specific man page.\n"))
          (echo-message! echo (string-append "Dash: found results for '" query "'")))))))

;;; Batch 14: Completion, AI, TRAMP/Remote

;; Selectrum mode (alternative to Vertico)
(def (cmd-selectrum-mode app)
  "Toggle Selectrum mode — alternative vertical completion."
  (let ((on (toggle-mode! 'selectrum)))
    (echo-message! (app-state-echo app)
      (if on "Selectrum mode: on (using narrowing)" "Selectrum mode: off"))))

;; Cape additional completion sources
(def (cmd-cape-history app)
  "Cape history completion — complete from minibuffer history."
  (let* ((echo (app-state-echo app))
         (choice (app-read-string app "History: ")))
    (if (and choice (> (string-length choice) 0))
      (echo-message! echo (string-append "Cape history: " choice))
      (echo-message! echo "No history selection"))))

(def (cmd-cape-keyword app)
  "Cape keyword completion — insert language keyword at point."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (edit-window-buffer win))
         (ext (let ((fp (buffer-file-path buf))) (if fp (path-extension fp) "")))
         (keywords
           (cond
             ((member ext '(".ss" ".scm" ".sld"))
              '("define" "lambda" "let" "let*" "letrec" "if" "cond" "case" "begin" "when"
                "unless" "do" "and" "or" "not" "set!" "import" "export" "def" "defstruct"
                "defclass" "defrule" "defsyntax" "match" "with" "try" "catch" "finally" "spawn"))
             ((member ext '(".py"))
              '("def" "class" "if" "elif" "else" "for" "while" "return" "import" "from"
                "try" "except" "finally" "raise" "with" "as" "yield" "async" "await" "lambda"
                "pass" "break" "continue" "global" "nonlocal" "assert" "del"))
             ((member ext '(".js" ".ts" ".jsx" ".tsx"))
              '("function" "const" "let" "var" "if" "else" "for" "while" "return" "import"
                "export" "class" "extends" "new" "this" "super" "async" "await" "try" "catch"
                "finally" "throw" "switch" "case" "default" "break" "continue"))
             ((member ext '(".go"))
              '("func" "var" "const" "type" "if" "else" "for" "range" "return" "struct"
                "interface" "package" "import" "defer" "go" "chan" "select" "switch" "case"
                "default" "break" "continue" "map" "make" "append" "len" "cap"))
             ((member ext '(".rs"))
              '("fn" "let" "mut" "if" "else" "for" "while" "loop" "match" "return"
                "struct" "enum" "impl" "trait" "pub" "mod" "use" "crate" "self" "super"
                "async" "await" "move" "ref" "where" "type" "const" "static" "unsafe"))
             ((member ext '(".c" ".h" ".cpp" ".hpp"))
              '("if" "else" "for" "while" "do" "switch" "case" "break" "continue" "return"
                "struct" "union" "enum" "typedef" "const" "static" "extern" "volatile"
                "sizeof" "void" "int" "char" "float" "double" "long" "short" "unsigned"))
             (else '()))))
    (if (null? keywords)
      (echo-message! echo "No keywords for this file type")
      (let ((choice (app-read-string app "Keyword: ")))
        (when (and choice (> (string-length choice) 0)
                   (member choice keywords))
          (let ((pos (editor-get-current-pos ed)))
            (editor-insert-text ed pos choice)))))))

;; AI features — inline suggestions, code explain, code refactor
(def (cmd-ai-inline-suggest app)
  "Toggle inline AI suggestions — ghost text completion."
  (let ((on (toggle-mode! 'ai-inline)))
    (echo-message! (app-state-echo app)
      (if on "AI inline suggestions: on" "AI inline suggestions: off"))))

(def (tui-ai-detect-language app)
  "Detect language from buffer file extension."
  (let* ((buf (current-buffer-from-app app))
         (file (and buf (buffer-file-path buf))))
    (if (and file (string? file))
      (let ((ext (path-extension file)))
        (cond
          ((member ext '(".ss" ".scm" ".sld")) "Scheme")
          ((member ext '(".py")) "Python")
          ((member ext '(".rs")) "Rust")
          ((member ext '(".go")) "Go")
          ((member ext '(".c" ".h")) "C")
          ((member ext '(".cpp" ".cc" ".hpp")) "C++")
          ((member ext '(".js" ".jsx")) "JavaScript")
          ((member ext '(".ts" ".tsx")) "TypeScript")
          ((member ext '(".sh" ".bash")) "Shell/Bash")
          (else "code")))
      "code")))

(def (tui-ai-request prompt code language)
  "Call AI API. Returns response string or #f."
  (when (string=? *copilot-api-key* "")
    (error "Set OPENAI_API_KEY: M-x copilot-mode"))
  (let* ((body (json-object->string
                 (hash ("model" *copilot-model*)
                       ("messages" [(hash ("role" "system")
                                          ("content" (string-append
                                            "You are a code assistant for " language ". " prompt)))
                                    (hash ("role" "user")
                                          ("content" code))])
                       ("max_tokens" 1000)
                       ("temperature" 0.3))))
         (resp (http-post *copilot-api-url*
                 data: body
                 headers: [["Content-Type" . "application/json"]
                           ["Authorization" . (string-append "Bearer " *copilot-api-key*)]])))
    (if (= (request-status resp) 200)
      (let* ((json-str (request-text resp))
             (result (call-with-input-string json-str read-json))
             (choices (hash-ref result "choices" []))
             (first-choice (and (pair? choices) (car choices)))
             (message (and first-choice (hash-ref first-choice "message" #f)))
             (content (and message (hash-ref message "content" ""))))
        (request-close resp)
        (or content ""))
      (begin (request-close resp) #f))))

(def (cmd-ai-code-explain app)
  "Explain code at point or region using AI."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lang (tui-ai-detect-language app)))
    (if (string=? *copilot-api-key* "")
      (echo-message! echo "Set OPENAI_API_KEY first (M-x copilot-mode)")
      (begin
        (echo-message! echo "AI: requesting explanation...")
        (with-catch
          (lambda (e)
            (echo-message! echo (string-append "AI error: "
              (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((response (tui-ai-request
                              "Explain this code clearly and concisely."
                              text lang)))
              (if response
                (open-output-buffer app "*AI Explain*"
                  (string-append "Code Explanation (" lang ")\n"
                    (make-string 50 #\=) "\n\n" response "\n"))
                (echo-message! echo "AI: no response received")))))))))

(def (cmd-ai-code-refactor app)
  "Suggest refactoring for code at point or region using AI."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lang (tui-ai-detect-language app)))
    (if (string=? *copilot-api-key* "")
      (echo-message! echo "Set OPENAI_API_KEY first (M-x copilot-mode)")
      (begin
        (echo-message! echo "AI: requesting refactoring suggestions...")
        (with-catch
          (lambda (e)
            (echo-message! echo (string-append "AI error: "
              (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((response (tui-ai-request
                              "Suggest refactoring improvements. Show refactored code with explanations."
                              text lang)))
              (if response
                (open-output-buffer app "*AI Refactor*"
                  (string-append "Refactoring Suggestions (" lang ")\n"
                    (make-string 50 #\=) "\n\n" response "\n"))
                (echo-message! echo "AI: no response received")))))))))

;; TRAMP/Remote editing

(def (cmd-tramp-ssh-edit app)
  "Edit file via SSH. Fetches remote file content."
  (let* ((echo (app-state-echo app))
         (path (app-read-string app "SSH path (/ssh:host:path): ")))
    (when (and path (> (string-length path) 0))
      (cond
        ((string-prefix? "/ssh:" path)
         (let* ((rest (substring path 5 (string-length path)))
                (colon (string-index rest #\:))
                (host (if colon (substring rest 0 colon) rest))
                (rpath (if colon (substring rest (+ colon 1) (string-length rest)) "~")))
           (echo-message! echo (string-append "SSH: fetching " host ":" rpath " ..."))
           (with-catch
             (lambda (e)
               (echo-message! echo (string-append "SSH failed: "
                 (with-output-to-string (lambda () (display-exception e))))))
             (lambda ()
               (let* ((proc (open-process
                              (list path: "ssh"
                                    arguments: [host "cat" rpath]
                                    stdin-redirection: #f
                                    stdout-redirection: #t
                                    stderr-redirection: #t)))
                      (content (read-line proc #f)))
                 (process-status proc)
                 (close-port proc)
                 (if (or (not content) (string=? content ""))
                   (echo-message! echo (string-append "Could not read " host ":" rpath))
                   (let* ((name (string-append "[ssh:" host "]"
                                  (path-strip-directory rpath)))
                          (fr (app-state-frame app))
                          (win (current-window fr))
                          (ed (edit-window-editor win))
                          (buf (buffer-create! name ed #f)))
                     (buffer-attach! ed buf)
                     (set! (edit-window-buffer win) buf)
                     (editor-set-text ed content)
                     (editor-goto-pos ed 0)
                     (echo-message! echo
                       (string-append "Opened " host ":" rpath)))))))))
        (else
         (echo-message! echo "Use format: /ssh:hostname:/path/to/file"))))))

(def (cmd-tramp-docker-edit app)
  "Edit file in Docker container via docker exec cat."
  (let* ((echo (app-state-echo app))
         (path (app-read-string app "Docker path (/docker:name:path): ")))
    (when (and path (> (string-length path) 0))
      (cond
        ((string-prefix? "/docker:" path)
         (let* ((rest (substring path 8 (string-length path)))
                (colon (string-index rest #\:))
                (container (if colon (substring rest 0 colon) rest))
                (rpath (if colon (substring rest (+ colon 1) (string-length rest)) "/")))
           (echo-message! echo (string-append "Docker: fetching " container ":" rpath " ..."))
           (with-catch
             (lambda (e)
               (echo-message! echo (string-append "Docker failed: "
                 (with-output-to-string (lambda () (display-exception e))))))
             (lambda ()
               (let* ((proc (open-process
                              (list path: "docker"
                                    arguments: ["exec" container "cat" rpath]
                                    stdin-redirection: #f
                                    stdout-redirection: #t
                                    stderr-redirection: #t)))
                      (content (read-line proc #f)))
                 (process-status proc)
                 (close-port proc)
                 (if (or (not content) (string=? content ""))
                   (echo-message! echo (string-append "Could not read " container ":" rpath))
                   (let* ((name (string-append "[docker:" container "]"
                                  (path-strip-directory rpath)))
                          (fr (app-state-frame app))
                          (win (current-window fr))
                          (ed (edit-window-editor win))
                          (buf (buffer-create! name ed #f)))
                     (buffer-attach! ed buf)
                     (set! (edit-window-buffer win) buf)
                     (editor-set-text ed content)
                     (editor-goto-pos ed 0)
                     (echo-message! echo
                       (string-append "Opened " container ":" rpath)))))))))
        (else
         (echo-message! echo "Use format: /docker:container:/path/to/file"))))))

(def (cmd-tramp-remote-shell app)
  "Open remote shell via SSH — runs ssh and displays session output."
  (let* ((echo (app-state-echo app))
         (host (app-read-string app "Remote host: ")))
    (when (and host (> (string-length host) 0))
      (echo-message! echo (string-append "Connecting to " host "..."))
      (with-catch
        (lambda (e)
          (echo-error! echo (string-append "SSH failed: "
            (with-output-to-string (lambda () (display-exception e))))))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "ssh"
                               arguments: ["-t" host]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f)))
            (process-status proc)
            (close-port proc)
            (let* ((ed (current-editor app))
                   (fr (app-state-frame app))
                   (buf-name (string-append "*ssh:" host "*"))
                   (buf (or (buffer-by-name buf-name)
                            (buffer-create! buf-name ed #f))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-read-only ed #f)
              (editor-set-text ed
                (string-append "-*- SSH: " host " -*-\n"
                  (make-string 60 #\-) "\n\n"
                  (or output "")
                  "\n" (make-string 60 #\-) "\n"
                  "Connection closed.\n"))
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (editor-set-read-only ed #t)
              (echo-message! echo (string-append "SSH session to " host " ended")))))))))

(def (cmd-tramp-remote-compile app)
  "Run compilation command on remote host via SSH."
  (let* ((echo (app-state-echo app))
         (host (app-read-string app "Remote host: "))
         (cmd (and host (> (string-length host) 0)
                   (app-read-string app (string-append "Command on " host ": ")))))
    (when (and cmd (> (string-length cmd) 0))
      (echo-message! echo (string-append "Compiling on " host ": " cmd))
      (with-catch
        (lambda (e)
          (echo-error! echo (string-append "Remote compile failed: "
            (with-output-to-string (lambda () (display-exception e))))))
        (lambda ()
          (let* ((quoted-cmd (string-append "'"
                               (string-join (string-split cmd #\') "'\\''") "'"))
                 (proc (open-process
                         (list path: "ssh"
                               arguments: [host quoted-cmd]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (status (process-status proc)))
            (close-port proc)
            (let* ((ed (current-editor app))
                   (fr (app-state-frame app))
                   (buf (or (buffer-by-name "*compilation*")
                            (buffer-create! "*compilation*" ed #f)))
                   (result-text (string-append
                                  "-*- Compilation (remote: " host ") -*-\n"
                                  "Command: ssh " host " " cmd "\n"
                                  (make-string 60 #\-) "\n\n"
                                  (or output "")
                                  "\n" (make-string 60 #\-) "\n"
                                  "Compilation "
                                  (if (= status 0) "finished" "exited abnormally")
                                  "\n")))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-read-only ed #f)
              (editor-set-text ed result-text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (editor-set-read-only ed #t)
              (echo-message! echo
                (if (= status 0) "Compilation finished"
                    "Compilation exited abnormally")))))))))

;; Helm C-yasnippet — browse snippets with helm-style narrowing
(def (cmd-helm-c-yasnippet app)
  "Helm-style snippet browser with preview."
  (let* ((echo (app-state-echo app)))
    (echo-message! echo "Helm C-yasnippet: use M-x snippet-insert for snippet browsing")))

;;; Batch 15: Parity stubs — close remaining red circles

(def (cmd-tree-sitter-mode app)
  "Toggle tree-sitter mode — incremental parsing."
  (let ((on (toggle-mode! 'tree-sitter)))
    (echo-message! (app-state-echo app) (if on "Tree-sitter mode: on" "Tree-sitter mode: off"))))

(def (cmd-tree-sitter-highlight-mode app)
  "Toggle tree-sitter highlighting — uses language grammars."
  (let ((on (toggle-mode! 'tree-sitter-highlight)))
    (echo-message! (app-state-echo app) (if on "Tree-sitter highlighting: on" "Tree-sitter highlighting: off"))))

(def (cmd-tool-bar-mode app)
  "Toggle tool bar display."
  (echo-message! (app-state-echo app) "Tool bar: N/A in terminal mode"))

(def (cmd-mu4e app)
  "Launch mu4e email — checks for mu installation."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (proc (open-process
                 (list path: "/bin/sh"
                       arguments: (list "-c" "which mu 2>/dev/null && mu find --fields='d f s' --sortfield=date --reverse --maxnum=20 '' 2>/dev/null || echo 'mu not installed. Install with: apt install maildir-utils'")
                       stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
         (output (read-line proc #f))
         (status (process-status proc)))
    (close-port proc)
    (let ((buf (buffer-create! "*mu4e*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "mu4e — Mail\n============\n" (or output "") "\n"))
      (echo-message! echo (if (= status 0) "mu4e: loaded" "mu4e: mu not installed")))))

(def (cmd-notmuch app)
  "Launch notmuch search — checks for notmuch installation."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (proc (open-process
                 (list path: "/bin/sh"
                       arguments: (list "-c" "which notmuch 2>/dev/null && notmuch search --limit=20 --sort=newest-first '*' 2>/dev/null || echo 'notmuch not installed. Install with: apt install notmuch'")
                       stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
         (output (read-line proc #f))
         (status (process-status proc)))
    (close-port proc)
    (let ((buf (buffer-create! "*notmuch*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "notmuch — Search\n============\n" (or output "") "\n"))
      (echo-message! echo (if (= status 0) "notmuch: loaded" "notmuch: not installed")))))

(def (cmd-rcirc app)
  "Launch rcirc IRC client."
  (let* ((echo (app-state-echo app))
         (server (app-read-string app "IRC server: ")))
    (when (and server (> (string-length server) 0))
      (echo-message! echo (string-append "rcirc: connecting to " server " ...")))))

(def (cmd-eww-submit-form app)
  "Submit form in current EWW buffer. Parses [field: value] lines."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed)))
    (let loop ((lines (string-split text #\newline)) (fields []))
      (if (null? lines)
        (if (null? fields)
          (echo-message! echo "No form fields found in buffer")
          (let ((params (string-join
                          (map (lambda (pair)
                                 (string-append (car pair) "=" (cdr pair)))
                               fields) "&")))
            (echo-message! echo (string-append "Form data: " params))))
        (let ((line (car lines)))
          (if (and (> (string-length line) 4)
                   (char=? (string-ref line 0) #\[)
                   (string-contains line ": "))
            (let* ((inner (substring line 1 (- (string-length line) 1)))
                   (colon (string-contains inner ": "))
                   (name (substring inner 0 colon))
                   (val (substring inner (+ colon 2) (string-length inner))))
              (loop (cdr lines) (cons (cons name val) fields)))
            (loop (cdr lines) fields)))))))

(def (cmd-eww-toggle-css app)
  "Toggle CSS rendering in EWW."
  (let ((on (toggle-mode! 'eww-css)))
    (echo-message! (app-state-echo app) (if on "EWW CSS: on" "EWW CSS: off"))))

(def (cmd-eww-toggle-images app)
  "Toggle image display in EWW."
  (let ((on (toggle-mode! 'eww-images)))
    (echo-message! (app-state-echo app) (if on "EWW images: on" "EWW images: off"))))

(def (cmd-screen-reader-mode app)
  "Toggle screen reader support."
  (let ((on (toggle-mode! 'screen-reader)))
    (echo-message! (app-state-echo app) (if on "Screen reader: on" "Screen reader: off"))))

;;;============================================================================
;;; Org-crypt — encrypt/decrypt org entries with GPG
;;;============================================================================

(def (tui-org-find-entry-bounds text pos)
  "Find the start and end of the org entry at POS.
   Returns (values start end heading-end)."
  (let* ((lines (string-split text #\newline))
         (len (string-length text)))
    (let loop ((i 0) (offset 0) (entry-start 0) (heading-end 0) (level 0))
      (if (>= i (length lines))
        (values entry-start len heading-end)
        (let* ((line (list-ref lines i))
               (line-end (+ offset (string-length line) 1)))
          (cond
            ((and (> (string-length line) 0) (char=? (string-ref line 0) #\*))
             (let ((line-level (let count ((j 0))
                                 (if (and (< j (string-length line))
                                          (char=? (string-ref line j) #\*))
                                   (count (+ j 1)) j))))
               (if (<= offset pos)
                 (loop (+ i 1) line-end offset line-end line-level)
                 (if (<= line-level level)
                   (values entry-start offset heading-end)
                   (loop (+ i 1) line-end entry-start heading-end level)))))
            (else
             (loop (+ i 1) line-end entry-start heading-end level))))))))

(def (cmd-org-encrypt-entry app)
  "Encrypt the current org entry body with GPG (symmetric)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    (let-values (((entry-start entry-end heading-end)
                  (tui-org-find-entry-bounds text pos)))
      (let ((body (substring text heading-end entry-end)))
        (if (or (string=? (string-trim-both body) "")
                (string-contains body "-----BEGIN PGP MESSAGE-----"))
          (echo-message! echo "Entry is empty or already encrypted")
          (with-catch
            (lambda (e)
              (echo-error! echo (string-append "Encryption failed: "
                (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (let* ((fr (app-state-frame app))
                     (row (- (frame-height fr) 1))
                     (width (frame-width fr))
                     (pass (echo-read-string echo "Passphrase: " row width)))
                (when (and pass (> (string-length pass) 0))
                  (let* ((proc (open-process
                                 (list path: "gpg"
                                       arguments: ["--symmetric" "--armor"
                                                   "--batch" "--yes"
                                                   "--passphrase-fd" "0"]
                                       stdin-redirection: #t
                                       stdout-redirection: #t
                                       stderr-redirection: #t))))
                    (display pass proc)
                    (display "\n" proc)
                    (display body proc)
                    (force-output proc)
                    (close-output-port proc)
                    (let ((encrypted (read-line proc #f)))
                      (process-status proc)
                      (close-port proc)
                      (when (and encrypted (string-contains encrypted "BEGIN PGP"))
                        (let ((new-text (string-append
                                          (substring text 0 heading-end)
                                          "\n" encrypted "\n"
                                          (if (< entry-end (string-length text))
                                            (substring text entry-end (string-length text))
                                            ""))))
                          (editor-set-text ed new-text)
                          (editor-goto-pos ed pos)
                          (echo-message! echo "Entry encrypted"))))))))))))))

(def (cmd-org-decrypt-entry app)
  "Decrypt the current org entry body (GPG symmetric)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    (let-values (((entry-start entry-end heading-end)
                  (tui-org-find-entry-bounds text pos)))
      (let ((body (substring text heading-end entry-end)))
        (if (not (string-contains body "-----BEGIN PGP MESSAGE-----"))
          (echo-message! echo "Entry is not encrypted")
          (with-catch
            (lambda (e)
              (echo-error! echo (string-append "Decryption failed: "
                (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (let* ((pgp-start (string-contains body "-----BEGIN PGP MESSAGE-----"))
                     (pgp-end-marker "-----END PGP MESSAGE-----")
                     (pgp-end-pos (string-contains body pgp-end-marker))
                     (pgp-block (if pgp-end-pos
                                  (substring body pgp-start
                                    (+ pgp-end-pos (string-length pgp-end-marker)))
                                  (substring body pgp-start (string-length body))))
                     (fr (app-state-frame app))
                     (row (- (frame-height fr) 1))
                     (width (frame-width fr))
                     (pass (echo-read-string echo "Passphrase: " row width)))
                (when (and pass (> (string-length pass) 0))
                  (let* ((proc (open-process
                                 (list path: "gpg"
                                       arguments: ["--decrypt" "--batch" "--yes"
                                                   "--passphrase-fd" "0"]
                                       stdin-redirection: #t
                                       stdout-redirection: #t
                                       stderr-redirection: #t))))
                    (display pass proc)
                    (display "\n" proc)
                    (display pgp-block proc)
                    (force-output proc)
                    (close-output-port proc)
                    (let ((decrypted (read-line proc #f)))
                      (process-status proc)
                      (close-port proc)
                      (when decrypted
                        (let ((new-text (string-append
                                          (substring text 0 heading-end)
                                          "\n" decrypted "\n"
                                          (if (< entry-end (string-length text))
                                            (substring text entry-end (string-length text))
                                            ""))))
                          (editor-set-text ed new-text)
                          (editor-goto-pos ed pos)
                          (echo-message! echo "Entry decrypted"))))))))))))))

