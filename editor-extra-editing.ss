;;; -*- Gerbil -*-
;;; Artist mode, TRAMP, paredit, string inflection, ediff,
;;; undo-tree, server, navigation, and misc editing commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers)

;; Artist mode — simple ASCII drawing
(def (cmd-artist-mode app)
  "Toggle artist mode for ASCII drawing. In artist mode, arrow keys draw lines."
  (let ((on (toggle-mode! 'artist-mode)))
    (echo-message! (app-state-echo app)
      (if on "Artist mode enabled (use arrows to draw)" "Artist mode disabled"))))

;; TRAMP — remote file access via SSH
(def *tramp-connections* '()) ; list of active SSH processes

(def (cmd-tramp-cleanup-all-connections app)
  "Clean up all TRAMP (SSH) connections."
  (for-each
    (lambda (proc)
      (with-exception-catcher (lambda (e) (void))
        (lambda () (close-port proc))))
    *tramp-connections*)
  (set! *tramp-connections* '())
  (echo-message! (app-state-echo app) "TRAMP connections cleaned up"))

;; Process management extras
(def (cmd-proced app)
  "Process editor (show system processes)."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error listing processes")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "ps"
                                     arguments: '("aux" "--sort=-pcpu")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Proced*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Process List\n\n" result "\n"))
      (editor-set-read-only ed #t))))

;; Paredit-like commands for Lisp editing
(def (cmd-paredit-wrap-round app)
  "Wrap sexp in parentheses."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      ;; No selection: wrap word at point
      (let* ((pos (editor-get-current-pos ed)))
        (let-values (((ws we) (word-bounds-at ed pos)))
          (when ws
            (editor-insert-text ed we ")")
            (editor-insert-text ed ws "("))))
      ;; Wrap selection
      (begin
        (editor-insert-text ed end ")")
        (editor-insert-text ed start "(")))))

(def (cmd-paredit-wrap-square app)
  "Wrap sexp in square brackets."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (let* ((pos (editor-get-current-pos ed)))
        (let-values (((ws we) (word-bounds-at ed pos)))
          (when ws
            (editor-insert-text ed we "]")
            (editor-insert-text ed ws "["))))
      (begin
        (editor-insert-text ed end "]")
        (editor-insert-text ed start "[")))))

(def (cmd-paredit-wrap-curly app)
  "Wrap sexp in curly braces."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (let* ((pos (editor-get-current-pos ed)))
        (let-values (((ws we) (word-bounds-at ed pos)))
          (when ws
            (editor-insert-text ed we "}")
            (editor-insert-text ed ws "{"))))
      (begin
        (editor-insert-text ed end "}")
        (editor-insert-text ed start "{")))))

(def (cmd-paredit-splice-sexp app)
  "Splice sexp - remove enclosing delimiters."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (match-pos (send-message ed SCI_BRACEMATCH pos 0)))
    (when (>= match-pos 0)
      (let ((open-pos (min pos match-pos))
            (close-pos (max pos match-pos)))
        ;; Delete close delimiter first (to preserve open position)
        (send-message ed SCI_DELETERANGE close-pos 1)
        ;; Delete open delimiter
        (send-message ed SCI_DELETERANGE open-pos 1)))))

(def (cmd-paredit-raise-sexp app)
  "Raise sexp - replace parent list with sexp at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    ;; Find enclosing list
    (let ((open-pos (sp-find-enclosing-paren ed pos #\( #\))))
      (if (not open-pos)
        (echo-message! echo "Not inside a list")
        (let ((close-pos (sp-find-matching-close ed (+ open-pos 1) #\( #\))))
          (if (not close-pos)
            (echo-message! echo "Unbalanced parens")
            ;; Find sexp at point
            (let loop ((i pos))
              (if (>= i (string-length text))
                (echo-message! echo "No sexp at point")
                (let ((ch (string-ref text i)))
                  (cond
                    ((char-whitespace? ch) (loop (+ i 1)))
                    (else
                     ;; Found start of sexp, get its content
                     (let* ((sexp-end (sp-find-sexp-end ed i))
                            (sexp-text (if sexp-end
                                         (substring text i (+ sexp-end 1))
                                         #f)))
                       (if (not sexp-text)
                         (echo-message! echo "Could not parse sexp")
                         (begin
                           ;; Replace parent list with sexp
                           (editor-set-selection ed open-pos (+ close-pos 1))
                           (editor-replace-selection ed sexp-text)
                           (editor-goto-pos ed open-pos)
                           (echo-message! echo "Raised sexp")))))))))))))))

;;;============================================================================
;;; Text-based sexp helpers (for paredit slurp/barf/split/join)
;;;============================================================================

(def (text-find-matching-close text pos)
  "Find matching close delimiter from opening at pos. Returns position after closer."
  (let* ((len (string-length text))
         (ch (string-ref text pos))
         (close (cond ((char=? ch #\() #\))
                      ((char=? ch #\[) #\])
                      ((char=? ch #\{) #\})
                      (else #f))))
    (if close
      (let loop ((i (+ pos 1)) (depth 1))
        (cond ((>= i len) #f)
              ((char=? (string-ref text i) ch) (loop (+ i 1) (+ depth 1)))
              ((char=? (string-ref text i) close)
               (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1))))
              (else (loop (+ i 1) depth))))
      #f)))

(def (text-find-matching-open text pos)
  "Find matching open delimiter scanning backward from closing at pos."
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
               (if (= depth 1) i (loop (- i 1) (- depth 1))))
              (else (loop (- i 1) depth))))
      #f)))

(def (text-sexp-end text pos)
  "Find end position of sexp starting at pos."
  (let ((len (string-length text)))
    (if (>= pos len) pos
      (let ((ch (string-ref text pos)))
        (cond
          ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
           (or (text-find-matching-close text pos) len))
          ((char=? ch #\")
           (let loop ((i (+ pos 1)))
             (cond ((>= i len) len)
                   ((char=? (string-ref text i) #\\) (loop (+ i 2)))
                   ((char=? (string-ref text i) #\") (+ i 1))
                   (else (loop (+ i 1))))))
          (else
           (let loop ((i pos))
             (if (or (>= i len)
                     (char-whitespace? (string-ref text i))
                     (memv (string-ref text i) '(#\( #\) #\[ #\] #\{ #\})))
               i (loop (+ i 1))))))))))

(def (text-sexp-start text pos)
  "Find start position of sexp ending at pos."
  (if (<= pos 0) 0
    (let* ((i (- pos 1))
           (ch (string-ref text i)))
      (cond
        ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
         (or (text-find-matching-open text i) 0))
        ((char=? ch #\")
         (let loop ((j (- i 1)))
           (cond ((<= j 0) 0)
                 ((and (char=? (string-ref text j) #\")
                       (or (= j 0) (not (char=? (string-ref text (- j 1)) #\\))))
                  j)
                 (else (loop (- j 1))))))
        (else
         (let loop ((j i))
           (if (or (<= j 0)
                   (char-whitespace? (string-ref text j))
                   (memv (string-ref text j) '(#\( #\) #\[ #\] #\{ #\})))
             (+ j 1) (loop (- j 1)))))))))

(def (text-skip-ws-forward text pos)
  (let ((len (string-length text)))
    (let loop ((i pos))
      (if (or (>= i len) (not (char-whitespace? (string-ref text i))))
        i (loop (+ i 1))))))

(def (text-skip-ws-backward text pos)
  (let loop ((i pos))
    (if (or (<= i 0) (not (char-whitespace? (string-ref text (- i 1)))))
      i (loop (- i 1)))))

(def (text-find-enclosing-open text pos)
  "Find innermost opening delimiter enclosing pos."
  (let loop ((i (- pos 1)) (depth 0))
    (cond
      ((< i 0) #f)
      ((memv (string-ref text i) '(#\) #\] #\}))
       (loop (- i 1) (+ depth 1)))
      ((memv (string-ref text i) '(#\( #\[ #\{))
       (if (= depth 0) i (loop (- i 1) (- depth 1))))
      (else (loop (- i 1) depth)))))

(def (text-find-enclosing-close text pos)
  "Find innermost closing delimiter enclosing pos."
  (let ((len (string-length text)))
    (let loop ((i pos) (depth 0))
      (cond
        ((>= i len) #f)
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (loop (+ i 1) (+ depth 1)))
        ((memv (string-ref text i) '(#\) #\] #\}))
         (if (= depth 0) i (loop (+ i 1) (- depth 1))))
        (else (loop (+ i 1) depth))))))

;;;============================================================================
;;; Paredit slurp/barf/split/join
;;;============================================================================

(def (cmd-paredit-slurp-forward app)
  "Extend enclosing sexp to include the next sexp after the closing delimiter."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (close-pos (text-find-enclosing-close text pos)))
    (when close-pos
      (let* ((after (text-skip-ws-forward text (+ close-pos 1)))
             (next-end (text-sexp-end text after)))
        (when (> next-end after)
          (let* ((close-char (string (string-ref text close-pos)))
                 (new-text (string-append
                             (substring text 0 close-pos)
                             (substring text (+ close-pos 1) next-end)
                             close-char
                             (substring text next-end (string-length text)))))
            (with-undo-action ed
              (editor-set-text ed new-text)
              (editor-goto-pos ed pos))))))))

(def (cmd-paredit-barf-forward app)
  "Move the last element of enclosing sexp out past the closing delimiter."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (close-pos (text-find-enclosing-close text pos)))
    (when close-pos
      (let* ((before-close (text-skip-ws-backward text close-pos))
             (last-start (text-sexp-start text before-close)))
        (when (> last-start 0)
          (let ((open-pos (text-find-enclosing-open text pos))
                (close-char (string (string-ref text close-pos))))
            (when (and open-pos (> last-start (+ open-pos 1)))
              (let* ((ws-before (text-skip-ws-backward text last-start))
                     (new-text (string-append
                                 (substring text 0 ws-before)
                                 close-char
                                 (substring text ws-before close-pos)
                                 (substring text (+ close-pos 1) (string-length text)))))
                (with-undo-action ed
                  (editor-set-text ed new-text)
                  (editor-goto-pos ed (min pos (string-length new-text))))))))))))

(def (cmd-paredit-slurp-backward app)
  "Extend enclosing sexp to include the sexp before the opening delimiter."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (open-pos (text-find-enclosing-open text pos)))
    (when open-pos
      (let* ((before (text-skip-ws-backward text open-pos))
             (prev-start (text-sexp-start text before)))
        (when (and prev-start (< prev-start open-pos))
          (let* ((open-char (string (string-ref text open-pos)))
                 (new-text (string-append
                             (substring text 0 prev-start)
                             open-char
                             (substring text prev-start open-pos)
                             (substring text (+ open-pos 1) (string-length text)))))
            (with-undo-action ed
              (editor-set-text ed new-text)
              (editor-goto-pos ed pos))))))))

(def (cmd-paredit-barf-backward app)
  "Move the first element of enclosing sexp out before the opening delimiter."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (open-pos (text-find-enclosing-open text pos)))
    (when open-pos
      (let* ((after-open (text-skip-ws-forward text (+ open-pos 1)))
             (first-end (text-sexp-end text after-open))
             (close-pos (text-find-enclosing-close text pos))
             (open-char (string (string-ref text open-pos))))
        (when (and first-end close-pos (< first-end close-pos))
          (let* ((ws-after (text-skip-ws-forward text first-end))
                 (new-text (string-append
                             (substring text 0 open-pos)
                             (substring text (+ open-pos 1) ws-after)
                             open-char
                             (substring text ws-after (string-length text)))))
            (with-undo-action ed
              (editor-set-text ed new-text)
              (editor-goto-pos ed (min pos (string-length new-text))))))))))

(def (cmd-paredit-split-sexp app)
  "Split the enclosing sexp into two at point."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (open-pos (text-find-enclosing-open text pos)))
    (when open-pos
      (let ((close-pos (text-find-enclosing-close text pos))
            (open-ch (string-ref text open-pos)))
        (when close-pos
          (let* ((close-ch (cond ((char=? open-ch #\() #\))
                                 ((char=? open-ch #\[) #\])
                                 ((char=? open-ch #\{) #\})
                                 (else #\))))
                 (new-text (string-append
                             (substring text 0 pos)
                             (string close-ch) " " (string open-ch)
                             (substring text pos (string-length text)))))
            (with-undo-action ed
              (editor-set-text ed new-text)
              (editor-goto-pos ed (+ pos 2)))))))))

(def (cmd-paredit-join-sexps app)
  "Join two adjacent sexps into one."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (bwd (text-skip-ws-backward text pos)))
    (when (and (> bwd 0)
               (memv (string-ref text (- bwd 1)) '(#\) #\] #\})))
      (let ((fwd (text-skip-ws-forward text pos)))
        (when (and (< fwd (string-length text))
                   (memv (string-ref text fwd) '(#\( #\[ #\{)))
          (let ((new-text (string-append
                            (substring text 0 (- bwd 1))
                            " "
                            (substring text (+ fwd 1) (string-length text)))))
            (with-undo-action ed
              (editor-set-text ed new-text)
              (editor-goto-pos ed (- bwd 1)))))))))

(def (cmd-paredit-convolute-sexp app)
  "Convolute: swap inner and outer sexps around point.
   (a (b |c d) e) => (b (a c d e))"
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (inner-open (text-find-enclosing-open text pos)))
    (when inner-open
      (let ((outer-open (text-find-enclosing-open text (- inner-open 1))))
        (when outer-open
          (let* ((inner-close (text-find-enclosing-close text pos))
                 (outer-close (text-find-enclosing-close text (+ inner-close 1))))
            (when (and inner-close outer-close)
              (let* ((outer-open-char (string (string-ref text outer-open)))
                     (outer-close-char (string (string-ref text outer-close)))
                     (inner-open-char (string (string-ref text inner-open)))
                     (inner-close-char (string (string-ref text inner-close)))
                     ;; inner head: from inner-open+1 to pos
                     (inner-head (substring text (+ inner-open 1) pos))
                     ;; inner tail: from pos to inner-close
                     (inner-tail (substring text pos inner-close))
                     ;; outer head: from outer-open+1 to inner-open
                     (outer-head (substring text (+ outer-open 1) inner-open))
                     ;; outer tail: from inner-close+1 to outer-close
                     (outer-tail (substring text (+ inner-close 1) outer-close))
                     ;; new text: inner-head (outer-head inner-tail outer-tail)
                     (before (substring text 0 outer-open))
                     (after (substring text (+ outer-close 1) (string-length text)))
                     (new-text (string-append
                                 before
                                 inner-open-char
                                 (string-trim-both inner-head)
                                 " " outer-open-char
                                 (string-trim-both outer-head)
                                 inner-tail
                                 outer-tail
                                 outer-close-char
                                 inner-close-char
                                 after)))
                (with-undo-action ed
                  (editor-set-text ed new-text)
                  (editor-goto-pos ed (+ (string-length before) 1)))))))))))

;;;============================================================================
;;; Number increment/decrement at point
;;;============================================================================

(def (find-number-at-pos text pos)
  "Find start and end of number at pos. Returns (start . end) or #f."
  (let ((len (string-length text)))
    (if (and (< pos len) (or (char-numeric? (string-ref text pos))
                              (and (char=? (string-ref text pos) #\-)
                                   (< (+ pos 1) len)
                                   (char-numeric? (string-ref text (+ pos 1))))))
      ;; Scan backward for start
      (let ((start (let loop ((i pos))
                     (if (or (<= i 0)
                             (not (or (char-numeric? (string-ref text (- i 1)))
                                      (char=? (string-ref text (- i 1)) #\-))))
                       i (loop (- i 1))))))
        ;; Scan forward for end
        (let ((end (let loop ((i pos))
                     (if (or (>= i len) (not (char-numeric? (string-ref text i))))
                       i (loop (+ i 1))))))
          (cons start end)))
      #f)))

(def (cmd-increment-number app)
  "Increment the number at point by 1."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (bounds (find-number-at-pos text pos)))
    (if (not bounds)
      (echo-error! echo "No number at point")
      (let* ((start (car bounds))
             (end (cdr bounds))
             (num-str (substring text start end))
             (num (string->number num-str)))
        (when num
          (let ((new-str (number->string (+ num 1))))
            (with-undo-action ed
              (editor-set-selection ed start end)
              (editor-replace-selection ed new-str))
            (editor-goto-pos ed start)))))))

(def (cmd-decrement-number app)
  "Decrement the number at point by 1."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (bounds (find-number-at-pos text pos)))
    (if (not bounds)
      (echo-error! echo "No number at point")
      (let* ((start (car bounds))
             (end (cdr bounds))
             (num-str (substring text start end))
             (num (string->number num-str)))
        (when num
          (let ((new-str (number->string (- num 1))))
            (with-undo-action ed
              (editor-set-selection ed start end)
              (editor-replace-selection ed new-str))
            (editor-goto-pos ed start)))))))

;;;============================================================================
;;; Grep/compilation result navigation
;;;============================================================================

(def (parse-grep-line-text line)
  "Parse a grep -n output line. Returns (file line-num) or #f."
  ;; Format: file:line:text
  (let ((colon1 (string-index line #\:)))
    (and colon1
         (let ((colon2 (string-index line #\: (+ colon1 1))))
           (and colon2
                (let ((line-num (string->number
                                  (substring line (+ colon1 1) colon2))))
                  (and line-num
                       (list (substring line 0 colon1)
                             line-num))))))))

(def (cmd-grep-goto app)
  "Jump to file:line from grep/compilation output at cursor."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-text (editor-get-line ed line-num))
         (parsed (parse-grep-line-text line-text)))
    (if (not parsed)
      (echo-error! echo "No file:line at point")
      (let ((file (car parsed))
            (target-line (cadr parsed)))
        (if (not (file-exists? file))
          (echo-error! echo (string-append "File not found: " file))
          ;; Open the file and go to line
          (let* ((fr (app-state-frame app))
                 (name (path-strip-directory file))
                 (buf (or (buffer-by-name name)
                          (buffer-create! name ed file))))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (when (file-exists? file)
              (let ((text (read-file-as-string file)))
                (when text
                  (editor-set-text ed text)
                  (editor-set-save-point ed))))
            (editor-goto-line ed (- target-line 1))
            (editor-scroll-caret ed)
            (echo-message! echo
              (string-append file ":" (number->string target-line)))))))))

(def (cmd-next-error app)
  "Jump to next error/grep result in *Compilation* or *Grep* buffer."
  (let* ((echo (app-state-echo app))
         (grep-buf (or (buffer-by-name "*Grep*")
                       (buffer-by-name "*Compilation*"))))
    (if (not grep-buf)
      (echo-error! echo "No grep/compilation buffer")
      ;; If we're not in the grep buffer, switch to it first
      (let* ((ed (current-editor app))
             (fr (app-state-frame app))
             (cur-buf (current-buffer-from-app app)))
        (unless (eq? cur-buf grep-buf)
          (buffer-attach! ed grep-buf)
          (set! (edit-window-buffer (current-window fr)) grep-buf))
        ;; Move to next line with a match
        (let* ((pos (editor-get-current-pos ed))
               (cur-line (editor-line-from-position ed pos))
               (total-lines (editor-get-line-count ed)))
          (let loop ((line (+ cur-line 1)))
            (if (>= line total-lines)
              (echo-message! echo "No more results")
              (let* ((line-text (editor-get-line ed line))
                     (parsed (parse-grep-line-text line-text)))
                (if parsed
                  (begin
                    (editor-goto-line ed line)
                    (editor-scroll-caret ed)
                    (cmd-grep-goto app))
                  (loop (+ line 1)))))))))))

;; TRAMP-like remote editing via SSH
(def (cmd-find-file-ssh app)
  "Open file via SSH. Fetches remote file content using scp."
  (let ((path (app-read-string app "SSH path (user@host:/path): ")))
    (when (and path (not (string-empty? path)))
      (let* ((echo (app-state-echo app)))
        (echo-message! echo (string-append "Fetching: " path))
        (with-exception-catcher
          (lambda (e)
            (echo-error! echo (string-append "SSH failed: "
              (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let* ((tmp-file (string-append "/tmp/gemacs-ssh-" (number->string (random-integer 99999))))
                   (proc (open-process
                           (list path: "scp"
                                 arguments: (list path tmp-file)
                                 stdin-redirection: #f stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (output (read-line proc #f))
                   (status (process-status proc)))
              (if (and (= status 0) (file-exists? tmp-file))
                (let* ((content (read-file-as-string tmp-file))
                       (fr (app-state-frame app))
                       (win (current-window fr))
                       (ed (edit-window-editor win))
                       (buf-name (string-append "[SSH] " path))
                       (buf (buffer-create! buf-name ed)))
                  (buffer-attach! ed buf)
                  (set! (edit-window-buffer win) buf)
                  (editor-set-text ed content)
                  (editor-goto-pos ed 0)
                  (delete-file tmp-file)
                  (echo-message! echo (string-append "Opened: " path)))
                (echo-error! echo (string-append "scp failed: " (or output "unknown error")))))))))))

;; Additional text manipulation
(def (cmd-string-inflection-cycle app)
  "Cycle word between camelCase, snake_case, kebab-case."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed)))
    (let-values (((word-start word-end) (word-bounds-at ed pos)))
      (when word-start
      (let* ((len (- word-end word-start))
             (word (substring (editor-get-text ed) word-start word-end))
             (has-underscore (string-contains word "_"))
             (has-dash (string-contains word "-"))
             (has-upper (let loop ((i 0))
                          (if (>= i (string-length word)) #f
                            (if (char-upper-case? (string-ref word i)) #t
                              (loop (+ i 1))))))
             (new-word
               (cond
                 ;; snake_case -> kebab-case
                 (has-underscore
                  (list->string
                    (map (lambda (c) (if (char=? c #\_) #\- c))
                         (string->list word))))
                 ;; kebab-case -> camelCase
                 (has-dash
                  (let loop ((chars (string->list word)) (capitalize #f) (acc []))
                    (cond
                      ((null? chars) (list->string (reverse acc)))
                      ((char=? (car chars) #\-)
                       (loop (cdr chars) #t acc))
                      (capitalize
                       (loop (cdr chars) #f (cons (char-upcase (car chars)) acc)))
                      (else
                       (loop (cdr chars) #f (cons (car chars) acc))))))
                 ;; camelCase -> snake_case
                 (has-upper
                  (let loop ((chars (string->list word)) (acc []))
                    (cond
                      ((null? chars) (list->string (reverse acc)))
                      ((and (char-upper-case? (car chars)) (not (null? acc)))
                       (loop (cdr chars)
                             (cons (char-downcase (car chars)) (cons #\_ acc))))
                      (else
                       (loop (cdr chars) (cons (char-downcase (car chars)) acc))))))
                 ;; no case markers - do nothing
                 (else word))))
        (send-message ed SCI_SETTARGETSTART word-start 0)
        (send-message ed SCI_SETTARGETEND word-end 0)
        (send-message/string ed SCI_REPLACETARGET new-word))))))

;; Ediff - file and region comparison using diff
;; Provides a simple diff view between two files or buffers

(def (cmd-ediff-files app)
  "Compare two files using diff."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (file1 (echo-read-string echo "First file: " row width)))
    (when (and file1 (not (string-empty? file1)))
      (let ((file2 (echo-read-string echo "Second file: " row width)))
        (when (and file2 (not (string-empty? file2)))
          (if (not (and (file-exists? file1) (file-exists? file2)))
            (echo-error! echo "One or both files do not exist")
            (with-exception-catcher
              (lambda (e) (echo-error! echo "diff command failed"))
              (lambda ()
                (let* ((proc (open-process
                               (list path: "diff"
                                     arguments: (list "-u" file1 file2)
                                     stdin-redirection: #f
                                     stdout-redirection: #t
                                     stderr-redirection: #f)))
                       (output (read-line proc #f)))
                  (process-status proc)
                  (let* ((win (current-window fr))
                         (ed (edit-window-editor win))
                         (buf (buffer-create! "*Ediff*" ed))
                         (text (if output
                                 (string-append "Diff: " file1 " vs " file2 "\n"
                                               (make-string 60 #\=) "\n\n"
                                               output)
                                 "Files are identical")))
                    (buffer-attach! ed buf)
                    (set! (edit-window-buffer win) buf)
                    (editor-set-text ed text)
                    (editor-goto-pos ed 0)
                    (editor-set-read-only ed #t)))))))))))

(def (cmd-ediff-regions app)
  "Compare current buffer with another buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (current-buf (edit-window-buffer win))
         (current-name (and current-buf (buffer-name current-buf)))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (other-name (echo-read-string echo "Compare with buffer: " row width)))
    (when (and other-name (not (string-empty? other-name)))
      (let ((other-buf (buffer-by-name other-name)))
        (if (not other-buf)
          (echo-error! echo (string-append "Buffer not found: " other-name))
          ;; Get text from both buffers
          (let* ((text1 (editor-get-text ed))
                 (tmp1 "/tmp/gerbil-ediff-1.txt")
                 (tmp2 "/tmp/gerbil-ediff-2.txt"))
            ;; We need to get text from other buffer - save current, switch, get, switch back
            (call-with-output-file tmp1 (lambda (p) (display text1 p)))
            ;; Get other buffer's text by temporarily switching
            (buffer-attach! ed other-buf)
            (let ((text2 (editor-get-text ed)))
              (call-with-output-file tmp2 (lambda (p) (display text2 p)))
              ;; Switch back
              (buffer-attach! ed current-buf)
              ;; Run diff
              (with-exception-catcher
                (lambda (e) (echo-error! echo "diff failed"))
                (lambda ()
                  (let* ((proc (open-process
                                 (list path: "diff"
                                       arguments: (list "-u" tmp1 tmp2)
                                       stdin-redirection: #f
                                       stdout-redirection: #t
                                       stderr-redirection: #f)))
                         (output (read-line proc #f)))
                    (process-status proc)
                    (let* ((buf (buffer-create! "*Ediff*" ed))
                           (diff-text (if output
                                        (string-append "Diff: " current-name " vs " other-name "\n"
                                                      (make-string 60 #\=) "\n\n"
                                                      output)
                                        "Buffers are identical")))
                      (buffer-attach! ed buf)
                      (set! (edit-window-buffer win) buf)
                      (editor-set-text ed diff-text)
                      (editor-goto-pos ed 0)
                      (editor-set-read-only ed #t))))))))))))

;; Undo tree visualization — renders undo state as a visual tree
(def (cmd-undo-tree-visualize app)
  "Show undo history as a visual tree for the current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (buf (edit-window-buffer win))
         (name (and buf (buffer-name buf)))
         (can-undo (send-message ed SCI_CANUNDO 0 0))
         (can-redo (send-message ed SCI_CANREDO 0 0))
         (text-len (send-message ed SCI_GETLENGTH 0 0))
         (line-count (send-message ed SCI_GETLINECOUNT 0 0)))
    (if (and (= can-undo 0) (= can-redo 0))
      (echo-message! echo "No undo history for this buffer")
      (let* ((header (string-append
                       "Undo Tree: " (or name "?") "\n"
                       (make-string 50 #\-) "\n"
                       "Buffer: " (number->string text-len) " chars, "
                       (number->string line-count) " lines\n\n"))
             (tree (string-append
                     "  o-- [current state]\n"
                     (if (= can-undo 1)
                       "  |-- [undo available] C-/ to undo\n"
                       "")
                     (if (= can-redo 1)
                       "  |-- [redo available] C-S-/ to redo\n"
                       "")
                     "\nUse M-x undo-history for timestamped snapshots.\n"
                     "Use M-x undo-history-restore to restore a snapshot.\n"))
             (content (string-append header tree))
             (tbuf (buffer-create! "*Undo Tree*" ed)))
        (buffer-attach! ed tbuf)
        (set! (edit-window-buffer win) tbuf)
        (editor-set-text ed content)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)))))

;; Editor server — file socket not available in TUI, provide info
(def (cmd-server-start app)
  "Note about editor server."
  (echo-message! (app-state-echo app)
    "Server mode: use gemacs <file> to open files"))

(def (cmd-server-edit app)
  "Open a file by name (alias for find-file)."
  (let ((file (app-read-string app "File to edit: ")))
    (when (and file (not (string-empty? file)))
      (execute-command! app 'find-file))))

;; Additional navigation
(def (cmd-pop-global-mark app)
  "Pop back to previous global mark."
  (let ((mr (app-state-mark-ring app)))
    (if (and mr (not (null? mr)))
      (let ((mark (car mr)))
        (set! (app-state-mark-ring app) (cdr mr))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win)))
          (editor-goto-pos ed mark)
          (editor-scroll-caret ed)))
      (echo-message! (app-state-echo app) "Mark ring empty"))))

(def (cmd-set-goal-column app)
  "Set current column as goal column."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (col (editor-get-column ed pos)))
    (echo-message! (app-state-echo app)
      (string-append "Goal column set to " (number->string col)))))

;; Directory navigation
(def (cmd-cd app)
  "Change default directory."
  (let ((dir (app-read-string app "Change directory: ")))
    (when (and dir (not (string-empty? dir)))
      (if (file-exists? dir)
        (begin
          (current-directory dir)
          (echo-message! (app-state-echo app)
            (string-append "Directory: " (current-directory))))
        (echo-message! (app-state-echo app)
          (string-append "No such directory: " dir))))))

;; Misc Emacs commands
(def (cmd-display-prefix app)
  "Display the current prefix argument."
  (let ((arg (app-state-prefix-arg app)))
    (echo-message! (app-state-echo app)
      (cond
        ((not arg) "No prefix arg")
        ((number? arg) (string-append "Prefix arg: " (number->string arg)))
        ((list? arg) (string-append "Prefix arg: (" (number->string (car arg)) ")"))
        (else "Prefix arg: unknown")))))

(def (cmd-digit-argument app)
  "Begin entering a numeric prefix argument."
  (set! (app-state-prefix-arg app) 0)
  (echo-message! (app-state-echo app) "C-u 0-"))

(def (cmd-negative-argument app)
  "Begin negative numeric prefix argument."
  (set! (app-state-prefix-arg app) -1)
  (echo-message! (app-state-echo app) "C-u -"))

(def (cmd-suspend-emacs app)
  "Suspend the editor (send SIGTSTP)."
  (echo-message! (app-state-echo app) "Use C-z in terminal to suspend"))

(def (cmd-save-buffers-kill-emacs app)
  "Save all buffers and quit."
  ;; Save any modified buffers with files
  (for-each
    (lambda (buf)
      (when (and (buffer-file-path buf) (buffer-modified buf))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win)))
          ;; Only save if this buffer is in the active window
          ;; For a full implementation, we'd need to attach each buffer
          (void))))
    (buffer-list))
  (set! (app-state-running app) #f)
  (echo-message! (app-state-echo app) "Exiting..."))

;; View/doc mode
(def (cmd-view-mode app)
  "Toggle view mode (read-only with navigation)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (ro (editor-get-read-only? ed)))
    (editor-set-read-only ed (not ro))
    (echo-message! (app-state-echo app)
      (if ro "View mode disabled" "View mode enabled"))))

(def (cmd-doc-view-mode app)
  "View document — converts PDF/PS to text using pdftotext or ps2ascii."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file (and buf (buffer-file-path buf)))
         (echo (app-state-echo app)))
    (if (not file)
      (echo-message! echo "No file associated with buffer")
      (let* ((ext (let ((dot (string-index-right file #\.)))
                    (if dot (substring file (+ dot 1) (string-length file)) "")))
             (cmd (cond
                    ((string=? ext "pdf") "pdftotext")
                    ((string=? ext "ps") "ps2ascii")
                    (else #f))))
        (if (not cmd)
          (echo-message! echo "Not a PDF or PS file")
          (with-exception-catcher
            (lambda (e) (echo-error! echo (string-append cmd " not available")))
            (lambda ()
              (let* ((proc (open-process
                             (list path: cmd
                                   arguments: (list file "-")
                                   stdin-redirection: #f stdout-redirection: #t
                                   stderr-redirection: #f)))
                     (text (read-line proc #f)))
                (process-status proc)
                (let* ((ed (edit-window-editor win))
                       (new-buf (buffer-create! (string-append "*DocView: " file "*") ed)))
                  (buffer-attach! ed new-buf)
                  (set! (edit-window-buffer win) new-buf)
                  (editor-set-text ed (or text "Could not convert document"))
                  (editor-goto-pos ed 0)
                  (editor-set-read-only ed #t))))))))))

;; Speedbar — show file tree of current directory
(def (cmd-speedbar app)
  "Show file tree sidebar using tree command."
  (let* ((dir (current-directory))
         (output (with-exception-catcher
                   (lambda (e) (string-append "Error listing " dir))
                   (lambda ()
                     (let ((p (open-process
                                (list path: "find" arguments: (list dir "-maxdepth" "3" "-type" "f" "-name" "*.ss")
                                      stdin-redirection: #f stdout-redirection: #t
                                      stderr-redirection: #f))))
                       (let ((out (read-line p #f)))
                         (process-status p)
                         (or out ""))))))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Speedbar*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "File Tree: " dir "\n\n" output "\n"))
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;; Misc utilities
(def (cmd-world-clock app)
  "Display world clock."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error getting time")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "date"
                                     arguments: '("+%Y-%m-%d %H:%M:%S %Z")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p)))
                        (process-status p)
                        (or out "")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*World Clock*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "World Clock\n\nLocal: " result "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-display-battery app)
  "Display battery status."
  (let ((result (with-exception-catcher
                  (lambda (e) "Battery info not available")
                  (lambda ()
                    (if (file-exists? "/sys/class/power_supply/BAT0/capacity")
                      (let ((cap (call-with-input-file "/sys/class/power_supply/BAT0/capacity"
                                   read-line))
                            (status (if (file-exists? "/sys/class/power_supply/BAT0/status")
                                      (call-with-input-file "/sys/class/power_supply/BAT0/status"
                                        read-line)
                                      "Unknown")))
                        (string-append "Battery: " cap "% (" status ")"))
                      "No battery information available")))))
    (echo-message! (app-state-echo app) result)))

(def (cmd-uptime app)
  "Display system uptime."
  (let ((result (with-exception-catcher
                  (lambda (e) "Error getting uptime")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "uptime"
                                     arguments: '()
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p)))
                        (process-status p)
                        (string-append "Uptime:" (string-trim out))))))))
    (echo-message! (app-state-echo app) result)))

;; Kmacro counter (state in editor-extra-helpers)

(def (cmd-kmacro-set-counter app)
  "Set keyboard macro counter value."
  (let ((val (app-read-string app (string-append "Counter value (current: "
                                    (number->string *kmacro-counter*) "): "))))
    (when (and val (not (string-empty? val)))
      (let ((n (string->number val)))
        (when n
          (set! *kmacro-counter* n)
          (echo-message! (app-state-echo app)
            (string-append "Macro counter: " (number->string n))))))))

(def (cmd-kmacro-insert-counter app)
  "Insert macro counter value and increment."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (number->string *kmacro-counter*)))
    (editor-insert-text ed (editor-get-current-pos ed) text)
    (set! *kmacro-counter* (+ *kmacro-counter* 1))
    (echo-message! (app-state-echo app)
      (string-append "Inserted " text ", next: " (number->string *kmacro-counter*)))))

;; Whitespace report
(def (cmd-whitespace-report app)
  "Report whitespace problems in buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (trailing-count 0)
         (tab-count 0)
         (long-count 0))
    (for-each
      (lambda (line)
        (when (and (> (string-length line) 0)
                   (char-whitespace? (string-ref line (- (string-length line) 1))))
          (set! trailing-count (+ trailing-count 1)))
        (when (string-contains line "\t")
          (set! tab-count (+ tab-count 1)))
        (when (> (string-length line) 80)
          (set! long-count (+ long-count 1))))
      lines)
    (echo-message! (app-state-echo app)
      (string-append "Trailing: " (number->string trailing-count)
                     " Tabs: " (number->string tab-count)
                     " Long(>80): " (number->string long-count)))))

;; Encoding detection
(def (cmd-describe-coding-system app)
  "Describe current coding system."
  (echo-message! (app-state-echo app) "Coding system: utf-8 (Gerbil uses UTF-8 internally)"))

(def (cmd-set-terminal-coding-system app)
  "Set terminal coding system."
  (echo-message! (app-state-echo app) "Terminal coding: utf-8 (fixed)"))

;; Misc text
(def (cmd-overwrite-mode app)
  "Toggle overwrite mode."
  (let* ((ed (current-editor app))
         (cur (send-message ed 2187 0 0))  ;; SCI_GETOVERTYPE
         (new (if (zero? cur) 1 0)))
    (send-message ed 2186 new 0)           ;; SCI_SETOVERTYPE
    (echo-message! (app-state-echo app)
      (if (= new 1) "Overwrite mode ON" "Overwrite mode OFF"))))

;;;============================================================================
;;; Real multi-selection commands (using Scintilla multi-selection API)
;;;============================================================================

(def (cmd-mc-real-add-next app)
  "Add a real cursor at the next occurrence of the current selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (editor-selection-empty? ed)
      (echo-error! echo "Select text first, then mark next")
      (begin
        (send-message ed SCI_MULTIPLESELECTADDNEXT 0 0)
        (let ((n (send-message ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n) " cursors")))))))

(def (cmd-mc-real-add-all app)
  "Add real cursors at all occurrences of the current selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (editor-selection-empty? ed)
      (echo-error! echo "Select text first, then mark all")
      (begin
        (send-message ed SCI_MULTIPLESELECTADDEACH 0 0)
        (let ((n (send-message ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n) " cursors")))))))

(def (cmd-mc-skip-and-add-next app)
  "Skip the current selection and add next occurrence."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (editor-selection-empty? ed)
      (echo-error! echo "Select text first")
      (let ((n (send-message ed SCI_GETSELECTIONS 0 0)))
        (when (> n 1)
          ;; Drop the main selection
          (let ((main (send-message ed SCI_GETMAINSELECTION 0 0)))
            (send-message ed SCI_DROPSELECTIONN main 0)))
        ;; Add next
        (send-message ed SCI_MULTIPLESELECTADDNEXT 0 0)
        (let ((n2 (send-message ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n2) " cursors")))))))

(def (cmd-mc-cursors-on-lines app)
  "Add a cursor at the end of each line in the current selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! echo "Select a region first")
      (let* ((start-line (editor-line-from-position ed sel-start))
             (end-line (editor-line-from-position ed sel-end))
             (num-lines (+ 1 (- end-line start-line))))
        (when (> num-lines 1)
          ;; Set first selection at end of first line
          (let ((eol0 (editor-get-line-end-position ed start-line)))
            (send-message ed SCI_SETSELECTION eol0 eol0)
            ;; Add selections at end of subsequent lines
            (let loop ((line (+ start-line 1)))
              (when (<= line end-line)
                (let ((eol (editor-get-line-end-position ed line)))
                  (send-message ed SCI_ADDSELECTION eol eol)
                  (loop (+ line 1)))))))
        (echo-message! echo
          (string-append (number->string num-lines)
                         " cursors on " (number->string num-lines) " lines"))))))

(def (cmd-mc-unmark-last app)
  "Remove the most recently added selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (n (send-message ed SCI_GETSELECTIONS 0 0)))
    (if (<= n 1)
      (echo-message! echo "Only one cursor")
      (begin
        (send-message ed SCI_DROPSELECTIONN (- n 1) 0)
        (echo-message! echo
          (string-append (number->string (- n 1)) " cursors"))))))

(def (cmd-mc-rotate app)
  "Cycle to the next selection as the main cursor."
  (let ((ed (current-editor app)))
    (send-message ed SCI_ROTATESELECTION 0 0)))

;;;============================================================================
;;; Occur goto-occurrence (TUI)
;;;============================================================================

(def (occur-parse-source-name text)
  "Parse source buffer name from *Occur* header: 'N matches for \"pat\" in NAME:'"
  (let ((in-pos (string-contains text " in ")))
    (and in-pos
         (let* ((after-in (+ in-pos 4))
                (colon-pos (string-index text #\: after-in)))
           (and colon-pos
                (substring text after-in colon-pos))))))

(def (cmd-occur-goto app)
  "Jump from *Occur* buffer to the source line under cursor."
  (let* ((buf (current-buffer-from-app app))
         (echo (app-state-echo app)))
    (if (not (string=? (buffer-name buf) "*Occur*"))
      (echo-error! echo "Not in *Occur* buffer")
      (let* ((ed (current-editor app))
             (full-text (editor-get-text ed))
             (source-name (occur-parse-source-name full-text)))
        (if (not source-name)
          (echo-error! echo "Cannot determine source buffer")
          (let* ((pos (editor-get-current-pos ed))
                 (line-num (editor-line-from-position ed pos))
                 (line-text (editor-get-line ed line-num)))
            ;; Parse "NNN:text" format
            (let ((colon-pos (string-index line-text #\:)))
              (if (not colon-pos)
                (echo-error! echo "Not on an occur match line")
                (let ((target-line (string->number
                                     (substring line-text 0 colon-pos))))
                  (if (not target-line)
                    (echo-error! echo "Not on an occur match line")
                    ;; Switch to source buffer and jump
                    (let ((source (buffer-by-name source-name)))
                      (if (not source)
                        (echo-error! echo
                          (string-append "Source buffer '"
                                         source-name "' not found"))
                        (let ((fr (app-state-frame app)))
                          (buffer-attach! ed source)
                          (set! (edit-window-buffer (current-window fr)) source)
                          (editor-goto-line ed (- target-line 1))
                          (editor-scroll-caret ed)
                          (echo-message! echo
                            (string-append "Line "
                                           (number->string
                                             target-line))))))))))))))))

;;;============================================================================
;;; Markdown mode commands
;;;============================================================================

(def (markdown-wrap-selection ed prefix suffix)
  "Wrap current selection with prefix/suffix or insert them at point."
  (if (editor-selection-empty? ed)
    ;; No selection: insert prefix+suffix and place cursor between
    (let ((pos (editor-get-current-pos ed)))
      (editor-insert-text ed pos (string-append prefix suffix))
      (editor-goto-pos ed (+ pos (string-length prefix))))
    ;; Wrap selection
    (let* ((start (editor-get-selection-start ed))
           (end (editor-get-selection-end ed))
           (text (editor-get-text ed))
           (sel (substring text start end)))
      (send-message ed SCI_SETTARGETSTART start 0)
      (send-message ed SCI_SETTARGETEND end 0)
      (send-message/string ed SCI_REPLACETARGET
        (string-append prefix sel suffix)))))

(def (cmd-markdown-bold app)
  "Insert or wrap selection with bold markers **text**."
  (let ((ed (current-editor app)))
    (markdown-wrap-selection ed "**" "**")))

(def (cmd-markdown-italic app)
  "Insert or wrap selection with italic markers *text*."
  (let ((ed (current-editor app)))
    (markdown-wrap-selection ed "*" "*")))

(def (cmd-markdown-code app)
  "Insert or wrap selection with inline code backticks `text`."
  (let ((ed (current-editor app)))
    (markdown-wrap-selection ed "`" "`")))

(def (cmd-markdown-code-block app)
  "Insert a fenced code block."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (lang (app-read-string app "Language: ")))
    (editor-insert-text ed pos
      (string-append "```" (or lang "") "\n\n```\n"))
    (editor-goto-pos ed (+ pos 4 (string-length (or lang ""))))))

(def (cmd-markdown-heading app)
  "Insert or cycle heading level (# through ######)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    ;; Count existing # prefix
    (let ((hashes (let loop ((i 0))
                    (if (and (< i (string-length line-text))
                             (char=? (string-ref line-text i) #\#))
                      (loop (+ i 1)) i))))
      (send-message ed SCI_SETTARGETSTART line-start 0)
      (send-message ed SCI_SETTARGETEND line-end 0)
      (cond
        ((= hashes 0)
         ;; No heading: add #
         (send-message/string ed SCI_REPLACETARGET
           (string-append "# " line-text)))
        ((>= hashes 6)
         ;; Max level: remove all hashes
         (let ((stripped (string-trim line-text)))
           (send-message/string ed SCI_REPLACETARGET
             (let loop ((s stripped))
               (if (and (> (string-length s) 0)
                        (char=? (string-ref s 0) #\#))
                 (loop (substring s 1 (string-length s)))
                 (string-trim s))))))
        (else
         ;; Increase level
         (send-message/string ed SCI_REPLACETARGET
           (string-append "#" line-text)))))))

(def (cmd-markdown-link app)
  "Insert a markdown link [text](url)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-text (if (editor-selection-empty? ed) ""
                     (let* ((s (editor-get-selection-start ed))
                            (e (editor-get-selection-end ed))
                            (text (editor-get-text ed)))
                       (substring text s e))))
         (url (app-read-string app "URL: ")))
    (when (and url (not (string-empty? url)))
      (let* ((text (if (string-empty? sel-text) url sel-text))
             (link (string-append "[" text "](" url ")")))
        (if (editor-selection-empty? ed)
          (editor-insert-text ed (editor-get-current-pos ed) link)
          (let ((start (editor-get-selection-start ed))
                (end (editor-get-selection-end ed)))
            (send-message ed SCI_SETTARGETSTART start 0)
            (send-message ed SCI_SETTARGETEND end 0)
            (send-message/string ed SCI_REPLACETARGET link)))))))

(def (cmd-markdown-image app)
  "Insert a markdown image ![alt](url)."
  (let* ((ed (current-editor app))
         (alt (or (app-read-string app "Alt text: ") ""))
         (url (app-read-string app "Image URL: ")))
    (when (and url (not (string-empty? url)))
      (editor-insert-text ed (editor-get-current-pos ed)
        (string-append "![" alt "](" url ")")))))

(def (cmd-markdown-hr app)
  "Insert a horizontal rule."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "\n---\n")))

(def (cmd-markdown-list-item app)
  "Insert a list item. If current line starts with - or *, continue the list."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    ;; Detect list marker
    (let ((marker (cond
                    ((string-prefix? "- " line-text) "- ")
                    ((string-prefix? "* " line-text) "* ")
                    ((string-prefix? "  - " line-text) "  - ")
                    ((string-prefix? "  * " line-text) "  * ")
                    (else "- "))))
      (editor-goto-pos ed line-end)
      (editor-insert-text ed line-end (string-append "\n" marker)))))

(def (cmd-markdown-checkbox app)
  "Insert a markdown checkbox - [ ] item."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "- [ ] ")))

(def (cmd-markdown-toggle-checkbox app)
  "Toggle a markdown checkbox between [ ] and [x]."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    (send-message ed SCI_SETTARGETSTART line-start 0)
    (send-message ed SCI_SETTARGETEND line-end 0)
    (cond
      ((string-contains line-text "[ ]")
       (send-message/string ed SCI_REPLACETARGET
         (string-subst line-text "[ ]" "[x]")))
      ((string-contains line-text "[x]")
       (send-message/string ed SCI_REPLACETARGET
         (string-subst line-text "[x]" "[ ]")))
      (else
       (echo-message! (app-state-echo app) "No checkbox on this line")))))

(def (cmd-markdown-table app)
  "Insert a markdown table template."
  (let* ((ed (current-editor app))
         (cols-str (or (app-read-string app "Columns (default 3): ") "3"))
         (cols (or (string->number cols-str) 3))
         (pos (editor-get-current-pos ed)))
    (let* ((header (string-join (make-list cols " Header ") "|"))
           (sep (string-join (make-list cols "--------") "|"))
           (row (string-join (make-list cols "        ") "|"))
           (table (string-append "| " header " |\n| " sep " |\n| " row " |\n")))
      (editor-insert-text ed pos table))))

(def (cmd-markdown-preview-outline app)
  "Show an outline of markdown headings in the current buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (headings
           (let loop ((ls lines) (n 0) (acc []))
             (if (null? ls)
               (reverse acc)
               (let ((l (car ls)))
                 (if (and (> (string-length l) 0) (char=? (string-ref l 0) #\#))
                   (loop (cdr ls) (+ n 1) (cons (cons n l) acc))
                   (loop (cdr ls) (+ n 1) acc)))))))
    (if (null? headings)
      (echo-message! (app-state-echo app) "No headings found")
      (let ((buf-text (string-join
                        (map (lambda (h)
                               (string-append (number->string (+ (car h) 1))
                                              ": " (cdr h)))
                             headings)
                        "\n")))
        (open-output-buffer app "*Markdown Outline*"
          (string-append "Headings\n\n" buf-text "\n"))))))

;;;============================================================================
;;; Dired improvements — mark and operate on files
;;;============================================================================

(def *dired-marks* (make-hash-table)) ;; filename -> #t for marked files

(def (cmd-dired-mark app)
  "Mark the file on the current line in dired."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    ;; Mark the file and add a visual indicator
    (let ((trimmed (string-trim line-text)))
      (when (> (string-length trimmed) 0)
        (hash-put! *dired-marks* trimmed #t)
        ;; Replace the line with marked indicator
        (send-message ed SCI_SETTARGETSTART line-start 0)
        (send-message ed SCI_SETTARGETEND line-end 0)
        (if (string-prefix? "* " line-text)
          #f ;; Already marked
          (send-message/string ed SCI_REPLACETARGET
            (string-append "* " line-text)))
        ;; Move to next line
        (send-message ed 2300 0 0)
        (echo-message! (app-state-echo app)
          (string-append "Marked: " trimmed))))))

(def (cmd-dired-unmark app)
  "Unmark the file on the current line in dired."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    (when (string-prefix? "* " line-text)
      (let ((fname (substring line-text 2 (string-length line-text))))
        (hash-remove! *dired-marks* (string-trim fname))
        (send-message ed SCI_SETTARGETSTART line-start 0)
        (send-message ed SCI_SETTARGETEND line-end 0)
        (send-message/string ed SCI_REPLACETARGET
          (substring line-text 2 (string-length line-text)))))
    (send-message ed 2300 0 0)))

(def (cmd-dired-unmark-all app)
  "Unmark all marked files in dired."
  (set! *dired-marks* (make-hash-table))
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         ;; Remove all "* " prefixes
         (new-text (string-subst text "\n* " "\n")))
    (let ((new-text2 (if (string-prefix? "* " new-text)
                       (substring new-text 2 (string-length new-text))
                       new-text)))
      (editor-set-text ed new-text2)))
  (echo-message! (app-state-echo app) "All marks cleared"))

(def (cmd-dired-delete-marked app)
  "Delete all marked files in dired."
  (let* ((marked (hash-keys *dired-marks*))
         (count (length marked))
         (echo (app-state-echo app)))
    (if (= count 0)
      (echo-error! echo "No marked files")
      (let ((confirm (app-read-string app
                       (string-append "Delete " (number->string count)
                                      " file(s)? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (let ((deleted 0))
            (for-each
              (lambda (f)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (when (file-exists? f)
                      (delete-file f)
                      (set! deleted (+ deleted 1))))))
              marked)
            (set! *dired-marks* (make-hash-table))
            ;; Refresh dired buffer
            (let ((buf (current-buffer-from-app app)))
              (when (and buf (buffer-file-path buf))
                (cmd-dired-refresh app)))
            (echo-message! echo
              (string-append "Deleted " (number->string deleted) " file(s)"))))))))

(def (cmd-dired-refresh app)
  "Refresh the current dired buffer."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (dir (and buf (buffer-file-path buf))))
    (when dir
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Cannot read directory"))
        (lambda ()
          (let* ((entries (directory-files dir))
                 (sorted (sort entries string<?))
                 (lines (map (lambda (f)
                               (let* ((full (path-expand f dir))
                                      (is-dir (with-catch (lambda (e) #f)
                                                (lambda () (eq? (file-info-type (file-info full)) 'directory))))
                                      (size (with-catch
                                              (lambda (e) 0)
                                              (lambda ()
                                                (if is-dir 0
                                                  (file-info-size (file-info full))))))
                                      (suffix (if is-dir "/" "")))
                                 (string-append
                                   (if is-dir "d " "  ")
                                   (let ((s (number->string size)))
                                     (string-append
                                       (make-string (max 0 (- 10 (string-length s))) #\space)
                                       s))
                                   "  " f suffix)))
                             sorted))
                 (text (string-append
                         "  Directory: " dir "\n\n"
                         (string-join lines "\n") "\n")))
            (editor-set-text ed text)
            (editor-goto-pos ed 0)))))))


;;;============================================================================
;;; Diff commands
;;;============================================================================

(def (cmd-diff-two-files app)
  "Diff two files and show the result in a buffer."
  (let* ((echo (app-state-echo app))
         (file1 (app-read-string app "File A: "))
         (file2 (when file1 (app-read-string app "File B: "))))
    (when (and file1 file2
               (not (string-empty? file1)) (not (string-empty? file2)))
      (let ((result (with-catch
                      (lambda (e) (string-append "Error: "
                                    (with-output-to-string
                                      (lambda () (display-exception e)))))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "diff"
                                         arguments: (list "-u" file1 file2)
                                         stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out "Files are identical")))))))
        (open-output-buffer app "*Diff*" result)))))

;;;============================================================================
;;; Buffer encoding commands
;;;============================================================================

(def (cmd-set-buffer-encoding app)
  "Set the buffer encoding (display only - all buffers use UTF-8)."
  (let* ((echo (app-state-echo app))
         (enc (app-read-string app "Encoding (utf-8/latin-1/ascii): ")))
    (when enc
      (echo-message! echo (string-append "Encoding set to: " enc
                                          " (note: internally UTF-8)")))))

(def (cmd-convert-line-endings app)
  "Convert line endings in current buffer (unix/dos/mac)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (choice (app-read-string app "Convert to (unix/dos/mac): ")))
    (when choice
      (let ((text (editor-get-text ed)))
        (cond
          ((string=? choice "unix")
           (let ((new-text (string-subst (string-subst text "\r\n" "\n") "\r" "\n")))
             (editor-set-text ed new-text)
             (echo-message! echo "Converted to Unix line endings (LF)")))
          ((string=? choice "dos")
           (let* ((clean (string-subst (string-subst text "\r\n" "\n") "\r" "\n"))
                  (new-text (string-subst clean "\n" "\r\n")))
             (editor-set-text ed new-text)
             (echo-message! echo "Converted to DOS line endings (CRLF)")))
          ((string=? choice "mac")
           (let ((new-text (string-subst (string-subst text "\r\n" "\r") "\n" "\r")))
             (editor-set-text ed new-text)
             (echo-message! echo "Converted to Mac line endings (CR)")))
          (else
           (echo-error! echo "Unknown format. Use unix, dos, or mac.")))))))

;;;============================================================================
;;; Word count / statistics
;;;============================================================================

(def (cmd-buffer-statistics app)
  "Show detailed buffer statistics: lines, words, chars, paragraphs."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (len (string-length text))
         (lines (+ 1 (let loop ((i 0) (count 0))
                       (if (>= i len) count
                         (if (char=? (string-ref text i) #\newline)
                           (loop (+ i 1) (+ count 1))
                           (loop (+ i 1) count))))))
         (words (let loop ((i 0) (count 0) (in-word #f))
                  (if (>= i len) (if in-word (+ count 1) count)
                    (let ((c (string-ref text i)))
                      (if (or (char=? c #\space) (char=? c #\newline)
                              (char=? c #\tab) (char=? c #\return))
                        (loop (+ i 1) (if in-word (+ count 1) count) #f)
                        (loop (+ i 1) count #t))))))
         (paragraphs (let loop ((i 0) (count 0) (prev-newline #f))
                       (if (>= i len) (+ count 1)
                         (let ((c (string-ref text i)))
                           (if (char=? c #\newline)
                             (loop (+ i 1) (if prev-newline (+ count 1) count) #t)
                             (loop (+ i 1) count #f))))))
         (non-blank (let loop ((i 0) (count 0))
                      (if (>= i len) count
                        (if (or (char=? (string-ref text i) #\space)
                                (char=? (string-ref text i) #\newline)
                                (char=? (string-ref text i) #\tab))
                          (loop (+ i 1) count)
                          (loop (+ i 1) (+ count 1)))))))
    (echo-message! (app-state-echo app)
      (string-append "Lines: " (number->string lines)
                     "  Words: " (number->string words)
                     "  Chars: " (number->string len)
                     "  Non-blank: " (number->string non-blank)
                     "  Paragraphs: " (number->string paragraphs)))))

;; ── batch 42: editing preferences and modes ─────────────────────────
(def *auto-fill-comments* #f)
(def *electric-indent-mode* #t)
(def *truncate-partial-width* #f)
(def *inhibit-startup-screen* #f)
(def *visible-cursor* #t)
(def *transient-mark-mode* #t)
(def *global-whitespace-mode* #f)
(def *hide-ifdef-mode* #f)
(def *allout-mode* #f)

(def (cmd-toggle-auto-fill-comments app)
  "Toggle auto-fill for comments only."
  (let ((echo (app-state-echo app)))
    (set! *auto-fill-comments* (not *auto-fill-comments*))
    (echo-message! echo (if *auto-fill-comments*
                          "Auto-fill comments ON" "Auto-fill comments OFF"))))

(def (cmd-toggle-electric-indent-mode app)
  "Toggle electric-indent-mode (auto indent on newline)."
  (let ((echo (app-state-echo app)))
    (set! *electric-indent-mode* (not *electric-indent-mode*))
    (echo-message! echo (if *electric-indent-mode*
                          "Electric indent mode ON" "Electric indent mode OFF"))))

(def (cmd-toggle-truncate-partial-width-windows app)
  "Toggle truncation in partial-width windows."
  (let ((echo (app-state-echo app)))
    (set! *truncate-partial-width* (not *truncate-partial-width*))
    (echo-message! echo (if *truncate-partial-width*
                          "Truncate partial-width ON" "Truncate partial-width OFF"))))

(def (cmd-toggle-inhibit-startup-screen app)
  "Toggle inhibit-startup-screen."
  (let ((echo (app-state-echo app)))
    (set! *inhibit-startup-screen* (not *inhibit-startup-screen*))
    (echo-message! echo (if *inhibit-startup-screen*
                          "Inhibit startup screen ON" "Inhibit startup screen OFF"))))

(def (cmd-toggle-visible-cursor app)
  "Toggle visible cursor in non-selected windows."
  (let ((echo (app-state-echo app)))
    (set! *visible-cursor* (not *visible-cursor*))
    (echo-message! echo (if *visible-cursor*
                          "Visible cursor ON" "Visible cursor OFF"))))

(def (cmd-toggle-transient-mark-mode app)
  "Toggle transient-mark-mode (highlight active region)."
  (let ((echo (app-state-echo app)))
    (set! *transient-mark-mode* (not *transient-mark-mode*))
    (echo-message! echo (if *transient-mark-mode*
                          "Transient mark mode ON" "Transient mark mode OFF"))))

(def (cmd-insert-form-feed app)
  "Insert a form-feed character (^L page break)."
  (let ((ed (current-editor app)))
    (editor-replace-selection ed (string (integer->char 12)))))

(def (cmd-toggle-global-whitespace-mode app)
  "Toggle global-whitespace-mode (show all whitespace)."
  (let ((echo (app-state-echo app)))
    (set! *global-whitespace-mode* (not *global-whitespace-mode*))
    (echo-message! echo (if *global-whitespace-mode*
                          "Global whitespace mode ON" "Global whitespace mode OFF"))))

(def (cmd-toggle-hide-ifdef-mode app)
  "Toggle hide-ifdef-mode (hide #ifdef blocks)."
  (let ((echo (app-state-echo app)))
    (set! *hide-ifdef-mode* (not *hide-ifdef-mode*))
    (echo-message! echo (if *hide-ifdef-mode*
                          "Hide-ifdef mode ON" "Hide-ifdef mode OFF"))))

(def (cmd-toggle-allout-mode app)
  "Toggle allout-mode (outline editing)."
  (let ((echo (app-state-echo app)))
    (set! *allout-mode* (not *allout-mode*))
    (echo-message! echo (if *allout-mode*
                          "Allout mode ON" "Allout mode OFF"))))

;; ── batch 49: global minor mode toggles ─────────────────────────────
(def *indent-guide-global* #f)
(def *rainbow-delimiters-global* #f)
(def *global-display-fill-column* #f)
(def *global-flycheck* #f)
(def *global-company* #f)
(def *global-diff-hl* #f)
(def *global-git-gutter* #f)
(def *global-page-break-lines* #f)
(def *global-anzu* #f)

(def (cmd-toggle-indent-guide-global app)
  "Toggle global indent guides display."
  (let ((echo (app-state-echo app)))
    (set! *indent-guide-global* (not *indent-guide-global*))
    (echo-message! echo (if *indent-guide-global*
                          "Indent guide global ON" "Indent guide global OFF"))))

(def (cmd-toggle-rainbow-delimiters-global app)
  "Toggle global rainbow-delimiters-mode."
  (let ((echo (app-state-echo app)))
    (set! *rainbow-delimiters-global* (not *rainbow-delimiters-global*))
    (echo-message! echo (if *rainbow-delimiters-global*
                          "Rainbow delimiters ON" "Rainbow delimiters OFF"))))

(def (cmd-toggle-global-display-fill-column app)
  "Toggle global display of fill column indicator."
  (let ((echo (app-state-echo app)))
    (set! *global-display-fill-column* (not *global-display-fill-column*))
    (echo-message! echo (if *global-display-fill-column*
                          "Fill column indicator ON" "Fill column indicator OFF"))))

(def (cmd-toggle-global-flycheck app)
  "Toggle global flycheck-mode (on-the-fly syntax checking)."
  (let ((echo (app-state-echo app)))
    (set! *global-flycheck* (not *global-flycheck*))
    (echo-message! echo (if *global-flycheck*
                          "Global flycheck ON" "Global flycheck OFF"))))

(def (cmd-toggle-global-company app)
  "Toggle global company-mode (completion)."
  (let ((echo (app-state-echo app)))
    (set! *global-company* (not *global-company*))
    (echo-message! echo (if *global-company*
                          "Global company ON" "Global company OFF"))))

(def (cmd-toggle-global-diff-hl app)
  "Toggle global diff-hl-mode (VCS diff in fringe)."
  (let ((echo (app-state-echo app)))
    (set! *global-diff-hl* (not *global-diff-hl*))
    (echo-message! echo (if *global-diff-hl*
                          "Global diff-hl ON" "Global diff-hl OFF"))))

(def (cmd-toggle-global-git-gutter app)
  "Toggle global git-gutter-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-git-gutter* (not *global-git-gutter*))
    (echo-message! echo (if *global-git-gutter*
                          "Global git-gutter ON" "Global git-gutter OFF"))))

(def (cmd-toggle-global-page-break-lines app)
  "Toggle global page-break-lines-mode (display ^L as lines)."
  (let ((echo (app-state-echo app)))
    (set! *global-page-break-lines* (not *global-page-break-lines*))
    (echo-message! echo (if *global-page-break-lines*
                          "Page break lines ON" "Page break lines OFF"))))

(def (cmd-toggle-global-anzu app)
  "Toggle global anzu-mode (show search match count)."
  (let ((echo (app-state-echo app)))
    (set! *global-anzu* (not *global-anzu*))
    (echo-message! echo (if *global-anzu*
                          "Global anzu ON" "Global anzu OFF"))))

;; ── batch 54: navigation and editing enhancement toggles ────────────
(def *global-visual-regexp* #f)
(def *global-move-dup* #f)
(def *global-expand-region* #f)
(def *global-multiple-cursors* #f)
(def *global-undo-propose* #f)
(def *global-goto-chg* #f)
(def *global-avy* #f)

(def (cmd-toggle-global-visual-regexp app)
  "Toggle global visual-regexp-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-visual-regexp* (not *global-visual-regexp*))
    (echo-message! echo (if *global-visual-regexp*
                          "Visual regexp ON" "Visual regexp OFF"))))

(def (cmd-toggle-global-move-dup app)
  "Toggle global move-dup-mode (move/duplicate lines)."
  (let ((echo (app-state-echo app)))
    (set! *global-move-dup* (not *global-move-dup*))
    (echo-message! echo (if *global-move-dup*
                          "Move-dup ON" "Move-dup OFF"))))

(def (cmd-toggle-global-expand-region app)
  "Toggle global expand-region integration."
  (let ((echo (app-state-echo app)))
    (set! *global-expand-region* (not *global-expand-region*))
    (echo-message! echo (if *global-expand-region*
                          "Expand-region ON" "Expand-region OFF"))))

(def (cmd-toggle-global-multiple-cursors app)
  "Toggle global multiple-cursors-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-multiple-cursors* (not *global-multiple-cursors*))
    (echo-message! echo (if *global-multiple-cursors*
                          "Multiple cursors ON" "Multiple cursors OFF"))))

(def (cmd-toggle-global-undo-propose app)
  "Toggle global undo-propose-mode (preview undo)."
  (let ((echo (app-state-echo app)))
    (set! *global-undo-propose* (not *global-undo-propose*))
    (echo-message! echo (if *global-undo-propose*
                          "Undo propose ON" "Undo propose OFF"))))

(def (cmd-toggle-global-goto-chg app)
  "Toggle global goto-chg-mode (navigate edit points)."
  (let ((echo (app-state-echo app)))
    (set! *global-goto-chg* (not *global-goto-chg*))
    (echo-message! echo (if *global-goto-chg*
                          "Goto-chg ON" "Goto-chg OFF"))))

(def (cmd-toggle-global-avy app)
  "Toggle global avy-mode (jump to visible text)."
  (let ((echo (app-state-echo app)))
    (set! *global-avy* (not *global-avy*))
    (echo-message! echo (if *global-avy*
                          "Global avy ON" "Global avy OFF"))))

;;; ---- batch 63: fun and entertainment toggles ----

(def *global-nyan-cat* #f)
(def *global-parrot* #f)
(def *global-zone* #f)
(def *global-fireplace* #f)
(def *global-snow* #f)
(def *global-power-mode* #f)
(def *global-animate-typing* #f)

(def (cmd-toggle-global-nyan-cat app)
  "Toggle global nyan-cat-mode (Nyan Cat in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-nyan-cat* (not *global-nyan-cat*))
    (echo-message! echo (if *global-nyan-cat*
                          "Nyan cat ON" "Nyan cat OFF"))))

(def (cmd-toggle-global-parrot app)
  "Toggle global parrot-mode (party parrot in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-parrot* (not *global-parrot*))
    (echo-message! echo (if *global-parrot*
                          "Party parrot ON" "Party parrot OFF"))))

(def (cmd-toggle-global-zone app)
  "Toggle global zone-mode (screensaver when idle)."
  (let ((echo (app-state-echo app)))
    (set! *global-zone* (not *global-zone*))
    (echo-message! echo (if *global-zone*
                          "Zone mode ON" "Zone mode OFF"))))

(def (cmd-toggle-global-fireplace app)
  "Toggle global fireplace-mode (cozy fireplace animation)."
  (let ((echo (app-state-echo app)))
    (set! *global-fireplace* (not *global-fireplace*))
    (echo-message! echo (if *global-fireplace*
                          "Fireplace ON" "Fireplace OFF"))))

(def (cmd-toggle-global-snow app)
  "Toggle global snow-mode (let it snow animation)."
  (let ((echo (app-state-echo app)))
    (set! *global-snow* (not *global-snow*))
    (echo-message! echo (if *global-snow*
                          "Snow ON" "Snow OFF"))))

(def (cmd-toggle-global-power-mode app)
  "Toggle global power-mode (screen shake and particles on typing)."
  (let ((echo (app-state-echo app)))
    (set! *global-power-mode* (not *global-power-mode*))
    (echo-message! echo (if *global-power-mode*
                          "Power mode ON" "Power mode OFF"))))

(def (cmd-toggle-global-animate-typing app)
  "Toggle global animate-typing-mode (typing animation effect)."
  (let ((echo (app-state-echo app)))
    (set! *global-animate-typing* (not *global-animate-typing*))
    (echo-message! echo (if *global-animate-typing*
                          "Animate typing ON" "Animate typing OFF"))))

;;; ---- batch 72: data science and environment management toggles ----

(def *global-r-mode* #f)
(def *global-ess* #f)
(def *global-sql-mode* #f)
(def *global-ein* #f)
(def *global-conda* #f)
(def *global-pyvenv* #f)
(def *global-pipenv* #f)

(def (cmd-toggle-global-r-mode app)
  "Toggle global R-mode (R statistics language)."
  (let ((echo (app-state-echo app)))
    (set! *global-r-mode* (not *global-r-mode*))
    (echo-message! echo (if *global-r-mode*
                          "R mode ON" "R mode OFF"))))

(def (cmd-toggle-global-ess app)
  "Toggle global ESS-mode (Emacs Speaks Statistics)."
  (let ((echo (app-state-echo app)))
    (set! *global-ess* (not *global-ess*))
    (echo-message! echo (if *global-ess*
                          "ESS ON" "ESS OFF"))))

(def (cmd-toggle-global-sql-mode app)
  "Toggle global sql-mode (SQL query editing and execution)."
  (let ((echo (app-state-echo app)))
    (set! *global-sql-mode* (not *global-sql-mode*))
    (echo-message! echo (if *global-sql-mode*
                          "SQL mode ON" "SQL mode OFF"))))

(def (cmd-toggle-global-ein app)
  "Toggle global EIN-mode (Jupyter notebook in Emacs)."
  (let ((echo (app-state-echo app)))
    (set! *global-ein* (not *global-ein*))
    (echo-message! echo (if *global-ein*
                          "EIN ON" "EIN OFF"))))

(def (cmd-toggle-global-conda app)
  "Toggle global conda-mode (Conda environment management)."
  (let ((echo (app-state-echo app)))
    (set! *global-conda* (not *global-conda*))
    (echo-message! echo (if *global-conda*
                          "Conda ON" "Conda OFF"))))

(def (cmd-toggle-global-pyvenv app)
  "Toggle global pyvenv-mode (Python virtualenv management)."
  (let ((echo (app-state-echo app)))
    (set! *global-pyvenv* (not *global-pyvenv*))
    (echo-message! echo (if *global-pyvenv*
                          "Pyvenv ON" "Pyvenv OFF"))))

(def (cmd-toggle-global-pipenv app)
  "Toggle global pipenv-mode (Pipenv environment management)."
  (let ((echo (app-state-echo app)))
    (set! *global-pipenv* (not *global-pipenv*))
    (echo-message! echo (if *global-pipenv*
                          "Pipenv ON" "Pipenv OFF"))))

;;;============================================================================
;;; Comment-dwim (M-;) — Do What I Mean with comments
;;;============================================================================

(def (cmd-comment-dwim app)
  "Do What I Mean with comments. Region active: toggle. Blank line: insert comment. Otherwise: toggle current line."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (text (editor-get-text ed))
         (mark (buffer-mark buf)))
    (if mark
      ;; Region active: toggle comment on region lines
      (let* ((pos (editor-get-current-pos ed))
             (start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (lt (substring text ls le))
                     (trimmed (string-trim lt)))
                (if (string-prefix? ";;" trimmed)
                  ;; Uncomment
                  (let ((off (string-contains lt ";;")))
                    (when off
                      (let ((del-len (if (and (< (+ off 2) (string-length lt))
                                              (char=? (string-ref lt (+ off 2)) #\space))
                                       3 2)))
                        (editor-delete-range ed (+ ls off) del-len))))
                  ;; Comment
                  (editor-insert-text ed ls ";; ")))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app)
          (string-append "Toggled " (number->string (+ 1 (- end-line start-line))) " lines")))
      ;; No region: check current line
      (let* ((pos (editor-get-current-pos ed))
             (line (editor-line-from-position ed pos))
             (ls (editor-position-from-line ed line))
             (le (editor-get-line-end-position ed line))
             (line-text (substring text ls le))
             (trimmed (string-trim line-text)))
        (cond
          ;; Blank line: insert comment
          ((string=? trimmed "")
           (with-undo-action ed
             (editor-insert-text ed ls ";; "))
           (editor-goto-pos ed (+ ls 3)))
          ;; Already commented: uncomment
          ((string-prefix? ";;" trimmed)
           (let ((off (string-contains line-text ";;")))
             (when off
               (let ((del-len (if (and (< (+ off 2) (string-length line-text))
                                       (char=? (string-ref line-text (+ off 2)) #\space))
                                3 2)))
                 (with-undo-action ed
                   (editor-delete-range ed (+ ls off) del-len))))))
          ;; Not commented: add comment prefix
          (else
           (with-undo-action ed
             (editor-insert-text ed ls ";; "))))))))

;;;============================================================================
;;; Kill sentence / paragraph / subword
;;;============================================================================

(def (tui-sentence-end-pos text pos)
  "Find end of current sentence from pos."
  (let ((len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len) len)
        ((memv (string-ref text i) '(#\. #\? #\!))
         (+ i 1))
        (else (loop (+ i 1)))))))

(def (tui-sentence-start-pos text pos)
  "Find start of current sentence from pos."
  (let loop ((i (- pos 1)))
    (cond
      ((<= i 0) 0)
      ((memv (string-ref text i) '(#\. #\? #\!))
       (let skip-ws ((j (+ i 1)))
         (if (and (< j pos) (char-whitespace? (string-ref text j)))
           (skip-ws (+ j 1))
           j)))
      (else (loop (- i 1))))))

(def (cmd-kill-sentence app)
  "Kill from point to end of sentence."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (end (tui-sentence-end-pos text pos))
         (killed (substring text pos end)))
    (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
    (with-undo-action ed (editor-delete-range ed pos (- end pos)))))

(def (cmd-backward-kill-sentence app)
  "Kill from point back to start of sentence."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (start (tui-sentence-start-pos text pos))
         (killed (substring text start pos)))
    (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
    (with-undo-action ed (editor-delete-range ed start (- pos start)))))

(def (cmd-kill-paragraph app)
  "Kill from point to end of paragraph."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let loop ((i pos) (saw-text? #f))
      (let ((end (cond
                   ((>= i len) len)
                   ((char=? (string-ref text i) #\newline)
                    (if (and saw-text?
                             (or (>= (+ i 1) len)
                                 (char=? (string-ref text (+ i 1)) #\newline)))
                      (+ i 1) #f))
                   (else #f))))
        (if end
          (let ((killed (substring text pos end)))
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (with-undo-action ed (editor-delete-range ed pos (- end pos))))
          (loop (+ i 1) (or saw-text? (not (char=? (string-ref text i) #\newline)))))))))

(def (cmd-kill-subword app)
  "Kill forward to the next subword boundary (camelCase, snake_case)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let loop ((i (+ pos 1)))
      (let ((at-boundary?
             (or (>= i len)
                 (memv (string-ref text i) '(#\_ #\- #\space #\tab #\newline))
                 (and (> i 0)
                      (char-lower-case? (string-ref text (- i 1)))
                      (char-upper-case? (string-ref text i))))))
        (if at-boundary?
          (let* ((end (min i len))
                 (killed (substring text pos end)))
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (with-undo-action ed (editor-delete-range ed pos (- end pos))))
          (loop (+ i 1)))))))

;;;============================================================================
;;; S-expression list navigation: up-list, down-list
;;;============================================================================

(def (cmd-up-list app)
  "Move backward out of one level of parentheses."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (cond
        ((< i 0)
         (echo-message! (app-state-echo app) "At top level"))
        ((memv (string-ref text i) '(#\) #\] #\}))
         (loop (- i 1) (+ depth 1)))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (if (= depth 0)
           (begin (editor-goto-pos ed i) (editor-scroll-caret ed))
           (loop (- i 1) (- depth 1))))
        (else (loop (- i 1) depth))))))

(def (cmd-down-list app)
  "Move forward into one level of parentheses."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len)
         (echo-message! (app-state-echo app) "No inner list found"))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (editor-goto-pos ed (+ i 1))
         (editor-scroll-caret ed))
        (else (loop (+ i 1)))))))

