;;; -*- Gerbil -*-
;;; Qt commands edit2 - text utilities, dired ops, macros, registers, navigation
;;; Part of the qt/commands-*.ss module chain.
;;; Split from qt/commands-edit.ss to keep files under 2000 lines.

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
        :gemacs/qt/image
        :gemacs/qt/modeline
        :gemacs/qt/commands-core
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit)

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

(def (count-words-in text)
  "Count words in a string using whitespace word boundaries."
  (let ((len (string-length text)))
    (let loop ((i 0) (in-word #f) (count 0))
      (if (>= i len) count
        (let ((ch (string-ref text i)))
          (if (char-whitespace? ch)
            (loop (+ i 1) #f count)
            (loop (+ i 1) #t (if in-word count (+ count 1)))))))))

(def (count-lines-in text)
  "Count lines in a string."
  (let ((len (string-length text)))
    (if (= len 0) 0
      (let loop ((i 0) (count 1))
        (if (>= i len) count
          (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ count 1) count)))))))

(def (cmd-count-words app)
  "Count words, lines, and characters. Shows region stats if active, buffer stats otherwise."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      ;; Region active — show region stats
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (words (count-words-in region))
             (lines (count-lines-in region))
             (chars (string-length region)))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string lines) " lines, "
                         (number->string words) " words, "
                         (number->string chars) " characters")))
      ;; No region — show buffer stats
      (let* ((text (qt-plain-text-edit-text ed))
             (len (string-length text))
             (num-lines (qt-plain-text-edit-line-count ed))
             (words (count-words-in text)))
        (echo-message! (app-state-echo app)
          (string-append "Buffer has " (number->string num-lines) " lines, "
                         (number->string words) " words, "
                         (number->string len) " characters"))))))

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
          ;; Fresh expansion — search current buffer, then file-backed buffers on disk
          (let* ((current-words (collect-buffer-words text))
                 (other-words
                   (let ((seen (make-hash-table)))
                     (for-each (lambda (w) (hash-put! seen w #t)) current-words)
                     ;; Also collect words from other buffers' files on disk
                     (let loop ((bufs (buffer-list)) (acc []))
                       (if (null? bufs) acc
                         (let* ((b (car bufs))
                                (fp (buffer-file-path b)))
                           (if (not fp)
                             (loop (cdr bufs) acc)
                             (let ((file-text (with-catch (lambda (e) #f)
                                                (lambda ()
                                                  (and (file-exists? fp)
                                                       (call-with-input-file fp
                                                         (lambda (p) (read-line p #f))))))))
                               (if (not file-text)
                                 (loop (cdr bufs) acc)
                                 (let ((words (collect-buffer-words file-text)))
                                   (loop (cdr bufs)
                                     (append acc
                                       (filter (lambda (w)
                                                 (and (not (hash-get seen w))
                                                      (begin (hash-put! seen w #t) #t)))
                                               words))))))))))))
                 (all-words (append current-words other-words))
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
      (let ((filename (expand-filename filename)))
        (if (file-exists? filename)
          (let ((content (read-file-as-string filename)))
            (when content
              (qt-plain-text-edit-insert-text! (current-qt-editor app) content)
              (echo-message! echo (string-append "Inserted " filename))))
          (echo-error! echo (string-append "File not found: " filename)))))))

;;;============================================================================
;;; Shell command (M-!)
;;;============================================================================

(def (cmd-shell-command app)
  (let* ((echo (app-state-echo app))
         (cmd (qt-echo-read-string app "Shell command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (echo-message! echo (string-append "Running: " cmd "..."))
      (async-process! cmd
        callback: (lambda (result)
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (buf (or (buffer-by-name "*Shell Output*")
                          (qt-buffer-create! "*Shell Output*" ed #f))))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed result)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! echo (string-append "Shell: " cmd))))))))

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
          (sci-send ed SCI_SETREADONLY 0 0)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (sci-send ed SCI_SETREADONLY 1 0))))))


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


(def (cmd-occur-next app)
  "Move to the next match line in *Occur* buffer."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Occur*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (lines (string-split text #\newline))
             ;; Find current line number from position
             (cur-line (let loop ((i 0) (chars 0))
                         (if (or (null? (list-tail lines i))
                                 (>= i (length lines)))
                           i
                           (let ((line-len (+ (string-length (list-ref lines i)) 1)))
                             (if (> (+ chars line-len) pos)
                               i
                               (loop (+ i 1) (+ chars line-len))))))))
        ;; Find next line starting with a digit and containing ':'
        (let loop ((l (+ cur-line 1)) (char-pos 0))
          ;; Calculate char-pos for line l
          (when (< l (length lines))
            (let* ((target-pos (let lp ((i 0) (p 0))
                                 (if (>= i l) p
                                   (lp (+ i 1) (+ p (string-length (list-ref lines i)) 1)))))
                   (line-text (list-ref lines l)))
              (if (and (> (string-length line-text) 0)
                       (char-numeric? (string-ref line-text 0))
                       (string-index line-text #\:))
                (begin
                  (qt-plain-text-edit-set-cursor-position! ed target-pos)
                  (qt-plain-text-edit-ensure-cursor-visible! ed))
                (loop (+ l 1) 0)))))))))

(def (cmd-occur-prev app)
  "Move to the previous match line in *Occur* buffer."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Occur*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (lines (string-split text #\newline))
             (cur-line (let loop ((i 0) (chars 0))
                         (if (>= i (length lines))
                           i
                           (let ((line-len (+ (string-length (list-ref lines i)) 1)))
                             (if (> (+ chars line-len) pos)
                               i
                               (loop (+ i 1) (+ chars line-len))))))))
        (let loop ((l (- cur-line 1)))
          (when (>= l 0)
            (let* ((target-pos (let lp ((i 0) (p 0))
                                 (if (>= i l) p
                                   (lp (+ i 1) (+ p (string-length (list-ref lines i)) 1)))))
                   (line-text (list-ref lines l)))
              (if (and (> (string-length line-text) 0)
                       (char-numeric? (string-ref line-text 0))
                       (string-index line-text #\:))
                (begin
                  (qt-plain-text-edit-set-cursor-position! ed target-pos)
                  (qt-plain-text-edit-ensure-cursor-visible! ed))
                (loop (- l 1))))))))))

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

(def (cmd-name-last-kbd-macro app)
  "Give a name to the last recorded keyboard macro."
  (let ((macro (app-state-macro-last app)))
    (if (or (not macro) (null? macro))
      (echo-error! (app-state-echo app) "No macro defined")
      (let ((name (qt-echo-read-string app "Name for macro: ")))
        (when (and name (> (string-length name) 0))
          (hash-put! (app-state-macro-named app) name macro)
          (echo-message! (app-state-echo app)
            (string-append "Macro saved as '" name "'")))))))

(def (cmd-call-named-kbd-macro app)
  "Execute a named keyboard macro."
  (let* ((named (app-state-macro-named app))
         (names (sort (map car (hash->list named)) string<?)))
    (if (null? names)
      (echo-error! (app-state-echo app) "No named macros")
      (let ((name (qt-echo-read-with-narrowing app "Run macro:" names)))
        (when (and name (> (string-length name) 0))
          (let ((macro (hash-get named name)))
            (if macro
              (for-each
                (lambda (event)
                  (case (car event)
                    ((self-insert)
                     (qt-plain-text-edit-insert-text!
                       (current-qt-editor app) (cdr event)))
                    ((command)
                     (execute-command! app (cdr event)))))
                macro)
              (echo-error! (app-state-echo app)
                (string-append "No macro named '" name "'")))))))))

(def (cmd-list-kbd-macros app)
  "List all named keyboard macros."
  (let* ((named (app-state-macro-named app))
         (names (sort (map car (hash->list named)) string<?)))
    (if (null? names)
      (echo-message! (app-state-echo app) "No named macros")
      (echo-message! (app-state-echo app)
        (string-append "Macros: " (string-join names ", "))))))

(def (cmd-save-kbd-macros app)
  "Save named keyboard macros to ~/.gemacs-macros."
  (let ((named (app-state-macro-named app)))
    (if (= (hash-length named) 0)
      (echo-message! (app-state-echo app) "No macros to save")
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Error saving macros"))
        (lambda ()
          (let ((path (path-expand ".gemacs-macros"
                        (user-info-home (user-info (user-name))))))
            (call-with-output-file path
              (lambda (port)
                (for-each
                  (lambda (pair)
                    (write (cons (car pair) (cdr pair)) port)
                    (newline port))
                  (hash->list named)))))
          (echo-message! (app-state-echo app)
            (string-append "Saved " (number->string (hash-length named)) " macros")))))))

(def (cmd-load-kbd-macros app)
  "Load named keyboard macros from ~/.gemacs-macros."
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "No saved macros found"))
    (lambda ()
      (let* ((path (path-expand ".gemacs-macros"
                     (user-info-home (user-info (user-name)))))
             (named (app-state-macro-named app)))
        (call-with-input-file path
          (lambda (port)
            (let loop ()
              (let ((datum (read port)))
                (when (not (eof-object? datum))
                  (when (pair? datum)
                    (hash-put! named (car datum) (cdr datum)))
                  (loop))))))
        (echo-message! (app-state-echo app)
          (string-append "Loaded " (number->string (hash-length named)) " macros"))))))

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
          ;; File register (file . path)
          ((and (pair? val) (eq? (car val) 'file))
           (let ((path (cdr val)))
             (if (file-exists? path)
               (let* ((name (path-strip-directory path))
                      (fr (app-state-frame app))
                      (ed (current-qt-editor app))
                      ;; Reuse existing buffer or create new
                      (existing (buffer-by-name name))
                      (buf (or existing
                               (qt-buffer-create! name ed path))))
                 (qt-buffer-attach! ed buf)
                 (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                 (unless existing
                   (let ((text (read-file-as-string path)))
                     (when text
                       (qt-plain-text-edit-set-text! ed text)
                       (qt-text-document-set-modified!
                         (buffer-doc-pointer buf) #f)
                       (qt-plain-text-edit-set-cursor-position! ed 0))))
                 (echo-message! echo (string-append "Opened file: " path)))
               (echo-error! echo
                 (string-append "File not found: " path)))))
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

(def (cmd-file-to-register app)
  "Save a file path to a register for quick jumping."
  (let* ((input (qt-echo-read-string app "File to register (register char): "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (buf (current-qt-buffer app))
             (path (buffer-file-path buf)))
        (if path
          (begin
            (hash-put! (app-state-registers app) reg (cons 'file path))
            (echo-message! echo
              (string-append "File " path " saved to register " (string reg))))
          (echo-error! echo "Buffer has no file"))))))

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

