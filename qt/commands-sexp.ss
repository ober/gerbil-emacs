;;; -*- Gerbil -*-
;;; Qt commands sexp - sexp navigation, registers, toggle commands, text transforms
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gerbil-emacs/qt/sci-shim
        :gerbil-emacs/core
        :gerbil-emacs/editor
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/terminal
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/highlight
        :gerbil-emacs/qt/modeline
        :gerbil-emacs/qt/commands-core
        :gerbil-emacs/qt/commands-edit
        :gerbil-emacs/qt/commands-search
        :gerbil-emacs/qt/commands-file)

;;;============================================================================
;;; Count chars region, count words buffer/region
;;;============================================================================

(def (cmd-count-chars-region app)
  "Count characters in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (count (- end start)))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string count) " chars"))))))

(def (cmd-count-words-region app)
  "Count words in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (words (length (filter (lambda (s) (> (string-length s) 0))
                                    (string-split region #\space)))))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string words) " words"))))))

(def (cmd-count-words-buffer app)
  "Count words, lines, and chars in entire buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (chars (string-length text))
         (lines (+ 1 (length (string-split text #\newline))))
         (words (length (filter (lambda (s) (> (string-length s) 0))
                                (string-split text #\space)))))
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (number->string words) " words, "
                     (number->string lines) " lines, "
                     (number->string chars) " chars"))))

;;;============================================================================
;;; Buffer stats
;;;============================================================================

(def (cmd-buffer-stats app)
  "Show statistics about the current buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (chars (string-length text))
         (lines (+ 1 (length (filter (lambda (s) #t) (string-split text #\newline)))))
         (words (length (filter (lambda (s) (> (string-length s) 0))
                                (string-split text #\space))))
         (blanks (length (filter (lambda (s) (= (string-length (string-trim-both s)) 0))
                                 (string-split text #\newline))))
         (path (buffer-file-path buf)))
    (echo-message! (app-state-echo app)
      (string-append (buffer-name buf) ": "
                     (number->string lines) "L "
                     (number->string words) "W "
                     (number->string chars) "C "
                     (number->string blanks) " blank"
                     (if path (string-append " [" path "]") "")))))

;;;============================================================================
;;; List processes
;;;============================================================================

(def (cmd-list-processes app)
  "Show running subprocesses."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (lines ["Type\tBuffer"
                 "----\t------"]))
    (for-each
      (lambda (buf)
        (cond
          ((repl-buffer? buf)
           (set! lines (cons (string-append "REPL\t" (buffer-name buf)) lines)))
          ((shell-buffer? buf)
           (set! lines (cons (string-append "Shell\t" (buffer-name buf)) lines)))
          ((eshell-buffer? buf)
           (set! lines (cons (string-append "Eshell\t" (buffer-name buf)) lines)))
          ((terminal-buffer? buf)
           (set! lines (cons (string-append "Term\t" (buffer-name buf)) lines)))))
      (buffer-list))
    (let* ((text (string-join (reverse lines) "\n"))
           (buf (qt-buffer-create! "*Processes*" ed #f)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app)
        (string-append (number->string (- (length lines) 2)) " processes")))))

;;;============================================================================
;;; View messages
;;;============================================================================

(def (cmd-view-messages app)
  "Show *Messages* buffer with recent echo area messages."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-buffer-create! "*Messages*" ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed "(Messages buffer - recent activity shown here)")
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;;============================================================================
;;; What buffer / what face
;;;============================================================================

(def (cmd-what-buffer app)
  "Show current buffer name and file path."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (echo-message! (app-state-echo app)
      (if path
        (string-append (buffer-name buf) " [" path "]")
        (buffer-name buf)))))

(def (cmd-what-face app)
  "Show style info at point."
  (echo-message! (app-state-echo app)
    "Font style info not available in Qt plain text mode"))

;;;============================================================================
;;; Insert helpers
;;;============================================================================

(def (cmd-insert-buffer-name app)
  "Insert current buffer name at point."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (qt-plain-text-edit-insert-text! ed (buffer-name buf))))

(def (cmd-insert-file-name app)
  "Insert current buffer's file path at point."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (qt-plain-text-edit-insert-text! ed path)
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-insert-char app)
  "Insert a character by Unicode code point."
  (let ((input (qt-echo-read-string app "Unicode code point (hex): ")))
    (when input
      (let ((n (string->number (string-append "#x" input))))
        (if (and n (> n 0) (< n #x110000))
          (qt-plain-text-edit-insert-text! (current-qt-editor app)
            (string (integer->char n)))
          (echo-error! (app-state-echo app) "Invalid code point"))))))

;;;============================================================================
;;; Rename file and buffer
;;;============================================================================

(def (cmd-rename-file-and-buffer app)
  "Rename the current file on disk and rename the buffer."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((new-name (qt-echo-read-string app "New name: ")))
        (when new-name
          (let ((new-path (path-expand new-name (path-directory path))))
            (with-catch
              (lambda (e)
                (echo-error! (app-state-echo app)
                  (string-append "Rename failed: " (with-output-to-string
                    (lambda () (display-exception e))))))
              (lambda ()
                (rename-file path new-path)
                (set! (buffer-file-path buf) new-path)
                (set! (buffer-name buf) (path-strip-directory new-path))
                (echo-message! (app-state-echo app)
                  (string-append "Renamed to " new-path))))))))))

;;;============================================================================
;;; Sort numeric
;;;============================================================================

(def (cmd-sort-numeric app)
  "Sort lines in region by leading number."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (extract-num
               (lambda (s)
                 (let ((trimmed (string-trim s)))
                   (or (string->number
                         (let loop ((i 0) (acc ""))
                           (if (or (>= i (string-length trimmed))
                                   (not (or (char-numeric? (string-ref trimmed i))
                                            (char=? (string-ref trimmed i) #\-)
                                            (char=? (string-ref trimmed i) #\.))))
                             acc
                             (loop (+ i 1)
                                   (string-append acc (string (string-ref trimmed i)))))))
                       0))))
             (sorted (sort lines (lambda (a b) (< (extract-num a) (extract-num b)))))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted numerically")))))

;;;============================================================================
;;; Sort fields
;;;============================================================================

(def (cmd-sort-fields app)
  "Sort lines by a given field number."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((field-str (qt-echo-read-string app "Sort by field #: ")))
        (when field-str
          (let ((field-num (string->number field-str)))
            (when (and field-num (> field-num 0))
              (let* ((pos (qt-plain-text-edit-cursor-position ed))
                     (start (min mark pos))
                     (end (max mark pos))
                     (text (qt-plain-text-edit-text ed))
                     (region (substring text start end))
                     (lines (string-split region #\newline))
                     (get-field
                       (lambda (line)
                         (let ((fields (filter (lambda (s) (> (string-length s) 0))
                                              (string-split (string-trim line) #\space))))
                           (if (>= (length fields) field-num)
                             (list-ref fields (- field-num 1))
                             ""))))
                     (sorted (sort lines (lambda (a b) (string<? (get-field a) (get-field b)))))
                     (result (string-join sorted "\n"))
                     (new-text (string-append (substring text 0 start) result
                                              (substring text end (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed start)
                (set! (buffer-mark buf) #f)
                (echo-message! (app-state-echo app)
                  (string-append "Sorted by field " field-str))))))))))

;;;============================================================================
;;; Align regexp
;;;============================================================================

(def (cmd-align-regexp app)
  "Align region by a given string."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((pattern (qt-echo-read-string app "Align on: ")))
        (when (and pattern (> (string-length pattern) 0))
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 ;; Find max column of first occurrence of pattern
                 (max-col
                   (let loop ((ls lines) (mc 0))
                     (if (null? ls) mc
                       (let ((idx (string-contains (car ls) pattern)))
                         (loop (cdr ls) (if idx (max mc idx) mc))))))
                 ;; Align each line
                 (aligned
                   (map (lambda (line)
                          (let ((idx (string-contains line pattern)))
                            (if idx
                              (string-append
                                (substring line 0 idx)
                                (make-string (- max-col idx) #\space)
                                (substring line idx (string-length line)))
                              line)))
                        lines))
                 (result (string-join aligned "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app) "Aligned")))))))

;;;============================================================================
;;; Window management: enlarge, shrink, balance
;;;============================================================================

(def (adjust-window-size! app delta)
  "Adjust current window size by DELTA pixels in the splitter."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (when (> n 1)
      (let* ((splitter (qt-frame-splitter fr))
             (idx (qt-frame-current-idx fr))
             ;; Get current sizes
             (sizes (let loop ((i 0) (acc []))
                      (if (>= i n) (reverse acc)
                        (loop (+ i 1) (cons (qt-splitter-size-at splitter i) acc)))))
             ;; Find a neighbor to take/give from
             (neighbor (if (< idx (- n 1)) (+ idx 1) (- idx 1)))
             (cur-size (list-ref sizes idx))
             (nbr-size (list-ref sizes neighbor))
             (new-cur (max 50 (+ cur-size delta)))
             (new-nbr (max 50 (- nbr-size delta))))
        ;; Build new sizes list
        (let ((new-sizes (let loop ((i 0) (s sizes) (acc []))
                           (if (null? s) (reverse acc)
                             (loop (+ i 1) (cdr s)
                                   (cons (cond
                                           ((= i idx) new-cur)
                                           ((= i neighbor) new-nbr)
                                           (else (car s)))
                                         acc))))))
          (qt-splitter-set-sizes! splitter new-sizes))))))

(def (cmd-enlarge-window app)
  "Enlarge the current window by 50 pixels."
  (adjust-window-size! app 50)
  (echo-message! (app-state-echo app) "Window enlarged"))

(def (cmd-shrink-window app)
  "Shrink the current window by 50 pixels."
  (adjust-window-size! app -50)
  (echo-message! (app-state-echo app) "Window shrunk"))

(def (cmd-balance-windows app)
  "Make all windows the same size."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (n (length wins)))
    (when (> n 1)
      (let ((splitter (qt-frame-splitter fr))
            (equal-size 500)) ; Each panel gets same size hint
        (qt-splitter-set-sizes! splitter
          (let loop ((i 0) (acc '()))
            (if (>= i n) (reverse acc)
              (loop (+ i 1) (cons equal-size acc))))))))
  (echo-message! (app-state-echo app) "Windows balanced"))

;;;============================================================================
;;; Move to window line (M-r: cycle top/center/bottom)
;;;============================================================================

(def *recenter-position-qt* 'center)

(def (cmd-move-to-window-line app)
  "Move point to center, then top, then bottom of visible window."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (total-lines (length lines))
         (current-line (qt-plain-text-edit-cursor-line ed))
         ;; Approximate visible lines (assume 30)
         (visible-lines 30)
         (first-vis (max 0 (- current-line (quotient visible-lines 2))))
         (target-line
           (case *recenter-position-qt*
             ((center) (+ first-vis (quotient visible-lines 2)))
             ((top) first-vis)
             ((bottom) (+ first-vis (- visible-lines 1))))))
    ;; Move to target line start
    (let ((target (min (- total-lines 1) (max 0 target-line))))
      (qt-plain-text-edit-set-cursor-position! ed
        (line-start-position text target))
      (qt-plain-text-edit-ensure-cursor-visible! ed))
    ;; Cycle
    (set! *recenter-position-qt*
      (case *recenter-position-qt*
        ((center) 'top)
        ((top) 'bottom)
        ((bottom) 'center)))))

;;;============================================================================
;;; Upcase initials region (title case)
;;;============================================================================

(def (cmd-upcase-initials-region app)
  "Capitalize the first letter of each word in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (result (string-titlecase region))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Title-cased")))))

;;;============================================================================
;;; S-expression: backward-up-list, forward-up-list, mark-paragraph
;;;============================================================================

(def (cmd-backward-up-list app)
  "Move backward up one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (if (< i 0)
        (echo-error! (app-state-echo app) "At top level")
        (let ((ch (string-ref text i)))
          (cond
            ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
             (loop (- i 1) (+ depth 1)))
            ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
             (if (= depth 0)
               (begin
                 (qt-plain-text-edit-set-cursor-position! ed i)
                 (qt-plain-text-edit-ensure-cursor-visible! ed))
               (loop (- i 1) (- depth 1))))
            (else (loop (- i 1) depth))))))))

(def (cmd-forward-up-list app)
  "Move forward out of one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i pos) (depth 0))
      (if (>= i len)
        (echo-error! (app-state-echo app) "At top level")
        (let ((ch (string-ref text i)))
          (cond
            ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
             (loop (+ i 1) (+ depth 1)))
            ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
             (if (= depth 0)
               (begin
                 (qt-plain-text-edit-set-cursor-position! ed (+ i 1))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))
               (loop (+ i 1) (- depth 1))))
            (else (loop (+ i 1) depth))))))))

(def (cmd-mark-paragraph app)
  "Set mark at end of paragraph, point at beginning."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         ;; Find paragraph start (go back to blank line or beginning)
         (para-start
           (let loop ((i (- pos 1)))
             (if (< i 0) 0
               (if (and (char=? (string-ref text i) #\newline)
                        (or (= i 0)
                            (char=? (string-ref text (- i 1)) #\newline)))
                 (+ i 1)
                 (loop (- i 1))))))
         ;; Find paragraph end
         (para-end
           (let loop ((i pos))
             (if (>= i len) len
               (if (and (char=? (string-ref text i) #\newline)
                        (or (>= (+ i 1) len)
                            (char=? (string-ref text (+ i 1)) #\newline)))
                 (+ i 1)
                 (loop (+ i 1)))))))
    (set! (buffer-mark buf) para-end)
    (qt-plain-text-edit-set-cursor-position! ed para-start)
    (echo-message! (app-state-echo app) "Paragraph marked")))

;;;============================================================================
;;; Open rectangle (insert blank space)
;;;============================================================================

(def (cmd-open-rectangle app)
  "Insert blank space in rectangle region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (echo (app-state-echo app)))
    (if (not mark)
      (echo-error! echo "No mark set")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (col1 (column-at-position text start))
             (col2 (column-at-position text end))
             (left-col (min col1 col2))
             (right-col (max col1 col2))
             (width (- right-col left-col))
             (lines (string-split text #\newline)))
        (let-values (((start-line end-line) (region-line-range text start end)))
          (let* ((new-lines
                   (let loop ((ls lines) (i 0) (acc []))
                     (if (null? ls) (reverse acc)
                       (if (and (>= i start-line) (<= i end-line))
                         (let* ((l (car ls))
                                (len (string-length l))
                                (padded (if (< len left-col)
                                          (string-append l (make-string (- left-col len) #\space))
                                          l))
                                (new-line (string-append
                                            (substring padded 0 left-col)
                                            (make-string width #\space)
                                            (substring padded left-col (string-length padded)))))
                           (loop (cdr ls) (+ i 1) (cons new-line acc)))
                         (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
                 (new-text (string-join new-lines "\n")))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (min start (string-length new-text)))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Rectangle opened")))))))

;;;============================================================================
;;; Hippie expand (acts like dabbrev-expand)
;;;============================================================================

(def (cmd-hippie-expand app)
  "Expand word at point using buffer content (like dabbrev)."
  (cmd-dabbrev-expand app))

;;;============================================================================
;;; Split line
;;;============================================================================

(def (cmd-split-line app)
  "Split line at point, indenting continuation to current column."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (col (column-at-position text pos)))
    (qt-plain-text-edit-insert-text! ed
      (string-append "\n" (make-string col #\space)))))

;;;============================================================================
;;; Copy from above
;;;============================================================================

(def (cmd-copy-from-above app)
  "Copy character from the line above at the same column."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (col (column-at-position text pos))
         (current-line (qt-plain-text-edit-cursor-line ed)))
    (if (= current-line 0)
      (echo-error! (app-state-echo app) "No line above")
      (let* ((lines (string-split text #\newline))
             (above-line (list-ref lines (- current-line 1))))
        (if (>= col (string-length above-line))
          (echo-error! (app-state-echo app) "Above line shorter")
          (qt-plain-text-edit-insert-text! ed
            (string (string-ref above-line col))))))))

;;;============================================================================
;;; Increment register
;;;============================================================================

(def (cmd-increment-register app)
  "Increment the value in a register by 1."
  (let* ((echo (app-state-echo app))
         (regs (app-state-registers app))
         (name (qt-echo-read-string app "Register to increment: ")))
    (when (and name (= (string-length name) 1))
      (let* ((ch (string-ref name 0))
             (val (hash-get regs ch)))
        (cond
          ((not val) (echo-error! echo "Register empty"))
          ((string? val)
           (let ((n (string->number val)))
             (if n
               (begin
                 (hash-put! regs ch (number->string (+ n 1)))
                 (echo-message! echo (string-append "Register " name " = "
                                                     (number->string (+ n 1)))))
               (echo-error! echo "Register does not contain a number"))))
          (else (echo-error! echo "Register does not contain text")))))))

;;;============================================================================
;;; Delete pair
;;;============================================================================

(def (cmd-delete-pair app)
  "Delete the delimiter pair surrounding point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find enclosing open delimiter
    (let loop ((i (- pos 1)) (depth 0))
      (if (< i 0)
        (echo-error! (app-state-echo app) "No enclosing delimiters")
        (let ((ch (string-ref text i)))
          (cond
            ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
             (loop (- i 1) (+ depth 1)))
            ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
             (if (= depth 0)
               ;; Found open - now find matching close
               (let ((close-ch (cond ((char=? ch #\() #\))
                                     ((char=? ch #\[) #\])
                                     ((char=? ch #\{) #\}))))
                 (let cloop ((j (+ i 1)) (d 0))
                   (if (>= j len)
                     (echo-error! (app-state-echo app) "No matching close")
                     (let ((c (string-ref text j)))
                       (cond
                         ((char=? c ch) (cloop (+ j 1) (+ d 1)))
                         ((char=? c close-ch)
                          (if (= d 0)
                            ;; Delete close first (higher position), then open
                            (let* ((new-text (string-append
                                               (substring text 0 j)
                                               (substring text (+ j 1) len)))
                                   (new-text2 (string-append
                                                (substring new-text 0 i)
                                                (substring new-text (+ i 1)
                                                           (string-length new-text)))))
                              (qt-plain-text-edit-set-text! ed new-text2)
                              (qt-plain-text-edit-set-cursor-position! ed i))
                            (cloop (+ j 1) (- d 1))))
                         (else (cloop (+ j 1) d)))))))
               (loop (- i 1) (- depth 1))))
            (else (loop (- i 1) depth))))))))

;;;============================================================================
;;; Sudo write
;;;============================================================================

(def (cmd-sudo-write app)
  "Save file with sudo (using tee)."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (ed (current-qt-editor app)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((text (qt-plain-text-edit-text ed)))
        (with-catch
          (lambda (e)
            (echo-error! (app-state-echo app)
              (string-append "Sudo write failed: " (with-output-to-string
                (lambda () (display-exception e))))))
          (lambda ()
            (let ((proc (open-process
                          (list path: "/usr/bin/sudo"
                                arguments: (list "tee" path)
                                stdin-redirection: #t
                                stdout-redirection: #t))))
              (display text proc)
              (close-output-port proc)
              (process-status proc)
              (close-port proc)
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (echo-message! (app-state-echo app)
                (string-append "Wrote " path " (sudo)")))))))))

;;;============================================================================
;;; Ediff buffers
;;;============================================================================

(def (cmd-ediff-buffers app)
  "Compare two buffers by running diff."
  (let* ((bufs (buffer-list))
         (names (map buffer-name bufs))
         (a-name (qt-echo-read-string-with-completion app "Buffer A: " names)))
    (when a-name
      (let ((b-name (qt-echo-read-string-with-completion app "Buffer B: " names)))
        (when b-name
          (let* ((buf-a (find (lambda (b) (string=? (buffer-name b) a-name)) bufs))
                 (buf-b (find (lambda (b) (string=? (buffer-name b) b-name)) bufs)))
            (if (or (not buf-a) (not buf-b))
              (echo-error! (app-state-echo app) "Buffer not found")
              ;; Write both to temp files and diff
              (let* ((tmp-a (path-expand "ediff-a" (or (getenv "TMPDIR") "/tmp")))
                     (tmp-b (path-expand "ediff-b" (or (getenv "TMPDIR") "/tmp"))))
                ;; Get text from buffers
                (let ((text-a "")
                      (text-b ""))
                  ;; Find editors showing these buffers
                  (let ((fr (app-state-frame app)))
                    (for-each
                      (lambda (win)
                        (when (eq? (qt-edit-window-buffer win) buf-a)
                          (set! text-a (qt-plain-text-edit-text (qt-edit-window-editor win))))
                        (when (eq? (qt-edit-window-buffer win) buf-b)
                          (set! text-b (qt-plain-text-edit-text (qt-edit-window-editor win)))))
                      (qt-frame-windows fr)))
                  (call-with-output-file tmp-a (lambda (p) (display text-a p)))
                  (call-with-output-file tmp-b (lambda (p) (display text-b p)))
                  (let* ((proc (open-process
                                 (list path: "/usr/bin/diff"
                                       arguments: (list "-u" tmp-a tmp-b)
                                       stdout-redirection: #t)))
                         (output (read-line proc #f))
                         (_ (process-status proc))
                         (ed (current-qt-editor app))
                         (fr (app-state-frame app))
                         (diff-buf (qt-buffer-create! "*Ediff*" ed #f)))
                    (close-port proc)
                    (qt-buffer-attach! ed diff-buf)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                    (qt-plain-text-edit-set-text! ed (or output "No differences"))
                    (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                    (qt-plain-text-edit-set-cursor-position! ed 0)
                    ;; Clean up temp files
                    (with-catch void (lambda () (delete-file tmp-a)))
                    (with-catch void (lambda () (delete-file tmp-b)))
                    (echo-message! (app-state-echo app) "Diff complete")))))))))))

;;;============================================================================
;;; Highlight symbol / clear highlight
;;;============================================================================

(def (word-at-point ed)
  "Extract the word at the cursor position. Returns (values word start end) or (values #f 0 0)."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (>= pos len)
      (values #f 0 0)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\-)
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i
                        (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\-)
                                     (char=? (string-ref text i) #\_))))
                      i
                      (loop (+ i 1)))))
             (word (substring text start end)))
        (if (> (string-length word) 0)
          (values word start end)
          (values #f 0 0))))))

(def (count-occurrences text word)
  "Count non-overlapping occurrences of word in text."
  (let ((wlen (string-length word))
        (tlen (string-length text)))
    (let loop ((i 0) (count 0))
      (if (> (+ i wlen) tlen) count
        (if (string=? (substring text i (+ i wlen)) word)
          (loop (+ i wlen) (+ count 1))
          (loop (+ i 1) count))))))

(def (cmd-highlight-symbol app)
  "Toggle highlight of the word at point. Shows occurrence count."
  (let ((ed (current-qt-editor app)))
    (let-values (((word start end) (word-at-point ed)))
      (if (not word)
        (echo-error! (app-state-echo app) "No word at point")
        ;; Toggle: if already highlighting this word, clear it
        (if (and (app-state-last-search app)
                 (string=? (app-state-last-search app) word))
          (begin
            (set! (app-state-last-search app) #f)
            (echo-message! (app-state-echo app) "Highlights cleared"))
          (let* ((text (qt-plain-text-edit-text ed))
                 (count (count-occurrences text word)))
            (set! (app-state-last-search app) word)
            (echo-message! (app-state-echo app)
              (string-append "Highlighting: " word
                             " (" (number->string count) " occurrences)"))))))))

(def (cmd-highlight-symbol-next app)
  "Jump to next occurrence of the highlighted symbol."
  (let ((search (app-state-last-search app)))
    (if (not search)
      ;; If nothing highlighted, highlight word at point first
      (cmd-highlight-symbol app)
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (slen (string-length search))
             (tlen (string-length text))
             ;; Search forward from pos+1
             (found (let loop ((i (+ pos 1)))
                      (cond
                        ((> (+ i slen) tlen) #f)
                        ((string=? (substring text i (+ i slen)) search) i)
                        (else (loop (+ i 1)))))))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app)
              (string-append "\"" search "\" found")))
          ;; Wrap around
          (let ((wrapped (let loop ((i 0))
                           (cond
                             ((> (+ i slen) pos) #f)
                             ((string=? (substring text i (+ i slen)) search) i)
                             (else (loop (+ i 1)))))))
            (if wrapped
              (begin
                (qt-plain-text-edit-set-cursor-position! ed wrapped)
                (echo-message! (app-state-echo app)
                  (string-append "\"" search "\" (wrapped)")))
              (echo-message! (app-state-echo app) "No more occurrences"))))))))

(def (cmd-highlight-symbol-prev app)
  "Jump to previous occurrence of the highlighted symbol."
  (let ((search (app-state-last-search app)))
    (if (not search)
      (cmd-highlight-symbol app)
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (slen (string-length search))
             (tlen (string-length text))
             ;; Search backward from pos-1
             (found (let loop ((i (- pos 1)))
                      (cond
                        ((< i 0) #f)
                        ((and (<= (+ i slen) tlen)
                              (string=? (substring text i (+ i slen)) search)) i)
                        (else (loop (- i 1)))))))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app)
              (string-append "\"" search "\" found")))
          ;; Wrap around from end
          (let ((wrapped (let loop ((i (- tlen slen)))
                           (cond
                             ((< i (+ pos 1)) #f)
                             ((string=? (substring text i (+ i slen)) search) i)
                             (else (loop (- i 1)))))))
            (if wrapped
              (begin
                (qt-plain-text-edit-set-cursor-position! ed wrapped)
                (echo-message! (app-state-echo app)
                  (string-append "\"" search "\" (wrapped)")))
              (echo-message! (app-state-echo app) "No more occurrences"))))))))

(def (cmd-clear-highlight app)
  "Clear the current search highlight."
  (set! (app-state-last-search app) #f)
  (echo-message! (app-state-echo app) "Highlights cleared"))

;;;============================================================================
;;; Repeat complex command
;;;============================================================================

(def (cmd-repeat-complex-command app)
  "Repeat the last command that was executed."
  (let ((last-cmd (app-state-last-command app)))
    (if last-cmd
      (begin
        (execute-command! app last-cmd)
        (echo-message! (app-state-echo app)
          (string-append "Repeated: " (symbol->string last-cmd))))
      (echo-error! (app-state-echo app) "No previous command"))))

;;;============================================================================
;;; Undo history / undo tree visualization
;;;============================================================================

;; Track buffer snapshots for undo history browsing.
;; Each buffer name maps to a list of (timestamp . text-snapshot) pairs (newest first).
(def *undo-history* (make-hash-table)) ;; buffer-name -> list of (timestamp . text)
(def *undo-history-max* 50) ;; max snapshots per buffer

(def (undo-history-record! buf-name text)
  "Record a snapshot of the buffer text for undo history."
  (let* ((existing (or (hash-get *undo-history* buf-name) []))
         ;; Don't record if text is same as most recent
         (already-same (and (pair? existing)
                            (string=? (cdar existing) text))))
    (unless already-same
      (let* ((timestamp (inexact->exact (floor (time->seconds (current-time)))))
             (entry (cons timestamp text))
             (new-list (cons entry existing))
             ;; Trim to max size
             (trimmed (if (> (length new-list) *undo-history-max*)
                       (let loop ((l new-list) (n 0) (acc []))
                         (if (or (null? l) (>= n *undo-history-max*))
                           (reverse acc)
                           (loop (cdr l) (+ n 1) (cons (car l) acc))))
                       new-list)))
        (hash-put! *undo-history* buf-name trimmed)))))

(def (cmd-undo-history app)
  "Show the undo history for the current buffer. Navigate and restore past states."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (buf-name (buffer-name buf))
         (current-text (qt-plain-text-edit-text ed))
         (history (or (hash-get *undo-history* buf-name) [])))
    ;; Record current state if not already there
    (undo-history-record! buf-name current-text)
    (let ((history (or (hash-get *undo-history* buf-name) [])))
      (if (null? history)
        (echo-message! (app-state-echo app) "No undo history for this buffer")
        ;; Display history in a buffer
        (let* ((fr (app-state-frame app))
               (hist-buf (or (buffer-by-name "*Undo History*")
                             (qt-buffer-create! "*Undo History*" ed #f)))
               (lines
                 (let loop ((entries history) (i 0) (acc []))
                   (if (null? entries) (reverse acc)
                     (let* ((entry (car entries))
                            (ts (car entry))
                            (text (cdr entry))
                            (tlen (string-length text))
                            (line-count (let lp ((j 0) (c 1))
                                          (cond ((>= j tlen) c)
                                                ((char=? (string-ref text j) #\newline) (lp (+ j 1) (+ c 1)))
                                                (else (lp (+ j 1) c)))))
                            (marker (if (= i 0) " <- current" ""))
                            ;; Format timestamp as relative
                            (now (inexact->exact (floor (time->seconds (current-time)))))
                            (age (- now ts))
                            (age-str (cond
                                       ((< age 60) (string-append (number->string age) "s ago"))
                                       ((< age 3600) (string-append (number->string (quotient age 60)) "m ago"))
                                       ((< age 86400) (string-append (number->string (quotient age 3600)) "h ago"))
                                       (else (string-append (number->string (quotient age 86400)) "d ago"))))
                            (preview (let ((first-change
                                            (substring text 0 (min 60 tlen))))
                                       (let ((nl (string-contains first-change "\n")))
                                         (if nl (substring first-change 0 nl) first-change))))
                            (line (string-append
                                    (number->string i) ": "
                                    age-str " | "
                                    (number->string tlen) " chars, "
                                    (number->string line-count) " lines"
                                    marker
                                    "\n   " preview)))
                       (loop (cdr entries) (+ i 1) (cons line acc))))))
               (header (string-append "Undo History for: " buf-name "\n"
                                      "Type the snapshot number to restore, or q to quit.\n"
                                      (make-string 60 #\-) "\n"))
               (content (string-append header (string-join lines "\n"))))
          (qt-buffer-attach! ed hist-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) hist-buf)
          (qt-plain-text-edit-set-text! ed content)
          (qt-text-document-set-modified! (buffer-doc-pointer hist-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

(def (cmd-undo-history-restore app)
  "Restore a snapshot from undo history. Prompts for snapshot number."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (buf-name (buffer-name buf))
         (history (or (hash-get *undo-history* buf-name) [])))
    (if (null? history)
      (echo-message! (app-state-echo app) "No undo history")
      (let* ((input (qt-echo-read-string app
                      (string-append "Restore snapshot (0-" (number->string (- (length history) 1)) "): ")))
             (num (string->number (string-trim input))))
        (cond
          ((not num)
           (echo-error! (app-state-echo app) "Not a number"))
          ((or (< num 0) (>= num (length history)))
           (echo-error! (app-state-echo app) "Invalid snapshot number"))
          (else
           (let* ((entry (list-ref history num))
                  (text (cdr entry)))
             (qt-plain-text-edit-set-text! ed text)
             (qt-plain-text-edit-set-cursor-position! ed 0)
             (echo-message! (app-state-echo app)
               (string-append "Restored snapshot " (number->string num))))))))))

(def (cmd-flush-undo app)
  "Clear the undo history."
  (let* ((buf (current-qt-buffer app))
         (buf-name (buffer-name buf)))
    (hash-put! *undo-history* buf-name [])
    (echo-message! (app-state-echo app) "Undo history cleared")))

;;;============================================================================
;;; Untabify buffer
;;;============================================================================

(def (cmd-untabify-buffer app)
  "Convert all tabs to spaces in the entire buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (if (not (string-contains text "\t"))
      (echo-message! (app-state-echo app) "No tabs found")
      (let* ((parts (string-split text #\tab))
             (result (string-join parts "        ")))
        (qt-plain-text-edit-set-text! ed result)
        (echo-message! (app-state-echo app) "Untabified buffer")))))

;;;============================================================================
;;; Narrow to defun
;;;============================================================================

(def (cmd-narrow-to-defun app)
  "Narrow buffer to the current function/defun."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find start of current top-level form (search backward for unindented open paren)
    (let ((defun-start
            (let loop ((i pos))
              (cond
                ((< i 0) 0)
                ((and (char=? (string-ref text i) #\()
                      (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                 i)
                (else (loop (- i 1)))))))
      ;; Find end of defun using matching paren
      (let ((defun-end
              (let loop ((i defun-start) (depth 0))
                (cond
                  ((>= i len) len)
                  ((char=? (string-ref text i) #\()
                   (loop (+ i 1) (+ depth 1)))
                  ((char=? (string-ref text i) #\))
                   (if (= depth 1) (+ i 1)
                     (loop (+ i 1) (- depth 1))))
                  (else (loop (+ i 1) depth))))))
        ;; Include trailing newline
        (let ((end (if (and (< defun-end len) (char=? (string-ref text defun-end) #\newline))
                     (+ defun-end 1) defun-end)))
          (let* ((buf (current-qt-buffer app))
                 (region (substring text defun-start end)))
            ;; Use the existing narrowing infrastructure
            (hash-put! *narrow-state* buf (list text defun-start end))
            (qt-plain-text-edit-set-text! ed region)
            (qt-plain-text-edit-set-cursor-position! ed (- pos defun-start))
            (echo-message! (app-state-echo app) "Narrowed to defun")))))))

;;;============================================================================
;;; Expand region - smart selection expansion
;;;============================================================================

(def *expand-region-stack* []) ;; stack of (start . end) for contract-region

(def (cmd-expand-region app)
  "Progressively expand the selection: word -> line -> paragraph -> sexp -> buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Push current selection onto stack
    (when (not (= sel-start sel-end))
      (set! *expand-region-stack* (cons (cons sel-start sel-end) *expand-region-stack*)))
    (cond
      ;; No selection -> select word at point
      ((= sel-start sel-end)
       (let* ((word-start (let loop ((i pos))
                            (if (or (= i 0) (not (or (char-alphabetic? (string-ref text (- i 1)))
                                                      (char-numeric? (string-ref text (- i 1)))
                                                      (char=? (string-ref text (- i 1)) #\_)
                                                      (char=? (string-ref text (- i 1)) #\-))))
                              i (loop (- i 1)))))
              (word-end (let loop ((i pos))
                          (if (or (>= i len) (not (or (char-alphabetic? (string-ref text i))
                                                       (char-numeric? (string-ref text i))
                                                       (char=? (string-ref text i) #\_)
                                                       (char=? (string-ref text i) #\-))))
                            i (loop (+ i 1))))))
         (when (< word-start word-end)
           (set! *expand-region-stack* (cons (cons sel-start sel-end) *expand-region-stack*))
           (qt-plain-text-edit-set-selection! ed word-start word-end))))
      ;; Selection within a line -> expand to whole line
      ((let* ((line-start (let loop ((i sel-start))
                            (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                              i (loop (- i 1)))))
              (line-end (let loop ((i sel-end))
                          (if (or (>= i len) (char=? (string-ref text i) #\newline))
                            i (loop (+ i 1))))))
         (and (or (> sel-start line-start) (< sel-end line-end))
              (begin
                (qt-plain-text-edit-set-selection! ed line-start (min (+ line-end 1) len))
                #t))))
      ;; Selection is line(s) -> expand to paragraph
      ((let* ((para-start (let loop ((i sel-start))
                            (cond
                              ((<= i 0) 0)
                              ((and (char=? (string-ref text (- i 1)) #\newline)
                                    (or (= (- i 1) 0) (char=? (string-ref text (- i 2)) #\newline)))
                               i)
                              (else (loop (- i 1))))))
              (para-end (let loop ((i sel-end))
                          (cond
                            ((>= i len) len)
                            ((and (char=? (string-ref text i) #\newline)
                                  (or (>= (+ i 1) len) (char=? (string-ref text (+ i 1)) #\newline)))
                             (+ i 1))
                            (else (loop (+ i 1)))))))
         (and (or (> sel-start para-start) (< sel-end para-end))
              (begin
                (qt-plain-text-edit-set-selection! ed para-start para-end)
                #t))))
      ;; Already paragraph -> expand to whole buffer
      (else
       (qt-plain-text-edit-set-selection! ed 0 len)))))

(def (cmd-contract-region app)
  "Contract the selection to the previous expand-region state."
  (if (null? *expand-region-stack*)
    (echo-message! (app-state-echo app) "No previous selection")
    (let* ((prev (car *expand-region-stack*))
           (ed (current-qt-editor app)))
      (set! *expand-region-stack* (cdr *expand-region-stack*))
      (if (= (car prev) (cdr prev))
        (qt-plain-text-edit-set-cursor-position! ed (car prev))
        (qt-plain-text-edit-set-selection! ed (car prev) (cdr prev))))))

;;;============================================================================
;;; String inflection (case conversion)
;;;============================================================================

(def (split-identifier word)
  "Split an identifier into its component words, handling camelCase, snake_case, kebab-case."
  (let ((len (string-length word)))
    (if (= len 0) []
      (let loop ((i 0) (start 0) (acc []))
        (cond
          ((>= i len)
           (reverse (if (< start i) (cons (substring word start i) acc) acc)))
          ;; Split on underscore or hyphen
          ((or (char=? (string-ref word i) #\_) (char=? (string-ref word i) #\-))
           (let ((part (if (< start i) (list (substring word start i)) [])))
             (loop (+ i 1) (+ i 1) (append part acc))))
          ;; Split on camelCase boundary (lowercase followed by uppercase)
          ((and (> i start)
                (char-upper-case? (string-ref word i))
                (char-lower-case? (string-ref word (- i 1))))
           (loop i i (cons (substring word start i) acc)))
          (else (loop (+ i 1) start acc)))))))

(def (to-snake-case parts)
  (string-join (map string-downcase parts) "_"))

(def (to-kebab-case parts)
  (string-join (map string-downcase parts) "-"))

(def (to-camel-case parts)
  (if (null? parts) ""
    (string-append
      (string-downcase (car parts))
      (apply string-append
        (map (lambda (p)
               (if (> (string-length p) 0)
                 (string-append
                   (string (char-upcase (string-ref p 0)))
                   (string-downcase (substring p 1 (string-length p))))
                 ""))
             (cdr parts))))))

(def (to-pascal-case parts)
  (apply string-append
    (map (lambda (p)
           (if (> (string-length p) 0)
             (string-append
               (string (char-upcase (string-ref p 0)))
               (string-downcase (substring p 1 (string-length p))))
             ""))
         parts)))

(def (to-upper-case parts)
  (string-join (map string-upcase parts) "_"))

(def (cmd-string-inflection-cycle app)
  "Cycle the identifier at point through: snake_case -> kebab-case -> camelCase -> PascalCase -> UPPER_CASE."
  (let ((ed (current-qt-editor app)))
    (let-values (((word start end) (word-at-point ed)))
      (if (not word)
        (echo-error! (app-state-echo app) "No word at point")
        (let* ((parts (split-identifier word))
               (snake (to-snake-case parts))
               (kebab (to-kebab-case parts))
               (camel (to-camel-case parts))
               (pascal (to-pascal-case parts))
               (upper (to-upper-case parts))
               ;; Determine current form and pick next
               (next (cond
                       ((string=? word snake) kebab)
                       ((string=? word kebab) camel)
                       ((string=? word camel) pascal)
                       ((string=? word pascal) upper)
                       (else snake)))
               (text (qt-plain-text-edit-text ed))
               (new-text (string-append
                           (substring text 0 start)
                           next
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start)
          (echo-message! (app-state-echo app) (string-append word " -> " next)))))))

;;;============================================================================
;;; Number increment/decrement at point
;;;============================================================================

(def (number-at-point ed)
  "Find a number at cursor position. Returns (values num-string start end) or (values #f 0 0)."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (or (>= pos len) (not (or (char-numeric? (string-ref text pos))
                                    (and (char=? (string-ref text pos) #\-)
                                         (< (+ pos 1) len)
                                         (char-numeric? (string-ref text (+ pos 1)))))))
      (values #f 0 0)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\-))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len) (not (char-numeric? (string-ref text i))))
                      i (loop (+ i 1)))))
             (num-str (substring text start end)))
        (values num-str start end)))))

(def (cmd-increment-number app)
  "Increment the number at point."
  (let ((ed (current-qt-editor app)))
    (let-values (((num-str start end) (number-at-point ed)))
      (if (not num-str)
        (echo-error! (app-state-echo app) "No number at point")
        (let* ((num (string->number num-str))
               (new-num (+ num 1))
               (new-str (number->string new-num))
               (text (qt-plain-text-edit-text ed))
               (new-text (string-append
                           (substring text 0 start)
                           new-str
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start))))))

(def (cmd-decrement-number app)
  "Decrement the number at point."
  (let ((ed (current-qt-editor app)))
    (let-values (((num-str start end) (number-at-point ed)))
      (if (not num-str)
        (echo-error! (app-state-echo app) "No number at point")
        (let* ((num (string->number num-str))
               (new-num (- num 1))
               (new-str (number->string new-num))
               (text (qt-plain-text-edit-text ed))
               (new-text (string-append
                           (substring text 0 start)
                           new-str
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start))))))

;;;============================================================================
;;; Browse URL / open link at point
;;;============================================================================

(def (url-at-point ed)
  "Find a URL at or near the cursor position. Returns URL string or #f."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Search backward for URL start
    (let* ((line-start (let loop ((i pos))
                         (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                           i (loop (- i 1)))))
           (line-end (let loop ((i pos))
                       (if (or (>= i len) (char=? (string-ref text i) #\newline))
                         i (loop (+ i 1)))))
           (line (substring text line-start line-end)))
      ;; Find http:// or https:// in the line
      (let loop ((i 0))
        (let ((http-pos (string-contains (substring line i (string-length line)) "http")))
          (if (not http-pos) #f
            (let* ((url-start (+ i http-pos))
                   ;; Find end of URL (whitespace or common delimiters)
                   (url-end (let lp ((j url-start))
                              (if (or (>= j (string-length line))
                                      (char=? (string-ref line j) #\space)
                                      (char=? (string-ref line j) #\tab)
                                      (char=? (string-ref line j) #\>)
                                      (char=? (string-ref line j) #\))
                                      (char=? (string-ref line j) #\])
                                      (char=? (string-ref line j) #\"))
                                j (lp (+ j 1)))))
                   (url (substring line url-start url-end)))
              ;; Check if it's a valid URL prefix
              (if (or (string-prefix? "http://" url) (string-prefix? "https://" url))
                url
                (loop (+ url-start 4))))))))))

(def (cmd-browse-url-at-point app)
  "Open the URL at point in an external browser."
  (let* ((ed (current-qt-editor app))
         (url (url-at-point ed)))
    (if (not url)
      (echo-error! (app-state-echo app) "No URL at point")
      (begin
        (open-process
          (list path: "xdg-open"
                arguments: (list url)
                stdout-redirection: #f
                stderr-redirection: #f))
        (echo-message! (app-state-echo app)
          (string-append "Opening: " url))))))

(def (cmd-browse-url app)
  "Prompt for a URL and open it in an external browser."
  (let ((url (qt-echo-read-string app "URL: ")))
    (when (> (string-length url) 0)
      (let ((full-url (if (or (string-prefix? "http://" url) (string-prefix? "https://" url))
                        url (string-append "https://" url))))
        (open-process
          (list path: "xdg-open"
                arguments: (list full-url)
                stdout-redirection: #f
                stderr-redirection: #f))
        (echo-message! (app-state-echo app)
          (string-append "Opening: " full-url))))))

;;;============================================================================
;;; Imenu - jump to definition (multi-language)
;;;============================================================================

;; Note: cmd-imenu is defined near goto-definition with multi-language support

;;;============================================================================
;;; Cycle tab width
;;;============================================================================

(def *tab-width* 4)

(def (cmd-cycle-tab-width app)
  "Cycle tab stop width: 2 -> 4 -> 8 -> 2."
  (set! *tab-width*
    (cond ((= *tab-width* 2) 4)
          ((= *tab-width* 4) 8)
          (else 2)))
  (echo-message! (app-state-echo app)
    (string-append "Tab width: " (number->string *tab-width*))))

;;;============================================================================
;;; Toggle indent tabs mode
;;;============================================================================

(def *indent-tabs-mode* #f)

(def (cmd-toggle-indent-tabs-mode app)
  "Toggle between tabs and spaces for indentation."
  (set! *indent-tabs-mode* (not *indent-tabs-mode*))
  (echo-message! (app-state-echo app)
    (if *indent-tabs-mode* "Indent with tabs" "Indent with spaces")))

;;;============================================================================
;;; Replace string (non-interactive replace-all)
;;;============================================================================

(def (cmd-replace-string app)
  "Replace all occurrences of a string."
  (let ((from (qt-echo-read-string app "Replace string: ")))
    (when from
      (let ((to (qt-echo-read-string app (string-append "Replace \"" from "\" with: "))))
        (when to
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (count 0)
                 (result (let loop ((s text) (acc ""))
                           (let ((idx (string-contains s from)))
                             (if idx
                               (begin
                                 (set! count (+ count 1))
                                 (loop (substring s (+ idx (string-length from)) (string-length s))
                                       (string-append acc (substring s 0 idx) to)))
                               (string-append acc s))))))
            (qt-plain-text-edit-set-text! ed result)
            (echo-message! (app-state-echo app)
              (string-append "Replaced " (number->string count) " occurrences"))))))))

;;;============================================================================
;;; String insert file (alias)
;;;============================================================================

(def (cmd-string-insert-file app)
  "Insert a file's contents (alias for insert-file)."
  (cmd-insert-file app))

;;;============================================================================
;;; Navigation: goto-column, recenter-top/bottom, window positions
;;;============================================================================

(def (cmd-goto-column app)
  "Move cursor to a specified column on the current line."
  (let ((input (qt-echo-read-string app "Go to column: ")))
    (when input
      (let ((col (string->number input)))
        (when (and col (>= col 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (line (qt-plain-text-edit-cursor-line ed))
                 (line-start (line-start-position text line))
                 (lines (string-split text #\newline))
                 (line-len (if (< line (length lines))
                             (string-length (list-ref lines line))
                             0))
                 (target-col (min col line-len)))
            (qt-plain-text-edit-set-cursor-position! ed (+ line-start target-col))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-goto-line-relative app)
  "Move cursor by a relative number of lines."
  (let ((input (qt-echo-read-string app "Relative lines (+/-): ")))
    (when input
      (let ((n (string->number input)))
        (when n
          (let* ((ed (current-qt-editor app))
                 (line (qt-plain-text-edit-cursor-line ed))
                 (text (qt-plain-text-edit-text ed))
                 (total (length (string-split text #\newline)))
                 (target (max 0 (min (- total 1) (+ line n)))))
            (qt-plain-text-edit-set-cursor-position! ed
              (line-start-position text target))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-recenter-top app)
  "Scroll so cursor is at the top of the window."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-ensure-cursor-visible! ed)
    (echo-message! (app-state-echo app) "Recentered to top")))

(def (cmd-recenter-bottom app)
  "Scroll so cursor is at the bottom of the window."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-ensure-cursor-visible! ed)
    (echo-message! (app-state-echo app) "Recentered to bottom")))

;;;============================================================================
;;; Character case commands
;;;============================================================================

(def (cmd-upcase-char app)
  "Upcase the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (char-upcase (string-ref text pos)))
             (new-text (string-append (substring text 0 pos)
                                      (string ch)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-downcase-char app)
  "Downcase the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (char-downcase (string-ref text pos)))
             (new-text (string-append (substring text 0 pos)
                                      (string ch)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-toggle-case-at-point app)
  "Toggle case of character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (string-ref text pos))
             (toggled (if (char-upper-case? ch)
                        (char-downcase ch)
                        (char-upcase ch)))
             (new-text (string-append (substring text 0 pos)
                                      (string toggled)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-capitalize-region app)
  "Capitalize the first letter of each word in region."
  (cmd-upcase-initials-region app))

;;;============================================================================
;;; Copy commands
;;;============================================================================

(def (cmd-copy-buffer-name app)
  "Copy buffer name to kill ring."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (set! (app-state-kill-ring app) (cons name (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied: " name))))

(def (cmd-copy-current-line app)
  "Copy the current line to kill ring."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines)) (list-ref lines line) "")))
    (set! (app-state-kill-ring app) (cons line-text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) "Line copied")))

(def (cmd-copy-word app)
  "Copy word at point to kill ring."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1))))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i)))))
                      i (loop (+ i 1)))))
             (word (substring text start end)))
        (when (> (string-length word) 0)
          (set! (app-state-kill-ring app) (cons word (app-state-kill-ring app)))
          (echo-message! (app-state-echo app) (string-append "Copied: " word)))))))

(def (cmd-copy-file-path app)
  "Copy file path to kill ring."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (set! (app-state-kill-ring app) (cons path (app-state-kill-ring app)))
        (echo-message! (app-state-echo app) (string-append "Copied: " path)))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-copy-line-number app)
  "Copy current line number to kill ring."
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (s (number->string line)))
    (set! (app-state-kill-ring app) (cons s (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied line: " s))))

(def (cmd-copy-region-as-kill app)
  "Copy region without deactivating mark (alias for copy-region)."
  (cmd-copy-region app))

(def (cmd-yank-whole-line app)
  "Yank the first item from kill ring as a whole line."
  (let* ((ed (current-qt-editor app))
         (kr (app-state-kill-ring app))
         (text (and (pair? kr) (car kr))))
    (if (not text)
      (echo-error! (app-state-echo app) "Kill ring empty")
      (begin
        ;; Move to beginning of line, insert text + newline
        (let* ((pos (qt-plain-text-edit-cursor-position ed))
               (full (qt-plain-text-edit-text ed))
               (line-start (line-start-position full (qt-plain-text-edit-cursor-line ed))))
          (qt-plain-text-edit-set-cursor-position! ed line-start)
          (qt-plain-text-edit-insert-text! ed (string-append text "\n")))))))

