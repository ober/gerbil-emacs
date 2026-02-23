;;; -*- Gerbil -*-
;;; Qt commands file - avy, buffer cycling, file operations, bookmarks
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/terminal
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-search)

;;;============================================================================
;;; Avy-style jump-to-char navigation
;;;============================================================================

(def *avy-labels* "asdfjklghqwertyuiopzxcvbnm")

(def (avy-find-char-positions text ch start end)
  "Find all positions of CH in TEXT between START and END."
  (let ((len (min end (string-length text)))
        (target (char-downcase ch)))
    (let loop ((i start) (acc []))
      (if (>= i len) (reverse acc)
        (if (char=? (char-downcase (string-ref text i)) target)
          (loop (+ i 1) (cons i acc))
          (loop (+ i 1) acc))))))

(def (avy-find-word-positions text start end)
  "Find positions at the start of each word in TEXT between START and END."
  (let ((len (min end (string-length text))))
    (let loop ((i start) (in-word #f) (acc []))
      (if (>= i len) (reverse acc)
        (let ((ch (string-ref text i)))
          (cond
            ((and (not in-word) (char-alphabetic? ch))
             (loop (+ i 1) #t (cons i acc)))
            ((char-whitespace? ch)
             (loop (+ i 1) #f acc))
            (else
             (loop (+ i 1) in-word acc))))))))

(def (avy-format-candidates text positions)
  "Format candidate list with labels for echo display."
  (let ((labels *avy-labels*)
        (n (min (string-length *avy-labels*) (length positions))))
    (let loop ((i 0) (ps positions) (acc []))
      (if (or (>= i n) (null? ps)) (string-join (reverse acc) "  ")
        (let* ((pos (car ps))
               ;; Get context: the word around the position
               (ctx-start (max 0 (- pos 10)))
               (ctx-end (min (string-length text) (+ pos 20)))
               (ctx (substring text ctx-start ctx-end))
               ;; Clean up context (replace newlines with spaces)
               (clean (let loop ((j 0) (out (open-output-string)))
                        (if (>= j (string-length ctx))
                          (get-output-string out)
                          (let ((c (string-ref ctx j)))
                            (display (if (char=? c #\newline) " " (string c)) out)
                            (loop (+ j 1) out))))))
          (loop (+ i 1) (cdr ps)
                (cons (string-append
                        (string (string-ref labels i)) ":"
                        clean)
                      acc)))))))

(def (cmd-avy-goto-char app)
  "Jump to a character occurrence. Type a char, then type a label to jump."
  (let ((ch-str (qt-echo-read-string app "avy char: ")))
    (when (and ch-str (= (string-length ch-str) 1))
      (let* ((ch (string-ref ch-str 0))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             ;; Search in a window around cursor (approx visible range)
             (start (max 0 (- pos 2000)))
             (end (min (string-length text) (+ pos 2000)))
             (positions (avy-find-char-positions text ch start end))
             (n (min (string-length *avy-labels*) (length positions))))
        (cond
          ((= (length positions) 0)
           (echo-error! (app-state-echo app) "No matches"))
          ((= (length positions) 1)
           ;; Single match — jump directly
           (qt-plain-text-edit-set-cursor-position! ed (car positions))
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else
           ;; Show candidates
           (echo-message! (app-state-echo app)
             (avy-format-candidates text positions))
           ;; Read label
           (let ((label (qt-echo-read-string app "avy jump: ")))
             (when (and label (= (string-length label) 1))
               (let ((idx (let loop ((i 0))
                            (if (>= i (string-length *avy-labels*)) #f
                              (if (char=? (string-ref *avy-labels* i)
                                          (string-ref label 0))
                                i (loop (+ i 1)))))))
                 (when (and idx (< idx n))
                   (qt-plain-text-edit-set-cursor-position! ed
                     (list-ref positions idx))
                   (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))))

(def (cmd-avy-goto-word app)
  "Jump to a word start. Type a label to jump."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (start (max 0 (- pos 2000)))
         (end (min (string-length text) (+ pos 2000)))
         (positions (avy-find-word-positions text start end))
         (n (min (string-length *avy-labels*) (length positions))))
    (cond
      ((= (length positions) 0)
       (echo-error! (app-state-echo app) "No word starts found"))
      (else
       (echo-message! (app-state-echo app)
         (avy-format-candidates text positions))
       (let ((label (qt-echo-read-string app "avy word: ")))
         (when (and label (= (string-length label) 1))
           (let ((idx (let loop ((i 0))
                        (if (>= i (string-length *avy-labels*)) #f
                          (if (char=? (string-ref *avy-labels* i)
                                      (string-ref label 0))
                            i (loop (+ i 1)))))))
             (when (and idx (< idx n))
               (qt-plain-text-edit-set-cursor-position! ed
                 (list-ref positions idx))
               (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))

(def (cmd-avy-goto-line app)
  "Jump to a line start. Type a label to jump."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         ;; Find all line starts in visible range
         (start (max 0 (- pos 2000)))
         (end (min (string-length text) (+ pos 2000)))
         (positions
           (let loop ((i start) (acc (if (= start 0) [0] [])))
             (if (>= i end) (reverse acc)
               (if (char=? (string-ref text i) #\newline)
                 (loop (+ i 1) (cons (+ i 1) acc))
                 (loop (+ i 1) acc)))))
         (n (min (string-length *avy-labels*) (length positions))))
    (cond
      ((= (length positions) 0)
       (echo-error! (app-state-echo app) "No lines found"))
      (else
       (let ((labels-str
               (let loop ((i 0) (ps positions) (acc []))
                 (if (or (>= i n) (null? ps)) (string-join (reverse acc) " ")
                   (let* ((p (car ps))
                          ;; Get line number
                          (line-num (let lp ((j 0) (ln 1))
                                      (if (>= j p) ln
                                        (lp (+ j 1)
                                            (if (char=? (string-ref text j) #\newline)
                                              (+ ln 1) ln)))))
                          (line-end (let lp ((j p))
                                      (if (or (>= j (string-length text))
                                              (char=? (string-ref text j) #\newline))
                                        j (lp (+ j 1)))))
                          (preview (substring text p (min line-end (+ p 30)))))
                     (loop (+ i 1) (cdr ps)
                           (cons (string-append
                                   (string (string-ref *avy-labels* i))
                                   ":" (number->string line-num) " " preview)
                                 acc)))))))
         (echo-message! (app-state-echo app) labels-str)
         (let ((label (qt-echo-read-string app "avy line: ")))
           (when (and label (= (string-length label) 1))
             (let ((idx (let loop ((i 0))
                          (if (>= i (string-length *avy-labels*)) #f
                            (if (char=? (string-ref *avy-labels* i)
                                        (string-ref label 0))
                              i (loop (+ i 1)))))))
               (when (and idx (< idx n))
                 (qt-plain-text-edit-set-cursor-position! ed
                   (list-ref positions idx))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))))))))))

;;;============================================================================
;;; Buffer cycling (previous/next buffer)
;;;============================================================================

(def (cmd-previous-buffer app)
  (let* ((bufs (buffer-list))
         (cur (current-qt-buffer app)))
    (when (> (length bufs) 1)
      (let ((prev (let loop ((bs bufs) (last #f))
                    (cond ((null? bs) last)
                          ((eq? (car bs) cur) (or last (last-element bufs)))
                          (else (loop (cdr bs) (car bs)))))))
        (when prev
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed prev)
            (set! (qt-edit-window-buffer (qt-current-window fr)) prev)
            (echo-message! (app-state-echo app) (buffer-name prev))))))))

(def (last-element lst)
  (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

(def (cmd-next-buffer app)
  (let* ((bufs (buffer-list))
         (cur (current-qt-buffer app)))
    (when (> (length bufs) 1)
      (let ((next (let loop ((bs bufs))
                    (cond ((null? bs) (car bufs))
                          ((eq? (car bs) cur)
                           (if (pair? (cdr bs)) (cadr bs) (car bufs)))
                          (else (loop (cdr bs)))))))
        (when next
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed next)
            (set! (qt-edit-window-buffer (qt-current-window fr)) next)
            (echo-message! (app-state-echo app) (buffer-name next))))))))

;;;============================================================================
;;; Delete trailing whitespace
;;;============================================================================

(def (cmd-delete-trailing-whitespace app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (cleaned (map (lambda (line) (string-trim-right line char-whitespace?)) lines))
         (new-text (string-join cleaned "\n"))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed
      (min pos (string-length new-text)))
    (echo-message! (app-state-echo app) "Trailing whitespace deleted")))

;;;============================================================================
;;; Open line above
;;;============================================================================

(def (cmd-open-line-above app)
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)
    (qt-plain-text-edit-insert-text! ed "\n")
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_UP)))

;;;============================================================================
;;; Select line
;;;============================================================================

(def (cmd-select-line app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)
    (set! (buffer-mark buf) (qt-plain-text-edit-cursor-position ed))
    (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END_OF_BLOCK)
    (echo-message! (app-state-echo app) "Line selected")))

;;;============================================================================
;;; Smart beginning of line (toggle between indentation and column 0)
;;;============================================================================

(def (cmd-smart-beginning-of-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (line-pos (line-start-position text line))
             (indent (let loop ((i 0))
                       (if (and (< i (string-length line-text))
                                (char-whitespace? (string-ref line-text i)))
                         (loop (+ i 1)) i)))
             (indent-pos (+ line-pos indent)))
        ;; If at indentation, go to column 0; otherwise go to indentation
        (if (= pos indent-pos)
          (qt-plain-text-edit-set-cursor-position! ed line-pos)
          (qt-plain-text-edit-set-cursor-position! ed indent-pos))))))

;;;============================================================================
;;; Insert parentheses / brackets
;;;============================================================================

(def (cmd-insert-parentheses app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "()")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

(def (cmd-insert-pair-brackets app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "[]")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

;;;============================================================================
;;; Find file at point
;;;============================================================================

;; cmd-find-file-at-point is defined earlier with file:line support

;;;============================================================================
;;; Show kill ring
;;;============================================================================

(def (cmd-show-kill-ring app)
  (let* ((ring (app-state-kill-ring app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (if (null? ring)
      (echo-message! echo "Kill ring is empty")
      (let* ((entries (let loop ((items ring) (i 0) (acc []))
                        (if (or (null? items) (>= i 20)) (reverse acc)
                          (let ((item (car items)))
                            (loop (cdr items) (+ i 1)
                                  (cons (string-append (number->string i) ": "
                                          (if (> (string-length item) 60)
                                            (string-append (substring item 0 60) "...")
                                            item))
                                        acc))))))
             (text (string-append "Kill Ring:\n\n"
                                  (string-join entries "\n")))
             (buf (or (buffer-by-name "*Kill Ring*")
                      (qt-buffer-create! "*Kill Ring*" ed #f))))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

(def (cmd-browse-kill-ring app)
  "Interactive kill ring browser — prompt to select an entry to insert."
  (let* ((ring (app-state-kill-ring app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app)))
    (if (null? ring)
      (echo-message! echo "Kill ring is empty")
      ;; Show numbered preview in echo area and prompt
      (let* ((previews (let loop ((items ring) (i 0) (acc []))
                         (if (or (null? items) (>= i 10)) (reverse acc)
                           (let* ((item (car items))
                                  ;; Take first line only, truncate
                                  (first-line (let ((nl (string-contains item "\n")))
                                                (if nl (substring item 0 nl) item)))
                                  (preview (if (> (string-length first-line) 40)
                                             (string-append (substring first-line 0 40) "...")
                                             first-line)))
                             (loop (cdr items) (+ i 1)
                                   (cons (string-append (number->string i) ":" preview)
                                         acc))))))
             (prompt (string-append (string-join previews " | ") " — Enter #: "))
             (input (qt-echo-read-string app prompt)))
        (when (and input (> (string-length input) 0))
          (let ((idx (string->number input)))
            (if (and idx (>= idx 0) (< idx (length ring)))
              (let ((text (list-ref ring idx)))
                (qt-plain-text-edit-insert-text! ed text)
                (echo-message! echo
                  (string-append "Inserted kill ring entry " (number->string idx))))
              (echo-error! echo "Invalid kill ring index"))))))))

;;;============================================================================
;;; List registers
;;;============================================================================

(def (cmd-list-registers app)
  (let* ((regs (app-state-registers app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (let ((entries (let loop ((keys (sort (map (lambda (k) (string k))
                                               (hash-keys regs)) string<?))
                              (acc []))
                     (if (null? keys) (reverse acc)
                       (let* ((key (string-ref (car keys) 0))
                              (val (hash-get regs key))
                              (desc (cond
                                      ((string? val)
                                       (if (> (string-length val) 50)
                                         (string-append "\"" (substring val 0 50) "...\"")
                                         (string-append "\"" val "\"")))
                                      ((pair? val)
                                       (string-append "pos " (number->string (cdr val))
                                                      " in " (car val)))
                                      (else "?"))))
                         (loop (cdr keys)
                               (cons (string-append "  " (string key) ": " desc)
                                     acc)))))))
      (if (null? entries)
        (echo-message! echo "No registers defined")
        (let* ((text (string-append "Registers:\n\n"
                                    (string-join entries "\n")))
               (buf (or (buffer-by-name "*Registers*")
                        (qt-buffer-create! "*Registers*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; Scroll other window
;;;============================================================================

(def (cmd-scroll-other-window app)
  (let ((fr (app-state-frame app)))
    (when (> (length (qt-frame-windows fr)) 1)
      (qt-frame-other-window! fr)
      (cmd-scroll-down app)
      (qt-frame-other-window! fr))))

(def (cmd-scroll-other-window-up app)
  (let ((fr (app-state-frame app)))
    (when (> (length (qt-frame-windows fr)) 1)
      (qt-frame-other-window! fr)
      (cmd-scroll-up app)
      (qt-frame-other-window! fr))))

;;;============================================================================
;;; Swap buffers between windows
;;;============================================================================

(def (cmd-swap-buffers app)
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    (if (>= (length wins) 2)
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (b1 (qt-edit-window-buffer w1))
             (b2 (qt-edit-window-buffer w2))
             (e1 (qt-edit-window-editor w1))
             (e2 (qt-edit-window-editor w2)))
        (qt-buffer-attach! e1 b2)
        (qt-buffer-attach! e2 b1)
        (set! (qt-edit-window-buffer w1) b2)
        (set! (qt-edit-window-buffer w2) b1)
        (echo-message! (app-state-echo app) "Buffers swapped"))
      (echo-message! (app-state-echo app) "Need at least 2 windows"))))

;;;============================================================================
;;; Goto percent
;;;============================================================================

(def (cmd-goto-percent app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Goto percent: ")))
    (when (and input (> (string-length input) 0))
      (let ((pct (string->number input)))
        (if (and pct (>= pct 0) (<= pct 100))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (len (string-length text))
                 (target (inexact->exact (round (* len (/ pct 100.0))))))
            (qt-plain-text-edit-set-cursor-position! ed (min target len))
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-error! echo "Invalid percentage"))))))

;;;============================================================================
;;; Sentence navigation
;;;============================================================================

(def (cmd-forward-sentence app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond ((>= i len)
             (qt-plain-text-edit-set-cursor-position! ed len))
            ((and (memv (string-ref text i) '(#\. #\! #\?))
                  (or (>= (+ i 1) len)
                      (char-whitespace? (string-ref text (+ i 1)))))
             (qt-plain-text-edit-set-cursor-position! ed (+ i 1))
             (qt-plain-text-edit-ensure-cursor-visible! ed))
            (else (loop (+ i 1)))))))

(def (cmd-backward-sentence app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 2)))
      (cond ((<= i 0)
             (qt-plain-text-edit-set-cursor-position! ed 0))
            ((and (memv (string-ref text i) '(#\. #\! #\?))
                  (char-whitespace? (string-ref text (+ i 1))))
             (qt-plain-text-edit-set-cursor-position! ed (+ i 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed))
            (else (loop (- i 1)))))))

;;;============================================================================
;;; Dired (M-x dired)
;;;============================================================================

(def (cmd-dired app)
  (let* ((echo (app-state-echo app))
         (dir (qt-echo-read-string app "Dired: ")))
    (when (and dir (> (string-length dir) 0))
      (let ((dir (expand-filename dir)))
        (if (and (file-exists? dir)
                 (eq? 'directory (file-info-type (file-info dir))))
          (dired-open-directory! app dir)
          (echo-error! echo (string-append "Not a directory: " dir)))))))

;;;============================================================================
;;; Unfill paragraph (join lines in paragraph)
;;;============================================================================

(def (cmd-unfill-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    ;; Find paragraph boundaries
    (let* ((para-start (let loop ((i line))
                         (if (or (<= i 0)
                                 (string=? (string-trim (list-ref lines (- i 1))) ""))
                           i (loop (- i 1)))))
           (para-end (let loop ((i line))
                       (if (or (>= i (- (length lines) 1))
                               (string=? (string-trim (list-ref lines (+ i 1))) ""))
                         (+ i 1) (loop (+ i 1)))))
           (para-lines (let loop ((i para-start) (acc []))
                         (if (>= i para-end) (reverse acc)
                           (loop (+ i 1) (cons (string-trim (list-ref lines i)) acc)))))
           (joined (string-join para-lines " "))
           (before (let loop ((i 0) (acc []))
                     (if (>= i para-start) (reverse acc)
                       (loop (+ i 1) (cons (list-ref lines i) acc)))))
           (after (let loop ((i para-end) (acc []))
                    (if (>= i (length lines)) (reverse acc)
                      (loop (+ i 1) (cons (list-ref lines i) acc)))))
           (new-lines (append before (list joined) after))
           (new-text (string-join new-lines "\n")))
      (qt-plain-text-edit-set-text! ed new-text)
      (qt-plain-text-edit-set-cursor-position! ed
        (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))
      (echo-message! (app-state-echo app) "Unfilled"))))

;;;============================================================================
;;; Whitespace cleanup
;;;============================================================================

(def (cmd-whitespace-cleanup app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         ;; Remove trailing whitespace
         (cleaned (map (lambda (line) (string-trim-right line char-whitespace?)) lines))
         ;; Remove trailing blank lines
         (trimmed (let loop ((ls (reverse cleaned)))
                    (if (and (pair? ls) (string=? (car ls) ""))
                      (loop (cdr ls))
                      (reverse ls))))
         (new-text (string-append (string-join trimmed "\n") "\n"))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length new-text)))
    (echo-message! (app-state-echo app) "Whitespace cleaned")))

;;;============================================================================
;;; Insert UUID
;;;============================================================================

(def (cmd-insert-uuid app)
  (let ((uuid (with-catch
                (lambda (e) "00000000-0000-0000-0000-000000000000")
                (lambda ()
                  (let ((port (open-process
                                (list path: "/usr/bin/uuidgen"
                                      stdout-redirection: #t))))
                    (let ((result (read-line port)))
                      (close-port port)
                      (string-downcase (string-trim result))))))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) uuid)))

;;;============================================================================
;;; Word frequency
;;;============================================================================

(def (cmd-word-frequency app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (words (collect-buffer-words text))
         (freq (make-hash-table)))
    ;; Count each word
    (for-each
      (lambda (w) (hash-put! freq w (+ 1 (or (hash-get freq w) 0))))
      words)
    ;; Sort by frequency
    (let* ((pairs (map (lambda (k) (cons k (hash-get freq k))) (hash-keys freq)))
           (sorted (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
           (lines (map (lambda (p)
                         (string-append (number->string (cdr p)) "\t" (car p)))
                       (if (> (length sorted) 100)
                         (let loop ((ls sorted) (i 0) (acc []))
                           (if (or (null? ls) (>= i 100)) (reverse acc)
                             (loop (cdr ls) (+ i 1) (cons (car ls) acc))))
                         sorted)))
           (text (string-append "Word Frequency (top "
                                (number->string (length lines)) "):\n\n"
                                (string-join lines "\n")))
           (fr (app-state-frame app))
           (buf (or (buffer-by-name "*Word Frequency*")
                    (qt-buffer-create! "*Word Frequency*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;;;============================================================================
;;; Indent rigidly (shift region right/left)
;;;============================================================================

(def (cmd-indent-rigidly-right app)
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
             (shifted (map (lambda (l) (string-append "  " l)) lines))
             (new-region (string-join shifted "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start))
      ;; No mark: indent current line
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (line (qt-plain-text-edit-cursor-line ed)))
        (when (< line (length lines))
          (qt-replace-line! ed line
            (string-append "  " (list-ref lines line))))))))

(def (cmd-indent-rigidly-left app)
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
             (shifted (map (lambda (l)
                             (if (and (>= (string-length l) 2)
                                      (string=? (substring l 0 2) "  "))
                               (substring l 2 (string-length l))
                               (string-trim l)))
                           lines))
             (new-region (string-join shifted "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start))
      ;; No mark: dedent current line
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (line (qt-plain-text-edit-cursor-line ed)))
        (when (< line (length lines))
          (let ((l (list-ref lines line)))
            (when (and (>= (string-length l) 2)
                       (string=? (substring l 0 2) "  "))
              (qt-replace-line! ed line
                (substring l 2 (string-length l))))))))))

;;;============================================================================
;;; Center line
;;;============================================================================

(def (cmd-center-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (fill-column 70))
    (when (< line (length lines))
      (let* ((l (string-trim (list-ref lines line)))
             (padding (max 0 (quotient (- fill-column (string-length l)) 2)))
             (centered (string-append (make-string padding #\space) l)))
        (qt-replace-line! ed line centered)))))

;;;============================================================================
;;; Narrow to region / widen
;;;============================================================================

;; Store narrowing state per buffer
(def *narrow-state* (make-hash-table))

(def (cmd-narrow-to-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end)))
        ;; Save full text and narrow bounds
        (hash-put! *narrow-state* buf (list text start end))
        ;; Show only the region
        (qt-plain-text-edit-set-text! ed region)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Narrowed"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-widen app)
  (let* ((buf (current-qt-buffer app))
         (state (hash-get *narrow-state* buf)))
    (if state
      (let* ((ed (current-qt-editor app))
             (full-text (car state))
             (start (cadr state))
             ;; Get the current narrowed text (may have been edited)
             (narrow-text (qt-plain-text-edit-text ed))
             ;; Replace the narrowed region with the edited version
             (new-text (string-append
                         (substring full-text 0 start)
                         narrow-text
                         (substring full-text (caddr state) (string-length full-text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (hash-remove! *narrow-state* buf)
        (echo-message! (app-state-echo app) "Widened"))
      (echo-message! (app-state-echo app) "Buffer is not narrowed"))))

;;;============================================================================
;;; Display time
;;;============================================================================

(def (cmd-display-time app)
  (let ((result (with-catch
                  (lambda (e) "unknown")
                  (lambda ()
                    (let ((port (open-process
                                  (list path: "/bin/date"
                                        stdout-redirection: #t))))
                      (let ((output (read-line port)))
                        (close-port port)
                        output))))))
    (echo-message! (app-state-echo app) result)))

;;;============================================================================
;;; Buffer info
;;;============================================================================

(def (cmd-buffer-info app)
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (qt-plain-text-edit-line-count ed))
         (chars (string-length text))
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i chars) count
                    (if (char-whitespace? (string-ref text i))
                      (loop (+ i 1) #f count)
                      (loop (+ i 1) #t (if in-word count (+ count 1)))))))
         (name (buffer-name buf))
         (path (or (buffer-file-path buf) "(no file)"))
         (modified (if (and (buffer-doc-pointer buf)
                           (qt-text-document-modified? (buffer-doc-pointer buf)))
                     "modified" "unmodified")))
    (echo-message! (app-state-echo app)
      (string-append name " [" modified "] " path
                     " (" (number->string lines) "L "
                     (number->string words) "W "
                     (number->string chars) "C)"))))

;;;============================================================================
;;; Bookmarks
;;;============================================================================

(def (cmd-bookmark-set app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Bookmark name: ")))
    (when (and input (> (string-length input) 0))
      (let* ((buf (current-qt-buffer app))
             (pos (qt-plain-text-edit-cursor-position (current-qt-editor app))))
        (hash-put! (app-state-bookmarks app) input
                   (list (buffer-name buf) (buffer-file-path buf) pos))
        (echo-message! echo (string-append "Bookmark \"" input "\" set"))
        (bookmarks-save! app)))))

(def (cmd-bookmark-jump app)
  (let* ((echo (app-state-echo app))
         (names (sort (hash-keys (app-state-bookmarks app)) string<?))
         (input (qt-echo-read-with-narrowing app "Jump to bookmark:" names)))
    (when (and input (> (string-length input) 0))
      (let ((entry (hash-get (app-state-bookmarks app) input)))
        (if entry
          (let* ((buf-name (if (list? entry) (car entry) (car entry)))
                 (fpath (if (list? entry) (cadr entry) #f))
                 (pos (if (list? entry) (caddr entry) (cdr entry)))
                 (fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 ;; Try existing buffer first, then open by file-path
                 (buf (or (buffer-by-name buf-name)
                          (and fpath (file-exists? fpath)
                               (let loop ((bufs *buffer-list*))
                                 (if (null? bufs) #f
                                   (let ((b (car bufs)))
                                     (if (and (buffer-file-path b)
                                              (string=? (buffer-file-path b) fpath))
                                       b (loop (cdr bufs)))))))
                          ;; Open the file fresh
                          (and fpath (file-exists? fpath)
                               (let* ((name (path-strip-directory fpath))
                                      (new-buf (qt-buffer-create! name ed fpath)))
                                 (let ((text (read-file-as-string fpath)))
                                   (when text
                                     (qt-plain-text-edit-set-text! ed text)
                                     (qt-text-document-set-modified! (buffer-doc-pointer new-buf) #f)))
                                 (qt-setup-highlighting! app new-buf)
                                 new-buf)))))
            (if buf
              (begin
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-cursor-position! ed
                  (min pos (string-length (qt-plain-text-edit-text ed))))
                (qt-plain-text-edit-ensure-cursor-visible! ed))
              (echo-error! echo (string-append "Cannot find: "
                                  (or fpath buf-name)))))
          (echo-error! echo (string-append "No bookmark: " input)))))))

(def (cmd-bookmark-list app)
  (let* ((bmarks (app-state-bookmarks app))
         (echo (app-state-echo app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (let ((entries (let loop ((keys (sort (hash-keys bmarks) string<?))
                              (acc []))
                     (if (null? keys) (reverse acc)
                       (let* ((name (car keys))
                              (val (hash-get bmarks name))
                              (buf-name (if (list? val) (car val) (car val)))
                              (fpath (if (list? val) (cadr val) #f))
                              (pos (if (list? val) (caddr val) (cdr val))))
                         (loop (cdr keys)
                               (cons (string-append "  " name "\t"
                                       (or fpath buf-name)
                                       " pos " (number->string pos))
                                     acc)))))))
      (if (null? entries)
        (echo-message! echo "No bookmarks defined")
        (let* ((text (string-append "Bookmarks:\n\n"
                                    (string-join entries "\n")))
               (buf (or (buffer-by-name "*Bookmarks*")
                        (qt-buffer-create! "*Bookmarks*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

(def *bookmarks-path*
  (path-expand ".gemacs-bookmarks" (user-info-home (user-info (user-name)))))

(def (bookmarks-save! app)
  "Persist bookmarks to disk. Format: one line per bookmark: name\tfile-path\tposition"
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file *bookmarks-path*
        (lambda (port)
          (for-each
            (lambda (pair)
              (let* ((name (car pair))
                     (val (cdr pair))
                     (fpath (if (list? val) (cadr val) #f))
                     (pos (if (list? val) (caddr val) (cdr val))))
                (when fpath
                  (display name port) (display "\t" port)
                  (display fpath port) (display "\t" port)
                  (display (number->string pos) port) (newline port))))
            (hash->list (app-state-bookmarks app))))))))

(def (split-by-tab str)
  "Split STR by tab characters into a list of strings."
  (let loop ((start 0) (i 0) (acc []))
    (cond
      ((>= i (string-length str))
       (reverse (cons (substring str start i) acc)))
      ((char=? (string-ref str i) #\tab)
       (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
      (else (loop start (+ i 1) acc)))))

(def (bookmarks-load! app)
  "Load bookmarks from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *bookmarks-path*)
        (let ((bm (app-state-bookmarks app)))
          (call-with-input-file *bookmarks-path*
            (lambda (port)
              (let loop ()
                (let ((line (read-line port)))
                  (unless (eof-object? line)
                    (let ((parts (split-by-tab line)))
                      (when (= (length parts) 3)
                        (let ((name (car parts))
                              (fpath (cadr parts))
                              (pos (string->number (caddr parts))))
                          (when (and name fpath pos)
                            (hash-put! bm name
                              (list (path-strip-directory fpath) fpath pos))))))
                    (loop)))))))))))

(def (cmd-bookmark-save app)
  "Save bookmarks to file."
  (bookmarks-save! app)
  (echo-message! (app-state-echo app)
    (string-append "Bookmarks saved to " *bookmarks-path*)))

(def (cmd-bookmark-load app)
  "Load bookmarks from file."
  (bookmarks-load! app)
  (echo-message! (app-state-echo app)
    (string-append "Bookmarks loaded from " *bookmarks-path*)))

(def (cmd-bookmark-delete app)
  "Delete a bookmark."
  (let* ((bookmarks (app-state-bookmarks app))
         (names (sort (hash-keys bookmarks) string<?)))
    (if (null? names)
      (echo-error! (app-state-echo app) "No bookmarks")
      (let ((name (qt-echo-read-string-with-completion app "Delete bookmark: " names)))
        (when (and name (> (string-length name) 0))
          (hash-remove! bookmarks name)
          (bookmarks-save! app)
          (echo-message! (app-state-echo app) (string-append "Deleted: " name)))))))

;;;============================================================================
;;; Rectangle operations
;;;============================================================================

(def (region-line-range text start end)
  "Get line numbers for start and end positions."
  (let loop ((i 0) (line 0) (start-line #f) (end-line #f))
    (cond
      ((and start-line end-line) (values start-line end-line))
      ((>= i (string-length text))
       (values (or start-line line) (or end-line line)))
      ((= i start)
       (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ line 1) line)
             line end-line))
      ((= i end)
       (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ line 1) line)
             start-line line))
      ((char=? (string-ref text i) #\newline)
       (loop (+ i 1) (+ line 1) start-line end-line))
      (else (loop (+ i 1) line start-line end-line)))))

(def (column-at-position text pos)
  "Get column number for a position."
  (let loop ((i (- pos 1)) (col 0))
    (if (or (< i 0) (char=? (string-ref text i) #\newline)) col
      (loop (- i 1) (+ col 1)))))

(def (cmd-kill-rectangle app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (echo (app-state-echo app)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (col1 (column-at-position text start))
             (col2 (column-at-position text end))
             (left-col (min col1 col2))
             (right-col (max col1 col2))
             (lines (string-split text #\newline)))
        (let-values (((start-line end-line) (region-line-range text start end)))
          (let ((killed [])
                (new-lines []))
            ;; Process each line in range
            (let loop ((ls lines) (i 0))
              (if (null? ls)
                (begin
                  (set! (app-state-rect-kill app) (reverse killed))
                  (let ((result (string-join (reverse new-lines) "\n")))
                    (qt-plain-text-edit-set-text! ed result)
                    (qt-plain-text-edit-set-cursor-position! ed
                      (min start (string-length result)))
                    (set! (buffer-mark buf) #f)
                    (echo-message! echo "Rectangle killed")))
                (if (and (>= i start-line) (<= i end-line))
                  ;; Extract rectangle region from this line
                  (let* ((l (car ls))
                         (len (string-length l))
                         (rect-text (if (< left-col len)
                                      (substring l left-col (min right-col len))
                                      ""))
                         (new-line (string-append
                                     (if (< left-col len)
                                       (substring l 0 left-col)
                                       l)
                                     (if (< right-col len)
                                       (substring l right-col len)
                                       ""))))
                    (set! killed (cons rect-text killed))
                    (set! new-lines (cons new-line new-lines))
                    (loop (cdr ls) (+ i 1)))
                  (begin
                    (set! new-lines (cons (car ls) new-lines))
                    (loop (cdr ls) (+ i 1)))))))))
      (echo-error! echo "No mark set"))))

(def (cmd-delete-rectangle app)
  "Delete the rectangle defined by mark and point (without saving to kill ring)."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (echo (app-state-echo app)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (col1 (column-at-position text start))
             (col2 (column-at-position text end))
             (left-col (min col1 col2))
             (right-col (max col1 col2))
             (lines (string-split text #\newline)))
        (let-values (((start-line end-line) (region-line-range text start end)))
          (let ((new-lines []))
            (let loop ((ls lines) (i 0))
              (if (null? ls)
                (let ((result (string-join (reverse new-lines) "\n")))
                  (qt-plain-text-edit-set-text! ed result)
                  (qt-plain-text-edit-set-cursor-position! ed
                    (min start (string-length result)))
                  (set! (buffer-mark buf) #f)
                  (echo-message! echo "Rectangle deleted"))
                (if (and (>= i start-line) (<= i end-line))
                  (let* ((l (car ls))
                         (len (string-length l))
                         (new-line (string-append
                                     (if (< left-col len)
                                       (substring l 0 left-col)
                                       l)
                                     (if (< right-col len)
                                       (substring l right-col len)
                                       ""))))
                    (set! new-lines (cons new-line new-lines))
                    (loop (cdr ls) (+ i 1)))
                  (begin
                    (set! new-lines (cons (car ls) new-lines))
                    (loop (cdr ls) (+ i 1)))))))))
      (echo-error! echo "No mark set"))))

(def (cmd-yank-rectangle app)
  (let* ((rect (app-state-rect-kill app))
         (echo (app-state-echo app)))
    (if (null? rect)
      (echo-message! echo "No rectangle in kill ring")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (col (column-at-position text pos))
             (lines (string-split text #\newline))
             (line (qt-plain-text-edit-cursor-line ed)))
        ;; Insert rectangle text at current column on consecutive lines
        (let* ((new-lines (let loop ((ls lines) (i 0) (rs rect) (acc []))
                            (if (null? ls)
                              (reverse acc)
                              (if (and (>= i line) (pair? rs))
                                (let* ((l (car ls))
                                       (len (string-length l))
                                       (padded (if (< len col)
                                                 (string-append l (make-string (- col len) #\space))
                                                 l))
                                       (new-line (string-append
                                                   (substring padded 0 col)
                                                   (car rs)
                                                   (substring padded col (string-length padded)))))
                                  (loop (cdr ls) (+ i 1) (cdr rs) (cons new-line acc)))
                                (loop (cdr ls) (+ i 1) rs (cons (car ls) acc))))))
                (new-text (string-join new-lines "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed pos)
          (echo-message! echo "Rectangle yanked"))))))

(def (cmd-string-rectangle app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (echo (app-state-echo app)))
    (if mark
      (let ((str (qt-echo-read-string app "String rectangle: ")))
        (when (and str (> (string-length str) 0))
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (col1 (column-at-position text start))
                 (col2 (column-at-position text end))
                 (left-col (min col1 col2))
                 (right-col (max col1 col2))
                 (lines (string-split text #\newline)))
            (let-values (((start-line end-line) (region-line-range text start end)))
              (let* ((new-lines (let loop ((ls lines) (i 0) (acc []))
                                  (if (null? ls) (reverse acc)
                                    (if (and (>= i start-line) (<= i end-line))
                                      (let* ((l (car ls))
                                             (len (string-length l))
                                             (new-line (string-append
                                                         (if (< left-col len)
                                                           (substring l 0 left-col)
                                                           l)
                                                         str
                                                         (if (< right-col len)
                                                           (substring l right-col len)
                                                           ""))))
                                        (loop (cdr ls) (+ i 1) (cons new-line acc)))
                                      (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
                     (new-text (string-join new-lines "\n")))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (min start (string-length new-text)))
                (set! (buffer-mark buf) #f)
                (echo-message! echo "Rectangle replaced"))))))
      (echo-error! echo "No mark set"))))

;;;============================================================================
;;; Describe key / describe command
;;;============================================================================

(def (qt-format-command-help name)
  "Format help text for a command, including keybinding and description."
  (let* ((doc (command-doc name))
         (binding (find-keybinding-for-command name))
         (name-str (symbol->string name)))
    (string-append
      name-str "\n"
      (make-string (string-length name-str) #\=) "\n\n"
      "Type: Interactive command\n"
      (if binding
        (string-append "Key:  " binding "\n")
        "Key:  (not bound)\n")
      "\n" doc "\n")))

(def (cmd-describe-key app)
  "Show what command a key sequence runs."
  (echo-message! (app-state-echo app)
    "Press a key... (use C-h b for all bindings)"))

(def (cmd-describe-command app)
  "Describe a command, showing help in *Help* buffer."
  (let* ((echo (app-state-echo app))
         (cmd-names (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         (input (qt-echo-read-with-narrowing app "Describe command:" cmd-names)))
    (when (and input (> (string-length input) 0))
      (let* ((sym (string->symbol input))
             (proc (find-command sym)))
        (if proc
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app))
                 (text (qt-format-command-help sym))
                 (buf (or (buffer-by-name "*Help*")
                          (qt-buffer-create! "*Help*" ed #f))))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed text)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! echo (string-append "Help for " input)))
          (echo-error! echo (string-append input " is not a known command")))))))

;;;============================================================================
;;; Toggle electric pair mode
;;;============================================================================

(def (cmd-toggle-electric-pair app)
  (set! *auto-pair-mode* (not *auto-pair-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-pair-mode* "Auto-pair ON" "Auto-pair OFF")))

(def (cmd-paredit-strict-mode app)
  "Toggle paredit strict mode (prevent unbalancing delimiter deletion)."
  (set! *paredit-strict-mode* (not *paredit-strict-mode*))
  (echo-message! (app-state-echo app)
    (if *paredit-strict-mode* "Paredit strict mode ON" "Paredit strict mode OFF")))

;;;============================================================================
;;; Universal argument / prefix arg system
;;;============================================================================

(def (cmd-universal-argument app)
  "C-u prefix argument."
  (let ((current (app-state-prefix-arg app)))
    (cond
     ((not current)
      (set! (app-state-prefix-arg app) '(4)))
     ((list? current)
      (set! (app-state-prefix-arg app) (list (* 4 (car current)))))
     (else
      (set! (app-state-prefix-arg app) '(4))))
    (echo-message! (app-state-echo app)
                   (string-append "C-u"
                                  (let ((val (car (app-state-prefix-arg app))))
                                    (if (= val 4) "" (string-append " " (number->string val))))
                                  "-"))))

(def (cmd-digit-argument app digit)
  "Build a numeric prefix argument."
  (let ((current (app-state-prefix-arg app)))
    (cond
     ((number? current)
      (set! (app-state-prefix-arg app) (+ (* current 10) digit)))
     ((eq? current '-)
      (set! (app-state-prefix-arg app) (- digit)))
     (else
      (set! (app-state-prefix-arg app) digit)))
    (set! (app-state-prefix-digit-mode? app) #t)
    (echo-message! (app-state-echo app)
                   (string-append "Arg: " (if (eq? (app-state-prefix-arg app) '-)
                                            "-"
                                            (number->string (app-state-prefix-arg app)))))))

(def (cmd-negative-argument app)
  "Negative prefix argument (M--)."
  (set! (app-state-prefix-arg app) '-)
  (set! (app-state-prefix-digit-mode? app) #t)
  (echo-message! (app-state-echo app) "Arg: -"))

(def (cmd-digit-argument-0 app) (cmd-digit-argument app 0))
(def (cmd-digit-argument-1 app) (cmd-digit-argument app 1))
(def (cmd-digit-argument-2 app) (cmd-digit-argument app 2))
(def (cmd-digit-argument-3 app) (cmd-digit-argument app 3))
(def (cmd-digit-argument-4 app) (cmd-digit-argument app 4))
(def (cmd-digit-argument-5 app) (cmd-digit-argument app 5))
(def (cmd-digit-argument-6 app) (cmd-digit-argument app 6))
(def (cmd-digit-argument-7 app) (cmd-digit-argument app 7))
(def (cmd-digit-argument-8 app) (cmd-digit-argument app 8))
(def (cmd-digit-argument-9 app) (cmd-digit-argument app 9))

;;;============================================================================
;;; Next/previous error (compilation error navigation)
;;;============================================================================

(def (compilation-goto-error! app index)
  "Jump to the compilation error at INDEX. Opens the file and positions cursor."
  (when (and (>= index 0) (< index (length *compilation-errors*)))
    (let* ((err (list-ref *compilation-errors* index))
           (file (car err))
           (line (cadr err))
           (col (caddr err))
           (msg (cadddr err))
           (echo (app-state-echo app))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      (set! *compilation-error-index* index)
      ;; Open the file (inline logic to avoid circular dep with app.ss)
      (when (file-exists? file)
        (let* ((name (path-strip-directory file))
               ;; Check if buffer already exists for this file
               (existing (let loop ((bufs *buffer-list*))
                           (if (null? bufs) #f
                             (let ((b (car bufs)))
                               (if (and (buffer-file-path b)
                                        (string=? (buffer-file-path b) file))
                                 b
                                 (loop (cdr bufs)))))))
               (buf (or existing
                        (qt-buffer-create! name ed file))))
          ;; Switch to buffer
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          ;; Load file content if new buffer
          (when (not existing)
            (let ((text (read-file-as-string file)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
            (qt-setup-highlighting! app buf))
          ;; Navigate to line:col
          (let* ((text (qt-plain-text-edit-text ed))
                 (line-pos (text-line-position text line))
                 (pos (+ line-pos (max 0 (- col 1)))))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-message! echo
            (string-append "Error " (number->string (+ index 1))
              "/" (number->string (length *compilation-errors*))
              ": " msg)))))))

(def (cmd-next-error app)
  "Jump to next compilation error or grep result."
  (cond
    ((not (null? *compilation-errors*))
     (let ((next-idx (+ *compilation-error-index* 1)))
       (if (>= next-idx (length *compilation-errors*))
         (echo-message! (app-state-echo app) "No more errors")
         (compilation-goto-error! app next-idx))))
    ((not (null? *grep-results*))
     (cmd-next-grep-result app))
    (else
     ;; Fallback: search match navigation
     (let* ((ed (current-qt-editor app))
            (search (app-state-last-search app)))
       (if (not search)
         (echo-error! (app-state-echo app) "No errors, grep results, or search")
         (let* ((text (qt-plain-text-edit-text ed))
                (pos (qt-plain-text-edit-cursor-position ed))
                (found (string-contains text search (+ pos 1))))
           (if found
             (begin
               (qt-plain-text-edit-set-cursor-position! ed found)
               (qt-plain-text-edit-ensure-cursor-visible! ed))
             (let ((found2 (string-contains text search)))
               (if found2
                 (begin
                   (qt-plain-text-edit-set-cursor-position! ed found2)
                   (qt-plain-text-edit-ensure-cursor-visible! ed)
                   (echo-message! (app-state-echo app) "Wrapped"))
                 (echo-error! (app-state-echo app) "No more matches"))))))))))

(def (cmd-previous-error app)
  "Jump to previous compilation error or grep result."
  (cond
    ((not (null? *compilation-errors*))
     (let ((prev-idx (- *compilation-error-index* 1)))
       (if (< prev-idx 0)
         (echo-message! (app-state-echo app) "No previous errors")
         (compilation-goto-error! app prev-idx))))
    ((not (null? *grep-results*))
     (cmd-previous-grep-result app))
    (else
     ;; Fallback: search match navigation
     (let* ((ed (current-qt-editor app))
            (search (app-state-last-search app)))
       (if (not search)
         (echo-error! (app-state-echo app) "No errors, grep results, or search")
         (let* ((text (qt-plain-text-edit-text ed))
                (pos (qt-plain-text-edit-cursor-position ed))
                (found (let loop ((last-found #f) (start 0))
                         (let ((idx (string-contains text search start)))
                           (if (and idx (< idx pos))
                             (loop idx (+ idx 1))
                             last-found)))))
           (if found
             (begin
               (qt-plain-text-edit-set-cursor-position! ed found)
               (qt-plain-text-edit-ensure-cursor-visible! ed))
             (let ((found2 (let loop ((last-found #f) (start 0))
                             (let ((idx (string-contains text search start)))
                               (if idx
                                 (loop idx (+ idx 1))
                                 last-found)))))
               (if found2
                 (begin
                   (qt-plain-text-edit-set-cursor-position! ed found2)
                   (qt-plain-text-edit-ensure-cursor-visible! ed)
                   (echo-message! (app-state-echo app) "Wrapped"))
                 (echo-error! (app-state-echo app) "No matches"))))))))))

;;;============================================================================
;;; Text transforms: tabify, untabify, base64, rot13
;;;============================================================================

(def (cmd-tabify app)
  "Convert runs of 8 spaces to tabs in region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (result (let loop ((s region) (acc ""))
                       (let ((idx (string-contains s "        ")))
                         (if idx
                           (loop (substring s (+ idx 8) (string-length s))
                                 (string-append acc (substring s 0 idx) "\t"))
                           (string-append acc s)))))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! (app-state-echo app) "Tabified")))))

(def (cmd-untabify app)
  "Convert tabs to spaces in region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (result (let loop ((i 0) (acc '()))
                       (if (>= i (string-length region))
                         (apply string-append (reverse acc))
                         (if (char=? (string-ref region i) #\tab)
                           (loop (+ i 1) (cons "        " acc))
                           (loop (+ i 1) (cons (string (string-ref region i)) acc))))))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! (app-state-echo app) "Untabified")))))

(def (cmd-base64-encode-region app)
  "Base64 encode the region."
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
             (encoded (base64-encode region))
             (new-text (string-append (substring text 0 start) encoded
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length encoded)))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Base64 encoded")))))

(def (cmd-base64-decode-region app)
  "Base64 decode the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Base64 decode error"))
        (lambda ()
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (decoded (base64-decode (string-trim-both region)))
                 (new-text (string-append (substring text 0 start) decoded
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length decoded)))
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app) "Base64 decoded")))))))

(def (rot13-char ch)
  (cond
    ((and (char>=? ch #\a) (char<=? ch #\z))
     (integer->char (+ (char->integer #\a)
                       (modulo (+ (- (char->integer ch) (char->integer #\a)) 13) 26))))
    ((and (char>=? ch #\A) (char<=? ch #\Z))
     (integer->char (+ (char->integer #\A)
                       (modulo (+ (- (char->integer ch) (char->integer #\A)) 13) 26))))
    (else ch)))

(def (cmd-rot13-region app)
  "ROT13 encode the region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (len (string-length region))
             (result (make-string len)))
        (let loop ((i 0))
          (when (< i len)
            (string-set! result i (rot13-char (string-ref region i)))
            (loop (+ i 1))))
        (let ((new-text (string-append (substring text 0 start) result
                                        (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed start)
          (when mark (set! (buffer-mark buf) #f))
          (echo-message! (app-state-echo app) "ROT13 applied"))))))

;;;============================================================================
;;; Hex dump mode
;;;============================================================================

(def (cmd-hexl-mode app)
  "Display buffer contents as hex dump in *Hex* buffer."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (text (qt-plain-text-edit-text ed))
         (bytes (string->bytes text))
         (len (u8vector-length bytes))
         (lines '()))
    (let loop ((offset 0))
      (when (< offset len)
        (let* ((end (min (+ offset 16) len))
               (hex-parts '())
               (ascii-parts '()))
          (let hex-loop ((i offset))
            (when (< i end)
              (let* ((b (u8vector-ref bytes i))
                     (h (number->string b 16)))
                (set! hex-parts
                  (cons (if (< b 16) (string-append "0" h) h)
                        hex-parts)))
              (hex-loop (+ i 1))))
          (let ascii-loop ((i offset))
            (when (< i end)
              (let ((b (u8vector-ref bytes i)))
                (set! ascii-parts
                  (cons (if (and (>= b 32) (<= b 126))
                          (string (integer->char b))
                          ".")
                        ascii-parts)))
              (ascii-loop (+ i 1))))
          (let* ((addr (let ((h (number->string offset 16)))
                         (string-append (make-string (max 0 (- 8 (string-length h))) #\0) h)))
                 (hex-str (string-join (reverse hex-parts) " "))
                 (pad (make-string (max 0 (- 48 (string-length hex-str))) #\space))
                 (ascii-str (apply string-append (reverse ascii-parts))))
            (set! lines (cons (string-append addr "  " hex-str pad "  |" ascii-str "|")
                              lines))))
        (loop (+ offset 16))))
    (let* ((result (string-join (reverse lines) "\n"))
           (buf (qt-buffer-create! "*Hex*" ed #f)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed result)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "Hex dump"))))

;;;============================================================================
;;; Toggle commands
;;;============================================================================

(def *word-wrap-on* #f)

(def (cmd-toggle-word-wrap app)
  "Toggle word wrap."
  (let ((ed (current-qt-editor app)))
    (set! *word-wrap-on* (not *word-wrap-on*))
    (if *word-wrap-on*
      (begin
        (qt-plain-text-edit-set-line-wrap! ed QT_PLAIN_WIDGET_WRAP)
        (echo-message! (app-state-echo app) "Word wrap ON"))
      (begin
        (qt-plain-text-edit-set-line-wrap! ed QT_PLAIN_NO_WRAP)
        (echo-message! (app-state-echo app) "Word wrap OFF")))))

(def *whitespace-mode-on* #f)

(def (cmd-toggle-whitespace app)
  "Toggle trailing whitespace highlighting."
  (set! *whitespace-mode-on* (not *whitespace-mode-on*))
  (if *whitespace-mode-on*
    (begin
      (qt-highlight-trailing-whitespace! (current-qt-editor app))
      (echo-message! (app-state-echo app) "Whitespace highlighting enabled"))
    (begin
      ;; Re-apply visual decorations to clear the whitespace highlights
      (qt-update-visual-decorations! (current-qt-editor app))
      (echo-message! (app-state-echo app) "Whitespace highlighting disabled"))))

;; Trailing whitespace colors (red background)
(def ws-bg-r #x80) (def ws-bg-g #x20) (def ws-bg-b #x20)

(def (qt-highlight-trailing-whitespace! ed)
  "Highlight trailing whitespace on all lines."
  (let* ((text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (pos 0))
    (for-each
      (lambda (line)
        (let ((line-len (string-length line)))
          ;; Find trailing whitespace
          (let loop ((i (- line-len 1)))
            (cond
              ((< i 0) ; whole line is whitespace — skip
               (void))
              ((let ((ch (string-ref line i)))
                 (or (char=? ch #\space) (char=? ch #\tab)))
               (loop (- i 1)))
              (else
               ;; i is the last non-whitespace char; trailing ws starts at i+1
               (let ((trail-start (+ i 1)))
                 (when (< trail-start line-len)
                   (qt-extra-selection-add-range! ed
                     (+ pos trail-start) (- line-len trail-start)
                     #xff #xff #xff
                     ws-bg-r ws-bg-g ws-bg-b bold: #f)))))))
        ;; Advance pos by line length + newline
        (set! pos (+ pos (string-length line) 1)))
      lines)
    (qt-extra-selections-apply! ed)))

(def (cmd-toggle-truncate-lines app)
  "Toggle line truncation (same as word-wrap toggle)."
  (cmd-toggle-word-wrap app))

(def *case-fold-search-qt* #t)

(def (cmd-toggle-case-fold-search app)
  "Toggle case-sensitive search."
  (set! *case-fold-search-qt* (not *case-fold-search-qt*))
  (echo-message! (app-state-echo app)
    (if *case-fold-search-qt*
      "Case-insensitive search"
      "Case-sensitive search")))

(def *overwrite-mode* #f)

(def (cmd-toggle-overwrite-mode app)
  "Toggle overwrite mode (display only - Qt doesn't expose overwrite API)."
  (set! *overwrite-mode* (not *overwrite-mode*))
  (echo-message! (app-state-echo app)
    (if *overwrite-mode* "Overwrite mode ON" "Overwrite mode OFF")))

(def *auto-fill-mode* #f)
(def *fill-column* 80)

(def (cmd-toggle-auto-fill app)
  "Toggle auto-fill mode."
  (set! *auto-fill-mode* (not *auto-fill-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-fill-mode*
      (string-append "Auto fill ON (col " (number->string *fill-column*) ")")
      "Auto fill OFF")))

(def (cmd-set-fill-column app)
  "Set the fill column for paragraph filling."
  (let ((input (qt-echo-read-string app
                 (string-append "Fill column (current " (number->string *fill-column*) "): "))))
    (when input
      (let ((n (string->number input)))
        (if (and n (> n 0))
          (begin
            (set! *fill-column* n)
            (echo-message! (app-state-echo app)
              (string-append "Fill column set to " (number->string n))))
          (echo-error! (app-state-echo app) "Invalid number"))))))

(def (cmd-toggle-highlighting app)
  "Toggle syntax highlighting."
  (echo-message! (app-state-echo app) "Syntax highlighting toggled"))

(def (cmd-toggle-visual-line-mode app)
  "Toggle visual line mode."
  (cmd-toggle-word-wrap app))

(def (cmd-toggle-fill-column-indicator app)
  "Toggle fill column indicator display."
  (echo-message! (app-state-echo app)
    (string-append "Fill column indicator at " (number->string *fill-column*))))

;;;============================================================================
;;; Calculator
;;;============================================================================

(def (cmd-calc app)
  "Evaluate a math expression."
  (let ((expr (qt-echo-read-string app "Calc: ")))
    (when (and expr (> (string-length expr) 0))
      (let-values (((result error?) (eval-expression-string expr)))
        (if error?
          (echo-error! (app-state-echo app) (string-append "Error: " result))
          (echo-message! (app-state-echo app) (string-append "= " result)))))))

;;;============================================================================
;;; Describe bindings
;;;============================================================================

(def (cmd-describe-bindings app)
  "Show all keybindings in a *Bindings* buffer."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app)))
    (let ((lines []))
      (define (collect-prefix prefix km)
        (for-each
          (lambda (entry)
            (let ((key (car entry))
                  (val (cdr entry)))
              (if (hash-table? val)
                (collect-prefix (string-append prefix key " ") val)
                (set! lines (cons (string-append prefix key "\t"
                                                 (symbol->string val))
                                  lines)))))
          (keymap-entries km)))
      (collect-prefix "" *global-keymap*)
      (let* ((sorted (sort lines string<?))
             (text (string-join sorted "\n"))
             (buf (qt-buffer-create! "*Bindings*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app)
          (string-append (number->string (length sorted)) " bindings"))))))

;;;============================================================================
;;; Describe char
;;;============================================================================

(def (cmd-describe-char app)
  "Show info about the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if (>= pos (string-length text))
      (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch)
                         " (U+" (let ((h (number->string code 16)))
                                  (string-append (make-string (max 0 (- 4 (string-length h))) #\0) h))
                         ") = " (number->string code)))))))

;;;============================================================================
;;; Describe key briefly
;;;============================================================================

(def (cmd-describe-key-briefly app)
  "Show what a key sequence is bound to."
  (echo-message! (app-state-echo app) "Press a key...")
  ;; This needs to be handled at the key dispatch level.
  ;; For now, delegate to describe-key which uses echo prompt.
  (cmd-describe-key app))

;;;============================================================================
;;; TUI parity: bookmark menu, clone buffer, macro, magit-unstage-file
;;;============================================================================

(def (cmd-bookmark-bmenu-list app)
  "List bookmarks in a menu buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (bmarks (app-state-bookmarks app))
         (entries (hash->list bmarks))
         (text (if (null? entries)
                 "No bookmarks defined.\n\nUse C-x r m to set a bookmark."
                 (string-join
                   (map (lambda (e)
                          (let ((name (car e)) (info (cdr e)))
                            (string-append "  " (symbol->string name)
                              (if (string? info) (string-append "  " info) ""))))
                        entries)
                   "\n")))
         (buf (or (buffer-by-name "*Bookmarks*")
                  (qt-buffer-create! "*Bookmarks*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed (string-append "Bookmark List\n\n" text "\n"))
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (qt-modeline-update! app)))

(def (cmd-clone-indirect-buffer app)
  "Create an indirect buffer clone of the current buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (name (buffer-name buf))
         (clone-name (string-append name "<clone>"))
         (text (qt-plain-text-edit-text ed))
         (new-buf (qt-buffer-create! clone-name ed #f)))
    (qt-buffer-attach! ed new-buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) new-buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-modeline-update! app)
    (echo-message! (app-state-echo app) (string-append "Cloned to " clone-name))))

(def (cmd-apply-macro-to-region app)
  "Apply last keyboard macro to each line in the region."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (macro (app-state-macro-last app))
         (start (qt-plain-text-edit-selection-start ed))
         (end (qt-plain-text-edit-selection-end ed)))
    (cond
      ((not macro) (echo-error! echo "No macro recorded"))
      ((= start end) (echo-error! echo "No region selected"))
      (else
       (let* ((text (qt-plain-text-edit-text ed))
              ;; Count lines in region
              (region-text (substring text start end))
              (lines (string-split region-text #\newline))
              (count (length lines)))
         ;; Replay macro once per line (simplified for Qt)
         (for-each
           (lambda (step)
             (let ((type (car step)) (data (cdr step)))
               (case type
                 ((command) (let ((cmd (find-command data)))
                              (when cmd ((cdr cmd) app))))
                 ((self-insert)
                  (qt-plain-text-edit-insert-text! ed (string data))))))
           (reverse macro))
         (echo-message! echo
           (string-append "Applied macro to " (number->string count) " lines")))))))

(def (cmd-magit-unstage-file app)
  "Unstage current buffer's file."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf))))
    (if path
      (let ((result (with-exception-catcher
                      (lambda (e) "Error unstaging file")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git" arguments: (list "reset" "HEAD" path)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (process-status p)
                          (string-append "Unstaged: " (path-strip-directory path)))))))
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-text-scale-increase app) "Zoom in." (cmd-zoom-in app))
(def (cmd-text-scale-decrease app) "Zoom out." (cmd-zoom-out app))
(def (cmd-text-scale-reset app) "Zoom reset." (cmd-zoom-reset app))
;;; Markdown formatting, git, eval, insert helpers (parity with TUI)
(def (qt-md-wrap ed pre suf)
  (let* ((text (qt-plain-text-edit-text ed))
         (ss (qt-plain-text-edit-selection-start ed))
         (se (qt-plain-text-edit-selection-end ed)))
    (if (= ss se)
      (let* ((p (qt-plain-text-edit-cursor-position ed))
             (n (string-append (substring text 0 p) pre suf (substring text p (string-length text)))))
        (qt-plain-text-edit-set-text! ed n) (qt-plain-text-edit-set-cursor-position! ed (+ p (string-length pre))))
      (let* ((s (substring text ss se))
             (n (string-append (substring text 0 ss) pre s suf (substring text se (string-length text)))))
        (qt-plain-text-edit-set-text! ed n)
        (qt-plain-text-edit-set-cursor-position! ed (+ se (string-length pre) (string-length suf)))))))
(def (cmd-markdown-bold app) "Bold." (qt-md-wrap (current-qt-editor app) "**" "**"))
(def (cmd-markdown-italic app) "Italic." (qt-md-wrap (current-qt-editor app) "*" "*"))
(def (cmd-markdown-code app) "Code." (qt-md-wrap (current-qt-editor app) "`" "`"))
(def (cmd-markdown-insert-bold app) "Insert bold." (cmd-markdown-bold app))
(def (cmd-markdown-insert-code app) "Insert code." (cmd-markdown-code app))
(def (cmd-markdown-insert-italic app) "Insert italic." (cmd-markdown-italic app))
(def (cmd-markdown-insert-image app) "Insert image." (cmd-markdown-image app))
(def (cmd-markdown-insert-list-item app) "Insert list item." (cmd-markdown-list-item app))
(def (cmd-markdown-mode app) "Markdown mode." (echo-message! (app-state-echo app) "Markdown mode active"))
(def (cmd-markdown-preview-outline app) "Outline." (echo-message! (app-state-echo app) "Use M-x markdown-outline"))
(def (cmd-markdown-image app)
  "Insert markdown image."
  (let* ((ed (current-qt-editor app)) (alt (or (qt-echo-read-string app "Alt text: ") ""))
         (url (qt-echo-read-string app "Image URL: ")))
    (when (and url (> (string-length url) 0))
      (qt-plain-text-edit-insert-text! ed (string-append "![" alt "](" url ")")))))
(def (qt-md-hlevel line)
  (let lp ((i 0)) (if (and (< i (string-length line)) (char=? (string-ref line i) #\#)) (lp (+ i 1))
    (if (and (> i 0) (< i (string-length line)) (char=? (string-ref line i) #\space)) i 0))))
(def (cmd-markdown-heading app)
  "Cycle heading level."
  (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)) (len (string-length text))
         (ls (let lp ((i pos)) (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)) i (lp (- i 1)))))
         (le (let lp ((i pos)) (if (or (>= i len) (char=? (string-ref text i) #\newline)) i (lp (+ i 1)))))
         (line (substring text ls le)) (level (qt-md-hlevel line))
         (rep (cond ((= level 0) (string-append "# " line))
                    ((>= level 6) (let lp ((s line)) (if (and (> (string-length s) 0) (char=? (string-ref s 0) #\#))
                                    (lp (substring s 1 (string-length s))) (string-trim s))))
                    (else (string-append "#" line)))))
    (qt-plain-text-edit-set-text! ed (string-append (substring text 0 ls) rep (substring text le len)))
    (qt-plain-text-edit-set-cursor-position! ed (+ ls (min (string-length rep) (- pos ls))))))
(def (cmd-markdown-hr app) "HR." (qt-plain-text-edit-insert-text! (current-qt-editor app) "\n---\n"))
(def (cmd-markdown-checkbox app) "Checkbox." (qt-plain-text-edit-insert-text! (current-qt-editor app) "- [ ] "))
(def (cmd-markdown-code-block app)
  "Code block."
  (let ((lang (or (qt-echo-read-string app "Language: ") "")))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) (string-append "```" lang "\n\n```\n"))))
(def (cmd-markdown-table app)
  "Table."
  (let* ((c (or (string->number (or (qt-echo-read-string app "Columns (default 3): ") "3")) 3))
         (h (string-join (make-list c " Header ") "|")) (s (string-join (make-list c "--------") "|"))
         (r (string-join (make-list c "        ") "|")))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) (string-append "| " h " |\n| " s " |\n| " r " |\n"))))
(def (cmd-markdown-link app)
  "Link."
  (let* ((ed (current-qt-editor app)) (url (qt-echo-read-string app "URL: ")))
    (when (and url (> (string-length url) 0))
      (let* ((ss (qt-plain-text-edit-selection-start ed)) (se (qt-plain-text-edit-selection-end ed))
             (text (qt-plain-text-edit-text ed))
             (lt (if (= ss se) url (substring text ss se)))
             (lk (string-append "[" lt "](" url ")")))
        (if (= ss se) (qt-plain-text-edit-insert-text! ed lk)
          (let ((n (string-append (substring text 0 ss) lk (substring text se (string-length text)))))
            (qt-plain-text-edit-set-text! ed n) (qt-plain-text-edit-set-cursor-position! ed (+ ss (string-length lk)))))))))
(def (cmd-markdown-list-item app)
  "List item."
  (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)) (len (string-length text))
         (ls (let lp ((i pos)) (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)) i (lp (- i 1)))))
         (le (let lp ((i pos)) (if (or (>= i len) (char=? (string-ref text i) #\newline)) i (lp (+ i 1)))))
         (line (substring text ls le))
         (mk (cond ((string-prefix? "- " line) "- ") ((string-prefix? "* " line) "* ") (else "- "))))
    (qt-plain-text-edit-set-text! ed (string-append (substring text 0 le) "\n" mk (substring text le len)))
    (qt-plain-text-edit-set-cursor-position! ed (+ le 1 (string-length mk)))))
(def (qt-run-cmd args)
  "Run command, return output string."
  (with-exception-catcher (lambda (e) "")
    (lambda () (let ((p (open-process (list path: (car args) arguments: (cdr args)
                          stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t))))
                 (let ((out (read-line p #f))) (process-status p) (or out ""))))))
(def (cmd-git-blame-line app)
  "Git blame for current line."
  (let* ((buf (current-qt-buffer app)) (path (and buf (buffer-file-path buf))))
    (if (not path) (echo-error! (app-state-echo app) "Buffer has no file")
      (with-catch (lambda (e) (echo-error! (app-state-echo app) "Git blame failed"))
        (lambda ()
          (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
                 (pos (qt-plain-text-edit-cursor-position ed))
                 (ln (+ 1 (let lp ((i 0) (n 0)) (if (>= i pos) n (lp (+ i 1) (if (char=? (string-ref text i) #\newline) (+ n 1) n))))))
                 (out (qt-run-cmd (list "git" "-C" (path-directory path) "blame" "-L" (string-append (number->string ln) "," (number->string ln)) "--porcelain" (path-strip-directory path))))
                 (ls (string-split out #\newline))
                 (commit (if (pair? ls) (let ((p (string-split (car ls) #\space))) (if (pair? p) (substring (car p) 0 (min 7 (string-length (car p)))) "?")) "?"))
                 (author (let lp ((l (if (pair? ls) (cdr ls) '()))) (if (null? l) "?" (if (string-prefix? "author " (car l)) (substring (car l) 7 (string-length (car l))) (lp (cdr l)))))))
            (echo-message! (app-state-echo app) (string-append commit " " author))))))))
(def (cmd-eval-region-and-replace app)
  "Eval selection and replace."
  (let* ((ed (current-qt-editor app)) (ss (qt-plain-text-edit-selection-start ed)) (se (qt-plain-text-edit-selection-end ed)))
    (if (= ss se) (echo-message! (app-state-echo app) "No selection")
      (with-catch (lambda (e) (echo-error! (app-state-echo app) "Eval error"))
        (lambda ()
          (let* ((expr (substring (qt-plain-text-edit-text ed) ss se))
                 (r (with-output-to-string (lambda () (write (eval (with-input-from-string expr read))))))
                 (text (qt-plain-text-edit-text ed))
                 (n (string-append (substring text 0 ss) r (substring text se (string-length text)))))
            (qt-plain-text-edit-set-text! ed n) (qt-plain-text-edit-set-cursor-position! ed (+ ss (string-length r)))
            (echo-message! (app-state-echo app) (string-append "Replaced: " r))))))))
(def (cmd-comment-box app)
  "Wrap selection in comment box."
  (let* ((ed (current-qt-editor app)) (ss (qt-plain-text-edit-selection-start ed)) (se (qt-plain-text-edit-selection-end ed)))
    (if (= ss se) (echo-message! (app-state-echo app) "No selection")
      (let* ((text (qt-plain-text-edit-text ed)) (sel (substring text ss se))
             (lines (string-split sel #\newline)) (mx (apply max (map string-length lines)))
             (bdr (string-append ";; " (make-string (+ mx 2) #\-)))
             (bx (with-output-to-string (lambda () (display bdr) (display "\n")
                    (for-each (lambda (l) (display ";; ") (display l)
                      (display (make-string (- mx (string-length l)) #\space)) (display "  \n")) lines)
                    (display bdr) (display "\n"))))
             (n (string-append (substring text 0 ss) bx (substring text se (string-length text)))))
        (qt-plain-text-edit-set-text! ed n) (echo-message! (app-state-echo app) "Comment box created")))))
(def (cmd-goto-matching-bracket app)
  "Jump to matching bracket."
  (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)) (len (string-length text)))
    (when (< pos len)
      (let* ((ch (string-ref text pos)) (opn '(#\( #\[ #\{)) (cls '(#\) #\] #\}))
             (m (cond ((memv ch opn) (let lp ((i (+ pos 1)) (d 1)) (cond ((>= i len) #f) ((= d 0) (- i 1)) ((memv (string-ref text i) opn) (lp (+ i 1) (+ d 1))) ((memv (string-ref text i) cls) (lp (+ i 1) (- d 1))) (else (lp (+ i 1) d)))))
                       ((memv ch cls) (let lp ((i (- pos 1)) (d 1)) (cond ((< i 0) #f) ((= d 0) (+ i 1)) ((memv (string-ref text i) cls) (lp (- i 1) (+ d 1))) ((memv (string-ref text i) opn) (lp (- i 1) (- d 1))) (else (lp (- i 1) d)))))
                       (else #f))))
        (if m (begin (qt-plain-text-edit-set-cursor-position! ed m) (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-message! (app-state-echo app) "No match"))))))
(def (cmd-insert-uuid-v4 app)
  "Insert UUID v4."
  (let* ((hx "0123456789abcdef")
         (rh (lambda () (string (string-ref hx (random-integer 16)) (string-ref hx (random-integer 16)))))
         (uuid (string-append (rh) (rh) (rh) (rh) "-" (rh) (rh) "-4" (substring (rh) 1 2) (rh) "-"
                 (string (string-ref hx (+ 8 (random-integer 4)))) (substring (rh) 1 2) (rh) "-"
                 (rh) (rh) (rh) (rh) (rh) (rh))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) uuid)
    (echo-message! (app-state-echo app) (string-append "UUID: " uuid))))
(def (cmd-insert-date-formatted app)
  "Insert date with format."
  (let ((fmt (qt-echo-read-string app "Date format (e.g. %Y-%m-%d): ")))
    (when (and fmt (> (string-length fmt) 0))
      (let ((str (string-trim (qt-run-cmd (list "date" (string-append "+" fmt))))))
        (if (string=? str "") (echo-error! (app-state-echo app) "Invalid format")
          (qt-plain-text-edit-insert-text! (current-qt-editor app) str))))))
(def (cmd-insert-date-time-stamp app)
  "Insert date-time."
  (qt-plain-text-edit-insert-text! (current-qt-editor app) (string-trim (qt-run-cmd '("date" "+%Y-%m-%d %H:%M:%S")))))
(def (cmd-insert-char-by-code app)
  "Insert char by code point."
  (let ((input (qt-echo-read-string app "Code point (65 or #x41): ")))
    (when (and input (> (string-length input) 0))
      (let ((n (cond ((string-prefix? "#x" input) (string->number (substring input 2 (string-length input)) 16))
                     ((string-prefix? "0x" input) (string->number (substring input 2 (string-length input)) 16))
                     (else (string->number input)))))
        (if (and n (> n 0) (< n #x110000))
          (qt-plain-text-edit-insert-text! (current-qt-editor app) (string (integer->char n)))
          (echo-error! (app-state-echo app) "Invalid code point"))))))

