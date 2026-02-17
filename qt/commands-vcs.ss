;;; -*- Gerbil -*-
;;; Qt commands vcs - batch commands, magit, search, wgrep, eval
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :std/pregexp
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
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide)

;;;============================================================================

(def (cmd-transpose-sexps app)
  "Transpose the s-expressions before and after point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find the sexp before point (backward)
    (let* ((before-end pos)
           (before-start
             (let loop ((i (- pos 1)) (depth 0))
               (cond
                 ((< i 0) 0)
                 ((and (memq (string-ref text i) '(#\) #\] #\})) (= depth 0))
                  ;; Find matching open
                  (let ((close-ch (string-ref text i))
                        (open-ch (cond ((char=? (string-ref text i) #\)) #\()
                                       ((char=? (string-ref text i) #\]) #\[)
                                       (else #\{))))
                    (let inner ((j (- i 1)) (d 1))
                      (cond
                        ((< j 0) 0)
                        ((char=? (string-ref text j) close-ch) (inner (- j 1) (+ d 1)))
                        ((char=? (string-ref text j) open-ch)
                         (if (= d 1) j (inner (- j 1) (- d 1))))
                        (else (inner (- j 1) d))))))
                 ((and (char-alphabetic? (string-ref text i)) (= depth 0))
                  (let loop2 ((j i))
                    (if (and (> j 0) (or (char-alphabetic? (string-ref text (- j 1)))
                                         (char-numeric? (string-ref text (- j 1)))
                                         (char=? (string-ref text (- j 1)) #\-)))
                      (loop2 (- j 1)) j)))
                 ((char-whitespace? (string-ref text i)) (loop (- i 1) depth))
                 (else i))))
           ;; Find the sexp after point (forward)
           (after-start pos)
           (after-end
             (let loop ((i pos))
               (cond
                 ((>= i len) len)
                 ((memq (string-ref text i) '(#\( #\[ #\{))
                  (let ((open-ch (string-ref text i))
                        (close-ch (cond ((char=? (string-ref text i) #\() #\))
                                        ((char=? (string-ref text i) #\[) #\])
                                        (else #\}))))
                    (let inner ((j (+ i 1)) (d 1))
                      (cond
                        ((>= j len) len)
                        ((char=? (string-ref text j) open-ch) (inner (+ j 1) (+ d 1)))
                        ((char=? (string-ref text j) close-ch)
                         (if (= d 1) (+ j 1) (inner (+ j 1) (- d 1))))
                        (else (inner (+ j 1) d))))))
                 ((char-alphabetic? (string-ref text i))
                  (let loop2 ((j (+ i 1)))
                    (if (and (< j len) (or (char-alphabetic? (string-ref text j))
                                           (char-numeric? (string-ref text j))
                                           (char=? (string-ref text j) #\-)))
                      (loop2 (+ j 1)) j)))
                 ((char-whitespace? (string-ref text i)) (loop (+ i 1)))
                 (else (+ i 1))))))
      (when (and (< before-start before-end) (< after-start after-end))
        (let ((sexp1 (substring text before-start before-end))
              (mid (substring text before-end after-start))
              (sexp2 (substring text after-start after-end)))
          (qt-plain-text-edit-set-selection! ed before-start after-end)
          (qt-plain-text-edit-remove-selected-text! ed)
          (qt-plain-text-edit-insert-text! ed (string-append sexp2 mid sexp1))
          (echo-message! (app-state-echo app) "S-expressions transposed"))))))

(def (cmd-transpose-paragraphs app)
  "Transpose the paragraph before point with the one after."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let* ((para-start
             (let loop ((i (- pos 1)))
               (cond
                 ((< i 0) 0)
                 ((and (char=? (string-ref text i) #\newline)
                       (or (= i 0)
                           (and (> i 0) (char=? (string-ref text (- i 1)) #\newline))))
                  (+ i 1))
                 (else (loop (- i 1))))))
           (para-end
             (let loop ((i pos))
               (cond
                 ((>= i len) len)
                 ((and (char=? (string-ref text i) #\newline)
                       (< (+ i 1) len)
                       (char=? (string-ref text (+ i 1)) #\newline))
                  i)
                 (else (loop (+ i 1))))))
           (next-start
             (let loop ((i (+ para-end 1)))
               (cond
                 ((>= i len) #f)
                 ((not (or (char=? (string-ref text i) #\newline)
                           (char=? (string-ref text i) #\space)))
                  i)
                 (else (loop (+ i 1))))))
           (next-end
             (if next-start
               (let loop ((i next-start))
                 (cond
                   ((>= i len) len)
                   ((and (char=? (string-ref text i) #\newline)
                         (< (+ i 1) len)
                         (char=? (string-ref text (+ i 1)) #\newline))
                    i)
                   (else (loop (+ i 1)))))
               #f)))
      (if (and next-start next-end)
        (let* ((para1 (substring text para-start para-end))
               (sep (substring text para-end next-start))
               (para2 (substring text next-start next-end)))
          (qt-plain-text-edit-set-selection! ed para-start next-end)
          (qt-plain-text-edit-remove-selected-text! ed)
          (qt-plain-text-edit-insert-text! ed (string-append para2 sep para1))
          (echo-message! (app-state-echo app) "Paragraphs transposed"))
        (echo-message! (app-state-echo app) "No next paragraph to transpose")))))

(def (cmd-zap-up-to-char app)
  "Kill text up to but not including the specified character."
  (let ((input (qt-echo-read-string app "Zap up to char: ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (found (let loop ((i (+ pos 1)))
                      (cond
                        ((>= i (string-length text)) #f)
                        ((char=? (string-ref text i) ch) i)
                        (else (loop (+ i 1)))))))
        (if found
          (let ((killed (substring text pos found)))
            (qt-plain-text-edit-set-selection! ed pos found)
            (qt-plain-text-edit-remove-selected-text! ed)
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
              (string-append "Zapped to '" (string ch) "'")))
          (echo-error! (app-state-echo app)
            (string-append "'" (string ch) "' not found")))))))

(def (cmd-zap-to-char-inclusive app)
  "Kill text up to and including the specified character."
  (let ((input (qt-echo-read-string app "Zap to char (inclusive): ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (found (let loop ((i (+ pos 1)))
                      (cond
                        ((>= i (string-length text)) #f)
                        ((char=? (string-ref text i) ch) (+ i 1))
                        (else (loop (+ i 1)))))))
        (if found
          (let ((killed (substring text pos found)))
            (qt-plain-text-edit-set-selection! ed pos found)
            (qt-plain-text-edit-remove-selected-text! ed)
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app))))
          (echo-error! (app-state-echo app)
            (string-append "'" (string ch) "' not found")))))))

(def *qt-last-regexp-search* "")

(def (cmd-search-forward-regexp app)
  "Forward regex search (C-M-s). Uses pregexp."
  (let* ((default *qt-last-regexp-search*)
         (prompt (if (string=? default "")
                   "Regexp search: "
                   (string-append "Regexp search [" default "]: ")))
         (input (qt-echo-read-string app prompt)))
    (when input
      (let ((pattern (if (string=? input "") default input)))
        (when (> (string-length pattern) 0)
          (set! *qt-last-regexp-search* pattern)
          (set! (app-state-last-search app) pattern)
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (cur-pos (qt-plain-text-edit-cursor-position ed))
                 ;; Search forward from cursor
                 (match (pregexp-match-positions pattern text cur-pos)))
            (if match
              (let* ((pair (car match))
                     (start (car pair))
                     (end (cdr pair)))
                (qt-plain-text-edit-set-cursor-position! ed end)
                (qt-plain-text-edit-ensure-cursor-visible! ed)
                ;; Highlight match
                (qt-update-visual-decorations! ed)
                (qt-extra-selection-add-range! ed start (- end start)
                  #x00 #x00 #x00 #x00 #xdd #xff bold: #t)
                (qt-extra-selections-apply! ed))
              ;; Try wrapping from start
              (let ((match2 (pregexp-match-positions pattern text 0)))
                (if match2
                  (let* ((pair2 (car match2))
                         (start2 (car pair2))
                         (end2 (cdr pair2)))
                    (qt-plain-text-edit-set-cursor-position! ed end2)
                    (qt-plain-text-edit-ensure-cursor-visible! ed)
                    (qt-update-visual-decorations! ed)
                    (qt-extra-selection-add-range! ed start2 (- end2 start2)
                      #x00 #x00 #x00 #x00 #xdd #xff bold: #t)
                    (qt-extra-selections-apply! ed)
                    (echo-message! (app-state-echo app) "Wrapped"))
                  (echo-error! (app-state-echo app)
                    (string-append "No regexp match: " pattern)))))))))))

(def (cmd-query-replace-regexp app)
  "Interactive regex query-replace (C-M-%). Uses pregexp."
  (let ((from (qt-echo-read-string app "Regexp replace: ")))
    (when (and from (> (string-length from) 0))
      (let ((to (qt-echo-read-string app
                  (string-append "Replace regexp \"" from "\" with: "))))
        (when to
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (new-text (pregexp-replace* from text to))
                 ;; Count matches
                 (count (let loop ((s text) (n 0))
                          (let ((m (pregexp-match-positions from s)))
                            (if m
                              (let ((end (cdr (car m))))
                                (loop (substring s (max end 1) (string-length s))
                                      (+ n 1)))
                              n)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (echo-message! (app-state-echo app)
              (string-append "Replaced " (number->string count)
                             " occurrence" (if (= count 1) "" "s")))))))))

(def (cmd-copy-from-below app)
  "Copy character from the line below."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (cur-line (qt-plain-text-edit-cursor-line ed))
         (col (- pos (line-start-position text cur-line))))
    (if (< (+ cur-line 1) (length lines))
      (let ((below (list-ref lines (+ cur-line 1))))
        (if (< col (string-length below))
          (qt-plain-text-edit-insert-text! ed (string (string-ref below col)))
          (echo-message! (app-state-echo app) "Line below too short")))
      (echo-message! (app-state-echo app) "No line below"))))

(def (cmd-copy-symbol-at-point app)
  "Copy the symbol at point to kill ring."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((sym-char? (lambda (ch)
                          (or (char-alphabetic? ch) (char-numeric? ch)
                              (char=? ch #\-) (char=? ch #\_)
                              (char=? ch #\!) (char=? ch #\?)
                              (char=? ch #\*))))
             (start (let loop ((i pos))
                      (if (and (> i 0) (sym-char? (string-ref text (- i 1))))
                        (loop (- i 1)) i)))
             (end (let loop ((i pos))
                    (if (and (< i len) (sym-char? (string-ref text i)))
                      (loop (+ i 1)) i)))
             (sym (substring text start end)))
        (if (> (string-length sym) 0)
          (begin
            (set! (app-state-kill-ring app) (cons sym (app-state-kill-ring app)))
            (echo-message! (app-state-echo app) (string-append "Copied: " sym)))
          (echo-message! (app-state-echo app) "No symbol at point"))))))

(def (cmd-copy-word-at-point app)
  "Copy word at point to kill ring (alias for copy-word)."
  (cmd-copy-word app))

(def (cmd-delete-to-end-of-line app)
  "Delete from point to end of line (without killing)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-end (+ (line-start-position text line) (string-length (list-ref lines line)))))
    (when (> line-end pos)
      (qt-plain-text-edit-set-selection! ed pos line-end)
      (qt-plain-text-edit-remove-selected-text! ed))))

(def (cmd-delete-to-beginning-of-line app)
  "Delete from point to beginning of line (without killing)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (line-start (line-start-position text line)))
    (when (> pos line-start)
      (qt-plain-text-edit-set-selection! ed line-start pos)
      (qt-plain-text-edit-remove-selected-text! ed))))

(def (cmd-delete-horizontal-space-forward app)
  "Delete spaces and tabs after point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (end (let loop ((i pos))
                (if (and (< i len) (or (char=? (string-ref text i) #\space)
                                       (char=? (string-ref text i) #\tab)))
                  (loop (+ i 1)) i))))
    (when (> end pos)
      (qt-plain-text-edit-set-selection! ed pos end)
      (qt-plain-text-edit-remove-selected-text! ed))))

(def (cmd-cycle-spacing app)
  "Cycle between one space, no spaces, and original spacing."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         ;; Find whitespace boundaries around point
         (start (let loop ((i pos))
                  (if (and (> i 0) (or (char=? (string-ref text (- i 1)) #\space)
                                       (char=? (string-ref text (- i 1)) #\tab)))
                    (loop (- i 1)) i)))
         (end (let loop ((i pos))
                (if (and (< i len) (or (char=? (string-ref text i) #\space)
                                       (char=? (string-ref text i) #\tab)))
                  (loop (+ i 1)) i)))
         (ws-len (- end start)))
    (cond
      ((> ws-len 1) ;; Multiple spaces → one space
       (qt-plain-text-edit-set-selection! ed start end)
       (qt-plain-text-edit-remove-selected-text! ed)
       (qt-plain-text-edit-insert-text! ed " "))
      ((= ws-len 1) ;; One space → no spaces
       (qt-plain-text-edit-set-selection! ed start end)
       (qt-plain-text-edit-remove-selected-text! ed))
      (else ;; No spaces → one space
       (qt-plain-text-edit-insert-text! ed " ")))))

(def (cmd-swap-windows app)
  "Swap buffers between two windows."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    (if (>= (length wins) 2)
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (b1 (qt-edit-window-buffer w1))
             (b2 (qt-edit-window-buffer w2))
             (e1 (qt-edit-window-editor w1))
             (e2 (qt-edit-window-editor w2)))
        ;; Swap buffer associations
        (set! (qt-edit-window-buffer w1) b2)
        (set! (qt-edit-window-buffer w2) b1)
        ;; Swap editor content
        (let ((t1 (qt-plain-text-edit-text e1))
              (t2 (qt-plain-text-edit-text e2)))
          (qt-plain-text-edit-set-text! e1 t2)
          (qt-plain-text-edit-set-text! e2 t1))
        (echo-message! (app-state-echo app) "Windows swapped"))
      (echo-message! (app-state-echo app) "Only one window"))))

(def (cmd-rotate-windows app)
  "Rotate window layout by cycling to next window."
  (let ((wins (qt-frame-windows (app-state-frame app))))
    (if (>= (length wins) 2)
      (begin
        (qt-frame-other-window! (app-state-frame app))
        (echo-message! (app-state-echo app) "Windows rotated"))
      (echo-message! (app-state-echo app) "Only one window"))))

;; cmd-toggle-line-comment is defined later (near end of file)
;; cmd-narrow-to-defun is defined earlier (near narrowing section)

(def (cmd-fold-all app)
  "Fold all top-level forms."
  (echo-message! (app-state-echo app) "Fold all (not supported in QPlainTextEdit)"))

(def (cmd-unfold-all app)
  "Unfold all forms."
  (echo-message! (app-state-echo app) "Unfold all (not supported in QPlainTextEdit)"))

(def (cmd-toggle-auto-pair-mode app)
  "Toggle automatic pairing of brackets/quotes."
  (set! *auto-pair-mode* (not *auto-pair-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-pair-mode* "Auto-pair mode ON" "Auto-pair mode OFF")))

(def (cmd-mark-page app)
  "Select the entire buffer (alias for mark-whole-buffer)."
  (cmd-select-all app))

(def (cmd-mark-whole-buffer app)
  "Select the entire buffer."
  (cmd-select-all app))

(def (cmd-view-lossage app)
  "Display recent keystrokes in a *Help* buffer."
  (let* ((text (string-append "Recent keystrokes:\n\n"
                              (key-lossage->string app)
                              "\n"))
         (fr (app-state-frame app))
         (ed (qt-current-editor fr))
         (buf (qt-buffer-create! "*Help*" ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

(def (cmd-bookmark-rename app)
  "Rename a bookmark."
  (let* ((bm (app-state-bookmarks app))
         (names (sort (hash-keys bm) string<?))
         (old-name (qt-echo-read-string-with-completion app "Rename bookmark: " names)))
    (when (and old-name (> (string-length old-name) 0))
      (if (hash-get bm old-name)
        (let ((new-name (qt-echo-read-string app "New name: ")))
          (when (and new-name (> (string-length new-name) 0))
            (hash-put! bm new-name (hash-get bm old-name))
            (hash-remove! bm old-name)
            (bookmarks-save! app)
            (echo-message! (app-state-echo app) (string-append old-name " -> " new-name))))
        (echo-error! (app-state-echo app) (string-append "No bookmark: " old-name))))))

(def (cmd-view-register app)
  "View contents of a register."
  (let ((input (qt-echo-read-string app "View register: ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (regs (app-state-registers app))
             (val (hash-get regs ch)))
        (if val
          (echo-message! (app-state-echo app)
            (string-append "Register " (string ch) ": "
              (cond
                ((string? val) (if (> (string-length val) 60)
                                 (string-append (substring val 0 60) "...")
                                 val))
                ((number? val) (number->string val))
                (else "(complex value)"))))
          (echo-message! (app-state-echo app)
            (string-append "Register " (string ch) " is empty")))))))

(def (cmd-sort-imports app)
  "Sort import lines in the current buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         ;; Find consecutive import lines (starting with (import or :)
         (result
           (let loop ((ls lines) (imports []) (in-import? #f) (acc []))
             (cond
               ((null? ls)
                (if (pair? imports)
                  (reverse (append (reverse (sort (reverse imports) string<?)) acc))
                  (reverse acc)))
               ((or (string-contains (car ls) "(import ")
                    (and in-import? (> (string-length (car ls)) 0)
                         (or (char=? (string-ref (car ls) 0) #\space)
                             (char=? (string-ref (car ls) 0) #\tab)
                             (char-alphabetic? (string-ref (car ls) 0)))))
                (loop (cdr ls) (cons (car ls) imports) #t acc))
               (else
                (if (pair? imports)
                  (loop (cdr ls) [] #f
                    (cons (car ls) (append (reverse (sort (reverse imports) string<?)) acc)))
                  (loop (cdr ls) [] #f (cons (car ls) acc)))))))
         (new-text (string-join result "\n")))
    (qt-plain-text-edit-set-text! ed new-text)
    (echo-message! (app-state-echo app) "Imports sorted")))

(def (cmd-replace-string-all app)
  "Replace all occurrences of a string (no prompting)."
  (let ((from (qt-echo-read-string app "Replace all: ")))
    (when from
      (let ((to (qt-echo-read-string app (string-append "Replace \"" from "\" with: "))))
        (when to
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed)))
            (let loop ((result text) (count 0))
              (let ((found (string-contains result from)))
                (if found
                  (loop (string-append (substring result 0 found) to
                          (substring result (+ found (string-length from))
                                    (string-length result)))
                        (+ count 1))
                  (begin
                    (qt-plain-text-edit-set-text! ed result)
                    (echo-message! (app-state-echo app)
                      (string-append "Replaced " (number->string count)
                                     " occurrences"))))))))))))

(def (cmd-replace-in-region app)
  "Replace all occurrences within the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let ((search (qt-echo-read-string app "Replace in region: ")))
        (when search
          (let ((replace (qt-echo-read-string app "Replace with: ")))
            (when replace
              (let* ((start (min pos mark))
                     (end (max pos mark))
                     (text (qt-plain-text-edit-text ed))
                     (region (substring text start end))
                     (slen (string-length search))
                     (result (let loop ((p 0) (acc []))
                               (let ((f (string-contains region search p)))
                                 (if f
                                   (loop (+ f slen)
                                     (cons replace (cons (substring region p f) acc)))
                                   (apply string-append
                                     (reverse (cons (substring region p (string-length region)) acc))))))))
                (qt-plain-text-edit-set-selection! ed start end)
                (qt-plain-text-edit-remove-selected-text! ed)
                (qt-plain-text-edit-insert-text! ed result)
                (set! (buffer-mark buf) #f)
                (echo-message! (app-state-echo app) "Replaced in region"))))))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-write-region app)
  "Write the region to a file."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let ((filename (qt-echo-read-string app "Write region to file: ")))
        (when filename
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (substring (qt-plain-text-edit-text ed) start end)))
            (with-output-to-file filename (lambda () (display text)))
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app)
              (string-append "Wrote " (number->string (- end start)) " chars to " filename)))))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-search-forward-word app)
  "Search forward for the word at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (and (< pos len)
             (let ((ch (string-ref text pos)))
               (or (char-alphabetic? ch) (char-numeric? ch))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (char-alphabetic? ch) (char-numeric? ch))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (char-alphabetic? ch) (char-numeric? ch))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (found (string-contains text word end)))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app) (string-append "Found: " word)))
          (echo-error! (app-state-echo app) (string-append "\"" word "\" not found below"))))
      (echo-error! (app-state-echo app) "Not on a word"))))

(def (cmd-search-backward-word app)
  "Search backward for the word at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (and (< pos len)
             (let ((ch (string-ref text pos)))
               (or (char-alphabetic? ch) (char-numeric? ch))))
      (let* ((start (let loop ((p pos))
                      (if (and (> p 0)
                               (let ((ch (string-ref text (- p 1))))
                                 (or (char-alphabetic? ch) (char-numeric? ch))))
                        (loop (- p 1)) p)))
             (end (let loop ((p pos))
                    (if (and (< p len)
                             (let ((ch (string-ref text p)))
                               (or (char-alphabetic? ch) (char-numeric? ch))))
                      (loop (+ p 1)) p)))
             (word (substring text start end))
             (found (let loop ((p 0) (last-found #f))
                      (let ((f (string-contains text word p)))
                        (if (and f (< f start))
                          (loop (+ f 1) f)
                          last-found)))))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app) (string-append "Found: " word)))
          (echo-error! (app-state-echo app) (string-append "\"" word "\" not found above"))))
      (echo-error! (app-state-echo app) "Not on a word"))))

(def (cmd-count-occurrences app)
  "Count occurrences of a string in the buffer."
  (let ((pat (qt-echo-read-string app "Count occurrences of: ")))
    (when pat
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (count (let loop ((p 0) (n 0))
                      (let ((f (string-contains text pat p)))
                        (if f (loop (+ f (max 1 (string-length pat))) (+ n 1)) n)))))
        (echo-message! (app-state-echo app)
          (string-append (number->string count) " occurrences of \"" pat "\""))))))

(def (cmd-kill-matching-buffers app)
  "Kill all buffers whose names match a pattern."
  (let ((pattern (qt-echo-read-string app "Kill buffers matching: ")))
    (when pattern
      (let ((killed 0))
        (for-each
          (lambda (buf)
            (when (string-contains (buffer-name buf) pattern)
              (set! killed (+ killed 1))))
          (buffer-list))
        (echo-message! (app-state-echo app)
          (string-append "Would kill " (number->string killed) " matching buffers"))))))

(def *recent-files* [])
(def *recent-files-max* 50)
(def *recent-files-path*
  (path-expand ".gemacs-recent-files" (user-info-home (user-info (user-name)))))

(def (recent-files-add! path)
  "Add a file path to the recent files list (most recent first, no duplicates)."
  (when (and path (string? path) (> (string-length path) 0))
    (let ((abs-path (path-expand path)))
      ;; Remove existing entry if present, then prepend
      (set! *recent-files*
        (cons abs-path
          (let loop ((files *recent-files*) (acc []))
            (cond
              ((null? files) (reverse acc))
              ((string=? (car files) abs-path) (loop (cdr files) acc))
              (else (loop (cdr files) (cons (car files) acc)))))))
      ;; Trim to max size
      (when (> (length *recent-files*) *recent-files-max*)
        (set! *recent-files*
          (let loop ((files *recent-files*) (n 0) (acc []))
            (if (or (null? files) (>= n *recent-files-max*))
              (reverse acc)
              (loop (cdr files) (+ n 1) (cons (car files) acc))))))
      ;; Save to disk
      (recent-files-save!))))

(def (recent-files-save!)
  "Persist recent files list to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (call-with-output-file *recent-files-path*
        (lambda (port)
          (for-each (lambda (f) (display f port) (newline port))
                    *recent-files*))))))

(def (recent-files-load!)
  "Load recent files list from disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (when (file-exists? *recent-files-path*)
        (set! *recent-files*
          (call-with-input-file *recent-files-path*
            (lambda (port)
              (let loop ((acc []))
                (let ((line (read-line port)))
                  (if (eof-object? line)
                    (reverse acc)
                    (if (> (string-length line) 0)
                      (loop (cons line acc))
                      (loop acc))))))))))))

(def (cmd-list-recent-files app)
  "Show list of recently opened files."
  (if (null? *recent-files*)
    (echo-message! (app-state-echo app) "No recent files")
    (let* ((text (string-join *recent-files* "\n"))
           (fr (app-state-frame app))
           (ed (qt-current-editor fr))
           (buf (qt-buffer-create! "*Recent Files*" ed #f)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text))))

(def (cmd-clear-recent-files app)
  "Clear the recent files list."
  (set! *recent-files* [])
  (recent-files-save!)
  (echo-message! (app-state-echo app) "Recent files cleared"))

(def (cmd-recentf-open app)
  "Open a recently visited file using completion."
  (if (null? *recent-files*)
    (echo-message! (app-state-echo app) "No recent files")
    (let ((choice (qt-echo-read-string-with-completion app "Recent file: " *recent-files*)))
      (when (and choice (> (string-length choice) 0))
        (cond
          ;; Directory -> dired
          ((and (file-exists? choice)
                (eq? 'directory (file-info-type (file-info choice))))
           (dired-open-directory! app choice))
          ;; Regular file
          (else
           (let* ((name (path-strip-directory choice))
                  (fr (app-state-frame app))
                  (ed (current-qt-editor app))
                  (buf (qt-buffer-create! name ed choice)))
             (qt-buffer-attach! ed buf)
             (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
             (when (file-exists? choice)
               (let ((text (read-file-as-string choice)))
                 (when text
                   (qt-plain-text-edit-set-text! ed text)
                   (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                   (qt-plain-text-edit-set-cursor-position! ed 0))))
             (qt-setup-highlighting! app buf))))))))

(def (cmd-recentf-cleanup app)
  "Remove non-existent files from the recent files list."
  (let* ((before (length *recent-files*))
         (cleaned (filter file-exists? *recent-files*))
         (removed (- before (length cleaned))))
    (set! *recent-files* cleaned)
    (recent-files-save!)
    (echo-message! (app-state-echo app)
      (string-append "Removed " (number->string removed) " non-existent files"))))

(def (cmd-multi-occur app)
  "Search for pattern across all file-visiting buffers."
  (let ((pat (qt-echo-read-string app "Multi-occur: ")))
    (when pat
      (let* ((fr (app-state-frame app))
             (file-results
               (let loop ((bufs (buffer-list)) (acc []))
                 (if (null? bufs)
                   (reverse acc)
                   (let* ((buf (car bufs))
                          (file (buffer-file-path buf)))
                     (if (and file (file-exists? file))
                       (with-catch
                         (lambda (e) (loop (cdr bufs) acc))
                         (lambda ()
                           (let* ((content (read-file-as-string file))
                                  (lines (string-split content #\newline))
                                  (matches
                                    (let mloop ((ls lines) (n 1) (hits []))
                                      (if (null? ls) (reverse hits)
                                        (if (string-contains (car ls) pat)
                                          (mloop (cdr ls) (+ n 1)
                                            (cons (string-append (buffer-name buf) ":"
                                                    (number->string n) ": " (car ls))
                                                  hits))
                                          (mloop (cdr ls) (+ n 1) hits))))))
                             (loop (cdr bufs) (append acc matches)))))
                       (loop (cdr bufs) acc))))))
             (output (if (null? file-results)
                       (string-append "No matches for: " pat)
                       (string-join file-results "\n")))
             (ed (qt-current-editor fr))
             (buf (qt-buffer-create! "*Multi-Occur*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed output)
        (echo-message! (app-state-echo app)
          (string-append (number->string (length file-results)) " matches for: " pat))))))

(def (cmd-align-current app)
  "Align the region on a separator."
  (let ((sep (qt-echo-read-string app "Align on: ")))
    (when sep
      (let* ((ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (mark (buffer-mark buf))
             (pos (qt-plain-text-edit-cursor-position ed)))
        (if mark
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (max-col (let loop ((ls lines) (max-c 0))
                            (if (null? ls) max-c
                              (let ((p (string-contains (car ls) sep)))
                                (loop (cdr ls) (if p (max max-c p) max-c))))))
                 (aligned (map (lambda (l)
                                 (let ((p (string-contains l sep)))
                                   (if p
                                     (string-append (substring l 0 p)
                                       (make-string (- max-col p) #\space)
                                       (substring l p (string-length l)))
                                     l)))
                               lines))
                 (result (string-join aligned "\n")))
            (qt-plain-text-edit-set-selection! ed start end)
            (qt-plain-text-edit-remove-selected-text! ed)
            (qt-plain-text-edit-insert-text! ed result)
            (set! (buffer-mark buf) #f)
            (echo-message! (app-state-echo app) "Aligned"))
          (echo-error! (app-state-echo app) "No mark set"))))))

(def (cmd-clear-rectangle app)
  "Clear text in a rectangle region (replace with spaces)."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (line1 (qt-plain-text-edit-cursor-line ed))
             (mark-line (let loop ((i 0) (p 0))
                          (if (>= p mark) i
                            (if (< i (length lines))
                              (loop (+ i 1) (+ p (string-length (list-ref lines i)) 1))
                              i))))
             (col1 (- pos (line-start-position text line1)))
             (col2 (- mark (line-start-position text mark-line)))
             (start-line (min line1 mark-line))
             (end-line (max line1 mark-line))
             (start-col (min col1 col2))
             (end-col (max col1 col2))
             (new-lines
               (let loop ((i 0) (ls lines) (acc []))
                 (if (null? ls) (reverse acc)
                   (if (and (>= i start-line) (<= i end-line))
                     (let* ((l (car ls))
                            (llen (string-length l))
                            (sc (min start-col llen))
                            (ec (min end-col llen))
                            (spaces (make-string (- ec sc) #\space))
                            (new-l (string-append (substring l 0 sc) spaces
                                     (if (< ec llen) (substring l ec llen) ""))))
                       (loop (+ i 1) (cdr ls) (cons new-l acc)))
                     (loop (+ i 1) (cdr ls) (cons (car ls) acc)))))))
        (qt-plain-text-edit-set-text! ed (string-join new-lines "\n"))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Rectangle cleared"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-describe-mode app)
  "Describe the current buffer mode."
  (let* ((buf (current-qt-buffer app))
         (lang (buffer-lexer-lang buf)))
    (echo-message! (app-state-echo app)
      (string-append "Major mode: " (if lang (symbol->string lang) "fundamental")))))

(def (cmd-describe-face app)
  "Describe the face at point."
  (echo-message! (app-state-echo app) "QPlainTextEdit uses uniform styling"))

(def (cmd-describe-function app)
  "Describe a function/command."
  (let ((name (qt-echo-read-string app "Describe function: ")))
    (when name
      (let ((sym (string->symbol name)))
        (if (find-command sym)
          (echo-message! (app-state-echo app) (string-append name " is a registered command"))
          (echo-error! (app-state-echo app) (string-append name " is not a command")))))))

(def (cmd-describe-variable app)
  "Describe a variable."
  (echo-message! (app-state-echo app) "Variable inspection not available"))

(def (cmd-describe-syntax app)
  "Describe syntax at point."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (if (< pos len)
      (let* ((ch (string-ref text pos))
             (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch) " (U+"
            (number->string code 16) ") "
            (cond ((char-alphabetic? ch) "letter")
                  ((char-numeric? ch) "digit")
                  ((char-whitespace? ch) "whitespace")
                  ((memq ch '(#\( #\) #\[ #\] #\{ #\})) "bracket")
                  ((memq ch '(#\" #\')) "string delimiter")
                  (else "punctuation")))))
      (echo-message! (app-state-echo app) "End of buffer"))))

(def (cmd-insert-lorem-ipsum app)
  "Insert Lorem Ipsum text."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-insert-text! ed
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.\n")))

(def (cmd-insert-current-date-iso app)
  "Insert date in ISO format (YYYY-MM-DD)."
  (let* ((ed (current-qt-editor app))
         (t (time->seconds (current-time)))
         (date-str (let* ((secs (inexact->exact (floor t)))
                          (out (open-process (list path: "date" arguments: '("+%Y-%m-%d")))))
                     (let ((result (read-line out)))
                       (close-port out)
                       (if (string? result) result "???")))))
    (qt-plain-text-edit-insert-text! ed date-str)))

(def (cmd-insert-time app)
  "Insert current time."
  (let* ((ed (current-qt-editor app))
         (out (open-process (list path: "date" arguments: '("+%H:%M:%S")))))
    (let ((result (read-line out)))
      (close-port out)
      (when (string? result)
        (qt-plain-text-edit-insert-text! ed result)))))

;;;============================================================================
;;; Eldoc — automatic function signature display
;;;============================================================================

(def *eldoc-mode* #t)  ;; enabled by default for Gerbil/Scheme buffers

;; Signature database: symbol-string -> "(sym arg1 arg2 ...)"
(def *gerbil-signatures* (make-hash-table))

(def (eldoc-init-signatures!)
  "Populate the signature database with common Gerbil functions."
  ;; Hash tables
  (hash-put! *gerbil-signatures* "hash-get" "(hash-get table key)")
  (hash-put! *gerbil-signatures* "hash-put!" "(hash-put! table key value)")
  (hash-put! *gerbil-signatures* "hash-remove!" "(hash-remove! table key)")
  (hash-put! *gerbil-signatures* "hash-key?" "(hash-key? table key)")
  (hash-put! *gerbil-signatures* "hash-ref" "(hash-ref table key [default])")
  (hash-put! *gerbil-signatures* "hash-update!" "(hash-update! table key proc [default])")
  (hash-put! *gerbil-signatures* "hash-for-each" "(hash-for-each proc table)")
  (hash-put! *gerbil-signatures* "hash-map" "(hash-map proc table)")
  (hash-put! *gerbil-signatures* "hash-fold" "(hash-fold proc init table)")
  (hash-put! *gerbil-signatures* "hash->list" "(hash->list table)")
  (hash-put! *gerbil-signatures* "list->hash-table" "(list->hash-table alist)")
  (hash-put! *gerbil-signatures* "make-hash-table" "(make-hash-table [size:])")
  (hash-put! *gerbil-signatures* "hash-length" "(hash-length table)")
  (hash-put! *gerbil-signatures* "hash-keys" "(hash-keys table)")
  (hash-put! *gerbil-signatures* "hash-values" "(hash-values table)")
  (hash-put! *gerbil-signatures* "hash-copy" "(hash-copy table)")
  (hash-put! *gerbil-signatures* "hash-merge" "(hash-merge table1 table2)")
  (hash-put! *gerbil-signatures* "hash-merge!" "(hash-merge! table1 table2)")
  ;; Lists
  (hash-put! *gerbil-signatures* "map" "(map proc list ...)")
  (hash-put! *gerbil-signatures* "for-each" "(for-each proc list ...)")
  (hash-put! *gerbil-signatures* "filter" "(filter pred list)")
  (hash-put! *gerbil-signatures* "foldl" "(foldl proc init list)")
  (hash-put! *gerbil-signatures* "foldr" "(foldr proc init list)")
  (hash-put! *gerbil-signatures* "append" "(append list ...)")
  (hash-put! *gerbil-signatures* "reverse" "(reverse list)")
  (hash-put! *gerbil-signatures* "length" "(length list)")
  (hash-put! *gerbil-signatures* "assoc" "(assoc key alist [=])")
  (hash-put! *gerbil-signatures* "member" "(member elem list)")
  (hash-put! *gerbil-signatures* "sort" "(sort list less?)")
  (hash-put! *gerbil-signatures* "iota" "(iota count [start step])")
  (hash-put! *gerbil-signatures* "take" "(take list n)")
  (hash-put! *gerbil-signatures* "drop" "(drop list n)")
  (hash-put! *gerbil-signatures* "find" "(find pred list)")
  (hash-put! *gerbil-signatures* "any" "(any pred list)")
  (hash-put! *gerbil-signatures* "every" "(every pred list)")
  (hash-put! *gerbil-signatures* "partition" "(partition pred list)")
  ;; Strings
  (hash-put! *gerbil-signatures* "string-append" "(string-append str ...)")
  (hash-put! *gerbil-signatures* "string-length" "(string-length str)")
  (hash-put! *gerbil-signatures* "string-ref" "(string-ref str index)")
  (hash-put! *gerbil-signatures* "substring" "(substring str start end)")
  (hash-put! *gerbil-signatures* "string-contains" "(string-contains str search)")
  (hash-put! *gerbil-signatures* "string-prefix?" "(string-prefix? prefix str)")
  (hash-put! *gerbil-signatures* "string-suffix?" "(string-suffix? suffix str)")
  (hash-put! *gerbil-signatures* "string-split" "(string-split str sep)")
  (hash-put! *gerbil-signatures* "string-join" "(string-join strs [sep])")
  (hash-put! *gerbil-signatures* "string-upcase" "(string-upcase str)")
  (hash-put! *gerbil-signatures* "string-downcase" "(string-downcase str)")
  (hash-put! *gerbil-signatures* "number->string" "(number->string num [radix])")
  (hash-put! *gerbil-signatures* "string->number" "(string->number str [radix])")
  (hash-put! *gerbil-signatures* "string->symbol" "(string->symbol str)")
  (hash-put! *gerbil-signatures* "symbol->string" "(symbol->string sym)")
  ;; I/O
  (hash-put! *gerbil-signatures* "open-input-file" "(open-input-file path)")
  (hash-put! *gerbil-signatures* "open-output-file" "(open-output-file path)")
  (hash-put! *gerbil-signatures* "call-with-input-file" "(call-with-input-file path proc)")
  (hash-put! *gerbil-signatures* "call-with-output-file" "(call-with-output-file path proc)")
  (hash-put! *gerbil-signatures* "read-line" "(read-line [port])")
  (hash-put! *gerbil-signatures* "display" "(display obj [port])")
  (hash-put! *gerbil-signatures* "write" "(write obj [port])")
  (hash-put! *gerbil-signatures* "newline" "(newline [port])")
  (hash-put! *gerbil-signatures* "close-port" "(close-port port)")
  ;; Control flow
  (hash-put! *gerbil-signatures* "with-catch" "(with-catch handler thunk)")
  (hash-put! *gerbil-signatures* "call-with-current-continuation" "(call/cc proc)")
  (hash-put! *gerbil-signatures* "values" "(values val ...)")
  (hash-put! *gerbil-signatures* "call-with-values" "(call-with-values producer consumer)")
  (hash-put! *gerbil-signatures* "dynamic-wind" "(dynamic-wind before thunk after)")
  (hash-put! *gerbil-signatures* "error" "(error message irritants ...)")
  (hash-put! *gerbil-signatures* "raise" "(raise exception)")
  ;; Paths
  (hash-put! *gerbil-signatures* "path-expand" "(path-expand path [origin])")
  (hash-put! *gerbil-signatures* "path-directory" "(path-directory path)")
  (hash-put! *gerbil-signatures* "path-strip-directory" "(path-strip-directory path)")
  (hash-put! *gerbil-signatures* "path-extension" "(path-extension path)")
  (hash-put! *gerbil-signatures* "path-strip-extension" "(path-strip-extension path)")
  (hash-put! *gerbil-signatures* "file-exists?" "(file-exists? path)")
  (hash-put! *gerbil-signatures* "file-info" "(file-info path)")
  (hash-put! *gerbil-signatures* "create-directory" "(create-directory path)")
  (hash-put! *gerbil-signatures* "delete-file" "(delete-file path)")
  (hash-put! *gerbil-signatures* "rename-file" "(rename-file old new)")
  ;; Threads / concurrency
  (hash-put! *gerbil-signatures* "spawn" "(spawn thunk)")
  (hash-put! *gerbil-signatures* "spawn/name" "(spawn/name name thunk)")
  (hash-put! *gerbil-signatures* "thread-sleep!" "(thread-sleep! secs)")
  (hash-put! *gerbil-signatures* "make-mutex" "(make-mutex [name])")
  (hash-put! *gerbil-signatures* "mutex-lock!" "(mutex-lock! mutex [timeout])")
  (hash-put! *gerbil-signatures* "mutex-unlock!" "(mutex-unlock! mutex)")
  (hash-put! *gerbil-signatures* "make-channel" "(make-channel [name])")
  (hash-put! *gerbil-signatures* "channel-put" "(channel-put channel value)")
  (hash-put! *gerbil-signatures* "channel-get" "(channel-get channel)")
  ;; Syntax sugar
  (hash-put! *gerbil-signatures* "def" "(def (name args ...) body ...)")
  (hash-put! *gerbil-signatures* "defstruct" "(defstruct name (field ...) [transparent: #t])")
  (hash-put! *gerbil-signatures* "defclass" "(defclass name (super ...) (slot ...))")
  (hash-put! *gerbil-signatures* "defrule" "(defrule (name pattern ...) template)")
  (hash-put! *gerbil-signatures* "let" "(let ((var val) ...) body ...)")
  (hash-put! *gerbil-signatures* "let*" "(let* ((var val) ...) body ...)")
  (hash-put! *gerbil-signatures* "letrec" "(letrec ((var val) ...) body ...)")
  (hash-put! *gerbil-signatures* "when" "(when test body ...)")
  (hash-put! *gerbil-signatures* "unless" "(unless test body ...)")
  (hash-put! *gerbil-signatures* "cond" "(cond (test expr ...) ... [(else expr ...)])")
  (hash-put! *gerbil-signatures* "case" "(case key ((datum ...) expr ...) ... [(else expr ...)])")
  (hash-put! *gerbil-signatures* "match" "(match expr (pattern body ...) ...)")
  (hash-put! *gerbil-signatures* "parameterize" "(parameterize ((param val) ...) body ...)")
  ;; Vectors
  (hash-put! *gerbil-signatures* "vector-ref" "(vector-ref vec index)")
  (hash-put! *gerbil-signatures* "vector-set!" "(vector-set! vec index val)")
  (hash-put! *gerbil-signatures* "vector-length" "(vector-length vec)")
  (hash-put! *gerbil-signatures* "make-vector" "(make-vector n [fill])")
  (hash-put! *gerbil-signatures* "vector->list" "(vector->list vec)")
  (hash-put! *gerbil-signatures* "list->vector" "(list->vector list)")
  ;; JSON
  (hash-put! *gerbil-signatures* "json-object->string" "(json-object->string obj)")
  (hash-put! *gerbil-signatures* "string->json-object" "(string->json-object str)")
  ;; Process
  (hash-put! *gerbil-signatures* "open-process" "(open-process settings)")
  (hash-put! *gerbil-signatures* "process-status" "(process-status proc)")
  ;; Apply / call
  (hash-put! *gerbil-signatures* "apply" "(apply proc arg ... args)")
  (hash-put! *gerbil-signatures* "call-with-port" "(call-with-port port proc)"))

;; Initialize the database once
(eldoc-init-signatures!)

(def (enclosing-function-at-point text pos)
  "Find the function name of the enclosing s-expression at POS.
   Returns the symbol name string or #f."
  (let ((len (string-length text)))
    (and (> len 0) (<= pos len)
         ;; Scan backwards for the opening paren of the enclosing form
         (let scan ((i (min (- pos 1) (- len 1))) (depth 0))
           (and (>= i 0)
                (let ((ch (string-ref text i)))
                  (cond
                    ((char=? ch #\)) (scan (- i 1) (+ depth 1)))
                    ((char=? ch #\()
                     (if (> depth 0)
                       (scan (- i 1) (- depth 1))
                       ;; Found opening paren — extract the function name after it
                       (let ((start (+ i 1)))
                         (let sym-end ((j start))
                           (if (and (< j len)
                                    (let ((c (string-ref text j)))
                                      (or (char-alphabetic? c) (char-numeric? c)
                                          (char=? c #\-) (char=? c #\_)
                                          (char=? c #\?) (char=? c #\!)
                                          (char=? c #\>) (char=? c #\/)
                                          (char=? c #\*) (char=? c #\+))))
                             (sym-end (+ j 1))
                             (if (> j start)
                               (substring text start j)
                               #f))))))
                    (else (scan (- i 1) depth)))))))))

(def *eldoc-last-sym* #f) ;; avoid flicker — don't re-display same sig

(def (eldoc-display! app)
  "Check cursor position and show signature in echo area if applicable."
  (when *eldoc-mode*
    (let* ((fr (app-state-frame app))
           (buf (qt-edit-window-buffer (qt-current-window fr)))
           (lang (buffer-lexer-lang buf)))
      ;; Only for Scheme-like languages
      (when (memq lang '(scheme gerbil lisp))
        (let* ((ed (qt-edit-window-editor (qt-current-window fr)))
               (text (qt-plain-text-edit-text ed))
               (pos (qt-plain-text-edit-cursor-position ed))
               (sym (enclosing-function-at-point text pos)))
          (cond
            ((and sym (not (equal? sym *eldoc-last-sym*)))
             (let ((sig (hash-get *gerbil-signatures* sym)))
               (set! *eldoc-last-sym* sym)
               (when sig
                 (echo-message! (app-state-echo app) sig))))
            ((and (not sym) *eldoc-last-sym*)
             (set! *eldoc-last-sym* #f))))))))

;; Xref back stack: list of (file-path . position) or (buffer-name . position)
(def *xref-back-stack* [])

(def (xref-push-location! app)
  "Push current location onto xref back stack."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (set! *xref-back-stack*
      (cons (cons (or (buffer-file-path buf) (buffer-name buf)) pos)
            (if (> (length *xref-back-stack*) 50)
              (let loop ((l *xref-back-stack*) (n 0) (acc []))
                (if (or (null? l) (>= n 50)) (reverse acc)
                  (loop (cdr l) (+ n 1) (cons (car l) acc))))
              *xref-back-stack*)))))

(def (symbol-at-point ed)
  "Extract the symbol name at cursor position."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (and (< pos len)
         (let* ((sym-char? (lambda (ch)
                   (or (char-alphabetic? ch) (char-numeric? ch)
                       (char=? ch #\-) (char=? ch #\_)
                       (char=? ch #\?) (char=? ch #\!))))
                (start (let loop ((i pos))
                         (if (and (> i 0) (sym-char? (string-ref text (- i 1))))
                           (loop (- i 1)) i)))
                (end (let loop ((i pos))
                       (if (and (< i len) (sym-char? (string-ref text i)))
                         (loop (+ i 1)) i)))
                (sym (substring text start end)))
           (if (> (string-length sym) 0) sym #f)))))

(def *def-patterns*
  '("(def (" "(def " "(defstruct " "(defclass " "(defrule " "(defmethod "
    "(defsyntax " "(defmacro " "(defun " "(define (" "(define "))

(def (find-def-in-text text sym)
  "Search TEXT for a definition of SYM. Returns char position or #f."
  (let loop ((patterns *def-patterns*))
    (if (null? patterns) #f
      (let ((pattern (string-append (car patterns) sym)))
        (let ((found (string-contains text pattern)))
          (if found
            ;; Verify it's a word boundary (next char is space, paren, or newline)
            (let ((end-pos (+ found (string-length pattern))))
              (if (or (>= end-pos (string-length text))
                      (let ((ch (string-ref text end-pos)))
                        (or (char=? ch #\space) (char=? ch #\)) (char=? ch #\newline)
                            (char=? ch #\tab))))
                found
                (loop (cdr patterns))))
            (loop (cdr patterns))))))))

(def (find-def-in-project sym root)
  "Search project files for definition of SYM. Returns (file . char-pos) or #f."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/grep"
                           arguments: (list "-rnl"
                             (string-append "(def[a-z]* (" sym "\\|def[a-z]* " sym)
                             "--include=*.ss" "--include=*.scm"
                             root)
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        (and output
             (let ((files (let loop ((s output) (acc []))
                            (let ((nl (string-index s #\newline)))
                              (if nl
                                (loop (substring s (+ nl 1) (string-length s))
                                      (cons (substring s 0 nl) acc))
                                (reverse (if (> (string-length s) 0)
                                           (cons s acc) acc)))))))
               ;; Search each file for the actual definition position
               (let loop ((fs files))
                 (if (null? fs) #f
                   (let ((text (with-catch (lambda (e) #f)
                                 (lambda () (read-file-as-string (car fs))))))
                     (if text
                       (let ((pos (find-def-in-text text sym)))
                         (if pos (cons (car fs) pos)
                           (loop (cdr fs))))
                       (loop (cdr fs))))))))))))

(def (cmd-goto-definition app)
  "Jump to definition of symbol at point. Searches current buffer, then project."
  (let* ((ed (current-qt-editor app))
         (sym (symbol-at-point ed)))
    (if (not sym)
      (echo-message! (app-state-echo app) "No symbol at point")
      (let* ((text (qt-plain-text-edit-text ed))
             (local-pos (find-def-in-text text sym)))
        (if local-pos
          ;; Found in current buffer
          (begin
            (xref-push-location! app)
            (qt-plain-text-edit-set-cursor-position! ed local-pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            (echo-message! (app-state-echo app) (string-append "-> " sym)))
          ;; Search project
          (let* ((root (current-project-root app))
                 (result (find-def-in-project sym root)))
            (if result
              (let* ((file (car result))
                     (pos (cdr result))
                     (fr (app-state-frame app)))
                (xref-push-location! app)
                ;; Open file and jump
                (let* ((name (path-strip-directory file))
                       (existing (let loop ((bufs *buffer-list*))
                                   (if (null? bufs) #f
                                     (let ((b (car bufs)))
                                       (if (and (buffer-file-path b)
                                                (string=? (buffer-file-path b) file))
                                         b (loop (cdr bufs)))))))
                       (buf (or existing (qt-buffer-create! name ed file))))
                  (qt-buffer-attach! ed buf)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                  (when (not existing)
                    (let ((ftext (read-file-as-string file)))
                      (when ftext
                        (qt-plain-text-edit-set-text! ed ftext)
                        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
                    (qt-setup-highlighting! app buf))
                  (qt-plain-text-edit-set-cursor-position! ed pos)
                  (qt-plain-text-edit-ensure-cursor-visible! ed)
                  (echo-message! (app-state-echo app) (string-append "-> " sym " in " name))))
              (echo-error! (app-state-echo app)
                (string-append "Definition not found: " sym)))))))))

(def (cmd-xref-back app)
  "Pop xref stack and return to previous location."
  (if (null? *xref-back-stack*)
    (echo-error! (app-state-echo app) "No previous location")
    (let* ((loc (car *xref-back-stack*))
           (path-or-name (car loc))
           (pos (cdr loc))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      (set! *xref-back-stack* (cdr *xref-back-stack*))
      ;; Find buffer by file-path or name
      (let ((buf (or (let loop ((bufs *buffer-list*))
                       (if (null? bufs) #f
                         (let ((b (car bufs)))
                           (if (and (buffer-file-path b)
                                    (string=? (buffer-file-path b) path-or-name))
                             b (loop (cdr bufs))))))
                     (buffer-by-name path-or-name))))
        (if buf
          (begin
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (string-length (qt-plain-text-edit-text ed))))
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            (echo-message! (app-state-echo app) "Back"))
          (echo-error! (app-state-echo app) "Buffer no longer exists"))))))

(def (imenu-extract-definitions text lang)
  "Extract symbol definitions from TEXT based on LANG.
Returns list of (name . line-number) pairs."
  (let ((lines (string-split text #\newline))
        (defs []))
    (let loop ((ls lines) (line-num 1))
      (if (null? ls)
        (reverse defs)
        (let ((line (car ls)))
          ;; Match based on language
          (let ((found
                 (cond
                   ;; Gerbil/Scheme/Lisp: (def, (defstruct, (defclass, (defrule, etc.
                   ((memq lang '(scheme gerbil lisp))
                    (cond
                      ;; (def (name ...) or (def name
                      ((and (>= (string-length line) 5)
                            (string-prefix? "(def " line))
                       (let* ((rest (substring line 5 (string-length line)))
                              (rest (if (and (> (string-length rest) 0)
                                             (char=? (string-ref rest 0) #\())
                                      (substring rest 1 (string-length rest))
                                      rest)))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\-) (char=? ch #\_)
                                          (char=? ch #\!) (char=? ch #\?))))
                             (scan (+ i 1))
                             (and (> i 0) (substring rest 0 i))))))
                      ;; (defstruct name, (defclass name, etc.
                      ((or (string-prefix? "(defstruct " line)
                           (string-prefix? "(defclass " line)
                           (string-prefix? "(defrule " line)
                           (string-prefix? "(defsyntax " line)
                           (string-prefix? "(defmethod " line))
                       (let* ((space-pos (string-index line #\space 1))
                              (rest (and space-pos
                                         (substring line (+ space-pos 1)
                                                    (string-length line)))))
                         (and rest
                              (let scan ((i 0))
                                (if (and (< i (string-length rest))
                                         (let ((ch (string-ref rest i)))
                                           (or (char-alphabetic? ch) (char-numeric? ch)
                                               (char=? ch #\-) (char=? ch #\_)
                                               (char=? ch #\!) (char=? ch #\?))))
                                  (scan (+ i 1))
                                  (and (> i 0) (substring rest 0 i)))))))
                      (else #f)))
                   ;; Python: def/class at start of line
                   ((eq? lang 'python)
                    (cond
                      ((string-prefix? "def " line)
                       (let* ((rest (substring line 4 (string-length line))))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\_))))
                             (scan (+ i 1))
                             (and (> i 0) (string-append "def " (substring rest 0 i)))))))
                      ((string-prefix? "class " line)
                       (let* ((rest (substring line 6 (string-length line))))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\_))))
                             (scan (+ i 1))
                             (and (> i 0) (string-append "class " (substring rest 0 i)))))))
                      (else #f)))
                   ;; C/C++/Java/Go/Rust/JS/TS: function name(
                   ((memq lang '(c cpp java go rust javascript typescript))
                    ;; Look for lines containing "name(" that start at indent 0
                    ;; and don't start with # (preprocessor) or // (comment)
                    (and (> (string-length line) 0)
                         (not (char=? (string-ref line 0) #\#))
                         (not (string-prefix? "//" line))
                         (not (string-prefix? " " line))
                         (not (string-prefix? "\t" line))
                         ;; Look for word( pattern
                         (let ((paren-pos (string-index line #\()))
                           (and paren-pos (> paren-pos 0)
                                ;; Extract the word before (
                                (let scan ((i (- paren-pos 1)))
                                  (if (and (>= i 0)
                                           (let ((ch (string-ref line i)))
                                             (or (char-alphabetic? ch) (char-numeric? ch)
                                                 (char=? ch #\_))))
                                    (scan (- i 1))
                                    (let ((name (substring line (+ i 1) paren-pos)))
                                      (and (> (string-length name) 0)
                                           ;; Skip common keywords
                                           (not (member name '("if" "for" "while" "switch"
                                                               "return" "else" "catch"
                                                               "sizeof" "typeof")))
                                           name))))))))
                   ;; Shell: function name() or name()
                   ((memq lang '(shell bash))
                    (or (and (string-prefix? "function " line)
                             (let* ((rest (substring line 9 (string-length line))))
                               (let scan ((i 0))
                                 (if (and (< i (string-length rest))
                                          (let ((ch (string-ref rest i)))
                                            (or (char-alphabetic? ch) (char-numeric? ch)
                                                (char=? ch #\_))))
                                   (scan (+ i 1))
                                   (and (> i 0) (substring rest 0 i))))))
                        #f))
                   ;; Ruby: def name
                   ((eq? lang 'ruby)
                    (and (string-prefix? "def " line)
                         (let* ((rest (substring line 4 (string-length line))))
                           (let scan ((i 0))
                             (if (and (< i (string-length rest))
                                      (let ((ch (string-ref rest i)))
                                        (or (char-alphabetic? ch) (char-numeric? ch)
                                            (char=? ch #\_) (char=? ch #\?))))
                               (scan (+ i 1))
                               (and (> i 0) (substring rest 0 i)))))))
                   ;; Fallback: look for "def " prefix
                   (else
                    (and (string-prefix? "(def " line)
                         (let* ((rest (substring line 5 (string-length line)))
                                (rest (if (and (> (string-length rest) 0)
                                               (char=? (string-ref rest 0) #\())
                                        (substring rest 1 (string-length rest))
                                        rest)))
                           (let scan ((i 0))
                             (if (and (< i (string-length rest))
                                      (let ((ch (string-ref rest i)))
                                        (or (char-alphabetic? ch) (char-numeric? ch)
                                            (char=? ch #\-) (char=? ch #\_))))
                               (scan (+ i 1))
                               (and (> i 0) (substring rest 0 i))))))))))
            (when found
              (set! defs (cons (cons found line-num) defs)))
            (loop (cdr ls) (+ line-num 1))))))))

(def (cmd-imenu app)
  "List definitions in the current buffer and jump to selected one."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (lang (buffer-lexer-lang buf))
         (defs (imenu-extract-definitions text lang))
         (echo (app-state-echo app)))
    (if (null? defs)
      (echo-error! echo "No definitions found")
      (let* ((names (map (lambda (d)
                           (string-append (car d) " (L" (number->string (cdr d)) ")"))
                         defs))
             (choice (qt-echo-read-string-with-completion app "Go to: " names)))
        (when (and choice (> (string-length choice) 0))
          ;; Find the matching definition
          (let ((found (let loop ((ds defs) (ns names))
                         (cond
                           ((null? ds) #f)
                           ((string=? choice (car ns)) (car ds))
                           (else (loop (cdr ds) (cdr ns)))))))
            (when found
              (let* ((line-num (cdr found))
                     (target-pos (text-line-position text line-num)))
                (qt-plain-text-edit-set-cursor-position! ed target-pos)
                (qt-plain-text-edit-ensure-cursor-visible! ed)
                (echo-message! echo (string-append (car found) " — line "
                                                   (number->string line-num)))))))))))

(def (cmd-show-word-count app)
  "Show word count for the entire buffer."
  (cmd-count-words-buffer app))

(def (cmd-show-char-count app)
  "Show character count."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (echo-message! (app-state-echo app)
      (string-append (number->string (string-length text)) " characters"))))

(def (cmd-insert-path-separator app)
  "Insert a path separator."
  (qt-plain-text-edit-insert-text! (current-qt-editor app) "/"))

(def (cmd-maximize-window app)
  "Maximize current window by deleting others."
  (cmd-delete-other-windows app))

(def (cmd-minimize-window app)
  "Minimize window (placeholder)."
  (echo-message! (app-state-echo app) "Window minimized"))

(def (cmd-delete-matching-lines app)
  "Delete lines matching a pattern (alias for flush-lines)."
  (cmd-flush-lines app))

(def (cmd-delete-non-matching-lines app)
  "Delete lines not matching a pattern (alias for keep-lines)."
  (cmd-keep-lines app))

(def (cmd-copy-matching-lines app)
  "Copy lines matching a pattern to kill ring."
  (let ((pat (qt-echo-read-string app "Copy lines matching: ")))
    (when pat
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (matches (filter (lambda (l) (string-contains l pat)) lines)))
        (if (pair? matches)
          (let ((result (string-join matches "\n")))
            (set! (app-state-kill-ring app) (cons result (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
              (string-append (number->string (length matches)) " lines copied")))
          (echo-message! (app-state-echo app) "No matching lines"))))))

(def (cmd-count-lines-buffer app)
  "Count lines in the buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline)))
    (echo-message! (app-state-echo app)
      (string-append (number->string (length lines)) " lines"))))

(def (cmd-count-words-paragraph app)
  "Count words in the current paragraph."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         ;; Find paragraph boundaries
         (para-start
           (let loop ((i (- pos 1)))
             (cond
               ((< i 0) 0)
               ((and (char=? (string-ref text i) #\newline)
                     (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                (+ i 1))
               (else (loop (- i 1))))))
         (para-end
           (let loop ((i pos))
             (cond
               ((>= i len) len)
               ((and (char=? (string-ref text i) #\newline)
                     (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\newline))
                i)
               (else (loop (+ i 1))))))
         (para (substring text para-start para-end))
         (words (let loop ((i 0) (in-word? #f) (count 0))
                  (if (>= i (string-length para))
                    (if in-word? (+ count 1) count)
                    (let ((ch (string-ref para i)))
                      (if (char-whitespace? ch)
                        (loop (+ i 1) #f (if in-word? (+ count 1) count))
                        (loop (+ i 1) #t count)))))))
    (echo-message! (app-state-echo app)
      (string-append (number->string words) " words in paragraph"))))

(def (cmd-convert-to-unix app)
  "Convert line endings to Unix (LF)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (new-text (let loop ((i 0) (acc []))
                     (cond
                       ((>= i (string-length text))
                        (list->string (reverse acc)))
                       ((and (char=? (string-ref text i) #\return)
                             (< (+ i 1) (string-length text))
                             (char=? (string-ref text (+ i 1)) #\newline))
                        (loop (+ i 2) (cons #\newline acc)))
                       ((char=? (string-ref text i) #\return)
                        (loop (+ i 1) (cons #\newline acc)))
                       (else (loop (+ i 1) (cons (string-ref text i) acc)))))))
    (qt-plain-text-edit-set-text! ed new-text)
    (echo-message! (app-state-echo app) "Converted to Unix line endings")))

(def (cmd-convert-to-dos app)
  "Convert line endings to DOS (CRLF)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         ;; First convert to Unix, then to DOS
         (unix (let loop ((i 0) (acc []))
                 (cond
                   ((>= i (string-length text))
                    (list->string (reverse acc)))
                   ((and (char=? (string-ref text i) #\return)
                         (< (+ i 1) (string-length text))
                         (char=? (string-ref text (+ i 1)) #\newline))
                    (loop (+ i 2) (cons #\newline acc)))
                   ((char=? (string-ref text i) #\return)
                    (loop (+ i 1) (cons #\newline acc)))
                   (else (loop (+ i 1) (cons (string-ref text i) acc))))))
         (dos (let loop ((i 0) (acc []))
                (cond
                  ((>= i (string-length unix))
                   (list->string (reverse acc)))
                  ((char=? (string-ref unix i) #\newline)
                   (loop (+ i 1) (cons #\newline (cons #\return acc))))
                  (else (loop (+ i 1) (cons (string-ref unix i) acc)))))))
    (qt-plain-text-edit-set-text! ed dos)
    (echo-message! (app-state-echo app) "Converted to DOS line endings")))

(def (cmd-show-line-endings app)
  "Show the line ending style of current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (has-cr (string-contains text "\r\n"))
         (style (if has-cr "DOS (CRLF)" "Unix (LF)")))
    (echo-message! (app-state-echo app) (string-append "Line endings: " style))))

(def (cmd-wrap-lines-at-column app)
  "Wrap long lines at a specified column."
  (let ((col-str (qt-echo-read-string app "Wrap at column: ")))
    (when col-str
      (let ((col (string->number col-str)))
        (when (and col (> col 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (lines (string-split text #\newline))
                 (wrapped
                   (apply append
                     (map (lambda (line)
                            (if (<= (string-length line) col)
                              (list line)
                              ;; Break line at word boundaries
                              (let loop ((rest line) (acc []))
                                (if (<= (string-length rest) col)
                                  (reverse (cons rest acc))
                                  (let* ((break-pos
                                           (let bloop ((i col))
                                             (cond
                                               ((<= i 0) col)
                                               ((char=? (string-ref rest i) #\space) i)
                                               (else (bloop (- i 1))))))
                                         (frag (substring rest 0 break-pos))
                                         (remaining (if (< break-pos (string-length rest))
                                                      (substring rest (+ break-pos 1)
                                                                 (string-length rest))
                                                      "")))
                                    (loop remaining (cons frag acc)))))))
                          lines)))
                 (new-text (string-join wrapped "\n")))
            (qt-plain-text-edit-set-text! ed new-text)
            (echo-message! (app-state-echo app)
              (string-append "Lines wrapped at column " col-str))))))))

(def (cmd-strip-line-numbers app)
  "Strip leading line numbers from each line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (stripped (map (lambda (line)
                          ;; Strip leading digits + optional separator (: or . or space)
                          (let loop ((i 0))
                            (cond
                              ((>= i (string-length line)) line)
                              ((char-numeric? (string-ref line i)) (loop (+ i 1)))
                              ((and (> i 0) (memq (string-ref line i) '(#\: #\. #\space #\tab)))
                               (let ((rest (substring line (+ i 1) (string-length line))))
                                 (if (and (> (string-length rest) 0)
                                          (char=? (string-ref rest 0) #\space))
                                   (substring rest 1 (string-length rest))
                                   rest)))
                              (else line))))
                        lines)))
    (qt-plain-text-edit-set-text! ed (string-join stripped "\n"))
    (echo-message! (app-state-echo app) "Line numbers stripped")))

(def (cmd-goto-word-at-point app)
  "Search for the word at point (same as search-forward-word)."
  (cmd-search-forward-word app))

(def (cmd-unindent-region app)
  "Remove one level of indentation from the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (dedented (map (lambda (l)
                              (cond
                                ((and (>= (string-length l) 2)
                                      (char=? (string-ref l 0) #\space)
                                      (char=? (string-ref l 1) #\space))
                                 (substring l 2 (string-length l)))
                                ((and (>= (string-length l) 1)
                                      (char=? (string-ref l 0) #\tab))
                                 (substring l 1 (string-length l)))
                                (else l)))
                            lines))
             (result (string-join dedented "\n")))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region unindented"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-number-region app)
  "Add line numbers to the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (numbered (let loop ((ls lines) (n 1) (acc []))
                         (if (null? ls) (reverse acc)
                           (loop (cdr ls) (+ n 1)
                             (cons (string-append (number->string n) ": " (car ls)) acc)))))
             (result (string-join numbered "\n")))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

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

(def (cmd-name-last-kbd-macro app)
  "Name the last keyboard macro."
  (let ((name (qt-echo-read-string app "Name for macro: ")))
    (when name
      (echo-message! (app-state-echo app)
        (string-append "Macro named: " name " (naming not fully supported)")))))

(def (cmd-show-environment app)
  "Show environment variables."
  (let* ((fr (app-state-frame app))
         (ed (qt-current-editor fr))
         (out (open-process (list path: "env" arguments: '())))
         (text (read-line out #f)))
    (close-port out)
    (when (string? text)
      (let ((buf (qt-buffer-create! "*Environment*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)))))

(def (cmd-show-keybinding-for app)
  "Show keybinding for a command."
  (cmd-where-is app))

(def (cmd-first-error app)
  "Jump to the first search match."
  (let* ((ed (current-qt-editor app))
         (search (app-state-last-search app)))
    (if search
      (let* ((text (qt-plain-text-edit-text ed))
             (found (string-contains text search)))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app) (string-append "First: " search)))
          (echo-error! (app-state-echo app) "Not found")))
      (echo-error! (app-state-echo app) "No search"))))

(def (cmd-find-grep app)
  "Run grep on files (using shell grep command)."
  (let ((pat (qt-echo-read-string app "Grep for: ")))
    (when pat
      (let ((dir (qt-echo-read-string app "In directory: ")))
        (when dir
          (let* ((fr (app-state-frame app))
                 (ed (qt-current-editor fr))
                 (out (open-process
                        (list path: "grep" arguments: (list "-rn" pat dir)
                              stderr-redirection: #t)))
                 (text (read-line out #f)))
            (close-port out)
            (let ((buf (qt-buffer-create! "*Grep*" ed #f)))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed (or text "No results")))))))))

;; cmd-project-grep moved below with project commands

(def *project-root-markers*
  '(".git" "gerbil.pkg" "Makefile" "build.ss" "Cargo.toml"
    "package.json" "pyproject.toml" "setup.py" "go.mod"
    "CMakeLists.txt" ".project-root"))

(def (detect-project-root path)
  "Find project root by searching upward for marker files/dirs."
  (and path
       (let loop ((dir (path-directory path)))
         (if (string=? dir "/") #f
           (if (let check ((markers *project-root-markers*))
                 (and (pair? markers)
                      (or (file-exists? (path-expand (car markers) dir))
                          (check (cdr markers)))))
             dir
             (let ((parent (path-directory (string-append dir "/"))))
               (if (string=? parent dir) #f
                 (loop parent))))))))

(def (current-project-root app)
  "Get project root for current buffer, or current directory."
  (let ((path (buffer-file-path (current-qt-buffer app))))
    (or (detect-project-root path)
        (current-directory))))

(def (project-list-files root)
  "List files in project ROOT using find, excluding common build/vcs dirs."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/find"
                           arguments: (list root
                             "-type" "f"
                             "-not" "-path" "*/.git/*"
                             "-not" "-path" "*/.gerbil/*"
                             "-not" "-path" "*/node_modules/*"
                             "-not" "-path" "*/__pycache__/*"
                             "-not" "-path" "*/target/*"
                             "-not" "-path" "*/.build/*"
                             "-not" "-name" "*.o"
                             "-not" "-name" "*.o1")
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        (if output
          (let loop ((s output) (acc []))
            (let ((nl (string-index s #\newline)))
              (if nl
                (loop (substring s (+ nl 1) (string-length s))
                      (cons (substring s 0 nl) acc))
                (reverse (if (> (string-length s) 0) (cons s acc) acc)))))
          [])))))

(def (cmd-project-find-file app)
  "Find file in the project directory with completion."
  (let* ((root (current-project-root app))
         (files (project-list-files root))
         ;; Make paths relative to project root for nicer display
         (prefix-len (+ (string-length root) (if (char=? (string-ref root (- (string-length root) 1)) #\/) 0 1)))
         (relative-files (map (lambda (f)
                                (if (> (string-length f) prefix-len)
                                  (substring f prefix-len (string-length f))
                                  f))
                              files))
         (input (qt-echo-read-string-with-completion app
                  (string-append "Find file in " (path-strip-directory root) ": ")
                  relative-files)))
    (when (and input (> (string-length input) 0))
      (let ((full-path (path-expand input root)))
        (recent-files-add! full-path)
        (let* ((name (path-strip-directory full-path))
               (fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (qt-buffer-create! name ed full-path)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? full-path)
            (let ((text (read-file-as-string full-path)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)))
            (file-mtime-record! full-path))
          (qt-setup-highlighting! app buf)
          (echo-message! (app-state-echo app) (string-append "Opened: " full-path)))))))

(def (cmd-project-compile app)
  "Compile the project from project root."
  (let* ((root (current-project-root app))
         (default (or (app-state-last-compile app)
                      (cond
                        ((file-exists? (path-expand "gerbil.pkg" root)) "gerbil build")
                        ((file-exists? (path-expand "Makefile" root)) "make")
                        ((file-exists? (path-expand "Cargo.toml" root)) "cargo build")
                        (else "make"))))
         (cmd (qt-echo-read-string app
                (string-append "Compile in " root " [" default "]: "))))
    (when cmd
      (let ((actual-cmd (if (string=? cmd "") default cmd)))
        (set! (app-state-last-compile app) actual-cmd)
        (compilation-run-command! app (string-append "cd " root " && " actual-cmd))))))

(def (cmd-project-grep app)
  "Grep in the project directory."
  (let* ((root (current-project-root app))
         (pattern (qt-echo-read-string app
                    (string-append "Grep in " (path-strip-directory root) ": "))))
    (when (and pattern (> (string-length pattern) 0))
      (grep-run-and-show! app pattern root '("-rn")))))

(def (cmd-reindent-buffer app)
  "Re-indent the entire buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         ;; Simple: re-indent based on paren depth
         (reindented
           (let loop ((ls lines) (depth 0) (acc []))
             (if (null? ls) (reverse acc)
               (let* ((line (car ls))
                      (trimmed (string-trim line))
                      ;; Count closing parens at start
                      (close-first (let cloop ((i 0) (d 0))
                                     (if (>= i (string-length trimmed)) d
                                       (case (string-ref trimmed i)
                                         ((#\) #\] #\}) (cloop (+ i 1) (+ d 1)))
                                         (else d)))))
                      (this-depth (max 0 (- depth close-first)))
                      (indent (make-string (* this-depth 2) #\space))
                      (new-line (string-append indent trimmed))
                      ;; Count net depth change
                      (delta (let dloop ((i 0) (d 0))
                               (if (>= i (string-length trimmed)) d
                                 (case (string-ref trimmed i)
                                   ((#\( #\[ #\{) (dloop (+ i 1) (+ d 1)))
                                   ((#\) #\] #\}) (dloop (+ i 1) (- d 1)))
                                   (else (dloop (+ i 1) d)))))))
                 (loop (cdr ls) (max 0 (+ depth delta)) (cons new-line acc)))))))
    (qt-plain-text-edit-set-text! ed (string-join reindented "\n"))
    (echo-message! (app-state-echo app) "Buffer re-indented")))

(def (cmd-fill-individual-paragraphs app)
  "Fill each paragraph in the region individually."
  (cmd-fill-paragraph app))

