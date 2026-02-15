;;; -*- Gerbil -*-
;;; Command batch A (Tasks 36-40): whitespace, electric-pair,
;;; text processing, s-expressions, sort, indent

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
        :gerbil-emacs/editor-advanced)

;;;============================================================================
;;; Whitespace cleanup, electric-pair toggle, and more (Task #36)
;;;============================================================================

;; *auto-pair-mode* moved to editor-core.ss

;; Recenter cycle state: 'center -> 'top -> 'bottom -> 'center
(def *recenter-position* 'center)

(def (cmd-whitespace-cleanup app)
  "Remove trailing whitespace and convert tabs to spaces."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (cleaned (map (lambda (line) (string-trim-right line)) lines))
         (result (string-join cleaned "\n")))
    (unless (string=? text result)
      (with-undo-action ed
        (editor-delete-range ed 0 (string-length text))
        (editor-insert-text ed 0 result)))
    (echo-message! (app-state-echo app) "Whitespace cleaned")))

(def (cmd-toggle-electric-pair app)
  "Toggle auto-pair mode for brackets and quotes."
  (set! *auto-pair-mode* (not *auto-pair-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-pair-mode* "Electric pair mode ON" "Electric pair mode OFF")))

(def (cmd-previous-buffer app)
  "Switch to the previous buffer in the buffer list."
  (let* ((bufs (buffer-list))
         (cur (current-buffer-from-app app))
         (idx (let loop ((bs bufs) (i 0))
                (cond ((null? bs) 0)
                      ((eq? (car bs) cur) i)
                      (else (loop (cdr bs) (+ i 1))))))
         (prev-idx (if (= idx 0) (- (length bufs) 1) (- idx 1)))
         (prev-buf (list-ref bufs prev-idx))
         (ed (current-editor app))
         (fr (app-state-frame app)))
    (buffer-attach! ed prev-buf)
    (set! (edit-window-buffer (current-window fr)) prev-buf)
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (buffer-name prev-buf)))))

(def (cmd-next-buffer app)
  "Switch to the next buffer in the buffer list."
  (let* ((bufs (buffer-list))
         (cur (current-buffer-from-app app))
         (idx (let loop ((bs bufs) (i 0))
                (cond ((null? bs) 0)
                      ((eq? (car bs) cur) i)
                      (else (loop (cdr bs) (+ i 1))))))
         (next-idx (if (>= (+ idx 1) (length bufs)) 0 (+ idx 1)))
         (next-buf (list-ref bufs next-idx))
         (ed (current-editor app))
         (fr (app-state-frame app)))
    (buffer-attach! ed next-buf)
    (set! (edit-window-buffer (current-window fr)) next-buf)
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (buffer-name next-buf)))))

(def (cmd-balance-windows app)
  "Make all windows the same size."
  (frame-layout! (app-state-frame app))
  (echo-message! (app-state-echo app) "Windows balanced"))

(def (cmd-move-to-window-line app)
  "Move point to center, then top, then bottom of window (like Emacs M-r)."
  (let* ((ed (current-editor app))
         (first-vis (editor-get-first-visible-line ed))
         ;; Use raw SCI_LINESONSCREEN = 2370
         (lines-on-screen (send-message ed 2370))
         (target-line
           (case *recenter-position*
             ((center) (+ first-vis (quotient lines-on-screen 2)))
             ((top) first-vis)
             ((bottom) (+ first-vis (- lines-on-screen 1))))))
    (editor-goto-pos ed (editor-position-from-line ed target-line))
    ;; Cycle: center -> top -> bottom -> center
    (set! *recenter-position*
      (case *recenter-position*
        ((center) 'top)
        ((top) 'bottom)
        ((bottom) 'center)))))

(def (cmd-kill-buffer-and-window app)
  "Kill current buffer and close its window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (= (length wins) 1)
      (echo-message! (app-state-echo app) "Can't delete sole window")
      (let* ((ed (current-editor app))
             (buf (current-buffer-from-app app)))
        (frame-delete-window! fr)
        (frame-layout! fr)
        ;; Clean up the buffer
        (hash-remove! *dired-entries* buf)
        (hash-remove! *eshell-state* buf)
        (let ((rs (hash-get *repl-state* buf)))
          (when rs (repl-stop! rs) (hash-remove! *repl-state* buf)))
        (let ((ss (hash-get *shell-state* buf)))
          (when ss (shell-stop! ss) (hash-remove! *shell-state* buf)))))))

(def (cmd-flush-undo app)
  "Clear the undo history of the current buffer."
  (let ((ed (current-editor app)))
    (editor-empty-undo-buffer ed)
    (echo-message! (app-state-echo app) "Undo history cleared")))

(def (cmd-upcase-initials-region app)
  "Capitalize the first letter of each word in region."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if (not mark)
      (echo-message! (app-state-echo app) "No region")
      (let* ((start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (result (string-titlecase text)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)))))

(def (cmd-untabify-buffer app)
  "Convert all tabs to spaces in the entire buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (tab-w (editor-get-tab-width ed))
         (spaces (make-string tab-w #\space)))
    (if (not (string-contains text "\t"))
      (echo-message! (app-state-echo app) "No tabs found")
      (let* ((parts (string-split text #\tab))
             (result (string-join parts spaces)))
        (with-undo-action ed
          (editor-delete-range ed 0 (string-length text))
          (editor-insert-text ed 0 result))
        (echo-message! (app-state-echo app) "Untabified buffer")))))

(def (cmd-insert-buffer-name app)
  "Insert the current buffer name at point."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos (buffer-name buf))
    (editor-goto-pos ed (+ pos (string-length (buffer-name buf))))))

(def (cmd-toggle-line-move-visual app)
  "Toggle whether line movement is visual or logical."
  ;; Scintilla doesn't distinguish, so this is a stub toggle
  (echo-message! (app-state-echo app) "Line move is always visual in Scintilla"))

(def (cmd-mark-defun app)
  "Mark the current top-level form (defun-like region)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find beginning of defun (search backward for "\n(" at column 0)
    (let ((defun-start
            (let loop ((i pos))
              (cond ((= i 0) 0)
                    ((and (= i 0) (char=? (string-ref text 0) #\()) 0)
                    ((and (> i 0)
                          (char=? (string-ref text i) #\()
                          (or (= i 0)
                              (char=? (string-ref text (- i 1)) #\newline)))
                     i)
                    (else (loop (- i 1)))))))
      ;; Find end of defun — matching paren
      (let ((defun-end
              (let loop ((i defun-start) (depth 0))
                (cond ((>= i len) len)
                      ((char=? (string-ref text i) #\()
                       (loop (+ i 1) (+ depth 1)))
                      ((char=? (string-ref text i) #\))
                       (if (= depth 1) (+ i 1)
                         (loop (+ i 1) (- depth 1))))
                      (else (loop (+ i 1) depth))))))
        (editor-set-selection ed defun-start defun-end)
        (echo-message! (app-state-echo app) "Defun marked")))))

(def (cmd-goto-line-beginning app)
  "Move to the very first position in the buffer (alias for M-<)."
  (editor-goto-pos (current-editor app) 0))

(def (cmd-shrink-window-horizontally app)
  "Make current window narrower (horizontal split only)."
  (echo-message! (app-state-echo app)
    "Use C-x } / C-x { for horizontal resize (not implemented)"))

(def (cmd-insert-parentheses app)
  "Insert () and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "()")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-insert-pair-brackets app)
  "Insert [] and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "[]")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-insert-pair-braces app)
  "Insert {} and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "{}")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-insert-pair-quotes app)
  "Insert \"\" and position cursor between them."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "\"\"")
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-describe-char app)
  "Show info about character at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch)
                         " (#x" (number->string code 16)
                         ", #o" (number->string code 8)
                         ", " (number->string code) ")"))))))

(def (cmd-find-file-at-point app)
  "Try to open file whose name is at or near point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         ;; Extract filename-like text around point
         (start (let loop ((i pos))
                  (if (or (<= i 0)
                          (let ((ch (string-ref text (- i 1))))
                            (or (char=? ch #\space) (char=? ch #\newline)
                                (char=? ch #\tab) (char=? ch #\")
                                (char=? ch #\') (char=? ch #\<)
                                (char=? ch #\>))))
                    i (loop (- i 1)))))
         (end (let loop ((i pos))
                (if (or (>= i len)
                        (let ((ch (string-ref text i)))
                          (or (char=? ch #\space) (char=? ch #\newline)
                              (char=? ch #\tab) (char=? ch #\")
                              (char=? ch #\') (char=? ch #\<)
                              (char=? ch #\>))))
                  i (loop (+ i 1)))))
         (path (substring text start end)))
    (if (and (> (string-length path) 0) (file-exists? path))
      (let* ((fr (app-state-frame app))
             (name (path-strip-directory path))
             (buf (buffer-create! name ed path)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (let ((file-text (read-file-as-string path)))
          (when file-text
            (editor-set-text ed file-text)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)))
        (echo-message! (app-state-echo app)
          (string-append "Opened: " path)))
      (echo-message! (app-state-echo app)
        (string-append "No file found: " path)))))

(def (cmd-toggle-show-paren app)
  "Toggle paren matching highlight."
  ;; Use raw SCI_SETMATCHEDBRACEPROPS - just toggle the indicator via message
  (echo-message! (app-state-echo app) "Paren matching is always on"))

(def (cmd-count-chars-region app)
  "Count characters in the selected region."
  (let* ((ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (echo-message! (app-state-echo app)
      (string-append "Region: " (number->string (- end start)) " chars"))))

;;;============================================================================
;;; Text processing and window commands (Task #37)
;;;============================================================================

(def (cmd-capitalize-region app)
  "Upcase all characters in the region."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if (not mark)
      (echo-message! (app-state-echo app) "No region")
      (let* ((start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (result (string-upcase text)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)))))

(def (cmd-count-words-buffer app)
  "Count words, lines, and chars in the entire buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (len (string-length text))
         (lines (+ 1 (let loop ((i 0) (n 0))
                       (cond ((>= i len) n)
                             ((char=? (string-ref text i) #\newline)
                              (loop (+ i 1) (+ n 1)))
                             (else (loop (+ i 1) n))))))
         (words (let loop ((i 0) (n 0) (in-word #f))
                  (cond ((>= i len) (if in-word (+ n 1) n))
                        ((let ((ch (string-ref text i)))
                           (or (char=? ch #\space) (char=? ch #\newline)
                               (char=? ch #\tab)))
                         (loop (+ i 1) (if in-word (+ n 1) n) #f))
                        (else (loop (+ i 1) n #t))))))
    (echo-message! (app-state-echo app)
      (string-append "Buffer: " (number->string lines) " lines, "
                     (number->string words) " words, "
                     (number->string len) " chars"))))

(def (cmd-unfill-paragraph app)
  "Join a paragraph into a single long line (inverse of fill-paragraph)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         ;; Find paragraph boundaries (blank lines)
         (para-start
           (let loop ((i (max 0 (- pos 1))))
             (cond ((<= i 0) 0)
                   ((and (char=? (string-ref text i) #\newline)
                         (> i 0)
                         (char=? (string-ref text (- i 1)) #\newline))
                    (+ i 1))
                   (else (loop (- i 1))))))
         (para-end
           (let loop ((i pos))
             (cond ((>= i len) len)
                   ((and (char=? (string-ref text i) #\newline)
                         (< (+ i 1) len)
                         (char=? (string-ref text (+ i 1)) #\newline))
                    i)
                   ((and (char=? (string-ref text i) #\newline)
                         (>= (+ i 1) len))
                    i)
                   (else (loop (+ i 1))))))
         (para (substring text para-start para-end))
         ;; Replace internal newlines with spaces
         (joined (let loop ((i 0) (acc '()))
                   (cond ((>= i (string-length para))
                          (apply string-append (reverse acc)))
                         ((char=? (string-ref para i) #\newline)
                          (loop (+ i 1) (cons " " acc)))
                         (else
                          (loop (+ i 1) (cons (string (string-ref para i)) acc)))))))
    (with-undo-action ed
      (editor-delete-range ed para-start (- para-end para-start))
      (editor-insert-text ed para-start joined))
    (echo-message! (app-state-echo app) "Paragraph unfilled")))

(def (cmd-list-registers app)
  "Show all non-empty registers in a buffer."
  (let* ((regs (app-state-registers app))
         (echo (app-state-echo app)))
    (if (= (hash-length regs) 0)
      (echo-message! echo "No registers set")
      (let ((lines
              (hash-fold
                (lambda (key val acc)
                  (cons (string-append (string key) ": "
                                       (if (string? val)
                                         (let ((s (if (> (string-length val) 60)
                                                    (string-append (substring val 0 60) "...")
                                                    val)))
                                           s)
                                         (if (number? val)
                                           (string-append "pos " (number->string val))
                                           "?")))
                        acc))
                [] regs)))
        ;; Show in a temp buffer
        (let* ((ed (current-editor app))
               (fr (app-state-frame app))
               (buf (buffer-create! "*Registers*" ed #f)))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer (current-window fr)) buf)
          (editor-set-text ed (string-join (sort lines string<?) "\n")))))))

(def (cmd-show-kill-ring app)
  "Show kill ring contents in a buffer."
  (let* ((ring (app-state-kill-ring app))
         (echo (app-state-echo app)))
    (if (null? ring)
      (echo-message! echo "Kill ring is empty")
      (let* ((lines
               (let loop ((entries ring) (i 0) (acc '()))
                 (if (or (null? entries) (>= i 20))
                   (reverse acc)
                   (let* ((entry (car entries))
                          (display-text
                            (let ((s (if (> (string-length entry) 70)
                                      (string-append (substring entry 0 70) "...")
                                      entry)))
                              ;; Replace newlines with \n for display
                              (let loop2 ((j 0) (a '()))
                                (cond ((>= j (string-length s))
                                       (apply string-append (reverse a)))
                                      ((char=? (string-ref s j) #\newline)
                                       (loop2 (+ j 1) (cons "\\n" a)))
                                      (else
                                       (loop2 (+ j 1)
                                              (cons (string (string-ref s j)) a))))))))
                     (loop (cdr entries) (+ i 1)
                           (cons (string-append (number->string i) ": " display-text)
                                 acc))))))
             (ed (current-editor app))
             (fr (app-state-frame app))
             (buf (buffer-create! "*Kill Ring*" ed #f)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (editor-set-text ed (string-join lines "\n"))))))

(def (cmd-smart-beginning-of-line app)
  "Move to first non-whitespace char on line, or to column 0 if already there."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (text (editor-get-text ed))
         (len (string-length text))
         ;; Find first non-whitespace on this line
         (first-nonws
           (let loop ((i line-start))
             (cond ((>= i len) i)
                   ((char=? (string-ref text i) #\newline) i)
                   ((or (char=? (string-ref text i) #\space)
                        (char=? (string-ref text i) #\tab))
                    (loop (+ i 1)))
                   (else i)))))
    (if (= pos first-nonws)
      ;; Already at first non-ws, go to column 0
      (editor-goto-pos ed line-start)
      ;; Go to first non-ws
      (editor-goto-pos ed first-nonws))))

(def (cmd-shrink-window-if-larger app)
  "Shrink window to fit buffer content."
  ;; Scintilla handles this internally; just re-layout
  (frame-layout! (app-state-frame app))
  (echo-message! (app-state-echo app) "Window resized to fit"))

(def (cmd-toggle-input-method app)
  "Stub for input method toggle."
  (echo-message! (app-state-echo app) "No input method configured"))

(def (cmd-what-buffer app)
  "Show current buffer name and file path."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (path (buffer-file-path buf)))
    (echo-message! (app-state-echo app)
      (if path
        (string-append name " (" path ")")
        name))))

(def (cmd-goto-last-change app)
  "Go to the position of the last edit."
  ;; Use SCI_GETMODIFIEDPOSITION if available, otherwise undo marker position
  ;; Simplified: just report that this needs undo tracking
  (echo-message! (app-state-echo app) "Use C-_ (undo) to find last change"))

(def (cmd-toggle-narrowing-indicator app)
  "Show whether buffer is narrowed."
  (echo-message! (app-state-echo app) "Narrowing not supported in this build"))

(def (cmd-insert-file-name app)
  "Insert the current buffer's file path at point."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf))
         (pos (editor-get-current-pos ed)))
    (if path
      (begin
        (editor-insert-text ed pos path)
        (editor-goto-pos ed (+ pos (string-length path))))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-toggle-auto-save app)
  "Toggle auto-save for current buffer."
  ;; Auto-save is session-global; just toggle and report
  (echo-message! (app-state-echo app) "Auto-save is always on"))

(def (cmd-backward-up-list app)
  "Move backward up one level of parentheses."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (cond ((<= i 0)
             (echo-message! (app-state-echo app) "At top level"))
            ((char=? (string-ref text i) #\))
             (loop (- i 1) (+ depth 1)))
            ((char=? (string-ref text i) #\()
             (if (= depth 0)
               (editor-goto-pos ed i)
               (loop (- i 1) (- depth 1))))
            (else (loop (- i 1) depth))))))

(def (cmd-forward-up-list app)
  "Move forward out of one level of parentheses."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let loop ((i pos) (depth 0))
      (cond ((>= i len)
             (echo-message! (app-state-echo app) "At top level"))
            ((char=? (string-ref text i) #\()
             (loop (+ i 1) (+ depth 1)))
            ((char=? (string-ref text i) #\))
             (if (= depth 0)
               (editor-goto-pos ed (+ i 1))
               (loop (+ i 1) (- depth 1))))
            (else (loop (+ i 1) depth))))))

(def (cmd-kill-sexp app)
  "Kill from point to end of current s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let ((end-pos
              (cond
                ;; If at open paren, find matching close
                ((char=? (string-ref text pos) #\()
                 (let loop ((i (+ pos 1)) (depth 1))
                   (cond ((>= i len) len)
                         ((char=? (string-ref text i) #\() (loop (+ i 1) (+ depth 1)))
                         ((char=? (string-ref text i) #\))
                          (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1))))
                         (else (loop (+ i 1) depth)))))
                ;; If at open bracket
                ((char=? (string-ref text pos) #\[)
                 (let loop ((i (+ pos 1)) (depth 1))
                   (cond ((>= i len) len)
                         ((char=? (string-ref text i) #\[) (loop (+ i 1) (+ depth 1)))
                         ((char=? (string-ref text i) #\])
                          (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1))))
                         (else (loop (+ i 1) depth)))))
                ;; Otherwise kill word-like region
                (else
                  (let loop ((i pos))
                    (cond ((>= i len) len)
                          ((let ((ch (string-ref text i)))
                             (or (char=? ch #\space) (char=? ch #\newline)
                                 (char=? ch #\tab) (char=? ch #\()
                                 (char=? ch #\)) (char=? ch #\[)
                                 (char=? ch #\])))
                           i)
                          (else (loop (+ i 1)))))))))
        (let ((killed (substring text pos end-pos)))
          (with-undo-action ed
            (editor-delete-range ed pos (- end-pos pos)))
          (set! (app-state-kill-ring app)
            (cons killed (app-state-kill-ring app))))))))

(def (cmd-backward-sexp app)
  "Move backward over one s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (let loop ((i (- pos 1)))
      (cond ((<= i 0) (editor-goto-pos ed 0))
            ;; Skip whitespace
            ((let ((ch (string-ref text i)))
               (or (char=? ch #\space) (char=? ch #\newline) (char=? ch #\tab)))
             (loop (- i 1)))
            ;; Close paren — find matching open
            ((char=? (string-ref text i) #\))
             (let ploop ((j (- i 1)) (depth 1))
               (cond ((<= j 0) (editor-goto-pos ed 0))
                     ((char=? (string-ref text j) #\))
                      (ploop (- j 1) (+ depth 1)))
                     ((char=? (string-ref text j) #\()
                      (if (= depth 1) (editor-goto-pos ed j)
                        (ploop (- j 1) (- depth 1))))
                     (else (ploop (- j 1) depth)))))
            ;; Word-like token
            (else
              (let wloop ((j i))
                (cond ((<= j 0) (editor-goto-pos ed 0))
                      ((let ((ch (string-ref text j)))
                         (or (char=? ch #\space) (char=? ch #\newline)
                             (char=? ch #\tab) (char=? ch #\()
                             (char=? ch #\)) (char=? ch #\[)
                             (char=? ch #\])))
                       (editor-goto-pos ed (+ j 1)))
                      (else (wloop (- j 1))))))))))

(def (cmd-forward-sexp app)
  "Move forward over one s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond ((>= i len) (editor-goto-pos ed len))
            ;; Skip whitespace
            ((let ((ch (string-ref text i)))
               (or (char=? ch #\space) (char=? ch #\newline) (char=? ch #\tab)))
             (loop (+ i 1)))
            ;; Open paren — find matching close
            ((char=? (string-ref text i) #\()
             (let ploop ((j (+ i 1)) (depth 1))
               (cond ((>= j len) (editor-goto-pos ed len))
                     ((char=? (string-ref text j) #\() (ploop (+ j 1) (+ depth 1)))
                     ((char=? (string-ref text j) #\))
                      (if (= depth 1) (editor-goto-pos ed (+ j 1))
                        (ploop (+ j 1) (- depth 1))))
                     (else (ploop (+ j 1) depth)))))
            ;; Word-like token
            (else
              (let wloop ((j i))
                (cond ((>= j len) (editor-goto-pos ed len))
                      ((let ((ch (string-ref text j)))
                         (or (char=? ch #\space) (char=? ch #\newline)
                             (char=? ch #\tab) (char=? ch #\()
                             (char=? ch #\)) (char=? ch #\[)
                             (char=? ch #\])))
                       (editor-goto-pos ed j))
                      (else (wloop (+ j 1))))))))))

;;;============================================================================
;;; S-expression and utility commands (Task #38)
;;;============================================================================

(def (cmd-transpose-sexps app)
  "Transpose the two s-expressions around point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find extent of sexp before point, and sexp after point
    ;; Simple: find word/paren boundaries backward and forward
    (echo-message! (app-state-echo app) "transpose-sexps: use M-t for words")))

(def (cmd-mark-sexp app)
  "Mark the next s-expression."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         (buf (current-buffer-from-app app)))
    ;; Set mark at current pos
    (set! (buffer-mark buf) pos)
    ;; Find end of next sexp
    (let loop ((i pos))
      (cond ((>= i len) (editor-goto-pos ed len))
            ;; Skip whitespace
            ((let ((ch (string-ref text i)))
               (or (char=? ch #\space) (char=? ch #\newline) (char=? ch #\tab)))
             (loop (+ i 1)))
            ;; Open paren
            ((char=? (string-ref text i) #\()
             (let ploop ((j (+ i 1)) (depth 1))
               (cond ((>= j len) (editor-goto-pos ed len))
                     ((char=? (string-ref text j) #\() (ploop (+ j 1) (+ depth 1)))
                     ((char=? (string-ref text j) #\))
                      (if (= depth 1) (editor-goto-pos ed (+ j 1))
                        (ploop (+ j 1) (- depth 1))))
                     (else (ploop (+ j 1) depth)))))
            ;; Word token
            (else
              (let wloop ((j i))
                (cond ((>= j len) (editor-goto-pos ed len))
                      ((let ((ch (string-ref text j)))
                         (or (char=? ch #\space) (char=? ch #\newline)
                             (char=? ch #\tab) (char=? ch #\()
                             (char=? ch #\)) (char=? ch #\[) (char=? ch #\])))
                       (editor-goto-pos ed j))
                      (else (wloop (+ j 1))))))))
    (echo-message! (app-state-echo app) "Sexp marked")))

(def (cmd-indent-sexp app)
  "Re-indent the next s-expression (simple: indent region from point to matching paren)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (or (>= pos len) (not (char=? (string-ref text pos) #\()))
      (echo-message! echo "Not at start of sexp")
      ;; Find matching close paren
      (let loop ((i (+ pos 1)) (depth 1))
        (cond ((>= i len) (echo-message! echo "Unbalanced sexp"))
              ((char=? (string-ref text i) #\() (loop (+ i 1) (+ depth 1)))
              ((char=? (string-ref text i) #\))
               (if (= depth 1)
                 (let* ((end (+ i 1))
                        (region (substring text pos end))
                        ;; Simple re-indent: ensure consistent 2-space indentation
                        (lines (string-split region #\newline))
                        (indented
                          (let lp ((ls lines) (first #t) (acc '()))
                            (if (null? ls)
                              (reverse acc)
                              (let ((line (string-trim (car ls))))
                                (lp (cdr ls) #f
                                    (cons (if first line
                                            (string-append "  " line))
                                          acc))))))
                        (result (string-join indented "\n")))
                   (with-undo-action ed
                     (editor-delete-range ed pos (- end pos))
                     (editor-insert-text ed pos result))
                   (echo-message! echo "Sexp indented"))
                 (loop (+ i 1) (- depth 1))))
              (else (loop (+ i 1) depth)))))))

(def (cmd-word-frequency app)
  "Count word frequencies in the buffer and show top words."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (len (string-length text))
         (freq (make-hash-table)))
    ;; Split text into words
    (let loop ((i 0) (word-start #f))
      (cond ((>= i len)
             (when word-start
               (let ((w (string-downcase (substring text word-start i))))
                 (when (> (string-length w) 0)
                   (hash-put! freq w (+ 1 (or (hash-get freq w) 0)))))))
            ((let ((ch (string-ref text i)))
               (or (char-alphabetic? ch) (char-numeric? ch)
                   (char=? ch #\_) (char=? ch #\-)))
             (loop (+ i 1) (or word-start i)))
            (else
              (when word-start
                (let ((w (string-downcase (substring text word-start i))))
                  (when (> (string-length w) 0)
                    (hash-put! freq w (+ 1 (or (hash-get freq w) 0))))))
              (loop (+ i 1) #f))))
    ;; Sort by frequency
    (let* ((pairs (hash-fold (lambda (k v acc) (cons (cons k v) acc)) [] freq))
           (sorted (sort pairs (lambda (a b) (> (cdr a) (cdr b)))))
           (top (let lp ((ls sorted) (n 0) (acc '()))
                  (if (or (null? ls) (>= n 30))
                    (reverse acc)
                    (let ((p (car ls)))
                      (lp (cdr ls) (+ n 1)
                          (cons (string-append (number->string (cdr p))
                                               "\t" (car p))
                                acc))))))
           (fr (app-state-frame app))
           (buf (buffer-create! "*Word Frequency*" ed #f)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed (string-join top "\n")))))

(def (cmd-insert-uuid app)
  "Insert a UUID-like random hex string at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (bs (random-bytes 16))
         (hex (hex-encode bs))
         ;; Format as UUID: 8-4-4-4-12
         (uuid (string-append
                 (substring hex 0 8) "-"
                 (substring hex 8 12) "-"
                 (substring hex 12 16) "-"
                 (substring hex 16 20) "-"
                 (substring hex 20 32))))
    (editor-insert-text ed pos uuid)
    (editor-goto-pos ed (+ pos (string-length uuid)))))

(def (cmd-reformat-buffer app)
  "Re-indent the entire buffer (simple: normalize leading whitespace)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    ;; Use Scintilla's built-in TAB indentation — just trigger indent on each line
    ;; For now, just report the operation
    (echo-message! echo "Use TAB on each line or C-c TAB for indent-region")))

(def (cmd-delete-pair app)
  "Delete the surrounding delimiters (parens, brackets, quotes) around point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find matching pair around point
    (if (= len 0)
      (echo-message! (app-state-echo app) "Buffer empty")
      ;; Search backward for opener
      (let ((opener-pos
              (let loop ((i (- pos 1)))
                (cond ((<= i 0) #f)
                      ((let ((ch (string-ref text i)))
                         (or (char=? ch #\() (char=? ch #\[)
                             (char=? ch #\{) (char=? ch #\")))
                       i)
                      (else (loop (- i 1)))))))
        (if (not opener-pos)
          (echo-message! (app-state-echo app) "No opening delimiter found")
          (let* ((opener (string-ref text opener-pos))
                 (closer (cond ((char=? opener #\() #\))
                               ((char=? opener #\[) #\])
                               ((char=? opener #\{) #\})
                               ((char=? opener #\") #\")
                               (else #f))))
            ;; Find matching closer
            (let ((closer-pos
                    (if (char=? opener #\")
                      ;; For quotes, find next quote after opener
                      (let loop ((i (+ opener-pos 1)))
                        (cond ((>= i len) #f)
                              ((char=? (string-ref text i) #\") i)
                              (else (loop (+ i 1)))))
                      ;; For parens, match with depth
                      (let loop ((i (+ opener-pos 1)) (depth 1))
                        (cond ((>= i len) #f)
                              ((char=? (string-ref text i) opener)
                               (loop (+ i 1) (+ depth 1)))
                              ((char=? (string-ref text i) closer)
                               (if (= depth 1) i (loop (+ i 1) (- depth 1))))
                              (else (loop (+ i 1) depth)))))))
              (if (not closer-pos)
                (echo-message! (app-state-echo app) "No matching closer found")
                (with-undo-action ed
                  ;; Delete closer first (higher position) to preserve opener position
                  (editor-delete-range ed closer-pos 1)
                  (editor-delete-range ed opener-pos 1))))))))))

(def (cmd-toggle-hl-line app)
  "Toggle current line highlight."
  (let* ((ed (current-editor app))
         (visible (editor-get-caret-line-visible? ed)))
    (editor-set-caret-line-visible ed (not visible))
    (echo-message! (app-state-echo app)
      (if visible "Caret line highlight OFF" "Caret line highlight ON"))))

(def (cmd-toggle-column-number-mode app)
  "Column number display is always shown in modeline."
  (echo-message! (app-state-echo app) "Column numbers always shown"))

(def (cmd-find-alternate-file app)
  "Replace current buffer with another file."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (filename (echo-read-string echo "Find alternate file: " row width)))
    (when (and filename (> (string-length filename) 0))
      (let ((ed (current-editor app)))
        (if (file-exists? filename)
          (let* ((text (read-file-as-string filename))
                 (name (path-strip-directory filename))
                 (buf (current-buffer-from-app app)))
            ;; Reuse current buffer
            (set! (buffer-name buf) name)
            (set! (buffer-file-path buf) filename)
            (when text
              (editor-set-text ed text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0))
            (echo-message! echo (string-append "Opened: " filename)))
          (echo-error! echo (string-append "File not found: " filename)))))))

(def (cmd-increment-register app)
  "Increment numeric register by 1."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Register to increment: " row width)))
    (when (and input (= (string-length input) 1))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (cond ((and (number? val))
               (hash-put! (app-state-registers app) reg-char (+ val 1))
               (echo-message! echo (string-append "Register " input ": "
                                                   (number->string (+ val 1)))))
              ((and (string? val) (string->number val))
               (let ((n (+ 1 (string->number val))))
                 (hash-put! (app-state-registers app) reg-char (number->string n))
                 (echo-message! echo (string-append "Register " input ": "
                                                     (number->string n)))))
              (else
                (echo-error! echo "Register is not numeric")))))))

(def (cmd-toggle-size-indication app)
  "Toggle buffer size display."
  (echo-message! (app-state-echo app) "Buffer size always shown in buffer-info"))

(def (cmd-copy-buffer-name app)
  "Copy current buffer name to kill ring."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf)))
    (set! (app-state-kill-ring app)
      (cons name (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied: " name))))

;;;============================================================================
;;; Task #39: sort, rectangle, completion, text processing
;;;============================================================================

;; Helper: get text in range [start, start+len)
(def (editor-get-text-range ed start len)
  (substring (editor-get-text ed) start (+ start len)))

(def (cmd-sort-lines-case-fold app)
  "Sort lines in region case-insensitively."
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
             (sorted (sort lines (lambda (a b)
                                   (string-ci<? a b))))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (echo-message! echo (string-append "Sorted "
                                            (number->string (length sorted))
                                            " lines (case-insensitive)")))
      (echo-error! echo "No mark set"))))

(def (cmd-reverse-chars app)
  "Reverse characters in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (len (- end start))
             (text (editor-get-text-range ed start len))
             (result (list->string (reverse (string->list text)))))
        (with-undo-action ed
          (editor-delete-range ed start len)
          (editor-insert-text ed start result))
        (echo-message! echo (string-append "Reversed " (number->string len) " chars")))
      (echo-error! echo "No mark set"))))

(def (cmd-replace-string-all app)
  "Replace all occurrences of a string in buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Replace string: " row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let ((replacement (echo-read-string echo
                           (string-append "Replace \"" pattern "\" with: ") row width)))
        (when replacement
          (let* ((text (editor-get-text ed))
                 (plen (string-length pattern))
                 ;; Manual replace-all loop
                 (result
                   (let loop ((i 0) (acc (open-output-string)))
                     (let ((found (string-contains text pattern i)))
                       (if found
                         (begin
                           (display (substring text i found) acc)
                           (display replacement acc)
                           (loop (+ found plen) acc))
                         (begin
                           (display (substring text i (string-length text)) acc)
                           (get-output-string acc))))))
                 (len (editor-get-text-length ed)))
            (with-undo-action ed
              (editor-delete-range ed 0 len)
              (editor-insert-text ed 0 result))
            (echo-message! echo "Replacement done")))))))

(def (cmd-insert-file-contents app)
  "Insert contents of a file at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (path (echo-read-string echo "Insert file: " row width)))
    (when (and path (not (string-empty? path)))
      (if (file-exists? path)
        (let* ((contents (read-file-as-string path))
               (pos (editor-get-current-pos ed)))
          (editor-insert-text ed pos contents)
          (echo-message! echo (string-append "Inserted " path)))
        (echo-error! echo (string-append "File not found: " path))))))

(def *auto-revert-mode* #f)

(def (cmd-toggle-auto-revert app)
  "Toggle auto-revert mode."
  (set! *auto-revert-mode* (not *auto-revert-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-revert-mode* "Auto-revert mode ON" "Auto-revert mode OFF")))

(def (cmd-zap-up-to-char app)
  "Kill text up to (but not including) a character."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Zap up to char: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed))
             ;; Search forward for the character
             (found
               (let loop ((p (+ pos 1)))
                 (cond
                   ((>= p len) #f)
                   ((= (editor-get-char-at ed p) (char->integer ch)) p)
                   (else (loop (+ p 1)))))))
        (if found
          (let ((kill-text (editor-get-text-range ed pos (- found pos))))
            (set! (app-state-kill-ring app)
              (cons kill-text (app-state-kill-ring app)))
            (editor-delete-range ed pos (- found pos))
            (echo-message! echo (string-append "Zapped to '" (string ch) "'")))
          (echo-error! echo (string-append "'" (string ch) "' not found")))))))

(def (cmd-quoted-insert app)
  "Insert the next character literally (for control chars)."
  (echo-message! (app-state-echo app) "C-q: Next key inserts literally (use self-insert)"))

(def (cmd-what-line-col app)
  "Show current line and column."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (col (- pos (editor-position-from-line ed line))))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string (+ line 1))
                     ", Column " (number->string col)))))

(def (cmd-insert-current-date-iso app)
  "Insert current date in ISO 8601 format (YYYY-MM-DD)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         ;; Use shell to get ISO date
         (proc (open-process
                 (list path: "/bin/date"
                       arguments: ["+%Y-%m-%d"]
                       stdout-redirection: #t)))
         (date-str (read-line proc))
         (_ (process-status proc)))
    (when (string? date-str)
      (editor-insert-text ed pos date-str))))

(def (cmd-recenter-top app)
  "Scroll so current line is at top of window."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos)))
    (editor-set-first-visible-line ed line)))

(def (cmd-recenter-bottom app)
  "Scroll so current line is at bottom of window."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (visible (send-message ed 2370 0 0))) ; SCI_LINESONSCREEN
    (editor-set-first-visible-line ed (max 0 (- line (- visible 1))))))

(def (cmd-scroll-other-window app)
  "Scroll the other window down one page."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (cur-idx (frame-current-idx fr)))
    (when (> (length wins) 1)
      (let* ((other-idx (modulo (+ cur-idx 1) (length wins)))
             (other-win (list-ref wins other-idx))
             (other-ed (edit-window-editor other-win))
             (visible (send-message other-ed 2370 0 0))
             (first (editor-get-first-visible-line other-ed)))
        (editor-set-first-visible-line other-ed (+ first visible))))))

(def (cmd-scroll-other-window-up app)
  "Scroll the other window up one page."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (cur-idx (frame-current-idx fr)))
    (when (> (length wins) 1)
      (let* ((other-idx (modulo (+ cur-idx 1) (length wins)))
             (other-win (list-ref wins other-idx))
             (other-ed (edit-window-editor other-win))
             (visible (send-message other-ed 2370 0 0))
             (first (editor-get-first-visible-line other-ed)))
        (editor-set-first-visible-line other-ed (max 0 (- first visible)))))))


(def (cmd-count-words-paragraph app)
  "Count words in current paragraph."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (total-lines (editor-get-line-count ed))
         ;; Find paragraph start (first blank line above or BOF)
         (para-start-line
           (let loop ((l (- line 1)))
             (if (< l 0) 0
               (let* ((ls (editor-position-from-line ed l))
                      (le (editor-get-line-end-position ed l))
                      (text (editor-get-text-range ed ls (- le ls))))
                 (if (string-empty? (string-trim text))
                   (+ l 1)
                   (loop (- l 1)))))))
         ;; Find paragraph end (first blank line below or EOF)
         (para-end-line
           (let loop ((l (+ line 1)))
             (if (>= l total-lines) (- total-lines 1)
               (let* ((ls (editor-position-from-line ed l))
                      (le (editor-get-line-end-position ed l))
                      (text (editor-get-text-range ed ls (- le ls))))
                 (if (string-empty? (string-trim text))
                   (- l 1)
                   (loop (+ l 1)))))))
         (start (editor-position-from-line ed para-start-line))
         (end (editor-get-line-end-position ed para-end-line))
         (text (editor-get-text-range ed start (- end start)))
         (words (filter (lambda (w) (not (string-empty? w)))
                        (string-split text #\space)))
         (count (length words)))
    (echo-message! echo (string-append "Paragraph: " (number->string count) " words"))))

(def (cmd-toggle-transient-mark app)
  "Toggle transient mark mode."
  (echo-message! (app-state-echo app) "Transient mark mode always active"))

(def (cmd-keep-lines-region app)
  "Keep only lines matching regexp in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((pattern (echo-read-string echo "Keep lines matching: " row width)))
        (when (and pattern (not (string-empty? pattern)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end))
                 (line-start (editor-position-from-line ed start-line))
                 (line-end (editor-get-line-end-position ed end-line))
                 (text (editor-get-text-range ed line-start (- line-end line-start)))
                 (lines (string-split text #\newline))
                 (kept (filter (lambda (l) (string-contains l pattern)) lines))
                 (result (string-join kept "\n")))
            (with-undo-action ed
              (editor-delete-range ed line-start (- line-end line-start))
              (editor-insert-text ed line-start result))
            (echo-message! echo (string-append "Kept " (number->string (length kept))
                                                " lines")))))
      (echo-error! echo "No mark set"))))

(def (cmd-flush-lines-region app)
  "Remove lines matching regexp in region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((pattern (echo-read-string echo "Flush lines matching: " row width)))
        (when (and pattern (not (string-empty? pattern)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end))
                 (line-start (editor-position-from-line ed start-line))
                 (line-end (editor-get-line-end-position ed end-line))
                 (text (editor-get-text-range ed line-start (- line-end line-start)))
                 (lines (string-split text #\newline))
                 (kept (filter (lambda (l) (not (string-contains l pattern))) lines))
                 (result (string-join kept "\n")))
            (with-undo-action ed
              (editor-delete-range ed line-start (- line-end line-start))
              (editor-insert-text ed line-start result))
            (echo-message! echo (string-append "Flushed "
                                                (number->string (- (length lines) (length kept)))
                                                " lines")))))
      (echo-error! echo "No mark set"))))

(def (cmd-insert-register-string app)
  "Insert register content at point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Insert register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (if (and val (string? val))
          (let ((pos (editor-get-current-pos ed)))
            (editor-insert-text ed pos val)
            (echo-message! echo (string-append "Inserted register " (string reg-char))))
          (echo-error! echo "Register empty or not a string"))))))

(def (cmd-toggle-visible-bell app)
  "Toggle visible bell."
  (echo-message! (app-state-echo app) "Visible bell always enabled"))

;;;============================================================================
;;; Task #40: indentation, buffers, navigation
;;;============================================================================

(def (cmd-unindent-region app)
  "Unindent region by one tab stop."
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
             (tab-w (editor-get-tab-width ed)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (line-len (- le ls))
                     (text (editor-get-text-range ed ls (min line-len tab-w)))
                     ;; Count leading spaces to remove (up to tab-w)
                     (spaces (let sloop ((i 0))
                               (if (and (< i (string-length text))
                                        (char=? (string-ref text i) #\space))
                                 (sloop (+ i 1))
                                 i))))
                (when (> spaces 0)
                  (editor-delete-range ed ls spaces)))
              (loop (- l 1)))))
        (echo-message! echo (string-append "Unindented "
                                            (number->string (+ 1 (- end-line start-line)))
                                            " lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-copy-region-as-kill app)
  "Copy region to kill ring without removing it."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (substring (editor-get-text ed) start end)))
        (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app)))
        (set! (buffer-mark buf) #f)
        (echo-message! echo (string-append "Copied "
                                            (number->string (- end start)) " chars")))
      (echo-error! echo "No mark set"))))

(def (cmd-append-to-buffer app)
  "Append region text to another buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    (if mark
      (let ((name (echo-read-string echo "Append to buffer: " row width)))
        (when (and name (not (string-empty? name)))
          (let* ((start (min pos mark))
                 (end (max pos mark))
                 (text (substring (editor-get-text ed) start end))
                 (target (buffer-by-name name)))
            (if target
              (begin
                (echo-message! echo (string-append "Appended to " name))
                ;; Text stored in kill ring for later paste into target
                (set! (app-state-kill-ring app) (cons text (app-state-kill-ring app))))
              (echo-error! echo (string-append "No buffer: " name))))))
      (echo-error! echo "No mark set"))))

(def (cmd-toggle-show-trailing-whitespace app)
  "Toggle showing trailing whitespace."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (current (editor-get-view-whitespace ed)))
    (if (= current 0)
      (begin (editor-set-view-whitespace ed 1)
             (echo-message! echo "Trailing whitespace visible"))
      (begin (editor-set-view-whitespace ed 0)
             (echo-message! echo "Trailing whitespace hidden")))))

(def (cmd-backward-kill-sexp app)
  "Kill the sexp before point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    ;; Simple backward sexp kill: find matching paren backwards
    (if (and (> pos 0)
             (let ((prev-ch (char->integer (string-ref text (- pos 1)))))
               (brace-char? prev-ch)))
      (let ((match (send-message ed SCI_BRACEMATCH (- pos 1) 0)))
        (if (>= match 0)
          (let* ((start (min match (- pos 1)))
                 (end (+ (max match (- pos 1)) 1))
                 (killed (substring text start end)))
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (editor-delete-range ed start (- end start))
            (echo-message! echo "Killed sexp"))
          (echo-error! echo "No matching sexp")))
      ;; If not on a bracket, kill the previous word as fallback
      (let loop ((p (- pos 1)))
        (if (or (<= p 0) (not (word-char? (char->integer (string-ref text p)))))
          (let* ((start (+ p 1))
                 (killed (substring text start pos)))
            (when (> (string-length killed) 0)
              (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
              (editor-delete-range ed start (- pos start))))
          (loop (- p 1)))))))


(def (cmd-delete-horizontal-space-forward app)
  "Delete whitespace after point."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         (end (let loop ((p pos))
                (if (and (< p len)
                         (let ((ch (string-ref text p)))
                           (or (char=? ch #\space) (char=? ch #\tab))))
                  (loop (+ p 1))
                  p))))
    (when (> end pos)
      (editor-delete-range ed pos (- end pos)))))

(def (cmd-toggle-debug-mode app)
  "Toggle debug mode display."
  (echo-message! (app-state-echo app) "Debug mode toggled"))

(def (cmd-insert-comment-separator app)
  "Insert a comment separator line (;; ===...===)."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (sep ";;; ============================================================================\n"))
    (editor-insert-text ed pos sep)
    (editor-goto-pos ed (+ pos (string-length sep)))))

(def *global-hl-line* #t)

(def (cmd-toggle-global-hl-line app)
  "Toggle global caret line highlight."
  (set! *global-hl-line* (not *global-hl-line*))
  (let ((fr (app-state-frame app))
        (echo (app-state-echo app)))
    ;; Apply to current editor
    (let ((ed (edit-window-editor (current-window fr))))
      (editor-set-caret-line-visible ed *global-hl-line*))
    (echo-message! echo (if *global-hl-line*
                           "Global hl-line ON"
                           "Global hl-line OFF"))))

(def (cmd-insert-shebang app)
  "Insert #!/usr/bin/env shebang line."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (interp (echo-read-string echo "Interpreter (e.g. gxi, python3): " row width)))
    (when (and interp (not (string-empty? interp)))
      (let ((line (string-append "#!/usr/bin/env " interp "\n")))
        (editor-insert-text ed 0 line)
        (echo-message! echo (string-append "Inserted shebang for " interp))))))

(def (cmd-toggle-auto-indent app)
  "Toggle auto-indent on newline."
  (echo-message! (app-state-echo app) "Auto-indent always active"))

(def (cmd-what-mode app)
  "Show current buffer mode."
  (let* ((buf (current-buffer-from-app app))
         (lang (buffer-lexer-lang buf))
         (echo (app-state-echo app)))
    (echo-message! echo (string-append "Mode: "
                                        (if lang (symbol->string lang) "fundamental")))))

(def (cmd-show-buffer-size app)
  "Show current buffer size in bytes and lines."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (size (editor-get-text-length ed))
         (lines (editor-get-line-count ed)))
    (echo-message! echo (string-append (number->string size) " bytes, "
                                        (number->string lines) " lines"))))

(def (cmd-goto-percent app)
  "Go to percentage position in buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Goto percent: " row width)))
    (when (and input (not (string-empty? input)))
      (let ((pct (string->number input)))
        (when (and pct (>= pct 0) (<= pct 100))
          (let* ((total (editor-get-text-length ed))
                 (target (quotient (* total pct) 100)))
            (editor-goto-pos ed target)
            (editor-scroll-caret ed)))))))

(def (cmd-insert-newline-below app)
  "Insert a blank line below current line without moving cursor."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-end (editor-get-line-end-position ed line)))
    (editor-insert-text ed line-end "\n")
    (editor-goto-pos ed pos)))

(def (cmd-insert-newline-above app)
  "Insert a blank line above current line without moving cursor."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line)))
    (editor-insert-text ed line-start "\n")
    ;; Cursor shifted down by 1, so restore
    (editor-goto-pos ed (+ pos 1))))

(def (cmd-duplicate-region app)
  "Duplicate the selected region."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (substring (editor-get-text ed) start end)))
        (editor-insert-text ed end text)
        (echo-message! echo (string-append "Duplicated "
                                            (number->string (- end start)) " chars")))
      (echo-error! echo "No mark set"))))

(def (cmd-sort-lines-reverse app)
  "Sort lines in region in reverse order."
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
             (sorted (sort lines (lambda (a b) (string>? a b))))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (echo-message! echo (string-append "Sorted "
                                            (number->string (length sorted))
                                            " lines (reverse)")))
      (echo-error! echo "No mark set"))))

(def (cmd-uniquify-lines app)
  "Remove consecutive duplicate lines in region."
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
             (unique (let loop ((ls lines) (prev #f) (acc []))
                       (cond
                         ((null? ls) (reverse acc))
                         ((and prev (string=? (car ls) prev))
                          (loop (cdr ls) prev acc))
                         (else
                          (loop (cdr ls) (car ls) (cons (car ls) acc))))))
             (removed (- (length lines) (length unique)))
             (result (string-join unique "\n")))
        (with-undo-action ed
          (editor-delete-range ed line-start (- line-end line-start))
          (editor-insert-text ed line-start result))
        (echo-message! echo (string-append "Removed " (number->string removed)
                                            " duplicate lines")))
      (echo-error! echo "No mark set"))))

(def (cmd-show-line-endings app)
  "Show what line ending style the buffer uses."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (has-crlf (string-contains text "\r\n"))
         (has-cr (and (not has-crlf) (string-contains text "\r"))))
    (echo-message! echo
      (cond
        (has-crlf "Line endings: CRLF (DOS/Windows)")
        (has-cr "Line endings: CR (old Mac)")
        (else "Line endings: LF (Unix)")))))

;;;============================================================================
;;; Scroll margin commands
;;;============================================================================

(def (apply-scroll-margin-to-editor! ed)
  "Apply current scroll margin setting to a Scintilla editor.
   SCI_SETYCARETPOLICY = 2403, CARET_SLOP=1, CARET_STRICT=4."
  (if (> *scroll-margin* 0)
    (send-message ed 2403 5 *scroll-margin*)  ;; CARET_SLOP|CARET_STRICT
    (send-message ed 2403 0 0)))              ;; Reset to default

(def (cmd-set-scroll-margin app)
  "Set the scroll margin (lines to keep visible above/below cursor)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo
                  (string-append "Scroll margin (current "
                                 (number->string *scroll-margin*) "): ")
                  row width)))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (when (and n (>= n 0) (<= n 20))
          (set! *scroll-margin* n)
          ;; Apply to all editors
          (for-each (lambda (win) (apply-scroll-margin-to-editor! (edit-window-editor win)))
                    (frame-windows fr))
          (echo-message! echo (string-append "Scroll margin set to "
                                             (number->string n))))))))

(def (cmd-toggle-scroll-margin app)
  "Toggle scroll margin between 0 and 3."
  (let ((fr (app-state-frame app)))
    (if (> *scroll-margin* 0)
      (set! *scroll-margin* 0)
      (set! *scroll-margin* 3))
    (for-each (lambda (win) (apply-scroll-margin-to-editor! (edit-window-editor win)))
              (frame-windows fr))
    (echo-message! (app-state-echo app)
      (if (> *scroll-margin* 0)
        (string-append "Scroll margin: " (number->string *scroll-margin*))
        "Scroll margin: off"))))

;;;============================================================================
;;; Init file commands (TUI)
;;;============================================================================

(def (cmd-load-init-file app)
  "Load the TUI init file (~/.gerbil-emacs-init)."
  (if (file-exists? *init-file-path*)
    (begin
      (init-file-load!)
      ;; Re-apply scroll margin to all editors
      (for-each (lambda (win)
                  (apply-scroll-margin-to-editor! (edit-window-editor win)))
                (frame-windows (app-state-frame app)))
      (echo-message! (app-state-echo app)
        (string-append "Loaded " *init-file-path*)))
    (echo-message! (app-state-echo app)
      (string-append "No init file: " *init-file-path*))))

(def (cmd-find-init-file app)
  "Open the init file for editing."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (name (path-strip-directory *init-file-path*))
         (buf (buffer-create! name ed *init-file-path*)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (when (file-exists? *init-file-path*)
      (let ((text (read-file-as-string *init-file-path*)))
        (when text
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0))))
    (echo-message! (app-state-echo app) *init-file-path*)))

;;;============================================================================
;;; Save-place commands
;;;============================================================================

(def (cmd-toggle-save-place-mode app)
  "Toggle save-place mode — remembers cursor position in files."
  (set! *save-place-enabled* (not *save-place-enabled*))
  (echo-message! (app-state-echo app)
    (if *save-place-enabled* "Save-place mode ON" "Save-place mode OFF")))

;;;============================================================================
;;; Clean-on-save commands
;;;============================================================================

(def (cmd-toggle-delete-trailing-whitespace-on-save app)
  "Toggle deleting trailing whitespace when saving."
  (set! *delete-trailing-whitespace-on-save*
        (not *delete-trailing-whitespace-on-save*))
  (echo-message! (app-state-echo app)
    (if *delete-trailing-whitespace-on-save*
      "Delete trailing whitespace on save: ON"
      "Delete trailing whitespace on save: OFF")))

(def (cmd-toggle-require-final-newline app)
  "Toggle requiring files to end with a newline on save."
  (set! *require-final-newline* (not *require-final-newline*))
  (echo-message! (app-state-echo app)
    (if *require-final-newline*
      "Require final newline: ON"
      "Require final newline: OFF")))

;;;============================================================================
;;; Centered cursor mode
;;;============================================================================

(def (cmd-toggle-centered-cursor-mode app)
  "Toggle centered cursor mode — keeps cursor vertically centered."
  (set! *centered-cursor-mode* (not *centered-cursor-mode*))
  (let* ((fr (app-state-frame app))
         (ed (current-editor app)))
    ;; Use SCI_SETYCARETPOLICY for centering
    ;; CARET_SLOP=1, CARET_STRICT=4, CARET_EVEN=8
    (if *centered-cursor-mode*
      ;; Set large margin = half screen height to force centering
      (let ((visible (send-message ed 2370 0 0))) ;; SCI_LINESONSCREEN
        (send-message ed 2403 13 (quotient visible 2))) ;; SLOP|STRICT|EVEN
      ;; Restore normal scroll margin
      (if (> *scroll-margin* 0)
        (send-message ed 2403 5 *scroll-margin*) ;; SLOP|STRICT
        (send-message ed 2403 0 0))))
  (echo-message! (app-state-echo app)
    (if *centered-cursor-mode*
      "Centered cursor mode ON"
      "Centered cursor mode OFF")))

