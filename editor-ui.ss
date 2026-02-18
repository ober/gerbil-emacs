;;; -*- Gerbil -*-
;;; UI commands: goto-line, M-x, help, buffer list, query replace,
;;; indent, toggles, dired, REPL, yank-pop, occur, compile

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
        :gemacs/core
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/highlight
        :gemacs/editor-core
        (only-in :gemacs/org-table
                 org-table-on-table-line? org-table-next-cell))

;;;============================================================================
;;; Goto line
;;;============================================================================

(def (cmd-goto-line app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Goto line: " row width)))
    (when (and input (> (string-length input) 0))
      (let ((line-num (string->number input)))
        (if (and line-num (> line-num 0))
          (let* ((ed (current-editor app))
                 (target (- line-num 1)))  ; 0-based
            (editor-goto-line ed target)
            (editor-scroll-caret ed)
            (pulse-line! ed target)
            (echo-message! echo (string-append "Line " input)))
          (echo-error! echo "Invalid line number"))))))

;;;============================================================================
;;; M-x (execute extended command)
;;;============================================================================

(def (cmd-execute-extended-command app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd-names (sort (map symbol->string (hash-keys *all-commands*))
                          string<?))
         (input (echo-read-string-with-completion echo "M-x " cmd-names row width)))
    (when (and input (> (string-length input) 0))
      (let ((cmd-name (string->symbol input)))
        (execute-command! app cmd-name)))))

;;;============================================================================
;;; Help commands
;;;============================================================================

(def (cmd-describe-key app)
  "Prompt for a key, display its binding."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr)))
    ;; Draw prompt
    (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
    (tui-print! 0 row #xd8d8d8 #x181818 "Describe key: ")
    (tui-present!)
    ;; Wait for a key event
    (let ((ev (tui-poll-event)))
      (when (and ev (tui-event-key? ev))
        (let* ((key-str (key-event->string ev))
               (binding (keymap-lookup *global-keymap* key-str)))
          (cond
            ((hash-table? binding)
             (echo-message! echo (string-append key-str " is a prefix key")))
            ((symbol? binding)
             (echo-message! echo
               (string-append key-str " runs " (symbol->string binding))))
            (else
             (echo-message! echo
               (string-append key-str " is not bound")))))))))

(def (cmd-describe-command app)
  "Prompt for a command name, show if it exists."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Describe command: " row width)))
    (when (and input (> (string-length input) 0))
      (let ((cmd (find-command (string->symbol input))))
        (if cmd
          (echo-message! echo (string-append input " is a command"))
          (echo-error! echo (string-append input " is not a command")))))))

(def (cmd-list-bindings app)
  "Display all keybindings in a *Help* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (lines '()))
    ;; Collect global keymap bindings
    (for-each
      (lambda (entry)
        (let ((key (car entry))
              (val (cdr entry)))
          (cond
            ((symbol? val)
             (set! lines (cons (string-append "  " key "\t" (symbol->string val))
                               lines)))
            ((hash-table? val)
             ;; Prefix map: list its sub-bindings
             (for-each
               (lambda (sub-entry)
                 (let ((sub-key (car sub-entry))
                       (sub-val (cdr sub-entry)))
                   (when (symbol? sub-val)
                     (set! lines
                       (cons (string-append "  " key " " sub-key "\t"
                                            (symbol->string sub-val))
                             lines)))))
               (keymap-entries val))))))
      (keymap-entries *global-keymap*))
    ;; Sort and format
    (let* ((sorted (sort lines string<?))
           (text (string-append "Key Bindings:\n\n"
                                (string-join sorted "\n")
                                "\n")))
      ;; Create or reuse *Help* buffer
      (let ((buf (or (buffer-by-name "*Help*")
                     (buffer-create! "*Help*" ed #f))))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (editor-set-text ed text)
        (editor-set-save-point ed)
        (editor-goto-pos ed 0)
        (echo-message! (app-state-echo app) "*Help*")))))

;;;============================================================================
;;; Buffer list
;;;============================================================================

(def (cmd-list-buffers app)
  "Display all buffers in a *Buffer List* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (bufs (buffer-list))
         (header "  Buffer\t\tFile\n  ------\t\t----\n")
         (lines (map (lambda (buf)
                       (let ((name (buffer-name buf))
                             (path (or (buffer-file-path buf) "")))
                         (string-append "  " name "\t\t" path)))
                     bufs))
         (text (string-append header (string-join lines "\n") "\n")))
    (let ((buf (or (buffer-by-name "*Buffer List*")
                   (buffer-create! "*Buffer List*" ed #f))))
      (set! (buffer-lexer-lang buf) 'buffer-list)
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t)
      ;; Brighter caret line for buffer-list row selection
      (editor-set-caret-line-background ed #x2a2a4a)
      (echo-message! (app-state-echo app) "*Buffer List*"))))

;;;============================================================================
;;; Query replace
;;;============================================================================

(def (cmd-query-replace app)
  "Interactive search and replace (M-%)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (from-str (echo-read-string echo "Query replace: " row width)))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (echo-read-string echo
                      (string-append "Replace \"" from-str "\" with: ")
                      row width)))
        (when to-str
          (let ((ed (current-editor app)))
            (query-replace-loop! app ed from-str to-str)))))))

(def (query-replace-loop! app ed from-str to-str)
  "Drive the query-replace interaction."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (from-len (string-length from-str))
         (to-len (string-length to-str))
         (replaced 0))
    ;; Start searching from beginning
    (editor-goto-pos ed 0)
    (let loop ()
      (let ((text-len (editor-get-text-length ed))
            (pos (editor-get-current-pos ed)))
        ;; Search forward
        (send-message ed SCI_SETTARGETSTART pos)
        (send-message ed SCI_SETTARGETEND text-len)
        (send-message ed SCI_SETSEARCHFLAGS 0)
        (let ((found (send-message/string ed SCI_SEARCHINTARGET from-str)))
          (if (< found 0)
            ;; No more matches
            (echo-message! echo
              (string-append "Replaced " (number->string replaced) " occurrences"))
            ;; Found a match, highlight it
            (begin
              (editor-set-selection ed found (+ found from-len))
              (editor-scroll-caret ed)
              ;; Redraw so user can see the match
              (frame-refresh! fr)
              (position-cursor-for-replace! app)
              ;; Prompt: y/n/!/q
              (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
              (tui-print! 0 row #xd8d8d8 #x181818
                "Replace? (y)es (n)o (!)all (q)uit")
              (tui-present!)
              (let ((ev (tui-poll-event)))
                (when (and ev (tui-event-key? ev))
                  (let ((ch (tui-event-ch ev)))
                    (cond
                      ;; Yes: replace and continue
                      ((= ch (char->integer #\y))
                       (send-message ed SCI_SETTARGETSTART found)
                       (send-message ed SCI_SETTARGETEND (+ found from-len))
                       (send-message/string ed SCI_REPLACETARGET to-str)
                       (editor-goto-pos ed (+ found to-len))
                       (set! replaced (+ replaced 1))
                       (loop))
                      ;; No: skip
                      ((= ch (char->integer #\n))
                       (editor-goto-pos ed (+ found from-len))
                       (loop))
                      ;; All: replace all remaining
                      ((= ch (char->integer #\!))
                       (let all-loop ()
                         (let ((text-len2 (editor-get-text-length ed))
                               (pos2 (editor-get-current-pos ed)))
                           (send-message ed SCI_SETTARGETSTART pos2)
                           (send-message ed SCI_SETTARGETEND text-len2)
                           (let ((found2 (send-message/string ed SCI_SEARCHINTARGET from-str)))
                             (when (>= found2 0)
                               (send-message ed SCI_SETTARGETSTART found2)
                               (send-message ed SCI_SETTARGETEND (+ found2 from-len))
                               (send-message/string ed SCI_REPLACETARGET to-str)
                               (editor-goto-pos ed (+ found2 to-len))
                               (set! replaced (+ replaced 1))
                               (all-loop)))))
                       (echo-message! echo
                         (string-append "Replaced " (number->string replaced) " occurrences")))
                      ;; Quit
                      ((= ch (char->integer #\q))
                       (echo-message! echo
                         (string-append "Replaced " (number->string replaced) " occurrences")))
                      ;; Unknown key: skip
                      (else (loop)))))))))))))

(def (position-cursor-for-replace! app)
  "Helper to show cursor during query-replace."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (screen-x (send-message ed SCI_POINTXFROMPOSITION 0 pos))
         (screen-y (send-message ed SCI_POINTYFROMPOSITION 0 pos))
         (win-x (edit-window-x win))
         (win-y (edit-window-y win)))
    (tui-set-cursor! (+ win-x screen-x) (+ win-y screen-y))
    (tui-present!)))

;;;============================================================================
;;; Tab / indent
;;;============================================================================

(def (org-buffer? buf)
  "Check if buffer is an org-mode file."
  (or (eq? (buffer-lexer-lang buf) 'org)
      (let ((path (buffer-file-path buf)))
        (and (string? path) (string-suffix? ".org" path)))
      (let ((name (buffer-name buf)))
        (and (string? name)
             (> (string-length name) 4)
             (string-suffix? ".org" name)))))

(def (cmd-indent-or-complete app)
  "Insert appropriate indentation. In org buffers, dispatch to org-cycle
   or org-template-expand as appropriate."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app)))
    (cond
      ((dired-buffer? buf) (void))
      ((repl-buffer? buf)
       ;; In REPL, insert 2 spaces
       (let ((pos (editor-get-current-pos ed))
             (rs (hash-get *repl-state* buf)))
         (when (and rs (>= pos (repl-state-prompt-pos rs)))
           (editor-insert-text ed pos "  "))))
      ((org-buffer? buf)
       ;; In org-mode: check table, template, heading, or indent
       (cond
         ;; On a table line: align table and move to next cell
         ((org-table-on-table-line? ed)
          (org-table-next-cell ed))
         (else
          (let* ((text (editor-get-text ed))
                 (text-len (string-length text))
                 (pos (min (editor-get-current-pos ed) text-len))
                 ;; Find line boundaries from text (avoids byte/char position mismatch)
                 (line-start (let loop ((i (- pos 1)))
                               (cond ((< i 0) 0)
                                     ((char=? (string-ref text i) #\newline) (+ i 1))
                                     (else (loop (- i 1))))))
                 (line-end (let loop ((i pos))
                             (cond ((>= i text-len) text-len)
                                   ((char=? (string-ref text i) #\newline) i)
                                   (else (loop (+ i 1))))))
                 (line (substring text line-start line-end))
                 (trimmed (string-trim line)))
            (cond
              ;; <s TAB, <e TAB, etc. - template expansion
              ((and (>= (string-length trimmed) 2)
                    (char=? (string-ref trimmed 0) #\<)
                    (let ((key (string-ref trimmed 1)))
                      (memv key '(#\s #\e #\q #\v #\c #\C #\l #\h #\a))))
               (execute-command! app 'org-template-expand))
              ;; On a heading line - org-cycle fold/unfold
              ((and (> (string-length trimmed) 0)
                    (char=? (string-ref trimmed 0) #\*))
               (execute-command! app 'org-cycle))
              ;; Otherwise - regular indent
              (else
               (editor-insert-text ed pos "  ")))))))
      (else
       ;; Insert 2-space indent (Scheme convention)
       (let ((pos (editor-get-current-pos ed)))
         (editor-insert-text ed pos "  "))))))

;;;============================================================================
;;; Beginning/end of defun
;;;============================================================================

(def (cmd-beginning-of-defun app)
  "Move to the beginning of the current/previous top-level form."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Search backward for '(' at column 0
    (let loop ((i (- pos 1)))
      (cond
        ((< i 0)
         (editor-goto-pos ed 0)
         (echo-message! (app-state-echo app) "Beginning of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (editor-goto-pos ed i)
         (editor-scroll-caret ed))
        (else (loop (- i 1)))))))

(def (cmd-end-of-defun app)
  "Move to the end of the current top-level form."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; First find the start of the current/next defun
    (let find-start ((i pos))
      (cond
        ((>= i len)
         (editor-goto-pos ed len)
         (echo-message! (app-state-echo app) "End of buffer"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         ;; Found start of defun, now find matching close paren
         (let match ((j (+ i 1)) (depth 1))
           (cond
             ((>= j len) (editor-goto-pos ed len))
             ((= depth 0)
              (editor-goto-pos ed j)
              (editor-scroll-caret ed))
             ((char=? (string-ref text j) #\() (match (+ j 1) (+ depth 1)))
             ((char=? (string-ref text j) #\)) (match (+ j 1) (- depth 1)))
             (else (match (+ j 1) depth)))))
        (else (find-start (+ i 1)))))))

;;;============================================================================
;;; Toggle line numbers
;;;============================================================================

(def (cmd-toggle-line-numbers app)
  "Toggle line number margin on/off."
  (let ((ed (current-editor app)))
    (let ((cur-width (send-message ed SCI_GETMARGINWIDTHN 0 0)))
      (if (> cur-width 0)
        (begin
          (send-message ed SCI_SETMARGINWIDTHN 0 0)
          (echo-message! (app-state-echo app) "Line numbers off"))
        (begin
          ;; Set margin 0 to line numbers type
          (send-message ed SCI_SETMARGINTYPEN 0 SC_MARGIN_NUMBER)
          ;; Width of ~4 chars for line numbers
          (send-message ed SCI_SETMARGINWIDTHN 0 4)
          (echo-message! (app-state-echo app) "Line numbers on"))))))

;;;============================================================================
;;; Toggle word wrap
;;;============================================================================

(def (cmd-toggle-word-wrap app)
  "Toggle word wrap on/off."
  (let ((ed (current-editor app)))
    (let ((cur (editor-get-wrap-mode ed)))
      (if (= cur SC_WRAP_NONE)
        (begin
          (editor-set-wrap-mode ed SC_WRAP_WORD)
          (echo-message! (app-state-echo app) "Word wrap on"))
        (begin
          (editor-set-wrap-mode ed SC_WRAP_NONE)
          (echo-message! (app-state-echo app) "Word wrap off"))))))

;;;============================================================================
;;; Toggle whitespace visibility
;;;============================================================================

(def (cmd-toggle-whitespace app)
  "Toggle whitespace visibility."
  (let ((ed (current-editor app)))
    (let ((cur (editor-get-view-whitespace ed)))
      (if (= cur SCWS_INVISIBLE)
        (begin
          (editor-set-view-whitespace ed SCWS_VISIBLEALWAYS)
          (echo-message! (app-state-echo app) "Whitespace visible"))
        (begin
          (editor-set-view-whitespace ed SCWS_INVISIBLE)
          (echo-message! (app-state-echo app) "Whitespace hidden"))))))

;;;============================================================================
;;; Zoom
;;;============================================================================

(def (cmd-zoom-in app)
  (let ((ed (current-editor app)))
    (editor-zoom-in ed)
    (echo-message! (app-state-echo app)
                   (string-append "Zoom: " (number->string (editor-get-zoom ed))))))

(def (cmd-zoom-out app)
  (let ((ed (current-editor app)))
    (editor-zoom-out ed)
    (echo-message! (app-state-echo app)
                   (string-append "Zoom: " (number->string (editor-get-zoom ed))))))

(def (cmd-zoom-reset app)
  (let ((ed (current-editor app)))
    (editor-set-zoom ed 0)
    (echo-message! (app-state-echo app) "Zoom reset")))

;;;============================================================================
;;; Select all
;;;============================================================================

(def (cmd-select-all app)
  (let ((ed (current-editor app)))
    (editor-select-all ed)
    (echo-message! (app-state-echo app) "Mark set (whole buffer)")))

;;;============================================================================
;;; Duplicate line
;;;============================================================================

(def (cmd-duplicate-line app)
  "Duplicate the current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (line-text (editor-get-line ed line)))
    ;; Insert a copy after the current line
    (editor-goto-pos ed line-end)
    (editor-insert-text ed line-end (string-append "\n" (string-trim-right line-text #\newline)))))

;;;============================================================================
;;; Comment toggle (Scheme: ;; prefix)
;;;============================================================================

(def (cmd-toggle-comment app)
  "Toggle ;; comment on the current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (line-text (editor-get-line ed line))
         (trimmed (string-trim line-text)))
    (cond
      ;; Line starts with ";; " — remove it
      ((and (>= (string-length trimmed) 3)
            (string=? (substring trimmed 0 3) ";; "))
       ;; Find position of ";; " in the original line
       (let ((comment-pos (string-contains line-text ";; ")))
         (when comment-pos
           (editor-delete-range ed (+ line-start comment-pos) 3))))
      ;; Line starts with ";;" — remove it
      ((and (>= (string-length trimmed) 2)
            (string=? (substring trimmed 0 2) ";;"))
       (let ((comment-pos (string-contains line-text ";;")))
         (when comment-pos
           (editor-delete-range ed (+ line-start comment-pos) 2))))
      ;; Add ";; " at start of line
      (else
       (editor-insert-text ed line-start ";; ")))))

;;;============================================================================
;;; Transpose chars (C-t)
;;;============================================================================

(def (cmd-transpose-chars app)
  "Swap the two characters before point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (when (>= pos 2)
      (let* ((text (editor-get-text ed))
             (c1 (string-ref text (- pos 2)))
             (c2 (string-ref text (- pos 1))))
        (with-undo-action ed
          (editor-delete-range ed (- pos 2) 2)
          (editor-insert-text ed (- pos 2)
            (string c2 c1)))
        (editor-goto-pos ed pos)))))

;;;============================================================================
;;; Word case commands
;;;============================================================================

(def (word-char? ch)
  (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-)))

(def (word-at-point ed)
  "Get the word boundaries at/after current position.
   Returns (values start end) or (values #f #f) if no word."
  (let* ((pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Skip non-word chars
    (let skip ((i pos))
      (if (>= i len)
        (values #f #f)
        (let ((ch (string-ref text i)))
          (if (word-char? ch)
            ;; Found start of word, find end
            (let find-end ((j (+ i 1)))
              (if (>= j len)
                (values i j)
                (if (word-char? (string-ref text j))
                  (find-end (+ j 1))
                  (values i j))))
            (skip (+ i 1))))))))

(def (cmd-upcase-word app)
  "Convert the next word to uppercase."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when start
        (let* ((text (editor-get-text ed))
               (word (substring text start end))
               (upper (string-upcase word)))
          (with-undo-action ed
            (editor-delete-range ed start (- end start))
            (editor-insert-text ed start upper))
          (editor-goto-pos ed end))))))

(def (cmd-downcase-word app)
  "Convert the next word to lowercase."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when start
        (let* ((text (editor-get-text ed))
               (word (substring text start end))
               (lower (string-downcase word)))
          (with-undo-action ed
            (editor-delete-range ed start (- end start))
            (editor-insert-text ed start lower))
          (editor-goto-pos ed end))))))

(def (cmd-capitalize-word app)
  "Capitalize the next word."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when (and start (< start end))
        (let* ((text (editor-get-text ed))
               (word (substring text start end))
               (cap (string-append
                      (string-upcase (substring word 0 1))
                      (string-downcase (substring word 1 (string-length word))))))
          (with-undo-action ed
            (editor-delete-range ed start (- end start))
            (editor-insert-text ed start cap))
          (editor-goto-pos ed end))))))

;;;============================================================================
;;; Kill word (M-d)
;;;============================================================================

(def (cmd-kill-word app)
  "Kill from point to end of word."
  (let ((ed (current-editor app)))
    (let-values (((start end) (word-at-point ed)))
      (when start
        (let* ((pos (editor-get-current-pos ed))
               (kill-start (min pos start))
               (text (editor-get-text ed))
               (killed (substring text kill-start end)))
          ;; Add to kill ring
          (set! (app-state-kill-ring app)
                (cons killed (app-state-kill-ring app)))
          (editor-delete-range ed kill-start (- end kill-start)))))))

;;;============================================================================
;;; What line (M-g l)
;;;============================================================================

(def (cmd-what-line app)
  "Display current line number in echo area."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (col (+ 1 (editor-get-column ed pos)))
         (total (editor-get-line-count ed)))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string line)
                     " of " (number->string total)
                     ", Column " (number->string col)))))

;;;============================================================================
;;; Delete trailing whitespace
;;;============================================================================

(def (cmd-delete-trailing-whitespace app)
  "Remove trailing whitespace from all lines."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (lines (string-split text #\newline))
         (cleaned (map (lambda (line) (string-trim-right line char-whitespace?))
                       lines))
         (new-text (string-join cleaned "\n")))
    (when (not (string=? text new-text))
      (editor-set-text ed new-text)
      (editor-goto-pos ed (min pos (editor-get-text-length ed)))
      (echo-message! (app-state-echo app) "Trailing whitespace deleted"))))

;;;============================================================================
;;; Count words/lines
;;;============================================================================

(def (cmd-count-words app)
  "Display word, line, and character counts for the buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (chars (string-length text))
         (lines (editor-get-line-count ed))
         ;; Simple word count: count transitions from non-word to word chars
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i chars) count
                    (let ((ch (string-ref text i)))
                      (if (or (char-alphabetic? ch) (char-numeric? ch))
                        (loop (+ i 1) #t (if in-word count (+ count 1)))
                        (loop (+ i 1) #f count)))))))
    (echo-message! (app-state-echo app)
      (string-append "Lines: " (number->string lines)
                     "  Words: " (number->string words)
                     "  Chars: " (number->string chars)))))

;;;============================================================================
;;; Misc commands
;;;============================================================================

(def (cmd-keyboard-quit app)
  (echo-message! (app-state-echo app) "Quit")
  (set! (app-state-key-state app) (make-initial-key-state))
  ;; Deactivate mark and clear visual selection (Emacs C-g behavior)
  (let* ((buf (current-buffer-from-app app))
         (ed (current-editor app)))
    (when (buffer-mark buf)
      (set! (buffer-mark buf) #f)
      (let ((pos (editor-get-current-pos ed)))
        (editor-set-selection ed pos pos)))))

(def (cmd-quit app)
  "Quit the editor, prompting if there are unsaved file buffers."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         ;; Check for unsaved file buffers
         (unsaved (filter
                    (lambda (win)
                      (let ((buf (edit-window-buffer win))
                            (ed (edit-window-editor win)))
                        (and (buffer-file-path buf)
                             (editor-get-modify? ed))))
                    (frame-windows fr))))
    (if (null? unsaved)
      ;; No unsaved buffers — state saving handled by app.ss finally block
      (set! (app-state-running app) #f)
      ;; Prompt user
      (let* ((prompt (string-append
                       (number->string (length unsaved))
                       " modified buffer(s). Quit without saving? (y/n) "))
             (ans (echo-read-string echo prompt row width)))
        (when (and ans (> (string-length ans) 0)
                   (char=? (string-ref ans 0) #\y))
          (set! (app-state-running app) #f))))))

;;;============================================================================
;;; Dired (directory listing) support
;;;============================================================================

;; Moved to editor-core.ss

;; Moved to editor-core.ss

;;;============================================================================
;;; REPL commands
;;;============================================================================

;; repl-buffer-name moved to editor-core.ss
;; cmd-repl moved to editor-core.ss
;; cmd-repl-send moved to editor-core.ss

(def (cmd-eval-expression app)
  "Prompt for an expression in the echo area, eval it in-process."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Eval: " row width)))
    (when (and input (> (string-length input) 0))
      (let-values (((result error?) (eval-expression-string input)))
        (if error?
          (echo-error! echo result)
          (echo-message! echo result))))))

;;;============================================================================
;;; Yank-pop (M-y) — rotate through kill ring
;;;============================================================================

(def (cmd-yank-pop app)
  "Replace last yank with previous kill ring entry."
  (let ((kr (app-state-kill-ring app))
        (pos (app-state-last-yank-pos app))
        (len (app-state-last-yank-len app)))
    (if (or (null? kr) (not pos) (not len))
      (echo-error! (app-state-echo app) "No previous yank")
      (let* ((idx (modulo (+ (app-state-kill-ring-idx app) 1) (length kr)))
             (text (list-ref kr idx))
             (ed (current-editor app)))
        ;; Delete the previous yank
        (editor-delete-range ed pos len)
        ;; Insert the next kill ring entry
        (editor-insert-text ed pos text)
        (editor-goto-pos ed (+ pos (string-length text)))
        ;; Update tracking
        (set! (app-state-kill-ring-idx app) idx)
        (set! (app-state-last-yank-len app) (string-length text))))))

;;;============================================================================
;;; Occur mode (M-s o) — list matching lines
;;;============================================================================

(def (cmd-occur app)
  "List all lines matching a pattern in the current buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Occur: " row width)))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-editor app))
             (src-name (buffer-name (current-buffer-from-app app)))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matches '())
             (line-num 0))
        ;; Find matching lines
        (for-each
          (lambda (line)
            (set! line-num (+ line-num 1))
            (when (string-contains line pattern)
              (set! matches
                (cons (string-append
                        (number->string line-num) ":"
                        line)
                      matches))))
          lines)
        (let ((matches (reverse matches)))
          (if (null? matches)
            (echo-error! echo (string-append "No matches for: " pattern))
            ;; Display in *Occur* buffer
            (let* ((header (string-append (number->string (length matches))
                                          " matches for \"" pattern
                                          "\" in " src-name ":\n\n"))
                   (result-text (string-append header
                                               (string-join matches "\n")
                                               "\n"))
                   (buf (or (buffer-by-name "*Occur*")
                            (buffer-create! "*Occur*" ed #f))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-text ed result-text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (echo-message! echo
                (string-append (number->string (length matches))
                               " matches")))))))))

;;;============================================================================
;;; Compile mode (C-x c) — run build command
;;;============================================================================

(def (cmd-compile app)
  "Run a compile command and display output in *Compilation* buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (default (or (app-state-last-compile app) "make build"))
         (prompt (string-append "Compile command [" default "]: "))
         (input (echo-read-string echo prompt row width)))
    (when input
      (let* ((cmd (if (string=? input "") default input))
             (ed (current-editor app)))
        (set! (app-state-last-compile app) cmd)
        ;; Run the command and capture output
        (echo-message! echo (string-append "Running: " cmd))
        ;; Redraw to show message
        (frame-refresh! (app-state-frame app))
        (let* ((output (with-catch
                         (lambda (e)
                           (string-append "Error running command: "
                             (with-output-to-string
                               (lambda () (display-exception e)))))
                         (lambda ()
                           (let ((proc (open-process
                                         (list path: "/bin/sh"
                                               arguments: (list "-c" cmd)
                                               stdin-redirection: #f
                                               stdout-redirection: #t
                                               stderr-redirection: #t
                                               pseudo-terminal: #f))))
                             (let ((result (read-line proc #f)))
                               (let ((status (process-status proc)))
                                 (string-append
                                   (or result "")
                                   "\n\nProcess exited with status "
                                   (number->string status))))))))
               (text (string-append "-*- Compilation -*-\n"
                                    "Command: " cmd "\n"
                                    (make-string 60 #\-) "\n\n"
                                    output "\n"))
               (buf (or (buffer-by-name "*Compilation*")
                        (buffer-create! "*Compilation*" ed #f))))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer (current-window fr)) buf)
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0)
          (echo-message! echo "Compilation finished"))))))

;;;============================================================================
;;; Shell command on region (M-|)
;;;============================================================================

(def (cmd-shell-command-on-region app)
  "Pipe region through a shell command, display output."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Shell command on region: " row width)))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((ed (current-editor app))
             (buf (current-buffer-from-app app))
             (mark (buffer-mark buf)))
        (if (not mark)
          (echo-error! echo "No region (set mark first)")
          (let* ((pos (editor-get-current-pos ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (editor-get-text ed))
                 (region-text (substring text start end))
                 ;; Run command with region as stdin
                 (output (with-catch
                           (lambda (e)
                             (string-append "Error: "
                               (with-output-to-string
                                 (lambda () (display-exception e)))))
                           (lambda ()
                             (let ((proc (open-process
                                           (list path: "/bin/sh"
                                                 arguments: (list "-c" cmd)
                                                 stdin-redirection: #t
                                                 stdout-redirection: #t
                                                 stderr-redirection: #t
                                                 pseudo-terminal: #f))))
                               (display region-text proc)
                               (close-output-port proc)
                               (let ((result (read-line proc #f)))
                                 (process-status proc)
                                 (or result "")))))))
            ;; Display output in *Shell Output* buffer
            (let ((out-buf (or (buffer-by-name "*Shell Output*")
                               (buffer-create! "*Shell Output*" ed #f))))
              (buffer-attach! ed out-buf)
              (set! (edit-window-buffer (current-window fr)) out-buf)
              (editor-set-text ed output)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (set! (buffer-mark buf) #f)
              (echo-message! echo "Shell command done"))))))))

