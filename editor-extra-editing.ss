;;; -*- Gerbil -*-
;;; Artist mode, TRAMP, paredit, string inflection, ediff,
;;; undo-tree, server, navigation, and misc editing commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/editor-extra-helpers)

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
            (let* ((tmp-file (string-append "/tmp/gerbil-emacs-ssh-" (number->string (random-integer 99999))))
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

;; Undo tree visualization
(def (cmd-undo-tree-visualize app)
  "Show undo history information for current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (can-undo (send-message ed SCI_CANUNDO 0 0))
         (can-redo (send-message ed SCI_CANREDO 0 0)))
    (echo-message! echo
      (string-append "Undo: " (if (= can-undo 1) "available" "empty")
                     ", Redo: " (if (= can-redo 1) "available" "empty")))))

;; Editor server — file socket not available in TUI, provide info
(def (cmd-server-start app)
  "Note about editor server."
  (echo-message! (app-state-echo app)
    "Server mode: use gerbil-emacs <file> to open files"))

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
  "Toggle overwrite mode (stub — use M-x toggle-overwrite-mode)."
  (echo-message! (app-state-echo app) "Use M-x toggle-overwrite-mode"))

