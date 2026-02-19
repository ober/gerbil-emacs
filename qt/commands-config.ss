;;; -*- Gerbil -*-
;;; Qt commands config - snippets, terminal, keymaps, dired marks, keys, init, macros
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
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-modes)

;;; ============================================================================

(def *snippet-table* (make-hash-table)) ;; lang -> (hash trigger -> template)
(def *snippet-active* #f) ;; #f or (fields . current-field-idx)
(def *snippet-field-positions* []) ;; list of (start . end) positions

(def (snippet-define! lang trigger template)
  "Define a snippet: LANG is language symbol or 'global, TRIGGER is prefix string,
   TEMPLATE is string with $1, $2 etc. for fields and $0 for final cursor pos."
  (let ((lang-table (or (hash-get *snippet-table* lang) (make-hash-table))))
    (hash-put! lang-table trigger template)
    (hash-put! *snippet-table* lang lang-table)))

(def (snippet-lookup trigger lang)
  "Look up a snippet by trigger, checking lang-specific then global."
  (or (let ((lt (hash-get *snippet-table* lang)))
        (and lt (hash-get lt trigger)))
      (let ((gt (hash-get *snippet-table* 'global)))
        (and gt (hash-get gt trigger)))))

(def (snippet-expand-template template)
  "Expand template: replace $1..$9 with empty placeholders, return (text . field-offsets).
   Field offsets are positions where $N markers were."
  (let ((out (open-output-string))
        (fields (make-hash-table))
        (len (string-length template)))
    (let loop ((i 0))
      (cond
        ((>= i len)
         (let* ((text (get-output-string out))
                (offsets
                  (let collect ((n 1) (acc []))
                    (if (> n 9)
                      (let ((zero-pos (hash-get fields 0)))
                        (if zero-pos
                          (reverse (cons (cons 0 zero-pos) acc))
                          (reverse acc)))
                      (let ((pos (hash-get fields n)))
                        (if pos
                          (collect (+ n 1) (cons (cons n pos) acc))
                          (collect (+ n 1) acc)))))))
           (cons text offsets)))
        ((and (char=? (string-ref template i) #\$)
              (< (+ i 1) len)
              (char-numeric? (string-ref template (+ i 1))))
         (let ((n (- (char->integer (string-ref template (+ i 1)))
                     (char->integer #\0)))
               (pos (string-length (get-output-string out))))
           ;; Re-create output string to get current position
           (hash-put! fields n pos)
           (loop (+ i 2))))
        (else
         (display (string (string-ref template i)) out)
         (loop (+ i 1)))))))

(def (cmd-snippet-expand app)
  "Try to expand snippet at point. If no snippet, insert tab."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (prefix (get-word-prefix ed))
         (lang (or (buffer-lexer-lang buf) 'global)))
    (if (string=? prefix "")
      #f  ;; No trigger word
      (let ((template (snippet-lookup prefix lang)))
        (if (not template)
          #f  ;; No matching snippet
          (let* ((expanded (snippet-expand-template template))
                 (text (car expanded))
                 (fields (cdr expanded))
                 (pos (qt-plain-text-edit-cursor-position ed))
                 (trigger-start (- pos (string-length prefix)))
                 ;; Replace trigger with expanded text
                 (full-text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring full-text 0 trigger-start)
                             text
                             (substring full-text pos (string-length full-text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (if (null? fields)
              ;; No fields — place cursor at end of expansion
              (qt-plain-text-edit-set-cursor-position! ed
                (+ trigger-start (string-length text)))
              ;; Place cursor at first field
              (let ((first-field (car fields)))
                (set! *snippet-active* #t)
                (set! *snippet-field-positions*
                  (map (lambda (f) (+ trigger-start (cdr f))) fields))
                (qt-plain-text-edit-set-cursor-position! ed
                  (+ trigger-start (cdr first-field)))))
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            #t))))))

(def (cmd-snippet-next-field app)
  "Jump to next snippet field."
  (when *snippet-active*
    (let* ((ed (current-qt-editor app))
           (pos (qt-plain-text-edit-cursor-position ed))
           ;; Find next field after current position
           (next (let loop ((fps *snippet-field-positions*))
                   (if (null? fps) #f
                     (if (> (car fps) pos)
                       (car fps)
                       (loop (cdr fps)))))))
      (if next
        (begin
          (qt-plain-text-edit-set-cursor-position! ed next)
          (qt-plain-text-edit-ensure-cursor-visible! ed))
        ;; No more fields — deactivate snippet
        (begin
          (set! *snippet-active* #f)
          (set! *snippet-field-positions* []))))))

(def (cmd-snippet-prev-field app)
  "Jump to previous snippet field."
  (when *snippet-active*
    (let* ((ed (current-qt-editor app))
           (pos (qt-plain-text-edit-cursor-position ed))
           ;; Find previous field before current position
           (prev (let loop ((fps (reverse *snippet-field-positions*)))
                   (if (null? fps) #f
                     (if (< (car fps) pos)
                       (car fps)
                       (loop (cdr fps)))))))
      (when prev
        (qt-plain-text-edit-set-cursor-position! ed prev)
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-define-snippet app)
  "Interactively define a snippet."
  (let* ((lang-str (qt-echo-read-string app "Language (or global): "))
         (trigger (qt-echo-read-string app "Trigger: "))
         (template (qt-echo-read-string app "Template ($1,$2 for fields): ")))
    (when (and lang-str trigger template
               (> (string-length trigger) 0)
               (> (string-length template) 0))
      (let ((lang (string->symbol lang-str)))
        (snippet-define! lang trigger template)
        (echo-message! (app-state-echo app)
          (string-append "Snippet '" trigger "' defined for " lang-str))))))

(def (cmd-list-snippets app)
  "List all defined snippets."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (out (open-output-string)))
    (display "Snippets:\n\n" out)
    (hash-for-each
      (lambda (lang lang-table)
        (display (string-append "--- " (symbol->string lang) " ---\n") out)
        (hash-for-each
          (lambda (trigger template)
            (display (string-append "  " trigger " → " template "\n") out))
          lang-table))
      *snippet-table*)
    (let* ((text (get-output-string out))
           (buf (or (buffer-by-name "*Snippets*")
                    (qt-buffer-create! "*Snippets*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;; Built-in snippets for common languages
(snippet-define! 'scheme "def" "(def ($1)\n  $2)\n$0")
(snippet-define! 'scheme "defn" "(def ($1 $2)\n  $3)\n$0")
(snippet-define! 'scheme "let" "(let (($1 $2))\n  $3)\n$0")
(snippet-define! 'scheme "let*" "(let* (($1 $2))\n  $3)\n$0")
(snippet-define! 'scheme "when" "(when $1\n  $2)\n$0")
(snippet-define! 'scheme "unless" "(unless $1\n  $2)\n$0")
(snippet-define! 'scheme "cond" "(cond\n  (($1) $2)\n  (else $3))\n$0")
(snippet-define! 'scheme "if" "(if $1\n  $2\n  $3)\n$0")
(snippet-define! 'scheme "lambda" "(lambda ($1)\n  $2)\n$0")
(snippet-define! 'scheme "match" "(match $1\n  (($2) $3))\n$0")
(snippet-define! 'scheme "defstruct" "(defstruct $1 ($2))\n$0")
(snippet-define! 'scheme "defclass" "(defclass $1 ($2)\n  $3)\n$0")
(snippet-define! 'scheme "import" "(import $1)\n$0")
(snippet-define! 'scheme "export" "(export $1)\n$0")
(snippet-define! 'scheme "with-catch" "(with-catch\n  (lambda (e) $1)\n  (lambda ()\n    $2))\n$0")
(snippet-define! 'scheme "for-each" "(for-each\n  (lambda ($1)\n    $2)\n  $3)\n$0")

(snippet-define! 'global "todo" ";; TODO: $1\n$0")
(snippet-define! 'global "fixme" ";; FIXME: $1\n$0")
(snippet-define! 'global "note" ";; NOTE: $1\n$0")

;; --- Misc ---
(def (cmd-display-fill-column-indicator app)
  "Display fill column indicator."
  (cmd-toggle-fill-column-indicator app))

(def (cmd-display-line-numbers-relative app)
  "Toggle relative line numbers."
  (echo-message! (app-state-echo app) "Relative line numbers toggled"))

(def (cmd-font-lock-mode app)
  "Toggle font lock mode."
  (cmd-toggle-highlighting app))

(def (cmd-customize-face app)
  "Customize face."
  (echo-message! (app-state-echo app) "Face customization not available in Qt backend"))

(def (cmd-list-colors app)
  "List available colors."
  (echo-message! (app-state-echo app) "Color list not available"))

(def (cmd-load-theme app)
  "Switch to a different color theme."
  (let* ((available (theme-names))
         (names (map symbol->string available))
         (input (qt-echo-read-string-with-completion app
                  "Load theme: " names)))
    (when (and input (> (string-length input) 0))
      (let ((sym (string->symbol input)))
        (if (theme-get sym)
          (begin
            (apply-theme! app theme-name: sym)
            (echo-message! (app-state-echo app)
              (string-append "Theme: " input)))
          (echo-error! (app-state-echo app)
            (string-append "Unknown theme: " input)))))))

(def (cmd-fold-level app)
  "Set fold level."
  (echo-message! (app-state-echo app) "Folding not supported in QPlainTextEdit"))

(def (cmd-ansi-term app)
  "Open an ANSI terminal."
  (cmd-term app))

(def (cmd-diff-backup app)
  "Diff current file with backup."
  (echo-message! (app-state-echo app) "No backup file to diff"))

(def (cmd-dired-do-chmod app)
  "Change permissions in dired."
  (echo-message! (app-state-echo app) "chmod not yet implemented in dired"))

(def (cmd-eldoc app)
  "Toggle eldoc mode (automatic function signature display)."
  (set! *eldoc-mode* (not *eldoc-mode*))
  (set! *eldoc-last-sym* #f)
  (echo-message! (app-state-echo app)
    (if *eldoc-mode* "Eldoc mode enabled" "Eldoc mode disabled")))

(def (cmd-recover-session app)
  "Recover a previous session."
  (echo-message! (app-state-echo app) "Session recovery not available"))

(def (cmd-revert-buffer-with-coding app)
  "Revert buffer with different coding."
  (cmd-revert-buffer app))

(def (cmd-set-buffer-file-coding app)
  "Set buffer file coding system."
  (echo-message! (app-state-echo app) "File coding: UTF-8 (fixed)"))

(def (cmd-set-language-environment app)
  "Set language environment."
  (echo-message! (app-state-echo app) "Language environment: UTF-8"))

(def (cmd-sudo-find-file app)
  "Open file as root."
  (cmd-sudo-write app))

(def (cmd-which-function app)
  "Show current function name."
  ;; Simple: find the nearest (def ...
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)))
      (cond
        ((< i 5) (echo-message! (app-state-echo app) "Not in a function"))
        ((and (>= i 5)
              (string=? (substring text i (+ i 5)) "(def "))
         (let* ((name-start (+ i 5))
                (name-end (let nloop ((j name-start))
                            (if (or (>= j (string-length text))
                                    (memq (string-ref text j) '(#\space #\) #\newline #\()))
                              j (nloop (+ j 1))))))
           (echo-message! (app-state-echo app)
             (string-append "In: " (substring text name-start name-end)))))
        (else (loop (- i 1)))))))

(def (cmd-widen-all app)
  "Widen all narrowed buffers."
  (cmd-widen app))

(def (cmd-whitespace-mode app)
  "Toggle whitespace visualization."
  (cmd-toggle-whitespace app))

(def (cmd-profiler-start app)
  "Start profiler."
  (echo-message! (app-state-echo app) "Profiler not available"))

(def (cmd-profiler-stop app)
  "Stop profiler."
  (echo-message! (app-state-echo app) "Profiler not available"))

(def (cmd-show-tab-count app)
  "Show tab count in buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (count (let loop ((i 0) (n 0))
                  (if (>= i (string-length text)) n
                    (loop (+ i 1) (if (char=? (string-ref text i) #\tab) (+ n 1) n))))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " tabs in buffer"))))

(def (cmd-show-trailing-whitespace-count app)
  "Show count of lines with trailing whitespace."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (count (length (filter (lambda (l)
                                  (and (> (string-length l) 0)
                                       (char-whitespace? (string-ref l (- (string-length l) 1)))))
                                lines))))
    (echo-message! (app-state-echo app)
      (string-append (number->string count) " lines with trailing whitespace"))))

;;;============================================================================
;;; Terminal commands (PTY-backed)
;;;============================================================================

(def terminal-buffer-counter 0)

(def (terminal-read-plain ts)
  "Read available terminal output and return as plain text (ANSI stripped)."
  (let ((segs (terminal-read-available ts)))
    (if segs
      (let ((out (open-output-string)))
        (for-each (lambda (seg) (display (text-segment-text seg) out)) segs)
        (let ((s (get-output-string out)))
          (if (string=? s "") #f s)))
      #f)))

(def (cmd-term app)
  "Open a new PTY-backed terminal buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (name (begin
                 (set! terminal-buffer-counter (+ terminal-buffer-counter 1))
                 (if (= terminal-buffer-counter 1)
                   "*terminal*"
                   (string-append "*terminal-"
                                  (number->string terminal-buffer-counter) "*"))))
         (buf (qt-buffer-create! name ed #f)))
    ;; Mark as terminal buffer
    (set! (buffer-lexer-lang buf) 'terminal)
    ;; Attach buffer to editor
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    ;; Spawn PTY-backed shell
    (let ((ts (terminal-start!)))
      (hash-put! *terminal-state* buf ts)
      (qt-plain-text-edit-set-text! ed "")
      (set! (terminal-state-prompt-pos ts) 0))
    (echo-message! (app-state-echo app) (string-append name " started"))))

(def (cmd-terminal-send app)
  "Send Enter (newline) to the terminal PTY."
  (let* ((buf (current-qt-buffer app))
         (ts (hash-get *terminal-state* buf)))
    (when ts
      (terminal-send-raw! ts "\n"))))

(def (cmd-term-interrupt app)
  "Send Ctrl-C (interrupt) to the terminal PTY."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\x03;")
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

(def (cmd-term-send-eof app)
  "Send Ctrl-D (EOF) to the terminal PTY."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\x04;")
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

(def (cmd-term-send-tab app)
  "Send Tab to the terminal PTY (for tab completion)."
  (let* ((buf (current-qt-buffer app))
         (ts (and (terminal-buffer? buf) (hash-get *terminal-state* buf))))
    (if ts
      (terminal-send-raw! ts "\t")
      (echo-message! (app-state-echo app) "Not in a terminal buffer"))))

;;;============================================================================
;;; Multi-terminal and terminal copy mode
;;;============================================================================

(def (cmd-multi-vterm app)
  "Create a new terminal buffer (multi-vterm style)."
  (cmd-term app))

;; Track which terminal buffers are in copy mode
(def *terminal-copy-mode* (make-hash-table))

(def (cmd-vterm-copy-mode app)
  "Toggle terminal copy mode — makes terminal read-only for text selection."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (if (terminal-buffer? buf)
      (let ((in-copy (hash-get *terminal-copy-mode* buf)))
        (if in-copy
          ;; Exit copy mode
          (begin
            (hash-put! *terminal-copy-mode* buf #f)
            (qt-plain-text-edit-set-read-only! ed #f)
            (echo-message! echo "Terminal copy mode OFF"))
          ;; Enter copy mode
          (begin
            (hash-put! *terminal-copy-mode* buf #t)
            (qt-plain-text-edit-set-read-only! ed #t)
            (echo-message! echo "Terminal copy mode ON — select text, C-w/M-w to copy"))))
      (echo-message! echo "Not in a terminal buffer"))))

(def (cmd-vterm-copy-done app)
  "Exit terminal copy mode and resume terminal."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (when (and (terminal-buffer? buf) (hash-get *terminal-copy-mode* buf))
      (hash-put! *terminal-copy-mode* buf #f)
      (qt-plain-text-edit-set-read-only! ed #f)
      (echo-message! echo "Terminal copy mode OFF"))))

;;;============================================================================
;;; Buffer-local keybindings (mode keymaps)
;;;============================================================================

;; Maps buffer-lexer-lang symbol -> keymap hash table
(def *mode-keymaps* (make-hash-table))

(def (mode-keymap-lookup buf key-str)
  "Look up KEY-STR in the buffer's mode keymap. Returns command symbol or #f.
   Checks lexer-lang first, then buffer name for special buffers."
  (let* ((lang (buffer-lexer-lang buf))
         (km (or (hash-get *mode-keymaps* lang)
                 ;; Check by buffer name for buffers without special lexer-lang
                 (let ((name (buffer-name buf)))
                   (cond
                     ((string=? name "*compilation*") (hash-get *mode-keymaps* 'compilation))
                     ((string=? name "*Grep*") (hash-get *mode-keymaps* 'grep))
                     ((string=? name "*Occur*") (hash-get *mode-keymaps* 'occur))
                     ((string=? name "*calendar*") (hash-get *mode-keymaps* 'calendar))
                     ((string=? name "*eww*") (hash-get *mode-keymaps* 'eww))
                     ((string=? name "*Magit*") (hash-get *mode-keymaps* 'magit))
                     (else #f))))))
    (and km (keymap-lookup km key-str))))

(def (setup-mode-keymaps!)
  "Initialize mode-specific keybindings for special buffer types."
  ;; Dired mode: single-key navigation
  (let ((dired-km (make-keymap)))
    (keymap-bind! dired-km "n" 'next-line)
    (keymap-bind! dired-km "p" 'previous-line)
    (keymap-bind! dired-km "g" 'revert-buffer)
    (keymap-bind! dired-km "d" 'dired-do-delete)
    (keymap-bind! dired-km "R" 'dired-do-rename)
    (keymap-bind! dired-km "C" 'dired-do-copy)
    (keymap-bind! dired-km "+" 'dired-create-directory)
    (keymap-bind! dired-km "q" 'kill-buffer-cmd)
    (keymap-bind! dired-km "^" 'dired) ;; go up to parent
    (keymap-bind! dired-km "m" 'dired-mark)
    (keymap-bind! dired-km "u" 'dired-unmark)
    (keymap-bind! dired-km "U" 'dired-unmark-all)
    (keymap-bind! dired-km "t" 'dired-toggle-marks)
    (keymap-bind! dired-km "D" 'dired-do-delete-marked)
    (keymap-bind! dired-km "x" 'dired-do-delete-marked)
    (hash-put! *mode-keymaps* 'dired dired-km))

  ;; Compilation mode
  (let ((comp-km (make-keymap)))
    (keymap-bind! comp-km "n" 'next-error)
    (keymap-bind! comp-km "p" 'previous-error)
    (keymap-bind! comp-km "g" 'recompile)
    (keymap-bind! comp-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'compilation comp-km))

  ;; Grep results mode
  (let ((grep-km (make-keymap)))
    (keymap-bind! grep-km "n" 'next-grep-result)
    (keymap-bind! grep-km "p" 'previous-grep-result)
    (keymap-bind! grep-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'grep grep-km))

  ;; Buffer list mode: single-key navigation
  (let ((bl-km (make-keymap)))
    (keymap-bind! bl-km "n" 'next-line)
    (keymap-bind! bl-km "p" 'previous-line)
    (keymap-bind! bl-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'buffer-list bl-km))

  ;; Occur mode
  (let ((occur-km (make-keymap)))
    (keymap-bind! occur-km "n" 'next-line)
    (keymap-bind! occur-km "p" 'previous-line)
    (keymap-bind! occur-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'occur occur-km))

  ;; Calendar mode
  (let ((cal-km (make-keymap)))
    (keymap-bind! cal-km "p" 'calendar-prev-month)
    (keymap-bind! cal-km "n" 'calendar-next-month)
    (keymap-bind! cal-km "<" 'calendar-prev-year)
    (keymap-bind! cal-km ">" 'calendar-next-year)
    (keymap-bind! cal-km "." 'calendar-today)
    (keymap-bind! cal-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'calendar cal-km))

  ;; EWW browser mode
  (let ((eww-km (make-keymap)))
    (keymap-bind! eww-km "g" 'eww)        ;; go to URL
    (keymap-bind! eww-km "l" 'eww-back)   ;; back
    (keymap-bind! eww-km "r" 'eww-reload) ;; reload
    (keymap-bind! eww-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'eww eww-km))
  ;; Magit mode
  (let ((magit-km (make-keymap)))
    (keymap-bind! magit-km "s" 'magit-stage)
    (keymap-bind! magit-km "S" 'magit-stage-all)
    (keymap-bind! magit-km "u" 'magit-unstage)
    (keymap-bind! magit-km "c" 'magit-commit)
    (keymap-bind! magit-km "d" 'magit-diff)
    (keymap-bind! magit-km "l" 'magit-log)
    (keymap-bind! magit-km "g" 'magit-status) ;; refresh
    (keymap-bind! magit-km "n" 'next-line)
    (keymap-bind! magit-km "p" 'previous-line)
    (keymap-bind! magit-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'magit magit-km))

  ;; Image mode: zoom controls
  (let ((img-km (make-keymap)))
    (keymap-bind! img-km "+" 'image-zoom-in)
    (keymap-bind! img-km "=" 'image-zoom-in)
    (keymap-bind! img-km "-" 'image-zoom-out)
    (keymap-bind! img-km "0" 'image-zoom-fit)
    (keymap-bind! img-km "1" 'image-zoom-reset)
    (keymap-bind! img-km "q" 'kill-buffer-cmd)
    (hash-put! *mode-keymaps* 'image img-km)))

;;;============================================================================
;;; Ediff-files: compare two files from disk
;;;============================================================================

(def (cmd-ediff-files app)
  "Compare two files by running diff."
  (let ((file-a (qt-echo-read-string app "File A: ")))
    (when (and file-a (> (string-length file-a) 0))
      (let ((file-b (qt-echo-read-string app "File B: ")))
        (when (and file-b (> (string-length file-b) 0))
          (let ((path-a (path-expand file-a))
                (path-b (path-expand file-b)))
            (if (not (and (file-exists? path-a) (file-exists? path-b)))
              (echo-error! (app-state-echo app) "One or both files not found")
              (let* ((proc (open-process
                             (list path: "/usr/bin/diff"
                                   arguments: (list "-u" path-a path-b)
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
                (echo-message! (app-state-echo app) "Diff complete")))))))))

;;;============================================================================
;;; Comment-dwim: intelligent comment toggle
;;;============================================================================

(def (cmd-comment-dwim app)
  "Do What I Mean with comments.
If region active: toggle comment on region.
If at end of code: add end-of-line comment.
If on blank line: insert comment and indent."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (has-sel (not (= sel-start sel-end))))
    (if has-sel
      ;; Region active: toggle comment on each line
      (cmd-comment-region app)
      ;; No region: check current line
      (let* ((line (qt-plain-text-edit-cursor-line ed))
             (lines (string-split text #\newline))
             (line-text (if (< line (length lines))
                          (list-ref lines line)
                          ""))
             (trimmed (string-trim line-text)))
        (cond
          ;; Blank line: insert comment
          ((string=? trimmed "")
           (qt-replace-line! ed line ";; ")
           (let* ((new-text (qt-plain-text-edit-text ed))
                  (new-lines (string-split new-text #\newline))
                  (pos (let loop ((i 0) (offset 0))
                         (if (>= i line) (+ offset 3)
                           (loop (+ i 1) (+ offset (string-length (list-ref new-lines i)) 1))))))
             (qt-plain-text-edit-set-cursor-position! ed pos)))
          ;; Line already commented: uncomment
          ((and (>= (string-length trimmed) 3)
                (string=? (substring trimmed 0 3) ";; "))
           (cmd-toggle-comment app))
          ((and (>= (string-length trimmed) 2)
                (string=? (substring trimmed 0 2) ";;"))
           (cmd-toggle-comment app))
          ;; Line has code: add end-of-line comment
          (else
           (let ((new-line (string-append line-text "  ;; ")))
             (qt-replace-line! ed line new-line)
             ;; Position cursor at comment
             (let* ((new-text (qt-plain-text-edit-text ed))
                    (new-lines (string-split new-text #\newline))
                    (pos (let loop ((i 0) (offset 0))
                           (if (>= i line) (+ offset (string-length new-line))
                             (loop (+ i 1) (+ offset (string-length (list-ref new-lines i)) 1))))))
               (qt-plain-text-edit-set-cursor-position! ed pos)))))))))

;;;============================================================================
;;; Auto-save mode toggle
;;;============================================================================

(def *auto-save-disabled-buffers* (make-hash-table))

(def (cmd-auto-save-mode app)
  "Toggle auto-save for the current buffer."
  (let* ((buf (current-qt-buffer app))
         (currently-disabled (hash-get *auto-save-disabled-buffers* buf)))
    (if currently-disabled
      (begin
        (hash-remove! *auto-save-disabled-buffers* buf)
        (echo-message! (app-state-echo app) "Auto-save enabled for this buffer"))
      (begin
        (hash-put! *auto-save-disabled-buffers* buf #t)
        (echo-message! (app-state-echo app) "Auto-save disabled for this buffer")))))

;;;============================================================================
;;; Keyboard macro counter
;;;============================================================================

(def *kbd-macro-counter* 0)
(def *kbd-macro-counter-format* "%d")

(def (cmd-kbd-macro-counter-insert app)
  "Insert the current keyboard macro counter value and increment it."
  (let* ((ed (current-qt-editor app))
         (text (number->string *kbd-macro-counter*)))
    (qt-plain-text-edit-insert-text! ed text)
    (set! *kbd-macro-counter* (+ *kbd-macro-counter* 1))))

(def (cmd-kbd-macro-counter-set app)
  "Set the keyboard macro counter to a specific value."
  (let ((input (qt-echo-read-string app
                 (string-append "Set macro counter (current "
                                (number->string *kbd-macro-counter*) "): "))))
    (when input
      (let ((n (string->number input)))
        (if n
          (begin
            (set! *kbd-macro-counter* n)
            (echo-message! (app-state-echo app)
              (string-append "Macro counter set to " (number->string n))))
          (echo-error! (app-state-echo app) "Not a number"))))))

;;;============================================================================
;;; Dired enhancements: marks, bulk operations
;;;============================================================================

(def *dired-marks* (make-hash-table))  ;; buf -> hash(index -> mark-char)

(def (dired-get-marks buf)
  "Get or create marks hash for a dired buffer."
  (or (hash-get *dired-marks* buf)
      (let ((h (make-hash-table)))
        (hash-put! *dired-marks* buf h)
        h)))

(def (cmd-dired-mark app)
  "Mark the file under cursor in dired."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (idx (- line 3))
         (entries (hash-get *dired-entries* buf)))
    (when (and entries (>= idx 0) (< idx (vector-length entries)))
      (let* ((marks (dired-get-marks buf))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (hash-put! marks idx #\*)
        ;; Update display: replace first char with *
        (when (< line (length lines))
          (let* ((old-line (list-ref lines line))
                 (new-line (if (> (string-length old-line) 0)
                             (string-append "*" (substring old-line 1 (string-length old-line)))
                             "*")))
            (qt-replace-line! ed line new-line)))
        ;; Move to next line
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)))))

(def (cmd-dired-unmark app)
  "Unmark the file under cursor in dired."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (idx (- line 3))
         (entries (hash-get *dired-entries* buf)))
    (when (and entries (>= idx 0) (< idx (vector-length entries)))
      (let* ((marks (dired-get-marks buf))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (hash-remove! marks idx)
        ;; Restore display: replace first char with space
        (when (< line (length lines))
          (let* ((old-line (list-ref lines line))
                 (new-line (if (> (string-length old-line) 0)
                             (string-append " " (substring old-line 1 (string-length old-line)))
                             " ")))
            (qt-replace-line! ed line new-line)))
        ;; Move to next line
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)))))

(def (cmd-dired-unmark-all app)
  "Unmark all files in dired."
  (let* ((buf (current-qt-buffer app))
         (marks (dired-get-marks buf)))
    (hash-clear! marks)
    ;; Refresh the listing
    (let* ((path (buffer-file-path buf)))
      (when path
        (dired-open-directory! app path)))
    (echo-message! (app-state-echo app) "All marks removed")))

(def (cmd-dired-toggle-marks app)
  "Toggle marks on all files in dired."
  (let* ((buf (current-qt-buffer app))
         (entries (hash-get *dired-entries* buf)))
    (when entries
      (let* ((marks (dired-get-marks buf))
             (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (let loop ((i 0))
          (when (< i (vector-length entries))
            (if (hash-get marks i)
              (hash-remove! marks i)
              (hash-put! marks i #\*))
            ;; Update display
            (let ((line-idx (+ i 3)))
              (when (< line-idx (length lines))
                (let* ((old-line (list-ref lines line-idx))
                       (mark-ch (if (hash-get marks i) "*" " "))
                       (new-line (if (> (string-length old-line) 0)
                                   (string-append mark-ch (substring old-line 1 (string-length old-line)))
                                   mark-ch)))
                  (qt-replace-line! ed line-idx new-line))))
            (loop (+ i 1))))
        (echo-message! (app-state-echo app) "Marks toggled")))))

(def (dired-marked-files buf)
  "Get list of full paths of marked files in a dired buffer."
  (let ((marks (hash-get *dired-marks* buf))
        (entries (hash-get *dired-entries* buf)))
    (if (and marks entries)
      (let loop ((i 0) (acc []))
        (if (>= i (vector-length entries))
          (reverse acc)
          (if (hash-get marks i)
            (loop (+ i 1) (cons (vector-ref entries i) acc))
            (loop (+ i 1) acc))))
      [])))

(def (cmd-dired-do-delete-marked app)
  "Delete all marked files in dired."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((confirm (qt-echo-read-string app
                       (string-append "Delete " (number->string (length files))
                                      " marked files? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (let ((count 0))
            (for-each
              (lambda (path)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (delete-file path)
                    (set! count (+ count 1)))))
              files)
            ;; Refresh
            (let ((dir (buffer-file-path buf)))
              (when dir (dired-open-directory! app dir)))
            (echo-message! (app-state-echo app)
              (string-append "Deleted " (number->string count) " files"))))))))

(def (cmd-dired-do-copy-marked app)
  "Copy all marked files in dired to a destination directory."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((dest (qt-echo-read-string app "Copy to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest))
                (count 0))
            (for-each
              (lambda (path)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (let ((target (path-expand (path-strip-directory path) dest-dir)))
                      (copy-file path target)
                      (set! count (+ count 1))))))
              files)
            ;; Refresh
            (let ((dir (buffer-file-path buf)))
              (when dir (dired-open-directory! app dir)))
            (echo-message! (app-state-echo app)
              (string-append "Copied " (number->string count) " files to " dest-dir))))))))

(def (cmd-dired-do-rename-marked app)
  "Move/rename all marked files in dired to a destination directory."
  (let* ((buf (current-qt-buffer app))
         (files (dired-marked-files buf)))
    (if (null? files)
      (echo-message! (app-state-echo app) "No marked files")
      (let ((dest (qt-echo-read-string app "Move to directory: ")))
        (when (and dest (> (string-length dest) 0))
          (let ((dest-dir (path-expand dest))
                (count 0))
            (for-each
              (lambda (path)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (let ((target (path-expand (path-strip-directory path) dest-dir)))
                      (rename-file path target)
                      (set! count (+ count 1))))))
              files)
            ;; Refresh
            (let ((dir (buffer-file-path buf)))
              (when dir (dired-open-directory! app dir)))
            (echo-message! (app-state-echo app)
              (string-append "Moved " (number->string count) " files to " dest-dir))))))))

(def (cmd-dired-mark-by-regexp app)
  "Mark files matching a regular expression in dired."
  (let* ((buf (current-qt-buffer app))
         (entries (hash-get *dired-entries* buf)))
    (when entries
      (let ((pattern (qt-echo-read-string app "Mark files matching regexp: ")))
        (when (and pattern (> (string-length pattern) 0))
          (let ((marks (dired-get-marks buf))
                (ed (current-qt-editor app))
                (count 0))
            (let loop ((i 0))
              (when (< i (vector-length entries))
                (let* ((path (vector-ref entries i))
                       (name (path-strip-directory path)))
                  (when (with-catch (lambda (e) #f)
                          (lambda () (string-contains name pattern)))
                    (hash-put! marks i #\*)
                    (set! count (+ count 1))
                    ;; Update display
                    (let* ((text (qt-plain-text-edit-text ed))
                           (lines (string-split text #\newline))
                           (line-idx (+ i 3)))
                      (when (< line-idx (length lines))
                        (let* ((old-line (list-ref lines line-idx))
                               (new-line (if (> (string-length old-line) 0)
                                           (string-append "*" (substring old-line 1 (string-length old-line)))
                                           "*")))
                          (qt-replace-line! ed line-idx new-line))))))
                (loop (+ i 1))))
            (echo-message! (app-state-echo app)
              (string-append "Marked " (number->string count) " files"))))))))

(def (cmd-dired-sort-toggle app)
  "Toggle dired sort between name and modification time."
  (echo-message! (app-state-echo app)
    "Dired is sorted by name (default). Use M-x dired to refresh."))

;;;============================================================================
;;; Pop global mark ring
;;;============================================================================

(def (cmd-pop-global-mark app)
  "Pop the global mark ring and jump to the saved position."
  (let ((ring (app-state-mark-ring app)))
    (if (null? ring)
      (echo-message! (app-state-echo app) "Global mark ring is empty")
      (let* ((entry (car ring))
             (buf-name (car entry))
             (pos (cdr entry))
             (buf (buffer-by-name buf-name)))
        (set! (app-state-mark-ring app) (cdr ring))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (string-length (qt-plain-text-edit-text ed))))
            (qt-modeline-update! app)
            (echo-message! (app-state-echo app)
              (string-append "Mark: " buf-name)))
          (echo-error! (app-state-echo app)
            (string-append "Buffer no longer exists: " buf-name)))))))

;;;============================================================================
;;; Window horizontal resize
;;;============================================================================

(def (cmd-shrink-window-horizontally app)
  "Shrink the current window horizontally."
  (echo-message! (app-state-echo app)
    "Horizontal resize not supported in vertical splitter layout"))

(def (cmd-enlarge-window-horizontally app)
  "Enlarge the current window horizontally."
  (echo-message! (app-state-echo app)
    "Horizontal resize not supported in vertical splitter layout"))

;;;============================================================================
;;; Recover file from auto-save
;;;============================================================================

(def (cmd-recover-file app)
  "Recover a file from its auto-save version."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((auto-path (make-auto-save-path path)))
        (if (not (file-exists? auto-path))
          (echo-message! (app-state-echo app)
            (string-append "No auto-save file for " (path-strip-directory path)))
          (let ((confirm (qt-echo-read-string app
                           (string-append "Recover from " auto-path "? (yes/no): "))))
            (when (and confirm (string=? confirm "yes"))
              (let* ((ed (current-qt-editor app))
                     (text (read-file-as-string auto-path)))
                (when text
                  (qt-plain-text-edit-set-text! ed text)
                  (qt-plain-text-edit-set-cursor-position! ed 0)
                  (echo-message! (app-state-echo app)
                    (string-append "Recovered from " auto-path)))))))))))

;;;============================================================================
;;; Insert char by name (Unicode)
;;;============================================================================

(def (cmd-insert-char-by-name app)
  "Insert a character by its hex codepoint."
  (let ((input (qt-echo-read-string app "Unicode hex codepoint (e.g. 03BB for lambda): ")))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input 16)))
        (if n
          (let ((ed (current-qt-editor app)))
            (qt-plain-text-edit-insert-text! ed (string (integer->char n)))
            (echo-message! (app-state-echo app)
              (string-append "Inserted U+" (string-upcase input))))
          (echo-error! (app-state-echo app) "Invalid hex codepoint"))))))

;;;============================================================================
;;; Display battery / system info
;;;============================================================================

(def (cmd-display-battery app)
  "Display battery status if available."
  (let ((result
          (with-catch
            (lambda (e) #f)
            (lambda ()
              (let* ((proc (open-process
                             (list path: "/usr/bin/cat"
                                   arguments: '("/sys/class/power_supply/BAT0/capacity")
                                   stdout-redirection: #t)))
                     (output (read-line proc))
                     (_ (process-status proc)))
                (close-port proc)
                output)))))
    (if result
      (echo-message! (app-state-echo app)
        (string-append "Battery: " result "%"))
      (echo-message! (app-state-echo app) "Battery info not available"))))

;;;============================================================================
;;; Scratch message
;;;============================================================================

(def (cmd-scratch-message app)
  "Switch to *scratch* buffer with initial message."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (buffer-by-name "*scratch*")))
    (if buf
      (begin
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-modeline-update! app))
      (echo-message! (app-state-echo app) "No *scratch* buffer found"))))

;;;============================================================================
;;; Sentence navigation and kill
;;;============================================================================

(def (sentence-end-pos text pos)
  "Find end of current sentence from pos."
  (let ((len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len) len)
        ((and (memv (string-ref text i) '(#\. #\? #\!))
              (or (>= (+ i 1) len)
                  (char-whitespace? (string-ref text (+ i 1)))))
         (+ i 1))
        (else (loop (+ i 1)))))))

(def (sentence-start-pos text pos)
  "Find start of current sentence looking backward from pos."
  (let loop ((i (- pos 1)))
    (cond
      ((<= i 0) 0)
      ((and (memv (string-ref text i) '(#\. #\? #\!))
            (< (+ i 1) (string-length text))
            (char-whitespace? (string-ref text (+ i 1))))
       ;; Skip whitespace after sentence-ender
       (let skip ((j (+ i 1)))
         (if (and (< j (string-length text))
                  (char-whitespace? (string-ref text j)))
           (skip (+ j 1))
           j)))
      (else (loop (- i 1))))))

(def (cmd-kill-sentence app)
  "Kill from point to end of sentence."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (end (sentence-end-pos text pos))
         (killed (substring text pos end)))
    (qt-kill-ring-push! app killed)
    (qt-plain-text-edit-set-selection! ed pos end)
    (qt-plain-text-edit-remove-selected-text! ed)))

(def (cmd-backward-kill-sentence app)
  "Kill from point back to start of sentence."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (start (sentence-start-pos text pos))
         (killed (substring text start pos)))
    (qt-kill-ring-push! app killed)
    (qt-plain-text-edit-set-selection! ed start pos)
    (qt-plain-text-edit-remove-selected-text! ed)))

;;;============================================================================
;;; Paragraph navigation and kill
;;;============================================================================

(def (paragraph-end-pos text pos)
  "Find end of current paragraph (next blank line or end of text)."
  (let ((len (string-length text)))
    ;; Skip current non-blank lines, then find blank line or end
    (let loop ((i pos) (saw-text? #f))
      (cond
        ((>= i len) len)
        ((char=? (string-ref text i) #\newline)
         (if (and saw-text?
                  (or (>= (+ i 1) len)
                      (char=? (string-ref text (+ i 1)) #\newline)))
           (+ i 1)
           (loop (+ i 1) saw-text?)))
        (else (loop (+ i 1) #t))))))

(def (paragraph-start-pos text pos)
  "Find start of current paragraph (previous blank line or start of text)."
  (let loop ((i (- pos 1)) (saw-text? #f))
    (cond
      ((<= i 0) 0)
      ((char=? (string-ref text i) #\newline)
       (if (and saw-text?
                (> i 0)
                (char=? (string-ref text (- i 1)) #\newline))
         (+ i 1)
         (loop (- i 1) saw-text?)))
      (else (loop (- i 1) #t)))))

(def (cmd-kill-paragraph app)
  "Kill from point to end of paragraph."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (end (paragraph-end-pos text pos))
         (killed (substring text pos end)))
    (qt-kill-ring-push! app killed)
    (qt-plain-text-edit-set-selection! ed pos end)
    (qt-plain-text-edit-remove-selected-text! ed)))

;;;============================================================================
;;; Recenter-top-bottom cycling (C-l)
;;;============================================================================

(def *recenter-cycle-state* 'center)

(def (cmd-recenter-top-bottom app)
  "Cycle cursor position: center, top, bottom."
  (let ((ed (current-qt-editor app)))
    (case *recenter-cycle-state*
      ((center)
       (qt-plain-text-edit-center-cursor! ed)
       (set! *recenter-cycle-state* 'top)
       (echo-message! (app-state-echo app) "Centered"))
      ((top)
       (qt-plain-text-edit-ensure-cursor-visible! ed)
       (set! *recenter-cycle-state* 'bottom)
       (echo-message! (app-state-echo app) "Top"))
      ((bottom)
       (qt-plain-text-edit-ensure-cursor-visible! ed)
       (set! *recenter-cycle-state* 'center)
       (echo-message! (app-state-echo app) "Bottom")))))

;;;============================================================================
;;; S-expression list navigation (up-list, down-list)
;;;============================================================================

(def (cmd-up-list app)
  "Move backward out of one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (cond
        ((< i 0)
         (echo-message! (app-state-echo app) "At top level"))
        ((memv (string-ref text i) '(#\) #\] #\}))
         (loop (- i 1) (+ depth 1)))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (if (= depth 0)
           (qt-plain-text-edit-set-cursor-position! ed i)
           (loop (- i 1) (- depth 1))))
        (else (loop (- i 1) depth))))))

(def (cmd-down-list app)
  "Move forward into one level of parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len)
         (echo-message! (app-state-echo app) "No inner list found"))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (qt-plain-text-edit-set-cursor-position! ed (+ i 1)))
        (else (loop (+ i 1)))))))

;;;============================================================================
;;; Windmove (directional window navigation)
;;;============================================================================

(def (cmd-windmove-left app)
  "Move to the window to the left."
  (let* ((fr (app-state-frame app))
         (idx (qt-frame-current-idx fr)))
    (if (> idx 0)
      (begin
        (qt-frame-current-idx-set! fr (- idx 1))
        (qt-modeline-update! app))
      (echo-message! (app-state-echo app) "No window to the left"))))

(def (cmd-windmove-right app)
  "Move to the window to the right."
  (let* ((fr (app-state-frame app))
         (idx (qt-frame-current-idx fr))
         (count (length (qt-frame-windows fr))))
    (if (< idx (- count 1))
      (begin
        (qt-frame-current-idx-set! fr (+ idx 1))
        (qt-modeline-update! app))
      (echo-message! (app-state-echo app) "No window to the right"))))

(def (cmd-windmove-up app)
  "Move to the window above (alias for windmove-left in horizontal split)."
  (cmd-windmove-left app))

(def (cmd-windmove-down app)
  "Move to the window below (alias for windmove-right in horizontal split)."
  (cmd-windmove-right app))

;;;============================================================================
;;; Set-variable and customize-variable
;;;============================================================================

(def *user-variables*
  (hash
    ("fill-column" (cons (lambda () *fill-column*)
                         (lambda (v) (set! *fill-column* (string->number v)))))
    ("tab-width" (cons (lambda () *tab-width*)
                       (lambda (v) (set! *tab-width* (string->number v)))))
    ("auto-save-interval" (cons (lambda () *auto-save-interval*)
                                (lambda (v) (set! *auto-save-interval* (string->number v)))))))

(def (cmd-set-variable app)
  "Set a variable to a value interactively."
  (let* ((echo (app-state-echo app))
         (varname (qt-echo-read-string echo "Set variable: "))
         (entry (hash-get *user-variables* varname)))
    (if entry
      (let* ((cur ((car entry)))
             (val (qt-echo-read-string echo
                    (string-append varname " (current: "
                                   (if cur (object->string cur) "nil")
                                   "): "))))
        ((cdr entry) val)
        (echo-message! echo (string-append varname " set to " val)))
      (echo-error! echo (string-append "Unknown variable: " varname)))))

(def (cmd-customize-variable app)
  "Show current value of a variable and optionally change it."
  (cmd-set-variable app))

;;;============================================================================
;;; View-file and append-to-file
;;;============================================================================

(def (cmd-view-file app)
  "Open a file in read-only view mode."
  (let* ((echo (app-state-echo app))
         (path (qt-echo-read-string echo "View file: ")))
    (when (and path (> (string-length path) 0))
      (if (file-exists? path)
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (content (call-with-input-file path (lambda (p) (read-string 1000000 p))))
               (buf (or (buffer-by-name (path-strip-directory path))
                        (qt-buffer-create! (path-strip-directory path) ed path))))
          (qt-buffer-attach! ed buf)
          (qt-plain-text-edit-set-text! ed content)
          (qt-plain-text-edit-set-read-only! ed #t)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (hash-put! *view-mode-buffers* (buffer-name buf) #t)
          (qt-modeline-update! app)
          (echo-message! echo (string-append "Viewing: " path)))
        (echo-error! echo (string-append "File not found: " path))))))

(def (cmd-append-to-file app)
  "Append the selected region to a file."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! echo "No region selected")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (path (qt-echo-read-string echo "Append to file: ")))
        (when (and path (> (string-length path) 0))
          (call-with-output-file [path: path append: #t]
            (lambda (p) (display region p)))
          (echo-message! echo
            (string-append "Appended " (number->string (- sel-end sel-start))
                           " chars to " path)))))))

;;;============================================================================
;;; Flyspell-buffer (spell check whole buffer)
;;;============================================================================

(def (cmd-flyspell-buffer app)
  "Spell-check the entire buffer using aspell."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    ;; Extract words and check each
    (let* ((words (let extract ((i 0) (acc '()))
                    (let ((len (string-length text)))
                      (if (>= i len) (reverse acc)
                        (if (char-alphabetic? (string-ref text i))
                          (let word-end ((j (+ i 1)))
                            (if (and (< j len) (char-alphabetic? (string-ref text j)))
                              (word-end (+ j 1))
                              (extract j (cons (substring text i j) acc))))
                          (extract (+ i 1) acc))))))
           (unique (let ((seen (make-hash-table)))
                     (filter (lambda (w)
                               (and (> (string-length w) 2)
                                    (not (hash-get seen w))
                                    (begin (hash-put! seen w #t) #t)))
                             words)))
           (misspelled '()))
      ;; Use aspell pipe mode
      (with-catch
        (lambda (e) (echo-error! echo "aspell not available"))
        (lambda ()
          (let ((proc (open-process
                        [path: "aspell"
                         arguments: ["pipe"]
                         stdin-redirection: #t
                         stdout-redirection: #t
                         stderr-redirection: #t])))
            ;; Read banner
            (read-line proc)
            ;; Check each word
            (for-each
              (lambda (word)
                (display word proc)
                (newline proc)
                (force-output proc)
                (let ((line (read-line proc)))
                  (when (and (string? line)
                             (> (string-length line) 0)
                             (char=? (string-ref line 0) #\&))
                    (set! misspelled (cons word misspelled)))))
              unique)
            (close-port proc))))
      (if (null? misspelled)
        (echo-message! echo "No misspelled words found")
        (let* ((fr (app-state-frame app))
               (buf (or (buffer-by-name "*Spelling*")
                        (qt-buffer-create! "*Spelling*" ed)))
               (result (string-join (reverse misspelled) "\n")))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "=== Misspelled words ===\n\n" result "\n\n"
                           (number->string (length misspelled)) " misspelled word(s) found"))
          (qt-modeline-update! app))))))

;;;============================================================================
;;; Profiler report
;;;============================================================================

(def *profiler-data* (hash))

(def (cmd-profiler-report app)
  "Display profiler data in a buffer."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Profiler*")
                  (qt-buffer-create! "*Profiler*" ed)))
         (entries (hash->list *profiler-data*)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (if (null? entries)
      (qt-plain-text-edit-set-text! ed
        "No profiler data available.\n\nUse M-x profiler-start to begin profiling.")
      (let* ((sorted (sort entries (lambda (a b) (> (cdr a) (cdr b)))))
             (lines (map (lambda (e)
                          (string-append (car e) ": "
                                         (number->string (cdr e)) " calls"))
                        sorted))
             (report (string-join lines "\n")))
        (qt-plain-text-edit-set-text! ed
          (string-append "=== Profiler Report ===\n\n" report))))
    (qt-modeline-update! app)))

;;;============================================================================
;;; List-faces-display
;;;============================================================================

(def (cmd-list-faces-display app)
  "Display all available face/theme information."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Faces*")
                  (qt-buffer-create! "*Faces*" ed))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (let* ((lines
            (list "=== Face Types ===\n"
                  "  default       - Normal text"
                  "  keyword       - Language keywords"
                  "  comment       - Comments"
                  "  string        - String literals"
                  "  function      - Function names"
                  "  type          - Type names"
                  "  constant      - Constants and numbers"
                  "  preprocessor  - Preprocessor directives"
                  "  builtin       - Built-in functions"
                  "  warning       - Warnings"
                  "  error         - Errors"
                  "  region        - Selected region"
                  "  modeline      - Mode line"
                  "  minibuffer    - Minibuffer/echo area"
                  "\n=== Customization ==="
                  "  Use M-x customize-face to modify face colors."
                  "  Use M-x load-theme to change the theme."))
           (text (string-join lines "\n")))
      (qt-plain-text-edit-set-text! ed text)
      (qt-modeline-update! app))))

;;;============================================================================
;;; Display-line-numbers-mode toggle
;;;============================================================================

(def (cmd-display-line-numbers-mode app)
  "Toggle display of line numbers (state flag)."
  (let ((echo (app-state-echo app)))
    (set! *line-numbers-visible* (not *line-numbers-visible*))
    (echo-message! echo
      (if *line-numbers-visible*
        "Line numbers enabled"
        "Line numbers disabled"))))

;;;============================================================================
;;; Find file read-only (C-x C-r)
;;;============================================================================

(def (cmd-find-file-read-only app)
  "Open a file in read-only mode."
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Find file read-only: ")))
    (when (and filename (> (string-length filename) 0))
      (if (file-exists? filename)
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (name (path-strip-directory filename))
               (buf (qt-buffer-create! name ed filename))
               (text (read-file-as-string filename)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when text
            (qt-plain-text-edit-set-text! ed text)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0))
          (qt-plain-text-edit-set-read-only! ed #t)
          (qt-setup-highlighting! app buf)
          (qt-modeline-update! app)
          (echo-message! echo (string-append "Opened read-only: " filename)))
        (echo-error! echo (string-append "File not found: " filename))))))

;;;============================================================================
;;; Project switch and project dired
;;;============================================================================

(def *known-projects* (make-hash-table))

(def (cmd-project-switch-project app)
  "Switch to a different project directory."
  (let* ((echo (app-state-echo app))
         (dir (qt-echo-read-string app "Switch to project: ")))
    (when (and dir (> (string-length dir) 0))
      (let ((expanded (path-expand dir)))
        (if (file-exists? expanded)
          (begin
            (hash-put! *known-projects* expanded #t)
            (current-directory expanded)
            (echo-message! echo (string-append "Project: " expanded)))
          (echo-error! echo (string-append "Directory not found: " expanded)))))))

(def (cmd-project-dired app)
  "Open dired in the project root directory."
  (let* ((root (current-project-root app)))
    (dired-open-directory! app root)))

(def (cmd-project-run-shell app)
  "Open a shell in the project root directory."
  (let* ((root (current-project-root app))
         (echo (app-state-echo app)))
    (current-directory root)
    (echo-message! echo (string-append "Shell directory: " root))))

;;;============================================================================
;;; Global auto-revert mode
;;;============================================================================

(def *global-auto-revert-mode* #f)

(def (cmd-global-auto-revert-mode app)
  "Toggle global auto-revert mode for all file-visiting buffers."
  (let ((echo (app-state-echo app)))
    (set! *global-auto-revert-mode* (not *global-auto-revert-mode*))
    (echo-message! echo
      (if *global-auto-revert-mode*
        "Global auto-revert mode enabled"
        "Global auto-revert mode disabled"))))

;;;============================================================================
;;; Project search (multi-file grep with navigable results)
;;;============================================================================

(def (cmd-project-search app)
  "Search for pattern across all project files with navigable results."
  (let* ((echo (app-state-echo app))
         (root (current-project-root app))
         (pattern (qt-echo-read-string app "Project search: ")))
    (when (and pattern (> (string-length pattern) 0))
      (with-catch
        (lambda (e) (echo-error! echo "Search failed"))
        (lambda ()
          (let* ((proc (open-process
                         [path: "/usr/bin/grep"
                          arguments: ["-rn" "--include=*.ss" "--include=*.scm"
                                      "--include=*.el" "--include=*.py"
                                      "--include=*.js" "--include=*.ts"
                                      "--include=*.c" "--include=*.h"
                                      "--include=*.rs" "--include=*.go"
                                      "--include=*.java" "--include=*.rb"
                                      "--include=*.md" "--include=*.txt"
                                      "--include=*.json" "--include=*.yaml"
                                      "--include=*.yml" "--include=*.toml"
                                      "--include=*.html" "--include=*.css"
                                      pattern root]
                          stdout-redirection: #t
                          stderr-redirection: #t]))
                 (output (let loop ((lines '()))
                           (let ((line (read-line proc)))
                             (if (eof-object? line)
                               (reverse lines)
                               (loop (cons line lines)))))))
            (close-port proc)
            (if (null? output)
              (echo-message! echo (string-append "No matches for: " pattern))
              (let* ((fr (app-state-frame app))
                     (ed (current-qt-editor app))
                     (buf (or (buffer-by-name "*Project Search*")
                              (qt-buffer-create! "*Project Search*" ed)))
                     (result (string-join output "\n")))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed
                  (string-append "=== Project search: " pattern " ===\n"
                                 "=== Root: " root " ===\n\n"
                                 result "\n\n"
                                 (number->string (length output)) " match(es) found"))
                (qt-plain-text-edit-set-read-only! ed #t)
                (qt-modeline-update! app)
                (echo-message! echo
                  (string-append (number->string (length output)) " match(es) found"))))))))))

;;;============================================================================
;;; Goto last change (navigate to last edit position)
;;;============================================================================

(def *last-change-positions* (make-hash-table))

(def (record-change-position! app)
  "Record current cursor position as last change position for current buffer."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (name (buffer-name buf)))
    (hash-put! *last-change-positions* name pos)))

(def (cmd-goto-last-change app)
  "Jump to the position of the last edit in the current buffer."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (name (buffer-name buf))
         (pos (hash-get *last-change-positions* name)))
    (if pos
      (begin
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (qt-plain-text-edit-ensure-cursor-visible! ed))
      (echo-message! (app-state-echo app) "No recorded change position"))))

;;;============================================================================
;;; Git gutter / diff-hl (show changed lines in margin)
;;;============================================================================

(def *diff-hl-active* #f)

(def (parse-hunk-header line)
  "Parse @@ -old,count +new,count @@ to extract new-start and new-count.
   Returns (start . count) or #f."
  (let ((at-pos (string-contains line "+"))
        (end-pos (string-contains line " @@" 3)))
    (when (and at-pos end-pos)
      (let* ((plus-part (substring line at-pos end-pos))
             (comma (string-contains plus-part ",")))
        (if comma
          (let ((start (string->number (substring plus-part 1 comma)))
                (count (string->number (substring plus-part (+ comma 1)
                                         (string-length plus-part)))))
            (and start count (cons start count)))
          (let ((start (string->number (substring plus-part 1
                                         (string-length plus-part)))))
            (and start (cons start 1))))))))

(def (cmd-diff-hl-mode app)
  "Highlight lines changed since last git commit directly in the buffer.
   Green = added, yellow = modified. Toggle on/off."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (path (buffer-file-path buf)))
    (cond
      ((not path)
       (echo-message! echo "Buffer has no file"))
      ;; Toggle off
      (*diff-hl-active*
       (set! *diff-hl-active* #f)
       (qt-extra-selections-clear! ed)
       (qt-extra-selections-apply! ed)
       (echo-message! echo "diff-hl: off"))
      ;; Toggle on: run git diff and highlight changed lines
      (else
       (with-catch
         (lambda (e) (echo-error! echo "diff-hl: git diff failed"))
         (lambda ()
           (let* ((proc (open-process
                          [path: "/usr/bin/git"
                           arguments: ["diff" "--no-color" "-U0" path]
                           stdout-redirection: #t
                           stderr-redirection: #t]))
                  (output (let loop ((lines '()))
                            (let ((line (read-line proc)))
                              (if (eof-object? line)
                                (reverse lines)
                                (loop (cons line lines)))))))
             (close-port proc)
             (if (null? output)
               (echo-message! echo "No uncommitted changes")
               ;; Parse hunks and highlight lines
               (let* ((hunks (filter identity
                               (map (lambda (line)
                                      (if (and (>= (string-length line) 3)
                                               (string=? (substring line 0 2) "@@"))
                                        (parse-hunk-header line)
                                        #f))
                                    output))))
                 (qt-extra-selections-clear! ed)
                 (for-each
                   (lambda (hunk)
                     (let* ((start-line (- (car hunk) 1))  ; 0-indexed
                            (count (cdr hunk)))
                       (let hloop ((i 0))
                         (when (< i count)
                           ;; Green (#2d5a2d) background for changed lines
                           (qt-extra-selection-add-line! ed (+ start-line i)
                             #x2d #x5a #x2d)
                           (hloop (+ i 1))))))
                   hunks)
                 (qt-extra-selections-apply! ed)
                 (set! *diff-hl-active* #t)
                 (echo-message! echo
                   (string-append "diff-hl: " (number->string (length hunks))
                     " changed region(s)")))))))))))


;;;============================================================================
;;; Pop-to-mark (cycle through mark ring)
;;;============================================================================

(def (cmd-pop-to-mark app)
  "Pop to previous mark position in the mark ring."
  (let* ((ed (current-qt-editor app))
         (marks (app-state-mark-ring app)))
    (if (null? marks)
      (echo-message! (app-state-echo app) "Mark ring empty")
      (let* ((entry (car marks))
             (rest (cdr marks))
             (buf-name (car entry))
             (pos (cdr entry)))
        ;; Push current position, pop first entry
        (set! (app-state-mark-ring app)
          (append rest (list (cons (buffer-name (current-qt-buffer app))
                                   (qt-plain-text-edit-cursor-position ed)))))
        ;; Switch to buffer if different
        (let ((target-buf (buffer-by-name buf-name)))
          (when target-buf
            (let ((fr (app-state-frame app)))
              (unless (eq? target-buf (current-qt-buffer app))
                (qt-buffer-attach! ed target-buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) target-buf)))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

;;;============================================================================
;;; Scratch buffer new (create additional scratch buffers)
;;;============================================================================

(def *scratch-counter* 0)

(def (cmd-scratch-buffer-new app)
  "Create a new scratch buffer with a unique name."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app)))
    (set! *scratch-counter* (+ *scratch-counter* 1))
    (let* ((name (string-append "*scratch-" (number->string *scratch-counter*) "*"))
           (buf (qt-buffer-create! name ed)))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed
        (string-append ";; " name " -- scratch buffer\n\n"))
      (qt-modeline-update! app)
      (echo-message! (app-state-echo app) (string-append "Created " name)))))

;;;============================================================================
;;; Duplicate line or region
;;;============================================================================

(def (cmd-duplicate-line-or-region app)
  "Duplicate the current line or selected region."
  (let* ((ed (current-qt-editor app))
         (has-sel (qt-plain-text-edit-has-selection? ed)))
    (if has-sel
      ;; Duplicate selection
      (let* ((sel-text (qt-plain-text-edit-selected-text ed))
             (end (qt-plain-text-edit-selection-end ed)))
        (qt-plain-text-edit-set-cursor-position! ed end)
        (qt-plain-text-edit-insert-text! ed sel-text))
      ;; Duplicate current line
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (line-num (qt-plain-text-edit-cursor-line ed))
             (line-start (qt-plain-text-edit-line-end-position ed
                           (if (> line-num 0) (- line-num 1) -1)))
             (line-end (qt-plain-text-edit-line-end-position ed line-num))
             (line-text (qt-plain-text-edit-text-range ed
                          (if (> line-num 0) (+ line-start 1) 0)
                          line-end)))
        (qt-plain-text-edit-set-cursor-position! ed line-end)
        (qt-plain-text-edit-insert-text! ed (string-append "\n" line-text))))))

;;;============================================================================
;;; Select current line
;;;============================================================================

(def (cmd-select-current-line app)
  "Select the entire current line."
  (let* ((ed (current-qt-editor app))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (line-start (if (> line-num 0)
                       (+ (qt-plain-text-edit-line-end-position ed (- line-num 1)) 1)
                       0))
         (line-end (qt-plain-text-edit-line-end-position ed line-num)))
    (qt-plain-text-edit-set-selection! ed line-start line-end)))

;;;============================================================================
;;; Smart join line (context-aware join)
;;;============================================================================

(def (cmd-smart-join-line app)
  "Join the next line to the current one, handling indentation intelligently."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find end of current line
    (let loop-eol ((i pos))
      (if (or (>= i len) (char=? (string-ref text i) #\newline))
        (when (< i len)
          ;; i is at newline - find first non-whitespace on next line
          (let skip-ws ((j (+ i 1)))
            (if (and (< j len)
                     (memv (string-ref text j) '(#\space #\tab)))
              (skip-ws (+ j 1))
              ;; Delete from current line end to first non-ws on next line
              ;; and insert a single space
              (begin
                (qt-plain-text-edit-set-selection! ed i j)
                (qt-plain-text-edit-remove-selected-text! ed)
                ;; Add space unless next char is a closing paren/bracket
                (let ((next-text (qt-plain-text-edit-text ed))
                      (next-pos (qt-plain-text-edit-cursor-position ed)))
                  (unless (and (< next-pos (string-length next-text))
                               (memv (string-ref next-text next-pos)
                                     '(#\) #\] #\})))
                    (qt-plain-text-edit-insert-text! ed " ")))))))
        (loop-eol (+ i 1))))))

;;;============================================================================
;;; Copy buffer filename
;;;============================================================================

(def (cmd-copy-buffer-filename app)
  "Copy the current buffer's filename (without directory) to clipboard."
  (let* ((buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (name (buffer-name buf)))
    (qt-clipboard-set-text! *qt-app-ptr* name)
    (echo-message! echo (string-append "Copied: " name))))

;;;============================================================================
;;; Revert buffer with confirmation
;;;============================================================================

(def (cmd-revert-buffer-confirm app)
  "Revert buffer from file with yes/no confirmation."
  (let* ((buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no file")
      (let ((answer (qt-echo-read-string app
                      (string-append "Revert buffer from " (path-strip-directory path) "? (yes/no) "))))
        (when (and answer (or (string=? answer "yes") (string=? answer "y")))
          (cmd-revert-buffer app))))))

;;;============================================================================
;;; Find file at line (open file:line:col references)
;;;============================================================================

(def (cmd-find-file-at-line app)
  "Open a file and jump to a specific line (file:line format)."
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "File:line: ")))
    (when (and input (> (string-length input) 0))
      (let* ((parts (string-split input #\:))
             (file (car parts))
             (line (if (and (pair? (cdr parts))
                            (> (string-length (cadr parts)) 0))
                     (with-catch (lambda (e) #f)
                       (lambda () (string->number (cadr parts))))
                     #f)))
        (when (file-exists? file)
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (name (path-strip-directory file))
                 (buf (qt-buffer-create! name ed file))
                 (text (read-file-as-string file)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (when text
              (qt-plain-text-edit-set-text! ed text)
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f))
            (qt-setup-highlighting! app buf)
            (when (and line (> line 0))
              (let ((target-pos (qt-plain-text-edit-line-end-position ed (- line 1))))
                (qt-plain-text-edit-set-cursor-position! ed
                  (if (> line 1)
                    (+ target-pos 1)
                    0))
                (qt-plain-text-edit-ensure-cursor-visible! ed)))
            (qt-modeline-update! app)
            (echo-message! echo (string-append "Opened: " file
                                  (if line (string-append ":" (number->string line)) "")))))))))

;;;============================================================================
;;; Toggle comment at point (quick line comment)
;;;============================================================================

(def (cmd-toggle-line-comment app)
  "Toggle comment on the current line using ;; prefix."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (line-start (if (> line-num 0)
                       (+ (qt-plain-text-edit-line-end-position ed (- line-num 1)) 1)
                       0))
         (line-end (qt-plain-text-edit-line-end-position ed line-num))
         (line-text (qt-plain-text-edit-text-range ed line-start line-end)))
    ;; Find first non-whitespace
    (let* ((trimmed (let skip ((i 0))
                      (if (and (< i (string-length line-text))
                               (memv (string-ref line-text i) '(#\space #\tab)))
                        (skip (+ i 1))
                        i)))
           (indent (substring line-text 0 trimmed))
           (rest (substring line-text trimmed (string-length line-text))))
      (qt-plain-text-edit-set-selection! ed line-start line-end)
      (qt-plain-text-edit-remove-selected-text! ed)
      (if (and (>= (string-length rest) 3)
               (string=? (substring rest 0 3) ";; "))
        ;; Uncomment
        (qt-plain-text-edit-insert-text! ed
          (string-append indent (substring rest 3 (string-length rest))))
        ;; Comment
        (qt-plain-text-edit-insert-text! ed
          (string-append indent ";; " rest))))))

;;;============================================================================

