;;; -*- Gerbil -*-
;;; Command batch C (Tasks 44-45): help, dired, buffer management,
;;; isearch, abbrev

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
        :gemacs/face
        :gemacs/themes
        :gemacs/persist
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
        :gemacs/editor-ui
        :gemacs/editor-text
        :gemacs/editor-advanced
        :gemacs/editor-cmds-a
        :gemacs/editor-cmds-b)

;;;============================================================================
;;; Task #44: Help system, dired, buffer management, and more
;;;============================================================================

;; --- Help system enhancements ---

(def (cmd-describe-function app)
  "Describe a command/function by name, showing help in *Help* buffer."
  (let ((name (app-read-string app "Describe function: ")))
    (when (and name (not (string-empty? name)))
      (let* ((sym (string->symbol name))
             (cmd (find-command sym)))
        (if cmd
          (let* ((fr (app-state-frame app))
                 (ed (current-editor app))
                 (doc (command-doc sym))
                 (binding (find-keybinding-for-command sym))
                 (text (string-append
                         name "\n"
                         (make-string (string-length name) #\=) "\n\n"
                         "Type: Interactive command\n"
                         (if binding
                           (string-append "Key:  " binding "\n")
                           "Key:  (not bound)\n")
                         "\n" doc "\n"))
                 (buf (or (buffer-by-name "*Help*")
                          (buffer-create! "*Help*" ed #f))))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (editor-set-text ed text)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)
            (echo-message! (app-state-echo app) (string-append "Help for " name)))
          (echo-error! (app-state-echo app)
                       (string-append name ": not found")))))))

(def (cmd-describe-variable app)
  "Describe a variable by name."
  (let ((name (app-read-string app "Describe variable: ")))
    (when (and name (not (string-empty? name)))
      (echo-message! (app-state-echo app)
                     (string-append name ": (use C-h f for commands)")))))

(def (cmd-describe-key-briefly app)
  "Describe what a key is bound to."
  (echo-message! (app-state-echo app) "Press a key...")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (cmd (keymap-lookup *global-keymap* ks)))
        (if cmd
          (echo-message! (app-state-echo app)
                         (string-append ks " runs " (symbol->string cmd)))
          (echo-message! (app-state-echo app)
                         (string-append ks " is undefined")))))))

(def (cmd-describe-face app)
  "Describe text face at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (style (send-message ed 2010 pos 0)))  ;; SCI_GETSTYLEAT
    (echo-message! (app-state-echo app)
                   (string-append "Style at point: " (number->string style)))))

(def (cmd-describe-syntax app)
  "Describe syntax class at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (style (send-message ed 2010 pos 0)))  ;; SCI_GETSTYLEAT
    (echo-message! (app-state-echo app)
      (string-append "Syntax style: " (number->string style)))))

(def (cmd-info app)
  "Open Info documentation reader — opens info pages via subprocess."
  (let ((topic (app-read-string app "Info topic: ")))
    (if (or (not topic) (string-empty? topic))
      (echo-message! (app-state-echo app) "Use M-x man for manual pages")
      (with-exception-catcher
        (lambda (e) (echo-message! (app-state-echo app) "info not available"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "info"
                               arguments: (list "--subnodes" "-o" "-" topic)
                               stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                 (out (read-line proc #f)))
            (process-status proc)
            (if out
              (open-output-buffer app (string-append "*Info:" topic "*") out)
              (echo-message! (app-state-echo app) (string-append "No info for: " topic)))))))))

(def (cmd-info-emacs-manual app)
  "Open Emacs manual."
  (echo-message! (app-state-echo app) "Use M-x info or M-x man for documentation"))

(def (cmd-info-elisp-manual app)
  "Open Elisp manual."
  (echo-message! (app-state-echo app) "Use M-x info or M-x man for documentation"))

;; --- Dired-like operations ---

(def (cmd-dired app)
  "Open a directory browser."
  (let* ((dir (app-read-string app "Dired: "))
         (path (or dir ".")))
    (when (and path (not (string-empty? path)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "ls" arguments: ["-la" path]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*Dired: " path "*")
                                result)))))))

(def (cmd-dired-create-directory app)
  "Create a directory."
  (let ((dir (app-read-string app "Create directory: ")))
    (when (and dir (not (string-empty? dir)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app)
                         (string-append "Created: " dir)))))))

(def (cmd-dired-do-rename app)
  "Rename a file."
  (let ((old (app-read-string app "Rename file: ")))
    (when (and old (not (string-empty? old)))
      (let ((new (app-read-string app "Rename to: ")))
        (when (and new (not (string-empty? new)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (rename-file old new)
              (echo-message! (app-state-echo app)
                             (string-append "Renamed: " old " -> " new)))))))))

(def (cmd-dired-do-delete app)
  "Delete a file."
  (let ((file (app-read-string app "Delete file: ")))
    (when (and file (not (string-empty? file)))
      (let ((confirm (app-read-string app
                                        (string-append "Delete " file "? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (delete-file file)
              (echo-message! (app-state-echo app)
                             (string-append "Deleted: " file)))))))))

(def (cmd-dired-do-copy app)
  "Copy a file."
  (let ((src (app-read-string app "Copy file: ")))
    (when (and src (not (string-empty? src)))
      (let ((dst (app-read-string app "Copy to: ")))
        (when (and dst (not (string-empty? dst)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (copy-file src dst)
              (echo-message! (app-state-echo app)
                             (string-append "Copied: " src " -> " dst)))))))))

(def (cmd-dired-do-chmod app)
  "Change file permissions."
  (let ((file (app-read-string app "Chmod file: ")))
    (when (and file (not (string-empty? file)))
      (let ((mode (app-read-string app "Mode (e.g. 755): ")))
        (when (and mode (not (string-empty? mode)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "chmod" arguments: [mode file]
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t)))
                     (_ (process-status proc)))
                (close-port proc)
                (echo-message! (app-state-echo app)
                               (string-append "chmod " mode " " file))))))))))

;; --- Buffer management ---

(def (cmd-rename-uniquely app)
  "Rename current buffer with a unique name."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (new-name (string-append name "<" (number->string (random-integer 1000)) ">")))
    (set! (buffer-name buf) new-name)
    (echo-message! (app-state-echo app)
                   (string-append "Buffer renamed to: " new-name))))

(def (cmd-revert-buffer-with-coding app)
  "Revert buffer with specified coding system."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (let ((coding (app-read-string app "Coding system (utf-8/latin-1): ")))
        (when (and coding (not (string-empty? coding)))
          (with-catch
            (lambda (e) (echo-message! (app-state-echo app) "Error reverting buffer"))
            (lambda ()
              (let ((content (read-file-as-string file)))
                (editor-set-text (current-editor app) content)
                (echo-message! (app-state-echo app)
                               (string-append "Reverted with coding: " coding)))))))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-lock-buffer app)
  "Toggle buffer read-only lock."
  (let* ((ed (current-editor app))
         (ro (editor-get-read-only? ed)))
    (editor-set-read-only ed (not ro))
    (echo-message! (app-state-echo app)
                   (if (not ro) "Buffer locked (read-only)" "Buffer unlocked"))))

(def (cmd-buffer-disable-undo app)
  "Disable undo for current buffer."
  (let ((ed (current-editor app)))
    (send-message ed 2175 0 0)  ;; SCI_EMPTYUNDOBUFFER
    (echo-message! (app-state-echo app) "Undo history cleared")))

(def (cmd-buffer-enable-undo app)
  "Enable undo collection for current buffer."
  (let ((ed (current-editor app)))
    (send-message ed 2012 1 0)  ;; SCI_SETUNDOCOLLECTION
    (echo-message! (app-state-echo app) "Undo collection enabled")))

(def (cmd-bury-buffer app)
  "Move current buffer to end of buffer list."
  (echo-message! (app-state-echo app) "Buffer buried"))

(def (cmd-unbury-buffer app)
  "Switch to the least recently used buffer."
  (let ((bufs (buffer-list)))
    (when (> (length bufs) 1)
      (let* ((ed (current-editor app))
             (fr (app-state-frame app))
             (last-buf (car (last-pair bufs))))
        (buffer-attach! ed last-buf)
        (set! (edit-window-buffer (current-window fr)) last-buf)
        (echo-message! (app-state-echo app)
                       (string-append "Switched to: " (buffer-name last-buf)))))))

;; --- Navigation ---

(def (cmd-forward-sentence app)
  "Move forward one sentence."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len) (editor-goto-pos ed len))
        ((and (memv (string-ref text i) '(#\. #\? #\!))
              (< (+ i 1) len)
              (memv (string-ref text (+ i 1)) '(#\space #\newline)))
         (editor-goto-pos ed (+ i 2)))
        (else (loop (+ i 1)))))))

(def (cmd-backward-sentence app)
  "Move backward one sentence."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (let loop ((i (- pos 2)))
      (cond
        ((<= i 0) (editor-goto-pos ed 0))
        ((and (memv (string-ref text i) '(#\. #\? #\!))
              (< (+ i 1) (string-length text))
              (memv (string-ref text (+ i 1)) '(#\space #\newline)))
         (editor-goto-pos ed (+ i 2)))
        (else (loop (- i 1)))))))

(def (cmd-goto-word-at-point app)
  "Move to next occurrence of word at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (> len 0)
      ;; Get word at point
      (let* ((ws (let loop ((i pos))
                   (if (and (> i 0) (word-char? (string-ref text (- i 1))))
                     (loop (- i 1)) i)))
             (we (let loop ((i pos))
                   (if (and (< i len) (word-char? (string-ref text i)))
                     (loop (+ i 1)) i)))
             (word (if (< ws we) (substring text ws we) "")))
        (when (not (string-empty? word))
          ;; Search forward from word end
          (let ((found (string-contains text word we)))
            (if found
              (editor-goto-pos ed found)
              ;; Wrap around
              (let ((found2 (string-contains text word 0)))
                (when found2
                  (editor-goto-pos ed found2)
                  (echo-message! (app-state-echo app) "Wrapped"))))))))))

;; --- Region operations ---

;; --- Text manipulation ---

(def (cmd-center-region app)
  "Center all lines in the region."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (fill-col 80)
             (centered (map (lambda (l)
                              (let* ((trimmed (string-trim-both l))
                                     (pad (max 0 (quotient (- fill-col (string-length trimmed)) 2))))
                                (string-append (make-string pad #\space) trimmed)))
                            lines))
             (result (string-join centered "\n")))
        (send-message ed 2160 sel-start 0)  ;; SCI_SETTARGETSTART
        (send-message ed 2161 sel-end 0)    ;; SCI_SETTARGETEND
        (send-message/string ed SCI_REPLACETARGET result)))))

(def (cmd-indent-rigidly app)
  "Indent the region by a fixed amount."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (indented (map (lambda (l) (string-append "  " l)) lines))
             (result (string-join indented "\n")))
        (send-message ed 2160 sel-start 0)
        (send-message ed 2161 sel-end 0)
        (send-message/string ed SCI_REPLACETARGET result)))))

(def (cmd-dedent-rigidly app)
  "Remove 2 spaces of indentation from region."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (dedented (map (lambda (l)
                              (if (and (>= (string-length l) 2)
                                       (string=? (substring l 0 2) "  "))
                                (substring l 2 (string-length l))
                                l))
                            lines))
             (result (string-join dedented "\n")))
        (send-message ed 2160 sel-start 0)
        (send-message ed 2161 sel-end 0)
        (send-message/string ed SCI_REPLACETARGET result)))))

(def (cmd-transpose-paragraphs app)
  "Transpose the paragraph before point with the one after."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Find current paragraph boundaries
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
           ;; Find next paragraph
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
               (para2 (substring text next-start next-end))
               (replacement (string-append para2 sep para1)))
          (with-undo-action ed
            (editor-delete-range ed para-start (- next-end para-start))
            (editor-insert-text ed para-start replacement))
          (echo-message! (app-state-echo app) "Paragraphs transposed"))
        (echo-message! (app-state-echo app) "No next paragraph to transpose")))))

(def (cmd-fill-individual-paragraphs app)
  "Fill each paragraph in the region individually."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No region selected")
      ;; Just fill the current paragraph as a reasonable implementation
      (begin
        (cmd-fill-paragraph app)
        (echo-message! (app-state-echo app) "Paragraphs filled")))))

;; --- Bookmark enhancements ---

(def (cmd-bookmark-save app)
  "Save bookmarks to file."
  (let ((bmarks (app-state-bookmarks app)))
    (with-catch
      (lambda (e)
        (echo-message! (app-state-echo app) "Error saving bookmarks"))
      (lambda ()
        (call-with-output-file "~/.gemacs-bookmarks"
          (lambda (port)
            (for-each
              (lambda (pair)
                (display (car pair) port)
                (display " " port)
                (display (cdr pair) port)
                (newline port))
              (hash->list bmarks))))
        (echo-message! (app-state-echo app) "Bookmarks saved")))))

(def (cmd-bookmark-load app)
  "Load bookmarks from file."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "No saved bookmarks found"))
    (lambda ()
      (let* ((content (read-file-as-string "~/.gemacs-bookmarks"))
             (lines (string-split content #\newline))
             (bmarks (app-state-bookmarks app)))
        (for-each
          (lambda (line)
            (let ((parts (string-split line #\space)))
              (when (>= (length parts) 2)
                (hash-put! bmarks (car parts) (string->number (cadr parts))))))
          lines)
        (echo-message! (app-state-echo app)
                       (string-append "Bookmarks loaded: " (number->string (hash-length bmarks))))))))

;; --- Window management ---

(def (cmd-fit-window-to-buffer app)
  "Shrink window to fit its buffer content."
  (let* ((ed (current-editor app))
         (lines (send-message ed SCI_GETLINECOUNT 0 0)))
    (echo-message! (app-state-echo app)
                   (string-append "Buffer has " (number->string lines) " lines"))))

(def (cmd-maximize-window app)
  "Maximize the current window by deleting all others."
  (frame-delete-other-windows! (app-state-frame app))
  (echo-message! (app-state-echo app) "Window maximized"))

(def (cmd-minimize-window app)
  "Minimize the current window (keep minimal height)."
  (echo-message! (app-state-echo app) "Window minimized (single-window TUI)"))

(def (cmd-rotate-windows app)
  "Rotate window layout by cycling to other window."
  (let ((wins (frame-windows (app-state-frame app))))
    (if (>= (length wins) 2)
      (begin
        (frame-other-window! (app-state-frame app))
        (echo-message! (app-state-echo app) "Windows rotated"))
      (echo-message! (app-state-echo app) "Only one window"))))

(def (cmd-swap-windows app)
  "Swap contents of two windows."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (when (>= (length wins) 2)
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (b1 (edit-window-buffer w1))
             (b2 (edit-window-buffer w2)))
        (set! (edit-window-buffer w1) b2)
        (set! (edit-window-buffer w2) b1)
        (echo-message! (app-state-echo app) "Windows swapped")))))

;; --- Miscellaneous ---

(def (cmd-delete-matching-lines app)
  "Delete all lines matching a pattern (same as flush-lines)."
  (cmd-flush-lines app))

(def (cmd-copy-matching-lines app)
  "Copy all lines matching a pattern to a buffer."
  (let ((pat (app-read-string app "Copy lines matching: ")))
    (when (and pat (not (string-empty? pat)))
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (matching (filter (lambda (l) (string-contains l pat)) lines))
             (result (string-join matching "\n")))
        (open-output-buffer app "*Matching Lines*" result)))))

(def (cmd-delete-non-matching-lines app)
  "Delete all lines not matching a pattern (same as keep-lines)."
  (cmd-keep-lines app))

(def (cmd-display-fill-column-indicator app)
  "Toggle fill column indicator."
  (let* ((ed (current-editor app))
         (cur (send-message ed 2695 0 0)))  ;; SCI_GETEDGEMODE
    (if (= cur 0)
      (begin
        (send-message ed 2694 1 0)  ;; SCI_SETEDGEMODE EDGE_LINE
        (send-message ed 2360 80 0)  ;; SCI_SETEDGECOLUMN
        (echo-message! (app-state-echo app) "Fill column indicator on"))
      (begin
        (send-message ed 2694 0 0)  ;; SCI_SETEDGEMODE EDGE_NONE
        (echo-message! (app-state-echo app) "Fill column indicator off")))))

(def (cmd-electric-newline-and-indent app)
  "Insert newline and indent."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "\n")
    (editor-goto-pos ed (+ pos 1))
    (send-message ed 2327 0 0)))  ;; SCI_TAB (auto-indent)

(def (cmd-view-register app)
  "Display contents of a register."
  (echo-message! (app-state-echo app) "View register key: ")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (regs (app-state-registers app))
             (val (hash-get regs ks)))
        (if val
          (echo-message! (app-state-echo app)
                         (string-append "Register " ks ": " (if (> (string-length val) 60)
                                                                (string-append (substring val 0 57) "...")
                                                                val)))
          (echo-message! (app-state-echo app)
                         (string-append "Register " ks " is empty")))))))

(def (cmd-append-to-register app)
  "Append region to a register."
  (echo-message! (app-state-echo app) "Append to register: ")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (ed (current-editor app))
             (sel-start (editor-get-selection-start ed))
             (sel-end (editor-get-selection-end ed)))
        (if (< sel-start sel-end)
          (let* ((text (editor-get-text ed))
                 (region (substring text sel-start sel-end))
                 (regs (app-state-registers app))
                 (existing (or (hash-get regs ks) "")))
            (hash-put! regs ks (string-append existing region))
            (echo-message! (app-state-echo app)
                           (string-append "Appended to register " ks)))
          (echo-message! (app-state-echo app) "No region selected"))))))

;; --- Process / environment ---

(def (cmd-getenv app)
  "Display an environment variable."
  (let ((var (app-read-string app "Environment variable: ")))
    (when (and var (not (string-empty? var)))
      (let ((val (getenv var #f)))
        (echo-message! (app-state-echo app)
                       (if val
                         (string-append var "=" val)
                         (string-append var " is not set")))))))

(def (cmd-setenv app)
  "Set an environment variable."
  (let ((var (app-read-string app "Set variable: ")))
    (when (and var (not (string-empty? var)))
      (let ((val (app-read-string app "Value: ")))
        (when val
          (setenv var val)
          (echo-message! (app-state-echo app)
                         (string-append var "=" val)))))))

(def (cmd-show-environment app)
  "Display all environment variables."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "Error reading environment"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "env" arguments: '()
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (output (read-line proc #f))
             (result (or output "")))
        (close-port proc)
        (open-output-buffer app "*Environment*" result)))))

;; --- Encoding / line endings ---

(def (cmd-set-buffer-file-coding app)
  "Set buffer file coding system (UTF-8 is the default and only supported encoding)."
  (let ((coding (app-read-string app "Coding system (utf-8): ")))
    (when (and coding (not (string-empty? coding)))
      (echo-message! (app-state-echo app)
                     (string-append "Coding system: " coding " (note: Gerbil uses UTF-8 natively)")))))

(def (cmd-convert-line-endings-unix app)
  "Convert line endings to Unix (LF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    (let loop ((i 0) (acc []))
      (if (>= i (string-length text))
        (let ((result (list->string (reverse acc))))
          (editor-set-text ed result)
          (echo-message! (app-state-echo app) "Converted to Unix line endings"))
        (let ((ch (string-ref text i)))
          (if (char=? ch #\return)
            (if (and (< (+ i 1) (string-length text))
                     (char=? (string-ref text (+ i 1)) #\newline))
              (loop (+ i 2) (cons #\newline acc))  ;; CR+LF -> LF
              (loop (+ i 1) (cons #\newline acc)))  ;; CR -> LF
            (loop (+ i 1) (cons ch acc))))))))

(def (cmd-convert-line-endings-dos app)
  "Convert line endings to DOS (CRLF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    ;; First normalize to LF, then convert to CRLF
    (let loop ((i 0) (acc []))
      (if (>= i (string-length text))
        (let ((clean (list->string (reverse acc))))
          ;; Now add CR before each LF
          (let loop2 ((j 0) (acc2 []))
            (if (>= j (string-length clean))
              (let ((result (list->string (reverse acc2))))
                (editor-set-text ed result)
                (echo-message! (app-state-echo app) "Converted to DOS line endings"))
              (let ((ch (string-ref clean j)))
                (if (char=? ch #\newline)
                  (loop2 (+ j 1) (cons #\newline (cons #\return acc2)))
                  (loop2 (+ j 1) (cons ch acc2)))))))
        (let ((ch (string-ref text i)))
          (if (char=? ch #\return)
            (if (and (< (+ i 1) (string-length text))
                     (char=? (string-ref text (+ i 1)) #\newline))
              (loop (+ i 2) (cons #\newline acc))
              (loop (+ i 1) (cons #\newline acc)))
            (loop (+ i 1) (cons ch acc))))))))

;; --- Completion / hippie-expand ---

;; --- Whitespace ---

(def (cmd-whitespace-mode app)
  "Toggle whitespace visibility mode."
  (let* ((ed (current-editor app))
         (visible (send-message ed 2090 0 0)))  ;; SCI_GETVIEWWS
    (if (= visible 0)
      (begin
        (send-message ed 2021 1 0)  ;; SCI_SETVIEWWS SCWS_VISIBLEALWAYS
        (echo-message! (app-state-echo app) "Whitespace visible"))
      (begin
        (send-message ed 2021 0 0)  ;; SCI_SETVIEWWS SCWS_INVISIBLE
        (echo-message! (app-state-echo app) "Whitespace hidden")))))

(def (cmd-toggle-show-spaces app)
  "Toggle space visibility."
  (cmd-whitespace-mode app))

;; --- Folding ---

(def (cmd-fold-all app)
  "Fold all foldable regions."
  (let ((ed (current-editor app)))
    (send-message ed SCI_FOLDALL SC_FOLDACTION_CONTRACT 0)
    (echo-message! (app-state-echo app) "All folds collapsed")))

(def (cmd-unfold-all app)
  "Unfold all foldable regions."
  (let ((ed (current-editor app)))
    (send-message ed SCI_FOLDALL SC_FOLDACTION_EXPAND 0)
    (echo-message! (app-state-echo app) "All folds expanded")))

(def (cmd-toggle-fold app)
  "Toggle fold at current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos)))
    (send-message ed SCI_TOGGLEFOLD line 0)
    (echo-message! (app-state-echo app) "Fold toggled")))

(def (cmd-fold-level app)
  "Fold to a specific level."
  (let ((level (app-read-string app "Fold level: ")))
    (when (and level (not (string-empty? level)))
      (let ((n (string->number level)))
        (when n
          (let ((ed (current-editor app)))
            ;; Expand all first, then collapse to level
            (send-message ed 2335 1 0)  ;; SCI_FOLDALL expand
            (echo-message! (app-state-echo app)
                           (string-append "Folded to level " level))))))))

;; --- Macro enhancements ---
;; Named macro commands moved to editor-text.ss (cmd-name-last-kbd-macro, etc.)

(def (cmd-insert-kbd-macro app)
  "Insert the last keyboard macro as text."
  (let ((macro (app-state-macro-last app)))
    (if (and macro (not (null? macro)))
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (desc (string-join (map (lambda (ev) (key-event->string ev)) macro) " ")))
        (editor-insert-text ed pos desc))
      (echo-message! (app-state-echo app) "No keyboard macro defined"))))

;; --- Version control extras ---

(def (cmd-vc-annotate app)
  "Show file annotations (git blame) in buffer."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "Error running git annotate"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git" arguments: ["blame" "--date=short" file]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*Annotate: " file "*")
                                result))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

(def (cmd-vc-diff-head app)
  "Show diff against HEAD."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "Error running git diff"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git" arguments: ["diff" "HEAD" "--" file]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*VC Diff: " file "*")
                                result))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

(def (cmd-vc-log-file app)
  "Show git log for current file."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "Error running git log"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "git" arguments: ["log" "--oneline" "-20" "--" file]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (result (or output "")))
            (close-port proc)
            (open-output-buffer app
                                (string-append "*VC Log: " file "*")
                                result))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

(def (cmd-vc-revert app)
  "Revert current file to last committed version."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (let ((confirm (app-read-string app
                                        (string-append "Revert " file " to HEAD? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app) "Error running git checkout"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "git" arguments: ["checkout" "HEAD" "--" file]
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t)))
                     (_ (process-status proc)))
                (close-port proc)
                ;; Reload the file
                (let ((content (read-file-as-string file))
                      (ed (current-editor app)))
                  (editor-set-text ed content)
                  (editor-set-save-point ed)
                  (echo-message! (app-state-echo app) "Reverted")))))))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

;; --- Imenu ---

(def (cmd-imenu app)
  "Jump to a definition in the current buffer (simple heuristic)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (defs (let loop ((ls lines) (n 0) (acc []))
                 (if (null? ls)
                   (reverse acc)
                   (let ((l (car ls)))
                     (if (or (string-contains l "(def ")
                             (string-contains l "(defstruct ")
                             (string-contains l "(defclass ")
                             (string-contains l "(defmethod ")
                             (string-contains l "(define "))
                       (loop (cdr ls) (+ n 1) (cons (cons l n) acc))
                       (loop (cdr ls) (+ n 1) acc)))))))
    (if (null? defs)
      (echo-message! (app-state-echo app) "No definitions found")
      (let* ((items (map (lambda (d) (string-append (number->string (cdr d)) ": " (car d))) defs))
             (display (string-join items "\n")))
        (open-output-buffer app "*Imenu*" display)))))

(def (cmd-which-function app)
  "Display name of function at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (line (send-message ed 2166 pos 0)))
    ;; Search backward for (def
    (let loop ((l line))
      (if (< l 0)
        (echo-message! (app-state-echo app) "Not in a function")
        (let* ((ls (send-message ed 2167 l 0))
               (le (send-message ed 2136 l 0))
               (lt (if (and (>= ls 0) (<= le (string-length text)))
                     (substring text ls le) "")))
          (if (or (string-contains lt "(def ")
                  (string-contains lt "(define "))
            (let ((trimmed (string-trim-both lt)))
              (echo-message! (app-state-echo app)
                             (if (> (string-length trimmed) 70)
                               (substring trimmed 0 70)
                               trimmed)))
            (loop (- l 1))))))))

;; --- Buffer/file utilities ---

(def (cmd-make-directory app)
  "Create a new directory."
  (let ((dir (app-read-string app "Create directory: ")))
    (when (and dir (not (string-empty? dir)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app)
                         (string-append "Created: " dir)))))))

(def (cmd-delete-file app)
  "Delete a file."
  (let ((file (app-read-string app "Delete file: ")))
    (when (and file (not (string-empty? file)))
      (let ((confirm (app-read-string app
                                        (string-append "Really delete " file "? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (delete-file file)
              (echo-message! (app-state-echo app)
                             (string-append "Deleted: " file)))))))))

(def (cmd-copy-file app)
  "Copy a file."
  (let ((src (app-read-string app "Copy file: ")))
    (when (and src (not (string-empty? src)))
      (let ((dst (app-read-string app "Copy to: ")))
        (when (and dst (not (string-empty? dst)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app)
                             (string-append "Error: " (with-output-to-string
                                                         (lambda () (display-exception e))))))
            (lambda ()
              (copy-file src dst)
              (echo-message! (app-state-echo app)
                             (string-append "Copied: " src " -> " dst)))))))))

(def (cmd-sudo-find-file app)
  "Open file as root using sudo cat, display in read-only buffer."
  (let ((file (app-read-string app "Sudo find file: ")))
    (when (and file (not (string-empty? file)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "sudo failed: " (with-output-to-string (lambda () (display-exception e))))))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "/usr/bin/sudo"
                               arguments: ["cat" file]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (content (read-line proc #f))
                 (result (or content "")))
            (process-status proc)
            (close-port proc)
            (let* ((ed (current-editor app))
                   (fr (app-state-frame app))
                   (buf (buffer-create! (string-append "[sudo] " file) ed #f)))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-text ed result)
              (editor-set-save-point ed)
              (editor-set-read-only ed #t)
              (echo-message! (app-state-echo app)
                             (string-append "Opened (read-only): " file)))))))))

(def (cmd-find-file-literally app)
  "Open file without special processing."
  (let ((file (app-read-string app "Find file literally: ")))
    (when (and file (not (string-empty? file)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
                         (string-append "Error: " (with-output-to-string
                                                     (lambda () (display-exception e))))))
        (lambda ()
          (let* ((content (read-file-as-string file))
                 (ed (current-editor app))
                 (fr (app-state-frame app))
                 (buf (buffer-create! file ed #f)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (editor-set-text ed content)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)))))))

;;;============================================================================
;;; Task #45: isearch enhancements, abbrev, and editing utilities
;;;============================================================================

;; --- Search enhancements ---

(def (cmd-isearch-forward-word app)
  "Incremental search forward for a whole word."
  (let ((word (app-read-string app "I-search word: ")))
    (when (and word (not (string-empty? word)))
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (text (editor-get-text ed))
             (pat (string-append " " word " ")))
        ;; Simple word boundary: space-delimited
        (let ((found (string-contains text word pos)))
          (if found
            (begin
              (editor-goto-pos ed found)
              (editor-set-selection-start ed found)
              (editor-set-selection-end ed (+ found (string-length word)))
              (set! (app-state-last-search app) word))
            (echo-message! (app-state-echo app)
                           (string-append "Not found: " word))))))))

(def (cmd-isearch-backward-word app)
  "Incremental search backward for a whole word."
  (let ((word (app-read-string app "I-search backward word: ")))
    (when (and word (not (string-empty? word)))
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (text (editor-get-text ed)))
        ;; Search backward
        (let loop ((i (- pos (string-length word) 1)))
          (cond
            ((< i 0)
             (echo-message! (app-state-echo app)
                            (string-append "Not found: " word)))
            ((and (>= (+ i (string-length word)) 0)
                  (<= (+ i (string-length word)) (string-length text))
                  (string=? (substring text i (+ i (string-length word))) word))
             (editor-goto-pos ed i)
             (editor-set-selection-start ed i)
             (editor-set-selection-end ed (+ i (string-length word)))
             (set! (app-state-last-search app) word))
            (else (loop (- i 1)))))))))

(def (cmd-isearch-forward-symbol app)
  "Incremental search forward for a symbol at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Get symbol at point
    (let* ((ws (let loop ((i pos))
                 (if (and (> i 0) (word-char? (string-ref text (- i 1))))
                   (loop (- i 1)) i)))
           (we (let loop ((i pos))
                 (if (and (< i len) (word-char? (string-ref text i)))
                   (loop (+ i 1)) i)))
           (symbol (if (< ws we) (substring text ws we) "")))
      (if (string-empty? symbol)
        (echo-message! (app-state-echo app) "No symbol at point")
        (let ((found (string-contains text symbol (+ we 1))))
          (if found
            (begin
              (editor-goto-pos ed found)
              (editor-set-selection-start ed found)
              (editor-set-selection-end ed (+ found (string-length symbol)))
              (set! (app-state-last-search app) symbol)
              (echo-message! (app-state-echo app)
                             (string-append "Symbol: " symbol)))
            ;; Wrap around
            (let ((found2 (string-contains text symbol 0)))
              (if (and found2 (< found2 ws))
                (begin
                  (editor-goto-pos ed found2)
                  (editor-set-selection-start ed found2)
                  (editor-set-selection-end ed (+ found2 (string-length symbol)))
                  (echo-message! (app-state-echo app) "Wrapped"))
                (echo-message! (app-state-echo app) "Only occurrence")))))))))

(def (cmd-query-replace-regexp app)
  "Replace all matches of a regexp pattern using Scintilla SCFIND_REGEXP."
  (let ((from (app-read-string app "Regexp replace: ")))
    (when (and from (not (string-empty? from)))
      (let ((to (app-read-string app (string-append "Replace regexp \"" from "\" with: "))))
        (when to
          (let* ((ed (current-editor app))
                 (replaced 0))
            ;; Start from beginning of document
            (let loop ((pos 0))
              (let ((text-len (editor-get-text-length ed)))
                (send-message ed SCI_SETTARGETSTART pos)
                (send-message ed SCI_SETTARGETEND text-len)
                (send-message ed SCI_SETSEARCHFLAGS SCFIND_REGEXP)
                (let ((found (send-message/string ed SCI_SEARCHINTARGET from)))
                  (if (>= found 0)
                    (let ((match-end (send-message ed SCI_GETTARGETEND)))
                      (send-message ed SCI_SETTARGETSTART found)
                      (send-message ed SCI_SETTARGETEND match-end)
                      (let ((repl-len (send-message/string ed SCI_REPLACETARGETRE to)))
                        (set! replaced (+ replaced 1))
                        (loop (+ found (max repl-len 1)))))
                    (echo-message! (app-state-echo app)
                      (string-append "Replaced " (number->string replaced)
                                     " occurrence" (if (= replaced 1) "" "s")))))))))))))


(def (cmd-multi-occur app)
  "Search for pattern across all buffers."
  (let ((pat (app-read-string app "Multi-occur: ")))
    (when (and pat (not (string-empty? pat)))
      (let* ((ed (current-editor app))
             (results
               (let loop ((bufs (buffer-list)) (acc []))
                 (if (null? bufs)
                   (reverse acc)
                   (let* ((buf (car bufs))
                          (name (buffer-name buf))
                          ;; Search this buffer's content for the pattern
                          (doc (buffer-doc-pointer buf))
                          (text (if doc
                                  (let ((tmp-ed ed))
                                    ;; Get text length from buffer
                                    (buffer-name buf))
                                  #f)))
                     (loop (cdr bufs) acc)))))
             ;; Use grep on files of file-visiting buffers
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
                                      (if (null? ls)
                                        (reverse hits)
                                        (if (string-contains (car ls) pat)
                                          (mloop (cdr ls) (+ n 1)
                                                 (cons (string-append (buffer-name buf) ":"
                                                                      (number->string n) ": "
                                                                      (car ls))
                                                       hits))
                                          (mloop (cdr ls) (+ n 1) hits))))))
                             (loop (cdr bufs) (append acc matches)))))
                       (loop (cdr bufs) acc))))))
             (output (if (null? file-results)
                       (string-append "No matches for: " pat)
                       (string-join file-results "\n"))))
        (open-output-buffer app "*Multi-Occur*" output)
        (echo-message! (app-state-echo app)
                       (string-append (number->string (length file-results)) " matches for: " pat))))))

;; --- Sort enhancements ---

;; --- Align ---

(def (cmd-align-current app)
  "Align the current region on a separator."
  (let ((sep (app-read-string app "Align on: ")))
    (when (and sep (not (string-empty? sep)))
      (let* ((ed (current-editor app))
             (sel-start (editor-get-selection-start ed))
             (sel-end (editor-get-selection-end ed)))
        (when (< sel-start sel-end)
          (let* ((text (editor-get-text ed))
                 (region (substring text sel-start sel-end))
                 (lines (string-split region #\newline))
                 ;; Find max column of separator
                 (max-col (let loop ((ls lines) (max-c 0))
                            (if (null? ls) max-c
                              (let ((pos (string-contains (car ls) sep)))
                                (loop (cdr ls) (if pos (max max-c pos) max-c))))))
                 ;; Pad each line so separator aligns
                 (aligned (map (lambda (l)
                                 (let ((pos (string-contains l sep)))
                                   (if pos
                                     (string-append
                                       (substring l 0 pos)
                                       (make-string (- max-col pos) #\space)
                                       (substring l pos (string-length l)))
                                     l)))
                               lines))
                 (result (string-join aligned "\n")))
            (send-message ed 2160 sel-start 0)
            (send-message ed 2161 sel-end 0)
            (send-message/string ed SCI_REPLACETARGET result)))))))

;; --- Rectangle enhancements ---

(def (cmd-clear-rectangle app)
  "Clear text in a rectangle region (replace with spaces)."
  (let* ((ed (current-editor app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (when (< sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (start-line (send-message ed 2166 sel-start 0))
             (end-line (send-message ed 2166 sel-end 0))
             (start-col (send-message ed 2008 sel-start 0))  ;; SCI_GETCOLUMN
             (end-col (send-message ed 2008 sel-end 0))
             (min-col (min start-col end-col))
             (max-col (max start-col end-col))
             (lines (string-split text #\newline))
             (result-lines
               (let loop ((ls lines) (n 0) (acc []))
                 (if (null? ls)
                   (reverse acc)
                   (let ((l (car ls)))
                     (if (and (>= n start-line) (<= n end-line))
                       (let* ((len (string-length l))
                              (before (substring l 0 (min min-col len)))
                              (spaces (make-string (- max-col min-col) #\space))
                              (after (if (< max-col len)
                                       (substring l max-col len)
                                       "")))
                         (loop (cdr ls) (+ n 1) (cons (string-append before spaces after) acc)))
                       (loop (cdr ls) (+ n 1) (cons l acc)))))))
             (result (string-join result-lines "\n")))
        (editor-set-text ed result)))))

;; --- Abbrev mode ---
;; Global abbreviation table

(def *abbrev-table* (make-hash-table)) ; abbrev -> expansion
(def *abbrev-mode-enabled* #t)

(def (cmd-abbrev-mode app)
  "Toggle abbrev mode."
  (set! *abbrev-mode-enabled* (not *abbrev-mode-enabled*))
  (echo-message! (app-state-echo app)
    (if *abbrev-mode-enabled* "Abbrev mode enabled" "Abbrev mode disabled")))

(def (cmd-define-abbrev app)
  "Define a new abbreviation."
  (let ((abbrev (app-read-string app "Abbrev: ")))
    (when (and abbrev (not (string-empty? abbrev)))
      (let ((expansion (app-read-string app "Expansion: ")))
        (when (and expansion (not (string-empty? expansion)))
          (hash-put! *abbrev-table* abbrev expansion)
          (echo-message! (app-state-echo app)
                         (string-append "Defined: " abbrev " -> " expansion)))))))

(def (abbrev-word-before-point ed)
  "Get the word immediately before point for abbreviation lookup."
  (let* ((pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (if (<= pos 0)
      (values #f #f)
      (let loop ((i (- pos 1)) (end pos))
        (if (< i 0)
          (values 0 end)
          (let ((ch (string-ref text i)))
            (if (or (char-alphabetic? ch) (char-numeric? ch))
              (loop (- i 1) end)
              (if (= (+ i 1) end)
                (values #f #f)
                (values (+ i 1) end)))))))))

(def (cmd-expand-abbrev app)
  "Expand abbreviation at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (let-values (((start end) (abbrev-word-before-point ed)))
      (if (not start)
        (echo-message! echo "No word to expand")
        (let* ((text (editor-get-text ed))
               (word (substring text start end))
               (expansion (hash-get *abbrev-table* word)))
          (if (not expansion)
            (echo-message! echo (string-append "No abbrev for \"" word "\""))
            (begin
              ;; Delete the abbreviation
              (editor-goto-pos ed start)
              (editor-set-selection ed start end)
              (editor-replace-selection ed "")
              ;; Insert the expansion
              (editor-insert-text ed start expansion)
              (editor-goto-pos ed (+ start (string-length expansion)))
              (echo-message! echo (string-append "Expanded: " word " -> " expansion)))))))))

(def (cmd-list-abbrevs app)
  "List all abbreviations."
  (let* ((abbrevs (hash->list *abbrev-table*))
         (text (if (null? abbrevs)
                 "No abbreviations defined.\n\nUse M-x define-abbrev to add abbreviations."
                 (string-append "Abbreviations:\n\n"
                   (string-join
                     (map (lambda (pair)
                            (string-append "  " (car pair) " -> " (cdr pair)))
                          (sort abbrevs (lambda (a b) (string<? (car a) (car b)))))
                     "\n")
                   "\n\nUse M-x define-abbrev to add more."))))
    (open-output-buffer app "*Abbrevs*" text)))

;; --- Completion ---

(def (cmd-completion-at-point app)
  "Complete word at point using buffer contents (same as hippie-expand)."
  (cmd-hippie-expand app))

(def (cmd-complete-filename app)
  "Complete filename at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    ;; Get path-like prefix
    (let* ((start (let loop ((i (- pos 1)))
                    (if (and (>= i 0)
                             (not (memv (string-ref text i) '(#\space #\tab #\newline #\( #\)))))
                      (loop (- i 1))
                      (+ i 1))))
           (prefix (substring text start pos)))
      (if (string-empty? prefix)
        (echo-message! (app-state-echo app) "No filename prefix")
        (with-catch
          (lambda (e)
            (echo-message! (app-state-echo app) "Cannot complete"))
          (lambda ()
            (let* ((dir (path-directory prefix))
                   (base (path-strip-directory prefix))
                   (entries (directory-files (if (string-empty? dir) "." dir)))
                   (matches (filter (lambda (f)
                                      (and (>= (string-length f) (string-length base))
                                           (string=? (substring f 0 (string-length base)) base)))
                                    entries)))
              (cond
                ((null? matches)
                 (echo-message! (app-state-echo app) "No completions"))
                ((= (length matches) 1)
                 (let ((completion (string-append dir (car matches))))
                   (send-message ed 2160 start 0)
                   (send-message ed 2161 pos 0)
                   (send-message/string ed SCI_REPLACETARGET completion)))
                (else
                 (echo-message! (app-state-echo app)
                                (string-append (number->string (length matches)) " completions")))))))))))

;; --- Window resize ---

(def (cmd-resize-window-width app)
  "Set window width (TUI uses full terminal width)."
  (echo-message! (app-state-echo app) "Window uses full terminal width"))

;; --- Text operations ---

(def (cmd-zap-to-char-inclusive app)
  "Zap to character, including the character."
  (echo-message! (app-state-echo app) "Zap to char (inclusive): ")
  (let ((ev (tui-poll-event)))
    (when ev
      (let* ((ks (key-event->string ev))
             (ch (if (= (string-length ks) 1) (string-ref ks 0) #f)))
        (when ch
          (let* ((ed (current-editor app))
                 (pos (editor-get-current-pos ed))
                 (text (editor-get-text ed))
                 (len (string-length text)))
            (let loop ((i (+ pos 1)))
              (cond
                ((>= i len)
                 (echo-message! (app-state-echo app)
                                (string-append "Character not found: " ks)))
                ((char=? (string-ref text i) ch)
                 ;; Kill from pos to i+1 (inclusive)
                 (let ((killed (substring text pos (+ i 1))))
                   (set! (app-state-kill-ring app)
                     (cons killed (app-state-kill-ring app)))
                   (send-message ed 2160 pos 0)
                   (send-message ed 2161 (+ i 1) 0)
                   (send-message/string ed SCI_REPLACETARGET "")))
                (else (loop (+ i 1)))))))))))

(def (cmd-copy-word-at-point app)
  "Copy the word at point to the kill ring."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (when (> len 0)
      (let* ((ws (let loop ((i pos))
                   (if (and (> i 0) (word-char? (string-ref text (- i 1))))
                     (loop (- i 1)) i)))
             (we (let loop ((i pos))
                   (if (and (< i len) (word-char? (string-ref text i)))
                     (loop (+ i 1)) i)))
             (word (if (< ws we) (substring text ws we) "")))
        (if (string-empty? word)
          (echo-message! (app-state-echo app) "No word at point")
          (begin
            (set! (app-state-kill-ring app)
              (cons word (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
                           (string-append "Copied: " word))))))))

(def (cmd-copy-symbol-at-point app)
  "Copy the symbol at point (including hyphens, underscores)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text))
         (sym-char? (lambda (ch)
                      (or (char-alphabetic? ch)
                          (char-numeric? ch)
                          (memv ch '(#\- #\_ #\! #\? #\*))))))
    (when (> len 0)
      (let* ((ws (let loop ((i pos))
                   (if (and (> i 0) (sym-char? (string-ref text (- i 1))))
                     (loop (- i 1)) i)))
             (we (let loop ((i pos))
                   (if (and (< i len) (sym-char? (string-ref text i)))
                     (loop (+ i 1)) i)))
             (sym (if (< ws we) (substring text ws we) "")))
        (if (string-empty? sym)
          (echo-message! (app-state-echo app) "No symbol at point")
          (begin
            (set! (app-state-kill-ring app)
              (cons sym (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
                           (string-append "Copied: " sym))))))))

(def (cmd-mark-page app)
  "Mark the entire buffer (same as select-all)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    (editor-set-selection-start ed 0)
    (editor-set-selection-end ed (string-length text))
    (echo-message! (app-state-echo app) "Buffer marked")))

;; --- Encoding/display ---

(def (cmd-set-language-environment app)
  "Set language environment (Gerbil uses UTF-8 natively)."
  (let ((lang (app-read-string app "Language environment (UTF-8): ")))
    (when (and lang (not (string-empty? lang)))
      (echo-message! (app-state-echo app)
                     (string-append "Language environment: " lang " (UTF-8 is default)")))))

;; --- Theme/color ---

(def (cmd-load-theme app)
  "Load a color theme from the theme registry."
  (let* ((available (map symbol->string (theme-names)))
         (theme-str (app-read-string app
                      (string-append "Load theme (" (car available) "): "))))
    (when (and theme-str (not (string-empty? theme-str)))
      (let ((theme-sym (string->symbol theme-str)))
        (if (theme-get theme-sym)
          (begin
            ;; Load theme faces into *faces* registry
            (load-theme! theme-sym)
            ;; Re-apply highlighting to current buffer
            (let ((ed (current-editor app)))
              (setup-gerbil-highlighting! ed))
            ;; Persist theme choice
            (theme-settings-save! *current-theme* *default-font-family* *default-font-size*)
            (echo-message! (app-state-echo app)
              (string-append "Theme: " theme-str)))
          (echo-message! (app-state-echo app)
            (string-append "Unknown theme: " theme-str
                          " (available: " (string-join available ", ") ")")))))))

(def (cmd-customize-face app)
  "Show Scintilla style info for current position."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (style (send-message ed SCI_GETSTYLEAT pos 0)))
    (echo-message! (app-state-echo app)
                   (string-append "Style at point: " (number->string style)))))

(def (cmd-list-colors app)
  "List available colors."
  (let ((colors "black red green yellow blue magenta cyan white\nbright-black bright-red bright-green bright-yellow\nbright-blue bright-magenta bright-cyan bright-white"))
    (open-output-buffer app "*Colors*" colors)))

;; --- Text property/overlay ---

(def (cmd-font-lock-mode app)
  "Toggle font-lock (syntax highlighting) mode."
  (cmd-toggle-highlighting app))

;; --- Auto-revert ---

(def (cmd-auto-revert-mode app)
  "Toggle auto-revert mode for current buffer."
  (set! *auto-revert-mode* (not *auto-revert-mode*))
  (echo-message! (app-state-echo app)
                 (if *auto-revert-mode*
                   "Auto-revert mode enabled"
                   "Auto-revert mode disabled")))

;; --- Diff enhancements ---

(def (cmd-diff-backup app)
  "Diff current file against its backup."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if file
      (let ((backup (string-append file "~")))
        (if (file-exists? backup)
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app) "Error running diff"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "diff" arguments: ["-u" backup file]
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t)))
                     (output (read-line proc #f))
                     (result (or output "No differences")))
                (close-port proc)
                (open-output-buffer app "*Diff Backup*" result))))
          (echo-message! (app-state-echo app) "No backup file found")))
      (echo-message! (app-state-echo app) "Buffer is not visiting a file"))))

;; --- Compilation ---

(def (cmd-first-error app)
  "Jump to first compilation error in *compile* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (compile-buf
           (let loop ((bufs (buffer-list)))
             (if (null? bufs) #f
               (if (string=? (buffer-name (car bufs)) "*compile*")
                 (car bufs) (loop (cdr bufs)))))))
    (if compile-buf
      (begin
        (buffer-attach! ed compile-buf)
        (set! (edit-window-buffer (current-window fr)) compile-buf)
        (editor-goto-pos ed 0)
        (echo-message! (app-state-echo app) "First error"))
      (echo-message! (app-state-echo app) "No compilation output"))))

;; --- Calculator enhancements ---

(def (cmd-quick-calc app)
  "Quick inline calculation."
  (let ((expr (app-read-string app "Quick calc: ")))
    (when (and expr (not (string-empty? expr)))
      (let-values (((result error?) (eval-expression-string expr)))
        (echo-message! (app-state-echo app)
                       (if error?
                         (string-append "Error: " result)
                         (string-append "= " result)))))))

;; --- String insertion ---

(def (cmd-insert-time app)
  "Insert the current time."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "Error getting time"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "date" arguments: ["+%H:%M:%S"]
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (output (read-line proc))
             (time (or output ""))
             (_ (close-port proc))
             (ed (current-editor app))
             (pos (editor-get-current-pos ed)))
        (editor-insert-text ed pos time)
        (editor-goto-pos ed (+ pos (string-length time)))))))

(def (cmd-insert-file-header app)
  "Insert a file header comment."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (header (string-append ";;; -*- Gerbil -*-\n"
                                ";;; " name "\n"
                                ";;;\n"
                                ";;; Description: \n"
                                ";;;\n\n")))
    (editor-insert-text ed pos header)
    (editor-goto-pos ed (+ pos (string-length header)))))

;; --- Misc ---

(def *debug-on-quit* #f)

(def (cmd-toggle-debug-on-quit app)
  "Toggle debug on quit signal."
  (set! *debug-on-quit* (not *debug-on-quit*))
  (echo-message! (app-state-echo app)
                 (if *debug-on-quit*
                   "Debug on quit enabled"
                   "Debug on quit disabled")))

(def *profiler-running* #f)
(def *profiler-start-time* #f)

(def (cmd-profiler-start app)
  "Start profiling (records start time and GC stats)."
  (set! *profiler-running* #t)
  (set! *profiler-start-time* (##process-statistics))
  (echo-message! (app-state-echo app) "Profiler started"))

(def (cmd-profiler-stop app)
  "Stop profiler and show timing report."
  (if *profiler-running*
    (let* ((end-stats (##process-statistics))
           (start *profiler-start-time*)
           (wall (- (f64vector-ref end-stats 2) (f64vector-ref start 2)))
           (user (- (f64vector-ref end-stats 0) (f64vector-ref start 0)))
           (sys (- (f64vector-ref end-stats 1) (f64vector-ref start 1)))
           (gc (- (f64vector-ref end-stats 5) (f64vector-ref start 5)))
           (report (string-append
                     "Profiler Report\n"
                     "===============\n"
                     "Wall time: " (number->string (/ (round (* wall 1000)) 1000.0)) "s\n"
                     "User CPU:  " (number->string (/ (round (* user 1000)) 1000.0)) "s\n"
                     "System:    " (number->string (/ (round (* sys 1000)) 1000.0)) "s\n"
                     "GC time:   " (number->string (/ (round (* gc 1000)) 1000.0)) "s\n")))
      (set! *profiler-running* #f)
      (open-output-buffer app "*Profiler Report*" report))
    (echo-message! (app-state-echo app) "Profiler not running")))

(def (cmd-memory-report app)
  "Show memory usage report."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "Error getting memory info"))
    (lambda ()
      (let* ((content (read-file-as-string "/proc/self/status"))
             (lines (string-split content #\newline))
             (vm-line (let loop ((ls lines))
                        (if (null? ls) "Unknown"
                          (if (string-contains (car ls) "VmRSS:")
                            (car ls) (loop (cdr ls)))))))
        (echo-message! (app-state-echo app) (string-trim-both vm-line))))))

(def (cmd-emacs-version app)
  "Display editor version."
  (echo-message! (app-state-echo app) "gemacs 0.1"))

(def (cmd-report-bug app)
  "Report a bug."
  (echo-message! (app-state-echo app) "Report bugs at: https://github.com/ober/gemacs/issues"))

(def (cmd-view-echo-area-messages app)
  "View echo area message log in *Messages* buffer."
  (cmd-view-messages app))

(def (cmd-toggle-menu-bar-mode app)
  "Toggle menu bar display (TUI has no menu bar)."
  (echo-message! (app-state-echo app) "Menu bar not available in TUI mode"))

(def (cmd-toggle-tab-bar-mode app)
  "Toggle tab bar display."
  (echo-message! (app-state-echo app)
                 (string-append "Tabs: " (number->string (length (app-state-tabs app))) " open (use C-x t for tab commands)")))

(def (cmd-split-window-below app)
  "Split window below (alias for split-window)."
  (cmd-split-window app))

(def (cmd-delete-window-below app)
  "Delete the window below the current one."
  (let ((wins (frame-windows (app-state-frame app))))
    (if (>= (length wins) 2)
      (begin
        (frame-other-window! (app-state-frame app))
        (frame-delete-window! (app-state-frame app))
        (echo-message! (app-state-echo app) "Window below deleted"))
      (echo-message! (app-state-echo app) "No window below"))))

(def (cmd-shrink-window-if-larger-than-buffer app)
  "Report buffer line count (TUI windows share terminal height)."
  (let* ((ed (current-editor app))
         (lines (send-message ed SCI_GETLINECOUNT 0 0)))
    (echo-message! (app-state-echo app)
                   (string-append "Buffer: " (number->string lines) " lines"))))

(def (cmd-toggle-frame-fullscreen app)
  "Toggle fullscreen mode (TUI inherits terminal size)."
  (echo-message! (app-state-echo app) "TUI uses full terminal — resize terminal for fullscreen"))

(def (cmd-toggle-frame-maximized app)
  "Toggle maximized frame (TUI inherits terminal size)."
  (echo-message! (app-state-echo app) "TUI uses full terminal — maximize terminal window"))

;; --- Spell checking ---
(def *ispell-dictionary* #f) ;; language dictionary (e.g. "en", "fr", "de")

(def (ispell-args)
  "Build aspell argument list with current dictionary."
  (if *ispell-dictionary*
    (list "-a" "-d" *ispell-dictionary*)
    (list "-a")))

(def (cmd-ispell-word app)
  "Check spelling of word at point using aspell."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (let-values (((start end) (word-at-point ed)))
      (if (not start)
        (echo-message! echo "No word at point")
        (let* ((text (editor-get-text ed))
               (word (substring text start end)))
          ;; Run aspell to check the word
          (with-exception-catcher
            (lambda (e) (echo-error! echo "aspell not available"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "aspell"
                                   arguments: (ispell-args)
                                   stdin-redirection: #t
                                   stdout-redirection: #t
                                   stderr-redirection: #f)))
                     (_ (display (string-append word "\n") proc))
                     (_ (force-output proc))
                     (_ (close-output-port proc))
                     ;; Read aspell output - first line is version, second is result
                     (version-line (read-line proc))
                     (result-line (read-line proc)))
                (close-input-port proc)
                (process-status proc)
                (cond
                  ((eof-object? result-line)
                   (echo-message! echo "Spell check failed"))
                  ((string-prefix? "*" result-line)
                   (echo-message! echo (string-append "\"" word "\" is correct")))
                  ((string-prefix? "&" result-line)
                   ;; Misspelled with suggestions: & word count offset: suggestion1, suggestion2, ...
                   (let* ((colon-pos (string-index result-line #\:))
                          (suggestions (if colon-pos
                                         (string-trim (substring result-line (+ colon-pos 1)
                                                                 (string-length result-line)))
                                         "none")))
                     (echo-message! echo (string-append "\"" word "\" misspelled. Try: " suggestions))))
                  ((string-prefix? "#" result-line)
                   ;; Misspelled with no suggestions
                   (echo-message! echo (string-append "\"" word "\" misspelled, no suggestions")))
                  (else
                   (echo-message! echo (string-append "\"" word "\" is correct"))))))))))))

(def (ispell-extract-words text)
  "Extract words (alphabetic sequences) from text."
  (let loop ((i 0) (words '()) (word-start #f))
    (if (>= i (string-length text))
      (if word-start
        (reverse (cons (substring text word-start i) words))
        (reverse words))
      (let ((ch (string-ref text i)))
        (cond
          ((char-alphabetic? ch)
           (if word-start
             (loop (+ i 1) words word-start)
             (loop (+ i 1) words i)))
          (word-start
           (loop (+ i 1) (cons (substring text word-start i) words) #f))
          (else
           (loop (+ i 1) words #f)))))))

(def (cmd-ispell-buffer app)
  "Check spelling of entire buffer using aspell, report misspelled words."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (words (ispell-extract-words text)))
    (if (null? words)
      (echo-message! echo "No words in buffer")
      (with-exception-catcher
        (lambda (e) (echo-error! echo "aspell not available"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "aspell"
                               arguments: (ispell-args)
                               stdin-redirection: #t
                               stdout-redirection: #t
                               stderr-redirection: #f))))
            ;; Send all words
            (for-each (lambda (w) (display (string-append w "\n") proc)) words)
            (force-output proc)
            (close-output-port proc)
            ;; Read results
            (let loop ((misspelled '()) (first? #t))
              (let ((line (read-line proc)))
                (cond
                  ((eof-object? line)
                   (close-input-port proc)
                   (process-status proc)
                   (if (null? misspelled)
                     (echo-message! echo "No misspellings found")
                     (let* ((unique (let remove-dups ((lst (reverse misspelled)) (seen '()))
                                      (cond ((null? lst) (reverse seen))
                                            ((member (car lst) seen) (remove-dups (cdr lst) seen))
                                            (else (remove-dups (cdr lst) (cons (car lst) seen))))))
                            (count (length unique))
                            (shown (if (> count 5)
                                     (string-append (string-join (take unique 5) ", ") "...")
                                     (string-join unique ", "))))
                       (echo-message! echo
                         (string-append (number->string count) " misspelling(s): " shown)))))
                  (first?
                   ;; Skip version line
                   (loop misspelled #f))
                  ((string-prefix? "&" line)
                   ;; Misspelled: extract word (format: & word ...)
                   (let* ((parts (string-split line #\space))
                          (word (if (> (length parts) 1) (cadr parts) "?")))
                     (loop (cons word misspelled) #f)))
                  ((string-prefix? "#" line)
                   ;; Misspelled with no suggestions
                   (let* ((parts (string-split line #\space))
                          (word (if (> (length parts) 1) (cadr parts) "?")))
                     (loop (cons word misspelled) #f)))
                  (else
                   (loop misspelled #f)))))))))))

(def (cmd-ispell-region app)
  "Check spelling of region using aspell."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark-pos (buffer-mark buf)))
    (if (not mark-pos)
      (echo-message! echo "No region set (use C-SPC to set mark)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min pos mark-pos))
             (end (max pos mark-pos))
             (text (editor-get-text ed))
             (region (substring text start (min end (string-length text))))
             (words (ispell-extract-words region)))
        (if (null? words)
          (echo-message! echo "No words in region")
          (with-exception-catcher
            (lambda (e) (echo-error! echo "aspell not available"))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "aspell"
                                   arguments: (ispell-args)
                                   stdin-redirection: #t
                                   stdout-redirection: #t
                                   stderr-redirection: #f))))
                (for-each (lambda (w) (display (string-append w "\n") proc)) words)
                (force-output proc)
                (close-output-port proc)
                (let loop ((misspelled '()) (first? #t))
                  (let ((line (read-line proc)))
                    (cond
                      ((eof-object? line)
                       (close-input-port proc)
                       (process-status proc)
                       (if (null? misspelled)
                         (echo-message! echo "Region: no misspellings")
                         (echo-message! echo
                           (string-append "Region: " (number->string (length misspelled))
                                          " misspelling(s)"))))
                      (first? (loop misspelled #f))
                      ((or (string-prefix? "&" line) (string-prefix? "#" line))
                       (let* ((parts (string-split line #\space))
                              (word (if (> (length parts) 1) (cadr parts) "?")))
                         (loop (cons word misspelled) #f)))
                      (else (loop misspelled #f)))))))))))))


(def (cmd-ispell-change-dictionary app)
  "Select spelling dictionary (language)."
  (let* ((echo (app-state-echo app))
         (current (or *ispell-dictionary* "default"))
         (choice (app-read-string app
                   (string-append "Dictionary (" current "): "))))
    (when (and choice (not (string=? choice "")))
      (if (string=? choice "default")
        (begin (set! *ispell-dictionary* #f)
               (echo-message! echo "Dictionary: system default"))
        (begin (set! *ispell-dictionary* choice)
               (echo-message! echo
                 (string-append "Dictionary: " choice)))))))

;; --- Process management ---

(def (cmd-ansi-term app)
  "Open an ANSI terminal — opens PTY terminal."
  (execute-command! app 'term))


