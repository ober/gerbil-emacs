;;; -*- Gerbil -*-
;;; Advanced commands: misc navigation, where-is, apropos,
;;; universal argument, text transforms, hex dump, diff,
;;; checksum, eval buffer, ediff, calculator, modes, hippie expand

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
        :gemacs/editor-ui
        :gemacs/editor-text)

;;;============================================================================
;;; Misc navigation commands
;;;============================================================================

(def (cmd-exchange-point-and-mark app)
  "Swap point and mark positions."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No mark set")
      (let ((pos (editor-get-current-pos ed)))
        (set! (buffer-mark buf) pos)
        (editor-goto-pos ed mark)
        (echo-message! (app-state-echo app) "Mark and point exchanged")))))

(def (cmd-mark-whole-buffer app)
  "Mark the whole buffer (C-x h already does select-all, this is an alias)."
  (cmd-select-all app))

(def (cmd-recenter-top-bottom app)
  "Recenter display with point at center/top/bottom."
  ;; Simplified: just recenter
  (editor-scroll-caret (current-editor app)))

(def (cmd-what-page app)
  "Display what page and line the cursor is on."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (line (editor-line-from-position ed pos))
         ;; Count form feeds (page breaks) before point
         (pages
           (let loop ((i 0) (count 1))
             (if (>= i pos) count
               (if (char=? (string-ref text i) #\page)
                 (loop (+ i 1) (+ count 1))
                 (loop (+ i 1) count))))))
    (echo-message! (app-state-echo app)
      (string-append "Page " (number->string pages)
                     ", Line " (number->string (+ line 1))))))

(def (cmd-count-lines-region app)
  "Count lines in the region."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (lines (+ (- end-line start-line) 1))
             (chars (- end start)))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string lines)
                         " lines, " (number->string chars) " chars"))))))

(def (cmd-copy-line app)
  "Copy the current line to the kill ring without deleting."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (text (editor-get-text ed))
         (total (editor-get-line-count ed))
         ;; Include newline if not last line
         (end (if (< (+ line 1) total)
                (editor-position-from-line ed (+ line 1))
                line-end))
         (line-text (substring text line-start end)))
    (set! (app-state-kill-ring app)
      (cons line-text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) "Line copied")))

;;;============================================================================
;;; Help: where-is, apropos-command
;;;============================================================================

(def (cmd-where-is app)
  "Show what key a command is bound to."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Where is command: " row width)))
    (if (not input)
      (echo-message! echo "Cancelled")
      (let* ((cmd-name (string->symbol input))
             ;; Search all keymaps for this command
             (found '()))
        ;; Search global keymap
        (for-each
          (lambda (entry)
            (let ((key (car entry))
                  (val (cdr entry)))
              (cond
                ((eq? val cmd-name)
                 (set! found (cons key found)))
                ((hash-table? val)
                 ;; Search prefix map
                 (for-each
                   (lambda (sub)
                     (when (eq? (cdr sub) cmd-name)
                       (set! found (cons (string-append key " " (car sub)) found))))
                   (keymap-entries val))))))
          (keymap-entries *global-keymap*))
        (if (null? found)
          (echo-message! echo (string-append input " is not on any key"))
          (echo-message! echo
            (string-append input " is on "
                           (string-join (reverse found) ", "))))))))

(def (cmd-apropos-command app)
  "Search commands by name substring."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Apropos command: " row width)))
    (if (not input)
      (echo-message! echo "Cancelled")
      (let ((matches '()))
        ;; Search all registered commands
        (hash-for-each
          (lambda (name _proc)
            (when (string-contains (symbol->string name) input)
              (set! matches (cons (symbol->string name) matches))))
          *all-commands*)
        (if (null? matches)
          (echo-message! echo (string-append "No commands matching '" input "'"))
          (let* ((sorted (sort matches string<?))
                 (text (string-append "Commands matching '" input "':\n\n"
                                      (string-join sorted "\n") "\n")))
            ;; Show in *Help* buffer
            (let* ((ed (current-editor app))
                   (buf (or (buffer-by-name "*Help*")
                            (buffer-create! "*Help*" ed #f))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-text ed text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (echo-message! echo
                (string-append (number->string (length sorted))
                               " commands match")))))))))

;;;============================================================================
;;; Buffer: toggle-read-only, rename-buffer
;;;============================================================================

(def (cmd-toggle-read-only app)
  "Toggle the read-only state of the current buffer."
  (let* ((ed (current-editor app))
         (readonly? (editor-get-read-only? ed)))
    (editor-set-read-only ed (not readonly?))
    (echo-message! (app-state-echo app)
      (if readonly? "Buffer is now writable" "Buffer is now read-only"))))

(def (cmd-rename-buffer app)
  "Rename the current buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (old-name (buffer-name buf))
         (new-name (echo-read-string echo
                     (string-append "Rename buffer (was " old-name "): ")
                     row width)))
    (if (not new-name)
      (echo-message! echo "Cancelled")
      (if (string-empty? new-name)
        (echo-error! echo "Name cannot be empty")
        (begin
          (set! (buffer-name buf) new-name)
          (echo-message! echo
            (string-append "Renamed to " new-name)))))))

;;;============================================================================
;;; Other-window commands: find-file/switch-buffer in other window
;;;============================================================================

(def (cmd-switch-buffer-other-window app)
  "Switch to a buffer in the other window."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (<= (length wins) 1)
      ;; Split first, then switch buffer in the new window
      (begin
        (cmd-split-window app)
        (frame-other-window! fr)
        (cmd-switch-buffer app))
      ;; Already split: switch to other window, then prompt for buffer
      (begin
        (frame-other-window! fr)
        (cmd-switch-buffer app)))))

(def (cmd-find-file-other-window app)
  "Open a file in the other window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (<= (length wins) 1)
      (begin
        (cmd-split-window app)
        (frame-other-window! fr)
        (cmd-find-file app))
      (begin
        (frame-other-window! fr)
        (cmd-find-file app)))))

;;;============================================================================
;;; Emacs-style universal argument (C-u) stub
;;;============================================================================

(def (cmd-universal-argument app)
  "Universal argument (C-u). Sets or increments the prefix argument."
  (let ((current (app-state-prefix-arg app)))
    (cond
     ((not current)
      (set! (app-state-prefix-arg app) '(4)))
     ((list? current)
      (set! (app-state-prefix-arg app) (list (* 4 (car current)))))
     (else
      ;; If it was a number (e.g. from M-5), C-u resets it to (4)
      (set! (app-state-prefix-arg app) '(4))))
    (echo-message! (app-state-echo app)
                   (string-append "C-u"
                                  (let ((val (car (app-state-prefix-arg app))))
                                    (if (= val 4) "" (string-append " " (number->string val))))
                                  "-"))))

(def (cmd-digit-argument app digit)
  "Digit argument (M-0 to M-9). Builds a numeric prefix argument."
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
  "Negative argument (M--). Starts a negative numeric prefix argument."
  (set! (app-state-prefix-arg app) '-)
  (set! (app-state-prefix-digit-mode? app) #t)
  (echo-message! (app-state-echo app) "Arg: -"))

;; Individual digit argument commands for registry
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
;;; Text transforms: tabify, untabify, base64, rot13
;;;============================================================================

(def (cmd-tabify app)
  "Convert spaces to tabs in region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             ;; Replace runs of 8 spaces with tab (simple approach)
             (result (let loop ((s region) (acc ""))
                       (let ((idx (string-contains s "        ")))  ; 8 spaces
                         (if idx
                           (loop (substring s (+ idx 8) (string-length s))
                                 (string-append acc (substring s 0 idx) "\t"))
                           (string-append acc s))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo "Tabified")))))

(def (cmd-untabify app)
  "Convert tabs to spaces in region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             ;; Replace all tabs with 8 spaces
             (result (let loop ((i 0) (acc '()))
                       (if (>= i (string-length region))
                         (apply string-append (reverse acc))
                         (if (char=? (string-ref region i) #\tab)
                           (loop (+ i 1) (cons "        " acc))
                           (loop (+ i 1) (cons (string (string-ref region i)) acc)))))))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo "Untabified")))))

(def (cmd-base64-encode-region app)
  "Base64 encode the region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring (editor-get-text ed) start end))
             (encoded (base64-encode region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start encoded))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Base64 encoded")))))

(def (cmd-base64-decode-region app)
  "Base64 decode the region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (with-catch
        (lambda (e)
          (echo-error! echo "Base64 decode error"))
        (lambda ()
          (let* ((pos (editor-get-current-pos ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (region (substring (editor-get-text ed) start end))
                 (decoded (base64-decode (string-trim-both region))))
            (with-undo-action ed
              (editor-delete-range ed start (- end start))
              (editor-insert-text ed start decoded))
            (set! (buffer-mark buf) #f)
            (echo-message! echo "Base64 decoded")))))))

(def (rot13-char ch)
  "Apply ROT13 to a character."
  (cond
    ((and (char>=? ch #\a) (char<=? ch #\z))
     (integer->char (+ (char->integer #\a)
                       (modulo (+ (- (char->integer ch) (char->integer #\a)) 13) 26))))
    ((and (char>=? ch #\A) (char<=? ch #\Z))
     (integer->char (+ (char->integer #\A)
                       (modulo (+ (- (char->integer ch) (char->integer #\A)) 13) 26))))
    (else ch)))

(def (rot13-string s)
  "Apply ROT13 to a string."
  (let* ((len (string-length s))
         (result (make-string len)))
    (let loop ((i 0))
      (when (< i len)
        (string-set! result i (rot13-char (string-ref s i)))
        (loop (+ i 1))))
    result))

(def (cmd-rot13-region app)
  "ROT13 encode the region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (result (rot13-string region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo "ROT13 applied")))))

;;;============================================================================
;;; Hex dump display
;;;============================================================================

(def (cmd-hexl-mode app)
  "Display buffer contents as hex dump in *Hex* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (text (editor-get-text ed))
         (bytes (string->bytes text))
         (len (u8vector-length bytes))
         (lines '()))
    ;; Format hex dump, 16 bytes per line
    (let loop ((offset 0))
      (when (< offset len)
        (let* ((end (min (+ offset 16) len))
               (hex-parts '())
               (ascii-parts '()))
          ;; Hex portion
          (let hex-loop ((i offset))
            (when (< i end)
              (let* ((b (u8vector-ref bytes i))
                     (h (number->string b 16)))
                (set! hex-parts
                  (cons (if (< b 16) (string-append "0" h) h)
                        hex-parts)))
              (hex-loop (+ i 1))))
          ;; ASCII portion
          (let ascii-loop ((i offset))
            (when (< i end)
              (let ((b (u8vector-ref bytes i)))
                (set! ascii-parts
                  (cons (if (and (>= b 32) (<= b 126))
                          (string (integer->char b))
                          ".")
                        ascii-parts)))
              (ascii-loop (+ i 1))))
          ;; Format offset
          (let* ((off-str (number->string offset 16))
                 (off-padded (string-append
                               (make-string (max 0 (- 8 (string-length off-str))) #\0)
                               off-str))
                 (hex-str (string-join (reverse hex-parts) " "))
                 ;; Pad hex to consistent width (47 chars for 16 bytes)
                 (hex-padded (string-append hex-str
                               (make-string (max 0 (- 47 (string-length hex-str))) #\space)))
                 (ascii-str (apply string-append (reverse ascii-parts))))
            (set! lines
              (cons (string-append off-padded "  " hex-padded "  |" ascii-str "|")
                    lines))))
        (loop (+ offset 16))))
    ;; Display in *Hex* buffer
    (let* ((result (string-join (reverse lines) "\n"))
           (full-text (string-append "Hex Dump (" (number->string len) " bytes):\n\n"
                                     result "\n"))
           (buf (or (buffer-by-name "*Hex*")
                    (buffer-create! "*Hex*" ed #f))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed full-text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (echo-message! echo "*Hex*"))))

;;;============================================================================
;;; Count matches, delete duplicate lines
;;;============================================================================

(def (cmd-count-matches app)
  "Count occurrences of a pattern in the buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Count matches for: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (plen (string-length pattern))
             (count
               (if (= plen 0) 0
                 (let loop ((pos 0) (n 0))
                   (let ((idx (string-contains text pattern pos)))
                     (if idx
                       (loop (+ idx plen) (+ n 1))
                       n))))))
        (echo-message! echo
          (string-append (number->string count) " occurrence"
                         (if (= count 1) "" "s")
                         " of \"" pattern "\""))))))

(def (cmd-delete-duplicate-lines app)
  "Remove duplicate lines from region or buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (lines (string-split region #\newline))
             ;; Remove duplicates while preserving order
             (seen (make-hash-table))
             (unique
               (filter (lambda (line)
                         (if (hash-get seen line)
                           #f
                           (begin (hash-put! seen line #t) #t)))
                       lines))
             (removed (- (length lines) (length unique)))
             (result (string-join unique "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo
          (string-append "Removed " (number->string removed) " duplicate line"
                         (if (= removed 1) "" "s")))))))

;;;============================================================================
;;; Diff buffer with file
;;;============================================================================

(def (cmd-diff-buffer-with-file app)
  "Show diff between buffer contents and the saved file."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no associated file")
      (if (not (file-exists? path))
        (echo-error! echo (string-append "File not found: " path))
        (let* ((file-text (read-file-as-string path))
               (buf-text (editor-get-text ed))
               ;; Write both to temp files and run diff
               (pid (number->string (##current-process-id)))
               (tmp1 (string-append "/tmp/gemacs-diff-file-" pid))
               (tmp2 (string-append "/tmp/gemacs-diff-buf-" pid)))
          (write-string-to-file file-text tmp1)
          (write-string-to-file buf-text tmp2)
          (let* ((proc (open-process
                         (list path: "/usr/bin/diff"
                               arguments: (list "-u" tmp1 tmp2)
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (status (process-status proc)))
            ;; Clean up temp files
            (with-catch void (lambda () (delete-file tmp1)))
            (with-catch void (lambda () (delete-file tmp2)))
            (if (and output (> (string-length output) 0))
              ;; Show diff in *Diff* buffer
              (let ((diff-buf (or (buffer-by-name "*Diff*")
                                  (buffer-create! "*Diff*" ed #f))))
                (buffer-attach! ed diff-buf)
                (set! (edit-window-buffer (current-window fr)) diff-buf)
                (editor-set-text ed output)
                (editor-set-save-point ed)
                (editor-goto-pos ed 0)
                (echo-message! echo "*Diff*"))
              (echo-message! echo "No differences"))))))))

;;;============================================================================
;;; Checksum: SHA256
;;;============================================================================

(def (cmd-checksum app)
  "Show SHA256 checksum of the buffer or region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (editor-get-current-pos ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (hash-bytes (sha256 (string->bytes region)))
             (hex-str (hex-encode hash-bytes)))
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! echo (string-append "SHA256: " hex-str))))))

;;;============================================================================
;;; Async shell command
;;;============================================================================

(def (cmd-async-shell-command app)
  "Run a shell command asynchronously, showing output in *Async Shell*."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Async shell command: " row width)))
    (if (not cmd)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (proc (open-process
                     (list path: "/bin/sh"
                           arguments: (list "-c" cmd)
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (output (read-line proc #f))
             (status (process-status proc)))
        (if (and output (> (string-length output) 0))
          (let ((out-buf (or (buffer-by-name "*Async Shell*")
                             (buffer-create! "*Async Shell*" ed #f))))
            (buffer-attach! ed out-buf)
            (set! (edit-window-buffer (current-window fr)) out-buf)
            (editor-set-text ed
              (string-append "$ " cmd "\n\n" output "\n\n"
                             "(exit " (number->string status) ")"))
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)
            (echo-message! echo "*Async Shell*"))
          (echo-message! echo
            (string-append "Command finished (exit " (number->string status) ")")))))))

;;;============================================================================
;;; Toggle truncate lines
;;;============================================================================

(def (cmd-toggle-truncate-lines app)
  "Toggle line truncation (word wrap)."
  (cmd-toggle-word-wrap app))

;;;============================================================================
;;; Grep in buffer (interactive)
;;;============================================================================

(def (cmd-grep-buffer app)
  "Search for matching lines and show in *Grep* buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Grep buffer: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (buf-name (buffer-name (current-buffer-from-app app)))
             (lines (string-split text #\newline))
             (matches '())
             (line-num 0))
        ;; Collect matching lines with line numbers
        (for-each
          (lambda (line)
            (set! line-num (+ line-num 1))
            (when (string-contains line pattern)
              (set! matches
                (cons (string-append
                        (number->string line-num) ": " line)
                      matches))))
          lines)
        (if (null? matches)
          (echo-message! echo (string-append "No matches for '" pattern "'"))
          (let* ((result (string-append "Grep: " pattern " in " buf-name "\n\n"
                                        (string-join (reverse matches) "\n") "\n"))
                 (grep-buf (or (buffer-by-name "*Grep*")
                               (buffer-create! "*Grep*" ed #f))))
            (buffer-attach! ed grep-buf)
            (set! (edit-window-buffer (current-window fr)) grep-buf)
            (editor-set-text ed result)
            (editor-set-save-point ed)
            (editor-goto-pos ed 0)
            (echo-message! echo
              (string-append (number->string (length matches)) " match"
                             (if (= (length matches) 1) "" "es")))))))))

;;;============================================================================
;;; Misc: insert-date, insert-char
;;;============================================================================

(def (cmd-insert-date app)
  "Insert current date/time at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         ;; Use external date command for simplicity
         (proc (open-process
                 (list path: "/bin/date"
                       arguments: '()
                       stdout-redirection: #t)))
         (output (read-line proc))
         (status (process-status proc)))
    (when (and (string? output) (> (string-length output) 0))
      (editor-insert-text ed pos output))))

(def (cmd-insert-char app)
  "Insert a character by its Unicode code point."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Insert char (hex code): " row width)))
    (if (not input)
      (echo-message! echo "Cancelled")
      (let ((code (string->number input 16)))
        (if (not code)
          (echo-error! echo "Invalid hex code")
          (let* ((ed (current-editor app))
                 (pos (editor-get-current-pos ed))
                 (ch (string (integer->char code))))
            (editor-insert-text ed pos ch)
            (echo-message! echo
              (string-append "Inserted U+" input))))))))

;;;============================================================================
;;; Eval buffer / eval region
;;;============================================================================

(def (cmd-eval-buffer app)
  "Evaluate all top-level forms in the current buffer.
   Full Gerbil syntax supported (def, defstruct, hash, match, etc.)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (text (editor-get-text ed))
         (name (buffer-name buf)))
    (let-values (((count err) (load-user-string! text name)))
      (if err
        (echo-error! echo (string-append "Error: " err))
        (echo-message! echo (string-append "Evaluated " (number->string count)
                                           " forms in " name))))))

(def (cmd-eval-region app)
  "Evaluate the selected region as a Gerbil expression."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring (editor-get-text ed) start end)))
        (let-values (((result error?) (eval-expression-string region)))
          (set! (buffer-mark buf) #f)
          (if error?
            (echo-error! echo (string-append "Error: " result))
            (echo-message! echo (string-append "=> " result))))))))

;;;============================================================================
;;; Clone buffer, scratch buffer
;;;============================================================================

(def (cmd-clone-buffer app)
  "Create a copy of the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (current-buffer-from-app app))
         (text (editor-get-text ed))
         (new-name (string-append (buffer-name buf) "<clone>")))
    (let ((new-buf (buffer-create! new-name ed #f)))
      (buffer-attach! ed new-buf)
      (set! (edit-window-buffer (current-window fr)) new-buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (echo-message! echo (string-append "Cloned to " new-name)))))

(def (cmd-scratch-buffer app)
  "Switch to the *scratch* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name buffer-scratch-name)
                  (buffer-create! buffer-scratch-name ed #f))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (echo-message! echo buffer-scratch-name)))

;;;============================================================================
;;; Save some buffers
;;;============================================================================

(def (cmd-save-some-buffers app)
  "Save all modified buffers that have file paths."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (saved 0))
    ;; Iterate over all windows and save their buffers if modified
    (for-each
      (lambda (win)
        (let ((win-ed (edit-window-editor win))
              (buf (edit-window-buffer win)))
          (when (and (buffer-file-path buf)
                     (buffer-modified buf))
            (let ((text (editor-get-text win-ed)))
              (write-string-to-file (buffer-file-path buf) text)
              (editor-set-save-point win-ed)
              (set! saved (+ saved 1))))))
      (frame-windows fr))
    (if (= saved 0)
      (echo-message! echo "No buffers need saving")
      (echo-message! echo
        (string-append "Saved " (number->string saved) " buffer"
                       (if (= saved 1) "" "s"))))))

;;;============================================================================
;;; Revert buffer quick (no confirmation)
;;;============================================================================

(def (cmd-revert-buffer-quick app)
  "Revert buffer from disk without confirmation."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no file to revert from")
      (if (not (file-exists? path))
        (echo-error! echo (string-append "File not found: " path))
        (let ((text (read-file-as-string path)))
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0)
          (echo-message! echo (string-append "Reverted " path)))))))

;;;============================================================================
;;; Toggle syntax highlighting
;;;============================================================================

(def (cmd-toggle-highlighting app)
  "Toggle Gerbil syntax highlighting on the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app)))
    (if (buffer-lexer-lang buf)
      (begin
        ;; Turn off: clear lexer language, reset all styles to default
        (set! (buffer-lexer-lang buf) #f)
        (send-message ed SCI_STYLECLEARALL)
        (echo-message! echo "Highlighting off"))
      (begin
        ;; Turn on: set lexer language, re-apply highlighting
        (set! (buffer-lexer-lang buf) 'gerbil)
        (setup-gerbil-highlighting! ed)
        (echo-message! echo "Highlighting on")))))

;;;============================================================================
;;; Misc utility commands
;;;============================================================================

(def (cmd-view-lossage app)
  "Display recent keystrokes in a *Help* buffer."
  (let* ((text (string-append "Recent keystrokes:\n\n"
                              (key-lossage->string app)
                              "\n"))
         (ed (current-editor app))
         (buf (buffer-create! "*Help*" ed #f)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window (app-state-frame app))) buf)
    (editor-set-text ed text)
    (editor-goto-pos ed 0)
    (editor-set-save-point ed)))

(def (cmd-display-time app)
  "Display current time in echo area."
  (let* ((proc (open-process
                 (list path: "/bin/date"
                       arguments: '("+%Y-%m-%d %H:%M:%S")
                       stdout-redirection: #t)))
         (output (read-line proc))
         (status (process-status proc)))
    (if (string? output)
      (echo-message! (app-state-echo app) output)
      (echo-error! (app-state-echo app) "Cannot get time"))))

(def (cmd-pwd app)
  "Display current working directory."
  (echo-message! (app-state-echo app) (current-directory)))

;;;============================================================================
;;; Ediff (compare two buffers)
;;;============================================================================

(def (cmd-ediff-buffers app)
  "Compare two buffers and show differences in a *Diff* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (names (map buffer-name (buffer-list))))
    (if (< (length names) 2)
      (echo-error! echo "Need at least 2 buffers to compare")
      (let* ((name-a (echo-read-string echo "Buffer A: " row width))
             (buf-a (and name-a (buffer-by-name name-a))))
        (if (not buf-a)
          (echo-error! echo (string-append "No buffer: " (or name-a "")))
          (let* ((name-b (echo-read-string echo "Buffer B: " row width))
                 (buf-b (and name-b (buffer-by-name name-b))))
            (if (not buf-b)
              (echo-error! echo (string-append "No buffer: " (or name-b "")))
              ;; Get text from both buffers, write to temp files, diff
              (let* ((pid (number->string (##current-process-id)))
                     (tmp-a (string-append "/tmp/gerbil-ediff-a-" pid))
                     (tmp-b (string-append "/tmp/gerbil-ediff-b-" pid)))
                ;; We need the text from those buffers — find their windows
                (let ((text-a #f) (text-b #f))
                  (for-each
                    (lambda (win)
                      (let ((wb (edit-window-buffer win)))
                        (when (eq? wb buf-a)
                          (set! text-a (editor-get-text (edit-window-editor win))))
                        (when (eq? wb buf-b)
                          (set! text-b (editor-get-text (edit-window-editor win))))))
                    (frame-windows fr))
                  ;; Fallback: if buffer not in a window, use current editor temporarily
                  (unless text-a
                    (buffer-attach! ed buf-a)
                    (set! text-a (editor-get-text ed)))
                  (unless text-b
                    (buffer-attach! ed buf-b)
                    (set! text-b (editor-get-text ed)))
                  ;; Write to temp files and diff
                  (write-string-to-file tmp-a text-a)
                  (write-string-to-file tmp-b text-b)
                  (let* ((proc (open-process
                                 (list path: "/usr/bin/diff"
                                       arguments: (list "-u"
                                                        (string-append "--label=" name-a)
                                                        (string-append "--label=" name-b)
                                                        tmp-a tmp-b)
                                       stdout-redirection: #t
                                       stderr-redirection: #t)))
                         (output (read-line proc #f))
                         (status (process-status proc)))
                    ;; Cleanup temp files
                    (with-catch void (lambda () (delete-file tmp-a)))
                    (with-catch void (lambda () (delete-file tmp-b)))
                    ;; Show diff in buffer
                    (let ((diff-buf (buffer-create! "*Diff*" ed #f)))
                      (buffer-attach! ed diff-buf)
                      (set! (edit-window-buffer (current-window fr)) diff-buf)
                      (if (and (string? output) (> (string-length output) 0))
                        (editor-set-text ed output)
                        (editor-set-text ed "(no differences)\n"))
                      (editor-set-save-point ed)
                      (editor-goto-pos ed 0)
                      (editor-set-read-only ed #t))))))))))))

;;;============================================================================
;;; Simple calculator
;;;============================================================================

(def (cmd-calc app)
  "Evaluate a math expression from the echo area."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (expr (echo-read-string echo "Calc: " row width)))
    (when (and expr (> (string-length expr) 0))
      (let-values (((result error?) (eval-expression-string expr)))
        (if error?
          (echo-error! echo (string-append "Error: " result))
          (echo-message! echo (string-append "= " result)))))))

;;;============================================================================
;;; Toggle case-fold-search
;;;============================================================================

(def *case-fold-search* #t)

(def (cmd-toggle-case-fold-search app)
  "Toggle case-sensitive search."
  (set! *case-fold-search* (not *case-fold-search*))
  (echo-message! (app-state-echo app)
    (if *case-fold-search*
      "Case-insensitive search"
      "Case-sensitive search")))

;;;============================================================================
;;; Describe-bindings (full binding list in a buffer)
;;;============================================================================

(def (cmd-describe-bindings app)
  "Show all keybindings in a *Bindings* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app)))
    (let ((lines []))
      ;; Collect bindings from all keymaps
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
      ;; Sort and display
      (let* ((sorted (sort lines string<?))
             (text (string-join sorted "\n"))
             (buf (buffer-create! "*Bindings*" ed #f)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer (current-window fr)) buf)
        (editor-set-text ed text)
        (editor-set-save-point ed)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)
        (echo-message! echo
          (string-append (number->string (length sorted)) " bindings"))))))

;;;============================================================================
;;; Center line
;;;============================================================================

(def (cmd-center-line app)
  "Center the current line within fill-column (80)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (editor-get-line-end-position ed line-num))
         (text (substring (editor-get-text ed) line-start line-end))
         ;; Strip leading whitespace
         (trimmed (let loop ((i 0))
                    (if (and (< i (string-length text))
                             (or (char=? (string-ref text i) #\space)
                                 (char=? (string-ref text i) #\tab)))
                      (loop (+ i 1))
                      (substring text i (string-length text)))))
         (fill-col 80)
         (padding (max 0 (quotient (- fill-col (string-length trimmed)) 2)))
         (new-line (string-append (make-string padding #\space) trimmed)))
    (with-undo-action ed
      (editor-delete-range ed line-start (- line-end line-start))
      (editor-insert-text ed line-start new-line))))

;;;============================================================================
;;; What face (show current style info)
;;;============================================================================

(def (cmd-what-face app)
  "Show the Scintilla style at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (style (send-message ed SCI_GETSTYLEAT pos)))
    (echo-message! (app-state-echo app)
      (string-append "Style " (number->string style) " at pos "
                     (number->string pos)))))

;;;============================================================================
;;; List processes
;;;============================================================================

(def (cmd-list-processes app)
  "Show running subprocesses in *Processes* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (lines ["PID\tType\tBuffer"
                 "---\t----\t------"]))
    ;; Check REPL buffers
    (for-each
      (lambda (buf)
        (when (repl-buffer? buf)
          (let ((rs (hash-get *repl-state* buf)))
            (when rs
              (set! lines (cons
                (string-append "?\tREPL\t" (buffer-name buf))
                lines))))))
      (buffer-list))
    ;; Check shell buffers
    (for-each
      (lambda (buf)
        (when (shell-buffer? buf)
          (let ((ss (hash-get *shell-state* buf)))
            (when ss
              (set! lines (cons
                (string-append "?\tShell\t" (buffer-name buf))
                lines))))))
      (buffer-list))
    (let* ((text (string-join (reverse lines) "\n"))
           (proc-buf (buffer-create! "*Processes*" ed #f)))
      (buffer-attach! ed proc-buf)
      (set! (edit-window-buffer (current-window fr)) proc-buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t)
      (echo-message! echo "Process list"))))

;;;============================================================================
;;; View echo area messages (like *Messages*)
;;;============================================================================

(def *message-log* [])
(def *message-log-max* 100)

(def (log-message! msg)
  "Add a message to the message log."
  (set! *message-log* (cons msg *message-log*))
  (when (> (length *message-log*) *message-log-max*)
    (set! *message-log*
      (let loop ((msgs *message-log*) (n 0) (acc []))
        (if (or (null? msgs) (>= n *message-log-max*))
          (reverse acc)
          (loop (cdr msgs) (+ n 1) (cons (car msgs) acc)))))))

(def (cmd-view-messages app)
  "Show recent echo area messages in *Messages* buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (text (if (null? *message-log*)
                 "(no messages)\n"
                 (string-join (reverse *message-log*) "\n")))
         (buf (buffer-create! "*Messages*" ed #f)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (editor-set-text ed text)
    (editor-set-save-point ed)
    ;; Go to end to see latest messages
    (editor-goto-pos ed (string-length text))
    (editor-set-read-only ed #t)
    (echo-message! echo "*Messages*")))

;;;============================================================================
;;; Auto-fill mode toggle
;;;============================================================================

(def *auto-fill-mode* #f)

(def (cmd-toggle-auto-fill app)
  "Toggle auto-fill mode (line wrap at fill-column)."
  (set! *auto-fill-mode* (not *auto-fill-mode*))
  (echo-message! (app-state-echo app)
    (if *auto-fill-mode*
      "Auto-fill mode on"
      "Auto-fill mode off")))

;;; (delete-trailing-whitespace defined earlier at line ~1247)

;;;============================================================================
;;; Rename file (rename-file-and-buffer)
;;;============================================================================

(def (cmd-rename-file-and-buffer app)
  "Rename current file on disk and update the buffer name."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (old-path (buffer-file-path buf)))
    (if (not old-path)
      (echo-error! echo "Buffer is not visiting a file")
      (let ((new-path (echo-read-string echo
                        (string-append "Rename " old-path " to: ")
                        row width)))
        (when (and new-path (> (string-length new-path) 0))
          (with-catch
            (lambda (e)
              (echo-error! echo
                (string-append "Error: "
                  (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (rename-file old-path new-path)
              (set! (buffer-file-path buf) new-path)
              (set! (buffer-name buf) (path-strip-directory new-path))
              (echo-message! echo
                (string-append "Renamed to " new-path)))))))))

;;;============================================================================
;;; Kill buffer and delete file
;;;============================================================================

(def (cmd-delete-file-and-buffer app)
  "Delete the file on disk and kill the buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer is not visiting a file")
      (let ((confirm (echo-read-string echo
                       (string-append "Really delete " path "? (yes/no) ")
                       row width)))
        (when (and confirm (string=? confirm "yes"))
          (with-catch
            (lambda (e)
              (echo-error! echo
                (string-append "Error: "
                  (with-output-to-string (lambda () (display-exception e))))))
            (lambda ()
              (delete-file path)
              (echo-message! echo (string-append "Deleted " path))
              ;; Switch away from this buffer
              (let ((scratch (or (buffer-by-name buffer-scratch-name)
                                 (buffer-create! buffer-scratch-name ed #f))))
                (buffer-attach! ed scratch)
                (set! (edit-window-buffer
                        (current-window (app-state-frame app))) scratch)
                (buffer-list-remove! buf)))))))))

;;;============================================================================
;;; Sudo write (write file with sudo)
;;;============================================================================

(def (cmd-sudo-write app)
  "Write current buffer using sudo tee."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! echo "Buffer has no file path")
      (let* ((text (editor-get-text ed))
             (pid (number->string (##current-process-id)))
             (tmp (string-append "/tmp/gemacs-sudo-" pid)))
        (write-string-to-file tmp text)
        (let* ((proc (open-process
                        (list path: "/usr/bin/sudo"
                              arguments: (list "cp" tmp path)
                              stderr-redirection: #t)))
               (status (process-status proc)))
          (with-catch void (lambda () (delete-file tmp)))
          (if (= status 0)
            (begin
              (editor-set-save-point ed)
              (echo-message! echo (string-append "Saved (sudo) " path)))
            (echo-error! echo "sudo write failed")))))))

;;;============================================================================
;;; Sort region (different sort types)
;;;============================================================================

(def (cmd-sort-numeric app)
  "Sort lines in region numerically."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (lines (string-split text #\newline))
             (sorted (sort lines
                       (lambda (a b)
                         (let ((na (with-catch (lambda (e) 0)
                                     (lambda () (string->number a))))
                               (nb (with-catch (lambda (e) 0)
                                     (lambda () (string->number b)))))
                           (< (or na 0) (or nb 0))))))
             (new-text (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start new-text))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Sorted numerically")))))

;;;============================================================================
;;; Word count region
;;;============================================================================

(def (cmd-count-words-region app)
  "Count words in the selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (chars (- end start))
             (lines (length (string-split text #\newline)))
             (words (let loop ((i 0) (in-word #f) (count 0))
                      (if (>= i (string-length text))
                        (if in-word (+ count 1) count)
                        (let ((ch (string-ref text i)))
                          (if (or (char=? ch #\space) (char=? ch #\newline)
                                  (char=? ch #\tab))
                            (loop (+ i 1) #f (if in-word (+ count 1) count))
                            (loop (+ i 1) #t count)))))))
        (echo-message! echo
          (string-append "Region: " (number->string lines) " lines, "
                         (number->string words) " words, "
                         (number->string chars) " chars"))))))

;;;============================================================================
;;; Overwrite mode toggle
;;;============================================================================

(def *overwrite-mode* #f)

(def (cmd-toggle-overwrite-mode app)
  "Toggle overwrite mode (insert vs overwrite)."
  (set! *overwrite-mode* (not *overwrite-mode*))
  ;; SCI_SETOVERTYPE (2186) not in gerbil-scintilla constants — use raw value
  (let ((ed (current-editor app)))
    (send-message ed 2186 (if *overwrite-mode* 1 0)))
  (echo-message! (app-state-echo app)
    (if *overwrite-mode*
      "Overwrite mode on"
      "Overwrite mode off")))

;;;============================================================================
;;; Visual line mode (toggle word-wrap + line-at-a-time navigation)
;;;============================================================================

(def *visual-line-mode* #f)

(def (cmd-toggle-visual-line-mode app)
  "Toggle visual-line-mode (word wrap + visual line movement)."
  (set! *visual-line-mode* (not *visual-line-mode*))
  (let ((ed (current-editor app)))
    (send-message ed SCI_SETWRAPMODE
      (if *visual-line-mode* 1 0)))  ; SC_WRAP_WORD=1, SC_WRAP_NONE=0
  (echo-message! (app-state-echo app)
    (if *visual-line-mode*
      "Visual line mode on"
      "Visual line mode off")))

;;;============================================================================
;;; Set fill column
;;;============================================================================

(def *fill-column* 80)

(def (cmd-set-fill-column app)
  "Set the fill column for line wrapping and centering."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo
                  (string-append "Fill column (current: "
                                 (number->string *fill-column*) "): ")
                  row width)))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (if (and n (> n 0))
          (begin
            (set! *fill-column* n)
            (echo-message! echo
              (string-append "Fill column set to " (number->string n))))
          (echo-error! echo "Invalid number"))))))

;;;============================================================================
;;; Fill column indicator (display a vertical line)
;;;============================================================================

(def *fill-column-indicator* #f)

(def (cmd-toggle-fill-column-indicator app)
  "Toggle the fill column indicator (display in echo area)."
  (set! *fill-column-indicator* (not *fill-column-indicator*))
  (echo-message! (app-state-echo app)
    (if *fill-column-indicator*
      (string-append "Fill column indicator at " (number->string *fill-column*))
      "Fill column indicator off")))

;;;============================================================================
;;; Toggle debug on error
;;;============================================================================

(def *debug-on-error* #f)

(def (cmd-toggle-debug-on-error app)
  "Toggle debug-on-error mode."
  (set! *debug-on-error* (not *debug-on-error*))
  (echo-message! (app-state-echo app)
    (if *debug-on-error*
      "Debug on error enabled"
      "Debug on error disabled")))

;;;============================================================================
;;; Repeat complex command (re-execute last M-x command)
;;;============================================================================

(def *last-mx-command* #f)

(def (cmd-repeat-complex-command app)
  "Repeat the last M-x command."
  (let ((cmd *last-mx-command*))
    (if (symbol? cmd)
      (begin
        (echo-message! (app-state-echo app)
          (string-append "Repeating: " (symbol->string cmd)))
        (execute-command! app cmd))
      (echo-error! (app-state-echo app) "No previous M-x command"))))

;;;============================================================================
;;; Eldoc-like: show function signature at point
;;;============================================================================

(def (cmd-eldoc app)
  "Show info about the symbol at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find word boundaries around cursor
    (let* ((start (let loop ((i (- pos 1)))
                    (if (or (< i 0)
                            (let ((ch (string-ref text i)))
                              (not (or (char-alphabetic? ch)
                                       (char-numeric? ch)
                                       (char=? ch #\-)
                                       (char=? ch #\_)
                                       (char=? ch #\!)
                                       (char=? ch #\?)))))
                      (+ i 1) (loop (- i 1)))))
           (end (let loop ((i pos))
                  (if (or (>= i len)
                          (let ((ch (string-ref text i)))
                            (not (or (char-alphabetic? ch)
                                     (char-numeric? ch)
                                     (char=? ch #\-)
                                     (char=? ch #\_)
                                     (char=? ch #\!)
                                     (char=? ch #\?)))))
                    i (loop (+ i 1)))))
           (word (if (< start end) (substring text start end) #f)))
      (if word
        (echo-message! (app-state-echo app)
          (string-append "Symbol: " word))
        (echo-message! (app-state-echo app) "No symbol at point")))))

;;;============================================================================
;;; Highlight symbol at point (mark all occurrences)
;;;============================================================================

(def (cmd-highlight-symbol app)
  "Highlight all occurrences of the word at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Get word at point
    (let* ((start (let loop ((i (- pos 1)))
                    (if (or (< i 0)
                            (let ((ch (string-ref text i)))
                              (not (or (char-alphabetic? ch)
                                       (char-numeric? ch)
                                       (char=? ch #\_) (char=? ch #\-)))))
                      (+ i 1) (loop (- i 1)))))
           (end (let loop ((i pos))
                  (if (or (>= i len)
                          (let ((ch (string-ref text i)))
                            (not (or (char-alphabetic? ch)
                                     (char-numeric? ch)
                                     (char=? ch #\_) (char=? ch #\-)))))
                    i (loop (+ i 1)))))
           (word (if (< start end) (substring text start end) #f)))
      (if (not word)
        (echo-message! echo "No word at point")
        ;; Count occurrences
        (let ((count 0) (wlen (string-length word)))
          (let loop ((i 0))
            (when (<= (+ i wlen) len)
              (when (string=? (substring text i (+ i wlen)) word)
                (set! count (+ count 1)))
              (loop (+ i 1))))
          ;; Use Scintilla indicator to highlight
          (send-message ed SCI_INDICSETSTYLE 0 7)  ; INDIC_ROUNDBOX
          (send-message ed SCI_SETINDICATORCURRENT 0)
          ;; Clear previous highlights
          (send-message ed SCI_INDICATORCLEARRANGE 0 len)
          ;; Set highlights for each occurrence
          (let loop ((i 0))
            (when (<= (+ i wlen) len)
              (when (string=? (substring text i (+ i wlen)) word)
                (send-message ed SCI_INDICATORFILLRANGE i wlen))
              (loop (+ i 1))))
          (echo-message! echo
            (string-append (number->string count) " occurrence"
                           (if (= count 1) "" "s")
                           " of \"" word "\"")))))))

;;;============================================================================
;;; Clear highlight
;;;============================================================================

(def (cmd-clear-highlight app)
  "Clear all occurrence highlights."
  (let* ((ed (current-editor app))
         (len (editor-get-text-length ed)))
    (send-message ed SCI_SETINDICATORCURRENT 0)
    (send-message ed SCI_INDICATORCLEARRANGE 0 len)
    (echo-message! (app-state-echo app) "Highlights cleared")))

;;;============================================================================
;;; Indent rigidly (shift region left/right)
;;;============================================================================

(def (cmd-indent-rigidly-right app)
  "Shift selected lines right by 2 spaces."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (lines (string-split text #\newline))
             (indented (map (lambda (line) (string-append "  " line)) lines))
             (new-text (string-join indented "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start new-text))
        (set! (buffer-mark buf) start)
        (editor-goto-pos ed (+ start (string-length new-text)))
        (echo-message! echo "Indented right")))))

(def (cmd-indent-rigidly-left app)
  "Shift selected lines left by 2 spaces."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (substring (editor-get-text ed) start end))
             (lines (string-split text #\newline))
             (dedented (map (lambda (line)
                              (cond
                                ((and (>= (string-length line) 2)
                                      (char=? (string-ref line 0) #\space)
                                      (char=? (string-ref line 1) #\space))
                                 (substring line 2 (string-length line)))
                                ((and (> (string-length line) 0)
                                      (char=? (string-ref line 0) #\tab))
                                 (substring line 1 (string-length line)))
                                (else line)))
                            lines))
             (new-text (string-join dedented "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start new-text))
        (set! (buffer-mark buf) start)
        (editor-goto-pos ed (+ start (string-length new-text)))
        (echo-message! echo "Indented left")))))

;;;============================================================================
;;; Goto first/last non-blank line
;;;============================================================================

(def (cmd-goto-first-non-blank app)
  "Go to the first non-blank line in the buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline)))
    (let loop ((i 0) (pos 0))
      (if (>= i (length lines))
        (editor-goto-pos ed 0)
        (let ((line (list-ref lines i)))
          (if (> (string-length (string-trim line)) 0)
            (editor-goto-pos ed pos)
            (loop (+ i 1) (+ pos (string-length line) 1))))))))

(def (cmd-goto-last-non-blank app)
  "Go to the last non-blank line in the buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (total (length lines)))
    (let loop ((i (- total 1)) (last-pos (string-length text)))
      (if (< i 0)
        (editor-goto-pos ed last-pos)
        (let* ((line (list-ref lines i))
               (line-start (let lp ((j 0) (pos 0))
                             (if (>= j i) pos
                               (lp (+ j 1) (+ pos (string-length (list-ref lines j)) 1))))))
          (if (> (string-length (string-trim line)) 0)
            (editor-goto-pos ed line-start)
            (loop (- i 1) last-pos)))))))

;;;============================================================================
;;; Buffer statistics
;;;============================================================================

(def (cmd-buffer-stats app)
  "Show detailed buffer statistics."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (chars (string-length text))
         (lines (editor-get-line-count ed))
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i chars)
                    (if in-word (+ count 1) count)
                    (let ((ch (string-ref text i)))
                      (if (or (char=? ch #\space) (char=? ch #\newline)
                              (char=? ch #\tab))
                        (loop (+ i 1) #f (if in-word (+ count 1) count))
                        (loop (+ i 1) #t count))))))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (path (or (buffer-file-path buf) "(no file)")))
    (echo-message! echo
      (string-append name " | " path " | "
                     (number->string lines) "L "
                     (number->string words) "W "
                     (number->string chars) "C"))))

;;;============================================================================
;;; Toggle show tabs
;;;============================================================================

(def *show-tabs* #f)

(def (cmd-toggle-show-tabs app)
  "Toggle visible tab characters."
  (set! *show-tabs* (not *show-tabs*))
  (let ((ed (current-editor app)))
    (send-message ed SCI_SETVIEWWS
      (if *show-tabs* 1 0)))  ; SCWS_VISIBLEALWAYS=1
  (echo-message! (app-state-echo app)
    (if *show-tabs* "Show tabs on" "Show tabs off")))

;;;============================================================================
;;; Toggle show EOL
;;;============================================================================

(def *show-eol* #f)

(def (cmd-toggle-show-eol app)
  "Toggle visible end-of-line characters."
  (set! *show-eol* (not *show-eol*))
  (let ((ed (current-editor app)))
    (send-message ed SCI_SETVIEWEOL
      (if *show-eol* 1 0)))
  (echo-message! (app-state-echo app)
    (if *show-eol* "Show EOL on" "Show EOL off")))

;;;============================================================================
;;; Copy from above/below line
;;;============================================================================

(def (cmd-copy-from-above app)
  "Copy character from the line above at the same column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (col (- pos (editor-position-from-line ed line))))
    (if (= line 0)
      (echo-error! (app-state-echo app) "No line above")
      (let* ((above-start (editor-position-from-line ed (- line 1)))
             (above-end (editor-get-line-end-position ed (- line 1)))
             (above-len (- above-end above-start)))
        (if (>= col above-len)
          (echo-error! (app-state-echo app) "Line above too short")
          (let* ((text (editor-get-text ed))
                 (ch (string (string-ref text (+ above-start col)))))
            (editor-insert-text ed pos ch)))))))

(def (cmd-copy-from-below app)
  "Copy character from the line below at the same column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (total-lines (editor-get-line-count ed))
         (col (- pos (editor-position-from-line ed line))))
    (if (>= (+ line 1) total-lines)
      (echo-error! (app-state-echo app) "No line below")
      (let* ((below-start (editor-position-from-line ed (+ line 1)))
             (below-end (editor-get-line-end-position ed (+ line 1)))
             (below-len (- below-end below-start)))
        (if (>= col below-len)
          (echo-error! (app-state-echo app) "Line below too short")
          (let* ((text (editor-get-text ed))
                 (ch (string (string-ref text (+ below-start col)))))
            (editor-insert-text ed pos ch)))))))

;;;============================================================================
;;; Open line above (like vim O)
;;;============================================================================

(def (cmd-open-line-above app)
  "Insert a new line above the current line and move to it."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line)))
    (with-undo-action ed
      (editor-insert-text ed line-start "\n")
      (editor-goto-pos ed line-start))))

;;;============================================================================
;;; Select current line
;;;============================================================================

(def (cmd-select-line app)
  "Select the current line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (line-end (editor-get-line-end-position ed line))
         (buf (current-buffer-from-app app)))
    (set! (buffer-mark buf) line-start)
    ;; Move to the start of the next line if possible
    (let ((next-start (if (< (+ line 1) (editor-get-line-count ed))
                        (editor-position-from-line ed (+ line 1))
                        line-end)))
      (editor-goto-pos ed next-start)
      (echo-message! (app-state-echo app) "Line selected"))))

;;;============================================================================
;;; Split line (break line at point, keep indentation)
;;;============================================================================

(def (cmd-split-line app)
  "Split line at point and indent continuation to same column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (col (- pos line-start))
         (padding (make-string col #\space)))
    (with-undo-action ed
      (editor-insert-text ed pos (string-append "\n" padding)))))

;;;============================================================================
;;; Convert line endings
;;;============================================================================

(def (cmd-convert-to-unix app)
  "Convert buffer line endings to Unix (LF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    ;; Remove CRs (convert CRLF -> LF and standalone CR -> LF)
    (let loop ((i 0) (acc []))
      (if (>= i (string-length text))
        (let ((new-text (list->string (reverse acc))))
          (unless (string=? text new-text)
            (with-undo-action ed
              (editor-set-text ed new-text))
            (echo-message! (app-state-echo app) "Converted to Unix (LF)")))
        (let ((ch (string-ref text i)))
          (if (char=? ch #\return)
            (loop (+ i 1) acc)
            (loop (+ i 1) (cons ch acc))))))))

(def (cmd-convert-to-dos app)
  "Convert buffer line endings to DOS (CRLF)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (new-text (string-join lines "\r\n")))
    (unless (string=? text new-text)
      (with-undo-action ed
        (editor-set-text ed new-text))
      (echo-message! (app-state-echo app) "Converted to DOS (CRLF)"))))

;;;============================================================================
;;; Enlarge/shrink window
;;;============================================================================

(def (cmd-enlarge-window app)
  "Make current window taller (stub — adjusts height by 2 rows)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (h (edit-window-h win)))
    (set! (edit-window-h win) (+ h 2))
    (echo-message! (app-state-echo app) "Window enlarged")))

(def (cmd-shrink-window app)
  "Make current window shorter (stub — adjusts height by 2 rows)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (h (edit-window-h win)))
    (when (> h 4)
      (set! (edit-window-h win) (- h 2)))
    (echo-message! (app-state-echo app) "Window shrunk")))

;;;============================================================================
;;; What buffer encoding
;;;============================================================================

(def (cmd-what-encoding app)
  "Show the encoding of the current buffer."
  ;; Scintilla uses UTF-8 (codepage 65001) by default
  (echo-message! (app-state-echo app) "Encoding: UTF-8"))

;;;============================================================================
;;; Hippie expand (simple completion from buffer words)
;;;============================================================================

(def (cmd-hippie-expand app)
  "Complete word at point from buffer contents (simple hippie-expand)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find prefix at point
    (let* ((prefix-start
             (let loop ((i (- pos 1)))
               (if (or (< i 0)
                       (let ((ch (string-ref text i)))
                         (not (or (char-alphabetic? ch)
                                  (char-numeric? ch)
                                  (char=? ch #\_)
                                  (char=? ch #\-)))))
                 (+ i 1) (loop (- i 1)))))
           (prefix (substring text prefix-start pos))
           (plen (string-length prefix)))
      (if (= plen 0)
        (echo-message! echo "No prefix to complete")
        ;; Scan buffer for words starting with prefix
        (let ((candidates []))
          (let loop ((i 0))
            (when (< i len)
              (let* ((wstart
                       (let ws ((j i))
                         (if (or (>= j len)
                                 (let ((ch (string-ref text j)))
                                   (or (char-alphabetic? ch)
                                       (char-numeric? ch)
                                       (char=? ch #\_)
                                       (char=? ch #\-))))
                           j
                           (ws (+ j 1)))))
                     (wend
                       (let we ((j wstart))
                         (if (or (>= j len)
                                 (let ((ch (string-ref text j)))
                                   (not (or (char-alphabetic? ch)
                                            (char-numeric? ch)
                                            (char=? ch #\_)
                                            (char=? ch #\-)))))
                           j
                           (we (+ j 1))))))
                (when (> wend wstart)
                  (let ((word (substring text wstart wend)))
                    (when (and (> (string-length word) plen)
                               (string-prefix? prefix word)
                               (not (= wstart prefix-start))
                               (not (member word candidates)))
                      (set! candidates (cons word candidates))))
                  (loop wend))
                (when (= wend wstart)
                  (loop (+ wstart 1))))))
          (if (null? candidates)
            (echo-message! echo
              (string-append "No completions for \"" prefix "\""))
            ;; Insert first candidate
            (let ((completion (car (reverse candidates))))
              (with-undo-action ed
                (editor-delete-range ed prefix-start plen)
                (editor-insert-text ed prefix-start completion))
              (editor-goto-pos ed (+ prefix-start (string-length completion)))
              (echo-message! echo
                (string-append completion
                  (if (> (length candidates) 1)
                    (string-append " [" (number->string (length candidates))
                                   " candidates]")
                    ""))))))))))

;;;============================================================================
;;; Swap buffers in windows
;;;============================================================================

(def (cmd-swap-buffers app)
  "Swap buffers between current and next window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (< (length wins) 2)
      (echo-error! (app-state-echo app) "Only one window")
      (let* ((cur-idx (frame-current-idx fr))
             (next-idx (modulo (+ cur-idx 1) (length wins)))
             (cur-win (list-ref wins cur-idx))
             (next-win (list-ref wins next-idx))
             (cur-buf (edit-window-buffer cur-win))
             (next-buf (edit-window-buffer next-win)))
        ;; Swap the buffers
        (buffer-attach! (edit-window-editor cur-win) next-buf)
        (set! (edit-window-buffer cur-win) next-buf)
        (buffer-attach! (edit-window-editor next-win) cur-buf)
        (set! (edit-window-buffer next-win) cur-buf)
        (echo-message! (app-state-echo app) "Buffers swapped")))))

;;;============================================================================
;;; Toggle tab-width between 2/4/8
;;;============================================================================

(def (cmd-cycle-tab-width app)
  "Cycle tab width between 2, 4, and 8."
  (let* ((ed (current-editor app))
         (current (send-message ed SCI_GETTABWIDTH))
         (next (cond
                 ((= current 2) 4)
                 ((= current 4) 8)
                 (else 2))))
    (send-message ed SCI_SETTABWIDTH next)
    (echo-message! (app-state-echo app)
      (string-append "Tab width: " (number->string next)))))

;;;============================================================================
;;; Toggle use tabs vs spaces
;;;============================================================================

(def (cmd-toggle-indent-tabs-mode app)
  "Toggle between using tabs and spaces for indentation."
  (let* ((ed (current-editor app))
         (using-tabs (= 1 (send-message ed SCI_GETUSETABS))))
    (send-message ed SCI_SETUSETABS (if using-tabs 0 1))
    (echo-message! (app-state-echo app)
      (if using-tabs
        "Indent with spaces"
        "Indent with tabs"))))

;;;============================================================================
;;; Print buffer info
;;;============================================================================

(def (cmd-buffer-info app)
  "Show buffer name, file, and position info."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (editor-line-from-position ed pos)))
         (col (+ 1 (- pos (editor-position-from-line ed (- line 1))))))
    (echo-message! (app-state-echo app)
      (string-append (buffer-name buf)
                     " L" (number->string line)
                     " C" (number->string col)
                     " pos:" (number->string pos)
                     (if (buffer-file-path buf)
                       (string-append " " (buffer-file-path buf))
                       "")))))

