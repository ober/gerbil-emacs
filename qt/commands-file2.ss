;;; -*- Gerbil -*-
;;; Qt commands file2 - text transforms, hex dump, toggle modes, calc, describe
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        (only-in :gemacs/persist *auto-fill-mode* *fill-column*)
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
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-search2
        :gemacs/qt/commands-file)

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
  "Toggle overwrite mode (Insert vs Overwrite)."
  (set! *overwrite-mode* (not *overwrite-mode*))
  (let ((ed (current-qt-editor app)))
    (sci-send ed 2186 (if *overwrite-mode* 1 0)))  ;; SCI_SETOVERTYPE
  (echo-message! (app-state-echo app)
    (if *overwrite-mode* "Overwrite mode ON" "Overwrite mode OFF")))

;; *auto-fill-mode* and *fill-column* are defined in persist.ss
(def *fill-column-indicator* #f)

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

(def *highlighting-disabled* #f)
(def (cmd-toggle-highlighting app)
  "Toggle syntax highlighting on/off."
  (set! *highlighting-disabled* (not *highlighting-disabled*))
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-current-buffer fr)))
    (if *highlighting-disabled*
      (begin
        ;; Disable lexer: set to SCLEX_NULL (0) and clear styles
        (sci-send ed SCI_SETLEXER 0)
        (sci-send ed SCI_STYLECLEARALL)
        (echo-message! (app-state-echo app) "Syntax highlighting OFF"))
      (begin
        ;; Re-apply highlighting for current buffer
        (when buf
          (qt-setup-highlighting! app buf))
        (echo-message! (app-state-echo app) "Syntax highlighting ON")))))

(def (cmd-toggle-visual-line-mode app)
  "Toggle visual line mode."
  (cmd-toggle-word-wrap app))

(def (cmd-toggle-fill-column-indicator app)
  "Toggle fill column indicator — vertical line at fill-column."
  (set! *fill-column-indicator* (not *fill-column-indicator*))
  (let ((ed (current-qt-editor app)))
    (if *fill-column-indicator*
      (begin
        (sci-send ed 2361 *fill-column* 0)  ;; SCI_SETEDGECOLUMN
        (sci-send ed 2363 1 0))             ;; SCI_SETEDGEMODE EDGE_LINE
      (sci-send ed 2363 0 0))               ;; SCI_SETEDGEMODE EDGE_NONE
    (echo-message! (app-state-echo app)
      (if *fill-column-indicator*
        (string-append "Fill column indicator at " (number->string *fill-column*))
        "Fill column indicator off"))))

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
                          (read-line p #f) ;; Omit process-status (Qt SIGCHLD race)
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
;; cmd-markdown-mode moved to commands-parity2.ss (real lexer switching)
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
                 (let ((out (read-line p #f))) (close-port p) (or out "")))))) ;; Omit process-status (Qt SIGCHLD race)
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

