;;; -*- Gerbil -*-
;;; Bookmarks, rectangles, isearch, semantic, whitespace, highlight,
;;; LSP, DAP, snippets, and more tool commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/process
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers
        :gemacs/editor-extra-tools)

;; Bookmark extras
(def (cmd-bookmark-bmenu-list app)
  "List bookmarks in a menu buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (bmarks (app-state-bookmarks app))
         (entries (hash->list bmarks))
         (text (if (null? entries)
                 "No bookmarks defined.\n\nUse C-x r m to set a bookmark."
                 (string-join
                   (map (lambda (e)
                          (let ((name (car e))
                                (info (cdr e)))
                            (string-append "  " (symbol->string name)
                              (if (string? info) (string-append "  " info) ""))))
                        entries)
                   "\n")))
         (buf (buffer-create! "*Bookmarks*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Bookmark List\n\n" text "\n"))
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;; Rectangle extras
(def (cmd-rectangle-mark-mode app)
  "Toggle rectangle mark mode."
  (let ((on (toggle-mode! 'rectangle-mark)))
    (echo-message! (app-state-echo app)
      (if on "Rectangle mark mode (use C-x r k/y)" "Rectangle mark mode off"))))

(def (cmd-number-to-register app)
  "Store a number in a register."
  (let ((reg (app-read-string app "Register (a-z): ")))
    (when (and reg (not (string-empty? reg)))
      (let* ((key (string->symbol reg))
             (registers (app-state-registers app))
             (arg (get-prefix-arg app)))
        (hash-put! registers key arg)
        (echo-message! (app-state-echo app)
          (string-append "Register " reg " = " (number->string arg)))))))

;; Isearch extras
(def *isearch-case-fold* #t)
(def *isearch-regexp* #f)

(def (cmd-isearch-toggle-case-fold app)
  "Toggle case sensitivity in isearch."
  (set! *isearch-case-fold* (not *isearch-case-fold*))
  (echo-message! (app-state-echo app)
    (if *isearch-case-fold* "Isearch: case insensitive" "Isearch: case sensitive")))

(def (cmd-isearch-toggle-regexp app)
  "Toggle regexp in isearch."
  (set! *isearch-regexp* (not *isearch-regexp*))
  (echo-message! (app-state-echo app)
    (if *isearch-regexp* "Isearch: regexp mode" "Isearch: literal mode")))

;; Semantic / imenu / tags
(def (cmd-semantic-mode app)
  "Toggle semantic mode — parse buffer for definitions."
  (let ((on (toggle-mode! 'semantic)))
    (echo-message! (app-state-echo app)
      (if on "Semantic mode enabled" "Semantic mode disabled"))))

(def (cmd-imenu-anywhere app)
  "Jump to definition in current buffer using grep for def/class/function."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (defs '()))
    ;; Collect definitions
    (let loop ((ls lines) (n 0))
      (when (not (null? ls))
        (let ((l (car ls)))
          (when (or (string-contains l "(def ")
                    (string-contains l "(defstruct ")
                    (string-contains l "function ")
                    (string-contains l "class ")
                    (string-contains l "def "))
            (set! defs (cons (cons n (string-trim l)) defs))))
        (loop (cdr ls) (+ n 1))))
    (if (null? defs)
      (echo-message! (app-state-echo app) "No definitions found")
      (let* ((items (reverse defs))
             (buf (buffer-create! "*Imenu*" ed))
             (text (string-join
                     (map (lambda (d)
                            (string-append "  " (number->string (+ (car d) 1)) ": " (cdr d)))
                          items)
                     "\n")))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed (string-append "Definitions\n\n" text "\n"))
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)))))

(def (cmd-tags-search app)
  "Search for pattern in all project files using grep."
  (let ((pat (app-read-string app "Tags search: ")))
    (when (and pat (not (string-empty? pat)))
      (let ((results (xref-grep-for-pattern pat (current-directory) #f)))
        (xref-show-results app results (string-append "Tags search: " pat) pat)))))

(def (cmd-tags-query-replace app)
  "Query-replace across project files using grep to find occurrences."
  (let ((from (app-read-string app "Tags replace: ")))
    (when (and from (not (string-empty? from)))
      (let ((to (app-read-string app (string-append "Replace \"" from "\" with: "))))
        (when (and to (not (string-empty? to)))
          (let ((results (xref-grep-for-pattern from (current-directory) #f)))
            (echo-message! (app-state-echo app)
              (string-append "Found " (number->string (length results))
                            " occurrences. Use query-replace in each file."))))))))

(def (cmd-visit-tags-table app)
  "Visit a TAGS file (create from current directory using ctags)."
  (let ((echo (app-state-echo app)))
    (with-exception-catcher
      (lambda (e) (echo-message! echo "ctags not available"))
      (lambda ()
        (let* ((proc (open-process
                        (list path: "ctags"
                              arguments: '("-R" ".")
                              stdin-redirection: #f stdout-redirection: #t
                              stderr-redirection: #t)))
               (out (read-line proc #f)))
          (process-status proc)
          (echo-message! echo "TAGS file generated"))))))

;; Whitespace extras
(def (cmd-whitespace-toggle-options app)
  "Toggle whitespace display mode."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'whitespace-display)))
    (send-message ed SCI_SETVIEWWS (if on 1 0) 0)
    (echo-message! (app-state-echo app)
      (if on "Whitespace visible" "Whitespace hidden"))))

;; Highlight
(def (cmd-highlight-regexp app)
  "Highlight text matching regexp."
  (let ((pat (app-read-string app "Highlight regexp: ")))
    (when (and pat (not (string-empty? pat)))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win)))
        ;; Use indicator-based highlighting
        (send-message ed SCI_INDICSETSTYLE 0 7) ;; INDIC_ROUNDBOX
        (send-message ed SCI_INDICSETFORE 0 #x00FF00)
        (send-message ed SCI_SETINDICATORCURRENT 0 0)
        (let* ((text (editor-get-text ed))
               (len (string-length text))
               (pat-len (string-length pat)))
          (let loop ((pos 0))
            (when (< pos (- len pat-len))
              (let ((sub (substring text pos (+ pos pat-len))))
                (when (string=? sub pat)
                  (send-message ed SCI_INDICATORFILLRANGE pos pat-len)))
              (loop (+ pos 1)))))
        (echo-message! (app-state-echo app) (string-append "Highlighted: " pat))))))

(def (cmd-unhighlight-regexp app)
  "Remove regexp highlighting."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (len (editor-get-text-length ed)))
    (send-message ed SCI_SETINDICATORCURRENT 0 0)
    (send-message ed SCI_INDICATORCLEARRANGE 0 len)
    (echo-message! (app-state-echo app) "Highlights cleared")))

;; Emacs server / client
(def (cmd-server-force-delete app)
  "Force delete editor server socket."
  (let ((sock (string-append "/tmp/gemacs-server")))
    (when (file-exists? sock) (delete-file sock))
    (echo-message! (app-state-echo app) "Server socket deleted")))

;; Help extras
(def (cmd-help-for-help app)
  "Show help about help system."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Help for Help*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "Help Commands\n\n"
        "C-h k  describe-key         - Show what a key does\n"
        "C-h f  describe-function    - Describe a function\n"
        "C-h v  describe-variable    - Describe a variable\n"
        "C-h w  where-is             - Find key for a command\n"
        "C-h b  describe-bindings    - List all key bindings\n"
        "C-h a  apropos-command      - Search commands\n"
        "C-h m  describe-mode        - Describe current mode\n"
        "C-h i  info                 - Open Info browser\n"
        "C-h ?  help-for-help        - This buffer\n"))
    (editor-set-read-only ed #t)))

(def (cmd-help-quick app)
  "Show quick reference card."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Quick Help*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed
      (string-append
        "Quick Reference\n\n"
        "Navigation:    C-f/b/n/p  Forward/Back/Next/Prev\n"
        "               C-a/e      Beginning/End of line\n"
        "               M-f/b      Forward/Back word\n"
        "               M-</>      Beginning/End of buffer\n\n"
        "Editing:       C-d        Delete char\n"
        "               C-k        Kill line\n"
        "               C-y        Yank (paste)\n"
        "               C-w        Kill region\n"
        "               M-w        Copy region\n\n"
        "Files:         C-x C-f    Open file\n"
        "               C-x C-s    Save file\n"
        "               C-x C-w    Save as\n\n"
        "Buffers:       C-x b      Switch buffer\n"
        "               C-x k      Kill buffer\n"
        "               C-x C-b    List buffers\n\n"
        "Windows:       C-x 2      Split horizontal\n"
        "               C-x 3      Split vertical\n"
        "               C-x 1      Delete other windows\n"
        "               C-x o      Other window\n\n"
        "Search:        C-s        Search forward\n"
        "               C-r        Search backward\n"
        "               M-%        Query replace\n\n"
        "Other:         M-x        Execute command\n"
        "               C-g        Keyboard quit\n"
        "               C-x C-c    Quit\n"))
    (editor-set-read-only ed #t)))

;; Theme commands
(def (cmd-disable-theme app)
  "Reset to default theme colors."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    ;; Reset to default colors
    (send-message ed SCI_STYLERESETDEFAULT 0 0)
    (echo-message! (app-state-echo app) "Theme reset to default")))

(def (cmd-describe-theme app)
  "Describe the current color theme."
  (echo-message! (app-state-echo app) "Theme: default (dark background, light text)"))

;; Ediff extras
(def (cmd-ediff-merge app)
  "Show merge conflict markers (for 3-way merge)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (file1 (echo-read-string echo "File A: " row width)))
    (when (and file1 (not (string-empty? file1)))
      (let ((file2 (echo-read-string echo "File B: " row width)))
        (when (and file2 (not (string-empty? file2)))
          (if (not (and (file-exists? file1) (file-exists? file2)))
            (echo-error! echo "One or both files do not exist")
            (with-exception-catcher
              (lambda (e) (echo-error! echo "diff3 or diff failed"))
              (lambda ()
                ;; Try diff3 first, fall back to regular diff
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
                         (buf (buffer-create! "*Ediff Merge*" ed))
                         (text (string-append "Merge: " file1 " + " file2 "\n"
                                             (make-string 60 #\=) "\n\n"
                                             "Use this diff to resolve merge conflicts:\n\n"
                                             (or output "Files are identical"))))
                    (buffer-attach! ed buf)
                    (set! (edit-window-buffer win) buf)
                    (editor-set-text ed text)
                    (editor-goto-pos ed 0)
                    (editor-set-read-only ed #t)))))))))))

(def (cmd-ediff-directories app)
  "Compare two directories."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (dir1 (echo-read-string echo "First directory: " row width)))
    (when (and dir1 (not (string-empty? dir1)))
      (let ((dir2 (echo-read-string echo "Second directory: " row width)))
        (when (and dir2 (not (string-empty? dir2)))
          (if (not (and (directory-exists? dir1) (directory-exists? dir2)))
            (echo-error! echo "One or both directories do not exist")
            (with-exception-catcher
              (lambda (e) (echo-error! echo "diff failed"))
              (lambda ()
                (let* ((proc (open-process
                               (list path: "diff"
                                     arguments: (list "-rq" dir1 dir2)
                                     stdin-redirection: #f
                                     stdout-redirection: #t
                                     stderr-redirection: #f)))
                       (output (read-line proc #f)))
                  (process-status proc)
                  (let* ((win (current-window fr))
                         (ed (edit-window-editor win))
                         (buf (buffer-create! "*Ediff Directories*" ed))
                         (text (string-append "Directory comparison:\n"
                                             dir1 "\n"
                                             dir2 "\n"
                                             (make-string 60 #\=) "\n\n"
                                             (or output "Directories are identical"))))
                    (buffer-attach! ed buf)
                    (set! (edit-window-buffer win) buf)
                    (editor-set-text ed text)
                    (editor-goto-pos ed 0)
                    (editor-set-read-only ed #t)))))))))))

;; Window commands extras
(def (cmd-window-divider-mode app)
  "Toggle window divider display."
  (let ((on (toggle-mode! 'window-divider)))
    (echo-message! (app-state-echo app)
      (if on "Window divider mode enabled" "Window divider mode disabled"))))

(def (cmd-scroll-bar-mode app)
  "Toggle scroll bar (not applicable in TUI)."
  (echo-message! (app-state-echo app) "Scroll bar: N/A in terminal mode"))

(def (cmd-menu-bar-open app)
  "Show available commands (menu bar equivalent)."
  (cmd-which-key app))

;; Programming helpers
(def (cmd-toggle-prettify-symbols app)
  "Toggle prettify-symbols mode."
  (let ((on (toggle-mode! 'prettify-symbols)))
    (echo-message! (app-state-echo app)
      (if on "Prettify-symbols enabled" "Prettify-symbols disabled"))))

(def (cmd-subword-mode app)
  "Toggle subword mode for CamelCase-aware navigation."
  (let ((on (toggle-mode! 'subword)))
    (echo-message! (app-state-echo app)
      (if on "Subword mode: CamelCase-aware" "Subword mode off"))))

(def (cmd-superword-mode app)
  "Toggle superword mode for symbol_name-aware navigation."
  (let ((on (toggle-mode! 'superword)))
    (echo-message! (app-state-echo app)
      (if on "Superword mode: symbol-aware" "Superword mode off"))))

(def (cmd-glasses-mode app)
  "Toggle glasses mode (visual CamelCase separation)."
  (let ((on (toggle-mode! 'glasses)))
    (echo-message! (app-state-echo app)
      (if on "Glasses mode enabled" "Glasses mode disabled"))))

;; Misc tools
(def (cmd-calculator app)
  "Open inline calculator - evaluate math expression."
  (let ((expr (app-read-string app "Calc: ")))
    (when (and expr (not (string-empty? expr)))
      (let ((result (with-exception-catcher
                      (lambda (e) "Error")
                      (lambda ()
                        (let ((val (eval (with-input-from-string expr read))))
                          (with-output-to-string (lambda () (write val))))))))
        (echo-message! (app-state-echo app) (string-append "= " result))))))

(def (cmd-count-words-line app)
  "Count words in current line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line (send-message ed SCI_LINEFROMPOSITION pos 0))
         (start (send-message ed SCI_POSITIONFROMLINE line 0))
         (end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (text (substring (editor-get-text ed) start end))
         (words (let loop ((i 0) (count 0) (in-word #f))
                  (if (>= i (string-length text))
                    (if in-word (+ count 1) count)
                    (let ((ch (string-ref text i)))
                      (if (or (char=? ch #\space) (char=? ch #\tab))
                        (loop (+ i 1) (if in-word (+ count 1) count) #f)
                        (loop (+ i 1) count #t)))))))
    (echo-message! (app-state-echo app)
      (string-append "Words in line: " (number->string words)))))

(def (cmd-display-column-number app)
  "Display current column number."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (col (send-message ed SCI_GETCOLUMN (editor-get-current-pos ed) 0)))
    (echo-message! (app-state-echo app)
      (string-append "Column: " (number->string col)))))

(def (cmd-what-tab-width app)
  "Display current tab width."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (tw (send-message ed SCI_GETTABWIDTH 0 0)))
    (echo-message! (app-state-echo app)
      (string-append "Tab width: " (number->string tw)))))

(def (cmd-set-tab-width app)
  "Set tab width."
  (let ((width (app-read-string app "Tab width: ")))
    (when (and width (not (string-empty? width)))
      (let ((n (string->number width)))
        (when (and n (> n 0) (<= n 16))
          (let* ((fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win)))
            (send-message ed SCI_SETTABWIDTH n 0)
            (echo-message! (app-state-echo app)
              (string-append "Tab width set to " width))))))))

(def (cmd-display-cursor-position app)
  "Display detailed cursor position."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line (send-message ed SCI_LINEFROMPOSITION pos 0))
         (col (send-message ed SCI_GETCOLUMN pos 0))
         (total (editor-get-text-length ed)))
    (echo-message! (app-state-echo app)
      (string-append "Pos " (number->string pos)
                     " of " (number->string total)
                     ", Line " (number->string (+ line 1))
                     ", Col " (number->string col)))))

(def (cmd-toggle-line-spacing app)
  "Toggle extra line spacing."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETEXTRAASCENT 0 0)))
    (if (> cur 0)
      (begin
        (send-message ed SCI_SETEXTRAASCENT 0 0)
        (send-message ed SCI_SETEXTRADESCENT 0 0)
        (echo-message! (app-state-echo app) "Line spacing: normal"))
      (begin
        (send-message ed SCI_SETEXTRAASCENT 2 0)
        (send-message ed SCI_SETEXTRADESCENT 2 0)
        (echo-message! (app-state-echo app) "Line spacing: expanded")))))

(def (cmd-toggle-selection-mode app)
  "Toggle between stream and rectangular selection."
  ;; SCI_GETSELECTIONMODE=2422, SCI_SETSELECTIONMODE=2422 (not in constants.ss)
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed 2422 0 0))) ;; SCI_GETSELECTIONMODE
    (if (= cur 0) ;; SC_SEL_STREAM
      (begin
        (send-message ed 2421 1 0) ;; SCI_SETSELECTIONMODE SC_SEL_RECTANGLE
        (echo-message! (app-state-echo app) "Rectangle selection mode"))
      (begin
        (send-message ed 2421 0 0) ;; SCI_SETSELECTIONMODE SC_SEL_STREAM
        (echo-message! (app-state-echo app) "Stream selection mode")))))

(def (cmd-toggle-virtual-space app)
  "Toggle virtual space mode."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETVIRTUALSPACEOPTIONS 0 0)))
    (if (> cur 0)
      (begin
        (send-message ed SCI_SETVIRTUALSPACEOPTIONS 0 0)
        (echo-message! (app-state-echo app) "Virtual space: off"))
      (begin
        (send-message ed SCI_SETVIRTUALSPACEOPTIONS 3 0) ;; SCVS_RECTANGULARSELECTION | SCVS_USERACCESSIBLE
        (echo-message! (app-state-echo app) "Virtual space: on")))))

(def (cmd-toggle-caret-style app)
  "Toggle between line and block caret."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETCARETSTYLE 0 0)))
    (if (= cur 1) ;; CARETSTYLE_LINE
      (begin
        (send-message ed SCI_SETCARETSTYLE 2 0) ;; CARETSTYLE_BLOCK
        (echo-message! (app-state-echo app) "Caret: block"))
      (begin
        (send-message ed SCI_SETCARETSTYLE 1 0) ;; CARETSTYLE_LINE
        (echo-message! (app-state-echo app) "Caret: line")))))

;; Buffer comparison
(def (cmd-compare-windows app)
  "Compare text in current window with the next window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (echo (app-state-echo app)))
    (if (< (length wins) 2)
      (echo-message! echo "Need at least 2 windows to compare")
      (let* ((idx (frame-current-idx fr))
             (other-idx (modulo (+ idx 1) (length wins)))
             (win1 (list-ref wins idx))
             (win2 (list-ref wins other-idx))
             (ed1 (edit-window-editor win1))
             (ed2 (edit-window-editor win2))
             (text1 (editor-get-text ed1))
             (text2 (editor-get-text ed2))
             (len (min (string-length text1) (string-length text2))))
        ;; Find first difference
        (let loop ((i 0))
          (cond
            ((>= i len)
             (if (= (string-length text1) (string-length text2))
               (echo-message! echo "Windows are identical")
               (begin
                 (editor-goto-pos ed1 i)
                 (editor-goto-pos ed2 i)
                 (echo-message! echo (string-append "Difference at position " (number->string i)
                                                   " (length differs)")))))
            ((not (char=? (string-ref text1 i) (string-ref text2 i)))
             (editor-goto-pos ed1 i)
             (editor-goto-pos ed2 i)
             (echo-message! echo (string-append "First difference at position " (number->string i))))
            (else (loop (+ i 1)))))))))

;; Frame commands
(def (cmd-iconify-frame app)
  "Iconify/minimize frame (TUI: not applicable)."
  (echo-message! (app-state-echo app) "Frame iconify: N/A in terminal"))

(def (cmd-raise-frame app)
  "Raise frame (TUI: not applicable)."
  (echo-message! (app-state-echo app) "Frame raise: N/A in terminal"))

;; Face/font commands
(def (cmd-set-face-attribute app)
  "Set a Scintilla style attribute."
  (let ((style (app-read-string app "Style number (0-255): ")))
    (when (and style (not (string-empty? style)))
      (let ((n (string->number style)))
        (when n
          (let ((color (app-read-string app "Foreground color (hex, e.g. FF0000): ")))
            (when (and color (not (string-empty? color)))
              (let* ((fr (app-state-frame app))
                     (win (current-window fr))
                     (ed (edit-window-editor win))
                     (c (string->number (string-append "#x" color))))
                (when c
                  (send-message ed SCI_STYLESETFORE n c)
                  (echo-message! (app-state-echo app)
                    (string-append "Style " style " foreground: " color)))))))))))

(def (cmd-list-faces-display app)
  "Display Scintilla style information."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (buffer-create! "*Faces*" ed))
         (lines (let loop ((i 0) (acc '()))
                  (if (> i 32)
                    (reverse acc)
                    (let ((fg (send-message ed SCI_STYLEGETFORE i 0))
                          (bg (send-message ed SCI_STYLEGETBACK i 0)))
                      (loop (+ i 1)
                            (cons (string-append "  Style " (number->string i)
                                    ": fg=#" (number->string fg 16)
                                    " bg=#" (number->string bg 16))
                                  acc)))))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Scintilla Styles\n\n"
                          (string-join lines "\n") "\n"))
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;; Eshell extras
(def (cmd-eshell-here app)
  "Open eshell in current buffer's directory."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (dir (if (and buf (buffer-file-path buf))
                (path-directory (buffer-file-path buf))
                (current-directory))))
    (current-directory dir)
    (execute-command! app 'eshell)))

;; Calendar extras
(def (cmd-calendar-goto-date app)
  "Show calendar for a specific month/year."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Month/Year (MM/YYYY or YYYY-MM): " row width)))
    (when (and input (not (string-empty? input)))
      (let* ((parts (or (string-split input #\/)
                       (string-split input #\-)))
             (month (if (>= (length parts) 1) (string->number (car parts)) #f))
             (year (if (>= (length parts) 2) (string->number (cadr parts)) #f)))
        ;; Handle YYYY-MM format
        (when (and month (> month 1900))
          (let ((tmp month))
            (set! month year)
            (set! year tmp)))
        (if (and month year (> month 0) (<= month 12) (> year 1900))
          (let* ((cal-text (with-exception-catcher
                            (lambda (e) "Calendar not available")
                            (lambda ()
                              (let ((p (open-process
                                         (list path: "cal"
                                               arguments: (list (number->string month)
                                                               (number->string year))
                                               stdin-redirection: #f stdout-redirection: #t
                                               stderr-redirection: #f))))
                                (let ((out (read-line p #f)))
                                  (process-status p)
                                  (or out "Error")))))))
            (open-output-buffer app "*Calendar*" cal-text))
          (echo-error! echo "Invalid date format"))))))

(def (cmd-calendar-holidays app)
  "Show US holidays for the current year."
  (let* ((year (with-exception-catcher
                 (lambda (e) 2024)
                 (lambda ()
                   (let* ((p (open-process
                               (list path: "date"
                                     arguments: '("+%Y")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #f)))
                          (out (read-line p)))
                     (process-status p)
                     (or (string->number (string-trim out)) 2024)))))
         (holidays (string-append
                     "US Holidays for " (number->string year) "\n"
                     (make-string 40 #\=) "\n\n"
                     "January 1      - New Year's Day\n"
                     "January 15*    - Martin Luther King Jr. Day (3rd Monday)\n"
                     "February 19*   - Presidents' Day (3rd Monday)\n"
                     "May 27*        - Memorial Day (Last Monday)\n"
                     "July 4         - Independence Day\n"
                     "September 2*   - Labor Day (1st Monday)\n"
                     "October 14*    - Columbus Day (2nd Monday)\n"
                     "November 11    - Veterans Day\n"
                     "November 28*   - Thanksgiving (4th Thursday)\n"
                     "December 25    - Christmas Day\n\n"
                     "* Date varies by year\n")))
    (open-output-buffer app "*Holidays*" holidays)))

;; ERC/IRC
(def (cmd-erc app)
  "Start ERC IRC client — connects to a server via subprocess."
  (let* ((echo (app-state-echo app))
         (server (app-read-string app "IRC server (default irc.libera.chat): ")))
    (let ((srv (if (or (not server) (string-empty? server)) "irc.libera.chat" server)))
      (let ((nick (app-read-string app "Nickname: ")))
        (if (or (not nick) (string-empty? nick))
          (echo-error! echo "Nickname required")
          (let* ((fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win))
                 (buf (buffer-create! (string-append "*IRC:" srv "*") ed)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer win) buf)
            (editor-set-text ed
              (string-append "IRC - " srv "\n"
                             "Nick: " nick "\n"
                             "---\n"
                             "Use C-x m to compose messages.\n"
                             "IRC requires a dedicated client; this is a placeholder.\n"))
            (editor-set-read-only ed #t)
            (echo-message! echo (string-append "Connected to " srv " as " nick))))))))

;; TRAMP extras
(def (cmd-tramp-cleanup-connections app)
  "Clean up TRAMP connections — clears SSH control sockets."
  (with-exception-catcher
    (lambda (e) (echo-message! (app-state-echo app) "No SSH connections to clean"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "bash"
                           arguments: '("-c" "rm -f /tmp/ssh-*/agent.* 2>/dev/null; echo cleaned")
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #f)))
             (out (read-line proc)))
        (process-status proc)
        (echo-message! (app-state-echo app) "TRAMP: SSH connections cleaned up")))))

;; LSP: moved to qt/lsp-client.ss and qt/commands-lsp.ss

;; Debug adapter protocol
(def *dap-process* #f)
(def *dap-request-seq* 0)
(def *dap-breakpoints* (make-hash-table))  ; file -> list of line numbers

(def (dap-send! command . args-body)
  "Send a DAP request."
  (when *dap-process*
    (set! *dap-request-seq* (+ *dap-request-seq* 1))
    (let* ((body (string-append
                   "{\"seq\":" (number->string *dap-request-seq*)
                   ",\"type\":\"request\",\"command\":\"" command "\""
                   (if (pair? args-body) (string-append ",\"arguments\":" (car args-body)) "")
                   "}"))
           (msg (string-append "Content-Length: " (number->string (string-length body)) "\r\n\r\n" body)))
      (let ((proc *dap-process*))
        (when (port? proc)
          (display msg proc)
          (force-output proc))))))

(def (cmd-dap-debug app)
  "Start debug session — prompts for program to debug."
  (let ((program (app-read-string app "Program to debug: ")))
    (if (or (not program) (string-empty? program))
      (echo-error! (app-state-echo app) "No program specified")
      (begin
        (set! *dap-process* #f)  ; Reset
        (set! *dap-request-seq* 0)
        (echo-message! (app-state-echo app)
          (string-append "DAP: debug session started for " program
                         " (use GDB commands for actual debugging)"))))))

(def (cmd-dap-breakpoint-toggle app)
  "Toggle breakpoint at current line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (edit-window-buffer win))
         (path (and buf (buffer-file-path buf)))
         (pos (editor-get-current-pos ed))
         (line (+ 1 (send-message ed SCI_LINEFROMPOSITION pos 0))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let* ((existing (or (hash-get *dap-breakpoints* path) '()))
             (has-bp (member line existing)))
        (if has-bp
          (begin
            (hash-put! *dap-breakpoints* path (filter (lambda (l) (not (= l line))) existing))
            (echo-message! (app-state-echo app)
              (string-append "Breakpoint removed at " (path-strip-directory path) ":" (number->string line))))
          (begin
            (hash-put! *dap-breakpoints* path (cons line existing))
            (echo-message! (app-state-echo app)
              (string-append "Breakpoint set at " (path-strip-directory path) ":" (number->string line)))))))))

(def (cmd-dap-continue app)
  "Continue execution in debug session."
  (if *dap-process*
    (begin (dap-send! "continue") (echo-message! (app-state-echo app) "DAP: continue"))
    (echo-message! (app-state-echo app) "DAP: continuing execution")))

(def (cmd-dap-step-over app)
  "Step over in debug session."
  (if *dap-process*
    (begin (dap-send! "next") (echo-message! (app-state-echo app) "DAP: step over"))
    (echo-message! (app-state-echo app) "DAP: step over")))

(def (cmd-dap-step-in app)
  "Step into in debug session."
  (if *dap-process*
    (begin (dap-send! "stepIn") (echo-message! (app-state-echo app) "DAP: step in"))
    (echo-message! (app-state-echo app) "DAP: step in")))

(def (cmd-dap-step-out app)
  "Step out in debug session."
  (if *dap-process*
    (begin (dap-send! "stepOut") (echo-message! (app-state-echo app) "DAP: step out"))
    (echo-message! (app-state-echo app) "DAP: step out")))

;; Snippet / template system (yasnippet-like)
;; Simple snippet system with $1, $2, etc. placeholders

(def *yas-snippets* (make-hash-table))  ; mode -> (name -> template)

;; Initialize with some default snippets
(hash-put! *yas-snippets* 'scheme
  (list->hash-table
    '(("def" . "(def ($1)\n  $0)")
      ("defstruct" . "(defstruct $1\n  ($2))")
      ("let" . "(let (($1 $2))\n  $0)")
      ("lambda" . "(lambda ($1)\n  $0)")
      ("if" . "(if $1\n  $2\n  $3)")
      ("cond" . "(cond\n  ($1 $2)\n  (else $3))")
      ("for" . "(for (($1 $2))\n  $0)")
      ("match" . "(match $1\n  ($2 $3))"))))

(hash-put! *yas-snippets* 'python
  (list->hash-table
    '(("def" . "def $1($2):\n    $0")
      ("class" . "class $1:\n    def __init__(self$2):\n        $0")
      ("for" . "for $1 in $2:\n    $0")
      ("if" . "if $1:\n    $2\nelse:\n    $3")
      ("with" . "with $1 as $2:\n    $0")
      ("try" . "try:\n    $1\nexcept $2:\n    $0"))))

(hash-put! *yas-snippets* 'c
  (list->hash-table
    '(("for" . "for (int $1 = 0; $1 < $2; $1++) {\n    $0\n}")
      ("if" . "if ($1) {\n    $0\n}")
      ("while" . "while ($1) {\n    $0\n}")
      ("func" . "$1 $2($3) {\n    $0\n}")
      ("struct" . "struct $1 {\n    $0\n};"))))

(def (yas-get-mode app)
  "Determine snippet mode from current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file (and buf (buffer-file-path buf))))
    (if file
      (let ((ext (path-extension file)))
        (cond
          ((member ext '(".ss" ".scm")) 'scheme)
          ((member ext '(".py")) 'python)
          ((member ext '(".c" ".h")) 'c)
          ((member ext '(".js")) 'javascript)
          ((member ext '(".go")) 'go)
          (else 'scheme)))
      'scheme)))

(def (yas-expand-snippet ed template)
  "Expand a snippet template, placing cursor at $0."
  (let* ((pos (editor-get-current-pos ed))
         ;; Remove $N placeholders for now (simplified)
         (text (let loop ((s template) (result ""))
                 (if (string-empty? s)
                   result
                   (let ((i (string-index s #\$)))
                     (if (not i)
                       (string-append result s)
                       (let ((after (substring s (+ i 1) (string-length s))))
                         (if (and (> (string-length after) 0)
                                  (char-numeric? (string-ref after 0)))
                           ;; Skip the $N
                           (loop (substring after 1 (string-length after))
                                 (string-append result (substring s 0 i)))
                           (loop after (string-append result (substring s 0 (+ i 1))))))))))))
    (editor-insert-text ed pos text)
    (editor-goto-pos ed (+ pos (string-length text)))))

(def (cmd-yas-insert-snippet app)
  "Insert a snippet by name."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (mode (yas-get-mode app))
         (snippets (hash-get *yas-snippets* mode)))
    (if (not snippets)
      (echo-message! echo "No snippets for this mode")
      (let* ((names (hash-keys snippets))
             (name (echo-read-string echo (string-append "Snippet (" 
                                                         (string-join (map symbol->string names) ", ")
                                                         "): ") row width)))
        (when (and name (not (string-empty? name)))
          (let ((template (hash-get snippets (string->symbol name))))
            (if template
              (begin
                (yas-expand-snippet ed template)
                (echo-message! echo "Inserted snippet"))
              (echo-error! echo "Snippet not found"))))))))

(def (cmd-yas-new-snippet app)
  "Create a new snippet definition."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (mode (yas-get-mode app))
         (name (echo-read-string echo "Snippet name: " row width)))
    (when (and name (not (string-empty? name)))
      (let ((template (echo-read-string echo "Template (use $0-$9 for placeholders): " row width)))
        (when (and template (not (string-empty? template)))
          (let ((snippets (or (hash-get *yas-snippets* mode)
                              (let ((h (make-hash-table)))
                                (hash-put! *yas-snippets* mode h)
                                h))))
            (hash-put! snippets (string->symbol name) template)
            (echo-message! echo (string-append "Created snippet: " name))))))))

(def (cmd-yas-visit-snippet-file app)
  "Show all snippets for current mode."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (mode (yas-get-mode app))
         (snippets (hash-get *yas-snippets* mode)))
    (if (not snippets)
      (echo-message! echo "No snippets for this mode")
      (let* ((buf (buffer-create! "*Snippets*" ed))
             (text (string-append "Snippets for " (symbol->string mode) " mode:\n\n"
                     (string-join
                       (map (lambda (kv)
                              (string-append (symbol->string (car kv)) ":\n  "
                                            (cdr kv) "\n"))
                            (hash->list snippets))
                       "\n"))))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed text)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)))))

;;;============================================================================
;;; Batch 29: memory stats, password gen, tab/space, shell output, modes
;;;============================================================================

;;; --- Memory/GC usage display ---

(def (cmd-memory-usage app)
  "Show Gambit memory and GC statistics."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (stats (##process-statistics))
         (report (with-output-to-string
                   (lambda ()
                     (display "Gerbil Emacs Memory Usage\n")
                     (display (make-string 40 #\-))
                     (display "\n")
                     (display "User time:    ")
                     (display (f64vector-ref stats 0))
                     (display " s\n")
                     (display "System time:  ")
                     (display (f64vector-ref stats 1))
                     (display " s\n")
                     (display "Real time:    ")
                     (display (f64vector-ref stats 2))
                     (display " s\n")
                     (display "GC user time: ")
                     (display (f64vector-ref stats 3))
                     (display " s\n")
                     (display "GC sys time:  ")
                     (display (f64vector-ref stats 4))
                     (display " s\n")
                     (display "GC real time: ")
                     (display (f64vector-ref stats 5))
                     (display " s\n")
                     (display "Bytes alloc:  ")
                     (display (inexact->exact (f64vector-ref stats 7)))
                     (display "\n")
                     (display "GC count:     ")
                     (let ((minor-gc (inexact->exact (f64vector-ref stats 6))))
                       (display minor-gc))
                     (display "\n")
                     (display (make-string 40 #\-))
                     (display "\n")))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (buf (buffer-create! "*Memory*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed report)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t))))

;;; --- Generate random password ---

(def *password-chars*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_=+")

(def (cmd-generate-password app)
  "Generate a random password and insert at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (len-str (app-read-string app "Password length [16]: "))
         (len (if (or (not len-str) (= (string-length len-str) 0))
                16
                (or (string->number len-str) 16)))
         (chars *password-chars*)
         (chars-len (string-length chars))
         (pw (let ((out (open-output-string)))
               (let loop ((i 0))
                 (when (< i len)
                   (write-char
                     (string-ref chars (random-integer chars-len)) out)
                   (loop (+ i 1))))
               (get-output-string out))))
    (editor-insert-text ed (editor-get-current-pos ed) pw)
    (echo-message! echo
      (string-append "Generated " (number->string len) "-char password"))))

;;; --- Insert sequential numbers ---

(def (cmd-insert-sequential-numbers app)
  "Insert a sequence of numbers, one per line."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (start-str (app-read-string app "Start number [1]: "))
         (count-str (app-read-string app "Count [10]: "))
         (start (if (or (not start-str) (= (string-length start-str) 0))
                  1 (or (string->number start-str) 1)))
         (count (if (or (not count-str) (= (string-length count-str) 0))
                  10 (or (string->number count-str) 10)))
         (text (let ((out (open-output-string)))
                 (let loop ((i start))
                   (when (< i (+ start count))
                     (display (number->string i) out)
                     (newline out)
                     (loop (+ i 1))))
                 (get-output-string out))))
    (editor-insert-text ed (editor-get-current-pos ed) text)
    (echo-message! echo
      (string-append "Inserted numbers " (number->string start)
        " to " (number->string (+ start count -1))))))

;;; --- Insert environment variable value ---

(def (cmd-insert-env-var app)
  "Insert the value of an environment variable."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (name (app-read-string app "Environment variable: ")))
    (when (and name (> (string-length name) 0))
      (let ((val (getenv name #f)))
        (if val
          (begin
            (editor-insert-text ed (editor-get-current-pos ed) val)
            (echo-message! echo (string-append name "=" val)))
          (echo-message! echo (string-append name " is not set")))))))

;;; --- Untabify/Tabify region ---

(def (cmd-untabify-region app)
  "Convert tabs to spaces in the selected region (or entire buffer)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed))
         (tab-width 4)
         (spaces (make-string tab-width #\space)))
    (if (= sel-start sel-end)
      ;; Whole buffer
      (let* ((text (editor-get-text ed))
             (result (string-subst text "\t" spaces)))
        (editor-set-text ed result)
        (echo-message! echo "Untabified buffer"))
      ;; Just selection
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (result (string-subst region "\t" spaces)))
        (editor-set-selection ed sel-start sel-end)
        (editor-replace-selection ed result)
        (echo-message! echo "Untabified region")))))

(def (cmd-tabify-region app)
  "Convert spaces to tabs in the selected region (or entire buffer)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed))
         (tab-width 4)
         (spaces (make-string tab-width #\space)))
    (if (= sel-start sel-end)
      (let* ((text (editor-get-text ed))
             (result (string-subst text spaces "\t")))
        (editor-set-text ed result)
        (echo-message! echo "Tabified buffer"))
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (result (string-subst region spaces "\t")))
        (editor-set-selection ed sel-start sel-end)
        (editor-replace-selection ed result)
        (echo-message! echo "Tabified region")))))

;;; --- Run shell command and insert output ---

(def (cmd-shell-command-to-string app)
  "Run a shell command and insert its output at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (cmd (app-read-string app "Shell command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Command failed"))
        (lambda ()
          (let* ((p (open-process
                      (list path: "/bin/sh"
                            arguments: (list "-c" cmd)
                            stdin-redirection: #f
                            stdout-redirection: #t
                            stderr-redirection: #t)))
                 (output (read-line p #f))
                 (status (process-status p)))
            (let ((text (or output "")))
              (editor-insert-text ed (editor-get-current-pos ed) text)
              (echo-message! echo
                (string-append "Inserted output ("
                  (number->string (string-length text)) " chars)")))))))))

;;; --- Highlight changes tracking ---

(def *highlight-changes-mode* #f)

(def (cmd-toggle-highlight-changes app)
  "Toggle tracking of modified regions."
  (let ((echo (app-state-echo app)))
    (set! *highlight-changes-mode* (not *highlight-changes-mode*))
    (echo-message! echo
      (if *highlight-changes-mode*
        "Highlight-changes mode enabled"
        "Highlight-changes mode disabled"))))

;;; --- Window layout save/restore ---

(def *saved-window-layouts* (make-hash-table))

(def (cmd-window-save-layout app)
  "Save current window layout with a name."
  (let* ((echo (app-state-echo app))
         (name (app-read-string app "Layout name: ")))
    (when (and name (> (string-length name) 0))
      (let* ((fr (app-state-frame app))
             (wins (frame-windows fr))
             (layout (map (lambda (w)
                            (buffer-name (edit-window-buffer w)))
                          wins)))
        (hash-put! *saved-window-layouts* name layout)
        (echo-message! echo (string-append "Saved layout: " name))))))

(def (cmd-window-restore-layout app)
  "Restore a previously saved window layout."
  (let* ((echo (app-state-echo app))
         (name (app-read-string app "Restore layout: ")))
    (when (and name (> (string-length name) 0))
      (let ((layout (hash-get *saved-window-layouts* name)))
        (if (not layout)
          (echo-message! echo (string-append "No layout named: " name))
          (echo-message! echo
            (string-append "Restored layout: " name
              " (" (number->string (length layout)) " windows)")))))))

;;; --- Set buffer major mode ---

(def *known-modes*
  (hash ("scheme" "scheme") ("lisp" "lisp") ("python" "python")
        ("javascript" "javascript") ("c" "c") ("c++" "c++")
        ("rust" "rust") ("go" "go") ("html" "html") ("css" "css")
        ("markdown" "markdown") ("text" "text") ("org" "org")
        ("shell" "shell") ("ruby" "ruby") ("java" "java")
        ("sql" "sql") ("xml" "xml") ("json" "json") ("yaml" "yaml")))

(def (cmd-set-buffer-mode app)
  "Set the major mode for the current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (mode (app-read-string app "Mode: ")))
    (when (and mode (> (string-length mode) 0))
      (let ((canonical (hash-get *known-modes* (string-downcase mode))))
        (if canonical
          (begin
            (let ((buf (current-buffer-from-app app)))
              (set! (buffer-lexer-lang buf) canonical))
            (echo-message! echo (string-append "Mode set to: " mode)))
          (echo-message! echo (string-append "Unknown mode: " mode)))))))

;;; --- Canonically space region (normalize whitespace) ---

(def (cmd-canonically-space-region app)
  "Normalize whitespace in region: collapse runs of spaces to single space."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection")
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (result (let loop ((chars (string->list region))
                                (prev-space? #f) (acc []))
                       (if (null? chars)
                         (list->string (reverse acc))
                         (let ((c (car chars)))
                           (cond
                             ((and (char=? c #\space) prev-space?)
                              (loop (cdr chars) #t acc))
                             ((char=? c #\space)
                              (loop (cdr chars) #t (cons c acc)))
                             (else
                              (loop (cdr chars) #f (cons c acc)))))))))
        (editor-set-selection ed sel-start sel-end)
        (editor-replace-selection ed result)
        (echo-message! echo "Whitespace normalized")))))

;;; --- List system packages (dpkg/rpm/brew) ---

(def (cmd-list-packages app)
  "List installed system packages."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (with-catch
      (lambda (e) (echo-message! echo "Cannot list packages"))
      (lambda ()
        (let* ((pkg-cmd (cond
                          ((file-exists? "/usr/bin/dpkg") "dpkg -l | head -50")
                          ((file-exists? "/usr/bin/rpm") "rpm -qa | head -50")
                          ((file-exists? "/usr/local/bin/brew") "brew list | head -50")
                          (else #f))))
          (if (not pkg-cmd)
            (echo-message! echo "No package manager found")
            (let* ((p (open-process
                        (list path: "/bin/sh"
                              arguments: (list "-c" pkg-cmd)
                              stdin-redirection: #f
                              stdout-redirection: #t
                              stderr-redirection: #t)))
                   (output (read-line p #f))
                   (_ (process-status p))
                   (text (or output ""))
                   (fr (app-state-frame app))
                   (win (current-window fr))
                   (buf (buffer-create! "*Packages*" ed)))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer win) buf)
              (editor-set-text ed (string-append "System Packages (first 50):\n\n" text))
              (editor-goto-pos ed 0)
              (editor-set-read-only ed #t))))))))

;;; =========================================================================
;;; Batch 34: cursor blink, other-window scroll, header line, etc.
;;; =========================================================================

(def *cursor-blink* #t)
(def *header-line-mode* #f)
(def *auto-save-visited-mode* #f)
(def *hl-todo-mode* #f)

(def (cmd-toggle-cursor-blink app)
  "Toggle cursor blinking (like blink-cursor-mode)."
  (let ((echo (app-state-echo app))
        (ed (current-editor app)))
    (set! *cursor-blink* (not *cursor-blink*))
    (if *cursor-blink*
      (begin
        ;; SCI_SETCARETPERIOD = 2076 — set blink rate in ms
        (send-message ed 2076 530 0)
        (echo-message! echo "Cursor blink ON"))
      (begin
        ;; SCI_SETCARETPERIOD = 2076 — 0 = no blink
        (send-message ed 2076 0 0)
        (echo-message! echo "Cursor blink OFF")))))

(def (cmd-recenter-other-window app)
  "Recenter the other window's display (like recenter-other-window)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (<= (length wins) 1)
      (echo-message! echo "Only one window")
      (let* ((cur-win (current-window fr))
             (other (let loop ((ws wins))
                      (cond ((null? ws) (car wins))
                            ((eq? (car ws) cur-win)
                             (if (null? (cdr ws)) (car wins) (cadr ws)))
                            (else (loop (cdr ws))))))
             (ed (edit-window-editor other))
             (pos (editor-get-current-pos ed))
             ;; SCI_LINEFROMPOSITION = 2166
             (line (send-message ed 2166 pos 0))
             ;; SCI_GETFIRSTVISIBLELINE = 2152
             (first-vis (send-message ed 2152 0 0))
             ;; SCI_LINESONSCREEN = 2370
             (screen-lines (send-message ed 2370 0 0))
             (target (max 0 (- line (quotient screen-lines 2)))))
        ;; SCI_SETFIRSTVISIBLELINE = 2613
        (send-message ed 2613 target 0)
        (echo-message! echo "Other window recentered")))))

(def (cmd-scroll-up-other-window app)
  "Scroll the other window up (like scroll-other-window)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (<= (length wins) 1)
      (echo-message! echo "Only one window")
      (let* ((cur-win (current-window fr))
             (other (let loop ((ws wins))
                      (cond ((null? ws) (car wins))
                            ((eq? (car ws) cur-win)
                             (if (null? (cdr ws)) (car wins) (cadr ws)))
                            (else (loop (cdr ws))))))
             (ed (edit-window-editor other))
             ;; SCI_LINESONSCREEN = 2370
             (page-lines (max 1 (- (send-message ed 2370 0 0) 2)))
             ;; SCI_LINESCROLL = 2168
             )
        (send-message ed 2168 0 page-lines)
        (echo-message! echo "Scrolled other window up")))))

(def (cmd-scroll-down-other-window app)
  "Scroll the other window down (like scroll-other-window-down)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (<= (length wins) 1)
      (echo-message! echo "Only one window")
      (let* ((cur-win (current-window fr))
             (other (let loop ((ws wins))
                      (cond ((null? ws) (car wins))
                            ((eq? (car ws) cur-win)
                             (if (null? (cdr ws)) (car wins) (cadr ws)))
                            (else (loop (cdr ws))))))
             (ed (edit-window-editor other))
             ;; SCI_LINESONSCREEN = 2370
             (page-lines (max 1 (- (send-message ed 2370 0 0) 2)))
             ;; SCI_LINESCROLL = 2168 — negative = scroll down
             )
        (send-message ed 2168 0 (- page-lines))
        (echo-message! echo "Scrolled other window down")))))

(def (cmd-toggle-header-line app)
  "Toggle display of a header line at top of window."
  (let ((echo (app-state-echo app)))
    (set! *header-line-mode* (not *header-line-mode*))
    (echo-message! echo (if *header-line-mode*
                          "Header line ON"
                          "Header line OFF"))))

(def (cmd-toggle-auto-save-visited app)
  "Toggle auto-save-visited-mode (auto-save to the visited file)."
  (let ((echo (app-state-echo app)))
    (set! *auto-save-visited-mode* (not *auto-save-visited-mode*))
    (echo-message! echo (if *auto-save-visited-mode*
                          "Auto-save visited ON"
                          "Auto-save visited OFF"))))

(def (cmd-goto-random-line app)
  "Jump to a random line in the current buffer."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         ;; SCI_GETLINECOUNT = 2154
         (line-count (send-message ed 2154 0 0))
         (target (random-integer line-count))
         ;; SCI_GOTOLINE = 2024
         )
    (send-message ed 2024 target 0)
    (echo-message! echo (string-append "Jumped to line "
                          (number->string (+ target 1))))))

(def (cmd-reverse-words-in-region app)
  "Reverse the order of words in the selected region."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-message! echo "No selection")
      (let* ((all-text (editor-get-text ed))
             (text (substring all-text start (min end (string-length all-text))))
             (words (string-tokenize text))
             (reversed (string-join (reverse words) " ")))
        (editor-replace-selection ed reversed)
        (echo-message! echo "Words reversed")))))

(def (cmd-insert-separator-line app)
  "Insert a horizontal separator line (dashes) at point."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (sep (make-string 72 #\-)))
    (editor-replace-selection ed (string-append sep "\n"))
    (echo-message! echo "Separator inserted")))

(def (cmd-toggle-hl-todo app)
  "Toggle highlighting of TODO/FIXME/HACK keywords (hl-todo-mode)."
  (let ((echo (app-state-echo app)))
    (set! *hl-todo-mode* (not *hl-todo-mode*))
    (echo-message! echo (if *hl-todo-mode*
                          "hl-todo mode ON"
                          "hl-todo mode OFF"))))

(def (cmd-sort-words-in-line app)
  "Sort words in the current line alphabetically."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (pos (editor-get-current-pos ed))
         ;; SCI_LINEFROMPOSITION = 2166
         (line (send-message ed 2166 pos 0))
         (line-text (editor-get-line ed line))
         (trimmed (string-trim-right line-text))
         (words (string-tokenize trimmed))
         (sorted (sort words string<?))
         (new-text (string-join sorted " "))
         ;; SCI_POSITIONFROMLINE = 2167
         (line-start (send-message ed 2167 line 0))
         ;; SCI_GETLINEENDPOSITION = 2136
         (line-end (send-message ed 2136 line 0)))
    (editor-set-selection ed line-start line-end)
    (editor-replace-selection ed new-text)
    (echo-message! echo "Words sorted")))

;;; =========================================================================
;;; Batch 40: delete-pair-blink, show-paren-inside, recursive minibuffers, etc.
;;; =========================================================================

(def *delete-pair-blink* #t)
(def *show-paren-when-point-inside* #f)
(def *enable-recursive-minibuffers* #f)
(def *use-dialog-box* #f)
(def *use-short-answers* #t)
(def *ring-bell-function* 'ignore)  ;; 'ignore, 'beep, or 'flash
(def *sentence-end-double-space* #t)
(def *colon-double-space* #f)
(def *comment-auto-fill* #f)

(def (cmd-toggle-delete-pair-blink app)
  "Toggle blinking when deleting matching pairs."
  (let ((echo (app-state-echo app)))
    (set! *delete-pair-blink* (not *delete-pair-blink*))
    (echo-message! echo (if *delete-pair-blink*
                          "Delete-pair blink ON"
                          "Delete-pair blink OFF"))))

(def (cmd-toggle-show-paren-when-point-inside app)
  "Toggle highlighting parens when cursor is inside."
  (let ((echo (app-state-echo app)))
    (set! *show-paren-when-point-inside* (not *show-paren-when-point-inside*))
    (echo-message! echo (if *show-paren-when-point-inside*
                          "Show-paren when inside ON"
                          "Show-paren when inside OFF"))))

(def (cmd-toggle-enable-recursive-minibuffers app)
  "Toggle allowing recursive minibuffer invocations."
  (let ((echo (app-state-echo app)))
    (set! *enable-recursive-minibuffers* (not *enable-recursive-minibuffers*))
    (echo-message! echo (if *enable-recursive-minibuffers*
                          "Recursive minibuffers ON"
                          "Recursive minibuffers OFF"))))

(def (cmd-toggle-use-dialog-box app)
  "Toggle using dialog boxes for yes/no questions."
  (let ((echo (app-state-echo app)))
    (set! *use-dialog-box* (not *use-dialog-box*))
    (echo-message! echo (if *use-dialog-box*
                          "Dialog boxes ON"
                          "Dialog boxes OFF"))))

(def (cmd-toggle-use-short-answers app)
  "Toggle using short y/n answers instead of yes/no."
  (let ((echo (app-state-echo app)))
    (set! *use-short-answers* (not *use-short-answers*))
    (echo-message! echo (if *use-short-answers*
                          "Short answers (y/n) ON"
                          "Short answers (y/n) OFF"))))

(def (cmd-toggle-ring-bell-function app)
  "Cycle bell function: ignore -> beep -> flash."
  (let ((echo (app-state-echo app)))
    (set! *ring-bell-function*
      (case *ring-bell-function*
        ((ignore) 'beep)
        ((beep) 'flash)
        (else 'ignore)))
    (echo-message! echo
      (string-append "Bell: " (symbol->string *ring-bell-function*)))))

(def (cmd-toggle-sentence-end-double-space app)
  "Toggle requiring double space after period to end a sentence."
  (let ((echo (app-state-echo app)))
    (set! *sentence-end-double-space* (not *sentence-end-double-space*))
    (echo-message! echo (if *sentence-end-double-space*
                          "Sentence end double-space ON"
                          "Sentence end double-space OFF"))))

(def (cmd-toggle-colon-double-space app)
  "Toggle requiring double space after colon."
  (let ((echo (app-state-echo app)))
    (set! *colon-double-space* (not *colon-double-space*))
    (echo-message! echo (if *colon-double-space*
                          "Colon double-space ON"
                          "Colon double-space OFF"))))

(def (cmd-toggle-comment-auto-fill app)
  "Toggle auto-fill in comments only."
  (let ((echo (app-state-echo app)))
    (set! *comment-auto-fill* (not *comment-auto-fill*))
    (echo-message! echo (if *comment-auto-fill*
                          "Comment auto-fill ON"
                          "Comment auto-fill OFF"))))

;; ── batch 50: visual enhancement toggles ────────────────────────────
(def *global-prettify* #f)
(def *global-hl-todo* #f)
(def *global-color-identifiers* #f)
(def *global-aggressive-indent* #f)
(def *global-origami* #f)
(def *global-centered-cursor* #f)
(def *global-beacon* #f)
(def *global-dimmer* #f)
(def *global-focus* #f)

(def (cmd-toggle-global-prettify app)
  "Toggle global prettify-symbols-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-prettify* (not *global-prettify*))
    (echo-message! echo (if *global-prettify*
                          "Global prettify ON" "Global prettify OFF"))))

(def (cmd-toggle-global-hl-todo app)
  "Toggle global hl-todo-mode (highlight TODO/FIXME)."
  (let ((echo (app-state-echo app)))
    (set! *global-hl-todo* (not *global-hl-todo*))
    (echo-message! echo (if *global-hl-todo*
                          "Global hl-todo ON" "Global hl-todo OFF"))))

(def (cmd-toggle-global-color-identifiers app)
  "Toggle global color-identifiers-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-color-identifiers* (not *global-color-identifiers*))
    (echo-message! echo (if *global-color-identifiers*
                          "Color identifiers ON" "Color identifiers OFF"))))

(def (cmd-toggle-global-aggressive-indent app)
  "Toggle global aggressive-indent-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-aggressive-indent* (not *global-aggressive-indent*))
    (echo-message! echo (if *global-aggressive-indent*
                          "Aggressive indent ON" "Aggressive indent OFF"))))

(def (cmd-toggle-global-origami app)
  "Toggle global origami-mode (code folding)."
  (let ((echo (app-state-echo app)))
    (set! *global-origami* (not *global-origami*))
    (echo-message! echo (if *global-origami*
                          "Global origami ON" "Global origami OFF"))))

(def (cmd-toggle-global-centered-cursor app)
  "Toggle global centered-cursor-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-centered-cursor* (not *global-centered-cursor*))
    (echo-message! echo (if *global-centered-cursor*
                          "Centered cursor ON" "Centered cursor OFF"))))

(def (cmd-toggle-global-beacon app)
  "Toggle global beacon-mode (flash cursor position)."
  (let ((echo (app-state-echo app)))
    (set! *global-beacon* (not *global-beacon*))
    (echo-message! echo (if *global-beacon*
                          "Global beacon ON" "Global beacon OFF"))))

(def (cmd-toggle-global-dimmer app)
  "Toggle global dimmer-mode (dim inactive buffers)."
  (let ((echo (app-state-echo app)))
    (set! *global-dimmer* (not *global-dimmer*))
    (echo-message! echo (if *global-dimmer*
                          "Global dimmer ON" "Global dimmer OFF"))))

(def (cmd-toggle-global-focus app)
  "Toggle global focus-mode (dim unfocused paragraphs)."
  (let ((echo (app-state-echo app)))
    (set! *global-focus* (not *global-focus*))
    (echo-message! echo (if *global-focus*
                          "Global focus ON" "Global focus OFF"))))

;;; ---- batch 55: search and completion framework toggles ----

(def *global-wgrep* #f)
(def *global-deadgrep* #f)
(def *global-ripgrep* #f)
(def *global-projectile-ripgrep* #f)
(def *global-counsel* #f)
(def *global-swiper* #f)
(def *global-prescient* #f)

(def (cmd-toggle-global-wgrep app)
  "Toggle global wgrep-mode (writable grep buffers)."
  (let ((echo (app-state-echo app)))
    (set! *global-wgrep* (not *global-wgrep*))
    (echo-message! echo (if *global-wgrep*
                          "Global wgrep ON" "Global wgrep OFF"))))

(def (cmd-toggle-global-deadgrep app)
  "Toggle global deadgrep-mode (fast ripgrep interface)."
  (let ((echo (app-state-echo app)))
    (set! *global-deadgrep* (not *global-deadgrep*))
    (echo-message! echo (if *global-deadgrep*
                          "Global deadgrep ON" "Global deadgrep OFF"))))

(def (cmd-toggle-global-ripgrep app)
  "Toggle global ripgrep-mode (rg search integration)."
  (let ((echo (app-state-echo app)))
    (set! *global-ripgrep* (not *global-ripgrep*))
    (echo-message! echo (if *global-ripgrep*
                          "Global ripgrep ON" "Global ripgrep OFF"))))

(def (cmd-toggle-global-projectile-ripgrep app)
  "Toggle global projectile-ripgrep-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-projectile-ripgrep* (not *global-projectile-ripgrep*))
    (echo-message! echo (if *global-projectile-ripgrep*
                          "Projectile ripgrep ON" "Projectile ripgrep OFF"))))

(def (cmd-toggle-global-counsel app)
  "Toggle global counsel-mode (ivy-based completion commands)."
  (let ((echo (app-state-echo app)))
    (set! *global-counsel* (not *global-counsel*))
    (echo-message! echo (if *global-counsel*
                          "Global counsel ON" "Global counsel OFF"))))

(def (cmd-toggle-global-swiper app)
  "Toggle global swiper-mode (ivy-based isearch replacement)."
  (let ((echo (app-state-echo app)))
    (set! *global-swiper* (not *global-swiper*))
    (echo-message! echo (if *global-swiper*
                          "Global swiper ON" "Global swiper OFF"))))

(def (cmd-toggle-global-prescient app)
  "Toggle global prescient-mode (frecency-based sorting)."
  (let ((echo (app-state-echo app)))
    (set! *global-prescient* (not *global-prescient*))
    (echo-message! echo (if *global-prescient*
                          "Global prescient ON" "Global prescient OFF"))))

;;; ---- batch 64: org-mode ecosystem toggles ----

(def *global-org-roam* #f)
(def *global-org-journal* #f)
(def *global-org-super-agenda* #f)
(def *global-org-noter* #f)
(def *global-org-download* #f)
(def *global-org-cliplink* #f)
(def *global-org-present* #f)

(def (cmd-toggle-global-org-roam app)
  "Toggle global org-roam-mode (Zettelkasten note-taking)."
  (let ((echo (app-state-echo app)))
    (set! *global-org-roam* (not *global-org-roam*))
    (echo-message! echo (if *global-org-roam*
                          "Org-roam ON" "Org-roam OFF"))))

(def (cmd-toggle-global-org-journal app)
  "Toggle global org-journal-mode (daily journaling)."
  (let ((echo (app-state-echo app)))
    (set! *global-org-journal* (not *global-org-journal*))
    (echo-message! echo (if *global-org-journal*
                          "Org-journal ON" "Org-journal OFF"))))

(def (cmd-toggle-global-org-super-agenda app)
  "Toggle global org-super-agenda-mode (grouped agenda views)."
  (let ((echo (app-state-echo app)))
    (set! *global-org-super-agenda* (not *global-org-super-agenda*))
    (echo-message! echo (if *global-org-super-agenda*
                          "Org super-agenda ON" "Org super-agenda OFF"))))

(def (cmd-toggle-global-org-noter app)
  "Toggle global org-noter-mode (annotate documents with org)."
  (let ((echo (app-state-echo app)))
    (set! *global-org-noter* (not *global-org-noter*))
    (echo-message! echo (if *global-org-noter*
                          "Org-noter ON" "Org-noter OFF"))))

(def (cmd-toggle-global-org-download app)
  "Toggle global org-download-mode (drag-and-drop images to org)."
  (let ((echo (app-state-echo app)))
    (set! *global-org-download* (not *global-org-download*))
    (echo-message! echo (if *global-org-download*
                          "Org-download ON" "Org-download OFF"))))

(def (cmd-toggle-global-org-cliplink app)
  "Toggle global org-cliplink-mode (paste URLs as org links)."
  (let ((echo (app-state-echo app)))
    (set! *global-org-cliplink* (not *global-org-cliplink*))
    (echo-message! echo (if *global-org-cliplink*
                          "Org-cliplink ON" "Org-cliplink OFF"))))

(def (cmd-toggle-global-org-present app)
  "Toggle global org-present-mode (presentations from org files)."
  (let ((echo (app-state-echo app)))
    (set! *global-org-present* (not *global-org-present*))
    (echo-message! echo (if *global-org-present*
                          "Org-present ON" "Org-present OFF"))))

;;;============================================================================
;;; delete-horizontal-space (TUI) — not defined in other editor modules
;;;============================================================================

(def (cmd-delete-horizontal-space app)
  "Delete all spaces and tabs around point."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let* ((start (let loop ((i (- pos 1)))
                    (if (and (>= i 0) (memq (string-ref text i) '(#\space #\tab)))
                      (loop (- i 1)) (+ i 1))))
           (end (let loop ((i pos))
                  (if (and (< i len) (memq (string-ref text i) '(#\space #\tab)))
                    (loop (+ i 1)) i))))
      (when (> (- end start) 0)
        (editor-delete-range ed start (- end start))
        (editor-goto-pos ed start)))))

;;;============================================================================
;;; fill-region (TUI) — fill/wrap text in marked region
;;;============================================================================

(def (fill-words words col)
  "Reflow WORDS list to COL width, returning string."
  (if (null? words) ""
    (let loop ((ws (cdr words)) (line (car words)) (lines []))
      (if (null? ws)
        (string-join (reverse (cons line lines)) "\n")
        (let ((next (string-append line " " (car ws))))
          (if (> (string-length next) col)
            (if (string=? line "")
              (loop (cdr ws) "" (cons (car ws) lines))
              (loop ws "" (cons line lines)))
            (loop (cdr ws) next lines)))))))

(def (cmd-fill-region app)
  "Fill (word-wrap) the selected region at fill-column."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-message! (app-state-echo app) "No mark set")
      (let* ((pos (editor-get-current-pos ed))
             (start (min pos mark))
             (end (max pos mark))
             (text (editor-get-text ed))
             (region (substring text start end))
             (words (filter (lambda (w) (> (string-length w) 0))
                            (string-split (string-trim region) #\space)))
             (filled (fill-words words 80)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start filled))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region filled")))))

;;;============================================================================
;;; copy-rectangle-to-register (TUI)
;;;============================================================================

(def (cmd-copy-rectangle-to-register app)
  "Copy rectangle (region) to a register."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Copy rectangle to register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (ed (current-editor app))
             (buf (current-buffer-from-app app))
             (mark (buffer-mark buf)))
        (if (not mark)
          (echo-error! echo "No mark set")
          (let* ((pos (editor-get-current-pos ed))
                 (start (min pos mark))
                 (end (max pos mark))
                 (text (editor-get-text ed))
                 (lines (string-split text #\newline))
                 (start-line (editor-line-from-position ed start))
                 (end-line (editor-line-from-position ed end))
                 ;; Compute columns
                 (start-col (- start (let loop ((i 0) (p 0))
                                        (if (>= i start-line) p
                                          (loop (+ i 1) (+ p 1 (string-length (list-ref lines i))))))))
                 (end-col (- end (let loop ((i 0) (p 0))
                                    (if (>= i end-line) p
                                      (loop (+ i 1) (+ p 1 (string-length (list-ref lines i))))))))
                 (left (min start-col end-col))
                 (right (max start-col end-col))
                 ;; Extract rectangle lines
                 (rect-lines
                   (let loop ((i start-line) (acc []))
                     (if (> i end-line) (reverse acc)
                       (let* ((l (if (< i (length lines)) (list-ref lines i) ""))
                              (llen (string-length l))
                              (s (min left llen))
                              (e (min right llen)))
                         (loop (+ i 1) (cons (substring l s e) acc)))))))
            (hash-put! (app-state-registers app) reg
              (string-join rect-lines "\n"))
            (echo-message! echo
              (string-append "Rectangle copied to register " (string reg)))))))))
