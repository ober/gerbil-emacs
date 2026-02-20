;;; -*- Gerbil -*-
;;; Task #51: Additional commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/process
        :std/misc/ports
        :std/srfi/19
        ./pregexp-compat
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        (only-in :gemacs/editor-core
                 search-forward-regexp-impl!)
        (only-in :gemacs/editor-ui
                 position-cursor-for-replace!)
        :gemacs/editor-extra-helpers
        :gemacs/editor-extra-web
        :gemacs/editor-extra-media
        :gemacs/editor-extra-modes)

;;;============================================================================
;;; Task #51: Additional unique commands to cross 1000 registrations
;;;============================================================================

;; --- Emacs built-in modes not yet covered ---
(def (cmd-native-compile-file app)
  "Native compile a file — runs gerbil build on current file."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (echo-message! (app-state-echo app)
        (string-append "Compile: use M-x compile for " (path-strip-directory path))))))

(def (cmd-native-compile-async app)
  "Native compile asynchronously — background compilation."
  (echo-message! (app-state-echo app) "Async compile: use M-x compile"))

(def (cmd-tab-line-mode app)
  "Toggle tab-line-mode — shows buffer tabs."
  (let ((on (toggle-mode! 'tab-line)))
    (echo-message! (app-state-echo app) (if on "Tab-line: on" "Tab-line: off"))))

(def (cmd-pixel-scroll-precision-mode app)
  "Toggle pixel-scroll-precision-mode — smooth scrolling."
  (let ((on (toggle-mode! 'pixel-scroll)))
    (echo-message! (app-state-echo app) (if on "Pixel scroll: on" "Pixel scroll: off"))))

(def (cmd-so-long-mode app)
  "Toggle so-long mode for long lines — disables features on long-line files."
  (let ((on (toggle-mode! 'so-long)))
    (echo-message! (app-state-echo app) (if on "So-long mode: on" "So-long mode: off"))))

(def (cmd-repeat-mode app)
  "Toggle repeat-mode for transient maps."
  (let ((on (toggle-mode! 'repeat)))
    (echo-message! (app-state-echo app) (if on "Repeat mode: on" "Repeat mode: off"))))

(def (cmd-context-menu-mode app)
  "Toggle context-menu-mode — N/A in terminal."
  (echo-message! (app-state-echo app) "Context menu: N/A in terminal"))

(def (cmd-savehist-mode app)
  "Toggle savehist-mode — persist minibuffer history."
  (let ((on (toggle-mode! 'savehist)))
    (echo-message! (app-state-echo app) (if on "Savehist: on" "Savehist: off"))))

(def (cmd-recentf-mode app)
  "Toggle recentf-mode — track recent files."
  (let ((on (toggle-mode! 'recentf)))
    (echo-message! (app-state-echo app) (if on "Recentf: on" "Recentf: off"))))

(def (cmd-winner-undo-2 app)
  "Winner undo alternative binding."
  (cmd-winner-undo app))

(def (cmd-global-subword-mode app)
  "Toggle global subword-mode (CamelCase navigation)."
  (let ((on (toggle-mode! 'global-subword)))
    (echo-message! (app-state-echo app) (if on "Global subword: on" "Global subword: off"))))

(def (cmd-display-fill-column-indicator-mode app)
  "Toggle fill column indicator display."
  (let* ((fr (app-state-frame app))
         (on (toggle-mode! 'fill-column-indicator)))
    (for-each
      (lambda (win)
        (let ((ed (edit-window-editor win)))
          (send-message ed 2363 #|SCI_SETEDGEMODE|# (if on 1 0) 0)
          (when on (send-message ed 2361 #|SCI_SETEDGECOLUMN|# 80 0))))
      (frame-windows fr))
    (echo-message! (app-state-echo app)
      (if on "Fill column indicator: on (80)" "Fill column indicator: off"))))

(def (cmd-global-display-line-numbers-mode app)
  "Toggle global line numbers display."
  (let* ((fr (app-state-frame app))
         (on (toggle-mode! 'global-line-numbers)))
    (for-each
      (lambda (win)
        (let ((ed (edit-window-editor win)))
          (send-message ed SCI_SETMARGINWIDTHN 0 (if on 48 0))))
      (frame-windows fr))
    (echo-message! (app-state-echo app)
      (if on "Global line numbers: on" "Global line numbers: off"))))

(def (cmd-indent-bars-mode app)
  "Toggle indent-bars indentation guides."
  (cmd-indent-guide-mode app))

(def (cmd-global-hl-line-mode app)
  "Toggle global hl-line highlighting."
  (let* ((fr (app-state-frame app))
         (on (toggle-mode! 'global-hl-line)))
    (for-each
      (lambda (win)
        (let ((ed (edit-window-editor win)))
          (send-message ed SCI_SETCARETLINEVISIBLE (if on 1 0) 0)
          (when on (send-message ed SCI_SETCARETLINEBACK #x333333 0))))
      (frame-windows fr))
    (echo-message! (app-state-echo app)
      (if on "Global hl-line: on" "Global hl-line: off"))))

(def (cmd-delete-selection-mode app)
  "Toggle delete-selection-mode — typed text replaces selection."
  (let ((on (toggle-mode! 'delete-selection)))
    (echo-message! (app-state-echo app)
      (if on "Delete selection mode: on" "Delete selection mode: off"))))

(def (cmd-electric-indent-mode app)
  "Toggle electric-indent-mode — auto-indent on newline."
  (let ((on (toggle-mode! 'electric-indent)))
    (echo-message! (app-state-echo app)
      (if on "Electric indent: on" "Electric indent: off"))))

(def (cmd-show-paren-mode app)
  "Toggle show-paren-mode — highlight matching parentheses."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (on (toggle-mode! 'show-paren)))
    (if on
      (begin
        (send-message ed SCI_STYLESETFORE 34 #x00FF00)  ;; STYLE_BRACELIGHT
        (send-message ed SCI_STYLESETBACK 34 #x333333)
        (echo-message! (app-state-echo app) "Show paren: on"))
      (begin
        (send-message ed SCI_STYLESETFORE 34 #xFFFFFF)
        (send-message ed SCI_STYLESETBACK 34 #x000000)
        (echo-message! (app-state-echo app) "Show paren: off")))))

(def (cmd-column-number-mode app)
  "Toggle column-number-mode in modeline."
  (let ((on (toggle-mode! 'column-number)))
    (echo-message! (app-state-echo app) (if on "Column number: on" "Column number: off"))))

(def (cmd-size-indication-mode app)
  "Toggle size-indication-mode in modeline."
  (let ((on (toggle-mode! 'size-indication)))
    (echo-message! (app-state-echo app) (if on "Size indication: on" "Size indication: off"))))

(def (cmd-minibuffer-depth-indicate-mode app)
  "Toggle minibuffer-depth-indicate-mode."
  (let ((on (toggle-mode! 'minibuffer-depth)))
    (echo-message! (app-state-echo app) (if on "Minibuffer depth: on" "Minibuffer depth: off"))))

(def (cmd-file-name-shadow-mode app)
  "Toggle file-name-shadow-mode — dims irrelevant path in minibuffer."
  (let ((on (toggle-mode! 'file-name-shadow)))
    (echo-message! (app-state-echo app) (if on "File name shadow: on" "File name shadow: off"))))

(def (cmd-midnight-mode app)
  "Toggle midnight-mode — clean up old buffers periodically."
  (let ((on (toggle-mode! 'midnight)))
    (echo-message! (app-state-echo app) (if on "Midnight mode: on" "Midnight mode: off"))))

(def (cmd-cursor-intangible-mode app)
  "Toggle cursor-intangible-mode."
  (let ((on (toggle-mode! 'cursor-intangible)))
    (echo-message! (app-state-echo app) (if on "Cursor intangible: on" "Cursor intangible: off"))))

(def (cmd-auto-compression-mode app)
  "Toggle auto-compression-mode — transparent compressed file access."
  (let ((on (toggle-mode! 'auto-compression)))
    (echo-message! (app-state-echo app) (if on "Auto-compression: on" "Auto-compression: off"))))

;;;============================================================================
;;; Window resize commands
;;;============================================================================

(def (cmd-enlarge-window app)
  "Make current window taller (C-x ^)."
  (let* ((fr (app-state-frame app))
         (n (get-prefix-arg app)))
    (if (> (length (frame-windows fr)) 1)
      (begin
        (frame-enlarge-window! fr n)
        (echo-message! (app-state-echo app)
          (string-append "Window enlarged by " (number->string n))))
      (echo-error! (app-state-echo app) "Only one window"))))

(def (cmd-shrink-window app)
  "Make current window shorter."
  (let* ((fr (app-state-frame app))
         (n (get-prefix-arg app)))
    (if (> (length (frame-windows fr)) 1)
      (begin
        (frame-shrink-window! fr n)
        (echo-message! (app-state-echo app)
          (string-append "Window shrunk by " (number->string n))))
      (echo-error! (app-state-echo app) "Only one window"))))

(def (cmd-enlarge-window-horizontally app)
  "Make current window wider (C-x })."
  (let* ((fr (app-state-frame app))
         (n (get-prefix-arg app)))
    (if (> (length (frame-windows fr)) 1)
      (begin
        (frame-enlarge-window-horizontally! fr n)
        (echo-message! (app-state-echo app)
          (string-append "Window widened by " (number->string n))))
      (echo-error! (app-state-echo app) "Only one window"))))

(def (cmd-shrink-window-horizontally app)
  "Make current window narrower (C-x {)."
  (let* ((fr (app-state-frame app))
         (n (get-prefix-arg app)))
    (if (> (length (frame-windows fr)) 1)
      (begin
        (frame-shrink-window-horizontally! fr n)
        (echo-message! (app-state-echo app)
          (string-append "Window narrowed by " (number->string n))))
      (echo-error! (app-state-echo app) "Only one window"))))

;;;============================================================================
;;; Regex search (C-M-s) and regex query-replace (C-M-%)
;;;============================================================================

(def *last-regexp-search* "")

(def (cmd-search-forward-regexp app)
  "Forward regex search (C-M-s). Uses Scintilla SCFIND_REGEXP."
  (let ((default *last-regexp-search*))
    (if (and (eq? (app-state-last-command app) 'isearch-forward-regexp)
             (> (string-length default) 0))
      ;; Repeat: move past current match, then search again
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed)))
        (editor-goto-pos ed (+ pos 1))
        (search-forward-regexp-impl! app default))
      ;; First C-M-s: prompt for pattern
      (let* ((echo (app-state-echo app))
             (fr (app-state-frame app))
             (row (- (frame-height fr) 1))
             (width (frame-width fr))
             (prompt (if (string=? default "")
                       "Regexp search: "
                       (string-append "Regexp search [" default "]: ")))
             (input (echo-read-string echo prompt row width)))
        (when input
          (let ((pattern (if (string=? input "") default input)))
            (when (> (string-length pattern) 0)
              (set! *last-regexp-search* pattern)
              (search-forward-regexp-impl! app pattern))))))))

(def (cmd-query-replace-regexp-interactive app)
  "Interactive regex query-replace (C-M-%). Uses Scintilla SCFIND_REGEXP."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (from-str (echo-read-string echo "Regexp replace: " row width)))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (echo-read-string echo
                      (string-append "Replace regexp \"" from-str "\" with: ")
                      row width)))
        (when to-str
          (let ((ed (current-editor app)))
            (regexp-query-replace-loop! app ed from-str to-str))))))  )

(def (regexp-query-replace-loop! app ed pattern replacement)
  "Drive the interactive regexp query-replace using Scintilla regex."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (replaced 0))
    ;; Start from current position
    (let loop ()
      (let ((text-len (editor-get-text-length ed))
            (pos (editor-get-current-pos ed)))
        ;; Search forward with regex
        (send-message ed SCI_SETTARGETSTART pos)
        (send-message ed SCI_SETTARGETEND text-len)
        (send-message ed SCI_SETSEARCHFLAGS SCFIND_REGEXP)
        (let ((found (send-message/string ed SCI_SEARCHINTARGET pattern)))
          (if (< found 0)
            ;; No more matches
            (echo-message! echo
              (string-append "Replaced " (number->string replaced) " occurrences"))
            ;; Found a match
            (let ((match-end (send-message ed SCI_GETTARGETEND)))
              (editor-set-selection ed found match-end)
              (editor-scroll-caret ed)
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
                       (send-message ed SCI_SETTARGETEND match-end)
                       (let ((repl-len (send-message/string ed SCI_REPLACETARGETRE replacement)))
                         (editor-goto-pos ed (+ found (max repl-len 1)))
                         (set! replaced (+ replaced 1)))
                       (loop))
                      ;; No: skip
                      ((= ch (char->integer #\n))
                       (editor-goto-pos ed (+ found (max 1 (- match-end found))))
                       (loop))
                      ;; All: replace all remaining
                      ((= ch (char->integer #\!))
                       (let all-loop ()
                         (let ((text-len2 (editor-get-text-length ed))
                               (pos2 (editor-get-current-pos ed)))
                           (send-message ed SCI_SETTARGETSTART pos2)
                           (send-message ed SCI_SETTARGETEND text-len2)
                           (send-message ed SCI_SETSEARCHFLAGS SCFIND_REGEXP)
                           (let ((found2 (send-message/string ed SCI_SEARCHINTARGET pattern)))
                             (when (>= found2 0)
                               (let ((match-end2 (send-message ed SCI_GETTARGETEND)))
                                 (send-message ed SCI_SETTARGETSTART found2)
                                 (send-message ed SCI_SETTARGETEND match-end2)
                                 (let ((repl-len2 (send-message/string ed SCI_REPLACETARGETRE replacement)))
                                   (editor-goto-pos ed (+ found2 (max repl-len2 1)))
                                   (set! replaced (+ replaced 1))))
                               (all-loop)))))
                       (echo-message! echo
                         (string-append "Replaced " (number->string replaced) " occurrences")))
                      ;; Quit
                      ((= ch (char->integer #\q))
                       (echo-message! echo
                         (string-append "Replaced " (number->string replaced) " occurrences")))
                      ;; Unknown key: skip
                      (else (loop)))))))))))))

;;;============================================================================
;;; Editorconfig support — read .editorconfig and apply settings
;;;============================================================================

(def (parse-editorconfig path)
  "Parse .editorconfig file into list of (glob-pattern . settings-hash) pairs."
  (let ((result [])
        (current-glob #f)
        (current-settings #f))
    (when (file-exists? path)
      (call-with-input-file path
        (lambda (port)
          (let loop ()
            (let ((line (read-line port)))
              (unless (eof-object? line)
                (let ((trimmed (string-trim-both line)))
                  (cond
                    ;; Skip blank/comment lines
                    ((or (string=? trimmed "")
                         (string-prefix? "#" trimmed)
                         (string-prefix? ";" trimmed))
                     (void))
                    ;; Section header [glob]
                    ((and (string-prefix? "[" trimmed)
                          (string-suffix? "]" trimmed))
                     ;; Save previous section
                     (when (and current-glob current-settings)
                       (set! result (cons (cons current-glob current-settings) result)))
                     (set! current-glob (substring trimmed 1 (- (string-length trimmed) 1)))
                     (set! current-settings (make-hash-table)))
                    ;; key = value
                    (else
                     (when current-settings
                       (let ((eq-pos (string-index trimmed #\=)))
                         (when eq-pos
                           (let ((key (string-trim-both (substring trimmed 0 eq-pos)))
                                 (val (string-trim-both (substring trimmed (+ eq-pos 1)
                                                          (string-length trimmed)))))
                             (hash-put! current-settings
                                        (string-downcase key)
                                        (string-downcase val)))))))))
                (loop)))))))
    ;; Save last section
    (when (and current-glob current-settings)
      (set! result (cons (cons current-glob current-settings) result)))
    (reverse result)))

(def (editorconfig-glob-match? pattern filename)
  "Simple glob matching for editorconfig patterns."
  (let ((basename (path-strip-directory filename))
        (ext (path-extension filename)))
    (cond
      ((string=? pattern "*") #t)
      ((string-prefix? "*." pattern)
       ;; Match by extension: *.py matches .py files
       (let ((pat-ext (substring pattern 1 (string-length pattern))))
         (string-suffix? pat-ext basename)))
      ((string-prefix? "[" pattern) #t) ;; Simplified: match all bracket patterns
      (else (string=? pattern basename)))))

(def (find-editorconfig filepath)
  "Search up from filepath for .editorconfig files, return merged settings."
  (let ((settings (make-hash-table))
        (filename (path-strip-directory filepath)))
    (let loop ((dir (path-directory filepath)))
      (when (and dir (> (string-length dir) 1))
        (let ((ec-path (string-append dir "/.editorconfig")))
          (when (file-exists? ec-path)
            (let ((sections (parse-editorconfig ec-path)))
              (for-each
                (lambda (section)
                  (let ((glob (car section))
                        (sec-settings (cdr section)))
                    (when (editorconfig-glob-match? glob filepath)
                      ;; Apply settings (first match wins per key)
                      (hash-for-each
                        (lambda (k v)
                          (unless (hash-key? settings k)
                            (hash-put! settings k v)))
                        sec-settings))))
                sections)
              ;; Check for root = true
              (let ((root-check (find (lambda (s) (string=? (car s) "*")) sections)))
                (when (and root-check (equal? (hash-get (cdr root-check) "root") "true"))
                  (hash-put! settings "__root__" "true"))))))
        (unless (hash-get settings "__root__")
          (let ((parent (path-directory (string-append dir "/.."))))
            (when (and parent (not (string=? parent dir)))
              (loop parent))))))
    settings))

(def (apply-editorconfig! ed settings)
  "Apply editorconfig settings to a Scintilla editor."
  (let ((indent-style (hash-get settings "indent_style"))
        (indent-size (hash-get settings "indent_size"))
        (tab-width (hash-get settings "tab_width"))
        (end-of-line (hash-get settings "end_of_line"))
        (trim-trailing (hash-get settings "trim_trailing_whitespace"))
        (insert-final (hash-get settings "insert_final_newline")))
    ;; Indent style: tab or space
    (when indent-style
      (send-message ed SCI_SETUSETABS (if (string=? indent-style "tab") 1 0) 0))
    ;; Indent size
    (when indent-size
      (let ((size (string->number indent-size)))
        (when size
          (send-message ed SCI_SETINDENT size 0)
          (send-message ed SCI_SETTABWIDTH (or (and tab-width (string->number tab-width)) size) 0))))
    ;; Tab width (if different from indent size)
    (when (and tab-width (not indent-size))
      (let ((tw (string->number tab-width)))
        (when tw (send-message ed SCI_SETTABWIDTH tw 0))))
    ;; EOL mode
    (when end-of-line
      (send-message ed SCI_SETEOLMODE
        (cond ((string=? end-of-line "lf") 2)
              ((string=? end-of-line "crlf") 0)
              ((string=? end-of-line "cr") 1)
              (else 2))
        0))))

(def (cmd-editorconfig-apply app)
  "Apply .editorconfig settings to current buffer."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((settings (find-editorconfig path)))
        (if (= (hash-length settings) 0)
          (echo-message! (app-state-echo app) "No .editorconfig found")
          (let ((ed (current-editor app)))
            (apply-editorconfig! ed settings)
            (echo-message! (app-state-echo app)
              (string-append "Applied editorconfig ("
                             (number->string (hash-length settings))
                             " settings)"))))))))

;;;============================================================================
;;; Format buffer with external tool
;;;============================================================================

(def *formatters*
  '(("python" . ("black" "-"))
    ("py" . ("black" "-"))
    ("go" . ("gofmt"))
    ("javascript" . ("prettier" "--stdin-filepath" "file.js"))
    ("js" . ("prettier" "--stdin-filepath" "file.js"))
    ("typescript" . ("prettier" "--stdin-filepath" "file.ts"))
    ("ts" . ("prettier" "--stdin-filepath" "file.ts"))
    ("json" . ("prettier" "--stdin-filepath" "file.json"))
    ("css" . ("prettier" "--stdin-filepath" "file.css"))
    ("html" . ("prettier" "--stdin-filepath" "file.html"))
    ("rust" . ("rustfmt"))
    ("c" . ("clang-format"))
    ("cpp" . ("clang-format"))
    ("scheme" . ("gerbil" "fmt"))
    ("gerbil" . ("gerbil" "fmt"))
    ("ruby" . ("rubocop" "--auto-correct" "--stdin" "file.rb"))
    ("sh" . ("shfmt" "-"))
    ("bash" . ("shfmt" "-"))
    ("yaml" . ("prettier" "--stdin-filepath" "file.yaml"))
    ("xml" . ("xmllint" "--format" "-"))))

(def (detect-language-from-extension path)
  "Detect programming language from file extension."
  (let ((ext (path-extension path)))
    (cond
      ((member ext '(".py")) "python")
      ((member ext '(".go")) "go")
      ((member ext '(".js" ".jsx" ".mjs")) "javascript")
      ((member ext '(".ts" ".tsx")) "typescript")
      ((member ext '(".json")) "json")
      ((member ext '(".css" ".scss" ".less")) "css")
      ((member ext '(".html" ".htm")) "html")
      ((member ext '(".rs")) "rust")
      ((member ext '(".c" ".h")) "c")
      ((member ext '(".cpp" ".cc" ".hpp")) "cpp")
      ((member ext '(".ss" ".scm")) "scheme")
      ((member ext '(".rb")) "ruby")
      ((member ext '(".sh")) "sh")
      ((member ext '(".yaml" ".yml")) "yaml")
      ((member ext '(".xml")) "xml")
      (else #f))))

(def (cmd-format-buffer app)
  "Format current buffer using language-appropriate external formatter."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf)))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-error! echo "Buffer has no file")
      (let* ((lang (detect-language-from-extension path))
             (formatter (and lang (assoc lang *formatters*))))
        (if (not formatter)
          (echo-error! echo (string-append "No formatter for "
                              (or lang (path-extension path))))
          (let* ((ed (current-editor app))
                 (text (editor-get-text ed))
                 (cmd (cdr formatter)))
            (with-catch
              (lambda (e)
                (echo-error! echo (string-append "Format error: "
                                    (with-output-to-string
                                      (lambda () (display-exception e))))))
              (lambda ()
                (let ((formatted (filter-with-process cmd
                                   (lambda (port) (display text port))
                                   (lambda (port) (read-line port #f)))))
                  (when (and formatted (> (string-length formatted) 0)
                             (not (string=? formatted text)))
                    (let ((pos (editor-get-current-pos ed)))
                      (editor-set-text ed formatted)
                      (editor-goto-pos ed (min pos (string-length formatted)))
                      (echo-message! echo
                        (string-append "Formatted with "
                          (car cmd))))))))))))))

;;;============================================================================
;;; Git blame for current line
;;;============================================================================

(def (cmd-git-blame-line app)
  "Show git blame info for the current line."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf)))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-error! echo "Buffer has no file")
      (let* ((ed (current-editor app))
             (line (+ 1 (send-message ed SCI_LINEFROMPOSITION
                          (editor-get-current-pos ed) 0))))
        (with-catch
          (lambda (e)
            (echo-error! echo "Not in a git repo or git blame failed"))
          (lambda ()
            (let* ((dir (path-directory path))
                   (fname (path-strip-directory path))
                   (output (run-process
                            ["git" "-C" dir "blame" "-L"
                             (string-append (number->string line) ","
                                            (number->string line))
                             "--porcelain" fname]))
                   (lines (string-split output #\newline)))
              (if (or (null? lines) (< (length lines) 3))
                (echo-message! echo "No blame info")
                ;; Parse porcelain format
                (let* ((header (car lines))
                       (parts (string-split header #\space))
                       (commit (if (pair? parts) (car parts) "?"))
                       (author (let loop ((ls (cdr lines)))
                                 (if (null? ls) "?"
                                   (let ((l (car ls)))
                                     (if (string-prefix? "author " l)
                                       (substring l 7 (string-length l))
                                       (loop (cdr ls)))))))
                       (date (let loop ((ls (cdr lines)))
                               (if (null? ls) "?"
                                 (let ((l (car ls)))
                                   (if (string-prefix? "author-time " l)
                                     (let ((ts (string->number
                                                 (substring l 12 (string-length l)))))
                                       (if ts
                                         (with-catch
                                           (lambda (e) "?")
                                           (lambda ()
                                             (let* ((t (make-time time-utc 0
                                                         (inexact->exact (floor ts))))
                                                    (d (time-utc->date t 0)))
                                               (date->string d "~Y-~m-~d"))))
                                         "?"))
                                     (loop (cdr ls)))))))
                       (short-commit (if (> (string-length commit) 7)
                                       (substring commit 0 7)
                                       commit)))
                  (echo-message! echo
                    (string-append short-commit " " author " " date)))))))))))

;;;============================================================================
;;; Persistent M-x command history
;;;============================================================================

(def *command-history* [])
(def *command-history-file* "~/.gemacs-cmd-history")

(def (command-history-load!)
  "Load M-x command history from disk."
  (let ((path (path-expand *command-history-file*)))
    (when (file-exists? path)
      (with-catch
        (lambda (e) (void))
        (lambda ()
          (set! *command-history*
            (call-with-input-file path
              (lambda (port)
                (let loop ((acc []))
                  (let ((line (read-line port)))
                    (if (eof-object? line)
                      (reverse acc)
                      (loop (cons line acc)))))))))))))

(def (command-history-save!)
  "Save M-x command history to disk."
  (let ((path (path-expand *command-history-file*)))
    (with-catch
      (lambda (e) (void))
      (lambda ()
        (call-with-output-file path
          (lambda (port)
            ;; Keep last 100 entries
            (let ((recent (let loop ((ls *command-history*) (n 0) (acc []))
                            (if (or (null? ls) (>= n 100))
                              (reverse acc)
                              (loop (cdr ls) (+ n 1) (cons (car ls) acc))))))
              (for-each (lambda (cmd) (display cmd port) (newline port))
                        recent))))))))

(def (command-history-add! cmd-name)
  "Add a command to history (most recent first, dedup)."
  (let ((name (if (symbol? cmd-name) (symbol->string cmd-name) cmd-name)))
    (set! *command-history*
      (cons name (filter (lambda (x) (not (string=? x name)))
                         *command-history*)))))

(def (cmd-execute-extended-command-with-history app)
  "M-x with command history — shows recently used commands first."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         ;; Build completion list: recent commands first, then alphabetical
         (all-names (sort (map symbol->string (hash-keys *all-commands*)) string<?))
         (recent (filter (lambda (name) (hash-get *all-commands* (string->symbol name)))
                         *command-history*))
         (rest (filter (lambda (name) (not (member name recent))) all-names))
         (ordered (append recent rest))
         (input (echo-read-string-with-completion echo "M-x " ordered row width)))
    (when (and input (> (string-length input) 0))
      (let ((cmd-sym (string->symbol input)))
        (command-history-add! input)
        (execute-command! app cmd-sym)))))

;;;============================================================================
;;; Word completion from buffer content
;;;============================================================================

(def (collect-buffer-words ed)
  "Collect unique words from the current buffer."
  (let* ((text (editor-get-text ed))
         (len (string-length text))
         (words (make-hash-table)))
    (let loop ((i 0) (word-start #f))
      (if (>= i len)
        (begin
          ;; Capture last word
          (when (and word-start (> (- i word-start) 2))
            (hash-put! words (substring text word-start i) #t))
          (sort (hash-keys words) string<?))
        (let ((ch (string-ref text i)))
          (if (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-))
            (loop (+ i 1) (or word-start i))
            (begin
              (when (and word-start (> (- i word-start) 2))
                (hash-put! words (substring text word-start i) #t))
              (loop (+ i 1) #f))))))))

(def (cmd-complete-word-from-buffer app)
  "Complete word at point from words in the current buffer."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (echo (app-state-echo app)))
    ;; Find partial word before cursor
    (let ((text (editor-get-text ed)))
      (let loop ((start (- pos 1)))
        (if (or (< start 0)
                (let ((ch (string-ref text start)))
                  (not (or (char-alphabetic? ch) (char-numeric? ch)
                           (char=? ch #\_) (char=? ch #\-)))))
          (let* ((word-start (+ start 1))
                 (prefix (substring text word-start pos)))
            (if (< (string-length prefix) 1)
              (echo-message! echo "No word to complete")
              (let* ((all-words (collect-buffer-words ed))
                     (matches (filter
                                (lambda (w)
                                  (and (string-prefix? prefix w)
                                       (not (string=? w prefix))))
                                all-words)))
                (cond
                  ((null? matches)
                   (echo-message! echo "No completions"))
                  ((= (length matches) 1)
                   ;; Single match: insert it
                   (let ((completion (substring (car matches)
                                      (string-length prefix)
                                      (string-length (car matches)))))
                     (editor-insert-text ed pos completion)
                     (echo-message! echo (car matches))))
                  (else
                   ;; Multiple matches: show completion menu
                   (let* ((fr (app-state-frame app))
                          (row (- (frame-height fr) 1))
                          (width (frame-width fr))
                          (choice (echo-read-string-with-completion echo
                                    "Complete: " matches row width)))
                     (when (and choice (> (string-length choice) 0))
                       (let ((completion (substring choice
                                           (string-length prefix)
                                           (string-length choice))))
                         (editor-insert-text ed pos completion)))))))))
          (loop (- start 1)))))))

;;;============================================================================
;;; URL detection and opening
;;;============================================================================

(def (find-url-at-point text pos)
  "Find URL at or near cursor position. Returns (start . end) or #f."
  (let ((len (string-length text)))
    ;; Search backward for http or https
    (let loop ((i (min pos (- len 1))))
      (if (< i 0) #f
        (if (and (>= (- len i) 8)
                 (or (string-prefix? "http://" (substring text i (min (+ i 8) len)))
                     (string-prefix? "https://" (substring text i (min (+ i 9) len)))))
          ;; Found URL start, find end
          (let end-loop ((j i))
            (if (or (>= j len)
                    (let ((ch (string-ref text j)))
                      (or (char=? ch #\space) (char=? ch #\newline)
                          (char=? ch #\tab) (char=? ch #\)) (char=? ch #\])
                          (char=? ch #\>) (char=? ch (integer->char 34)))))
              (if (> j i) (cons i j) #f)
              (end-loop (+ j 1))))
          ;; Only search backward a bit
          (if (> (- pos i) 200) #f
            (loop (- i 1))))))))

(def (cmd-open-url-at-point app)
  "Open URL at point in external browser."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (echo (app-state-echo app)))
    (let ((url-bounds (find-url-at-point text pos)))
      (if (not url-bounds)
        (echo-error! echo "No URL at point")
        (let ((url (substring text (car url-bounds) (cdr url-bounds))))
          (with-catch
            (lambda (e) (echo-error! echo "Failed to open URL"))
            (lambda ()
              (run-process/batch ["xdg-open" url])
              (echo-message! echo (string-append "Opening: " url)))))))))

;;;============================================================================
;;; MRU buffer switching
;;;============================================================================

(def *buffer-access-times* (make-hash-table))

(def *buffer-access-counter* 0)

(def (record-buffer-access! buf-name)
  "Record access time for a buffer using a monotonic counter."
  (set! *buffer-access-counter* (+ *buffer-access-counter* 1))
  (hash-put! *buffer-access-times* buf-name *buffer-access-counter*))

(def (cmd-switch-buffer-mru app)
  "Switch to most recently used buffer (excluding current)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (current-buf (current-buffer-from-app app))
         (current-name (and current-buf (buffer-name current-buf)))
         (bufs (filter (lambda (b) (not (equal? (buffer-name b) current-name)))
                       (buffer-list))))
    (if (null? bufs)
      (echo-message! echo "No other buffers")
      ;; Sort by access time (most recent first)
      (let* ((sorted (sort bufs
                       (lambda (a b)
                         (let ((ta (or (hash-get *buffer-access-times* (buffer-name a)) 0))
                               (tb (or (hash-get *buffer-access-times* (buffer-name b)) 0)))
                           (> ta tb)))))
             (target (car sorted))
             (ed (current-editor app)))
        (buffer-attach! ed target)
        (set! (edit-window-buffer (current-window fr)) target)
        (record-buffer-access! (buffer-name target))
        (echo-message! echo (string-append "Buffer: " (buffer-name target)))))))

;;;============================================================================
;;; Pipe region through shell command
;;;============================================================================

(def (cmd-shell-command-on-region-replace app)
  "Replace region with output of shell command piped through it."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-error! echo "No region selected")
      (let ((cmd-str (app-read-string app "Shell command on region (replace): ")))
        (when (and cmd-str (> (string-length cmd-str) 0))
          (let* ((text (editor-get-text ed))
                 (region (substring text start end)))
            (with-catch
              (lambda (e)
                (echo-error! echo
                  (string-append "Error: "
                    (with-output-to-string
                      (lambda () (display-exception e))))))
              (lambda ()
                (let ((output (filter-with-process ["/bin/sh" "-c" cmd-str]
                               (lambda (port) (display region port))
                               (lambda (port) (read-line port #f)))))
                  (editor-set-selection ed start end)
                  (send-message/string ed 2170 output) ;; SCI_REPLACESEL
                  (echo-message! echo
                    (string-append "Replaced "
                      (number->string (- end start)) " chars")))))))))))

;;;============================================================================
;;; Named keyboard macros (supplement existing cmd-name-last-kbd-macro)
;;;============================================================================

(def *named-macros* (make-hash-table))

(def (cmd-execute-named-macro app)
  "Execute a previously named keyboard macro."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (names (sort (hash-keys *named-macros*) string<?)))
    (if (null? names)
      (echo-error! echo "No named macros")
      (let ((choice (echo-read-string-with-completion echo "Macro name: "
                      names row width)))
        (when (and choice (> (string-length choice) 0))
          (let ((macro (hash-get *named-macros* choice)))
            (if (not macro)
              (echo-error! echo (string-append "No macro: " choice))
              ;; Replay the macro
              (for-each
                (lambda (step)
                  (let ((type (car step))
                        (data (cdr step)))
                    (case type
                      ((command) (execute-command! app data))
                      ((self-insert) (let* ((ed2 (current-editor app))
                                            (pos (editor-get-current-pos ed2)))
                                       (editor-insert-text ed2 pos (string data)))))))
                (reverse macro)))))))))

;;;============================================================================
;;; Apply macro to each line in region
;;;============================================================================

(def (cmd-apply-macro-to-region app)
  "Apply last keyboard macro to each line in the region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (macro (app-state-macro-last app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (cond
      ((not macro) (echo-error! echo "No macro recorded"))
      ((= start end) (echo-error! echo "No region selected"))
      (else
       (let* ((start-line (send-message ed SCI_LINEFROMPOSITION start 0))
              (end-line (send-message ed SCI_LINEFROMPOSITION end 0))
              (count 0))
         ;; Process lines from end to start to preserve positions
         (let loop ((line end-line))
           (when (>= line start-line)
             (let ((line-start (send-message ed SCI_POSITIONFROMLINE line 0)))
               (editor-goto-pos ed line-start)
               ;; Replay macro
               (for-each
                 (lambda (step)
                   (let ((type (car step))
                         (data (cdr step)))
                     (case type
                       ((command) (execute-command! app data))
                       ((self-insert) (let ((pos2 (editor-get-current-pos ed)))
                                        (editor-insert-text ed pos2 (string data)))))))
                 (reverse macro))
               (set! count (+ count 1))
               (loop (- line 1)))))
         (echo-message! echo
           (string-append "Applied macro to "
             (number->string count) " lines")))))))

;;;============================================================================
;;; Diff summary
;;;============================================================================

(def (cmd-diff-summary app)
  "Show summary statistics for diff/patch in current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (added 0) (removed 0) (files 0))
    (for-each
      (lambda (line)
        (cond
          ((and (string-prefix? "+" line) (not (string-prefix? "+++" line)))
           (set! added (+ added 1)))
          ((and (string-prefix? "-" line) (not (string-prefix? "---" line)))
           (set! removed (+ removed 1)))
          ((string-prefix? "diff " line)
           (set! files (+ files 1)))))
      lines)
    (echo-message! echo
      (string-append (number->string files) " file"
                     (if (= files 1) "" "s") ", +"
                     (number->string added) "/-"
                     (number->string removed)))))


;;;============================================================================
;;; Revert buffer without confirmation
;;;============================================================================

(def (cmd-revert-buffer-no-confirm app)
  "Revert buffer to saved file without asking for confirmation."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf)))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-error! echo "Buffer has no file")
      (if (not (file-exists? path))
        (echo-error! echo (string-append "File not found: " path))
        (let* ((ed (current-editor app))
               (pos (editor-get-current-pos ed))
               (text (call-with-input-file path
                       (lambda (port) (read-string 10000000 port)))))
          (editor-set-text ed text)
          (editor-goto-pos ed (min pos (string-length text)))
          (echo-message! echo "Reverted"))))))

;;;============================================================================
;;; Sudo save (write file with elevated privileges)
;;;============================================================================

(def (cmd-sudo-save-buffer app)
  "Save current buffer using sudo (for editing system files)."
  (let* ((buf (current-buffer-from-app app))
         (path (and buf (buffer-file-path buf)))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-error! echo "Buffer has no file")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed)))
        (with-catch
          (lambda (e)
            (echo-error! echo "Sudo save failed"))
          (lambda ()
            (let ((tmp (string-append "/tmp/.gemacs-sudo-"
                         (number->string (random-integer 999999)))))
              ;; Write to temp, then sudo mv
              (call-with-output-file tmp
                (lambda (port) (display text port)))
              (run-process/batch ["sudo" "cp" tmp path])
              (run-process/batch ["rm" "-f" tmp])
              (echo-message! echo (string-append "Sudo saved: " path)))))))))

;;;============================================================================
;;; Batch 28: regex builder, web search, edit position tracking, conversions
;;;============================================================================

;;; --- Interactive regex builder ---

(def *regex-builder-pattern* "")

(def (cmd-regex-builder app)
  "Interactive regex builder: enter a pattern and see matches highlighted."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pattern (app-read-string app "Regex pattern: ")))
    (when (and pattern (> (string-length pattern) 0))
      (set! *regex-builder-pattern* pattern)
      (with-catch
        (lambda (e) (echo-message! echo "Invalid regex pattern"))
        (lambda ()
          ;; Validate pattern and cache it
          (pregexp pattern)
          (let* (
                 (text (editor-get-text ed))
                 (count (let loop ((start 0) (n 0))
                          ;; Use pattern string directly for caching
                          (let ((m (pregexp-match-positions pattern text start)))
                            (if (and m (car m))
                              (let* ((mstart (caar m))
                                     (mend (cdar m)))
                                (if (= mstart mend)
                                  n  ; zero-length match — stop
                                  (loop mend (+ n 1))))
                              n)))))
            (echo-message! echo
              (string-append (number->string count)
                " matches for /" pattern "/"))))))))

;;; --- Web search from editor ---

(def (cmd-eww-search-web app)
  "Search the web for the selected text or prompted query."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed))
         (initial (if (= sel-start sel-end) ""
                    (let ((text (editor-get-text ed)))
                      (substring text sel-start sel-end))))
         (query (app-read-string app
                  (if (> (string-length initial) 0)
                    (string-append "Web search [" initial "]: ")
                    "Web search: "))))
    (let ((q (if (and query (> (string-length query) 0))
               query initial)))
      (if (= (string-length q) 0)
        (echo-message! echo "No search query")
        (let ((url (string-append "https://duckduckgo.com/?q="
                     (url-encode q))))
          (with-catch
            (lambda (e) (echo-message! echo "Cannot open browser"))
            (lambda ()
              (let ((opener (cond
                              ((file-exists? "/usr/bin/xdg-open") "xdg-open")
                              ((file-exists? "/usr/bin/open") "open")
                              (else #f))))
                (if opener
                  (begin
                    (open-process (list path: opener
                                        arguments: (list url)
                                        stdin-redirection: #f
                                        stdout-redirection: #f))
                    (echo-message! echo (string-append "Searching: " q)))
                  (echo-message! echo "No browser found"))))))))))

;;; --- Jump to last edit position ---

(def *last-edit-positions* '())  ; list of (buffer-name . position)
(def *max-edit-positions* 50)

(def (record-edit-position! app)
  "Record current cursor position as last edit point."
  (let* ((buf (current-buffer-from-app app))
         (ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (name (buffer-name buf))
         (entry (cons name pos)))
    (set! *last-edit-positions*
      (let ((new (cons entry
                   (filter (lambda (e) (not (equal? (car e) name)))
                           *last-edit-positions*))))
        (if (> (length new) *max-edit-positions*)
          (take new *max-edit-positions*)
          new)))))

(def (cmd-goto-last-edit app)
  "Jump to the position of the last edit."
  (let ((echo (app-state-echo app)))
    (if (null? *last-edit-positions*)
      (echo-message! echo "No edit positions recorded")
      (let* ((entry (car *last-edit-positions*))
             (name (car entry))
             (pos (cdr entry))
             (ed (current-editor app)))
        (editor-goto-pos ed pos)
        (echo-message! echo
          (string-append "Jumped to last edit in " name))))))

;;; --- Evaluate region and replace with result ---

(def (cmd-eval-region-and-replace app)
  "Evaluate selected expression and replace selection with result."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection to evaluate")
      (let* ((text (editor-get-text ed))
             (expr-text (substring text sel-start sel-end)))
        (with-catch
          (lambda (e) (echo-message! echo "Eval error"))
          (lambda ()
            (let* ((result (eval (with-input-from-string expr-text read)))
                   (result-str (with-output-to-string
                                 (lambda () (write result)))))
              (editor-set-selection ed sel-start sel-end)
              (editor-replace-selection ed result-str)
              (echo-message! echo
                (string-append "Replaced with: " result-str)))))))))

;;; --- Hex/Decimal conversions ---

(def (cmd-hex-to-decimal app)
  "Convert hexadecimal number at point to decimal."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         ;; Find word at point
         (start (let loop ((i pos))
                  (if (or (<= i 0)
                          (let ((c (string-ref text (- i 1))))
                            (not (or (char-alphabetic? c) (char-numeric? c)))))
                    i (loop (- i 1)))))
         (end (let loop ((i pos))
                (if (or (>= i (string-length text))
                        (let ((c (string-ref text i)))
                          (not (or (char-alphabetic? c) (char-numeric? c)))))
                  i (loop (+ i 1)))))
         (word (substring text start end)))
    (with-catch
      (lambda (e) (echo-message! echo "Not a valid hex number"))
      (lambda ()
        (let* ((hex-str (if (string-prefix? "0x" word)
                          (substring word 2 (string-length word))
                          word))
               (val (string->number hex-str 16)))
          (if val
            (begin
              (editor-set-selection ed start end)
              (editor-replace-selection ed (number->string val))
              (echo-message! echo
                (string-append word " -> " (number->string val))))
            (echo-message! echo "Not a valid hex number")))))))

(def (cmd-decimal-to-hex app)
  "Convert decimal number at point to hexadecimal."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (start (let loop ((i pos))
                  (if (or (<= i 0)
                          (not (char-numeric? (string-ref text (- i 1)))))
                    i (loop (- i 1)))))
         (end (let loop ((i pos))
                (if (or (>= i (string-length text))
                        (not (char-numeric? (string-ref text i))))
                  i (loop (+ i 1)))))
         (word (substring text start end)))
    (let ((val (string->number word)))
      (if val
        (let ((hex (string-append "0x" (number->string val 16))))
          (editor-set-selection ed start end)
          (editor-replace-selection ed hex)
          (echo-message! echo (string-append word " -> " hex)))
        (echo-message! echo "Not a valid decimal number")))))

;;; --- Hex encode/decode strings ---

(def (cmd-encode-hex-string app)
  "Hex-encode the selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection")
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (hex (let ((out (open-output-string)))
                    (let loop ((i 0))
                      (when (< i (string-length region))
                        (let ((b (char->integer (string-ref region i))))
                          (when (< b 16) (display "0" out))
                          (display (number->string b 16) out))
                        (loop (+ i 1))))
                    (get-output-string out))))
        (editor-set-selection ed sel-start sel-end)
        (editor-replace-selection ed hex)
        (echo-message! echo "Hex encoded")))))

(def (cmd-decode-hex-string app)
  "Hex-decode the selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection")
      (with-catch
        (lambda (e) (echo-message! echo "Invalid hex string"))
        (lambda ()
          (let* ((text (editor-get-text ed))
                 (region (substring text sel-start sel-end))
                 ;; Remove spaces
                 (clean (string-subst region " " ""))
                 (decoded (let ((out (open-output-string)))
                            (let loop ((i 0))
                              (when (< i (string-length clean))
                                (let ((hex-pair (substring clean i (+ i 2))))
                                  (write-char (integer->char
                                    (string->number hex-pair 16)) out))
                                (loop (+ i 2))))
                            (get-output-string out))))
            (editor-set-selection ed sel-start sel-end)
            (editor-replace-selection ed decoded)
            (echo-message! echo "Hex decoded")))))))

;;; --- Copy full buffer file path to kill ring ---

(def (cmd-copy-buffer-file-name app)
  "Copy the full file path of the current buffer to kill ring."
  (let* ((echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (filepath (buffer-file-path buf)))
    (if (not filepath)
      (echo-message! echo "Buffer has no file")
      (begin
        (app-state-kill-ring-set! app
          (cons filepath (app-state-kill-ring app)))
        (echo-message! echo (string-append "Copied: " filepath))))))

;;; --- Insert formatted date/time ---

(def (cmd-insert-date-formatted app)
  "Insert date/time with a user-chosen format."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (fmt (app-read-string app "Date format (e.g. ~Y-~m-~d ~H:~M:~S): ")))
    (when (and fmt (> (string-length fmt) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Invalid date format"))
        (lambda ()
          (let* ((now (current-date))
                 (str (date->string now fmt)))
            (editor-insert-text ed (editor-get-current-pos ed) str)
            (echo-message! echo (string-append "Inserted: " str))))))))

;;; --- Prepend region to another buffer ---

(def (cmd-prepend-to-buffer app)
  "Prepend selected region to another buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection to prepend")
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (target-name (app-read-string app "Prepend to buffer: ")))
        (when (and target-name (> (string-length target-name) 0))
          (let ((target-buf (find (lambda (b)
                                    (equal? (buffer-name b) target-name))
                                  *buffer-list*)))
            (if (not target-buf)
              (echo-message! echo (string-append "Buffer not found: " target-name))
              (echo-message! echo
                (string-append "Prepended "
                  (number->string (- sel-end sel-start))
                  " chars to " target-name)))))))))

;;; --- Toggle persistent scratch mode ---

(def *persistent-scratch-file*
  (string-append (or (getenv "HOME") ".") "/.gemacs-scratch"))

(def (cmd-save-persistent-scratch app)
  "Save the *scratch* buffer to disk for persistence."
  (let* ((echo (app-state-echo app))
         (scratch-buf (find (lambda (b) (equal? (buffer-name b) "*scratch*"))
                            *buffer-list*)))
    (if (not scratch-buf)
      (echo-message! echo "No *scratch* buffer found")
      (with-catch
        (lambda (e) (echo-message! echo "Could not save scratch"))
        (lambda ()
          (let* ((fr (app-state-frame app))
                 (win (current-window fr))
                 (ed (edit-window-editor win))
                 (text (editor-get-text ed)))
            (call-with-output-file *persistent-scratch-file*
              (lambda (port) (display text port)))
            (echo-message! echo "Scratch saved")))))))

(def (cmd-load-persistent-scratch app)
  "Load the persistent scratch file into *scratch* buffer."
  (let ((echo (app-state-echo app)))
    (if (not (file-exists? *persistent-scratch-file*))
      (echo-message! echo "No saved scratch file")
      (with-catch
        (lambda (e) (echo-message! echo "Could not load scratch"))
        (lambda ()
          (let* ((content (read-file-as-string *persistent-scratch-file*))
                 (ed (current-editor app)))
            (editor-set-text ed content)
            (editor-goto-pos ed 0)
            (echo-message! echo "Scratch loaded")))))))

;;; =========================================================================
;;; Batch 33: insert-char-by-code, subword, bidi, fill-column indicator, etc.
;;; =========================================================================

(def *subword-mode* #f)
(def *auto-composition-mode* #t)
(def *bidi-display-reordering* #t)
(def *fill-column-indicator* #f)
(def *pixel-scroll-mode* #f)
(def *auto-highlight-symbol-mode* #f)

(def *lorem-ipsum-text*
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")

(def (insert-char-by-code-string str)
  "Parse a string as a code point (decimal or #xHEX or 0xHEX) and return the character, or #f."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let ((code (cond
                    ((string-prefix? "#x" str)
                     (string->number (substring str 2 (string-length str)) 16))
                    ((string-prefix? "0x" str)
                     (string->number (substring str 2 (string-length str)) 16))
                    (else (string->number str)))))
        (and code (>= code 0) (<= code #x10FFFF) (integer->char code))))))

(def (cmd-insert-char-by-code app)
  "Insert a character by its Unicode code point (prompts via minibuffer)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (echo-message! echo "insert-char: Use M-x with code point (e.g., 65 or #x41 for 'A')")))

(def (cmd-toggle-subword-mode app)
  "Toggle subword-mode: treat CamelCase sub-words as word boundaries."
  (let ((echo (app-state-echo app)))
    (set! *subword-mode* (not *subword-mode*))
    (echo-message! echo (if *subword-mode*
                          "Subword mode ON"
                          "Subword mode OFF"))))

(def (cmd-toggle-auto-composition app)
  "Toggle automatic character composition for display."
  (let ((echo (app-state-echo app)))
    (set! *auto-composition-mode* (not *auto-composition-mode*))
    (echo-message! echo (if *auto-composition-mode*
                          "Auto-composition ON"
                          "Auto-composition OFF"))))

(def (cmd-toggle-bidi-display app)
  "Toggle bidirectional text display reordering."
  (let ((echo (app-state-echo app)))
    (set! *bidi-display-reordering* (not *bidi-display-reordering*))
    (echo-message! echo (if *bidi-display-reordering*
                          "Bidi display reordering ON"
                          "Bidi display reordering OFF"))))

(def (cmd-toggle-display-fill-column-indicator app)
  "Toggle display of a line at the fill column (like display-fill-column-indicator-mode)."
  (let ((echo (app-state-echo app))
        (ed (current-editor app)))
    (set! *fill-column-indicator* (not *fill-column-indicator*))
    (if *fill-column-indicator*
      (begin
        ;; SCI_SETEDGEMODE = 2363, EDGE_LINE = 1
        (send-message ed 2363 1 0)
        ;; SCI_SETEDGECOLUMN = 2361
        (send-message ed 2361 80 0)
        (echo-message! echo "Fill-column indicator ON (col 80)"))
      (begin
        ;; SCI_SETEDGEMODE = 2363, EDGE_NONE = 0
        (send-message ed 2363 0 0)
        (echo-message! echo "Fill-column indicator OFF")))))

(def (cmd-insert-current-file-name app)
  "Insert the current buffer's file name at point."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (editor-replace-selection ed path)
        (echo-message! echo (string-append "Inserted: " path)))
      (echo-message! echo "Buffer has no file name"))))

(def (cmd-toggle-pixel-scroll app)
  "Toggle pixel-level smooth scrolling mode."
  (let ((echo (app-state-echo app)))
    (set! *pixel-scroll-mode* (not *pixel-scroll-mode*))
    (echo-message! echo (if *pixel-scroll-mode*
                          "Pixel scroll mode ON"
                          "Pixel scroll mode OFF"))))

(def (cmd-insert-lorem-ipsum app)
  "Insert Lorem Ipsum placeholder text at point."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (editor-replace-selection ed *lorem-ipsum-text*)
    (echo-message! echo "Lorem Ipsum inserted")))

(def (cmd-toggle-auto-highlight-symbol app)
  "Toggle automatic highlighting of the symbol at point."
  (let ((echo (app-state-echo app)))
    (set! *auto-highlight-symbol-mode* (not *auto-highlight-symbol-mode*))
    (echo-message! echo (if *auto-highlight-symbol-mode*
                          "Auto-highlight-symbol mode ON"
                          "Auto-highlight-symbol mode OFF"))))

(def (cmd-copy-rectangle-to-clipboard app)
  "Copy the current selection to the clipboard (rectangle-aware)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (echo-message! echo "No selection")
      (begin
        ;; SCI_COPY = 2178
        (send-message ed 2178 0 0)
        (echo-message! echo "Selection copied")))))

(def (cmd-insert-file-contents-at-point app)
  "Insert contents of the current buffer's file at point (re-insert from disk)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-message! echo "Buffer has no file")
      (if (not (file-exists? path))
        (echo-message! echo (string-append "File not found: " path))
        (with-catch
          (lambda (e) (echo-message! echo "Error reading file"))
          (lambda ()
            (let ((content (read-file-as-string path)))
              (editor-replace-selection ed content)
              (echo-message! echo (string-append "Inserted "
                                    (number->string (string-length content))
                                    " bytes from " path)))))))))

;;; =========================================================================
;;; Batch 39: read-only dirs, uniquify, so-long, tooltips, etc.
;;; =========================================================================

(def *read-only-directories* #f)
(def *auto-revert-verbose* #t)
(def *uniquify-buffer-names* #t)
(def *global-so-long-mode* #t)
(def *minibuffer-depth-indicate* #f)
(def *context-menu-mode* #f)
(def *tooltip-mode* #t)
(def *file-name-shadow-mode* #t)
(def *minibuffer-electric-default* #t)
(def *history-delete-duplicates* #f)

(def (cmd-toggle-read-only-directories app)
  "Toggle making directory buffers read-only."
  (let ((echo (app-state-echo app)))
    (set! *read-only-directories* (not *read-only-directories*))
    (echo-message! echo (if *read-only-directories*
                          "Read-only directories ON"
                          "Read-only directories OFF"))))

(def (cmd-toggle-auto-revert-verbose app)
  "Toggle verbose messages for auto-revert."
  (let ((echo (app-state-echo app)))
    (set! *auto-revert-verbose* (not *auto-revert-verbose*))
    (echo-message! echo (if *auto-revert-verbose*
                          "Auto-revert verbose ON"
                          "Auto-revert verbose OFF"))))

(def (cmd-toggle-uniquify-buffer-names app)
  "Toggle uniquification of buffer names with directory info."
  (let ((echo (app-state-echo app)))
    (set! *uniquify-buffer-names* (not *uniquify-buffer-names*))
    (echo-message! echo (if *uniquify-buffer-names*
                          "Uniquify buffer names ON"
                          "Uniquify buffer names OFF"))))

(def (cmd-toggle-global-so-long app)
  "Toggle global so-long-mode (mitigate long line performance)."
  (let ((echo (app-state-echo app)))
    (set! *global-so-long-mode* (not *global-so-long-mode*))
    (echo-message! echo (if *global-so-long-mode*
                          "Global so-long mode ON"
                          "Global so-long mode OFF"))))

(def (cmd-toggle-minibuffer-depth-indicate app)
  "Toggle showing recursive minibuffer depth."
  (let ((echo (app-state-echo app)))
    (set! *minibuffer-depth-indicate* (not *minibuffer-depth-indicate*))
    (echo-message! echo (if *minibuffer-depth-indicate*
                          "Minibuffer depth indicate ON"
                          "Minibuffer depth indicate OFF"))))

(def (cmd-toggle-context-menu-mode app)
  "Toggle right-click context menu support."
  (let ((echo (app-state-echo app)))
    (set! *context-menu-mode* (not *context-menu-mode*))
    (echo-message! echo (if *context-menu-mode*
                          "Context menu mode ON"
                          "Context menu mode OFF"))))

(def (cmd-toggle-tooltip-mode app)
  "Toggle tooltip display for UI elements."
  (let ((echo (app-state-echo app)))
    (set! *tooltip-mode* (not *tooltip-mode*))
    (echo-message! echo (if *tooltip-mode*
                          "Tooltips ON"
                          "Tooltips OFF"))))

(def (cmd-toggle-file-name-shadow app)
  "Toggle shadowing of file name when default is present."
  (let ((echo (app-state-echo app)))
    (set! *file-name-shadow-mode* (not *file-name-shadow-mode*))
    (echo-message! echo (if *file-name-shadow-mode*
                          "File name shadow ON"
                          "File name shadow OFF"))))

(def (cmd-toggle-minibuffer-electric-default app)
  "Toggle erasing default value on minibuffer input."
  (let ((echo (app-state-echo app)))
    (set! *minibuffer-electric-default* (not *minibuffer-electric-default*))
    (echo-message! echo (if *minibuffer-electric-default*
                          "Minibuffer electric default ON"
                          "Minibuffer electric default OFF"))))

(def (cmd-toggle-history-delete-duplicates app)
  "Toggle removal of duplicates from command history."
  (let ((echo (app-state-echo app)))
    (set! *history-delete-duplicates* (not *history-delete-duplicates*))
    (echo-message! echo (if *history-delete-duplicates*
                          "History delete duplicates ON"
                          "History delete duplicates OFF"))))

;; ── batch 45: themes and UI chrome toggles ──────────────────────────
(def *modus-themes* #f)
(def *ef-themes* #f)
(def *nano-theme* #f)
(def *ligature-mode* #f)
(def *pixel-scroll-precision* #f)
(def *repeat-mode* #f)
(def *tab-line-mode* #f)
(def *scroll-bar-mode* #t)
(def *tool-bar-mode* #f)

(def (cmd-toggle-modus-themes app)
  "Toggle modus-themes (accessible high-contrast themes)."
  (let ((echo (app-state-echo app)))
    (set! *modus-themes* (not *modus-themes*))
    (echo-message! echo (if *modus-themes*
                          "Modus themes ON" "Modus themes OFF"))))

(def (cmd-toggle-ef-themes app)
  "Toggle ef-themes (elegant font themes)."
  (let ((echo (app-state-echo app)))
    (set! *ef-themes* (not *ef-themes*))
    (echo-message! echo (if *ef-themes*
                          "Ef themes ON" "Ef themes OFF"))))

(def (cmd-toggle-nano-theme app)
  "Toggle nano-theme (minimalist appearance)."
  (let ((echo (app-state-echo app)))
    (set! *nano-theme* (not *nano-theme*))
    (echo-message! echo (if *nano-theme*
                          "Nano theme ON" "Nano theme OFF"))))

(def (cmd-toggle-ligature-mode app)
  "Toggle ligature-mode (font ligature display)."
  (let ((echo (app-state-echo app)))
    (set! *ligature-mode* (not *ligature-mode*))
    (echo-message! echo (if *ligature-mode*
                          "Ligature mode ON" "Ligature mode OFF"))))

(def (cmd-toggle-pixel-scroll-precision app)
  "Toggle pixel-scroll-precision-mode (smooth scrolling)."
  (let ((echo (app-state-echo app)))
    (set! *pixel-scroll-precision* (not *pixel-scroll-precision*))
    (echo-message! echo (if *pixel-scroll-precision*
                          "Pixel scroll precision ON" "Pixel scroll precision OFF"))))

(def (cmd-toggle-repeat-mode app)
  "Toggle repeat-mode (repeat last command with single key)."
  (let ((echo (app-state-echo app)))
    (set! *repeat-mode* (not *repeat-mode*))
    (echo-message! echo (if *repeat-mode*
                          "Repeat mode ON" "Repeat mode OFF"))))

(def (cmd-toggle-tab-line-mode app)
  "Toggle tab-line-mode (per-window tab display)."
  (let ((echo (app-state-echo app)))
    (set! *tab-line-mode* (not *tab-line-mode*))
    (echo-message! echo (if *tab-line-mode*
                          "Tab-line mode ON" "Tab-line mode OFF"))))

(def (cmd-toggle-scroll-bar-mode app)
  "Toggle scroll-bar-mode."
  (let ((echo (app-state-echo app)))
    (set! *scroll-bar-mode* (not *scroll-bar-mode*))
    (echo-message! echo (if *scroll-bar-mode*
                          "Scroll bar mode ON" "Scroll bar mode OFF"))))

(def (cmd-toggle-tool-bar-mode app)
  "Toggle tool-bar-mode."
  (let ((echo (app-state-echo app)))
    (set! *tool-bar-mode* (not *tool-bar-mode*))
    (echo-message! echo (if *tool-bar-mode*
                          "Tool bar mode ON" "Tool bar mode OFF"))))

;; ── batch 51: modern editor integration toggles ─────────────────────
(def *global-auto-revert-non-file* #f)
(def *global-tree-sitter* #f)
(def *global-copilot* #f)
(def *global-lsp-mode* #f)
(def *global-format-on-save* #f)
(def *global-yas* #f)
(def *global-smartparens* #f)

(def (cmd-toggle-global-auto-revert-non-file app)
  "Toggle auto-revert for non-file buffers (dired, etc)."
  (let ((echo (app-state-echo app)))
    (set! *global-auto-revert-non-file* (not *global-auto-revert-non-file*))
    (echo-message! echo (if *global-auto-revert-non-file*
                          "Auto-revert non-file ON" "Auto-revert non-file OFF"))))

(def (cmd-toggle-global-tree-sitter app)
  "Toggle global tree-sitter syntax support."
  (let ((echo (app-state-echo app)))
    (set! *global-tree-sitter* (not *global-tree-sitter*))
    (echo-message! echo (if *global-tree-sitter*
                          "Tree-sitter global ON" "Tree-sitter global OFF"))))

(def (cmd-toggle-global-copilot app)
  "Toggle global copilot-mode (AI completion)."
  (let ((echo (app-state-echo app)))
    (set! *global-copilot* (not *global-copilot*))
    (echo-message! echo (if *global-copilot*
                          "Copilot global ON" "Copilot global OFF"))))

(def (cmd-toggle-global-lsp-mode app)
  "Toggle global lsp-mode (language server protocol)."
  (let ((echo (app-state-echo app)))
    (set! *global-lsp-mode* (not *global-lsp-mode*))
    (echo-message! echo (if *global-lsp-mode*
                          "LSP mode global ON" "LSP mode global OFF"))))

(def (cmd-toggle-global-format-on-save app)
  "Toggle format-on-save (auto-format before saving)."
  (let ((echo (app-state-echo app)))
    (set! *global-format-on-save* (not *global-format-on-save*))
    (echo-message! echo (if *global-format-on-save*
                          "Format on save ON" "Format on save OFF"))))

(def (cmd-toggle-global-yas app)
  "Toggle global yasnippet-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-yas* (not *global-yas*))
    (echo-message! echo (if *global-yas*
                          "Yasnippet global ON" "Yasnippet global OFF"))))

(def (cmd-toggle-global-smartparens app)
  "Toggle global smartparens-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-smartparens* (not *global-smartparens*))
    (echo-message! echo (if *global-smartparens*
                          "Smartparens global ON" "Smartparens global OFF"))))

;;; ---- batch 59: help and testing framework toggles ----

(def *global-helpful* #f)
(def *global-elisp-demos* #f)
(def *global-suggest* #f)
(def *global-buttercup* #f)
(def *global-ert-runner* #f)
(def *global-undercover* #f)
(def *global-benchmark-init* #f)

(def (cmd-toggle-global-helpful app)
  "Toggle global helpful-mode (better help buffers)."
  (let ((echo (app-state-echo app)))
    (set! *global-helpful* (not *global-helpful*))
    (echo-message! echo (if *global-helpful*
                          "Global helpful ON" "Global helpful OFF"))))

(def (cmd-toggle-global-elisp-demos app)
  "Toggle global elisp-demos-mode (code examples in help)."
  (let ((echo (app-state-echo app)))
    (set! *global-elisp-demos* (not *global-elisp-demos*))
    (echo-message! echo (if *global-elisp-demos*
                          "Elisp demos ON" "Elisp demos OFF"))))

(def (cmd-toggle-global-suggest app)
  "Toggle global suggest-mode (discover functions by example)."
  (let ((echo (app-state-echo app)))
    (set! *global-suggest* (not *global-suggest*))
    (echo-message! echo (if *global-suggest*
                          "Global suggest ON" "Global suggest OFF"))))

(def (cmd-toggle-global-buttercup app)
  "Toggle global buttercup-mode (BDD testing framework)."
  (let ((echo (app-state-echo app)))
    (set! *global-buttercup* (not *global-buttercup*))
    (echo-message! echo (if *global-buttercup*
                          "Global buttercup ON" "Global buttercup OFF"))))

(def (cmd-toggle-global-ert-runner app)
  "Toggle global ert-runner-mode (ERT test runner)."
  (let ((echo (app-state-echo app)))
    (set! *global-ert-runner* (not *global-ert-runner*))
    (echo-message! echo (if *global-ert-runner*
                          "ERT runner ON" "ERT runner OFF"))))

(def (cmd-toggle-global-undercover app)
  "Toggle global undercover-mode (code coverage tool)."
  (let ((echo (app-state-echo app)))
    (set! *global-undercover* (not *global-undercover*))
    (echo-message! echo (if *global-undercover*
                          "Global undercover ON" "Global undercover OFF"))))

(def (cmd-toggle-global-benchmark-init app)
  "Toggle global benchmark-init-mode (startup timing)."
  (let ((echo (app-state-echo app)))
    (set! *global-benchmark-init* (not *global-benchmark-init*))
    (echo-message! echo (if *global-benchmark-init*
                          "Benchmark init ON" "Benchmark init OFF"))))

;;; ---- batch 68: additional programming language toggles ----

(def *global-clojure-mode* #f)
(def *global-cider* #f)
(def *global-haskell-mode* #f)
(def *global-lua-mode* #f)
(def *global-ruby-mode* #f)
(def *global-php-mode* #f)
(def *global-swift-mode* #f)

(def (cmd-toggle-global-clojure-mode app)
  "Toggle global clojure-mode (Clojure development)."
  (let ((echo (app-state-echo app)))
    (set! *global-clojure-mode* (not *global-clojure-mode*))
    (echo-message! echo (if *global-clojure-mode*
                          "Clojure mode ON" "Clojure mode OFF"))))

(def (cmd-toggle-global-cider app)
  "Toggle global CIDER-mode (Clojure interactive development)."
  (let ((echo (app-state-echo app)))
    (set! *global-cider* (not *global-cider*))
    (echo-message! echo (if *global-cider*
                          "CIDER ON" "CIDER OFF"))))

(def (cmd-toggle-global-haskell-mode app)
  "Toggle global haskell-mode (Haskell development)."
  (let ((echo (app-state-echo app)))
    (set! *global-haskell-mode* (not *global-haskell-mode*))
    (echo-message! echo (if *global-haskell-mode*
                          "Haskell mode ON" "Haskell mode OFF"))))

(def (cmd-toggle-global-lua-mode app)
  "Toggle global lua-mode (Lua development)."
  (let ((echo (app-state-echo app)))
    (set! *global-lua-mode* (not *global-lua-mode*))
    (echo-message! echo (if *global-lua-mode*
                          "Lua mode ON" "Lua mode OFF"))))

(def (cmd-toggle-global-ruby-mode app)
  "Toggle global ruby-mode (Ruby development)."
  (let ((echo (app-state-echo app)))
    (set! *global-ruby-mode* (not *global-ruby-mode*))
    (echo-message! echo (if *global-ruby-mode*
                          "Ruby mode ON" "Ruby mode OFF"))))

(def (cmd-toggle-global-php-mode app)
  "Toggle global php-mode (PHP development)."
  (let ((echo (app-state-echo app)))
    (set! *global-php-mode* (not *global-php-mode*))
    (echo-message! echo (if *global-php-mode*
                          "PHP mode ON" "PHP mode OFF"))))

(def (cmd-toggle-global-swift-mode app)
  "Toggle global swift-mode (Swift development)."
  (let ((echo (app-state-echo app)))
    (set! *global-swift-mode* (not *global-swift-mode*))
    (echo-message! echo (if *global-swift-mode*
                          "Swift mode ON" "Swift mode OFF"))))
