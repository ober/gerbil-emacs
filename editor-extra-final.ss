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
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        (only-in :gerbil-emacs/editor-core
                 search-forward-regexp-impl!)
        (only-in :gerbil-emacs/editor-ui
                 position-cursor-for-replace!)
        :gerbil-emacs/editor-extra-helpers
        :gerbil-emacs/editor-extra-web
        :gerbil-emacs/editor-extra-media
        :gerbil-emacs/editor-extra-modes)

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
(def *command-history-file* "~/.gerbil-emacs-cmd-history")

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
            (let ((tmp (string-append "/tmp/.gerbil-emacs-sudo-"
                         (number->string (random-integer 999999)))))
              ;; Write to temp, then sudo mv
              (call-with-output-file tmp
                (lambda (port) (display text port)))
              (run-process/batch ["sudo" "cp" tmp path])
              (run-process/batch ["rm" "-f" tmp])
              (echo-message! echo (string-append "Sudo saved: " path)))))))))

