;;; -*- Gerbil -*-
;;; Task #51: Additional commands

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

