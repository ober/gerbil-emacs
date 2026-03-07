;;; -*- Gerbil -*-
;;; Qt application and event loop for gemacs

(export qt-main qt-open-file!)

(import :std/sugar
        :std/misc/string
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/async
        :gemacs/editor
        (only-in :gemacs/persist init-file-load!
                 detect-major-mode buffer-local-set!
                 theme-settings-load! custom-faces-load!)
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/shell-history
        :gemacs/terminal
        :gemacs/chat
        :gemacs/qt/keymap
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/modeline
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/image
        :gemacs/qt/commands
        :gemacs/qt/lsp-client
        :gemacs/qt/commands-lsp
        :gemacs/qt/menubar
        :gemacs/ipc
        :gemacs/vtscreen)

;;;============================================================================
;;; Qt Application
;;;============================================================================

;; Auto-save path: #filename# (Emacs convention)
(def (qt-make-auto-save-path path)
  (let* ((dir (path-directory path))
         (name (path-strip-directory path)))
    (path-expand (string-append "#" name "#") dir)))

(def (qt-update-frame-title! app)
  "Update window title to show current buffer and file path."
  (let* ((fr (app-state-frame app))
         (win (qt-frame-main-win fr))
         (buf (qt-current-buffer fr))
         (name (buffer-name buf))
         (path (buffer-file-path buf))
         (modified? (and (buffer-doc-pointer buf)
                         (qt-text-document-modified? (buffer-doc-pointer buf))))
         (title (string-append
                  (if modified? "* " "")
                  name
                  (if path (string-append " - " path) "")
                  " - gemacs")))
    (qt-main-window-set-title! win title)))

(def (qt-update-mark-selection! app)
  "Update visual selection to reflect active mark region.
   When buffer-mark is set, highlights the region between mark and cursor.
   When mark is cleared, ensures no stale selection remains."
  (let* ((fr (app-state-frame app))
         (ed (qt-current-editor fr))
         (buf (qt-current-buffer fr))
         (mark (buffer-mark buf)))
    (if mark
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        ;; anchor=mark (fixed end), caret=pos (moving end).
        ;; Must NOT normalize with min/max: that swaps anchor/caret when
        ;; navigating backwards (mark > pos), causing SCI_GETCURRENTPOS to
        ;; return the mark position and making collapse-selection-to-caret!
        ;; snap back to the mark on every subsequent keypress.
        (qt-plain-text-edit-set-selection! ed mark pos))
      ;; No mark — deselect if anything is selected
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        (qt-plain-text-edit-set-selection! ed pos pos)))))

;; Which-key state — show available bindings after prefix key delay
(def *which-key-timer* #f)
(def *which-key-pending-keymap* #f)
(def *which-key-pending-prefix* #f)

(def (which-key-format-bindings km prefix-str)
  "Format keymap bindings for which-key display.
   Shows key → Description pairs with human-readable command names."
  (let* ((entries (keymap-entries km))
         (describe (lambda (cmd)
                     (cond
                       ((hash-table? cmd) "+prefix")
                       ((symbol? cmd) (command-name->description cmd))
                       (else "?"))))
         (strs (let loop ((es entries) (acc []))
                 (if (null? es) (reverse acc)
                   (let* ((e (car es))
                          (key (car e))
                          (val (cdr e))
                          (desc (describe val)))
                     (loop (cdr es)
                           (cons (string-append key " → " desc) acc)))))))
    (string-append prefix-str "- " (string-join strs "  "))))

;; Key-chord state — detect two rapid keystrokes as a chord
(def *chord-timer* #f)
(def *chord-pending-char* #f)  ;; first char of potential chord, or #f
(def *chord-pending-code* #f)  ;; saved raw Qt event for replay
(def *chord-pending-mods* #f)
(def *chord-pending-text* #f)

;; Tab bar state — populated during qt-main, used by qt-tabbar-update!
(def *tab-bar-layout* #f)
(def *tab-bar-buttons* '())  ;; list of (buffer . button) pairs
(def *tab-bar-last-state* #f)  ;; cache: (current-buf . buffer-count) to skip redundant updates
(def *tab-bar-widget* #f)     ;; the tab bar widget itself (for show/hide)

(def (qt-tabbar-update! app)
  "Rebuild the tab bar to reflect current buffer list."
  ;; Show/hide the tab bar widget
  (when *tab-bar-widget*
    (if *tab-bar-visible*
      (qt-widget-show! *tab-bar-widget*)
      (qt-widget-hide! *tab-bar-widget*)))
  (when (and *tab-bar-layout* *tab-bar-visible*)
    (let* ((fr (app-state-frame app))
           (current-buf (qt-edit-window-buffer (qt-current-window fr)))
           (bufs (buffer-list))
           (new-state (cons current-buf (length bufs))))
      ;; Skip update if nothing changed
      (unless (and *tab-bar-last-state*
                   (eq? (car new-state) (car *tab-bar-last-state*))
                   (= (cdr new-state) (cdr *tab-bar-last-state*)))
        (set! *tab-bar-last-state* new-state)
        ;; Destroy old buttons
        (for-each (lambda (pair) (qt-widget-destroy! (cdr pair))) *tab-bar-buttons*)
        (set! *tab-bar-buttons* '())
        ;; Create new buttons for each buffer
        (for-each
          (lambda (buf)
            (let* ((name (buffer-name buf))
                   (mod? (and (buffer-doc-pointer buf)
                              (qt-text-document-modified? (buffer-doc-pointer buf))))
                   (label (if mod? (string-append name " *") name))
                   (btn (qt-push-button-create label)))
              ;; Style: current buffer gets highlighted
              (let ((font-css (string-append " font-family: " *default-font-family*
                                             "; font-size: " (number->string (max 1 (- *default-font-size* 2))) "pt;")))
                (if (eq? buf current-buf)
                  (qt-widget-set-style-sheet! btn
                    (string-append "QPushButton { color: #ffffff; background: #404060; border: 1px solid #606080; border-radius: 3px; padding: 2px 8px;" font-css " }"))
                  (qt-widget-set-style-sheet! btn
                    (string-append "QPushButton { color: #a0a0a0; background: #252525; border: 1px solid #383838; border-radius: 3px; padding: 2px 8px;" font-css " }\n"
                                   "                   QPushButton:hover { color: #d8d8d8; background: #353535; }"))))
              ;; Click handler: switch to this buffer
              (qt-on-clicked! btn
                (lambda ()
                  (let* ((ed (qt-current-editor fr)))
                    (qt-buffer-attach! ed buf)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                    (qt-update-visual-decorations! ed)
                    (qt-modeline-update! app)
                    (set! *tab-bar-last-state* #f)  ;; force refresh
                    (qt-tabbar-update! app))))
              ;; Add to layout
              (qt-layout-add-widget! *tab-bar-layout* btn)
              (set! *tab-bar-buttons* (cons (cons buf btn) *tab-bar-buttons*))))
          bufs)
        ;; Add stretch at the end to push tabs left
        (qt-layout-add-stretch! *tab-bar-layout*)))))

;;;============================================================================
;;; PTY output polling helpers (used by timer callback)
;;;============================================================================

(def (qt-poll-shell-pty-msg! fr buf ss msg)
  "Handle one PTY message for a shell buffer in Qt.
   Uses VT100 screen buffer to properly handle cursor-addressing programs."
  (let ((tag (car msg))
        (data (cdr msg))
        (vt (shell-state-vtscreen ss)))
    (cond
      ((eq? tag 'data)
       (let loop ((wins (qt-frame-windows fr)))
         (when (pair? wins)
           (if (eq? (qt-edit-window-buffer (car wins)) buf)
             (let ((ed (qt-edit-window-editor (car wins))))
               ;; Save pre-PTY text on first data chunk
               (when (and vt (not (shell-state-pre-pty-text ss)))
                 (set! (shell-state-pre-pty-text ss)
                   (qt-plain-text-edit-text ed)))
               (if vt
                 ;; Feed data to VT100 screen buffer, then render
                 (begin
                   (vtscreen-feed! vt data)
                   (let* ((rendered (vtscreen-render vt))
                          ;; Full-screen programs (alt-screen): show only vtscreen
                          ;; Simple commands: prepend pre-PTY text
                          (full (if (vtscreen-alt-screen? vt)
                                  rendered
                                  (string-append (or (shell-state-pre-pty-text ss) "")
                                                 rendered))))
                     (qt-plain-text-edit-set-text! ed full)
                     (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                     (qt-plain-text-edit-ensure-cursor-visible! ed)))
                 ;; Fallback: strip and append (no vtscreen)
                 (begin
                   (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                   (qt-plain-text-edit-insert-text! ed (strip-ansi-codes data))
                   (qt-plain-text-edit-ensure-cursor-visible! ed))))
             (loop (cdr wins))))))
      ((eq? tag 'done)
       ;; Capture final vtscreen state before cleanup destroys it
       (let* ((alt-screen? (and vt (vtscreen-alt-screen? vt)))
              (final-render (and vt (vtscreen-render vt)))
              (pre-text (shell-state-pre-pty-text ss)))
         (shell-cleanup-pty! ss)
         (let loop ((wins (qt-frame-windows fr)))
           (when (pair? wins)
             (if (eq? (qt-edit-window-buffer (car wins)) buf)
               (let ((ed (qt-edit-window-editor (car wins))))
                 (let ((prompt (shell-prompt ss)))
                   (when pre-text
                     (if alt-screen?
                       ;; Full-screen program (top, vim): restore pre-PTY text
                       (qt-plain-text-edit-set-text! ed pre-text)
                       ;; Simple command (ls, ps): keep output
                       (let* ((output (or final-render ""))
                              (sep (if (and (> (string-length output) 0)
                                           (not (char=? (string-ref output (- (string-length output) 1)) #\newline)))
                                     "\n" ""))
                              (full (string-append pre-text output sep)))
                         (qt-plain-text-edit-set-text! ed full))))
                   (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                   (qt-plain-text-edit-insert-text! ed prompt)
                   (set! (shell-state-prompt-pos ss)
                     (string-length (qt-plain-text-edit-text ed)))
                   (qt-plain-text-edit-ensure-cursor-visible! ed)))
               (loop (cdr wins))))))))))

(def (qt-poll-terminal-pty-batch! fr buf ts data)
  "Handle batched PTY data for a terminal buffer.
   Processes all accumulated data at once, rendering only once."
  (let ((vt (terminal-state-vtscreen ts)))
    (let loop ((wins (qt-frame-windows fr)))
      (when (pair? wins)
        (if (eq? (qt-edit-window-buffer (car wins)) buf)
          (let ((ed (qt-edit-window-editor (car wins))))
            ;; Save pre-PTY text on first data chunk
            (when (and vt (not (terminal-state-pre-pty-text ts)))
              (set! (terminal-state-pre-pty-text ts)
                (qt-plain-text-edit-text ed)))
            (if vt
              (begin
                (vtscreen-feed! vt data)
                (let* ((rendered (vtscreen-render vt))
                       (full (if (vtscreen-alt-screen? vt)
                               rendered
                               (string-append (or (terminal-state-pre-pty-text ts) "")
                                              rendered))))
                  (qt-plain-text-edit-set-text! ed full)
                  (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                  (qt-plain-text-edit-ensure-cursor-visible! ed)))
              (begin
                (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                (qt-plain-text-edit-insert-text! ed (strip-ansi-codes data))
                (qt-plain-text-edit-ensure-cursor-visible! ed))))
          (loop (cdr wins)))))))

(def (qt-poll-terminal-pty-msg! fr buf ts msg)
  "Handle one PTY message for a terminal buffer in Qt.
   Data messages are handled via qt-poll-terminal-pty-batch! for efficiency."
  (let ((tag (car msg))
        (data (cdr msg))
        (vt (terminal-state-vtscreen ts)))
    (cond
      ((eq? tag 'data)
       ;; Single data message fallback
       (qt-poll-terminal-pty-batch! fr buf ts data))
      ((eq? tag 'done)
       ;; Capture final vtscreen state before cleanup destroys it
       (let* ((alt-screen? (and vt (vtscreen-alt-screen? vt)))
              (final-render (and vt (vtscreen-render vt)))
              (pre-text (terminal-state-pre-pty-text ts)))
         (terminal-cleanup-pty! ts)
         (let loop ((wins (qt-frame-windows fr)))
           (when (pair? wins)
             (if (eq? (qt-edit-window-buffer (car wins)) buf)
               (let ((ed (qt-edit-window-editor (car wins))))
                 (let ((prompt (terminal-prompt ts)))
                   (when pre-text
                     (if alt-screen?
                       ;; Full-screen program (top, vim): restore pre-PTY text
                       (qt-plain-text-edit-set-text! ed pre-text)
                       ;; Simple command (ls, ps): keep output
                       (let* ((output (or final-render ""))
                              (sep (if (and (> (string-length output) 0)
                                           (not (char=? (string-ref output (- (string-length output) 1)) #\newline)))
                                     "\n" ""))
                              (full (string-append pre-text output sep)))
                         (qt-plain-text-edit-set-text! ed full))))
                   (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                   (qt-plain-text-edit-insert-text! ed prompt)
                   (set! (terminal-state-prompt-pos ts)
                     (string-length (qt-plain-text-edit-text ed)))
                   (qt-plain-text-edit-ensure-cursor-visible! ed)))
               (loop (cdr wins))))))))))

(def (qt-main . args)
  (with-qt-app qt-app
    ;; Initialize runtime error log (~/.gemacs-errors.log)
    (init-gemacs-log!)
    ;; Initialize face system with standard faces
    (define-standard-faces!)
    ;; Load saved theme and font settings from ~/.gemacs-theme
    (let-values (((saved-theme saved-font-family saved-font-size) (theme-settings-load!)))
      ;; Apply saved theme if valid
      (when (and saved-theme (theme-get saved-theme))
        (set! *current-theme* saved-theme))
      ;; Apply saved font family if valid
      (when (and saved-font-family (not (string-empty? saved-font-family)))
        (set! *default-font-family* saved-font-family))
      ;; Apply saved font size if valid
      (when (and saved-font-size (>= saved-font-size 6) (<= saved-font-size 72))
        (set! *default-font-size* saved-font-size)))
    ;; Load theme (populates *faces* registry from theme definition)
    (load-theme! *current-theme*)
    ;; Load custom face overrides (overlays on top of theme)
    (custom-faces-load!)
    ;; Apply theme stylesheet
    (qt-app-set-style-sheet! qt-app (theme-stylesheet))

    (let* ((win (qt-main-window-create))
           ;; Central widget with vertical layout
           (central (qt-widget-create parent: win))
           (layout (qt-vbox-layout-create central))
           ;; Tab bar for buffer switching
           (tab-bar (qt-widget-create parent: central))
           (tab-layout (qt-hbox-layout-create tab-bar))
           ;; Main content area: splitter for editors
           (splitter (qt-splitter-create QT_VERTICAL parent: central))
           (_ (begin
                ;; Window dividers: visible blue handle between split panes
                (qt-splitter-set-handle-width! splitter 3)
                (qt-widget-set-style-sheet! splitter
                  "QSplitter::handle { background: #51afef; }")))
           ;; Echo label at bottom
           (echo-label (qt-label-create "" parent: central))
           ;; Initialize frame with one editor in the splitter
           (fr (qt-frame-init! win splitter))
           ;; Create app state
           (app (new-app-state fr)))

      ;; Tab bar styling
      (qt-widget-set-minimum-height! tab-bar 26)
      (qt-widget-set-style-sheet! tab-bar
        "background: #1e1e1e; border-bottom: 1px solid #383838;")
      (qt-layout-set-margins! tab-layout 2 2 2 2)
      (qt-layout-set-spacing! tab-layout 2)
      ;; Store tab bar references for dynamic updates
      (set! *tab-bar-layout* tab-layout)
      (set! *tab-bar-widget* tab-bar)

      ;; Echo label: ensure visible with minimum height and distinct style
      ;; Must be tall enough to display text clearly (not clipped)
      (qt-widget-set-minimum-height! echo-label 28)
      (let ((font-css (string-append " font-family: " *default-font-family*
                                     "; font-size: " (number->string *default-font-size*) "pt;")))
        (qt-widget-set-style-sheet! echo-label
          (string-append "color: #d8d8d8; background: #1e1e1e;" font-css " padding: 4px 6px; border-top: 1px solid #484848;")))

      ;; Layout: tab-bar at top, splitter takes remaining space, echo-label at bottom
      (qt-layout-add-widget! layout tab-bar)
      (qt-layout-add-widget! layout splitter)
      (qt-layout-add-widget! layout echo-label)
      (qt-layout-set-stretch-factor! layout tab-bar 0)
      (qt-layout-set-stretch-factor! layout splitter 1)
      (qt-layout-set-stretch-factor! layout echo-label 0)
      (qt-widget-set-size-policy! tab-bar QT_SIZE_PREFERRED QT_SIZE_FIXED)
      (qt-widget-set-size-policy! echo-label QT_SIZE_PREFERRED QT_SIZE_FIXED)
      (qt-layout-set-margins! layout 0 0 0 0)
      (qt-layout-set-spacing! layout 0)

      ;; Initialize inline minibuffer (hidden until needed)
      (qt-minibuffer-init! echo-label qt-app layout)

      ;; Store Qt app pointer for clipboard access from commands
      (set! *qt-app-ptr* qt-app)

      ;; Set up keybindings and commands
      (setup-default-bindings!)
      (setup-command-docs!)
      (qt-register-all-commands!)
      (gemacs-log! "commands registered: "
                   (number->string (hash-length *all-commands*)) " total")

      ;; Set up post-buffer-attach hook for image/text display toggling
      ;; When showing an image, install key handler on the scroll area and
      ;; set focus there — the Scintilla editor is hidden by QStackedWidget
      ;; so it can't receive key events.
      (let ((image-key-installed (make-hash-table-eq)))
        (set! *post-buffer-attach-hook*
          (lambda (editor buf)
            (if (image-buffer? buf)
              (begin
                (qt-show-image-buffer! editor buf)
                (let ((win (hash-get *editor-window-map* editor)))
                  (when (and win (qt-edit-window-image-scroll win))
                    (let ((scroll (qt-edit-window-image-scroll win)))
                      (unless (hash-get image-key-installed scroll)
                        ((app-state-key-handler app) scroll)
                        (hash-put! image-key-installed scroll #t))
                      (qt-widget-set-focus! scroll)))))
              (begin
                (qt-hide-image-buffer! editor)
                (qt-widget-set-focus! editor))))))

      ;; Load recent files, bookmarks, keys, abbrevs, history from disk
      (recent-files-load!)
      (bookmarks-load! app)
      (custom-keys-load!)
      (abbrevs-load!)
      (savehist-load!)
      (save-place-load!)
      (gsh-history-load!)
      (load-init-file!)
      (init-file-load!)  ;; plaintext ~/.gemacs-init (chords, key-translate, settings)

      ;; Menu bar and toolbar
      (qt-setup-menubar! app win)

      ;; Initial text in scratch buffer — restore from disk if available
      (let* ((ed (qt-current-editor fr))
             (saved (scratch-restore!))
             (text (or saved
                       (string-append
                        ";; Gerbil Emacs — *scratch*\n"
                        ";;\n"
                        ";; Key Bindings:\n"
                        ";;   C-x C-f   Find file        C-x C-s   Save buffer\n"
                        ";;   C-x b     Switch buffer     C-x k     Kill buffer\n"
                        ";;   C-x C-r   Recent files      M-x       Extended command\n"
                        ";;   C-s       Search forward    M-%       Query replace\n"
                        ";;   C-x 2     Split window      C-x o     Other window\n"
                        ";;   C-h f     Describe command   C-h k     Describe key\n"
                        ";;\n"
                        ";; This buffer is for Gerbil Scheme evaluation.\n"
                        ";; Type expressions and use M-x eval-buffer to evaluate.\n\n"))))
        (qt-plain-text-edit-set-text! ed text)
        (qt-text-document-set-modified! (buffer-doc-pointer
                                          (qt-current-buffer fr)) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (scratch-update-text! text))

      ;; Key handler — define once, install on each editor
      ;; Uses consuming variant so QPlainTextEdit doesn't process keys itself.
      (let ((key-handler
             (lambda ()
               (let* ((code (qt-last-key-code))
                      (mods (qt-last-key-modifiers))
                      (raw-text (qt-last-key-text))
                      ;; Apply key translation map to printable characters
                      (text (if (= (string-length raw-text) 1)
                              (string (key-translate-char (string-ref raw-text 0)))
                              raw-text)))
                ;; Record keystroke in lossage ring
                (let ((ks (qt-key-event->string code mods text)))
                  (when ks (key-lossage-record! app ks)))
                ;; Modal mode intercepts: isearch and query-replace
                (cond
                 (*isearch-active*
                  (let ((handled (isearch-handle-key! app code mods text)))
                    ;; Update visual decorations and modeline
                    (qt-update-visual-decorations!
                      (qt-current-editor (app-state-frame app)))
                    (qt-modeline-update! app)
                    (qt-echo-draw! (app-state-echo app) echo-label)
                    ;; If not handled, fall through to normal processing
                    (when (not handled)
                      (let-values (((action data new-state)
                                    (qt-key-state-feed! (app-state-key-state app)
                                                        code mods text)))
                        (set! (app-state-key-state app) new-state)
                        (when (eq? action 'command)
                          (execute-command! app data))))))
                 (*qreplace-active*
                  (qreplace-handle-key! app code mods text)
                  (qt-modeline-update! app)
                  (qt-echo-draw! (app-state-echo app) echo-label))
                 (else
                ;; Normal key processing — with chord detection
                (letrec
                  ((do-normal-key!
                    (lambda (code mods text)
                     (let-values (((action data new-state)
                                   (qt-key-state-feed! (app-state-key-state app)
                                                       code mods text)))
                       (set! (app-state-key-state app) new-state)
                       ;; Cancel which-key timer on any non-prefix action
                       (when (and *which-key-timer* (not (eq? action 'prefix)))
                         (qt-timer-stop! *which-key-timer*)
                         (set! *which-key-pending-keymap* #f))
                       ;; Describe-key interception: if pending, show what the key does
                       ;; instead of executing it (except for prefix keys which continue building)
                       (if (and *qt-describe-key-pending* (not (eq? action 'prefix)))
                         (let ((ks (qt-key-event->string code mods text)))
                           (qt-describe-key-result! app ks action data))
                       (case action
                         ((command)
                          ;; Record for keyboard macro
                          (when (and (app-state-macro-recording app)
                                     (not (memq data '(start-kbd-macro end-kbd-macro
                                                       call-last-kbd-macro call-named-kbd-macro
                                                       name-last-kbd-macro list-kbd-macros
                                                       save-kbd-macros load-kbd-macros))))
                            (set! (app-state-macro-recording app)
                              (cons (cons 'command data)
                                    (app-state-macro-recording app))))
                          ;; Clear echo on command
                          (when (and (echo-state-message (app-state-echo app))
                                     (null? (key-state-prefix-keys new-state)))
                            (echo-clear! (app-state-echo app)))
                          (execute-command! app data))
                         ((self-insert)
                          ;; Check mode keymap first — special modes override self-insert
                          (let* ((buf (qt-current-buffer (app-state-frame app)))
                                 (mode-cmd (mode-keymap-lookup buf data)))
                            (if mode-cmd
                              ;; Mode keymap has a binding for this key — execute as command
                              (execute-command! app mode-cmd)
                              ;; No mode binding — normal self-insert
                              (begin
                          (when (app-state-macro-recording app)
                            (set! (app-state-macro-recording app)
                              (cons (cons 'self-insert data)
                                    (app-state-macro-recording app))))
                          ;; Handle self-insert directly here for Qt
                          (let* ((ed (qt-current-editor (app-state-frame app)))
                                 (ch (string-ref data 0))
                                 (close-ch (and *auto-pair-mode*
                                                (let ((cc (auto-pair-char (char->integer ch))))
                                                  (and cc (integer->char cc)))))
                                 (n (get-prefix-arg app))) ; Get prefix arg
                            (cond
                              ;; Suppress in dired and image buffers
                              ((dired-buffer? buf) (void))
                              ((image-buffer? buf) (void))
                              ;; In REPL buffers, only allow after the prompt
                              ((repl-buffer? buf)
                               (let* ((pos (qt-plain-text-edit-cursor-position ed))
                                      (rs (hash-get *repl-state* buf)))
                                 (when (and rs (>= pos (repl-state-prompt-pos rs)))
                                   (let loop ((i 0))
                                     (when (< i n)
                                       (qt-plain-text-edit-insert-text! ed (string ch))
                                       (loop (+ i 1)))))))
                              ;; Eshell: allow typing after the last prompt
                              ((eshell-buffer? buf)
                               (let loop ((i 0))
                                 (when (< i n)
                                   (qt-plain-text-edit-insert-text! ed (string ch))
                                   (loop (+ i 1)))))
                              ;; Terminal: if PTY busy, send to PTY (honors echo settings);
                              ;; otherwise insert locally (gsh line mode)
                              ((terminal-buffer? buf)
                               (let ((ts (hash-get *terminal-state* buf)))
                                 (if (and ts (terminal-pty-busy? ts))
                                   ;; PTY running — send keystroke to child process
                                   ;; The PTY handles echo (hides password input, etc.)
                                   (terminal-send-input! ts (string ch))
                                   ;; No PTY — local line editing
                                   (let loop ((i 0))
                                     (when (< i n)
                                       (qt-plain-text-edit-insert-text! ed (string ch))
                                       (loop (+ i 1)))))))
                              ;; Shell: if PTY busy, send to PTY; otherwise insert locally
                              ((shell-buffer? buf)
                               (let ((ss (hash-get *shell-state* buf)))
                                 (if (and ss (shell-pty-busy? ss))
                                   ;; PTY running — send keystroke to child process
                                   (shell-send-input! ss (string ch))
                                   ;; No PTY — local line editing
                                   (let loop ((i 0))
                                     (when (< i n)
                                       (qt-plain-text-edit-insert-text! ed (string ch))
                                       (loop (+ i 1)))))))
                              (else
                               (cond
                                 ;; Auto-pair skip-over: typing closing delimiter when next char matches
                                 ((and *auto-pair-mode* (= n 1)
                                       (auto-pair-closing? (char->integer ch)))
                                  (let* ((pos (qt-plain-text-edit-cursor-position ed))
                                         (text (qt-plain-text-edit-text ed))
                                         (next-ch (and (< pos (string-length text))
                                                       (string-ref text pos))))
                                    (if (and next-ch (char=? next-ch ch))
                                      ;; Skip over existing closing char
                                      (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))
                                      ;; No match — insert normally
                                      (qt-plain-text-edit-insert-text! ed (string ch)))))
                                 ;; Auto-pair: insert both chars and place cursor between
                                 ((and close-ch (= n 1))
                                  (let ((pos (qt-plain-text-edit-cursor-position ed)))
                                    (qt-plain-text-edit-insert-text! ed (string ch close-ch))
                                    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))
                                 ;; Insert character n times
                                 (else
                                  (let ((str (make-string n ch)))
                                    (qt-plain-text-edit-insert-text! ed str)))))))
                          ;; Auto-fill: break line if past fill-column
                          (auto-fill-check! (qt-current-editor (app-state-frame app)))
                          (set! (app-state-prefix-arg app) #f)
                           (set! (app-state-prefix-digit-mode? app) #f))))) ; Reset prefix arg
                         ((prefix)
                          (let ((prefix-str
                                 (let loop ((keys (key-state-prefix-keys new-state))
                                            (acc ""))
                                   (if (null? keys) acc
                                     (loop (cdr keys)
                                           (if (string=? acc "")
                                             (car keys)
                                             (string-append acc " " (car keys))))))))
                            (echo-message! (app-state-echo app)
                                           (string-append prefix-str "-"))
                            ;; Start which-key timer to show available bindings
                            (set! *which-key-pending-keymap*
                                  (key-state-keymap new-state))
                            (set! *which-key-pending-prefix* prefix-str)
                            (qt-timer-start! *which-key-timer* 500)))
                         ((undefined)
                          (echo-error! (app-state-echo app)
                                       (string-append data " is undefined")))
                         ((ignore) (void))))  ;; bare modifier keys — do nothing (extra paren closes describe-key if)
                       ;; Update visual decorations (current-line + brace match)
                       (qt-update-visual-decorations!
                         (qt-current-editor (app-state-frame app)))
                       ;; Update mark/region visual selection
                       (qt-update-mark-selection! app)
                       ;; Update modeline, tab bar, title, and echo after each key
                       (qt-modeline-update! app)
                       (qt-tabbar-update! app)
                       (qt-update-frame-title! app)
                       (qt-echo-draw! (app-state-echo app) echo-label)))))
                  ;; Chord detection logic
                  (cond
                    ;; Case 1: A chord is pending and a new key arrived
                    (*chord-pending-char*
                     (qt-timer-stop! *chord-timer*)
                     (let* ((ch1 *chord-pending-char*)
                            (saved-code *chord-pending-code*)
                            (saved-mods *chord-pending-mods*)
                            (saved-text *chord-pending-text*)
                            ;; Is the new key also a plain printable character?
                            (ch2 (and (= (string-length text) 1)
                                      (> (char->integer (string-ref text 0)) 31)
                                      (zero? (bitwise-and mods QT_MOD_CTRL))
                                      (zero? (bitwise-and mods QT_MOD_ALT))
                                      (string-ref text 0)))
                            (chord-cmd (and ch2 (chord-lookup ch1 ch2))))
                       (set! *chord-pending-char* #f)
                       (if chord-cmd
                         ;; Chord matched — execute the chord command
                         (begin
                           (execute-command! app chord-cmd)
                           (qt-update-visual-decorations!
                             (qt-current-editor (app-state-frame app)))
                           (qt-update-mark-selection! app)
                           (qt-modeline-update! app)
                           (qt-tabbar-update! app)
                           (qt-update-frame-title! app)
                           (qt-echo-draw! (app-state-echo app) echo-label))
                         ;; No chord — replay saved key then process current key
                         (begin
                           (do-normal-key! saved-code saved-mods saved-text)
                           (do-normal-key! code mods text)))))

                    ;; Case 2: Printable key that could start a chord — save and wait
                    ((and (= (string-length text) 1)
                          (> (char->integer (string-ref text 0)) 31)
                          (zero? (bitwise-and mods QT_MOD_CTRL))
                          (zero? (bitwise-and mods QT_MOD_ALT))
                          (null? (key-state-prefix-keys (app-state-key-state app)))
                          (chord-start-char? (string-ref text 0)))
                     (set! *chord-pending-char* (string-ref text 0))
                     (set! *chord-pending-code* code)
                     (set! *chord-pending-mods* mods)
                     (set! *chord-pending-text* text)
                     (qt-timer-start! *chord-timer* *chord-timeout*))

                    ;; Case 3: Normal key — no chord involvement
                    (else
                     (do-normal-key! code mods text))))))))))

        ;; Install on the initial editor (consuming — editor doesn't see keys)
        (qt-on-key-press-consuming! (qt-current-editor fr) key-handler)

        ;; Store installer so split-window can install on new editors
        (set! (app-state-key-handler app)
              (lambda (editor)
                (qt-on-key-press-consuming! editor key-handler))))

      ;; ================================================================
      ;; Periodic tasks — registered with schedule-periodic!, driven
      ;; by one master Qt timer that also drains the async UI queue.
      ;; ================================================================

      ;; REPL/Shell/Terminal/Chat output polling (50ms)
      (schedule-periodic! 'repl-poll 50
        (lambda ()
          (for-each
            (lambda (buf)
              (when (repl-buffer? buf)
                (let ((rs (hash-get *repl-state* buf)))
                  (when rs
                    (let ((output (repl-read-available rs)))
                      (when output
                        ;; Find a window showing this buffer
                        (let loop ((wins (qt-frame-windows fr)))
                          (when (pair? wins)
                            (if (eq? (qt-edit-window-buffer (car wins)) buf)
                              (let* ((ed (qt-edit-window-editor (car wins)))
                                     ;; Strip trailing newline — append! adds its own
                                     (trimmed (string-trim-eol output)))
                                ;; Insert output + new prompt
                                (qt-plain-text-edit-append! ed trimmed)
                                (qt-plain-text-edit-append! ed repl-prompt)
                                (set! (repl-state-prompt-pos rs)
                                  (string-length (qt-plain-text-edit-text ed)))
                                (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                (qt-plain-text-edit-ensure-cursor-visible! ed))
                              (loop (cdr wins)))))))))))
            (buffer-list))
          ;; Poll Shell/Terminal PTY output
          (for-each
            (lambda (buf)
              (when (shell-buffer? buf)
                (let ((ss (hash-get *shell-state* buf)))
                  (when (and ss (shell-pty-busy? ss))
                    (let drain ()
                      (let ((msg (shell-poll-output ss)))
                        (when msg
                          (qt-poll-shell-pty-msg! fr buf ss msg)
                          (when (eq? (car msg) 'data)
                            (drain))))))))
              (when (terminal-buffer? buf)
                (let ((ts (hash-get *terminal-state* buf)))
                  (when (and ts (terminal-pty-busy? ts))
                    ;; Batch all pending data chunks, then render once
                    (let drain ((chunks []) (done-msg #f))
                      (let ((msg (terminal-poll-output ts)))
                        (cond
                          ((not msg)
                           ;; No more messages — render accumulated data
                           (when (pair? chunks)
                             (qt-poll-terminal-pty-batch! fr buf ts
                               (apply string-append (reverse chunks))))
                           (when done-msg
                             (qt-poll-terminal-pty-msg! fr buf ts done-msg)))
                          ((eq? (car msg) 'data)
                           (drain (cons (cdr msg) chunks) done-msg))
                          (else
                           ;; 'done message — render data first, then handle done
                           (when (pair? chunks)
                             (qt-poll-terminal-pty-batch! fr buf ts
                               (apply string-append (reverse chunks))))
                           (qt-poll-terminal-pty-msg! fr buf ts msg)))))))))
            (buffer-list))
          ;; Poll chat buffers (Claude CLI)
          (for-each
            (lambda (buf)
              (when (chat-buffer? buf)
                (let ((cs (hash-get *chat-state* buf)))
                  (when (and cs (chat-busy? cs))
                    (let ((result (chat-read-available cs)))
                      (when result
                        (let loop ((wins (qt-frame-windows fr)))
                          (when (pair? wins)
                            (if (eq? (qt-edit-window-buffer (car wins)) buf)
                              (let ((ed (qt-edit-window-editor (car wins))))
                                (cond
                                  ((string? result)
                                   (sci-send/string ed SCI_APPENDTEXT result (string-length result))
                                   (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                   (qt-plain-text-edit-ensure-cursor-visible! ed))
                                  ((and (pair? result) (string? (car result)))
                                   (let ((chunk (car result)))
                                     (sci-send/string ed SCI_APPENDTEXT chunk (string-length chunk)))
                                   (qt-plain-text-edit-append! ed "\nYou: ")
                                   (set! (chat-state-prompt-pos cs)
                                     (string-length (qt-plain-text-edit-text ed)))
                                   (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                   (qt-plain-text-edit-ensure-cursor-visible! ed))
                                  ((eq? result 'done)
                                   (qt-plain-text-edit-append! ed "\nYou: ")
                                   (set! (chat-state-prompt-pos cs)
                                     (string-length (qt-plain-text-edit-text ed)))
                                   (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                   (qt-plain-text-edit-ensure-cursor-visible! ed))))
                              (loop (cdr wins)))))))))))
            (buffer-list))))

      ;; Auto-save (30 seconds)
      ;; Collect text snapshots on UI thread (fast), write files in background
      (schedule-periodic! 'auto-save 30000
        (lambda ()
          ;; Phase 1: Collect snapshots on UI thread (must access Qt widgets here)
          (let ((save-jobs []))
            (for-each
              (lambda (buf)
                (let ((path (buffer-file-path buf)))
                  (when (and path
                             (buffer-doc-pointer buf)
                             (qt-text-document-modified? (buffer-doc-pointer buf)))
                    (let loop ((wins (qt-frame-windows fr)))
                      (when (pair? wins)
                        (if (eq? (qt-edit-window-buffer (car wins)) buf)
                          (let* ((ed (qt-edit-window-editor (car wins)))
                                 (text (qt-plain-text-edit-text ed))
                                 (auto-path (qt-make-auto-save-path path)))
                            (set! save-jobs (cons [auto-path . text] save-jobs)))
                          (loop (cdr wins))))))))
              (buffer-list))
            ;; Phase 2: Write all auto-save files in background thread
            (when (pair? save-jobs)
              (spawn/name 'auto-save
                (lambda ()
                  (for-each
                    (lambda (job)
                      (with-catch
                        (lambda (e) (gemacs-log! "Auto-save error: " (##object->string e)))
                        (lambda ()
                          (call-with-output-file (car job)
                            (lambda (port) (display (cdr job) port))))))
                    save-jobs)))))
          ;; Cache scratch buffer text for persistence (fast, stays on UI thread)
          (let ((scratch (buffer-by-name "*scratch*")))
            (when scratch
              (let loop ((wins (qt-frame-windows fr)))
                (when (pair? wins)
                  (if (eq? (qt-edit-window-buffer (car wins)) scratch)
                    (scratch-update-text!
                      (qt-plain-text-edit-text
                        (qt-edit-window-editor (car wins))))
                    (loop (cdr wins)))))))
          ;; Record undo history snapshots for modified buffers (fast, stays on UI thread)
          (for-each
            (lambda (win)
              (let* ((buf (qt-edit-window-buffer win))
                     (doc (buffer-doc-pointer buf)))
                (when (and doc (qt-text-document-modified? doc))
                  (let ((text (qt-plain-text-edit-text (qt-edit-window-editor win))))
                    (undo-history-record! (buffer-name buf) text)))))
            (qt-frame-windows fr))))

      ;; File modification watcher (5 seconds)
      ;; Mtime check on UI thread (fast stat), file read in background
      (schedule-periodic! 'file-watch 5000
        (lambda ()
          (when *auto-revert-mode*
            (for-each
              (lambda (buf)
                (let ((path (buffer-file-path buf))
                      (tail? (hash-get *auto-revert-tail-buffers* (buffer-name buf))))
                  (when (and path (file-mtime-changed? path))
                    (let ((doc (buffer-doc-pointer buf)))
                      (if (and (not tail?)
                               doc (qt-text-document-modified? doc))
                        (echo-message! (app-state-echo app)
                          (string-append (buffer-name buf)
                            " changed on disk (buffer modified, not reverting)"))
                        ;; Read file in background, update widget on UI thread
                        (async-read-file! path
                          (lambda (text)
                            (when text
                              (let loop ((wins (qt-frame-windows fr)))
                                (when (pair? wins)
                                  (if (eq? (qt-edit-window-buffer (car wins)) buf)
                                    (let* ((ed (qt-edit-window-editor (car wins)))
                                           (pos (qt-plain-text-edit-cursor-position ed)))
                                      (qt-plain-text-edit-set-text! ed text)
                                      (qt-text-document-set-modified! doc #f)
                                      (if tail?
                                        (begin
                                          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                          (qt-plain-text-edit-ensure-cursor-visible! ed))
                                        (begin
                                          (qt-plain-text-edit-set-cursor-position! ed
                                            (min pos (string-length text)))
                                          (qt-plain-text-edit-ensure-cursor-visible! ed)))
                                      (file-mtime-record! path))
                                    (loop (cdr wins)))))))))))))
              (buffer-list)))))

      ;; Eldoc / LSP cursor-idle (300ms)
      (schedule-periodic! 'eldoc 300
        (lambda ()
          (if (lsp-running?)
            (begin
              (lsp-eldoc-display! app)
              (lsp-diagnostic-at-cursor! app)
              (lsp-document-highlight! app)
              (lsp-inlay-hint-at-cursor! app))
            (eldoc-display! app))))

      ;; LSP auto-completion (500ms)
      (schedule-periodic! 'lsp-auto-complete 500
        (lambda () (lsp-auto-complete! app)))

      ;; LSP UI actions now drained by master timer via unified ui-queue

      ;; LSP didChange — send buffer content 1s after last edit
      (schedule-periodic! 'lsp-change 1000
        (lambda ()
          (when (lsp-running?)
            (let* ((fr (app-state-frame app))
                   (buf (qt-current-buffer fr))
                   (ed (qt-current-editor fr)))
              (when (and buf (buffer-file-path buf))
                (let* ((path (buffer-file-path buf))
                       (uri (file-path->uri path))
                       (text (qt-plain-text-edit-text ed)))
                  (when (lsp-content-changed? uri text)
                    (lsp-hook-did-change! app buf)
                    (lsp-record-sent-content! uri text))))))))

      ;; Install LSP UI handlers
      (lsp-install-handlers! app)

      ;; Which-key timer (one-shot, shows available prefix bindings)
      (set! *which-key-timer* (qt-timer-create))
      (qt-timer-set-single-shot! *which-key-timer* #t)
      (qt-on-timeout! *which-key-timer*
        (lambda ()
          (when (and *which-key-pending-keymap*
                     (not (null? (key-state-prefix-keys
                                   (app-state-key-state app)))))
            (echo-message! (app-state-echo app)
              (which-key-format-bindings
                *which-key-pending-keymap*
                *which-key-pending-prefix*)))))

      ;; Key-chord timer (one-shot, replays pending key on timeout)
      (set! *chord-timer* (qt-timer-create))
      (qt-timer-set-single-shot! *chord-timer* #t)
      (qt-on-timeout! *chord-timer*
        (lambda ()
          (when *chord-pending-char*
            (let ((saved-code *chord-pending-code*)
                  (saved-mods *chord-pending-mods*)
                  (saved-text *chord-pending-text*))
              (set! *chord-pending-char* #f)
              ;; Replay the pending key through normal key processing
              (let-values (((action data new-state)
                            (qt-key-state-feed! (app-state-key-state app)
                                                saved-code saved-mods saved-text)))
                (set! (app-state-key-state app) new-state)
                (case action
                  ((self-insert)
                   (let* ((buf (qt-current-buffer (app-state-frame app)))
                          (mode-cmd (mode-keymap-lookup buf data)))
                     (if mode-cmd
                       (execute-command! app mode-cmd)
                       (let* ((ed (qt-current-editor (app-state-frame app)))
                              (ch (string-ref data 0)))
                         (qt-plain-text-edit-insert-text! ed (string ch))))))
                  ((command)
                   (execute-command! app data))
                  (else (void)))
                ;; Update UI
                (qt-update-visual-decorations!
                  (qt-current-editor (app-state-frame app)))
                (qt-modeline-update! app)
                (qt-tabbar-update! app)
                (qt-update-frame-title! app)
                (qt-echo-draw! (app-state-echo app) echo-label))))))

      ;; Restore session if no files given on command line
      ;; Files are read in parallel (async-read-file! in qt-open-file!)
      (when (null? args)
        (let-values (((current-file entries) (session-restore-files)))
          (for-each
            (lambda (entry)
              (let ((path (car entry))
                    (pos (cdr entry)))
                (when (file-exists? path)
                  (qt-open-file! app path
                    ;; Restore cursor position after async file load
                    (lambda (app buf)
                      (let ((ed (qt-current-editor (app-state-frame app))))
                        (qt-plain-text-edit-set-cursor-position! ed
                          (min pos (string-length (qt-plain-text-edit-text ed))))
                        (qt-plain-text-edit-ensure-cursor-visible! ed)))))))
            entries)
          ;; Switch to the buffer that was current when session was saved
          (when current-file
            (let loop ((bufs (buffer-list)))
              (when (pair? bufs)
                (if (equal? (buffer-file-path (car bufs)) current-file)
                  (let ((ed (qt-current-editor fr)))
                    (qt-buffer-attach! ed (car bufs))
                    (set! (qt-edit-window-buffer (qt-current-window fr)) (car bufs)))
                  (loop (cdr bufs))))))))

      ;; Open files from command line — skip flags (handled in main.ss)
      (let ((files (filter (lambda (a) (not (string-prefix? "-" a))) args)))
        (for-each (lambda (file) (qt-open-file! app file)) files))

      ;; Show window
      (qt-main-window-set-central-widget! win central)
      (qt-main-window-set-title! win "gemacs")
      (qt-widget-resize! win 800 600)
      (qt-widget-show! win)

      ;; Initial modeline, tab bar, and title update (before any key press)
      (qt-modeline-update! app)
      (qt-tabbar-update! app)
      (qt-update-frame-title! app)

      ;; Start IPC server for gemacs-client
      (start-ipc-server!)
      (schedule-periodic! 'ipc 200
        (lambda ()
          (for-each (lambda (f) (qt-open-file! app f))
                    (ipc-poll-files!))))

      ;; Master timer — drives all periodic tasks and drains the async UI queue.
      ;; Single 50ms timer replaces 7+ individual Qt timers.
      (let ((master-timer (qt-timer-create)))
        (qt-on-timeout! master-timer master-timer-tick!)
        (qt-timer-start! master-timer 50))

      ;; Enter Qt event loop (blocks until quit)
      (qt-app-exec! qt-app)

      ;; Cleanup LSP server on exit
      (lsp-stop!)
      ;; Cleanup IPC server on exit
      (stop-ipc-server!))))

;;;============================================================================
;;; File opening helper
;;;============================================================================

(def (qt-open-file! app filename (on-loaded #f))
  "Open a file or directory in a new buffer, or view an image.
   Optional on-loaded callback is called with (app buf) after text is loaded."
  ;; Track in recent files
  (recent-files-add! filename)
  (cond
    ;; Directory -> dired
    ((and (file-exists? filename)
          (eq? 'directory (file-info-type (file-info filename))))
     (dired-open-directory! app filename))
    ;; Image file -> inline image buffer
    ((image-file? filename)
     (let* ((pixmap (qt-pixmap-load filename)))
       (if (qt-pixmap-null? pixmap)
         (begin
           (qt-pixmap-destroy! pixmap)
           (echo-error! (app-state-echo app)
             (string-append "Failed to load image: " filename)))
         (let* ((name (path-strip-directory filename))
                (fr (app-state-frame app))
                (ed (qt-current-editor fr))
                (buf (qt-buffer-create! name ed filename))
                (orig-w (qt-pixmap-width pixmap))
                (orig-h (qt-pixmap-height pixmap)))
           (set! (buffer-lexer-lang buf) 'image)
           (hash-put! *image-buffer-state* buf
             (list pixmap (box 1.0) orig-w orig-h))
           (buffer-touch! buf)
           (qt-buffer-attach! ed buf)
           (set! (qt-edit-window-buffer (qt-current-window fr)) buf)))))
    ;; Regular file -> text buffer
    (else
     (let* ((name (path-strip-directory filename))
            (fr (app-state-frame app))
            (ed (qt-current-editor fr))
            (buf (qt-buffer-create! name ed filename)))
       (buffer-touch! buf)
       (qt-buffer-attach! ed buf)
       (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
       (if (file-exists? filename)
         ;; Read file content in background thread
         (begin
           (qt-plain-text-edit-set-text! ed "Loading...")
           (async-read-file! filename
             (lambda (text)
               (when text
                 (let ((ed (qt-current-editor (app-state-frame app))))
                   (qt-plain-text-edit-set-text! ed text)
                   (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                   (qt-plain-text-edit-set-cursor-position! ed 0)))
               (file-mtime-record! filename)
               (qt-setup-highlighting! app buf)
               (let ((mode (detect-major-mode filename)))
                 (when mode
                   (buffer-local-set! buf 'major-mode mode)
                   (let ((mode-cmd (find-command mode)))
                     (when mode-cmd (mode-cmd app)))))
               (lsp-maybe-auto-start! app buf)
               (lsp-hook-did-open! app buf)
               (when on-loaded (on-loaded app buf)))))
         ;; New file — no content to read
         (begin
           (qt-setup-highlighting! app buf)
           (let ((mode (detect-major-mode filename)))
             (when mode
               (buffer-local-set! buf 'major-mode mode)
               (let ((mode-cmd (find-command mode)))
                 (when mode-cmd (mode-cmd app)))))
           (lsp-maybe-auto-start! app buf)
           (lsp-hook-did-open! app buf)))))))
