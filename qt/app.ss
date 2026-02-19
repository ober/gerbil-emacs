;;; -*- Gerbil -*-
;;; Qt application and event loop for gemacs

(export qt-main qt-open-file!)

(import :std/sugar
        :std/misc/string
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        (only-in :gemacs/persist init-file-load!
                 detect-major-mode buffer-local-set!)
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/terminal
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
        :gemacs/ipc)

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
  "Format keymap bindings for which-key display."
  (let* ((entries (keymap-entries km))
         (strs (let loop ((es entries) (acc []))
                 (if (null? es) (reverse acc)
                   (let* ((e (car es))
                          (key (car e))
                          (val (cdr e)))
                     (loop (cdr es)
                           (cons (string-append key ":"
                                   (cond
                                     ((symbol? val) (symbol->string val))
                                     ((hash-table? val) "+prefix")
                                     (else "?")))
                                 acc)))))))
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

(def (qt-main . args)
  (with-qt-app qt-app
    ;; Apply theme stylesheet (uses *current-theme* from commands.ss)
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
      (qt-register-all-commands!)

      ;; Set up post-buffer-attach hook for image/text display toggling
      (set! *post-buffer-attach-hook*
        (lambda (editor buf)
          (if (image-buffer? buf)
            (qt-show-image-buffer! editor buf)
            (qt-hide-image-buffer! editor))))

      ;; Load recent files, bookmarks, keys, abbrevs, history from disk
      (recent-files-load!)
      (bookmarks-load! app)
      (custom-keys-load!)
      (abbrevs-load!)
      (savehist-load!)
      (save-place-load!)
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
                       (case action
                         ((command)
                          ;; Record for keyboard macro
                          (when (and (app-state-macro-recording app)
                                     (not (memq data '(start-kbd-macro end-kbd-macro
                                                       call-last-kbd-macro))))
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
                              ;; Terminal: send character directly to PTY
                              ((terminal-buffer? buf)
                               (let ((ts (hash-get *terminal-state* buf)))
                                 (when ts
                                   (terminal-send-raw! ts (string ch)))))
                              ;; Shell: insert char locally (sent as complete line on Enter)
                              ((shell-buffer? buf)
                               (let loop ((i 0))
                                 (when (< i n)
                                   (qt-plain-text-edit-insert-text! ed (string ch))
                                   (loop (+ i 1)))))
                              (else
                               (if (and close-ch (= n 1)) ; Only auto-pair if n=1
                                 ;; Auto-pair: insert both chars and place cursor between
                                 (let ((pos (qt-plain-text-edit-cursor-position ed)))
                                   (qt-plain-text-edit-insert-text! ed (string ch close-ch))
                                   (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
                                 ;; Insert character n times
                                 (let ((str (make-string n ch)))
                                   (qt-plain-text-edit-insert-text! ed str))))))
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
                         ((ignore) (void)))  ;; bare modifier keys — do nothing
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

      ;; REPL output polling timer
      (let ((repl-timer (qt-timer-create)))
        (qt-on-timeout! repl-timer
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
                                  ;; Set prompt-pos from text length (same units
                                  ;; as substring extraction in cmd-repl-send)
                                  (set! (repl-state-prompt-pos rs)
                                    (string-length (qt-plain-text-edit-text ed)))
                                  ;; Move cursor to end so user types at the prompt
                                  (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                  (qt-plain-text-edit-ensure-cursor-visible! ed))
                                (loop (cdr wins)))))))))))
              (buffer-list))
            ;; Also poll shell buffers
            (for-each
              (lambda (buf)
                (when (shell-buffer? buf)
                  (let ((ss (hash-get *shell-state* buf)))
                    (when ss
                      (let ((raw-output (shell-read-available ss)))
                        (when raw-output
                          (let ((output (shell-filter-echo raw-output
                                          (shell-state-last-sent ss))))
                            (set! (shell-state-last-sent ss) #f)
                            (when (and output (> (string-length output) 0))
                              (let loop ((wins (qt-frame-windows fr)))
                                (when (pair? wins)
                                  (if (eq? (qt-edit-window-buffer (car wins)) buf)
                                    (let ((ed (qt-edit-window-editor (car wins))))
                                      (qt-plain-text-edit-append! ed output)
                                      (set! (shell-state-prompt-pos ss)
                                        (string-length (qt-plain-text-edit-text ed)))
                                      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                      (qt-plain-text-edit-ensure-cursor-visible! ed))
                                    (loop (cdr wins)))))))))))))
              (buffer-list))
            ;; Also poll terminal buffers
            (for-each
              (lambda (buf)
                (when (terminal-buffer? buf)
                  (let ((ts (hash-get *terminal-state* buf)))
                    (when ts
                      (let ((segs (terminal-read-available ts)))
                        (when segs
                          ;; Extract plain text from segments (strip ANSI colors)
                          (let ((plain (let ((out (open-output-string)))
                                         (for-each
                                           (lambda (seg) (display (text-segment-text seg) out))
                                           segs)
                                         (get-output-string out))))
                            (when (> (string-length plain) 0)
                              (let loop ((wins (qt-frame-windows fr)))
                                (when (pair? wins)
                                  (if (eq? (qt-edit-window-buffer (car wins)) buf)
                                    (let ((ed (qt-edit-window-editor (car wins))))
                                      (qt-plain-text-edit-append! ed plain)
                                      (set! (terminal-state-prompt-pos ts)
                                        (string-length (qt-plain-text-edit-text ed)))
                                      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                      (qt-plain-text-edit-ensure-cursor-visible! ed))
                                    (loop (cdr wins)))))))))))))
              (buffer-list))))
        (qt-timer-start! repl-timer 50))

      ;; Auto-save timer (every 30 seconds)
      (let ((auto-save-timer (qt-timer-create)))
        (qt-on-timeout! auto-save-timer
          (lambda ()
            (for-each
              (lambda (buf)
                (let ((path (buffer-file-path buf)))
                  ;; Only auto-save file-visiting buffers that are modified
                  (when (and path
                             (buffer-doc-pointer buf)
                             (qt-text-document-modified? (buffer-doc-pointer buf)))
                    ;; Find a window showing this buffer to get the text
                    (let loop ((wins (qt-frame-windows fr)))
                      (when (pair? wins)
                        (if (eq? (qt-edit-window-buffer (car wins)) buf)
                          (let* ((ed (qt-edit-window-editor (car wins)))
                                 (text (qt-plain-text-edit-text ed))
                                 (auto-path (qt-make-auto-save-path path)))
                            (with-catch
                              (lambda (e) #f)  ; Ignore auto-save errors
                              (lambda ()
                                (call-with-output-file auto-path
                                  (lambda (port) (display text port))))))
                          (loop (cdr wins))))))))
              (buffer-list))
            ;; Cache scratch buffer text for persistence
            (let ((scratch (buffer-by-name "*scratch*")))
              (when scratch
                (let loop ((wins (qt-frame-windows fr)))
                  (when (pair? wins)
                    (if (eq? (qt-edit-window-buffer (car wins)) scratch)
                      (scratch-update-text!
                        (qt-plain-text-edit-text
                          (qt-edit-window-editor (car wins))))
                      (loop (cdr wins)))))))
            ;; Record undo history snapshots for modified buffers
            (for-each
              (lambda (win)
                (let* ((buf (qt-edit-window-buffer win))
                       (doc (buffer-doc-pointer buf)))
                  (when (and doc (qt-text-document-modified? doc))
                    (let ((text (qt-plain-text-edit-text (qt-edit-window-editor win))))
                      (undo-history-record! (buffer-name buf) text)))))
              (qt-frame-windows fr))))
        (qt-timer-start! auto-save-timer 30000))

      ;; File modification watcher timer (every 5 seconds)
      (let ((file-watch-timer (qt-timer-create)))
        (qt-on-timeout! file-watch-timer
          (lambda ()
            (when *auto-revert-mode*
              (for-each
                (lambda (buf)
                  (let ((path (buffer-file-path buf))
                        (tail? (hash-get *auto-revert-tail-buffers* (buffer-name buf))))
                    (when (and path (file-mtime-changed? path))
                      ;; File changed externally
                      (let ((doc (buffer-doc-pointer buf)))
                        (if (and (not tail?)
                                 doc (qt-text-document-modified? doc))
                          ;; Buffer has unsaved changes — warn but don't revert
                          (echo-message! (app-state-echo app)
                            (string-append (buffer-name buf)
                              " changed on disk (buffer modified, not reverting)"))
                          ;; Auto-revert (always for tail-mode, otherwise only unmodified)
                          (let loop ((wins (qt-frame-windows fr)))
                            (when (pair? wins)
                              (if (eq? (qt-edit-window-buffer (car wins)) buf)
                                (let* ((ed (qt-edit-window-editor (car wins)))
                                       (pos (qt-plain-text-edit-cursor-position ed))
                                       (text (read-file-as-string path)))
                                  (when text
                                    (qt-plain-text-edit-set-text! ed text)
                                    (qt-text-document-set-modified! doc #f)
                                    (if tail?
                                      ;; Tail mode: scroll to end
                                      (begin
                                        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                        (qt-plain-text-edit-ensure-cursor-visible! ed))
                                      ;; Normal: preserve cursor position
                                      (begin
                                        (qt-plain-text-edit-set-cursor-position! ed
                                          (min pos (string-length text)))
                                        (qt-plain-text-edit-ensure-cursor-visible! ed)))
                                    (file-mtime-record! path)))
                                (loop (cdr wins))))))))))
                (buffer-list)))))
        (qt-timer-start! file-watch-timer 5000))

      ;; Eldoc timer — show function signatures on cursor idle
      ;; When LSP is running, also query signatureHelp
      (let ((eldoc-timer (qt-timer-create)))
        (qt-on-timeout! eldoc-timer
          (lambda ()
            (if (lsp-running?)
              (lsp-eldoc-display! app)
              (eldoc-display! app))))
        (qt-timer-start! eldoc-timer 300))

      ;; LSP UI action queue polling timer
      (let ((lsp-timer (qt-timer-create)))
        (qt-on-timeout! lsp-timer lsp-poll-ui-actions!)
        (qt-timer-start! lsp-timer 50))

      ;; LSP didChange timer — send buffer content to server 1s after last edit,
      ;; so diagnostics update while typing (not just on save).
      (let ((lsp-change-timer (qt-timer-create)))
        (qt-on-timeout! lsp-change-timer
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
        (qt-timer-start! lsp-change-timer 1000))

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
      (when (null? args)
        (let-values (((current-file entries) (session-restore-files)))
          (for-each
            (lambda (entry)
              (let ((path (car entry))
                    (pos (cdr entry)))
                (when (file-exists? path)
                  (qt-open-file! app path)
                  ;; Restore cursor position
                  (let ((ed (qt-current-editor fr)))
                    (qt-plain-text-edit-set-cursor-position! ed
                      (min pos (string-length (qt-plain-text-edit-text ed))))
                    (qt-plain-text-edit-ensure-cursor-visible! ed)))))
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
      (let ((ipc-timer (qt-timer-create)))
        (qt-on-timeout! ipc-timer
          (lambda ()
            (for-each (lambda (f) (qt-open-file! app f))
                      (ipc-poll-files!))))
        (qt-timer-start! ipc-timer 200))

      ;; Enter Qt event loop (blocks until quit)
      (qt-app-exec! qt-app)

      ;; Cleanup LSP server on exit
      (lsp-stop!)
      ;; Cleanup IPC server on exit
      (stop-ipc-server!))))

;;;============================================================================
;;; File opening helper
;;;============================================================================

(def (qt-open-file! app filename)
  "Open a file or directory in a new buffer, or view an image."
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
       (when (file-exists? filename)
         (let ((text (read-file-as-string filename)))
           (when text
             (qt-plain-text-edit-set-text! ed text)
             (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
             (qt-plain-text-edit-set-cursor-position! ed 0)))
         (file-mtime-record! filename))
       (qt-setup-highlighting! app buf)
       ;; Activate major mode from auto-mode-alist
       (let ((mode (detect-major-mode filename)))
         (when mode
           (buffer-local-set! buf 'major-mode mode)
           (let ((mode-cmd (find-command mode)))
             (when mode-cmd (mode-cmd app)))))
       ;; LSP: auto-start and notify didOpen
       (lsp-maybe-auto-start! app buf)
       (lsp-hook-did-open! app buf)))))
