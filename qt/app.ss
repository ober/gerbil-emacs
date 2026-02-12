;;; -*- Gerbil -*-
;;; Qt application and event loop for gerbil-emacs

(export qt-main qt-open-file!)

(import :std/sugar
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/qt/keymap
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/modeline
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/highlight
        :gerbil-emacs/qt/image
        :gerbil-emacs/qt/commands
        :gerbil-emacs/qt/menubar)

;;;============================================================================
;;; Qt Application
;;;============================================================================

;; Auto-save path: #filename# (Emacs convention)
(def (qt-make-auto-save-path path)
  (let* ((dir (path-directory path))
         (name (path-strip-directory path)))
    (path-expand (string-append "#" name "#") dir)))

(def (qt-main . args)
  (with-qt-app qt-app
    ;; Dark theme via stylesheet
    (qt-app-set-style-sheet! qt-app "
      QPlainTextEdit { background-color: #181818; color: #d8d8d8;
                       font-family: monospace; font-size: 10pt;
                       selection-background-color: #404060; }
      QLabel { color: #d8d8d8; background: #181818; font-family: monospace;
               font-size: 10pt; }
      QMainWindow { background: #181818; }
      QStatusBar { color: #d8d8d8; background: #282828; font-family: monospace;
                   font-size: 10pt; }
      QLineEdit { background: #181818; color: #d8d8d8; border: none;
                  font-family: monospace; font-size: 10pt; }
      QSplitter::handle { background: #383838; }")

    (let* ((win (qt-main-window-create))
           ;; Central widget with vertical layout
           (central (qt-widget-create parent: win))
           (layout (qt-vbox-layout-create central))
           ;; Main content area: splitter for editors
           (splitter (qt-splitter-create QT_VERTICAL parent: central))
           ;; Echo label at bottom
           (echo-label (qt-label-create "" parent: central))
           ;; Initialize frame with one editor in the splitter
           (fr (qt-frame-init! win splitter))
           ;; Create app state
           (app (new-app-state fr)))

      ;; Echo label: ensure visible with minimum height and distinct style
      (qt-widget-set-minimum-height! echo-label 20)
      (qt-widget-set-style-sheet! echo-label
        "color: #d8d8d8; background: #282828; font-family: monospace; font-size: 10pt; padding: 2px 4px;")

      ;; Layout: splitter takes remaining space, echo-label fixed at bottom
      (qt-layout-add-widget! layout splitter)
      (qt-layout-add-widget! layout echo-label)
      (qt-layout-set-margins! layout 0 0 0 0)
      (qt-layout-set-spacing! layout 0)

      ;; Set up keybindings and commands
      (setup-default-bindings!)
      (qt-register-all-commands!)

      ;; Menu bar and toolbar
      (qt-setup-menubar! app win)

      ;; Initial text in scratch buffer
      (let ((ed (qt-current-editor fr)))
        (qt-plain-text-edit-set-text! ed ";; *scratch*\n")
        (qt-text-document-set-modified! (buffer-doc-pointer
                                          (qt-current-buffer fr)) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0))

      ;; Key handler — define once, install on each editor
      ;; Uses consuming variant so QPlainTextEdit doesn't process keys itself.
      (let ((key-handler
             (lambda ()
               (let ((code (qt-last-key-code))
                     (mods (qt-last-key-modifiers))
                     (text (qt-last-key-text)))
                 (let-values (((action data new-state)
                               (qt-key-state-feed! (app-state-key-state app)
                                                   code mods text)))
                   (set! (app-state-key-state app) new-state)
                   (case action
                     ((command)
                      ;; Clear echo on command
                      (when (and (echo-state-message (app-state-echo app))
                                 (null? (key-state-prefix-keys new-state)))
                        (echo-clear! (app-state-echo app)))
                      (execute-command! app data))
                     ((self-insert)
                      (when (app-state-macro-recording app)
                        (set! (app-state-macro-recording app)
                          (cons (cons 'self-insert data)
                                (app-state-macro-recording app))))
                      ;; Handle self-insert directly here for Qt
                      (let* ((buf (qt-current-buffer (app-state-frame app)))
                             (ed (qt-current-editor (app-state-frame app)))
                             (ch (integer->char data))
                             (close-ch (and *auto-pair-mode* (auto-pair-char ch)))
                             (n (get-prefix-arg app))) ; Get prefix arg
                        (cond
                          ;; Suppress in dired buffers
                          ((dired-buffer? buf) (void))
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
                          ;; Shell: only after prompt-pos
                          ((shell-buffer? buf)
                           (let* ((pos (qt-plain-text-edit-cursor-position ed))
                                  (ss (hash-get *shell-state* buf)))
                             (when (and ss (>= pos (shell-state-prompt-pos ss)))
                               (let loop ((i 0))
                                 (when (< i n)
                                   (qt-plain-text-edit-insert-text! ed (string ch))
                                   (loop (+ i 1)))))))
                          (else
                           (if (and close-ch (= n 1)) ; Only auto-pair if n=1
                             ;; Auto-pair: insert both chars and place cursor between
                             (let ((pos (qt-plain-text-edit-cursor-position ed)))
                               (qt-plain-text-edit-insert-text! ed (string ch close-ch))
                               (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
                             ;; Insert character n times
                             (let ((str (make-string n ch)))
                               (qt-plain-text-edit-insert-text! ed str))))))
                      (set! (app-state-prefix-arg app) #f)
                       (set! (app-state-prefix-digit-mode? app) #f)) ; Reset prefix arg
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
                                       (string-append prefix-str "-"))))
                     ((undefined)
                      (echo-error! (app-state-echo app)
                                   (string-append data " is undefined")))
                     ((ignore) (void)))  ;; bare modifier keys — do nothing
                   ;; Update visual decorations (current-line + brace match)
                   (qt-update-visual-decorations!
                     (qt-current-editor (app-state-frame app)))
                   ;; Update modeline and echo after each key
                   (qt-modeline-update! app)
                   (qt-echo-draw! (app-state-echo app) echo-label))))))

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
                                (let ((ed (qt-edit-window-editor (car wins))))
                                  ;; Insert output + new prompt
                                  (qt-plain-text-edit-append! ed output)
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
                      (let ((output (shell-read-available ss)))
                        (when output
                          (let loop ((wins (qt-frame-windows fr)))
                            (when (pair? wins)
                              (if (eq? (qt-edit-window-buffer (car wins)) buf)
                                (let ((ed (qt-edit-window-editor (car wins))))
                                  (qt-plain-text-edit-append! ed output)
                                  (set! (shell-state-prompt-pos ss)
                                    (string-length (qt-plain-text-edit-text ed)))
                                  (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                                  (qt-plain-text-edit-ensure-cursor-visible! ed))
                                (loop (cdr wins)))))))))))
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
              (buffer-list))))
        (qt-timer-start! auto-save-timer 30000))

      ;; Open files from command line
      (for-each (lambda (file) (qt-open-file! app file)) args)

      ;; Show window
      (qt-main-window-set-central-widget! win central)
      (qt-main-window-set-title! win "gerbil-emacs")
      (qt-widget-resize! win 800 600)
      (qt-widget-show! win)

      ;; Initial modeline update (before any key press)
      (qt-modeline-update! app)

      ;; Enter Qt event loop
      (qt-app-exec! qt-app))))

;;;============================================================================
;;; File opening helper
;;;============================================================================

(def (qt-open-file! app filename)
  "Open a file or directory in a new buffer, or view an image."
  (cond
    ;; Directory -> dired
    ((and (file-exists? filename)
          (eq? 'directory (file-info-type (file-info filename))))
     (dired-open-directory! app filename))
    ;; Image file -> image viewer dialog
    ((image-file? filename)
     (let ((main-win (qt-frame-main-win (app-state-frame app))))
       (qt-view-image! app main-win filename)))
    ;; Regular file -> text buffer
    (else
     (let* ((name (path-strip-directory filename))
            (fr (app-state-frame app))
            (ed (qt-current-editor fr))
            (buf (qt-buffer-create! name ed filename)))
       (qt-buffer-attach! ed buf)
       (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
       (when (file-exists? filename)
         (let ((text (read-file-as-string filename)))
           (when text
             (qt-plain-text-edit-set-text! ed text)
             (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
             (qt-plain-text-edit-set-cursor-position! ed 0))))
       (qt-setup-highlighting! app buf)))))
