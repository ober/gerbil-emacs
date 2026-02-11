;;; -*- Gerbil -*-
;;; Qt application and event loop for gerbil-emacs

(export qt-main qt-open-file!)

(import :std/sugar
        :gerbil-qt/qt
        :gerbil-emacs/core
        :gerbil-emacs/qt/keymap
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/modeline
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/commands)

;;;============================================================================
;;; Qt Application
;;;============================================================================

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
                      ;; Suppress self-insert in dired buffers
                      (unless (eq? (buffer-lexer-lang
                                     (qt-current-buffer (app-state-frame app)))
                                   'dired)
                        (qt-plain-text-edit-insert-text!
                          (qt-current-editor (app-state-frame app)) data)))
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
                                   (string-append data " is undefined"))))
                   ;; Update modeline and echo after each key
                   (qt-modeline-update! app)
                   (qt-echo-draw! (app-state-echo app) echo-label))))))

        ;; Install on the initial editor (consuming — editor doesn't see keys)
        (qt-on-key-press-consuming! (qt-current-editor fr) key-handler)

        ;; Store installer so split-window can install on new editors
        (set! (app-state-key-handler app)
              (lambda (editor)
                (qt-on-key-press-consuming! editor key-handler))))

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
  "Open a file or directory in a new buffer."
  (if (and (file-exists? filename)
           (eq? 'directory (file-info-type (file-info filename))))
    ;; Open as dired
    (dired-open-directory! app filename)
    ;; Open as regular file
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
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))
