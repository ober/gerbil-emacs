;;; -*- Gerbil -*-
;;; Main application and event loop for gerbil-emacs

(export app-init! app-run! main)

(import :std/sugar
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/style
        :gerbil-scintilla/tui
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/editor)

;;;============================================================================
;;; Application initialization
;;;============================================================================

(def (app-init! files)
  "Initialize the editor. Returns an app-state."
  ;; Set up TUI
  (tui-init!)
  (tui-set-input-mode! (bitwise-ior TB_INPUT_ALT TB_INPUT_MOUSE))
  (tui-set-output-mode! TB_OUTPUT_TRUECOLOR)
  ;; Set terminal background to match editor dark theme
  (tui-set-clear-attrs! #x00d8d8d8 #x00181818)

  ;; Set up keybindings and commands
  (setup-default-bindings!)
  (register-all-commands!)

  ;; Create frame with one window
  (let* ((width (tui-width))
         (height (tui-height))
         (fr (frame-init! width height))
         (app (new-app-state fr)))

    ;; Configure dark theme on all editors
    (for-each (lambda (win) (setup-editor-theme! (edit-window-editor win)))
              (frame-windows fr))

    ;; Set initial text in scratch buffer
    (let ((ed (current-editor app)))
      (editor-set-text ed ";; *scratch*\n")
      (editor-set-save-point ed)
      (editor-goto-pos ed 0))

    ;; Open files from command line
    (for-each (lambda (file) (open-file-in-app! app file))
              files)

    app))

(def (open-file-in-app! app filename)
  "Open a file in a new buffer."
  (let* ((name (path-strip-directory filename))
         (ed (current-editor app))
         (buf (buffer-create! name ed filename))
         (fr (app-state-frame app)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (when (file-exists? filename)
      (let ((text (read-file-as-string filename)))
        (when text
          (editor-set-text ed text)
          (editor-set-save-point ed)
          (editor-goto-pos ed 0))))))

;;;============================================================================
;;; Event loop
;;;============================================================================

(def (app-run! app)
  "Main event loop."
  (let loop ()
    (when (app-state-running app)
      ;; Process Scintilla notifications
      (for-each (lambda (win)
                  (editor-poll-notifications (edit-window-editor win)))
                (frame-windows (app-state-frame app)))

      ;; Draw modelines, dividers, and echo area FIRST into the termbox buffer.
      ;; This must happen before editor-refresh because Scintilla's
      ;; Refresh() calls tb_present() internally.
      (draw-all-modelines! app)
      (frame-draw-dividers! (app-state-frame app))
      (let* ((fr (app-state-frame app))
             (echo-row (- (frame-height fr) 1))
             (width (frame-width fr)))
        (echo-draw! (app-state-echo app) echo-row width))

      ;; Refresh all editors (paints editor content + calls tb_present)
      (frame-refresh! (app-state-frame app))

      ;; Position cursor at caret in current window
      (position-cursor! app)

      ;; Wait for event with timeout
      (let ((ev (tui-peek-event 50)))
        (when ev
          (dispatch-event! app ev)))

      (loop))))

;;;============================================================================
;;; Editor theme (dark colors matching scintilla-termbox defaults)
;;;============================================================================

(def (setup-editor-theme! ed)
  "Configure dark terminal theme for an editor."
  ;; Default style: light gray on dark gray (matching semester.c reference)
  (editor-style-set-foreground ed STYLE_DEFAULT #xd8d8d8)
  (editor-style-set-background ed STYLE_DEFAULT #x181818)
  (send-message ed SCI_STYLECLEARALL)  ; propagate to all styles
  ;; White caret for visibility
  (editor-set-caret-foreground ed #xFFFFFF))

;;;============================================================================
;;; Drawing helpers
;;;============================================================================

(def (draw-all-modelines! app)
  (let* ((fr (app-state-frame app))
         (cur-idx (frame-current-idx fr))
         (windows (frame-windows fr)))
    (let loop ((wins windows) (i 0))
      (when (pair? wins)
        (modeline-draw! (car wins) (= i cur-idx))
        (loop (cdr wins) (+ i 1))))))

(def (position-cursor! app)
  "Position terminal cursor at the caret location in the current editor."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         ;; Use POINTX/POINTY for screen-relative coordinates
         ;; (accounts for margins, scroll, tab width)
         (screen-x (send-message ed SCI_POINTXFROMPOSITION 0 pos))
         (screen-y (send-message ed SCI_POINTYFROMPOSITION 0 pos))
         (win-x (edit-window-x win))
         (win-y (edit-window-y win)))
    (tui-set-cursor! (+ win-x screen-x) (+ win-y screen-y))
    ;; Present to make cursor position visible immediately
    (tui-present!)))

;;;============================================================================
;;; Event dispatch
;;;============================================================================

(def (dispatch-event! app ev)
  (cond
    ;; Resize event
    ((tui-event-resize? ev)
     (let ((w (tui-event-w ev))
           (h (tui-event-h ev)))
       (frame-resize! (app-state-frame app) w h)))

    ;; Mouse event
    ((tui-event-mouse? ev)
     (dispatch-mouse! app ev))

    ;; Key event
    ((tui-event-key? ev)
     (dispatch-key! app ev))))

(def (dispatch-mouse! app ev)
  "Forward mouse event to the appropriate editor."
  (let* ((mx (tui-event-x ev))
         (my (tui-event-y ev))
         (key (tui-event-key ev))
         (fr (app-state-frame app)))
    ;; Find which window the mouse is in
    (let loop ((wins (frame-windows fr)) (i 0))
      (when (pair? wins)
        (let* ((win (car wins))
               (wy (edit-window-y win))
               (wh (- (edit-window-h win) 1)))  ; edit area height
          (if (and (>= my wy) (< my (+ wy wh)))
            ;; Mouse is in this window's edit area
            (let ((ed (edit-window-editor win))
                  (event-type (cond
                                ((= key TB_KEY_MOUSE_LEFT) SCM_PRESS)
                                ((= key TB_KEY_MOUSE_RELEASE) SCM_RELEASE)
                                (else SCM_PRESS))))
              ;; Focus this window
              (set! (frame-current-idx fr) i)
              (editor-send-mouse ed event-type 1
                                 (- my wy) mx))
            (loop (cdr wins) (+ i 1))))))))

(def (dispatch-key! app ev)
  "Process a key event through the keymap state machine."
  (let ((echo (app-state-echo app)))
    ;; Clear echo message on next key press (unless in prefix)
    (when (and (echo-state-message echo)
               (null? (key-state-prefix-keys (app-state-key-state app))))
      (echo-clear! echo))

    (let-values (((action data new-state)
                  (key-state-feed! (app-state-key-state app) ev)))
      (set! (app-state-key-state app) new-state)
      (case action
        ((command)
         (execute-command! app data))
        ((prefix)
         ;; Show prefix in echo area
         (let ((prefix-str (let loop ((keys (key-state-prefix-keys new-state))
                                      (acc ""))
                             (if (null? keys) acc
                               (loop (cdr keys)
                                     (if (string=? acc "")
                                       (car keys)
                                       (string-append acc " " (car keys))))))))
           (echo-message! (app-state-echo app)
                          (string-append prefix-str "-"))))
        ((self-insert)
         (cmd-self-insert! app data))
        ((undefined)
         (echo-error! (app-state-echo app)
                      (string-append data " is undefined")))))))

;;;============================================================================
;;; Main entry point
;;;============================================================================

(def main
  (lambda args
    (let ((app (app-init! args)))
      (try
        (app-run! app)
        (finally
          (frame-shutdown! (app-state-frame app))
          (tui-shutdown!))))))
