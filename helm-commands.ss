;;; -*- Gerbil -*-
;;; Helm commands for gemacs (TUI backend)
;;;
;;; Wires helm sources to the TUI renderer and registers commands.
;;; Imported by app.ss / editor.ss for TUI builds.

(export
  register-helm-commands!
  setup-helm-bindings!
  cmd-helm-M-x
  cmd-helm-mini
  cmd-helm-buffers-list
  cmd-helm-find-files
  cmd-helm-occur
  cmd-helm-imenu
  cmd-helm-show-kill-ring
  cmd-helm-bookmarks
  cmd-helm-mark-ring
  cmd-helm-register
  cmd-helm-apropos
  cmd-helm-resume
  cmd-helm-toggle-mode)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/persist
        :gemacs/buffer
        :gemacs/window
        :gemacs/echo
        :gemacs/helm
        :gemacs/helm-sources
        :gemacs/helm-tui)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (tui-helm-run app sources buffer-name)
  "Create a helm session and run it in the TUI renderer.
   Returns the selected value or #f."
  (let ((session (make-new-session sources buffer-name)))
    (helm-tui-run! session)))

(def (current-editor app)
  "Get the current editor from the app's frame."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (idx (frame-current-idx fr)))
    (if (and (pair? wins) (< idx (length wins)))
      (edit-window-editor (list-ref wins idx))
      #f)))

(def (buffer-names-mru)
  "Get buffer names in MRU order."
  (map buffer-name *buffer-list*))

;;;============================================================================
;;; Helm M-x
;;;============================================================================

(def (cmd-helm-M-x app)
  "Execute a command with helm-style incremental narrowing."
  (let* ((src (helm-source-commands app))
         (session (make-new-session (list src) "*helm M-x*"))
         (result (helm-tui-run! session)))
    ;; The action is already wired in the source — but if we get a raw string,
    ;; we need to execute it
    (when (and result (string? result))
      ;; Extract command name from display string
      (let* ((space-pos (string-contains result "  ("))
             (cmd-name (if space-pos
                         (substring result 0 space-pos)
                         result)))
        (execute-command! app (string->symbol cmd-name))))))

;;;============================================================================
;;; Helm Mini (buffers + recent files)
;;;============================================================================

(def (cmd-helm-mini app)
  "Switch buffers or open recent files with helm-style narrowing."
  (let* ((sources (helm-mini-sources app))
         (session (make-new-session sources "*helm mini*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      ;; Try to find existing buffer first
      (let* ((clean-name (let ((bracket-pos (string-contains result " [")))
                           (if bracket-pos
                             (string-trim-right (substring result 0 bracket-pos))
                             result)))
             ;; Strip modified indicator and path suffix
             (buf-name (let ((star-pos (string-contains clean-name " *")))
                         (if star-pos
                           (substring clean-name 0 star-pos)
                           (let ((space-pos (string-contains clean-name "  ")))
                             (if space-pos
                               (substring clean-name 0 space-pos)
                               clean-name)))))
             (buf (buffer-by-name buf-name)))
        (if buf
          (let ((ed (current-editor app)))
            (when ed
              (buffer-attach! ed buf)
              (echo-message! (app-state-echo app)
                (string-append "Switched to: " buf-name))))
          ;; Maybe it's a recent file path
          (echo-message! (app-state-echo app)
            (string-append "Selected: " result)))))))

;;;============================================================================
;;; Helm Buffers List
;;;============================================================================

(def (cmd-helm-buffers-list app)
  "List and switch buffers with helm-style narrowing."
  (let* ((src (helm-source-buffers app))
         (session (make-new-session (list src) "*helm buffers*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      (let* ((buf-name (let ((star-pos (string-contains result " *")))
                         (if star-pos
                           (substring result 0 star-pos)
                           (let ((space-pos (string-contains result "  ")))
                             (if space-pos
                               (substring result 0 space-pos)
                               result)))))
             (buf (buffer-by-name buf-name)))
        (when buf
          (let ((ed (current-editor app)))
            (when ed
              (buffer-attach! ed buf)
              (echo-message! (app-state-echo app)
                (string-append "Switched to: " buf-name)))))))))

;;;============================================================================
;;; Helm Find Files
;;;============================================================================

(def (cmd-helm-find-files app)
  "Find and open files with helm-style narrowing."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (bufs *buffer-list*)
         (cur-buf (and (pair? bufs)
                       (let ((idx (frame-current-idx (app-state-frame app))))
                         (if (< idx (length bufs))
                           (list-ref bufs idx)
                           (car bufs)))))
         (start-dir (if (and cur-buf (buffer-file-path cur-buf))
                      (path-directory (buffer-file-path cur-buf))
                      (current-directory)))
         (src (helm-source-files app start-dir))
         (session (make-new-session (list src) "*helm find files*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      (echo-message! echo (string-append "Open: " result)))))

;;;============================================================================
;;; Helm Occur
;;;============================================================================

(def (cmd-helm-occur app)
  "Search lines in current buffer with helm narrowing."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (when ed
      (let* ((text-fn (lambda ()
                         (let ((len (send-message ed 2006 0 0)))  ;; SCI_GETLENGTH
                           (if (> len 0)
                             (editor-get-text ed)
                             ""))))
             (src (helm-source-occur app text-fn))
             (session (make-new-session (list src) "*helm occur*"))
             (result (helm-tui-run! session)))
        (when (and result (string? result))
          ;; Extract line number and go to it
          (let ((colon-pos (string-index result #\:)))
            (when colon-pos
              (let ((line-num (string->number (substring result 0 colon-pos))))
                (when line-num
                  (let ((pos (send-message ed 2167 (- line-num 1) 0)))  ;; SCI_POSITIONFROMLINE
                    (send-message ed 2024 pos 0)  ;; SCI_GOTOPOS
                    (echo-message! echo
                      (string-append "Line " (number->string line-num)))))))))))))

;;;============================================================================
;;; Helm Imenu (stub — uses definition regex)
;;;============================================================================

(def (cmd-helm-imenu app)
  "Navigate definitions in current buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (when ed
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (defs (let loop ((ls lines) (n 1) (acc []))
                     (if (null? ls) (reverse acc)
                       (let ((line (car ls)))
                         (if (or (string-prefix? "(def " line)
                                 (string-prefix? "(defstruct " line)
                                 (string-prefix? "(defclass " line)
                                 (string-prefix? "(define " line)
                                 (string-prefix? "(defmethod " line)
                                 (string-prefix? "(defrule " line))
                           (loop (cdr ls) (+ n 1)
                                 (cons (list (string-trim-both line) 'def n) acc))
                           (loop (cdr ls) (+ n 1) acc))))))
             (get-defs (lambda () defs))
             (src (helm-source-imenu app get-defs))
             (session (make-new-session (list src) "*helm imenu*"))
             (result (helm-tui-run! session)))
        (when (and result (string? result))
          (let ((l-pos (string-contains result "] L")))
            (when l-pos
              (let ((line-num (string->number
                               (substring result (+ l-pos 3)
                                          (string-length result)))))
                (when line-num
                  (let ((pos (send-message ed 2167 (- line-num 1) 0)))
                    (send-message ed 2024 pos 0)
                    (echo-message! echo
                      (string-append "Line " (number->string line-num)))))))))))))

;;;============================================================================
;;; Helm Show Kill Ring
;;;============================================================================

(def (cmd-helm-show-kill-ring app)
  "Browse and insert from kill ring."
  (let* ((src (helm-source-kill-ring app))
         (session (make-new-session (list src) "*helm kill ring*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      ;; Find the actual kill ring entry (undo the display transformation)
      (let* ((ring (app-state-kill-ring app))
             (ed (current-editor app)))
        (when (and ed (pair? ring))
          ;; Insert first matching entry
          (let ((entry (let loop ((entries ring))
                         (if (null? entries) (car ring)
                           (let ((display (string-map
                                            (lambda (c)
                                              (if (char=? c #\newline) (integer->char #x2424) c))
                                            (car entries))))
                             (if (string-prefix? (substring result 0
                                                   (min (string-length result)
                                                        (string-length display)))
                                                 display)
                               (car entries)
                               (loop (cdr entries))))))))
            (send-message/string ed 2170 entry)  ;; SCI_REPLACESEL
            (echo-message! (app-state-echo app) "Inserted from kill ring")))))))

;;;============================================================================
;;; Helm Bookmarks
;;;============================================================================

(def (cmd-helm-bookmarks app)
  "Browse and jump to bookmarks."
  (let* ((src (helm-source-bookmarks app))
         (session (make-new-session (list src) "*helm bookmarks*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      (let* ((space-pos (string-contains result "  "))
             (name (if space-pos
                     (substring result 0 space-pos)
                     result)))
        (echo-message! (app-state-echo app)
          (string-append "Bookmark: " name))))))

;;;============================================================================
;;; Helm Mark Ring
;;;============================================================================

(def (cmd-helm-mark-ring app)
  "Browse mark ring positions."
  (let* ((src (helm-source-mark-ring app))
         (session (make-new-session (list src) "*helm marks*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      (echo-message! (app-state-echo app)
        (string-append "Mark: " result)))))

;;;============================================================================
;;; Helm Register
;;;============================================================================

(def (cmd-helm-register app)
  "Browse registers."
  (let* ((src (helm-source-registers app))
         (session (make-new-session (list src) "*helm registers*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      (echo-message! (app-state-echo app)
        (string-append "Register: " result)))))

;;;============================================================================
;;; Helm Apropos
;;;============================================================================

(def (cmd-helm-apropos app)
  "Search commands with descriptions."
  (let* ((sources (helm-apropos-sources app))
         (session (make-new-session sources "*helm apropos*"))
         (result (helm-tui-run! session)))
    (when (and result (string? result))
      (let* ((dash-pos (string-contains result "  — "))
             (name (if dash-pos
                     (substring result 0 dash-pos)
                     result)))
        (echo-message! (app-state-echo app)
          (string-append name ": " (command-doc (string->symbol name))))))))

;;;============================================================================
;;; Helm Resume
;;;============================================================================

(def (cmd-helm-resume app)
  "Resume the last helm session."
  (let ((session (helm-session-resume)))
    (if session
      (begin
        (set! (helm-session-alive? session) #t)
        (helm-tui-run! session))
      (echo-message! (app-state-echo app) "No previous helm session"))))

;;;============================================================================
;;; Helm Mode Toggle
;;;============================================================================

(def (cmd-helm-toggle-mode app)
  "Toggle helm-mode. When on, standard keys use helm equivalents."
  (set! *helm-mode* (not *helm-mode*))
  (if *helm-mode*
    (begin
      (setup-helm-bindings!)
      (echo-message! (app-state-echo app) "Helm mode: on"))
    (begin
      ;; Restore default bindings
      (keymap-bind! *global-keymap* "M-x" 'execute-extended-command)
      (keymap-bind! *ctrl-x-map* "b" 'switch-buffer)
      (keymap-bind! *ctrl-x-map* "C-b" 'list-buffers)
      (keymap-bind! *global-keymap* "M-y" 'yank-pop)
      (keymap-bind! *ctrl-x-r-map* "b" 'bookmark-jump)
      (echo-message! (app-state-echo app) "Helm mode: off"))))

;;;============================================================================
;;; Helm keybindings (when helm-mode is on)
;;;============================================================================

(def (setup-helm-bindings!)
  "Override standard keybindings with helm equivalents."
  (keymap-bind! *global-keymap* "M-x" 'helm-M-x)
  (keymap-bind! *ctrl-x-map* "b" 'helm-mini)
  (keymap-bind! *ctrl-x-map* "C-b" 'helm-buffers-list)
  (keymap-bind! *global-keymap* "M-y" 'helm-show-kill-ring)
  (keymap-bind! *ctrl-x-r-map* "b" 'helm-bookmarks))

;;;============================================================================
;;; Command registration
;;;============================================================================

(def (register-helm-commands!)
  "Register all helm commands."
  (register-command! 'helm-M-x cmd-helm-M-x)
  (register-command! 'helm-mini cmd-helm-mini)
  (register-command! 'helm-buffers-list cmd-helm-buffers-list)
  (register-command! 'helm-find-files cmd-helm-find-files)
  (register-command! 'helm-occur cmd-helm-occur)
  (register-command! 'helm-imenu cmd-helm-imenu)
  (register-command! 'helm-show-kill-ring cmd-helm-show-kill-ring)
  (register-command! 'helm-bookmarks cmd-helm-bookmarks)
  (register-command! 'helm-mark-ring cmd-helm-mark-ring)
  (register-command! 'helm-register cmd-helm-register)
  (register-command! 'helm-apropos cmd-helm-apropos)
  (register-command! 'helm-resume cmd-helm-resume)
  (register-command! 'helm-mode cmd-helm-toggle-mode)
  (register-command! 'toggle-helm-mode cmd-helm-toggle-mode)

  ;; Documentation
  (register-command-doc! 'helm-M-x "Execute a command with helm-style incremental narrowing.")
  (register-command-doc! 'helm-mini "Switch buffers or open recent files with helm narrowing.")
  (register-command-doc! 'helm-buffers-list "List and switch buffers with helm narrowing.")
  (register-command-doc! 'helm-find-files "Find files with helm-style directory navigation.")
  (register-command-doc! 'helm-occur "Search lines in current buffer with helm narrowing.")
  (register-command-doc! 'helm-imenu "Navigate definitions with helm narrowing.")
  (register-command-doc! 'helm-show-kill-ring "Browse and insert from kill ring with helm.")
  (register-command-doc! 'helm-bookmarks "Browse bookmarks with helm.")
  (register-command-doc! 'helm-mark-ring "Browse mark ring with helm.")
  (register-command-doc! 'helm-register "Browse registers with helm.")
  (register-command-doc! 'helm-apropos "Search commands with descriptions.")
  (register-command-doc! 'helm-resume "Resume the last helm session.")
  (register-command-doc! 'helm-mode "Toggle helm-mode for incremental completion."))
