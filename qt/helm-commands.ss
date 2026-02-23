;;; -*- Gerbil -*-
;;; Helm commands for gemacs (Qt backend)
;;;
;;; Wires helm sources to the Qt renderer and registers commands.

(export
  qt-register-helm-commands!
  qt-setup-helm-bindings!
  qt-cmd-helm-M-x
  qt-cmd-helm-mini
  qt-cmd-helm-buffers-list
  qt-cmd-helm-find-files
  qt-cmd-helm-occur
  qt-cmd-helm-imenu
  qt-cmd-helm-show-kill-ring
  qt-cmd-helm-bookmarks
  qt-cmd-helm-mark-ring
  qt-cmd-helm-register
  qt-cmd-helm-apropos
  qt-cmd-helm-grep
  qt-cmd-helm-man
  qt-cmd-helm-resume
  qt-cmd-helm-toggle-mode)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/core
        :gemacs/persist
        :gemacs/qt/sci-shim
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/helm
        :gemacs/helm-sources
        :gemacs/qt/helm-qt)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (current-qt-editor* app)
  (qt-edit-window-editor (qt-current-window (app-state-frame app))))

(def (current-qt-buffer* app)
  (qt-edit-window-buffer (qt-current-window (app-state-frame app))))

(def (buffer-names-mru*)
  "Get buffer names in MRU order."
  (map buffer-name *buffer-list*))

;;;============================================================================
;;; Helm M-x
;;;============================================================================

(def (qt-cmd-helm-M-x app)
  "Execute a command with helm-style narrowing (Qt)."
  (let* ((src (helm-source-commands app))
         (session (make-new-session (list src) "*helm M-x*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (let* ((space-pos (string-contains result "  ("))
             (cmd-name (if space-pos
                         (substring result 0 space-pos)
                         result)))
        (execute-command! app (string->symbol cmd-name))))))

;;;============================================================================
;;; Helm Mini
;;;============================================================================

(def (qt-cmd-helm-mini app)
  "Switch buffers or open recent files with helm narrowing (Qt)."
  (let* ((sources (helm-mini-sources app))
         (session (make-new-session sources "*helm mini*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      ;; Strip source prefix "[Buffers] " if present
      (let* ((clean (if (string-prefix? "[" result)
                      (let ((bracket-end (string-index result #\])))
                        (if bracket-end
                          (string-trim (substring result (+ bracket-end 1)
                                                  (string-length result)))
                          result))
                      result))
             ;; Strip modified indicator and path
             (buf-name (let ((star-pos (string-contains clean " *")))
                         (if star-pos
                           (substring clean 0 star-pos)
                           (let ((space-pos (string-contains clean "  ")))
                             (if space-pos
                               (substring clean 0 space-pos)
                               clean)))))
             (buf (buffer-by-name buf-name)))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor* app)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (echo-message! (app-state-echo app)
              (string-append "Switched to: " buf-name)))
          ;; Recent file or new buffer
          (echo-message! (app-state-echo app)
            (string-append "Selected: " clean)))))))

;;;============================================================================
;;; Helm Buffers List
;;;============================================================================

(def (qt-cmd-helm-buffers-list app)
  "List and switch buffers with helm narrowing (Qt)."
  (let* ((src (helm-source-buffers app))
         (session (make-new-session (list src) "*helm buffers*"))
         (result (helm-qt-run! session app)))
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
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor* app)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (echo-message! (app-state-echo app)
              (string-append "Switched to: " buf-name))))))))

;;;============================================================================
;;; Helm Find Files
;;;============================================================================

(def (qt-cmd-helm-find-files app)
  "Find files with helm-style narrowing (Qt)."
  (let* ((echo (app-state-echo app))
         (cur-buf (qt-edit-window-buffer (qt-current-window (app-state-frame app))))
         (start-dir (if (and cur-buf (buffer-file-path cur-buf))
                      (path-directory (buffer-file-path cur-buf))
                      (current-directory)))
         (src (helm-source-files app start-dir))
         (session (make-new-session (list src) "*helm find files*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (echo-message! echo (string-append "Open: " result)))))

;;;============================================================================
;;; Helm Occur
;;;============================================================================

(def (qt-cmd-helm-occur app)
  "Search lines in current buffer with helm (Qt)."
  (let* ((ed (current-qt-editor* app))
         (echo (app-state-echo app)))
    (when ed
      (let* ((text (qt-plain-text-edit-text ed))
             (text-fn (lambda () text))
             (src (helm-source-occur app text-fn))
             (session (make-new-session (list src) "*helm occur*"))
             (result (helm-qt-run! session app)))
        (when (and result (string? result))
          (let ((colon-pos (string-index result #\:)))
            (when colon-pos
              (let ((line-num (string->number (substring result 0 colon-pos))))
                (when line-num
                  (let ((pos (sci-send ed SCI_POSITIONFROMLINE (- line-num 1))))
                    (sci-send ed SCI_GOTOPOS pos)
                    (echo-message! echo
                      (string-append "Line " (number->string line-num)))))))))))))

;;;============================================================================
;;; Helm Imenu
;;;============================================================================

(def (qt-cmd-helm-imenu app)
  "Navigate definitions in current buffer (Qt)."
  (let* ((ed (current-qt-editor* app))
         (echo (app-state-echo app)))
    (when ed
      (let* ((text (qt-plain-text-edit-text ed))
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
             (result (helm-qt-run! session app)))
        (when (and result (string? result))
          (let ((l-pos (string-contains result "] L")))
            (when l-pos
              (let ((line-num (string->number
                               (substring result (+ l-pos 3)
                                          (string-length result)))))
                (when line-num
                  (let ((pos (sci-send ed SCI_POSITIONFROMLINE (- line-num 1))))
                    (sci-send ed SCI_GOTOPOS pos)
                    (echo-message! echo
                      (string-append "Line " (number->string line-num)))))))))))))

;;;============================================================================
;;; Helm Show Kill Ring
;;;============================================================================

(def (qt-cmd-helm-show-kill-ring app)
  "Browse and insert from kill ring (Qt)."
  (let* ((src (helm-source-kill-ring app))
         (session (make-new-session (list src) "*helm kill ring*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (let* ((ring (app-state-kill-ring app))
             (ed (current-qt-editor* app)))
        (when (and ed (pair? ring))
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
            (qt-plain-text-edit-insert-text! ed entry)
            (echo-message! (app-state-echo app) "Inserted from kill ring")))))))

;;;============================================================================
;;; Helm Bookmarks / Mark Ring / Register / Apropos / Resume
;;;============================================================================

(def (qt-cmd-helm-bookmarks app)
  "Browse bookmarks with helm (Qt)."
  (let* ((src (helm-source-bookmarks app))
         (session (make-new-session (list src) "*helm bookmarks*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (echo-message! (app-state-echo app)
        (string-append "Bookmark: " result)))))

(def (qt-cmd-helm-mark-ring app)
  "Browse mark ring (Qt)."
  (let* ((src (helm-source-mark-ring app))
         (session (make-new-session (list src) "*helm marks*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (echo-message! (app-state-echo app)
        (string-append "Mark: " result)))))

(def (qt-cmd-helm-register app)
  "Browse registers (Qt)."
  (let* ((src (helm-source-registers app))
         (session (make-new-session (list src) "*helm registers*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (echo-message! (app-state-echo app)
        (string-append "Register: " result)))))

(def (qt-cmd-helm-apropos app)
  "Search commands with descriptions (Qt)."
  (let* ((sources (helm-apropos-sources app))
         (session (make-new-session sources "*helm apropos*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (let* ((dash-pos (string-contains result "  — "))
             (name (if dash-pos
                     (substring result 0 dash-pos)
                     result)))
        (echo-message! (app-state-echo app)
          (string-append name ": " (command-doc (string->symbol name))))))))

(def (qt-cmd-helm-grep app)
  "Search with grep/rg using helm narrowing (Qt)."
  (let* ((echo (app-state-echo app))
         (cur-buf (qt-edit-window-buffer (qt-current-window (app-state-frame app))))
         (search-dir (if (and cur-buf (buffer-file-path cur-buf))
                       (path-directory (buffer-file-path cur-buf))
                       (current-directory)))
         (src (helm-source-grep app search-dir))
         (session (make-new-session (list src) "*helm grep*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (let ((colon1 (string-index result #\:)))
        (if colon1
          (let* ((file (substring result 0 colon1))
                 (rest (substring result (+ colon1 1) (string-length result)))
                 (colon2 (string-index rest #\:))
                 (line-num (if colon2
                             (string->number (substring rest 0 colon2))
                             #f)))
            (echo-message! echo
              (string-append "Open: " file
                (if line-num (string-append " line " (number->string line-num)) ""))))
          (echo-message! echo (string-append "Grep: " result)))))))

(def (qt-cmd-helm-man app)
  "Browse man pages with helm narrowing (Qt)."
  (let* ((src (helm-source-man app))
         (session (make-new-session (list src) "*helm man*"))
         (result (helm-qt-run! session app)))
    (when (and result (string? result))
      (let ((paren-pos (string-index result #\()))
        (if paren-pos
          (let* ((name (string-trim-right (substring result 0 paren-pos)))
                 (close-paren (string-index result #\)))
                 (section (if close-paren
                            (substring result (+ paren-pos 1) close-paren)
                            "")))
            (echo-message! (app-state-echo app)
              (string-append "man " section " " name)))
          (echo-message! (app-state-echo app)
            (string-append "man: " result)))))))

(def (qt-cmd-helm-resume app)
  "Resume last helm session (Qt)."
  (let ((session (helm-session-resume)))
    (if session
      (begin
        (set! (helm-session-alive? session) #t)
        (helm-qt-run! session app))
      (echo-message! (app-state-echo app) "No previous helm session"))))

;;;============================================================================
;;; Helm Mode Toggle
;;;============================================================================

(def (qt-cmd-helm-toggle-mode app)
  "Toggle helm-mode (Qt)."
  (set! *helm-mode* (not *helm-mode*))
  (if *helm-mode*
    (begin
      (qt-setup-helm-bindings!)
      (echo-message! (app-state-echo app) "Helm mode: on"))
    (begin
      (keymap-bind! *global-keymap* "M-x" 'execute-extended-command)
      (keymap-bind! *ctrl-x-map* "b" 'switch-buffer)
      (keymap-bind! *ctrl-x-map* "C-b" 'list-buffers)
      (keymap-bind! *global-keymap* "M-y" 'yank-pop)
      (keymap-bind! *ctrl-x-r-map* "b" 'bookmark-jump)
      (echo-message! (app-state-echo app) "Helm mode: off"))))

(def (qt-setup-helm-bindings!)
  "Override standard keybindings with helm equivalents (Qt)."
  (keymap-bind! *global-keymap* "M-x" 'helm-M-x)
  (keymap-bind! *ctrl-x-map* "b" 'helm-mini)
  (keymap-bind! *ctrl-x-map* "C-b" 'helm-buffers-list)
  (keymap-bind! *global-keymap* "M-y" 'helm-show-kill-ring)
  (keymap-bind! *ctrl-x-r-map* "b" 'helm-bookmarks))

;;;============================================================================
;;; Qt command registration
;;;============================================================================

(def (qt-register-helm-commands!)
  "Register all helm commands for Qt backend."
  (register-command! 'helm-M-x qt-cmd-helm-M-x)
  (register-command! 'helm-mini qt-cmd-helm-mini)
  (register-command! 'helm-buffers-list qt-cmd-helm-buffers-list)
  (register-command! 'helm-find-files qt-cmd-helm-find-files)
  (register-command! 'helm-occur qt-cmd-helm-occur)
  (register-command! 'helm-imenu qt-cmd-helm-imenu)
  (register-command! 'helm-show-kill-ring qt-cmd-helm-show-kill-ring)
  (register-command! 'helm-bookmarks qt-cmd-helm-bookmarks)
  (register-command! 'helm-mark-ring qt-cmd-helm-mark-ring)
  (register-command! 'helm-register qt-cmd-helm-register)
  (register-command! 'helm-apropos qt-cmd-helm-apropos)
  (register-command! 'helm-grep qt-cmd-helm-grep)
  (register-command! 'helm-man qt-cmd-helm-man)
  (register-command! 'helm-resume qt-cmd-helm-resume)
  (register-command! 'helm-mode qt-cmd-helm-toggle-mode)
  (register-command! 'toggle-helm-mode qt-cmd-helm-toggle-mode)

  ;; Documentation (shared with TUI)
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
  (register-command-doc! 'helm-grep "Search with grep/rg using helm narrowing.")
  (register-command-doc! 'helm-man "Browse man pages with helm narrowing.")
  (register-command-doc! 'helm-resume "Resume the last helm session.")
  (register-command-doc! 'helm-mode "Toggle helm-mode for incremental completion."))
