;;; -*- Gerbil -*-
;;; Helm built-in sources for gemacs
;;;
;;; Backend-agnostic source definitions. Each source produces
;;; candidates from shared state (core.ss, persist.ss).
;;; Actions are lambdas that receive the app and real value.

(export
  ;; Source constructors (take app, return helm-source)
  helm-source-commands
  helm-source-buffers
  helm-source-recentf
  helm-source-buffer-not-found
  helm-source-files
  helm-source-occur
  helm-source-imenu
  helm-source-kill-ring
  helm-source-bookmarks
  helm-source-mark-ring
  helm-source-registers
  helm-source-apropos

  ;; Composed sessions
  helm-mini-sources
  helm-apropos-sources)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gemacs/core
        :gemacs/persist
        :gemacs/helm)

;;;============================================================================
;;; Helper: truncate string for display
;;;============================================================================

(def (truncate-string str max-len)
  (if (> (string-length str) max-len)
    (string-append (substring str 0 (- max-len 3)) "...")
    str))

(def (abbreviate-path path)
  "Abbreviate a file path, replacing $HOME with ~."
  (let ((home (getenv "HOME" "")))
    (if (and (> (string-length home) 0)
             (string-prefix? home path))
      (string-append "~" (substring path (string-length home) (string-length path)))
      path)))

;;;============================================================================
;;; Source: Commands (M-x)
;;;============================================================================

(def (helm-source-commands app)
  "Helm source for M-x command execution."
  (make-helm-source
    "Commands"
    (lambda ()
      ;; Get all command names, sorted alphabetically
      (let ((names (sort (map (lambda (p) (symbol->string (car p)))
                              (hash->list *all-commands*))
                         string<?)))
        ;; Format: "command-name    (C-x C-f)" with keybinding
        (map (lambda (name)
               (let ((binding (find-keybinding-for-command (string->symbol name))))
                 (if binding
                   (string-append name "  (" binding ")")
                   name)))
             names)))
    ;; Actions
    (list (cons "Execute" (lambda (display-str)
                            ;; Extract command name (before any "  (" keybinding suffix)
                            (let* ((space-pos (string-contains display-str "  ("))
                                   (cmd-name (if space-pos
                                               (substring display-str 0 space-pos)
                                               display-str)))
                              (execute-command! app (string->symbol cmd-name))))))
    ;; Persistent action: show docstring
    (lambda (display-str)
      (let* ((space-pos (string-contains display-str "  ("))
             (cmd-name (if space-pos
                         (substring display-str 0 space-pos)
                         display-str))
             (doc (command-doc (string->symbol cmd-name))))
        (echo-message! (app-state-echo app) doc)))
    #f    ;; display-fn
    #f    ;; real-fn
    #t    ;; fuzzy?
    #f    ;; volatile?
    200   ;; candidate-limit (many commands)
    #f    ;; keymap
    #f))  ;; follow?

;;;============================================================================
;;; Source: Buffers
;;;============================================================================

(def (helm-source-buffers app)
  "Helm source for buffer switching."
  (make-helm-source
    "Buffers"
    (lambda ()
      ;; Buffer names in list order (MRU)
      (map (lambda (buf)
             (let* ((name (buffer-name buf))
                    (mod (if (buffer-modified buf) " *" ""))
                    (path (buffer-file-path buf)))
               (if path
                 (string-append name mod "  " (abbreviate-path path))
                 (string-append name mod))))
           *buffer-list*))
    ;; Actions
    (list (cons "Switch" (lambda (display-str)
                           ;; Extract buffer name (before " *" or "  /")
                           (let* ((star-pos (string-contains display-str " *"))
                                  (space-pos (string-contains display-str "  "))
                                  (name (cond
                                          (star-pos (substring display-str 0 star-pos))
                                          (space-pos (substring display-str 0 space-pos))
                                          (else display-str))))
                             (execute-command! app (string->symbol "switch-to-buffer-by-name"))
                             ;; Directly switch via buffer-by-name
                             (let ((buf (buffer-by-name name)))
                               (when buf
                                 (echo-message! (app-state-echo app)
                                   (string-append "Switched to " name)))))))
          (cons "Kill" (lambda (display-str)
                         (let* ((space-pos (string-contains display-str " "))
                                (name (if space-pos
                                        (substring display-str 0 space-pos)
                                        display-str)))
                           (echo-message! (app-state-echo app)
                             (string-append "Kill buffer: " name))))))
    #f    ;; persistent-action
    #f    ;; display-fn
    #f    ;; real-fn
    #t    ;; fuzzy?
    #t    ;; volatile? (buffer list can change)
    50    ;; candidate-limit
    #f    ;; keymap
    #f))  ;; follow?

;;;============================================================================
;;; Source: Recent Files
;;;============================================================================

(def (helm-source-recentf app)
  "Helm source for recently opened files."
  (make-helm-source
    "Recent Files"
    (lambda ()
      (map abbreviate-path *recent-files*))
    ;; Actions
    (list (cons "Open" (lambda (display-str)
                         ;; Expand ~ back to $HOME
                         (let ((path (if (and (> (string-length display-str) 0)
                                              (char=? (string-ref display-str 0) #\~))
                                       (string-append (getenv "HOME" "")
                                                      (substring display-str 1
                                                                 (string-length display-str)))
                                       display-str)))
                           (echo-message! (app-state-echo app)
                             (string-append "Open: " path))))))
    #f    ;; persistent-action
    #f    ;; display-fn
    #f    ;; real-fn
    #t    ;; fuzzy?
    #f    ;; volatile?
    50    ;; candidate-limit
    #f    ;; keymap
    #f))  ;; follow?

;;;============================================================================
;;; Source: Buffer Not Found (create new buffer)
;;;============================================================================

(def (helm-source-buffer-not-found app)
  "Helm source for creating a new buffer from the search input."
  (make-helm-source
    "Create Buffer"
    (lambda () [])  ;; No candidates — will use the input text
    (list (cons "Create" (lambda (name)
                           (echo-message! (app-state-echo app)
                             (string-append "Create buffer: " name)))))
    #f #f #f #f #f 1 #f #f))

;;;============================================================================
;;; Source: Files (directory listing)
;;;============================================================================

(def (helm-source-files app dir)
  "Helm source for file navigation in a directory."
  (make-helm-source
    (string-append "Files in " (abbreviate-path dir))
    (lambda ()
      (with-catch
        (lambda (e) [])
        (lambda ()
          (let* ((entries (sort (directory-files dir) string<?))
                 (formatted (map (lambda (name)
                                   (let ((full (string-append dir "/" name)))
                                     (with-catch
                                       (lambda (e) name)
                                       (lambda ()
                                         (if (eq? 'directory
                                                  (file-info-type (file-info full)))
                                           (string-append name "/")
                                           name)))))
                                 entries)))
            (cons "../" formatted)))))
    ;; Actions
    (list (cons "Open" (lambda (display-str)
                         (let ((path (string-append dir "/"
                                       (if (string-suffix? "/" display-str)
                                         (substring display-str 0
                                                    (- (string-length display-str) 1))
                                         display-str))))
                           (echo-message! (app-state-echo app)
                             (string-append "Open: " path))))))
    #f    ;; persistent-action
    #f    ;; display-fn
    #f    ;; real-fn
    #t    ;; fuzzy?
    #f    ;; volatile?
    200   ;; candidate-limit
    #f    ;; keymap
    #f))  ;; follow?

;;;============================================================================
;;; Source: Occur (lines in current buffer)
;;;============================================================================

(def (helm-source-occur app get-buffer-text-fn)
  "Helm source for matching lines in current buffer.
   get-buffer-text-fn: (-> string) returns current buffer text."
  (make-helm-source
    "Occur"
    (lambda ()
      (let* ((text (get-buffer-text-fn))
             (lines (string-split text #\newline)))
        ;; Number each line
        (let loop ((ls lines) (n 1) (acc []))
          (if (null? ls)
            (reverse acc)
            (loop (cdr ls) (+ n 1)
                  (cons (string-append (number->string n) ": " (car ls))
                        acc))))))
    ;; Actions
    (list (cons "Go to line" (lambda (display-str)
                               ;; Extract line number from "N: text"
                               (let ((colon-pos (string-index display-str #\:)))
                                 (when colon-pos
                                   (let ((line-num (string->number
                                                     (substring display-str 0 colon-pos))))
                                     (when line-num
                                       (echo-message! (app-state-echo app)
                                         (string-append "Go to line " (number->string line-num))))))))))
    #f    ;; persistent-action
    #f    ;; display-fn
    #f    ;; real-fn
    #f    ;; fuzzy? — substring is better for code search
    #t    ;; volatile? (rebuild on every keystroke)
    200   ;; candidate-limit
    #f    ;; keymap
    #t))  ;; follow? (navigate to lines)

;;;============================================================================
;;; Source: Imenu (definitions in current buffer)
;;;============================================================================

(def (helm-source-imenu app get-definitions-fn)
  "Helm source for buffer definitions (functions, variables, etc.).
   get-definitions-fn: (-> list) returns list of (name kind line) tuples."
  (make-helm-source
    "Imenu"
    (lambda ()
      (let ((defs (get-definitions-fn)))
        (map (lambda (d)
               (let ((name (car d))
                     (kind (cadr d))
                     (line (caddr d)))
                 (string-append name "  [" (symbol->string kind) "] L"
                                (number->string line))))
             defs)))
    ;; Actions
    (list (cons "Go to" (lambda (display-str)
                          ;; Extract line from "name  [kind] LN"
                          (let ((l-pos (string-contains display-str "] L")))
                            (when l-pos
                              (let ((line-num (string->number
                                               (substring display-str (+ l-pos 3)
                                                          (string-length display-str)))))
                                (when line-num
                                  (echo-message! (app-state-echo app)
                                    (string-append "Go to line " (number->string line-num))))))))))
    #f #f #f #t #f 200 #f #t))

;;;============================================================================
;;; Source: Kill Ring
;;;============================================================================

(def (helm-source-kill-ring app)
  "Helm source for kill ring entries."
  (make-helm-source
    "Kill Ring"
    (lambda ()
      (let ((ring (app-state-kill-ring app)))
        (map (lambda (text)
               (truncate-string
                 (string-map (lambda (c) (if (char=? c #\newline) (integer->char #x2424) c))
                             text)
                 80))
             ring)))
    ;; Actions
    (list (cons "Insert" (lambda (display-str)
                           (echo-message! (app-state-echo app)
                             (string-append "Insert: "
                               (truncate-string display-str 40))))))
    #f #f #f #t #f 50 #f #f))

;;;============================================================================
;;; Source: Bookmarks
;;;============================================================================

(def (helm-source-bookmarks app)
  "Helm source for bookmark jumping."
  (make-helm-source
    "Bookmarks"
    (lambda ()
      (let ((bm (app-state-bookmarks app)))
        (map (lambda (pair)
               (let ((name (car pair))
                     (info (cdr pair)))
                 ;; info is (buffer-name file-path position) or (buffer-name . position)
                 (if (list? info)
                   (string-append (symbol->string name) "  "
                                  (if (> (length info) 1)
                                    (abbreviate-path (or (cadr info) ""))
                                    ""))
                   (symbol->string name))))
             (hash->list bm))))
    ;; Actions
    (list (cons "Jump" (lambda (display-str)
                         ;; Extract bookmark name
                         (let* ((space-pos (string-contains display-str "  "))
                                (name (if space-pos
                                        (substring display-str 0 space-pos)
                                        display-str)))
                           (echo-message! (app-state-echo app)
                             (string-append "Jump to bookmark: " name))))))
    #f #f #f #t #f 50 #f #f))

;;;============================================================================
;;; Source: Mark Ring
;;;============================================================================

(def (helm-source-mark-ring app)
  "Helm source for mark ring navigation."
  (make-helm-source
    "Mark Ring"
    (lambda ()
      (let ((ring (app-state-mark-ring app)))
        (map (lambda (entry)
               ;; entry is (buffer-name . position)
               (if (pair? entry)
                 (string-append (car entry) ":"
                                (number->string (cdr entry)))
                 (with-output-to-string (lambda () (write entry)))))
             ring)))
    (list (cons "Jump" (lambda (display-str)
                         (echo-message! (app-state-echo app)
                           (string-append "Jump to mark: " display-str)))))
    #f #f #f #t #f 50 #f #f))

;;;============================================================================
;;; Source: Registers
;;;============================================================================

(def (helm-source-registers app)
  "Helm source for register contents."
  (make-helm-source
    "Registers"
    (lambda ()
      (let ((regs (app-state-registers app)))
        (map (lambda (pair)
               (let ((key (car pair))
                     (val (cdr pair)))
                 (string-append (string key) ": "
                   (cond
                     ((string? val) (truncate-string val 60))
                     ((pair? val) (string-append (car val) ":"
                                                 (number->string (cdr val))))
                     (else (with-output-to-string (lambda () (write val))))))))
             (hash->list regs))))
    (list (cons "Insert/Jump" (lambda (display-str)
                                (echo-message! (app-state-echo app)
                                  (string-append "Register: " display-str)))))
    #f #f #f #t #f 50 #f #f))

;;;============================================================================
;;; Source: Apropos (commands + variables)
;;;============================================================================

(def (helm-source-apropos app)
  "Helm source for apropos — search all command names with docs."
  (make-helm-source
    "Apropos"
    (lambda ()
      (map (lambda (pair)
             (let* ((name (symbol->string (car pair)))
                    (doc (command-doc (car pair))))
               (string-append name "  — " (truncate-string doc 60))))
           (sort (hash->list *all-commands*)
                 (lambda (a b) (string<? (symbol->string (car a))
                                         (symbol->string (car b)))))))
    (list (cons "Describe" (lambda (display-str)
                             (let* ((dash-pos (string-contains display-str "  — "))
                                    (name (if dash-pos
                                            (substring display-str 0 dash-pos)
                                            display-str)))
                               (echo-message! (app-state-echo app)
                                 (string-append name ": " (command-doc (string->symbol name)))))))
          (cons "Execute" (lambda (display-str)
                            (let* ((dash-pos (string-contains display-str "  — "))
                                   (name (if dash-pos
                                           (substring display-str 0 dash-pos)
                                           display-str)))
                              (execute-command! app (string->symbol name))))))
    #f #f #f #t #f 200 #f #f))

;;;============================================================================
;;; Composed sessions
;;;============================================================================

(def (helm-mini-sources app)
  "Sources for helm-mini: buffers + recent files + create buffer."
  (list (helm-source-buffers app)
        (helm-source-recentf app)
        (helm-source-buffer-not-found app)))

(def (helm-apropos-sources app)
  "Sources for helm-apropos."
  (list (helm-source-apropos app)))
