;;; -*- Gerbil -*-
;;; Shared core for gerbil-emacs
;;;
;;; Backend-agnostic logic: keymap data structures, command registry,
;;; echo state, buffer metadata, app state, file I/O helpers.
;;; No Scintilla or TUI imports — this module is pure logic.

(export
  ;; Keymap data structures
  make-keymap
  keymap-bind!
  keymap-lookup
  (struct-out key-state)
  make-initial-key-state
  *global-keymap*
  *ctrl-x-map*
  setup-default-bindings!

  ;; App state
  (struct-out app-state)
  new-app-state

  ;; Command registry
  register-command!
  find-command
  execute-command!

  ;; Echo state (pure state mutations)
  (struct-out echo-state)
  make-initial-echo-state
  echo-message!
  echo-error!
  echo-clear!

  ;; Buffer metadata (struct + list, no FFI)
  (struct-out buffer)
  *buffer-list*
  buffer-list
  buffer-list-add!
  buffer-list-remove!
  buffer-by-name
  buffer-scratch-name

  ;; File I/O helpers
  read-file-as-string
  write-string-to-file)

(import :std/sugar)

;;;============================================================================
;;; Keymap data structure
;;;============================================================================

(def (make-keymap)
  (make-hash-table))

(def (keymap-bind! km key-str value)
  (hash-put! km key-str value))

(def (keymap-lookup km key-str)
  (hash-get km key-str))

;;;============================================================================
;;; Key state machine for multi-key sequences
;;;============================================================================

(defstruct key-state
  (keymap        ; current keymap to look up in
   prefix-keys)  ; list of accumulated key strings (for echo display)
  transparent: #t)

;;; Global keymaps
(def *global-keymap* (make-keymap))
(def *ctrl-x-map*   (make-keymap))

(def (make-initial-key-state)
  (make-key-state *global-keymap* []))

;;;============================================================================
;;; Default Emacs-like keybindings
;;;============================================================================

(def (setup-default-bindings!)
  ;; C-x prefix
  (keymap-bind! *global-keymap* "C-x" *ctrl-x-map*)

  ;; Navigation
  (keymap-bind! *global-keymap* "C-f" 'forward-char)
  (keymap-bind! *global-keymap* "C-b" 'backward-char)
  (keymap-bind! *global-keymap* "C-n" 'next-line)
  (keymap-bind! *global-keymap* "C-p" 'previous-line)
  (keymap-bind! *global-keymap* "C-a" 'beginning-of-line)
  (keymap-bind! *global-keymap* "C-e" 'end-of-line)
  (keymap-bind! *global-keymap* "C-v" 'scroll-down)
  (keymap-bind! *global-keymap* "C-l" 'recenter)

  ;; Arrow keys and navigation
  (keymap-bind! *global-keymap* "<up>"     'previous-line)
  (keymap-bind! *global-keymap* "<down>"   'next-line)
  (keymap-bind! *global-keymap* "<left>"   'backward-char)
  (keymap-bind! *global-keymap* "<right>"  'forward-char)
  (keymap-bind! *global-keymap* "<home>"   'beginning-of-line)
  (keymap-bind! *global-keymap* "<end>"    'end-of-line)
  (keymap-bind! *global-keymap* "<prior>"  'scroll-up)
  (keymap-bind! *global-keymap* "<next>"   'scroll-down)
  (keymap-bind! *global-keymap* "<delete>" 'delete-char)

  ;; Alt/Meta navigation
  (keymap-bind! *global-keymap* "M-f" 'forward-word)
  (keymap-bind! *global-keymap* "M-b" 'backward-word)
  (keymap-bind! *global-keymap* "M-v" 'scroll-up)
  (keymap-bind! *global-keymap* "M-<" 'beginning-of-buffer)
  (keymap-bind! *global-keymap* "M->" 'end-of-buffer)

  ;; Editing
  (keymap-bind! *global-keymap* "C-d" 'delete-char)
  (keymap-bind! *global-keymap* "DEL" 'backward-delete-char)
  (keymap-bind! *global-keymap* "C-h" 'backward-delete-char)
  (keymap-bind! *global-keymap* "C-k" 'kill-line)
  (keymap-bind! *global-keymap* "C-y" 'yank)
  (keymap-bind! *global-keymap* "C-w" 'kill-region)
  (keymap-bind! *global-keymap* "M-w" 'copy-region)
  (keymap-bind! *global-keymap* "C-_" 'undo)
  (keymap-bind! *global-keymap* "C-m" 'newline)
  (keymap-bind! *global-keymap* "C-j" 'newline)
  (keymap-bind! *global-keymap* "C-o" 'open-line)

  ;; Mark
  (keymap-bind! *global-keymap* "C-@" 'set-mark)

  ;; Search
  (keymap-bind! *global-keymap* "C-s" 'search-forward)
  (keymap-bind! *global-keymap* "C-r" 'search-backward)

  ;; Misc
  (keymap-bind! *global-keymap* "C-g" 'keyboard-quit)

  ;; C-x commands
  (keymap-bind! *ctrl-x-map* "C-s" 'save-buffer)
  (keymap-bind! *ctrl-x-map* "C-f" 'find-file)
  (keymap-bind! *ctrl-x-map* "C-c" 'quit)
  (keymap-bind! *ctrl-x-map* "b"   'switch-buffer)
  (keymap-bind! *ctrl-x-map* "k"   'kill-buffer-cmd)
  (keymap-bind! *ctrl-x-map* "2"   'split-window)
  (keymap-bind! *ctrl-x-map* "o"   'other-window)
  (keymap-bind! *ctrl-x-map* "0"   'delete-window)
  (keymap-bind! *ctrl-x-map* "1"   'delete-other-windows))

;;;============================================================================
;;; Echo state
;;;============================================================================

(defstruct echo-state
  (message   ; string or #f
   error?)   ; boolean: is message an error?
  transparent: #t)

(def (make-initial-echo-state)
  (make-echo-state #f #f))

(def (echo-message! echo msg)
  (set! (echo-state-message echo) msg)
  (set! (echo-state-error? echo) #f))

(def (echo-error! echo msg)
  (set! (echo-state-message echo) msg)
  (set! (echo-state-error? echo) #t))

(def (echo-clear! echo)
  (set! (echo-state-message echo) #f)
  (set! (echo-state-error? echo) #f))

;;;============================================================================
;;; Buffer structure and list
;;;============================================================================

(defstruct buffer
  (name        ; string: display name (e.g. "*scratch*", "foo.txt")
   file-path   ; string or #f: file path if visiting a file
   doc-pointer ; backend-specific document handle (Scintilla doc ptr or QTextDocument*)
   mark        ; integer or #f: mark position for region
   modified    ; boolean
   lexer-lang) ; symbol or #f: lexer language
  transparent: #t)

(def *buffer-list* [])

(def (buffer-list) *buffer-list*)

(def (buffer-list-add! buf)
  (set! *buffer-list* (cons buf *buffer-list*)))

(def (buffer-list-remove! buf)
  (set! *buffer-list*
    (let loop ((bufs *buffer-list*) (acc []))
      (cond
        ((null? bufs) (reverse acc))
        ((eq? (car bufs) buf) (loop (cdr bufs) acc))
        (else (loop (cdr bufs) (cons (car bufs) acc)))))))

(def (buffer-by-name name)
  "Find a buffer by name. Returns #f if not found."
  (let loop ((bufs *buffer-list*))
    (cond
      ((null? bufs) #f)
      ((string=? (buffer-name (car bufs)) name) (car bufs))
      (else (loop (cdr bufs))))))

(def buffer-scratch-name "*scratch*")

;;;============================================================================
;;; App state
;;;============================================================================

(defstruct app-state
  (frame         ; frame struct (backend-specific)
   echo          ; echo-state struct
   key-state     ; key-state struct
   running       ; boolean
   last-search   ; string or #f
   kill-ring)    ; list of killed text strings
  transparent: #t)

(def (new-app-state frame)
  (make-app-state
   frame
   (make-initial-echo-state)
   (make-initial-key-state)
   #t     ; running
   #f     ; last-search
   []))   ; kill-ring

;;;============================================================================
;;; Command registry
;;;============================================================================

(def *commands* (make-hash-table))

(def (register-command! name proc)
  (hash-put! *commands* name proc))

(def (find-command name)
  (hash-get *commands* name))

(def (execute-command! app name)
  (let ((cmd (find-command name)))
    (if cmd
      (cmd app)
      (echo-error! (app-state-echo app)
                   (string-append (symbol->string name) " is undefined")))))

;;;============================================================================
;;; File I/O helpers
;;;============================================================================

(def (read-file-as-string path)
  (call-with-input-file path
    (lambda (port) (read-line port #f))))

(def (write-string-to-file path str)
  (call-with-output-file path
    (lambda (port) (display str port))))
