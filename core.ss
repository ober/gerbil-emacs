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
  keymap-entries
  (struct-out key-state)
  make-initial-key-state
  *global-keymap*
  *ctrl-x-map*
  *meta-g-map*
  *help-map*
  *ctrl-x-r-map*
  *ctrl-c-map*
  *meta-s-map*
  *ctrl-x-4-map*
  *all-commands*
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

  ;; Shared helpers
  brace-char?

  ;; File I/O helpers
  read-file-as-string
  write-string-to-file

  ;; Dired (directory listing) shared logic
  *dired-entries*
  dired-buffer?
  strip-trailing-slash
  dired-format-listing

  ;; REPL shared logic
  repl-buffer?
  *repl-state*
  eval-expression-string)

(import :std/sugar
        :std/sort
        :std/srfi/13)

;;;============================================================================
;;; Keymap data structure
;;;============================================================================

(def (make-keymap)
  (make-hash-table))

(def (keymap-bind! km key-str value)
  (hash-put! km key-str value))

(def (keymap-lookup km key-str)
  (hash-get km key-str))

(def (keymap-entries km)
  "Return list of (key . value) for all entries in a keymap."
  (hash->list km))

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
(def *ctrl-x-r-map* (make-keymap))
(def *ctrl-c-map*   (make-keymap))
(def *meta-g-map*   (make-keymap))
(def *help-map*     (make-keymap))
(def *meta-s-map*   (make-keymap))
(def *ctrl-x-4-map* (make-keymap))

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
  (keymap-bind! *ctrl-x-map* "1"   'delete-other-windows)
  (keymap-bind! *ctrl-x-map* "3"   'split-window-right)

  ;; REPL
  (keymap-bind! *global-keymap* "M-:" 'eval-expression)
  ;; C-c prefix
  (keymap-bind! *global-keymap* "C-c" *ctrl-c-map*)
  (keymap-bind! *ctrl-c-map* "z"   'repl)

  ;; C-x r prefix (registers/bookmarks/rectangles)
  (keymap-bind! *ctrl-x-map* "r"   *ctrl-x-r-map*)
  (keymap-bind! *ctrl-x-r-map* "m" 'bookmark-set)
  (keymap-bind! *ctrl-x-r-map* "b" 'bookmark-jump)
  (keymap-bind! *ctrl-x-r-map* "l" 'bookmark-list)
  (keymap-bind! *ctrl-x-r-map* "k" 'kill-rectangle)
  (keymap-bind! *ctrl-x-r-map* "y" 'yank-rectangle)

  ;; M-x
  (keymap-bind! *global-keymap* "M-x" 'execute-extended-command)

  ;; Goto line (M-g prefix map)
  (keymap-bind! *global-keymap* "M-g" *meta-g-map*)
  (keymap-bind! *meta-g-map* "g"     'goto-line)
  (keymap-bind! *meta-g-map* "M-g"   'goto-line)

  ;; Help (C-h prefix map)
  (keymap-bind! *global-keymap* "C-h" *help-map*)
  (keymap-bind! *help-map* "k"     'describe-key)
  (keymap-bind! *help-map* "b"     'list-bindings)
  (keymap-bind! *help-map* "f"     'describe-command)

  ;; Buffer list
  (keymap-bind! *ctrl-x-map* "C-b" 'list-buffers)

  ;; Query replace
  (keymap-bind! *global-keymap* "M-%" 'query-replace)

  ;; Tab
  (keymap-bind! *global-keymap* "TAB" 'indent-or-complete)

  ;; Eshell (C-c e, since C-x e is call-last-kbd-macro)
  (keymap-bind! *ctrl-c-map* "e"   'eshell)

  ;; Shell
  (keymap-bind! *ctrl-x-map* "s"   'shell)

  ;; Redo
  (keymap-bind! *global-keymap* "M-_" 'redo)

  ;; Toggle line numbers
  (keymap-bind! *ctrl-x-map* "l"   'toggle-line-numbers)

  ;; Toggle word wrap
  (keymap-bind! *ctrl-x-map* "w"   'toggle-word-wrap)

  ;; Toggle whitespace
  (keymap-bind! *ctrl-x-map* "t"   'toggle-whitespace)

  ;; Zoom
  (keymap-bind! *global-keymap* "C-=" 'zoom-in)
  (keymap-bind! *global-keymap* "C--" 'zoom-out)
  (keymap-bind! *ctrl-x-map* "C-0" 'zoom-reset)

  ;; Select all
  (keymap-bind! *ctrl-x-map* "h"   'select-all)

  ;; Duplicate line
  (keymap-bind! *ctrl-x-map* "d"   'duplicate-line)

  ;; Comment toggle
  (keymap-bind! *global-keymap* "M-;" 'toggle-comment)

  ;; Transpose chars
  (keymap-bind! *global-keymap* "C-t" 'transpose-chars)

  ;; Upcase / downcase word
  (keymap-bind! *global-keymap* "M-u" 'upcase-word)
  (keymap-bind! *global-keymap* "M-l" 'downcase-word)
  (keymap-bind! *global-keymap* "M-c" 'capitalize-word)

  ;; Kill word
  (keymap-bind! *global-keymap* "M-d" 'kill-word)

  ;; What line
  (keymap-bind! *meta-g-map* "l"   'what-line)

  ;; Write file (save as)
  (keymap-bind! *ctrl-x-map* "C-w" 'write-file)

  ;; Revert buffer
  (keymap-bind! *ctrl-x-map* "C-r" 'revert-buffer)

  ;; Beginning/end of defun
  (keymap-bind! *global-keymap* "M-a" 'beginning-of-defun)
  (keymap-bind! *global-keymap* "M-e" 'end-of-defun)

  ;; Count words
  (keymap-bind! *global-keymap* "M-=" 'count-words)

  ;; Yank-pop (rotate kill ring)
  (keymap-bind! *global-keymap* "M-y" 'yank-pop)

  ;; Occur (search prefix M-s)
  (keymap-bind! *global-keymap* "M-s" *meta-s-map*)
  (keymap-bind! *meta-s-map* "o" 'occur)

  ;; Compile
  (keymap-bind! *ctrl-x-map* "c" 'compile)

  ;; Pipe region to shell
  (keymap-bind! *global-keymap* "M-|" 'shell-command-on-region)

  ;; Sort lines in region
  (keymap-bind! *ctrl-c-map* "^" 'sort-lines)

  ;; Go to matching paren
  (keymap-bind! *ctrl-c-map* "p" 'goto-matching-paren)

  ;; Join lines
  (keymap-bind! *global-keymap* "M-j" 'join-line)

  ;; Delete blank lines
  (keymap-bind! *ctrl-x-map* "C-o" 'delete-blank-lines)

  ;; Indent region
  (keymap-bind! *ctrl-c-map* "TAB" 'indent-region)

  ;; Downcase/upcase region
  (keymap-bind! *ctrl-x-map* "C-l" 'downcase-region)
  (keymap-bind! *ctrl-x-map* "C-u" 'upcase-region)

  ;; Shell command
  (keymap-bind! *global-keymap* "M-!" 'shell-command)

  ;; Fill paragraph
  (keymap-bind! *global-keymap* "M-q" 'fill-paragraph)

  ;; Insert file
  (keymap-bind! *ctrl-x-map* "i" 'insert-file)

  ;; Dynamic abbreviation
  (keymap-bind! *global-keymap* "M-/" 'dabbrev-expand)

  ;; What cursor position
  (keymap-bind! *ctrl-x-map* "=" 'what-cursor-position)

  ;; Keyboard macros
  (keymap-bind! *ctrl-x-map* "(" 'start-kbd-macro)
  (keymap-bind! *ctrl-x-map* ")" 'end-kbd-macro)
  (keymap-bind! *ctrl-x-map* "e" 'call-last-kbd-macro)

  ;; Mark ring
  (keymap-bind! *ctrl-c-map* "SPC" 'pop-mark)

  ;; Registers (save/insert text, point to register)
  (keymap-bind! *ctrl-x-r-map* "s" 'copy-to-register)
  (keymap-bind! *ctrl-x-r-map* "i" 'insert-register)
  (keymap-bind! *ctrl-x-r-map* "SPC" 'point-to-register)
  (keymap-bind! *ctrl-x-r-map* "j" 'jump-to-register)

  ;; Backward kill word
  (keymap-bind! *global-keymap* "M-DEL" 'backward-kill-word)

  ;; Zap to char
  (keymap-bind! *global-keymap* "M-z" 'zap-to-char)

  ;; Go to char position (M-g c)
  (keymap-bind! *meta-g-map* "c" 'goto-char)

  ;; Transpose words (M-t)
  (keymap-bind! *global-keymap* "M-t" 'transpose-words)

  ;; Transpose lines (C-x C-t)
  (keymap-bind! *ctrl-x-map* "C-t" 'transpose-lines)

  ;; Repeat last command (C-x z)
  (keymap-bind! *ctrl-x-map* "z" 'repeat)

  ;; Just one space (M-SPC)
  (keymap-bind! *global-keymap* "M-SPC" 'just-one-space)

  ;; Delete indentation (M-^)  -- note: M-^ was sort-lines, use C-c j instead
  ;; Already have M-j for join-line, keep M-^ for sort-lines

  ;; Goto next/previous error (M-g n / M-g p)
  (keymap-bind! *meta-g-map* "n" 'next-error)
  (keymap-bind! *meta-g-map* "p" 'previous-error)

  ;; Kill whole line
  (keymap-bind! *ctrl-c-map* "k" 'kill-whole-line)

  ;; Move line up/down (Alt+arrows)
  (keymap-bind! *global-keymap* "M-<up>" 'move-line-up)
  (keymap-bind! *global-keymap* "M-<down>" 'move-line-down)

  ;; Pipe buffer to shell
  (keymap-bind! *ctrl-c-map* "!" 'pipe-buffer)

  ;; Narrow to region / widen
  (keymap-bind! *ctrl-c-map* "n" 'narrow-to-region)
  (keymap-bind! *ctrl-c-map* "w" 'widen)

  ;; String insert
  (keymap-bind! *ctrl-c-map* "i" 'string-insert-file)

  ;; Rectangle: string-rectangle, open-rectangle
  (keymap-bind! *ctrl-x-r-map* "t" 'string-rectangle)
  (keymap-bind! *ctrl-x-r-map* "o" 'open-rectangle)

  ;; Number lines, reverse region
  (keymap-bind! *ctrl-c-map* "#" 'number-lines)
  (keymap-bind! *ctrl-c-map* "r" 'reverse-region)

  ;; Flush/keep lines
  (keymap-bind! *meta-s-map* "f" 'flush-lines)
  (keymap-bind! *meta-s-map* "k" 'keep-lines)

  ;; Align regexp
  (keymap-bind! *ctrl-c-map* "a" 'align-regexp)

  ;; Sort fields
  (keymap-bind! *ctrl-c-map* "s" 'sort-fields)

  ;; Mark word, mark paragraph, paragraph navigation
  (keymap-bind! *global-keymap* "M-@" 'mark-word)
  (keymap-bind! *global-keymap* "M-h" 'mark-paragraph)
  (keymap-bind! *global-keymap* "M-}" 'forward-paragraph)
  (keymap-bind! *global-keymap* "M-{" 'backward-paragraph)

  ;; Back to indentation, delete indentation
  (keymap-bind! *global-keymap* "M-m" 'back-to-indentation)
  (keymap-bind! *global-keymap* "M-^" 'delete-indentation)

  ;; Exchange point and mark
  (keymap-bind! *ctrl-x-map* "C-x" 'exchange-point-and-mark)

  ;; Info
  (keymap-bind! *ctrl-x-map* "C-p" 'what-page)
  (keymap-bind! *ctrl-c-map* "l" 'count-lines-region)

  ;; Copy line (C-c c)
  (keymap-bind! *ctrl-c-map* "c" 'copy-line)

  ;; Help: where-is, apropos
  (keymap-bind! *help-map* "w" 'where-is)
  (keymap-bind! *help-map* "a" 'apropos-command)

  ;; Buffer: toggle-read-only, rename-buffer
  (keymap-bind! *ctrl-x-map* "C-q" 'toggle-read-only)
  (keymap-bind! *ctrl-x-r-map* "n" 'rename-buffer)

  ;; Other-window commands (C-x 4 prefix)
  (keymap-bind! *ctrl-x-map* "4" *ctrl-x-4-map*)
  (keymap-bind! *ctrl-x-4-map* "b" 'switch-buffer-other-window)
  (keymap-bind! *ctrl-x-4-map* "f" 'find-file-other-window)

  ;; Text transforms
  (keymap-bind! *ctrl-c-map* "t" 'tabify)
  (keymap-bind! *ctrl-c-map* "3" 'rot13-region)
  (keymap-bind! *ctrl-c-map* "x" 'hexl-mode)

  ;; Count matches, dedup
  (keymap-bind! *meta-s-map* "c" 'count-matches)
  (keymap-bind! *ctrl-c-map* "u" 'delete-duplicate-lines))

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
   kill-ring     ; list of killed text strings
   kill-ring-idx ; integer: index into kill-ring for yank-pop rotation
   last-yank-pos ; integer or #f: position where last yank was inserted
   last-yank-len ; integer or #f: length of last yanked text
   last-compile  ; string or #f: last compile command
   bookmarks     ; hash-table: name -> (buffer-name . position)
   rect-kill     ; list of strings (rectangle kill ring)
   dabbrev-state ; list or #f: (prefix matches-remaining last-pos last-len)
   macro-recording ; list or #f: list of (action . data) being recorded
   macro-last    ; list or #f: last recorded macro
   mark-ring     ; list of (buffer-name . position) for mark history
   registers     ; hash-table: char -> string or (buffer-name . position)
   last-command  ; symbol or #f: name of last executed command
   key-handler)  ; procedure or #f: (lambda (editor) ...) installs key handler on editor
  transparent: #t)

(def (new-app-state frame)
  (make-app-state
   frame
   (make-initial-echo-state)
   (make-initial-key-state)
   #t                    ; running
   #f                    ; last-search
   []                    ; kill-ring
   0                     ; kill-ring-idx
   #f                    ; last-yank-pos
   #f                    ; last-yank-len
   #f                    ; last-compile
   (make-hash-table)     ; bookmarks
   []                    ; rect-kill
   #f                    ; dabbrev-state
   #f                    ; macro-recording
   #f                    ; macro-last
   []                    ; mark-ring
   (make-hash-table)     ; registers
   #f                    ; last-command
   #f))                  ; key-handler

;;;============================================================================
;;; Command registry
;;;============================================================================

(def *commands* (make-hash-table))

;; Alias for external access to command table
(def *all-commands* *commands*)

(def (register-command! name proc)
  (hash-put! *commands* name proc))

(def (find-command name)
  (hash-get *commands* name))

(def (execute-command! app name)
  (let ((cmd (find-command name)))
    (if cmd
      (begin
        (set! (app-state-last-command app) name)
        (cmd app))
      (echo-error! (app-state-echo app)
                   (string-append (symbol->string name) " is undefined")))))

;;;============================================================================
;;; Shared helpers
;;;============================================================================

(def (brace-char? ch)
  "Check if a character code represents a brace/paren/bracket."
  (or (= ch 40) (= ch 41)    ; ( )
      (= ch 91) (= ch 93)    ; [ ]
      (= ch 123) (= ch 125))) ; { }

;;;============================================================================
;;; File I/O helpers
;;;============================================================================

(def (read-file-as-string path)
  (call-with-input-file path
    (lambda (port) (read-line port #f))))

(def (write-string-to-file path str)
  (call-with-output-file path
    (lambda (port) (display str port))))

;;;============================================================================
;;; Dired (directory listing) shared logic
;;;============================================================================

;; Maps dired buffers to their entries vectors (index → full-path)
(def *dired-entries* (make-hash-table))

(def (dired-buffer? buf)
  "Check if this buffer is a dired (directory listing) buffer."
  (eq? (buffer-lexer-lang buf) 'dired))

(def (strip-trailing-slash path)
  (if (and (> (string-length path) 1)
           (char=? (string-ref path (- (string-length path) 1)) #\/))
    (substring path 0 (- (string-length path) 1))
    path))

(def (mode->permission-string mode)
  "Convert file permission bits to rwxrwxrwx string."
  (let ((p (bitwise-and mode #o777)))
    (string
      (if (not (zero? (bitwise-and p #o400))) #\r #\-)
      (if (not (zero? (bitwise-and p #o200))) #\w #\-)
      (if (not (zero? (bitwise-and p #o100))) #\x #\-)
      (if (not (zero? (bitwise-and p #o040))) #\r #\-)
      (if (not (zero? (bitwise-and p #o020))) #\w #\-)
      (if (not (zero? (bitwise-and p #o010))) #\x #\-)
      (if (not (zero? (bitwise-and p #o004))) #\r #\-)
      (if (not (zero? (bitwise-and p #o002))) #\w #\-)
      (if (not (zero? (bitwise-and p #o001))) #\x #\-))))

(def (format-size size)
  "Right-align size in 8-char field."
  (let ((s (number->string size)))
    (string-append (make-string (max 0 (- 8 (string-length s))) #\space) s)))

(def (dired-format-entry dir name)
  "Format one dired line for a file/directory entry."
  (let ((full (if (string=? name "..")
                (strip-trailing-slash (path-directory dir))
                (string-append dir "/" name))))
    (with-catch
      (lambda (e)
        (string-append "  ?????????? " (make-string 8 #\?) " " name))
      (lambda ()
        (let* ((info (file-info full))
               (type (file-info-type info))
               (mode (file-info-mode info))
               (size (file-info-size info))
               (type-char (case type
                            ((directory) #\d)
                            ((symbolic-link) #\l)
                            (else #\-)))
               (perms (mode->permission-string mode))
               (display-name (if (eq? type 'directory)
                               (string-append name "/")
                               name)))
          (string-append "  " (string type-char) perms " "
                         (format-size size) " " display-name))))))

(def (dired-format-listing dir)
  "Format a directory listing.
   Returns (values text entries-vector).
   entries-vector maps index i to the full path of entry at line (i + 3)."
  (let* ((raw-entries (directory-files
                        (list path: dir ignore-hidden: 'dot-and-dot-dot)))
         (entries (sort raw-entries string<?))
         ;; Separate directories and files, dirs first
         (dirs (filter (lambda (name)
                         (with-catch
                           (lambda (e) #f)
                           (lambda ()
                             (eq? 'directory
                                  (file-info-type
                                   (file-info (string-append dir "/" name)))))))
                       entries))
         (files (filter (lambda (name)
                          (with-catch
                            (lambda (e) #t)
                            (lambda ()
                              (not (eq? 'directory
                                        (file-info-type
                                         (file-info (string-append dir "/" name))))))))
                        entries))
         ;; ".." first, then dirs, then files
         (ordered (append '("..") dirs files))
         ;; Format lines
         (header (string-append "  " dir ":"))
         (total-line (string-append "  " (number->string (length entries))
                                    " entries"))
         (entry-lines (map (lambda (name) (dired-format-entry dir name))
                           ordered))
         (all-lines (append (list header total-line "") entry-lines))
         (text (string-join all-lines "\n"))
         ;; Build entries vector: index i → full path
         (paths (list->vector
                  (map (lambda (name)
                         (if (string=? name "..")
                           (strip-trailing-slash (path-directory dir))
                           (string-append dir "/" name)))
                       ordered))))
    (values text paths)))

;;;============================================================================
;;; REPL shared logic
;;;============================================================================

(def (repl-buffer? buf)
  "Check if this buffer is a REPL buffer."
  (eq? (buffer-lexer-lang buf) 'repl))

;; Maps REPL buffers to their repl-state structs
(def *repl-state* (make-hash-table))

(def (eval-expression-string str)
  "In-process eval: read+eval an expression string, capture output.
   Returns (values result-string error?)."
  (with-catch
    (lambda (e)
      (values (with-output-to-string (lambda () (display-exception e))) #t))
    (lambda ()
      (let* ((expr (with-input-from-string str read))
             (result (eval expr))
             (output (with-output-to-string (lambda () (write result)))))
        (values output #f)))))
