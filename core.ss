;;; -*- Gerbil -*-
;;; Shared core for gemacs
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
  *ctrl-c-l-map*
  *ctrl-c-m-map*
  *lsp-server-command*
  *meta-s-map*
  *ctrl-x-4-map*
  *all-commands*
  setup-default-bindings!

  ;; App state
  (struct-out app-state)
  new-app-state
  get-prefix-arg

  ;; Command registry
  register-command!
  find-command
  execute-command!
  *command-docs*
  register-command-doc!
  command-doc
  command-name->description
  find-keybinding-for-command
  setup-command-docs!

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

  ;; Key lossage
  key-lossage-record!
  key-lossage->string

  ;; Shared helpers
  brace-char?

  ;; Hooks
  *post-buffer-attach-hook*

  ;; File I/O helpers
  read-file-as-string
  write-string-to-file

  ;; Dired (directory listing) shared logic
  *dired-entries*
  dired-buffer?
  strip-trailing-slash
  dired-format-listing

  ;; Runtime error log file
  init-gemacs-log!
  gemacs-log!

  ;; Captured output logs
  append-error-log! append-output-log!
  get-error-log get-output-log
  clear-error-log! clear-output-log!
  has-captured-output?

  ;; REPL shared logic
  repl-buffer?
  *repl-state*
  eval-expression-string
  ensure-gerbil-eval!
  load-user-file!
  load-user-string!

  ;; Fuzzy matching
  fuzzy-match?
  fuzzy-score
  fuzzy-filter-sort

  ;; Key translation map
  *key-translation-map*
  key-translate!
  key-translate-char

  ;; Key-chord system
  *chord-map*
  *chord-first-chars*
  *chord-timeout*
  *chord-mode*
  key-chord-define-global
  chord-lookup
  chord-start-char?

  ;; Image buffer support
  *editor-window-map*
  *image-buffer-state*
  image-buffer?

  ;; Face system (from :gemacs/face)
  (struct-out face)
  new-face
  *faces*
  define-face!
  face-get
  face-ref
  set-face-attribute!
  face-clear!
  *default-font-family*
  *default-font-size*
  set-default-font!
  get-default-font
  parse-hex-color
  rgb->hex
  define-standard-faces!
  ;; Init file convenience API
  set-frame-font

  ;; Theme system (from :gemacs/themes)
  *themes*
  register-theme!
  theme-get
  theme-names
  theme-dark
  theme-light
  theme-solarized-dark
  theme-solarized-light
  theme-monokai
  theme-gruvbox-dark
  theme-gruvbox-light
  theme-dracula
  theme-nord
  theme-zenburn

  ;; Paredit strict mode
  *paredit-strict-mode*)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil/runtime/init
        :gerbil/expander
        :gemacs/face
        :gemacs/themes)

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
(def *ctrl-c-l-map* (make-keymap))
(def *ctrl-c-m-map* (make-keymap))
(def *lsp-server-command* "gerbil-lsp")  ;; overridable via ~/.gemacs-init
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
  (keymap-bind! *global-keymap* "C-l" 'recenter-top-bottom)

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
  (keymap-bind! *global-keymap* "C-/" 'undo)
  (keymap-bind! *global-keymap* "C-m" 'newline)
  (keymap-bind! *global-keymap* "C-j" 'newline)
  (keymap-bind! *global-keymap* "C-o" 'open-line)

  ;; Mark
  (keymap-bind! *global-keymap* "C-@" 'set-mark)

  ;; Search
  (keymap-bind! *global-keymap* "C-s" 'search-forward)
  (keymap-bind! *global-keymap* "C-r" 'search-backward)

  ;; Function keys
  (keymap-bind! *global-keymap* "<f11>" 'uncomment-region)
  (keymap-bind! *global-keymap* "<f12>" 'comment-region)

  ;; Universal argument
  (keymap-bind! *global-keymap* "C-u" 'universal-argument)

  ;; Digit arguments
  (keymap-bind! *global-keymap* "M-0" 'digit-argument-0)
  (keymap-bind! *global-keymap* "M-1" 'digit-argument-1)
  (keymap-bind! *global-keymap* "M-2" 'digit-argument-2)
  (keymap-bind! *global-keymap* "M-3" 'digit-argument-3)
  (keymap-bind! *global-keymap* "M-4" 'digit-argument-4)
  (keymap-bind! *global-keymap* "M-5" 'digit-argument-5)
  (keymap-bind! *global-keymap* "M-6" 'digit-argument-6)
  (keymap-bind! *global-keymap* "M-7" 'digit-argument-7)
  (keymap-bind! *global-keymap* "M-8" 'digit-argument-8)
  (keymap-bind! *global-keymap* "M-9" 'digit-argument-9)
  (keymap-bind! *global-keymap* "M--" 'negative-argument)

  ;; Misc
  (keymap-bind! *global-keymap* "C-g" 'keyboard-quit)

  ;; C-x commands
  (keymap-bind! *ctrl-x-map* "C-s" 'save-buffer)
  (keymap-bind! *ctrl-x-map* "C-f" 'find-file)
  (keymap-bind! *ctrl-x-map* "C-r" 'recentf-open)
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
  (keymap-bind! *ctrl-x-r-map* "d" 'delete-rectangle)
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

  ;; Regex search and replace
  (keymap-bind! *global-keymap* "C-M-s" 'isearch-forward-regexp)
  (keymap-bind! *global-keymap* "C-M-%" 'query-replace-regexp)
  ;; Completion popup (Scintilla autocomplete)
  (keymap-bind! *global-keymap* "C-M-i" 'complete-at-point)

  ;; Tab
  (keymap-bind! *global-keymap* "TAB" 'indent-or-complete)

  ;; Eshell (C-c e, since C-x e is call-last-kbd-macro)
  (keymap-bind! *ctrl-c-map* "e"   'eshell)

  ;; Shell (C-c $ — C-x s is save-some-buffers)
  (keymap-bind! *ctrl-c-map* "$"   'shell)

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

  ;; Revert buffer — accessible via M-x revert-buffer or C-c R (revert-buffer-quick)

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

  ;; Xref (go to definition / back)
  (keymap-bind! *global-keymap* "M-." 'goto-definition)
  (keymap-bind! *global-keymap* "M-," 'xref-back)

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
  (keymap-bind! *ctrl-x-r-map* "w" 'window-configuration-to-register)
  (keymap-bind! *ctrl-x-r-map* "f" 'file-to-register)

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

  ;; Goto next/previous error (M-g n / M-g p / M-g M-n / M-g M-p)
  (keymap-bind! *meta-g-map* "n" 'next-error)
  (keymap-bind! *meta-g-map* "p" 'previous-error)
  (keymap-bind! *meta-g-map* "M-n" 'next-error)
  (keymap-bind! *meta-g-map* "M-p" 'previous-error)

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
  ;; C-c l → LSP prefix map (replaces count-lines-region)
  (keymap-bind! *ctrl-c-map* "l" *ctrl-c-l-map*)

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
  (keymap-bind! *ctrl-c-map* "u" 'delete-duplicate-lines)

  ;; Diff buffer, checksum
  (keymap-bind! *ctrl-c-map* "d" 'diff-buffer-with-file)
  (keymap-bind! *ctrl-c-map* "5" 'checksum)

  ;; Async shell command
  (keymap-bind! *global-keymap* "M-&" 'async-shell-command)

  ;; Toggle truncate lines
  (keymap-bind! *ctrl-x-map* "$" 'toggle-truncate-lines)

  ;; Grep buffer
  (keymap-bind! *meta-s-map* "g" 'grep-buffer)

  ;; Insert date, insert char
  (keymap-bind! *ctrl-c-map* "D" 'insert-date)
  (keymap-bind! *ctrl-c-map* "8" 'insert-char)

  ;; Eval buffer/region/sexp
  (keymap-bind! *ctrl-c-map* "E" 'eval-buffer)
  (keymap-bind! *ctrl-c-map* "v" 'eval-region)
  (keymap-bind! *ctrl-x-map* "C-e" 'eval-last-sexp)
  (keymap-bind! *ctrl-c-map* "C-e" 'eval-last-sexp)
  (keymap-bind! *ctrl-c-map* "C-d" 'eval-defun)

  ;; Org-mode (C-c prefix, standard Emacs bindings)
  (keymap-bind! *ctrl-c-map* "C-n" 'org-next-heading)
  (keymap-bind! *ctrl-c-map* "C-p" 'org-prev-heading)
  (keymap-bind! *ctrl-c-map* "C-t" 'org-todo)
  (keymap-bind! *ctrl-c-map* "C-l" 'org-link)
  (keymap-bind! *ctrl-c-map* "C-o" 'org-open-at-point)
  (keymap-bind! *ctrl-c-map* "C-s" 'org-schedule)
  (keymap-bind! *ctrl-c-map* "C-q" 'org-set-tags)
  (keymap-bind! *ctrl-c-map* ","   'org-priority)

  ;; Clone buffer, scratch
  (keymap-bind! *ctrl-c-map* "b" 'clone-buffer)
  (keymap-bind! *ctrl-c-map* "S" 'scratch-buffer)

  ;; Save some buffers
  (keymap-bind! *ctrl-x-map* "s" 'save-some-buffers)  ; overrides shell

  ;; Revert quick
  (keymap-bind! *ctrl-c-map* "R" 'revert-buffer-quick)

  ;; Toggle highlighting
  (keymap-bind! *ctrl-c-map* "f" 'toggle-highlighting)

  ;; Display time
  (keymap-bind! *ctrl-c-map* "T" 'display-time)

  ;; Calculator
  (keymap-bind! *ctrl-c-map* "=" 'calc)

  ;; Case fold search toggle
  (keymap-bind! *ctrl-c-map* "C" 'toggle-case-fold-search)

  ;; Describe bindings (C-h B — uppercase to avoid collision with C-h b list-bindings)
  (keymap-bind! *help-map* "B" 'describe-bindings)

  ;; Center line
  (keymap-bind! *global-keymap* "M-o" 'center-line)

  ;; What face
  (keymap-bind! *ctrl-c-map* "F" 'what-face)

  ;; List processes
  (keymap-bind! *ctrl-c-map* "P" 'list-processes)

  ;; View messages
  (keymap-bind! *ctrl-c-map* "m" 'view-messages)

  ;; Auto fill toggle
  (keymap-bind! *ctrl-c-map* "q" 'toggle-auto-fill)

  ;; Delete trailing whitespace
  (keymap-bind! *ctrl-c-map* "W" 'delete-trailing-whitespace)

  ;; Ediff buffers
  (keymap-bind! *ctrl-c-map* "B" 'ediff-buffers)

  ;; Rename/delete file
  (keymap-bind! *ctrl-c-map* "M" 'rename-file-and-buffer)

  ;; Sudo write
  (keymap-bind! *ctrl-c-map* "X" 'sudo-write)

  ;; Sort numeric
  (keymap-bind! *ctrl-c-map* "N" 'sort-numeric)

  ;; Count words region
  (keymap-bind! *ctrl-c-map* "L" 'count-words-region)

  ;; Overwrite mode (Insert key)
  (keymap-bind! *global-keymap* "<insert>" 'toggle-overwrite-mode)

  ;; Visual line mode
  (keymap-bind! *ctrl-c-map* "V" 'toggle-visual-line-mode)

  ;; Fill column
  (keymap-bind! *ctrl-c-map* "." 'set-fill-column)
  (keymap-bind! *ctrl-c-map* "|" 'toggle-fill-column-indicator)

  ;; Repeat complex command (C-x ESC ESC in real Emacs, use C-c Z)
  (keymap-bind! *ctrl-c-map* "Z" 'repeat-complex-command)

  ;; Eldoc
  (keymap-bind! *ctrl-c-map* "I" 'eldoc)

  ;; Highlight symbol / clear
  (keymap-bind! *ctrl-c-map* "h" 'highlight-symbol)
  (keymap-bind! *ctrl-c-map* "H" 'clear-highlight)

  ;; Indent rigidly
  (keymap-bind! *ctrl-c-map* ">" 'indent-rigidly-right)
  (keymap-bind! *ctrl-c-map* "<" 'indent-rigidly-left)

  ;; Buffer stats
  (keymap-bind! *ctrl-c-map* "?" 'buffer-stats)

  ;; Show tabs/eol
  (keymap-bind! *ctrl-c-map* "4" 'toggle-show-tabs)
  (keymap-bind! *ctrl-c-map* "6" 'toggle-show-eol)

  ;; Copy from above/below
  (keymap-bind! *ctrl-c-map* "A" 'copy-from-above)

  ;; Open line above
  (keymap-bind! *ctrl-c-map* "O" 'open-line-above)

  ;; Select line
  (keymap-bind! *ctrl-c-map* "G" 'select-line)

  ;; Split line
  (keymap-bind! *ctrl-c-map* "J" 'split-line)

  ;; Hippie expand (M-TAB — alternative to M-/)
  (keymap-bind! *global-keymap* "M-TAB" 'hippie-expand)

  ;; Swap buffers between windows
  (keymap-bind! *ctrl-c-map* "9" 'swap-buffers)

  ;; Tab width cycle
  (keymap-bind! *ctrl-c-map* "7" 'cycle-tab-width)

  ;; Indent tabs mode
  (keymap-bind! *ctrl-c-map* "0" 'toggle-indent-tabs-mode)

  ;; Buffer info (C-c i)
  (keymap-bind! *ctrl-c-map* "j" 'buffer-info)

  ;; Enlarge/shrink window
  (keymap-bind! *ctrl-x-map* "^" 'enlarge-window)
  (keymap-bind! *ctrl-x-map* "-" 'shrink-window)

  ;; Whitespace cleanup
  (keymap-bind! *ctrl-c-map* "c" 'whitespace-cleanup)  ; overrides copy-line

  ;; Toggle electric pair (auto-pair brackets)
  (keymap-bind! *ctrl-c-map* "Q" 'toggle-electric-pair)

  ;; Previous/next buffer (C-x <left> / C-x <right>)
  (keymap-bind! *ctrl-x-map* "<left>" 'previous-buffer)
  (keymap-bind! *ctrl-x-map* "<right>" 'next-buffer)

  ;; Balance windows
  (keymap-bind! *ctrl-x-map* "+" 'balance-windows)

  ;; Window resize (C-x ^, C-x {, C-x })
  (keymap-bind! *ctrl-x-map* "^" 'enlarge-window)
  (keymap-bind! *ctrl-x-map* "{" 'shrink-window-horizontally)
  (keymap-bind! *ctrl-x-map* "}" 'enlarge-window-horizontally)

  ;; Move to window line (M-r — cycle top/center/bottom)
  (keymap-bind! *global-keymap* "M-r" 'move-to-window-line)

  ;; Kill buffer and window (C-x 4 0)
  (keymap-bind! *ctrl-x-4-map* "0" 'kill-buffer-and-window)

  ;; Flush undo history
  (keymap-bind! *ctrl-c-map* "/" 'flush-undo)

  ;; Upcase initials (titlecase) region
  (keymap-bind! *ctrl-c-map* "U" 'upcase-initials-region)

  ;; Untabify buffer
  (keymap-bind! *ctrl-c-map* "_" 'untabify-buffer)

  ;; Insert buffer name at point
  (keymap-bind! *ctrl-c-map* "%" 'insert-buffer-name)

  ;; Mark defun
  (keymap-bind! *ctrl-c-map* "y" 'mark-defun)

  ;; Insert pairs
  (keymap-bind! *ctrl-c-map* "(" 'insert-parentheses)
  (keymap-bind! *ctrl-c-map* "[" 'insert-pair-brackets)

  ;; Describe char at point
  (keymap-bind! *ctrl-c-map* "," 'describe-char)

  ;; Find file at point (C-c C-f to avoid overriding set-fill-column on C-c .)
  (keymap-bind! *ctrl-c-map* "o" 'find-file-at-point)

  ;; Count chars in region
  (keymap-bind! *ctrl-c-map* "K" 'count-chars-region)

  ;; Count words in entire buffer
  (keymap-bind! *ctrl-c-map* "+" 'count-words-buffer)

  ;; Unfill paragraph
  (keymap-bind! *ctrl-c-map* ";" 'unfill-paragraph)

  ;; List registers
  (keymap-bind! *ctrl-c-map* "@" 'list-registers)

  ;; Show kill ring
  (keymap-bind! *ctrl-c-map* "Y" 'show-kill-ring)

  ;; Smart beginning of line (C-a override — goes to indentation first)
  ;; Keep original C-a as beginning-of-line; use M-m for back-to-indentation
  ;; Add C-c C-a won't work; instead use C-c `
  (keymap-bind! *ctrl-c-map* "`" 'smart-beginning-of-line)

  ;; What buffer
  (keymap-bind! *ctrl-c-map* "~" 'what-buffer)

  ;; Narrowing indicator
  (keymap-bind! *ctrl-c-map* ":" 'toggle-narrowing-indicator)

  ;; Insert file name/path
  (keymap-bind! *ctrl-c-map* "&" 'insert-file-name)

  ;; S-expression navigation (C-M-* style — use M-g prefix for accessibility)
  (keymap-bind! *meta-g-map* "u" 'backward-up-list)
  (keymap-bind! *meta-g-map* "d" 'forward-up-list)
  (keymap-bind! *meta-g-map* "k" 'kill-sexp)
  (keymap-bind! *meta-g-map* "f" 'forward-sexp)
  (keymap-bind! *meta-g-map* "b" 'backward-sexp)

  ;; Mark sexp
  (keymap-bind! *meta-g-map* "SPC" 'mark-sexp)

  ;; Indent sexp
  (keymap-bind! *meta-g-map* "TAB" 'indent-sexp)

  ;; Word frequency analysis
  (keymap-bind! *ctrl-c-map* "*" 'word-frequency)

  ;; Insert UUID
  (keymap-bind! *ctrl-c-map* "'" 'insert-uuid)

  ;; Delete pair (surrounding delimiters)
  (keymap-bind! *ctrl-c-map* "}" 'delete-pair)

  ;; Toggle caret line highlight
  (keymap-bind! *ctrl-c-map* "{" 'toggle-hl-line)

  ;; Find alternate file
  (keymap-bind! *ctrl-x-map* "C-v" 'find-alternate-file)

  ;; Increment register
  (keymap-bind! *ctrl-x-r-map* "+" 'increment-register)

  ;; Copy buffer name to kill ring (via M-x or explicit binding)
  ;; No keybinding — accessible via M-x copy-buffer-name

  ;; Scroll other window (M-g v / M-g V)
  (keymap-bind! *meta-g-map* "v" 'scroll-other-window)
  (keymap-bind! *meta-g-map* "V" 'scroll-other-window-up)

  ;; Goto matching paren (M-g m)
  (keymap-bind! *meta-g-map* "m" 'goto-matching-paren)

  ;; Backward kill sexp (M-g DEL)
  (keymap-bind! *meta-g-map* "DEL" 'backward-kill-sexp)

  ;; Goto percent (M-g %)
  (keymap-bind! *meta-g-map* "%" 'goto-percent)

  ;; Paragraph navigation (M-{ / M-})
  (keymap-bind! *global-keymap* "M-{" 'backward-paragraph)
  (keymap-bind! *global-keymap* "M-}" 'forward-paragraph)

  ;; Sentence navigation (M-a / M-e)
  (keymap-bind! *global-keymap* "M-a" 'backward-sentence)
  (keymap-bind! *global-keymap* "M-e" 'forward-sentence)

  ;; Back to indentation (M-m)
  (keymap-bind! *global-keymap* "M-m" 'back-to-indentation)

  ;; Join lines (M-^)
  (keymap-bind! *global-keymap* "M-^" 'delete-indentation)

  ;; Hippie expand (M-/)
  (keymap-bind! *global-keymap* "M-/" 'hippie-expand)

  ;; Next/previous buffer (C-x right / C-x left)
  (keymap-bind! *ctrl-x-map* "RIGHT" 'next-buffer)
  (keymap-bind! *ctrl-x-map* "LEFT" 'previous-buffer)

  ;; Dired (C-x d)
  (keymap-bind! *ctrl-x-map* "d" 'dired)

  ;; Imenu (M-g i)
  (keymap-bind! *meta-g-map* "i" 'imenu)

  ;; What cursor position (C-x =)
  (keymap-bind! *ctrl-x-map* "=" 'what-cursor-position)

  ;; Insert register (C-x r i)
  (keymap-bind! *ctrl-x-r-map* "i" 'insert-register)

  ;; Code folding (M-g F toggle, M-g C fold-all, M-g E expand-all)
  (keymap-bind! *meta-g-map* "F" 'toggle-fold)
  (keymap-bind! *meta-g-map* "C" 'fold-all)
  (keymap-bind! *meta-g-map* "E" 'unfold-all)

  ;; Help describe key briefly (C-h c — already have C-h k)
  (keymap-bind! *help-map* "c" 'describe-key-briefly)
  (keymap-bind! *help-map* "d" 'describe-function)
  (keymap-bind! *help-map* "v" 'describe-variable)
  (keymap-bind! *help-map* "i" 'info)
  (keymap-bind! *help-map* "l" 'view-lossage)

  ;; All other new commands accessible via M-x
  )

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
  (set! (echo-state-error? echo) #t)
  (gemacs-log! "ERROR: " msg))

(def (echo-clear! echo)
  (set! (echo-state-message echo) #f)
  (set! (echo-state-error? echo) #f))

;;;============================================================================
;;; Hooks
;;;============================================================================

;; Called after buffer-attach! with (editor buffer) arguments.
;; Set by app-init! to restore syntax highlighting per buffer.
(def *post-buffer-attach-hook* (lambda (editor buf) (void)))

;;;============================================================================
;;; Buffer structure and list
;;;============================================================================

(defstruct buffer
  (name        ; string: display name (e.g. "*scratch*", "foo.txt")
   file-path   ; string or #f: file path if visiting a file
   doc-pointer ; backend-specific document handle (Scintilla doc ptr or QTextDocument*)
   mark        ; integer or #f: mark position for region
   modified    ; boolean
   lexer-lang  ; symbol or #f: lexer language
   backup-done?) ; boolean: whether backup file was created this session
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
   macro-named   ; hash-table: name -> list of (action . data) — named macros
   mark-ring     ; list of (buffer-name . position) for mark history
   registers     ; hash-table: char -> string or (buffer-name . position)
   last-command  ; symbol or #f: name of last executed command
   prefix-arg    ; any: current prefix argument (#f, integer, or list for C-u)
   prefix-digit-mode? ; boolean: are we currently collecting digit arguments?
   key-handler   ; procedure or #f: (lambda (editor) ...) installs key handler on editor
   winner-history      ; list of window configs: ((num-windows current-idx buffers) ...)
   winner-history-idx  ; integer: current position in winner-history for redo
   tabs                ; list of tabs: ((name buffer-names current-idx) ...)
   current-tab-idx     ; integer: current tab index
   key-lossage)        ; list of key strings (most recent first), max 300
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
   (make-hash-table)     ; macro-named
   []                    ; mark-ring
   (make-hash-table)     ; registers
   #f                    ; last-command
   #f                    ; prefix-arg
   #f                    ; prefix-digit-mode?
   #f                    ; key-handler
   []                    ; winner-history
   0                     ; winner-history-idx
   (list (list "Tab 1" '("*scratch*") 0)) ; tabs - initial tab
   0                     ; current-tab-idx
   []))                  ; key-lossage

(def (get-prefix-arg app (default 1))
  "Get the numeric value of the current prefix argument."
  (let ((arg (app-state-prefix-arg app)))
    (cond
     ((not arg) default)
     ((number? arg) arg)
     ((list? arg) (car arg))
     ((eq? arg '-) -1)
     (else default))))

;;;============================================================================
;;; Key lossage (last 300 keystrokes)
;;;============================================================================

(def *key-lossage-max* 300)

(def (key-lossage-record! app key-str)
  "Record a keystroke in the lossage ring."
  (let ((lossage (app-state-key-lossage app)))
    (set! (app-state-key-lossage app)
      (if (>= (length lossage) *key-lossage-max*)
        (cons key-str (list-head lossage (- *key-lossage-max* 1)))
        (cons key-str lossage)))))

(def (key-lossage->string app)
  "Format key lossage for display, 10 keys per line."
  (let ((keys (reverse (app-state-key-lossage app))))
    (if (null? keys)
      "(no keystrokes recorded)"
      (let loop ((ks keys) (col 0) (acc ""))
        (if (null? ks)
          acc
          (let* ((k (car ks))
                 (sep (if (and (> col 0) (= (modulo col 10) 0)) "\n" " "))
                 (new-acc (if (string=? acc "")
                            k
                            (string-append acc sep k))))
            (loop (cdr ks) (+ col 1) new-acc)))))))

(def (list-head lst n)
  "Return the first n elements of lst."
  (let loop ((l lst) (i 0) (acc []))
    (if (or (null? l) (>= i n))
      (reverse acc)
      (loop (cdr l) (+ i 1) (cons (car l) acc)))))

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

;; Command documentation registry
(def *command-docs* (make-hash-table))

(def (register-command-doc! name doc)
  "Register a docstring for a command."
  (hash-put! *command-docs* name doc))

(def (command-name->description name)
  "Convert a command symbol to a human-readable description.
   E.g. 'find-file -> \"Find file\", 'toggle-auto-revert -> \"Toggle auto revert\""
  (let* ((s (symbol->string name))
         (words (string-split s #\-))
         (capitalized (if (pair? words)
                       (cons (let ((w (car words)))
                               (if (> (string-length w) 0)
                                 (string-append (string (char-upcase (string-ref w 0)))
                                                (substring w 1 (string-length w)))
                                 w))
                             (cdr words))
                       words)))
    (string-join capitalized " ")))

(def (command-doc name)
  "Get the documentation for a command.
   Returns the registered docstring, or auto-generates one from the name."
  (or (hash-get *command-docs* name)
      (command-name->description name)))

(def (find-keybinding-for-command name)
  "Find the first keybinding for a command symbol. Returns key string or #f."
  (let ((result #f))
    (for-each
      (lambda (entry)
        (unless result
          (let ((key (car entry)) (val (cdr entry)))
            (cond
              ((eq? val name) (set! result key))
              ((hash-table? val)
               (for-each
                 (lambda (sub)
                   (unless result
                     (when (eq? (cdr sub) name)
                       (set! result (string-append key " " (car sub))))))
                 (keymap-entries val)))))))
      (keymap-entries *global-keymap*))
    result))

(def (execute-command! app name)
  (let ((cmd (find-command name)))
    (if cmd
      (begin
        (set! (app-state-last-command app) name)
        (with-catch
          (lambda (e)
            (let ((msg (with-output-to-string "" (lambda () (display-exception e)))))
              (gemacs-log! "COMMAND-ERROR: " (symbol->string name) ": " msg)
              (echo-error! (app-state-echo app)
                (string-append (symbol->string name) ": " msg))))
          (lambda () (cmd app)))
        ;; Reset prefix-arg unless the command was a prefix-building command
        (unless (memq name '(universal-argument digit-argument-0 digit-argument-1 digit-argument-2
                            digit-argument-3 digit-argument-4 digit-argument-5 digit-argument-6
                            digit-argument-7 digit-argument-8 digit-argument-9 negative-argument))
          (set! (app-state-prefix-arg app) #f)
          (set! (app-state-prefix-digit-mode? app) #f)))
      (echo-error! (app-state-echo app)
                   (string-append (symbol->string name) " is undefined")))))

(def (setup-command-docs!)
  "Register docstrings for commonly used commands."
  ;; File operations
  (register-command-doc! 'find-file "Visit a file in its own buffer. Prompts for a file path.")
  (register-command-doc! 'save-buffer "Save the current buffer to its file.")
  (register-command-doc! 'save-some-buffers "Save all modified file-visiting buffers.")
  (register-command-doc! 'write-file "Write the current buffer to a different file (Save As).")
  (register-command-doc! 'revert-buffer "Revert the buffer to the last saved version of its file.")
  (register-command-doc! 'revert-buffer-quick "Revert the buffer without confirmation.")
  ;; Buffer operations
  (register-command-doc! 'switch-buffer "Switch to a different buffer by name, with completion.")
  (register-command-doc! 'kill-buffer-cmd "Kill (close) a buffer. Prompts if modified.")
  (register-command-doc! 'list-buffers "Display a list of all buffers in a *Buffer List* buffer.")
  (register-command-doc! 'next-buffer "Switch to the next buffer in the buffer list.")
  (register-command-doc! 'previous-buffer "Switch to the previous buffer in the buffer list.")
  ;; Navigation
  (register-command-doc! 'forward-char "Move point right one character.")
  (register-command-doc! 'backward-char "Move point left one character.")
  (register-command-doc! 'forward-word "Move point forward one word.")
  (register-command-doc! 'backward-word "Move point backward one word.")
  (register-command-doc! 'next-line "Move point down one line, keeping the same column if possible.")
  (register-command-doc! 'previous-line "Move point up one line, keeping the same column if possible.")
  (register-command-doc! 'beginning-of-line "Move point to the beginning of the current line.")
  (register-command-doc! 'end-of-line "Move point to the end of the current line.")
  (register-command-doc! 'beginning-of-buffer "Move point to the beginning of the buffer.")
  (register-command-doc! 'end-of-buffer "Move point to the end of the buffer.")
  (register-command-doc! 'goto-line "Go to a specific line number.")
  (register-command-doc! 'goto-char "Go to a specific character position.")
  (register-command-doc! 'forward-paragraph "Move forward to the end of the next paragraph.")
  (register-command-doc! 'backward-paragraph "Move backward to the start of the previous paragraph.")
  (register-command-doc! 'scroll-up "Scroll the buffer up (forward) one screenful.")
  (register-command-doc! 'scroll-down "Scroll the buffer down (backward) one screenful.")
  (register-command-doc! 'recenter "Center the display around point.")
  ;; Editing
  (register-command-doc! 'self-insert "Insert the character that was typed.")
  (register-command-doc! 'newline "Insert a newline at point.")
  (register-command-doc! 'delete-char "Delete the character after point.")
  (register-command-doc! 'delete-backward-char "Delete the character before point.")
  (register-command-doc! 'kill-line "Kill from point to end of line. If at end, kill the newline.")
  (register-command-doc! 'kill-word "Kill characters forward to end of the next word.")
  (register-command-doc! 'backward-kill-word "Kill characters backward to beginning of the previous word.")
  (register-command-doc! 'kill-region "Kill the region (text between point and mark).")
  (register-command-doc! 'copy-region-as-kill "Copy the region to the kill ring without deleting it.")
  (register-command-doc! 'yank "Reinsert the last stretch of killed text.")
  (register-command-doc! 'yank-pop "Replace the just-yanked text with an earlier item from the kill ring.")
  (register-command-doc! 'undo "Undo the last editing change.")
  (register-command-doc! 'redo "Redo the last undone change.")
  (register-command-doc! 'indent-or-complete "Indent the current line or trigger completion.")
  ;; Search
  (register-command-doc! 'isearch-forward "Incremental search forward. Type characters to search.")
  (register-command-doc! 'isearch-backward "Incremental search backward.")
  (register-command-doc! 'query-replace "Interactively replace occurrences of a string.")
  (register-command-doc! 'query-replace-regexp "Interactively replace occurrences matching a regexp.")
  (register-command-doc! 'occur "Show all lines matching a pattern in an *Occur* buffer.")
  (register-command-doc! 'grep "Run grep and display results in a *Grep* buffer.")
  (register-command-doc! 'project-search "Search for a string across all files in the current project.")
  ;; Mark and region
  (register-command-doc! 'set-mark "Set the mark at point, starting a region.")
  (register-command-doc! 'exchange-point-and-mark "Swap point and mark, jumping to the other end of the region.")
  (register-command-doc! 'mark-whole-buffer "Mark the entire buffer as the region.")
  ;; Window management
  (register-command-doc! 'split-window-below "Split the current window into two, one above the other.")
  (register-command-doc! 'split-window-right "Split the current window into two side by side.")
  (register-command-doc! 'delete-window "Remove the current window from the frame.")
  (register-command-doc! 'delete-other-windows "Make the current window fill the entire frame.")
  (register-command-doc! 'other-window "Select the next window in cyclic order.")
  (register-command-doc! 'balance-windows "Make all windows the same height.")
  ;; M-x and help
  (register-command-doc! 'execute-extended-command "Read a command name with completion and execute it (M-x).")
  (register-command-doc! 'describe-key "Show what command a key is bound to, with documentation.")
  (register-command-doc! 'describe-command "Describe a command by name, showing its keybinding and documentation.")
  (register-command-doc! 'describe-function "Describe a function/command by name.")
  (register-command-doc! 'list-bindings "Display all keybindings in a *Help* buffer.")
  (register-command-doc! 'keyboard-quit "Abort the current operation.")
  ;; Shell and REPL
  (register-command-doc! 'eshell "Open or switch to the built-in Gerbil shell (eshell).")
  (register-command-doc! 'shell "Open or switch to an external shell buffer.")
  (register-command-doc! 'gerbil-repl "Open or switch to a Gerbil REPL (gxi) buffer.")
  (register-command-doc! 'eval-expression "Evaluate a Gerbil expression and show the result.")
  (register-command-doc! 'eval-buffer "Evaluate all forms in the current buffer.")
  (register-command-doc! 'load-file "Load and evaluate a Gerbil (.ss) file.")
  (register-command-doc! 'compile "Run a compilation command and display results.")
  ;; Dired
  (register-command-doc! 'dired "Open a directory editor (dired) for the given path.")
  (register-command-doc! 'dired-jump "Jump to the directory of the current file in dired.")
  ;; VCS
  (register-command-doc! 'magit-status "Show the git status of the current project (magit).")
  (register-command-doc! 'magit-log "Show the git log for the current project.")
  (register-command-doc! 'magit-diff "Show git diff for the current project.")
  (register-command-doc! 'magit-commit "Commit staged changes with a message.")
  (register-command-doc! 'magit-push "Push the current branch to the remote.")
  (register-command-doc! 'magit-pull "Pull from the remote into the current branch.")
  (register-command-doc! 'magit-blame "Show git blame annotations for the current file.")
  ;; Org-mode
  (register-command-doc! 'org-todo-cycle "Cycle the TODO state of the current org heading.")
  (register-command-doc! 'org-cycle "Cycle visibility of the current org heading subtree.")
  (register-command-doc! 'org-promote "Promote the current org heading (decrease level).")
  (register-command-doc! 'org-demote "Demote the current org heading (increase level).")
  (register-command-doc! 'org-schedule "Insert a SCHEDULED timestamp for the current heading.")
  (register-command-doc! 'org-deadline "Insert a DEADLINE timestamp for the current heading.")
  (register-command-doc! 'org-export "Export the current org buffer (HTML, Markdown, text, or LaTeX).")
  (register-command-doc! 'org-agenda "Open the org agenda view.")
  ;; Modes and toggles
  (register-command-doc! 'toggle-electric-pair "Toggle auto-pairing of brackets and quotes.")
  (register-command-doc! 'toggle-auto-revert "Toggle auto-revert mode: automatically reload files changed on disk.")
  (register-command-doc! 'toggle-line-numbers "Toggle display of line numbers in the editor margin.")
  (register-command-doc! 'toggle-whitespace-mode "Toggle whitespace visualization mode.")
  (register-command-doc! 'toggle-word-wrap "Toggle word wrapping of long lines.")
  ;; Completion and minibuffer
  (register-command-doc! 'switch-buffer "Switch to another buffer by name, with fuzzy completion.")
  (register-command-doc! 'recentf-open-files "Show recently opened files in a numbered list.")
  (register-command-doc! 'bookmark-set "Set a bookmark at the current position.")
  (register-command-doc! 'bookmark-jump "Jump to a named bookmark.")
  ;; Rectangle
  (register-command-doc! 'kill-rectangle "Kill the text in a rectangular region.")
  (register-command-doc! 'yank-rectangle "Insert the last killed rectangle.")
  (register-command-doc! 'string-rectangle "Replace each line of a rectangular region with a string.")
  ;; Other common commands
  (register-command-doc! 'comment-line "Comment or uncomment the current line or region.")
  (register-command-doc! 'fill-paragraph "Fill (reflow) the current paragraph to fill-column width.")
  (register-command-doc! 'sort-lines "Sort lines in the region alphabetically.")
  (register-command-doc! 'align-regexp "Align text in the region based on a regular expression.")
  (register-command-doc! 'narrow-to-region "Narrow the buffer to show only the region between point and mark.")
  (register-command-doc! 'widen "Widen from a narrowed region to show the full buffer again.")
  (register-command-doc! 'show-kill-ring "Display the kill ring contents in a buffer.")
  (register-command-doc! 'universal-argument "Begin a numeric argument for the next command (C-u).")
  ;; LSP
  (register-command-doc! 'lsp-start "Start the Language Server Protocol client for the current file type.")
  (register-command-doc! 'lsp-stop "Stop the running LSP server connection.")
  ;; Session
  (register-command-doc! 'session-save "Save the current session (open buffers) to disk.")
  (register-command-doc! 'session-restore "Restore a previously saved session.")
  (register-command-doc! 'desktop-save "Save the desktop (open buffers) for later restoration.")
  (register-command-doc! 'desktop-read "Restore a previously saved desktop."))

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
;;; Runtime error log file (~/.gemacs-errors.log)
;;;============================================================================

(def *gemacs-log-port* #f)
(def *gemacs-original-stderr* #f)

(def (init-gemacs-log!)
  "Initialize the runtime error log. Opens ~/.gemacs-errors.log for append,
   redirects current-error-port so Scheme-level stderr writes go to the log.
   Call once at startup (Qt or TUI)."
  (let ((log-path (string-append (getenv "HOME" "/tmp") "/.gemacs-errors.log")))
    ;; Save original stderr for fallback
    (set! *gemacs-original-stderr* (current-error-port))
    ;; Open log file in append mode
    (set! *gemacs-log-port* (open-output-file [path: log-path append: #t]))
    ;; Redirect Scheme current-error-port to the log file
    ;; This captures Gambit's display-exception, warning messages, etc.
    (current-error-port *gemacs-log-port*)
    (gemacs-log! "gemacs started")))

(def (gemacs-log! . args)
  "Write a timestamped line to the gemacs error log.
   Arguments are concatenated as strings."
  (when *gemacs-log-port*
    (let ((port *gemacs-log-port*)
          (ts (number->string (inexact->exact (floor (time->seconds (current-time)))))))
      (display "[" port)
      (display ts port)
      (display "] " port)
      (for-each (lambda (arg) (display arg port)) args)
      (newline port)
      (force-output port))))

;;;============================================================================
;;; Captured output logs (for eval stdout/stderr in Qt)
;;;============================================================================

(def *captured-errors* "")
(def *captured-output* "")

(def (append-error-log! text)
  (set! *captured-errors* (string-append *captured-errors* text))
  (gemacs-log! "EVAL-STDERR: " text))
(def (append-output-log! text)
  (set! *captured-output* (string-append *captured-output* text)))
(def (get-error-log) *captured-errors*)
(def (get-output-log) *captured-output*)
(def (clear-error-log!) (set! *captured-errors* ""))
(def (clear-output-log!) (set! *captured-output* ""))
(def (has-captured-output?)
  (or (> (string-length *captured-errors*) 0)
      (> (string-length *captured-output*) 0)))

;;;============================================================================
;;; REPL shared logic
;;;============================================================================

(def (repl-buffer? buf)
  "Check if this buffer is a REPL buffer."
  (eq? (buffer-lexer-lang buf) 'repl))

;; Maps REPL buffers to their repl-state structs
;; Use eq? table: buffer structs are mutable (transparent: #t), so equal?-based
;; tables break when fields like buffer-modified change after hash-put!.
(def *repl-state* (make-hash-table-eq))

(def *gerbil-eval-initialized* #f)

(def (ensure-gerbil-eval!)
  "Initialize the Gerbil expander on first use so eval supports full
   Gerbil syntax (def, defstruct, hash, match, import, etc.).
   Called lazily to avoid startup cost."
  (unless *gerbil-eval-initialized*
    (set! *gerbil-eval-initialized* #t)
    (gemacs-log! "ensure-gerbil-eval!: initializing expander via __load-gxi")
    (with-catch
      (lambda (e)
        (let ((msg (with-output-to-string (lambda () (display-exception e)))))
          (gemacs-log! "ensure-gerbil-eval! FAILED: " msg)
          (append-error-log! (string-append "Expander init failed: " msg "\n"))
          ;; Reset so user can retry
          (set! *gerbil-eval-initialized* #f)))
      (lambda ()
        (__load-gxi)
        (gemacs-log! "ensure-gerbil-eval!: expander initialized OK")))))

(def (eval-expression-string str)
  "In-process eval: read+eval an expression string, capture output.
   Returns (values result-string error?).
   Stdout/stderr side effects are appended to the captured output/error logs.
   Full Gerbil syntax supported (def, hash, match, etc.)."
  (ensure-gerbil-eval!)
  (with-catch
    (lambda (e)
      (let ((msg (with-output-to-string (lambda () (display-exception e)))))
        (append-error-log! msg)
        (values msg #t)))
    (lambda ()
      (let* ((out-port (open-output-string))
             (err-port (open-output-string))
             (expr (with-input-from-string str read))
             (result (parameterize ((current-output-port out-port)
                                    (current-error-port err-port))
                       (eval expr)))
             (stdout-text (get-output-string out-port))
             (stderr-text (get-output-string err-port))
             (output (with-output-to-string (lambda () (write result)))))
        (when (> (string-length stdout-text) 0)
          (append-output-log! stdout-text))
        (when (> (string-length stderr-text) 0)
          (append-error-log! stderr-text))
        (values output #f)))))

(def (load-user-file! path)
  "Load a .ss file by reading and evaluating each top-level form.
   Full Gerbil syntax supported after expander init.
   Stdout/stderr side effects are appended to the captured output/error logs.
   Returns (values num-loaded error-msg) where error-msg is #f on success."
  (ensure-gerbil-eval!)
  (with-catch
    (lambda (e)
      (let ((msg (with-output-to-string (lambda () (display-exception e)))))
        (append-error-log! msg)
        (values 0 msg)))
    (lambda ()
      (let ((port (open-input-file path))
            (out-port (open-output-string))
            (err-port (open-output-string)))
        (let loop ((count 0))
          (let ((form (read port)))
            (if (eof-object? form)
              (begin
                (close-input-port port)
                (let ((stdout-text (get-output-string out-port))
                      (stderr-text (get-output-string err-port)))
                  (when (> (string-length stdout-text) 0)
                    (append-output-log! stdout-text))
                  (when (> (string-length stderr-text) 0)
                    (append-error-log! stderr-text)))
                (values count #f))
              (begin
                (parameterize ((current-output-port out-port)
                               (current-error-port err-port))
                  (eval form))
                (loop (+ count 1))))))))))

(def (load-user-string! str (source "buffer"))
  "Eval all top-level forms in a string.
   Full Gerbil syntax supported after expander init.
   Stdout/stderr side effects are appended to the captured output/error logs.
   Returns (values num-loaded error-msg) where error-msg is #f on success."
  (ensure-gerbil-eval!)
  (with-catch
    (lambda (e)
      (let ((msg (with-output-to-string (lambda () (display-exception e)))))
        (append-error-log! msg)
        (values 0 msg)))
    (lambda ()
      (let ((port (open-input-string str))
            (out-port (open-output-string))
            (err-port (open-output-string)))
        (let loop ((count 0))
          (let ((form (read port)))
            (if (eof-object? form)
              (begin
                (let ((stdout-text (get-output-string out-port))
                      (stderr-text (get-output-string err-port)))
                  (when (> (string-length stdout-text) 0)
                    (append-output-log! stdout-text))
                  (when (> (string-length stderr-text) 0)
                    (append-error-log! stderr-text)))
                (values count #f))
              (begin
                (parameterize ((current-output-port out-port)
                               (current-error-port err-port))
                  (eval form))
                (loop (+ count 1))))))))))

;;;============================================================================
;;; Fuzzy matching
;;;============================================================================

(def (fuzzy-match? query target)
  "Check if query fuzzy-matches target. Characters must appear in order."
  (let ((qlen (string-length query))
        (tlen (string-length target)))
    (let loop ((qi 0) (ti 0))
      (cond
        ((>= qi qlen) #t)
        ((>= ti tlen) #f)
        ((char=? (char-downcase (string-ref query qi))
                 (char-downcase (string-ref target ti)))
         (loop (+ qi 1) (+ ti 1)))
        (else
         (loop qi (+ ti 1)))))))

(def (fuzzy-score query target)
  "Score a fuzzy match. Higher is better."
  (let ((qlen (string-length query))
        (tlen (string-length target)))
    (let loop ((qi 0) (ti 0) (score 0) (consecutive 0))
      (cond
        ((>= qi qlen) score)
        ((>= ti tlen) -1)
        ((char=? (char-downcase (string-ref query qi))
                 (char-downcase (string-ref target ti)))
         (let ((bonus (+ 1
                        (if (= ti 0) 3 0)
                        (* consecutive 2)
                        (if (and (> ti 0)
                                 (memv (string-ref target (- ti 1))
                                       '(#\- #\_ #\/ #\space)))
                          2 0))))
           (loop (+ qi 1) (+ ti 1) (+ score bonus) (+ consecutive 1))))
        (else
         (loop qi (+ ti 1) score 0))))))

(def (fuzzy-filter-sort query candidates)
  "Filter candidates by fuzzy match and sort by score (best first)."
  (let* ((scored (filter-map
                   (lambda (c)
                     (let ((s (fuzzy-score query c)))
                       (and (>= s 0) (cons s c))))
                   candidates))
         (sorted (sort scored (lambda (a b) (> (car a) (car b))))))
    (map cdr sorted)))

;;;============================================================================
;;; Key translation map
;;;============================================================================

;; Paredit strict mode — prevents deleting delimiters that would unbalance
(def *paredit-strict-mode* #f)

;; Maps char→char for input translation (e.g., swap brackets and parens)
(def *key-translation-map* (make-hash-table))

(def (key-translate! from to)
  "Register a character translation. FROM and TO are characters."
  (hash-put! *key-translation-map* from to))

(def (key-translate-char ch)
  "Translate a character through the key translation map.
   Returns the translated char, or the original if no mapping."
  (or (hash-get *key-translation-map* ch) ch))

;;;============================================================================
;;; Key-chord system
;;;============================================================================

;; Chord map: "AB" → command-symbol (always uppercase keys)
(def *chord-map* (make-hash-table))

;; Set of characters that can start a chord (uppercase)
(def *chord-first-chars* (make-hash-table))

;; Time window in milliseconds for second key of chord
(def *chord-timeout* 200)

;; Master toggle
(def *chord-mode* #t)

(def (key-chord-define-global two-char-str cmd)
  "Bind a 2-character chord to a command symbol.
   The string is uppercased for matching."
  (let ((upper (string-upcase two-char-str)))
    (hash-put! *chord-map* upper cmd)
    (hash-put! *chord-first-chars* (string-ref upper 0) #t)))

(def (chord-lookup ch1 ch2)
  "Look up a chord by two characters. Returns command symbol or #f."
  (hash-get *chord-map* (string (char-upcase ch1) (char-upcase ch2))))

(def (chord-start-char? ch)
  "Can this character start a chord? Only when chord-mode is on."
  (and *chord-mode*
       (hash-get *chord-first-chars* (char-upcase ch))))

;;;============================================================================
;;; Image buffer support
;;;============================================================================

;; Maps editor FFI pointer → qt-edit-window struct (for reverse lookup)
(def *editor-window-map* (make-hash-table))

;; Maps image buffer → (list pixmap zoom-ref orig-w orig-h)
(def *image-buffer-state* (make-hash-table-eq))

(def (image-buffer? buf)
  "Check if this buffer is an image buffer."
  (eq? (buffer-lexer-lang buf) 'image))
