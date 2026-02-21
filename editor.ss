;;; -*- Gerbil -*-
;;; TUI editor commands and app state for gemacs
;;; Facade module: imports sub-modules and registers all commands.

(export
  (struct-out app-state)
  new-app-state
  current-editor
  current-buffer-from-app
  execute-command!
  cmd-self-insert!
  cmd-negative-argument
  cmd-digit-argument
  *auto-pair-mode*
  auto-pair-char
  auto-pair-closing?
  register-all-commands!
  read-file-as-string
  expand-filename
  position-cursor-for-replace!
  ;; Auto-save and file modification tracking
  *auto-save-enabled*
  *auto-save-counter*
  *auto-save-interval*
  *buffer-mod-times*
  update-buffer-mod-time!
  auto-save-buffers!
  check-file-modifications!
  make-auto-save-path
  file-mod-time)

(import :gemacs/core
        :gemacs/editor-extra
        :gemacs/editor-core
        :gemacs/editor-ui
        :gemacs/editor-text
        :gemacs/editor-advanced
        :gemacs/editor-cmds-a
        :gemacs/editor-cmds-b
        :gemacs/editor-cmds-c)

;;;============================================================================
;;; Register all commands
;;;============================================================================

(def (register-all-commands!)
  ;; Navigation
  (register-command! 'forward-char cmd-forward-char)
  (register-command! 'backward-char cmd-backward-char)
  (register-command! 'next-line cmd-next-line)
  (register-command! 'previous-line cmd-previous-line)
  (register-command! 'beginning-of-line cmd-beginning-of-line)
  (register-command! 'end-of-line cmd-end-of-line)
  (register-command! 'forward-word cmd-forward-word)
  (register-command! 'backward-word cmd-backward-word)
  (register-command! 'beginning-of-buffer cmd-beginning-of-buffer)
  (register-command! 'end-of-buffer cmd-end-of-buffer)
  (register-command! 'scroll-down cmd-scroll-down)
  (register-command! 'scroll-up cmd-scroll-up)
  (register-command! 'recenter cmd-recenter)
  (register-command! 'recenter-top-bottom cmd-recenter-top-bottom)
  ;; Editing
  (register-command! 'delete-char cmd-delete-char)
  (register-command! 'backward-delete-char cmd-backward-delete-char)
  (register-command! 'backward-delete-char-untabify cmd-backward-delete-char-untabify)
  (register-command! 'newline cmd-newline)
  (register-command! 'open-line cmd-open-line)
  (register-command! 'undo cmd-undo)
  ;; (indent was here — now replaced by indent-or-complete)
  ;; Kill/Yank
  (register-command! 'kill-line cmd-kill-line)
  (register-command! 'yank cmd-yank)
  ;; Mark/Region
  (register-command! 'set-mark cmd-set-mark)
  (register-command! 'set-mark-command cmd-set-mark)  ; Emacs alias
  (register-command! 'kill-region cmd-kill-region)
  (register-command! 'copy-region cmd-copy-region)
  (register-command! 'kill-ring-save cmd-copy-region)  ; Emacs alias
  ;; File
  (register-command! 'find-file cmd-find-file)
  (register-command! 'save-buffer cmd-save-buffer)
  (register-command! 'save-file cmd-save-buffer)  ; alias
  ;; Buffer
  (register-command! 'switch-buffer cmd-switch-buffer)
  (register-command! 'kill-buffer-cmd cmd-kill-buffer-cmd)
  ;; Window
  (register-command! 'split-window cmd-split-window)
  (register-command! 'split-window-right cmd-split-window-right)
  (register-command! 'other-window cmd-other-window)
  (register-command! 'delete-window cmd-delete-window)
  (register-command! 'delete-other-windows cmd-delete-other-windows)
  ;; Search
  (register-command! 'search-forward cmd-search-forward)
  (register-command! 'search-backward cmd-search-backward)
  ;; REPL
  (register-command! 'repl cmd-repl)
  (register-command! 'eval-expression cmd-eval-expression)
  ;; Eshell
  (register-command! 'eshell cmd-eshell)
  ;; Shell
  (register-command! 'shell cmd-shell)
  ;; Terminal (PTY-backed vterm-like)
  (register-command! 'term cmd-term)
  (register-command! 'term-interrupt cmd-term-interrupt)
  (register-command! 'term-send-eof cmd-term-send-eof)
  (register-command! 'term-send-tab cmd-term-send-tab)
  ;; Goto line
  (register-command! 'goto-line cmd-goto-line)
  ;; M-x
  (register-command! 'execute-extended-command cmd-execute-extended-command)
  ;; Help
  (register-command! 'describe-key cmd-describe-key)
  (register-command! 'describe-command cmd-describe-command)
  (register-command! 'list-bindings cmd-list-bindings)
  ;; Buffer list
  (register-command! 'list-buffers cmd-list-buffers)
  ;; Query replace
  (register-command! 'query-replace cmd-query-replace)
  ;; Tab/indent
  (register-command! 'indent-or-complete cmd-indent-or-complete)
  ;; Redo
  (register-command! 'redo cmd-redo)
  ;; Toggles
  (register-command! 'toggle-line-numbers cmd-toggle-line-numbers)
  (register-command! 'toggle-word-wrap cmd-toggle-word-wrap)
  (register-command! 'toggle-whitespace cmd-toggle-whitespace)
  ;; Zoom
  (register-command! 'zoom-in cmd-zoom-in)
  (register-command! 'zoom-out cmd-zoom-out)
  (register-command! 'zoom-reset cmd-zoom-reset)
  ;; Select all
  (register-command! 'select-all cmd-select-all)
  ;; Duplicate line
  (register-command! 'duplicate-line cmd-duplicate-line)
  ;; Comment toggle
  (register-command! 'toggle-comment cmd-toggle-comment)
  ;; Transpose
  (register-command! 'transpose-chars cmd-transpose-chars)
  ;; Word case
  (register-command! 'upcase-word cmd-upcase-word)
  (register-command! 'downcase-word cmd-downcase-word)
  (register-command! 'capitalize-word cmd-capitalize-word)
  ;; Kill word
  (register-command! 'kill-word cmd-kill-word)
  ;; What line
  (register-command! 'what-line cmd-what-line)
  ;; Write file / revert
  (register-command! 'write-file cmd-write-file)
  (register-command! 'revert-buffer cmd-revert-buffer)
  ;; Defun navigation
  (register-command! 'beginning-of-defun cmd-beginning-of-defun)
  (register-command! 'end-of-defun cmd-end-of-defun)
  ;; Delete trailing whitespace
  (register-command! 'delete-trailing-whitespace cmd-delete-trailing-whitespace)
  ;; Count words
  (register-command! 'count-words cmd-count-words)
  ;; Yank-pop
  (register-command! 'yank-pop cmd-yank-pop)
  ;; Occur
  (register-command! 'occur cmd-occur)
  ;; Compile
  (register-command! 'compile cmd-compile)
  ;; Shell command on region
  (register-command! 'shell-command-on-region cmd-shell-command-on-region)
  ;; Sort lines
  (register-command! 'sort-lines cmd-sort-lines)
  ;; Bookmarks
  (register-command! 'bookmark-set cmd-bookmark-set)
  (register-command! 'bookmark-jump cmd-bookmark-jump)
  (register-command! 'bookmark-list cmd-bookmark-list)
  ;; Rectangle operations
  (register-command! 'kill-rectangle cmd-kill-rectangle)
  (register-command! 'delete-rectangle cmd-delete-rectangle)
  (register-command! 'yank-rectangle cmd-yank-rectangle)
  ;; Go to matching paren
  (register-command! 'goto-matching-paren cmd-goto-matching-paren)
  ;; Join line
  (register-command! 'join-line cmd-join-line)
  ;; Delete blank lines
  (register-command! 'delete-blank-lines cmd-delete-blank-lines)
  ;; Indent region
  (register-command! 'indent-region cmd-indent-region)
  ;; Case region
  (register-command! 'downcase-region cmd-downcase-region)
  (register-command! 'upcase-region cmd-upcase-region)
  ;; Shell command
  (register-command! 'shell-command cmd-shell-command)
  ;; Fill paragraph
  (register-command! 'fill-paragraph cmd-fill-paragraph)
  ;; Grep
  (register-command! 'grep cmd-grep)
  ;; Insert file
  (register-command! 'insert-file cmd-insert-file)
  (register-command! 'string-insert-file cmd-insert-file)  ; alias
  ;; Dabbrev and completion
  (register-command! 'dabbrev-expand cmd-dabbrev-expand)
  (register-command! 'complete-at-point cmd-complete-at-point)
  ;; What cursor position
  (register-command! 'what-cursor-position cmd-what-cursor-position)
  ;; Keyboard macros
  (register-command! 'start-kbd-macro cmd-start-kbd-macro)
  (register-command! 'end-kbd-macro cmd-end-kbd-macro)
  (register-command! 'call-last-kbd-macro cmd-call-last-kbd-macro)
  ;; Mark ring
  (register-command! 'pop-mark cmd-pop-mark)
  ;; Registers
  (register-command! 'copy-to-register cmd-copy-to-register)
  (register-command! 'insert-register cmd-insert-register)
  (register-command! 'point-to-register cmd-point-to-register)
  (register-command! 'jump-to-register cmd-jump-to-register)
  ;; Backward kill word, zap to char, goto char
  (register-command! 'backward-kill-word cmd-backward-kill-word)
  (register-command! 'zap-to-char cmd-zap-to-char)
  (register-command! 'goto-char cmd-goto-char)
  ;; Replace string (non-interactive)
  (register-command! 'replace-string cmd-replace-string)
  ;; Transpose
  (register-command! 'transpose-words cmd-transpose-words)
  (register-command! 'transpose-lines cmd-transpose-lines)
  ;; Just one space
  (register-command! 'just-one-space cmd-just-one-space)
  ;; Repeat
  (register-command! 'repeat cmd-repeat)
  ;; Next/previous error (search result navigation)
  (register-command! 'next-error cmd-next-error)
  (register-command! 'previous-error cmd-previous-error)
  ;; Kill whole line
  (register-command! 'kill-whole-line cmd-kill-whole-line)
  ;; Move line up/down
  (register-command! 'move-line-up cmd-move-line-up)
  (register-command! 'move-line-down cmd-move-line-down)
  ;; Pipe buffer
  (register-command! 'pipe-buffer cmd-pipe-buffer)
  ;; Narrow/widen
  (register-command! 'narrow-to-region cmd-narrow-to-region)
  (register-command! 'widen cmd-widen)
  ;; String rectangle, open rectangle
  (register-command! 'string-rectangle cmd-string-rectangle)
  (register-command! 'open-rectangle cmd-open-rectangle)
  ;; Number lines, reverse region
  (register-command! 'number-lines cmd-number-lines)
  (register-command! 'reverse-region cmd-reverse-region)
  ;; Flush/keep lines
  (register-command! 'flush-lines cmd-flush-lines)
  (register-command! 'keep-lines cmd-keep-lines)
  ;; Align
  (register-command! 'align-regexp cmd-align-regexp)
  ;; Sort fields
  (register-command! 'sort-fields cmd-sort-fields)
  ;; Mark word, mark paragraph, paragraph nav
  (register-command! 'mark-word cmd-mark-word)
  (register-command! 'mark-paragraph cmd-mark-paragraph)
  (register-command! 'forward-paragraph cmd-forward-paragraph)
  (register-command! 'backward-paragraph cmd-backward-paragraph)
  ;; Indentation nav
  (register-command! 'back-to-indentation cmd-back-to-indentation)
  (register-command! 'delete-indentation cmd-delete-indentation)
  ;; Whitespace
  (register-command! 'fixup-whitespace cmd-fixup-whitespace)
  ;; Point/mark
  (register-command! 'exchange-point-and-mark cmd-exchange-point-and-mark)
  ;; Info commands
  (register-command! 'what-page cmd-what-page)
  (register-command! 'count-lines-region cmd-count-lines-region)
  ;; Copy line
  (register-command! 'copy-line cmd-copy-line)
  ;; Help: where-is, apropos
  (register-command! 'where-is cmd-where-is)
  (register-command! 'apropos-command cmd-apropos-command)
  ;; Buffer: read-only, rename
  (register-command! 'toggle-read-only cmd-toggle-read-only)
  (register-command! 'rename-buffer cmd-rename-buffer)
  ;; Other-window
  (register-command! 'switch-buffer-other-window cmd-switch-buffer-other-window)
  (register-command! 'find-file-other-window cmd-find-file-other-window)
  ;; Universal argument
  (register-command! 'universal-argument cmd-universal-argument)
  (register-command! 'negative-argument cmd-negative-argument)
  (register-command! 'digit-argument-0 cmd-digit-argument-0)
  (register-command! 'digit-argument-1 cmd-digit-argument-1)
  (register-command! 'digit-argument-2 cmd-digit-argument-2)
  (register-command! 'digit-argument-3 cmd-digit-argument-3)
  (register-command! 'digit-argument-4 cmd-digit-argument-4)
  (register-command! 'digit-argument-5 cmd-digit-argument-5)
  (register-command! 'digit-argument-6 cmd-digit-argument-6)
  (register-command! 'digit-argument-7 cmd-digit-argument-7)
  (register-command! 'digit-argument-8 cmd-digit-argument-8)
  (register-command! 'digit-argument-9 cmd-digit-argument-9)
  ;; Tab insertion
  (register-command! 'tab-to-tab-stop cmd-tab-to-tab-stop)
  ;; Text transforms
  (register-command! 'tabify cmd-tabify)
  (register-command! 'untabify cmd-untabify)
  (register-command! 'base64-encode-region cmd-base64-encode-region)
  (register-command! 'base64-decode-region cmd-base64-decode-region)
  (register-command! 'rot13-region cmd-rot13-region)
  ;; Hex dump
  (register-command! 'hexl-mode cmd-hexl-mode)
  ;; Count/dedup
  (register-command! 'count-matches cmd-count-matches)
  (register-command! 'delete-duplicate-lines cmd-delete-duplicate-lines)
  ;; Diff, checksum
  (register-command! 'diff-buffer-with-file cmd-diff-buffer-with-file)
  (register-command! 'checksum cmd-checksum)
  ;; Async shell
  (register-command! 'async-shell-command cmd-async-shell-command)
  ;; Toggle truncate
  (register-command! 'toggle-truncate-lines cmd-toggle-truncate-lines)
  ;; Grep buffer
  (register-command! 'grep-buffer cmd-grep-buffer)
  ;; Insert date, insert char
  (register-command! 'insert-date cmd-insert-date)
  (register-command! 'insert-char cmd-insert-char)
  ;; Eval buffer/region
  (register-command! 'eval-buffer cmd-eval-buffer)
  (register-command! 'eval-region cmd-eval-region)
  ;; Clone buffer, scratch
  (register-command! 'clone-buffer cmd-clone-buffer)
  (register-command! 'scratch-buffer cmd-scratch-buffer)
  ;; Save some buffers
  (register-command! 'save-some-buffers cmd-save-some-buffers)
  ;; Revert quick
  (register-command! 'revert-buffer-quick cmd-revert-buffer-quick)
  ;; Highlighting toggle
  (register-command! 'toggle-highlighting cmd-toggle-highlighting)
  ;; Display time, pwd
  (register-command! 'display-time cmd-display-time)
  (register-command! 'pwd cmd-pwd)
  ;; Ediff
  (register-command! 'ediff-buffers cmd-ediff-buffers)
  ;; Calculator
  (register-command! 'calc cmd-calc)
  ;; Case fold search
  (register-command! 'toggle-case-fold-search cmd-toggle-case-fold-search)
  ;; Describe bindings
  (register-command! 'describe-bindings cmd-describe-bindings)
  ;; Center line
  (register-command! 'center-line cmd-center-line)
  ;; What face
  (register-command! 'what-face cmd-what-face)
  ;; List processes
  (register-command! 'list-processes cmd-list-processes)
  ;; Messages
  (register-command! 'view-messages cmd-view-messages)
  ;; Auto fill
  (register-command! 'toggle-auto-fill cmd-toggle-auto-fill)
  ;; Rename file
  (register-command! 'rename-file-and-buffer cmd-rename-file-and-buffer)
  ;; Delete file
  (register-command! 'delete-file-and-buffer cmd-delete-file-and-buffer)
  ;; Sudo write
  (register-command! 'sudo-write cmd-sudo-write)
  ;; Sort numeric
  (register-command! 'sort-numeric cmd-sort-numeric)
  ;; Count words region
  (register-command! 'count-words-region cmd-count-words-region)
  ;; Overwrite mode
  (register-command! 'toggle-overwrite-mode cmd-toggle-overwrite-mode)
  ;; Visual line mode
  (register-command! 'toggle-visual-line-mode cmd-toggle-visual-line-mode)
  ;; Fill column
  (register-command! 'set-fill-column cmd-set-fill-column)
  (register-command! 'toggle-fill-column-indicator cmd-toggle-fill-column-indicator)
  ;; Debug
  (register-command! 'toggle-debug-on-error cmd-toggle-debug-on-error)
  ;; Repeat complex command
  (register-command! 'repeat-complex-command cmd-repeat-complex-command)
  ;; Eldoc
  (register-command! 'eldoc cmd-eldoc)
  ;; Highlight symbol
  (register-command! 'highlight-symbol cmd-highlight-symbol)
  (register-command! 'clear-highlight cmd-clear-highlight)
  ;; Indent rigidly
  (register-command! 'indent-rigidly-right cmd-indent-rigidly-right)
  (register-command! 'indent-rigidly-left cmd-indent-rigidly-left)
  ;; Goto non-blank
  (register-command! 'goto-first-non-blank cmd-goto-first-non-blank)
  (register-command! 'goto-last-non-blank cmd-goto-last-non-blank)
  ;; Buffer stats
  (register-command! 'buffer-stats cmd-buffer-stats)
  ;; Show tabs/eol
  (register-command! 'toggle-show-tabs cmd-toggle-show-tabs)
  (register-command! 'toggle-show-eol cmd-toggle-show-eol)
  ;; Copy from above/below
  (register-command! 'copy-from-above cmd-copy-from-above)
  (register-command! 'copy-from-below cmd-copy-from-below)
  ;; Open line above
  (register-command! 'open-line-above cmd-open-line-above)
  ;; Select line
  (register-command! 'select-line cmd-select-line)
  ;; Split line
  (register-command! 'split-line cmd-split-line)
  ;; Line endings
  (register-command! 'convert-to-unix cmd-convert-to-unix)
  (register-command! 'convert-to-dos cmd-convert-to-dos)
  ;; Window
  (register-command! 'enlarge-window cmd-enlarge-window)
  (register-command! 'shrink-window cmd-shrink-window)
  ;; Encoding
  (register-command! 'what-encoding cmd-what-encoding)
  ;; Hippie expand
  (register-command! 'hippie-expand cmd-hippie-expand)
  ;; Swap buffers / transpose windows
  (register-command! 'swap-buffers cmd-swap-buffers)
  (register-command! 'transpose-windows cmd-swap-buffers)
  ;; Tab width
  (register-command! 'cycle-tab-width cmd-cycle-tab-width)
  (register-command! 'toggle-indent-tabs-mode cmd-toggle-indent-tabs-mode)
  ;; Buffer info
  (register-command! 'buffer-info cmd-buffer-info)
  ;; Whitespace cleanup
  (register-command! 'whitespace-cleanup cmd-whitespace-cleanup)
  ;; Electric pair toggle
  (register-command! 'toggle-electric-pair cmd-toggle-electric-pair)
  ;; Previous/next buffer
  (register-command! 'previous-buffer cmd-previous-buffer)
  (register-command! 'next-buffer cmd-next-buffer)
  ;; Balance windows
  (register-command! 'balance-windows cmd-balance-windows)
  ;; Move to window line (cycle top/center/bottom)
  (register-command! 'move-to-window-line cmd-move-to-window-line)
  ;; Kill buffer and window
  (register-command! 'kill-buffer-and-window cmd-kill-buffer-and-window)
  ;; Flush undo
  (register-command! 'flush-undo cmd-flush-undo)
  ;; Upcase initials region
  (register-command! 'upcase-initials-region cmd-upcase-initials-region)
  ;; Untabify buffer
  (register-command! 'untabify-buffer cmd-untabify-buffer)
  ;; Insert buffer name
  (register-command! 'insert-buffer-name cmd-insert-buffer-name)
  ;; Mark defun
  (register-command! 'mark-defun cmd-mark-defun)
  ;; Insert pairs
  (register-command! 'insert-parentheses cmd-insert-parentheses)
  (register-command! 'insert-pair-brackets cmd-insert-pair-brackets)
  (register-command! 'insert-pair-braces cmd-insert-pair-braces)
  (register-command! 'insert-pair-quotes cmd-insert-pair-quotes)
  ;; Describe char
  (register-command! 'describe-char cmd-describe-char)
  ;; Find file at point
  (register-command! 'find-file-at-point cmd-find-file-at-point)
  (register-command! 'ffap cmd-find-file-at-point)
  ;; Auto-fill-mode alias
  (register-command! 'auto-fill-mode cmd-toggle-auto-fill)
  ;; Count chars region
  (register-command! 'count-chars-region cmd-count-chars-region)
  ;; Capitalize region
  (register-command! 'capitalize-region cmd-capitalize-region)
  ;; Count words buffer
  (register-command! 'count-words-buffer cmd-count-words-buffer)
  ;; Unfill paragraph
  (register-command! 'unfill-paragraph cmd-unfill-paragraph)
  ;; List registers
  (register-command! 'list-registers cmd-list-registers)
  ;; Show kill ring
  (register-command! 'show-kill-ring cmd-show-kill-ring)
  ;; Smart beginning of line
  (register-command! 'smart-beginning-of-line cmd-smart-beginning-of-line)
  ;; What buffer
  (register-command! 'what-buffer cmd-what-buffer)
  ;; Toggle narrowing indicator
  (register-command! 'toggle-narrowing-indicator cmd-toggle-narrowing-indicator)
  ;; Insert file name
  (register-command! 'insert-file-name cmd-insert-file-name)
  ;; S-expression navigation
  (register-command! 'backward-up-list cmd-backward-up-list)
  (register-command! 'forward-up-list cmd-forward-up-list)
  (register-command! 'kill-sexp cmd-kill-sexp)
  (register-command! 'backward-sexp cmd-backward-sexp)
  (register-command! 'forward-sexp cmd-forward-sexp)
  ;; Transpose sexps
  (register-command! 'transpose-sexps cmd-transpose-sexps)
  ;; Mark sexp
  (register-command! 'mark-sexp cmd-mark-sexp)
  ;; Indent sexp
  (register-command! 'indent-sexp cmd-indent-sexp)
  ;; Word frequency
  (register-command! 'word-frequency cmd-word-frequency)
  ;; Insert UUID
  (register-command! 'insert-uuid cmd-insert-uuid)
  ;; Delete pair
  (register-command! 'delete-pair cmd-delete-pair)
  ;; Toggle hl-line
  (register-command! 'toggle-hl-line cmd-toggle-hl-line)
  ;; Find alternate file
  (register-command! 'find-alternate-file cmd-find-alternate-file)
  ;; Increment register
  (register-command! 'increment-register cmd-increment-register)
  ;; Copy buffer name
  (register-command! 'copy-buffer-name cmd-copy-buffer-name)
  ;; Sort lines case-insensitive
  (register-command! 'sort-lines-case-fold cmd-sort-lines-case-fold)
  ;; Reverse chars in region
  (register-command! 'reverse-chars cmd-reverse-chars)
  ;; Replace regexp
  (register-command! 'replace-string-all cmd-replace-string-all)
  ;; Insert file contents
  (register-command! 'insert-file-contents cmd-insert-file-contents)
  ;; Auto revert
  (register-command! 'toggle-auto-revert cmd-toggle-auto-revert)
  ;; Zap up to char
  (register-command! 'zap-up-to-char cmd-zap-up-to-char)
  ;; Quoted insert
  (register-command! 'quoted-insert cmd-quoted-insert)
  ;; What line/col
  (register-command! 'what-line-col cmd-what-line-col)
  ;; Insert ISO date
  (register-command! 'insert-current-date-iso cmd-insert-current-date-iso)
  ;; Recenter top/bottom
  (register-command! 'recenter-top cmd-recenter-top)
  (register-command! 'recenter-bottom cmd-recenter-bottom)
  ;; Scroll other window
  (register-command! 'scroll-other-window cmd-scroll-other-window)
  (register-command! 'scroll-other-window-up cmd-scroll-other-window-up)
  ;; Count words paragraph
  (register-command! 'count-words-paragraph cmd-count-words-paragraph)
  ;; Toggle transient mark
  (register-command! 'toggle-transient-mark cmd-toggle-transient-mark)
  ;; Keep/flush lines region
  (register-command! 'keep-lines-region cmd-keep-lines-region)
  (register-command! 'flush-lines-region cmd-flush-lines-region)
  ;; Insert register string
  (register-command! 'insert-register-string cmd-insert-register-string)
  ;; Visible bell
  (register-command! 'toggle-visible-bell cmd-toggle-visible-bell)
  ;; Unindent region (indent-region already registered above)
  (register-command! 'unindent-region cmd-unindent-region)
  ;; Copy region as kill
  (register-command! 'copy-region-as-kill cmd-copy-region-as-kill)
  ;; Append to buffer
  (register-command! 'append-to-buffer cmd-append-to-buffer)
  ;; Toggle trailing whitespace display
  (register-command! 'toggle-show-trailing-whitespace cmd-toggle-show-trailing-whitespace)
  ;; Backward kill sexp
  (register-command! 'backward-kill-sexp cmd-backward-kill-sexp)
  ;; Mark whole buffer
  (register-command! 'mark-whole-buffer cmd-mark-whole-buffer)
  ;; Cycle spacing
  (register-command! 'cycle-spacing cmd-cycle-spacing)
  ;; Delete horizontal space forward
  (register-command! 'delete-horizontal-space-forward cmd-delete-horizontal-space-forward)
  ;; Debug mode
  (register-command! 'toggle-debug-mode cmd-toggle-debug-mode)
  ;; Insert comment separator
  (register-command! 'insert-comment-separator cmd-insert-comment-separator)
  ;; Global hl-line
  (register-command! 'toggle-global-hl-line cmd-toggle-global-hl-line)
  ;; Insert shebang
  (register-command! 'insert-shebang cmd-insert-shebang)
  ;; Toggle auto indent
  (register-command! 'toggle-auto-indent cmd-toggle-auto-indent)
  ;; What mode
  (register-command! 'what-mode cmd-what-mode)
  ;; Show buffer size
  (register-command! 'show-buffer-size cmd-show-buffer-size)
  ;; Goto percent
  (register-command! 'goto-percent cmd-goto-percent)
  ;; Insert newline above/below
  (register-command! 'insert-newline-below cmd-insert-newline-below)
  (register-command! 'insert-newline-above cmd-insert-newline-above)
  ;; Duplicate region
  (register-command! 'duplicate-region cmd-duplicate-region)
  ;; Sort lines reverse
  (register-command! 'sort-lines-reverse cmd-sort-lines-reverse)
  ;; Uniquify lines
  (register-command! 'uniquify-lines cmd-uniquify-lines)
  ;; Show line endings
  (register-command! 'show-line-endings cmd-show-line-endings)
  ;; Comment/uncomment
  (register-command! 'comment-region cmd-comment-region)
  (register-command! 'uncomment-region cmd-uncomment-region)
  ;; Case at point
  (register-command! 'upcase-char cmd-upcase-char)
  (register-command! 'downcase-char cmd-downcase-char)
  (register-command! 'toggle-case-at-point cmd-toggle-case-at-point)
  ;; Write region
  (register-command! 'write-region cmd-write-region)
  ;; Kill matching buffers
  (register-command! 'kill-matching-buffers cmd-kill-matching-buffers)
  ;; Relative goto
  (register-command! 'goto-line-relative cmd-goto-line-relative)
  ;; Bookmark management
  (register-command! 'bookmark-delete cmd-bookmark-delete)
  (register-command! 'bookmark-rename cmd-bookmark-rename)
  ;; Mode info (toggle-window-dedicated, shrink/enlarge-window-horizontally, toggle-line-move-visual already registered)
  (register-command! 'describe-mode cmd-describe-mode)
  ;; Trailing lines
  (register-command! 'delete-trailing-lines cmd-delete-trailing-lines)
  ;; Line numbers
  (register-command! 'display-line-numbers-relative cmd-display-line-numbers-relative)
  ;; Column
  (register-command! 'goto-column cmd-goto-column)
  ;; Insert helpers
  (register-command! 'insert-line-number cmd-insert-line-number)
  (register-command! 'insert-buffer-filename cmd-insert-buffer-filename)
  ;; Copy helpers
  (register-command! 'copy-line-number cmd-copy-line-number)
  (register-command! 'copy-current-line cmd-copy-current-line)
  (register-command! 'copy-word cmd-copy-word)
  ;; Window position movement
  (register-command! 'move-to-window-top cmd-move-to-window-top)
  (register-command! 'move-to-window-bottom cmd-move-to-window-bottom)
  (register-command! 'move-to-window-middle cmd-move-to-window-middle)
  ;; Scrolling
  (register-command! 'scroll-left cmd-scroll-left)
  (register-command! 'scroll-right cmd-scroll-right)
  ;; Delete without kill
  (register-command! 'delete-to-end-of-line cmd-delete-to-end-of-line)
  (register-command! 'delete-to-beginning-of-line cmd-delete-to-beginning-of-line)
  ;; Yank line
  (register-command! 'yank-whole-line cmd-yank-whole-line)
  ;; Info
  (register-command! 'show-column-number cmd-show-column-number)
  (register-command! 'count-lines-buffer cmd-count-lines-buffer)
  ;; File management stubs (toggle-auto-save already registered)
  (register-command! 'recover-session cmd-recover-session)
  (register-command! 'toggle-backup-files cmd-toggle-backup-files)
  ;; Case conversion
  (register-command! 'camel-to-snake cmd-camel-to-snake)
  (register-command! 'snake-to-camel cmd-snake-to-camel)
  (register-command! 'kebab-to-camel cmd-kebab-to-camel)
  ;; Word operations
  (register-command! 'reverse-word cmd-reverse-word)
  (register-command! 'sort-words cmd-sort-words)
  ;; Counting
  (register-command! 'count-occurrences cmd-count-occurrences)
  (register-command! 'mark-lines-matching cmd-mark-lines-matching)
  (register-command! 'show-word-count cmd-show-word-count)
  (register-command! 'show-char-count cmd-show-char-count)
  (register-command! 'show-trailing-whitespace-count cmd-show-trailing-whitespace-count)
  (register-command! 'show-tab-count cmd-show-tab-count)
  ;; Line manipulation
  (register-command! 'number-region cmd-number-region)
  (register-command! 'strip-line-numbers cmd-strip-line-numbers)
  (register-command! 'prefix-lines cmd-prefix-lines)
  (register-command! 'suffix-lines cmd-suffix-lines)
  (register-command! 'remove-blank-lines cmd-remove-blank-lines)
  (register-command! 'collapse-blank-lines cmd-collapse-blank-lines)
  (register-command! 'trim-lines cmd-trim-lines)
  ;; Wrapping
  (register-command! 'wrap-lines-at-column cmd-wrap-lines-at-column)
  ;; Comments
  (register-command! 'toggle-line-comment cmd-toggle-line-comment)
  (register-command! 'insert-box-comment cmd-insert-box-comment)
  ;; File info
  (register-command! 'show-file-info cmd-show-file-info)
  (register-command! 'copy-file-path cmd-copy-file-path)
  ;; Insert
  (register-command! 'insert-timestamp cmd-insert-timestamp)
  (register-command! 'insert-lorem-ipsum cmd-insert-lorem-ipsum)
  (register-command! 'insert-path-separator cmd-insert-path-separator)
  ;; Eval and shell
  (register-command! 'eval-and-insert cmd-eval-and-insert)
  (register-command! 'shell-command-insert cmd-shell-command-insert)
  (register-command! 'pipe-region cmd-pipe-region)
  ;; Toggles/stubs
  (register-command! 'toggle-narrow-indicator cmd-toggle-narrow-indicator)
  (register-command! 'toggle-auto-complete cmd-toggle-auto-complete)
  (register-command! 'toggle-electric-indent cmd-toggle-electric-indent)
  (register-command! 'toggle-global-whitespace cmd-toggle-global-whitespace)
  ;; Narrow
  (register-command! 'narrow-to-defun cmd-narrow-to-defun)
  (register-command! 'widen-all cmd-widen-all)
  ;; Reindent
  (register-command! 'reindent-buffer cmd-reindent-buffer)
  ;; Font size aliases
  (register-command! 'increase-font-size cmd-increase-font-size)
  (register-command! 'decrease-font-size cmd-decrease-font-size)
  (register-command! 'reset-font-size cmd-reset-font-size)
  ;; Project
  (register-command! 'project-find-file cmd-project-find-file)
  (register-command! 'project-grep cmd-project-grep)
  (register-command! 'project-compile cmd-project-compile)
  ;; Word search
  (register-command! 'search-forward-word cmd-search-forward-word)
  (register-command! 'search-backward-word cmd-search-backward-word)
  (register-command! 'highlight-word-at-point cmd-highlight-word-at-point)
  ;; Replace in region
  (register-command! 'replace-in-region cmd-replace-in-region)
  ;; Go to definition
  (register-command! 'goto-definition cmd-goto-definition)
  ;; Frame stubs
  (register-command! 'toggle-eol-conversion cmd-toggle-eol-conversion)
  (register-command! 'make-frame cmd-make-frame)
  (register-command! 'delete-frame cmd-delete-frame)
  (register-command! 'toggle-menu-bar cmd-toggle-menu-bar)
  (register-command! 'toggle-tool-bar cmd-toggle-tool-bar)
  (register-command! 'toggle-scroll-bar cmd-toggle-scroll-bar)
  (register-command! 'suspend-frame cmd-suspend-frame)
  ;; Directory
  (register-command! 'list-directory cmd-list-directory)
  (register-command! 'find-grep cmd-find-grep)
  ;; C/C++ helpers
  (register-command! 'insert-header-guard cmd-insert-header-guard)
  (register-command! 'insert-include cmd-insert-include)
  ;; Scheme helpers
  (register-command! 'insert-import cmd-insert-import)
  (register-command! 'insert-export cmd-insert-export)
  (register-command! 'insert-defun cmd-insert-defun)
  (register-command! 'insert-let cmd-insert-let)
  (register-command! 'insert-cond cmd-insert-cond)
  (register-command! 'insert-match cmd-insert-match)
  (register-command! 'insert-when cmd-insert-when)
  (register-command! 'insert-unless cmd-insert-unless)
  (register-command! 'insert-lambda cmd-insert-lambda)
  ;; Toggles
  (register-command! 'toggle-auto-pair-mode cmd-toggle-auto-pair-mode)
  (register-command! 'toggle-flyspell cmd-toggle-flyspell)
  (register-command! 'toggle-flymake cmd-toggle-flymake)
  (register-command! 'toggle-lsp cmd-toggle-lsp)
  (register-command! 'lsp cmd-toggle-lsp)   ; alias: M-x lsp
  (register-command! 'toggle-auto-revert-global cmd-toggle-auto-revert-global)
  ;; Buffer info
  (register-command! 'count-buffers cmd-count-buffers)
  (register-command! 'list-recent-files cmd-list-recent-files)
  (register-command! 'clear-recent-files cmd-clear-recent-files)
  ;; Key info
  (register-command! 'show-keybinding-for cmd-show-keybinding-for)
  ;; Sort imports
  (register-command! 'sort-imports cmd-sort-imports)
  ;; Git
  (register-command! 'show-git-status cmd-show-git-status)
  (register-command! 'show-git-log cmd-show-git-log)
  (register-command! 'show-git-diff cmd-show-git-diff)
  (register-command! 'show-git-blame cmd-show-git-blame)
  ;; Misc
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'quit cmd-quit)
  ;; Task #44: Help system
  (register-command! 'describe-function cmd-describe-function)
  (register-command! 'describe-variable cmd-describe-variable)
  (register-command! 'describe-key-briefly cmd-describe-key-briefly)
  (register-command! 'view-lossage cmd-view-lossage)
  (register-command! 'describe-face cmd-describe-face)
  (register-command! 'describe-syntax cmd-describe-syntax)
  (register-command! 'info cmd-info)
  (register-command! 'info-emacs-manual cmd-info-emacs-manual)
  (register-command! 'info-elisp-manual cmd-info-elisp-manual)
  ;; Dired
  (register-command! 'dired cmd-dired)
  (register-command! 'dired-create-directory cmd-dired-create-directory)
  (register-command! 'dired-do-rename cmd-dired-do-rename)
  (register-command! 'dired-do-delete cmd-dired-do-delete)
  (register-command! 'dired-do-copy cmd-dired-do-copy)
  (register-command! 'dired-do-chmod cmd-dired-do-chmod)
  (register-command! 'dired-find-file cmd-dired-find-file)
  ;; Buffer management
  (register-command! 'rename-uniquely cmd-rename-uniquely)
  (register-command! 'revert-buffer-with-coding cmd-revert-buffer-with-coding)
  (register-command! 'lock-buffer cmd-lock-buffer)
  (register-command! 'buffer-disable-undo cmd-buffer-disable-undo)
  (register-command! 'buffer-enable-undo cmd-buffer-enable-undo)
  (register-command! 'bury-buffer cmd-bury-buffer)
  (register-command! 'unbury-buffer cmd-unbury-buffer)
  ;; Navigation
  (register-command! 'forward-sentence cmd-forward-sentence)
  (register-command! 'backward-sentence cmd-backward-sentence)
  (register-command! 'goto-word-at-point cmd-goto-word-at-point)
  ;; Region operations
  ;; Text manipulation
  (register-command! 'center-region cmd-center-region)
  (register-command! 'indent-rigidly cmd-indent-rigidly)
  (register-command! 'dedent-rigidly cmd-dedent-rigidly)
  (register-command! 'transpose-paragraphs cmd-transpose-paragraphs)
  (register-command! 'fill-individual-paragraphs cmd-fill-individual-paragraphs)
  ;; Bookmarks
  (register-command! 'bookmark-save cmd-bookmark-save)
  (register-command! 'bookmark-load cmd-bookmark-load)
  ;; Window management
  (register-command! 'fit-window-to-buffer cmd-fit-window-to-buffer)
  (register-command! 'maximize-window cmd-maximize-window)
  (register-command! 'minimize-window cmd-minimize-window)
  (register-command! 'rotate-windows cmd-rotate-windows)
  (register-command! 'swap-windows cmd-swap-windows)
  ;; Misc
  (register-command! 'delete-matching-lines cmd-delete-matching-lines)
  (register-command! 'copy-matching-lines cmd-copy-matching-lines)
  (register-command! 'delete-non-matching-lines cmd-delete-non-matching-lines)
  (register-command! 'display-fill-column-indicator cmd-display-fill-column-indicator)
  (register-command! 'electric-newline-and-indent cmd-electric-newline-and-indent)
  ;; Registers
  (register-command! 'view-register cmd-view-register)
  (register-command! 'append-to-register cmd-append-to-register)
  ;; Environment
  (register-command! 'getenv cmd-getenv)
  (register-command! 'setenv cmd-setenv)
  (register-command! 'show-environment cmd-show-environment)
  ;; Encoding
  (register-command! 'set-buffer-file-coding cmd-set-buffer-file-coding)
  (register-command! 'convert-line-endings-unix cmd-convert-line-endings-unix)
  (register-command! 'convert-line-endings-dos cmd-convert-line-endings-dos)
  ;; Completion
  ;; Whitespace
  (register-command! 'whitespace-mode cmd-whitespace-mode)
  (register-command! 'toggle-show-spaces cmd-toggle-show-spaces)
  ;; Folding
  (register-command! 'fold-all cmd-fold-all)
  (register-command! 'unfold-all cmd-unfold-all)
  (register-command! 'toggle-fold cmd-toggle-fold)
  (register-command! 'fold-level cmd-fold-level)
  ;; Macros
  (register-command! 'name-last-kbd-macro cmd-name-last-kbd-macro)
  (register-command! 'insert-kbd-macro cmd-insert-kbd-macro)
  ;; VC extras
  (register-command! 'vc-annotate cmd-vc-annotate)
  (register-command! 'vc-diff-head cmd-vc-diff-head)
  (register-command! 'vc-log-file cmd-vc-log-file)
  (register-command! 'vc-revert cmd-vc-revert)
  ;; Imenu
  (register-command! 'imenu cmd-imenu)
  (register-command! 'which-function cmd-which-function)
  ;; File utilities
  (register-command! 'make-directory cmd-make-directory)
  (register-command! 'delete-file cmd-delete-file)
  (register-command! 'copy-file cmd-copy-file)
  (register-command! 'sudo-find-file cmd-sudo-find-file)
  (register-command! 'find-file-literally cmd-find-file-literally)
  ;; Task #45: isearch, abbrev, editing utilities
  (register-command! 'isearch-forward-word cmd-isearch-forward-word)
  (register-command! 'isearch-backward-word cmd-isearch-backward-word)
  (register-command! 'isearch-forward-symbol cmd-isearch-forward-symbol)
  (register-command! 'query-replace-regexp cmd-query-replace-regexp)
  (register-command! 'multi-occur cmd-multi-occur)
  (register-command! 'align-current cmd-align-current)
  (register-command! 'clear-rectangle cmd-clear-rectangle)
  (register-command! 'abbrev-mode cmd-abbrev-mode)
  (register-command! 'define-abbrev cmd-define-abbrev)
  (register-command! 'expand-abbrev cmd-expand-abbrev)
  (register-command! 'list-abbrevs cmd-list-abbrevs)
  (register-command! 'completion-at-point cmd-completion-at-point)
  (register-command! 'complete-filename cmd-complete-filename)
  (register-command! 'resize-window-width cmd-resize-window-width)
  (register-command! 'zap-to-char-inclusive cmd-zap-to-char-inclusive)
  (register-command! 'copy-word-at-point cmd-copy-word-at-point)
  (register-command! 'copy-symbol-at-point cmd-copy-symbol-at-point)
  (register-command! 'mark-page cmd-mark-page)
  (register-command! 'toggle-input-method cmd-toggle-input-method)
  (register-command! 'set-language-environment cmd-set-language-environment)
  (register-command! 'load-theme cmd-load-theme)
  (register-command! 'customize-face cmd-customize-face)
  (register-command! 'list-colors cmd-list-colors)
  (register-command! 'font-lock-mode cmd-font-lock-mode)
  (register-command! 'auto-revert-mode cmd-auto-revert-mode)
  (register-command! 'diff-backup cmd-diff-backup)
  (register-command! 'first-error cmd-first-error)
  (register-command! 'quick-calc cmd-quick-calc)
  (register-command! 'insert-time cmd-insert-time)
  (register-command! 'insert-file-header cmd-insert-file-header)
  (register-command! 'toggle-debug-on-quit cmd-toggle-debug-on-quit)
  (register-command! 'profiler-start cmd-profiler-start)
  (register-command! 'profiler-stop cmd-profiler-stop)
  (register-command! 'memory-report cmd-memory-report)
  (register-command! 'emacs-version cmd-emacs-version)
  (register-command! 'report-bug cmd-report-bug)
  (register-command! 'view-echo-area-messages cmd-view-echo-area-messages)
  (register-command! 'toggle-menu-bar-mode cmd-toggle-menu-bar-mode)
  (register-command! 'toggle-tab-bar-mode cmd-toggle-tab-bar-mode)
  (register-command! 'split-window-below cmd-split-window-below)
  (register-command! 'delete-window-below cmd-delete-window-below)
  (register-command! 'shrink-window-if-larger-than-buffer cmd-shrink-window-if-larger-than-buffer)
  (register-command! 'toggle-frame-fullscreen cmd-toggle-frame-fullscreen)
  (register-command! 'toggle-frame-maximized cmd-toggle-frame-maximized)
  (register-command! 'ispell-word cmd-ispell-word)
  (register-command! 'ispell-buffer cmd-ispell-buffer)
  (register-command! 'ispell-region cmd-ispell-region)
  (register-command! 'term cmd-term)
  (register-command! 'ansi-term cmd-ansi-term)
  ;; Persistence: recentf, desktop, savehist
  (register-command! 'recentf-open cmd-recentf-open)
  (register-command! 'recentf-cleanup cmd-recentf-cleanup)
  (register-command! 'desktop-save cmd-desktop-save)
  (register-command! 'desktop-read cmd-desktop-read)
  (register-command! 'savehist-save cmd-savehist-save)
  (register-command! 'savehist-load cmd-savehist-load)
  ;; Scroll margin
  (register-command! 'set-scroll-margin cmd-set-scroll-margin)
  (register-command! 'toggle-scroll-margin cmd-toggle-scroll-margin)
  ;; Init file
  (register-command! 'load-init-file cmd-load-init-file)
  (register-command! 'find-init-file cmd-find-init-file)
  ;; Save-place
  (register-command! 'save-place-mode cmd-toggle-save-place-mode)
  (register-command! 'toggle-save-place-mode cmd-toggle-save-place-mode)
  ;; Clean-on-save
  (register-command! 'toggle-delete-trailing-whitespace-on-save cmd-toggle-delete-trailing-whitespace-on-save)
  (register-command! 'toggle-require-final-newline cmd-toggle-require-final-newline)
  ;; Centered cursor mode
  (register-command! 'centered-cursor-mode cmd-toggle-centered-cursor-mode)
  (register-command! 'toggle-centered-cursor-mode cmd-toggle-centered-cursor-mode)
  ;; Batch 3: Package/framework aliases (editor-cmds chain functions)
  (register-command! 'projectile-find-file cmd-project-find-file)
  (register-command! 'projectile-grep cmd-project-grep)
  (register-command! 'projectile-mode cmd-project-find-file)
  (register-command! 'projectile-run-project cmd-project-compile)
  (register-command! 'projectile-test-project cmd-project-compile)
  (register-command! 'helm-M-x cmd-execute-extended-command)
  (register-command! 'helm-find-files cmd-find-file)
  (register-command! 'helm-recentf cmd-recentf-open)
  (register-command! 'counsel-M-x cmd-execute-extended-command)
  (register-command! 'counsel-find-file cmd-find-file)
  (register-command! 'counsel-grep cmd-grep)
  (register-command! 'counsel-git-grep cmd-project-grep)
  (register-command! 'ivy-switch-buffer cmd-switch-buffer)
  (register-command! 'auto-complete cmd-complete-at-point)
  (register-command! 'paredit-mode cmd-toggle-auto-pair-mode)
  (register-command! 'smartparens-mode cmd-toggle-auto-pair-mode)
  (register-command! 'select-window-1 cmd-other-window)
  (register-command! 'select-window-2 cmd-other-window)
  (register-command! 'emmet-mode cmd-complete-at-point)
  (register-command! 'emmet-expand-line cmd-complete-at-point)
  ;; Batch 7: kmacro aliases (need editor-cmds scope)
  (register-command! 'kmacro-name-last-macro cmd-name-last-kbd-macro)
  (register-command! 'kmacro-edit-macro cmd-name-last-kbd-macro)
  ;; Batch 9: compilation alias (needs editor-ui scope)
  (register-command! 'compilation-minor-mode cmd-compile)
  ;; Batch 10: project aliases (needs editor-ui scope)
  (register-command! 'project-execute-extended-command cmd-execute-extended-command)
  (register-command! 'project-any-command cmd-execute-extended-command)
  ;; Batch 11: helm alias (needs editor-ui scope)
  (register-command! 'helm-occur cmd-occur)
  ;; Batch 13: aliases needing editor.ss scope
  (register-command! 'apropos-variable cmd-apropos-command)
  ;; Task #46+ (in editor-extra.ss)
  (register-extra-commands!))
