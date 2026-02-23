;;; -*- Gerbil -*-
;;; Parity registrations: commands defined in editor-*.ss but not yet
;;; registered with register-command! in the TUI layer.
;;; These commands already have full implementations — this module just
;;; exposes them to M-x by name, matching their Qt registrations.

(export register-parity-commands!
        cmd-dired-jump cmd-dired-up-directory cmd-dired-do-shell-command
        cmd-apropos-emacs cmd-indent-new-comment-line
        cmd-isearch-backward-regexp cmd-replace-regexp
        cmd-org-capture cmd-org-capture-finalize cmd-org-capture-abort
        cmd-org-refile cmd-org-time-stamp
        cmd-org-insert-link cmd-org-narrow-to-subtree cmd-org-sort
        cmd-project-switch-to-buffer cmd-project-kill-buffers
        cmd-vc-next-action
        ;; Batch 2
        cmd-sort-numeric-fields cmd-find-dired cmd-find-name-dired
        cmd-dired-hide-details cmd-desktop-save-mode
        cmd-org-babel-execute-src-block cmd-org-babel-tangle cmd-org-babel-kill-session
        cmd-other-frame cmd-winum-mode cmd-help-with-tutorial
        cmd-cua-mode cmd-org-archive-subtree cmd-org-toggle-heading
        cmd-magit-init cmd-magit-tag
        ;; Batch 4
        cmd-check-parens cmd-count-lines-page cmd-how-many
        ;; Batch 5
        cmd-delete-directory cmd-set-file-modes cmd-dired-do-chown cmd-butterfly
        ;; Batch 7
        cmd-debug-on-entry cmd-cancel-debug-on-entry
        ;; Batch 12
        register-batch12-aliases!
        ;; iedit
        cmd-iedit-mode
        ;; multi-terminal
        cmd-term-list cmd-term-next cmd-term-prev
        ;; EWW bookmarks
        cmd-eww-add-bookmark cmd-eww-list-bookmarks
        ;; Forge (GitHub)
        cmd-forge-list-prs cmd-forge-create-pr
        cmd-forge-list-issues cmd-forge-view-pr)

(import :std/sugar
        :std/srfi/1
        :std/srfi/13
        :std/sort
        :std/misc/string
        :std/misc/process
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gemacs/pregexp-compat pregexp pregexp-match)
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/echo
        (only-in :gemacs/org-babel
                 org-babel-find-src-block org-babel-execute
                 org-babel-tangle-to-files org-babel-insert-result
                 org-babel-kill-all-sessions)
        (only-in :gemacs/org-capture
                 org-capture-menu-string org-capture-template-key
                 org-capture-template-template org-capture-cursor-position
                 org-capture-start org-capture-finalize org-capture-abort
                 *org-capture-templates*)
        (only-in :gemacs/org-parse
                 org-heading-line? org-heading-stars-of-line)
        :gemacs/editor-core
        :gemacs/editor-text
        :gemacs/editor-ui
        :gemacs/editor-advanced
        :gemacs/editor-cmds-a
        :gemacs/editor-cmds-b
        :gemacs/editor-cmds-c
        (only-in :gemacs/editor-extra-helpers cmd-flyspell-mode)
        (only-in :gemacs/terminal terminal-buffer?)
        (only-in :gemacs/editor-extra-web
                 *eww-current-url* eww-display-page eww-fetch-url))

(def (register-parity-commands!)
  ;;; From editor-core.ss
  (register-command! 'backward-char cmd-backward-char)
  (register-command! 'backward-delete-char cmd-backward-delete-char)
  (register-command! 'backward-delete-char-untabify cmd-backward-delete-char-untabify)
  (register-command! 'backward-word cmd-backward-word)
  (register-command! 'beginning-of-buffer cmd-beginning-of-buffer)
  (register-command! 'beginning-of-line cmd-beginning-of-line)
  (register-command! 'copy-region cmd-copy-region)
  (register-command! 'delete-char cmd-delete-char)
  (register-command! 'delete-other-windows cmd-delete-other-windows)
  (register-command! 'delete-window cmd-delete-window)
  (register-command! 'dired-find-file cmd-dired-find-file)
  (register-command! 'end-of-buffer cmd-end-of-buffer)
  (register-command! 'end-of-line cmd-end-of-line)
  (register-command! 'eshell cmd-eshell)
  (register-command! 'find-file cmd-find-file)
  (register-command! 'forward-char cmd-forward-char)
  (register-command! 'forward-word cmd-forward-word)
  (register-command! 'kill-buffer-cmd cmd-kill-buffer-cmd)
  (register-command! 'kill-line cmd-kill-line)
  (register-command! 'kill-region cmd-kill-region)
  (register-command! 'newline cmd-newline)
  (register-command! 'next-line cmd-next-line)
  (register-command! 'open-line cmd-open-line)
  (register-command! 'other-window cmd-other-window)
  (register-command! 'previous-line cmd-previous-line)
  (register-command! 'recenter cmd-recenter)
  (register-command! 'redo cmd-redo)
  (register-command! 'repl cmd-repl)
  (register-command! 'revert-buffer cmd-revert-buffer)
  (register-command! 'save-buffer cmd-save-buffer)
  (register-command! 'scroll-down cmd-scroll-down)
  (register-command! 'scroll-up cmd-scroll-up)
  (register-command! 'search-backward cmd-search-backward)
  (register-command! 'search-forward cmd-search-forward)
  (register-command! 'set-mark cmd-set-mark)
  (register-command! 'shell cmd-shell)
  (register-command! 'split-window cmd-split-window)
  (register-command! 'split-window-right cmd-split-window-right)
  (register-command! 'switch-buffer cmd-switch-buffer)
  (register-command! 'tab-to-tab-stop cmd-tab-to-tab-stop)
  (register-command! 'term cmd-term)
  (register-command! 'term-interrupt cmd-term-interrupt)
  (register-command! 'term-send-eof cmd-term-send-eof)
  (register-command! 'term-send-tab cmd-term-send-tab)
  (register-command! 'term-list cmd-term-list)
  (register-command! 'term-next cmd-term-next)
  (register-command! 'term-prev cmd-term-prev)
  (register-command! 'undo cmd-undo)
  (register-command! 'write-file cmd-write-file)
  (register-command! 'yank cmd-yank)
  ;;; From editor-text.ss
  (register-command! 'align-regexp cmd-align-regexp)
  (register-command! 'back-to-indentation cmd-back-to-indentation)
  (register-command! 'backward-kill-word cmd-backward-kill-word)
  (register-command! 'backward-paragraph cmd-backward-paragraph)
  (register-command! 'bookmark-jump cmd-bookmark-jump)
  (register-command! 'bookmark-list cmd-bookmark-list)
  (register-command! 'bookmark-set cmd-bookmark-set)
  (register-command! 'call-last-kbd-macro cmd-call-last-kbd-macro)
  (register-command! 'complete-at-point cmd-complete-at-point)
  (register-command! 'copy-to-register cmd-copy-to-register)
  (register-command! 'dabbrev-expand cmd-dabbrev-expand)
  (register-command! 'delete-blank-lines cmd-delete-blank-lines)
  (register-command! 'delete-indentation cmd-delete-indentation)
  (register-command! 'delete-rectangle cmd-delete-rectangle)
  (register-command! 'downcase-region cmd-downcase-region)
  (register-command! 'end-kbd-macro cmd-end-kbd-macro)
  (register-command! 'fill-paragraph cmd-fill-paragraph)
  (register-command! 'fixup-whitespace cmd-fixup-whitespace)
  (register-command! 'flush-lines cmd-flush-lines)
  (register-command! 'forward-paragraph cmd-forward-paragraph)
  (register-command! 'goto-char cmd-goto-char)
  (register-command! 'goto-matching-paren cmd-goto-matching-paren)
  (register-command! 'grep cmd-grep)
  (register-command! 'indent-region cmd-indent-region)
  (register-command! 'insert-file cmd-insert-file)
  (register-command! 'insert-register cmd-insert-register)
  (register-command! 'join-line cmd-join-line)
  (register-command! 'jump-to-register cmd-jump-to-register)
  (register-command! 'just-one-space cmd-just-one-space)
  (register-command! 'keep-lines cmd-keep-lines)
  (register-command! 'kill-rectangle cmd-kill-rectangle)
  (register-command! 'kill-whole-line cmd-kill-whole-line)
  (register-command! 'mark-paragraph cmd-mark-paragraph)
  (register-command! 'mark-word cmd-mark-word)
  (register-command! 'move-line-down cmd-move-line-down)
  (register-command! 'move-line-up cmd-move-line-up)
  (register-command! 'narrow-to-region cmd-narrow-to-region)
  (register-command! 'number-lines cmd-number-lines)
  (register-command! 'open-rectangle cmd-open-rectangle)
  (register-command! 'pipe-buffer cmd-pipe-buffer)
  (register-command! 'point-to-register cmd-point-to-register)
  (register-command! 'pop-mark cmd-pop-mark)
  (register-command! 'previous-error cmd-previous-error)
  (register-command! 'repeat cmd-repeat)
  (register-command! 'replace-string cmd-replace-string)
  (register-command! 'reverse-region cmd-reverse-region)
  (register-command! 'shell-command cmd-shell-command)
  (register-command! 'sort-fields cmd-sort-fields)
  (register-command! 'sort-lines cmd-sort-lines)
  (register-command! 'start-kbd-macro cmd-start-kbd-macro)
  (register-command! 'string-rectangle cmd-string-rectangle)
  (register-command! 'transpose-lines cmd-transpose-lines)
  (register-command! 'transpose-words cmd-transpose-words)
  (register-command! 'upcase-region cmd-upcase-region)
  (register-command! 'what-cursor-position cmd-what-cursor-position)
  (register-command! 'widen cmd-widen)
  (register-command! 'yank-rectangle cmd-yank-rectangle)
  (register-command! 'zap-to-char cmd-zap-to-char)
  ;;; From editor-ui.ss
  (register-command! 'beginning-of-defun cmd-beginning-of-defun)
  (register-command! 'capitalize-word cmd-capitalize-word)
  (register-command! 'compile cmd-compile)
  (register-command! 'count-words cmd-count-words)
  (register-command! 'delete-trailing-whitespace cmd-delete-trailing-whitespace)
  (register-command! 'describe-command cmd-describe-command)
  (register-command! 'describe-key cmd-describe-key)
  (register-command! 'downcase-word cmd-downcase-word)
  (register-command! 'duplicate-line cmd-duplicate-line)
  (register-command! 'end-of-defun cmd-end-of-defun)
  (register-command! 'eval-expression cmd-eval-expression)
  (register-command! 'execute-extended-command cmd-execute-extended-command)
  (register-command! 'goto-line cmd-goto-line)
  (register-command! 'indent-or-complete cmd-indent-or-complete)
  (register-command! 'keyboard-quit cmd-keyboard-quit)
  (register-command! 'kill-word cmd-kill-word)
  (register-command! 'load-file cmd-load-file)
  (register-command! 'list-bindings cmd-list-bindings)
  (register-command! 'list-buffers cmd-list-buffers)
  (register-command! 'occur cmd-occur)
  (register-command! 'query-replace cmd-query-replace)
  (register-command! 'quit cmd-quit)
  (register-command! 'select-all cmd-select-all)
  (register-command! 'shell-command-on-region cmd-shell-command-on-region)
  (register-command! 'toggle-comment cmd-toggle-comment)
  (register-command! 'toggle-line-numbers cmd-toggle-line-numbers)
  (register-command! 'toggle-whitespace cmd-toggle-whitespace)
  (register-command! 'toggle-word-wrap cmd-toggle-word-wrap)
  (register-command! 'transpose-chars cmd-transpose-chars)
  (register-command! 'upcase-word cmd-upcase-word)
  (register-command! 'what-line cmd-what-line)
  (register-command! 'yank-pop cmd-yank-pop)
  (register-command! 'zoom-in cmd-zoom-in)
  (register-command! 'zoom-out cmd-zoom-out)
  (register-command! 'zoom-reset cmd-zoom-reset)
  ;;; From editor-advanced.ss
  (register-command! 'apropos-command cmd-apropos-command)
  (register-command! 'async-shell-command cmd-async-shell-command)
  (register-command! 'base64-decode-region cmd-base64-decode-region)
  (register-command! 'base64-encode-region cmd-base64-encode-region)
  (register-command! 'buffer-info cmd-buffer-info)
  (register-command! 'buffer-stats cmd-buffer-stats)
  (register-command! 'calc cmd-calc)
  (register-command! 'center-line cmd-center-line)
  (register-command! 'checksum cmd-checksum)
  (register-command! 'clear-highlight cmd-clear-highlight)
  (register-command! 'clone-buffer cmd-clone-buffer)
  (register-command! 'convert-to-dos cmd-convert-to-dos)
  (register-command! 'convert-to-unix cmd-convert-to-unix)
  (register-command! 'copy-from-above cmd-copy-from-above)
  (register-command! 'copy-from-below cmd-copy-from-below)
  (register-command! 'copy-line cmd-copy-line)
  (register-command! 'count-matches cmd-count-matches)
  (register-command! 'count-words-region cmd-count-words-region)
  (register-command! 'cycle-tab-width cmd-cycle-tab-width)
  (register-command! 'delete-duplicate-lines cmd-delete-duplicate-lines)
  (register-command! 'delete-file-and-buffer cmd-delete-file-and-buffer)
  (register-command! 'describe-bindings cmd-describe-bindings)
  (register-command! 'diff-buffer-with-file cmd-diff-buffer-with-file)
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
  (register-command! 'display-time cmd-display-time)
  (register-command! 'ediff-buffers cmd-ediff-buffers)
  (register-command! 'eldoc cmd-eldoc)
  (register-command! 'eval-buffer cmd-eval-buffer)
  (register-command! 'eval-region cmd-eval-region)
  (register-command! 'exchange-point-and-mark cmd-exchange-point-and-mark)
  (register-command! 'find-file-other-window cmd-find-file-other-window)
  (register-command! 'goto-first-non-blank cmd-goto-first-non-blank)
  (register-command! 'goto-last-non-blank cmd-goto-last-non-blank)
  (register-command! 'grep-buffer cmd-grep-buffer)
  (register-command! 'hexl-mode cmd-hexl-mode)
  (register-command! 'highlight-symbol cmd-highlight-symbol)
  (register-command! 'hippie-expand cmd-hippie-expand)
  (register-command! 'indent-rigidly-left cmd-indent-rigidly-left)
  (register-command! 'indent-rigidly-right cmd-indent-rigidly-right)
  (register-command! 'insert-char cmd-insert-char)
  (register-command! 'insert-date cmd-insert-date)
  (register-command! 'list-processes cmd-list-processes)
  (register-command! 'mark-whole-buffer cmd-mark-whole-buffer)
  (register-command! 'open-line-above cmd-open-line-above)
  (register-command! 'pwd cmd-pwd)
  (register-command! 'recenter-top-bottom cmd-recenter-top-bottom)
  (register-command! 'rename-buffer cmd-rename-buffer)
  (register-command! 'rename-file-and-buffer cmd-rename-file-and-buffer)
  (register-command! 'repeat-complex-command cmd-repeat-complex-command)
  (register-command! 'revert-buffer-quick cmd-revert-buffer-quick)
  (register-command! 'rot13-region cmd-rot13-region)
  (register-command! 'save-some-buffers cmd-save-some-buffers)
  (register-command! 'select-line cmd-select-line)
  (register-command! 'set-fill-column cmd-set-fill-column)
  (register-command! 'sort-numeric cmd-sort-numeric)
  (register-command! 'split-line cmd-split-line)
  (register-command! 'sudo-write cmd-sudo-write)
  (register-command! 'swap-buffers cmd-swap-buffers)
  (register-command! 'switch-buffer-other-window cmd-switch-buffer-other-window)
  (register-command! 'tabify cmd-tabify)
  (register-command! 'toggle-auto-fill cmd-toggle-auto-fill)
  (register-command! 'toggle-case-fold-search cmd-toggle-case-fold-search)
  (register-command! 'toggle-debug-on-error cmd-toggle-debug-on-error)
  (register-command! 'toggle-fill-column-indicator cmd-toggle-fill-column-indicator)
  (register-command! 'toggle-highlighting cmd-toggle-highlighting)
  (register-command! 'toggle-indent-tabs-mode cmd-toggle-indent-tabs-mode)
  (register-command! 'toggle-overwrite-mode cmd-toggle-overwrite-mode)
  (register-command! 'toggle-read-only cmd-toggle-read-only)
  (register-command! 'toggle-show-eol cmd-toggle-show-eol)
  (register-command! 'toggle-show-tabs cmd-toggle-show-tabs)
  (register-command! 'toggle-truncate-lines cmd-toggle-truncate-lines)
  (register-command! 'toggle-visual-line-mode cmd-toggle-visual-line-mode)
  (register-command! 'universal-argument cmd-universal-argument)
  (register-command! 'untabify cmd-untabify)
  (register-command! 'view-lossage cmd-view-lossage)
  (register-command! 'view-messages cmd-view-messages)
  (register-command! 'view-errors cmd-view-errors)
  (register-command! 'view-output cmd-view-output)
  (register-command! 'what-encoding cmd-what-encoding)
  (register-command! 'what-face cmd-what-face)
  (register-command! 'what-page cmd-what-page)
  (register-command! 'where-is cmd-where-is)
  ;;; From editor-cmds-a.ss
  (register-command! 'append-to-buffer cmd-append-to-buffer)
  (register-command! 'backward-kill-sexp cmd-backward-kill-sexp)
  (register-command! 'backward-sexp cmd-backward-sexp)
  (register-command! 'backward-up-list cmd-backward-up-list)
  (register-command! 'balance-windows cmd-balance-windows)
  (register-command! 'capitalize-region cmd-capitalize-region)
  (register-command! 'copy-buffer-name cmd-copy-buffer-name)
  (register-command! 'copy-region-as-kill cmd-copy-region-as-kill)
  (register-command! 'count-chars-region cmd-count-chars-region)
  (register-command! 'count-words-buffer cmd-count-words-buffer)
  (register-command! 'count-words-paragraph cmd-count-words-paragraph)
  (register-command! 'delete-horizontal-space-forward cmd-delete-horizontal-space-forward)
  (register-command! 'delete-pair cmd-delete-pair)
  (register-command! 'duplicate-region cmd-duplicate-region)
  (register-command! 'find-alternate-file cmd-find-alternate-file)
  (register-command! 'find-file-at-point cmd-find-file-at-point)
  (register-command! 'find-init-file cmd-find-init-file)
  (register-command! 'flush-lines-region cmd-flush-lines-region)
  (register-command! 'flush-undo cmd-flush-undo)
  (register-command! 'forward-sexp cmd-forward-sexp)
  (register-command! 'forward-up-list cmd-forward-up-list)
  (register-command! 'goto-percent cmd-goto-percent)
  (register-command! 'increment-register cmd-increment-register)
  (register-command! 'indent-sexp cmd-indent-sexp)
  (register-command! 'insert-buffer-name cmd-insert-buffer-name)
  (register-command! 'insert-comment-separator cmd-insert-comment-separator)
  (register-command! 'insert-current-date-iso cmd-insert-current-date-iso)
  (register-command! 'insert-file-name cmd-insert-file-name)
  (register-command! 'insert-newline-above cmd-insert-newline-above)
  (register-command! 'insert-newline-below cmd-insert-newline-below)
  (register-command! 'insert-pair-braces cmd-insert-pair-braces)
  (register-command! 'insert-pair-brackets cmd-insert-pair-brackets)
  (register-command! 'insert-pair-quotes cmd-insert-pair-quotes)
  (register-command! 'insert-parentheses cmd-insert-parentheses)
  (register-command! 'insert-register-string cmd-insert-register-string)
  (register-command! 'insert-shebang cmd-insert-shebang)
  (register-command! 'insert-uuid cmd-insert-uuid)
  (register-command! 'keep-lines-region cmd-keep-lines-region)
  (register-command! 'kill-buffer-and-window cmd-kill-buffer-and-window)
  (register-command! 'kill-sexp cmd-kill-sexp)
  (register-command! 'list-registers cmd-list-registers)
  (register-command! 'load-init-file cmd-load-init-file)
  (register-command! 'mark-defun cmd-mark-defun)
  (register-command! 'mark-sexp cmd-mark-sexp)
  (register-command! 'move-to-window-line cmd-move-to-window-line)
  (register-command! 'next-buffer cmd-next-buffer)
  (register-command! 'previous-buffer cmd-previous-buffer)
  (register-command! 'quoted-insert cmd-quoted-insert)
  (register-command! 'recenter-bottom cmd-recenter-bottom)
  (register-command! 'recenter-top cmd-recenter-top)
  (register-command! 'replace-string-all cmd-replace-string-all)
  (register-command! 'reverse-chars cmd-reverse-chars)
  (register-command! 'scroll-other-window cmd-scroll-other-window)
  (register-command! 'scroll-other-window-up cmd-scroll-other-window-up)
  (register-command! 'set-scroll-margin cmd-set-scroll-margin)
  (register-command! 'show-buffer-size cmd-show-buffer-size)
  (register-command! 'show-kill-ring cmd-show-kill-ring)
  (register-command! 'show-line-endings cmd-show-line-endings)
  (register-command! 'smart-beginning-of-line cmd-smart-beginning-of-line)
  (register-command! 'sort-lines-case-fold cmd-sort-lines-case-fold)
  (register-command! 'sort-lines-reverse cmd-sort-lines-reverse)
  (register-command! 'toggle-auto-indent cmd-toggle-auto-indent)
  (register-command! 'toggle-auto-revert cmd-toggle-auto-revert)
  (register-command! 'toggle-centered-cursor-mode cmd-toggle-centered-cursor-mode)
  (register-command! 'toggle-debug-mode cmd-toggle-debug-mode)
  (register-command! 'toggle-delete-trailing-whitespace-on-save cmd-toggle-delete-trailing-whitespace-on-save)
  (register-command! 'toggle-electric-pair cmd-toggle-electric-pair)
  (register-command! 'toggle-global-hl-line cmd-toggle-global-hl-line)
  (register-command! 'toggle-hl-line cmd-toggle-hl-line)
  (register-command! 'toggle-input-method cmd-toggle-input-method)
  (register-command! 'toggle-narrowing-indicator cmd-toggle-narrowing-indicator)
  (register-command! 'toggle-require-final-newline cmd-toggle-require-final-newline)
  (register-command! 'toggle-save-place-mode cmd-toggle-save-place-mode)
  (register-command! 'toggle-scroll-margin cmd-toggle-scroll-margin)
  (register-command! 'toggle-show-trailing-whitespace cmd-toggle-show-trailing-whitespace)
  (register-command! 'toggle-transient-mark cmd-toggle-transient-mark)
  (register-command! 'toggle-visible-bell cmd-toggle-visible-bell)
  (register-command! 'transpose-sexps cmd-transpose-sexps)
  (register-command! 'unfill-paragraph cmd-unfill-paragraph)
  (register-command! 'unindent-region cmd-unindent-region)
  (register-command! 'uniquify-lines cmd-uniquify-lines)
  (register-command! 'untabify-buffer cmd-untabify-buffer)
  (register-command! 'upcase-initials-region cmd-upcase-initials-region)
  (register-command! 'what-buffer cmd-what-buffer)
  (register-command! 'what-line-col cmd-what-line-col)
  (register-command! 'what-mode cmd-what-mode)
  (register-command! 'whitespace-cleanup cmd-whitespace-cleanup)
  (register-command! 'word-frequency cmd-word-frequency)
  (register-command! 'zap-up-to-char cmd-zap-up-to-char)
  ;;; From editor-cmds-b.ss
  (register-command! 'bookmark-delete cmd-bookmark-delete)
  (register-command! 'bookmark-rename cmd-bookmark-rename)
  (register-command! 'camel-to-snake cmd-camel-to-snake)
  (register-command! 'clear-recent-files cmd-clear-recent-files)
  (register-command! 'collapse-blank-lines cmd-collapse-blank-lines)
  (register-command! 'comment-region cmd-comment-region)
  (register-command! 'copy-current-line cmd-copy-current-line)
  (register-command! 'copy-file-path cmd-copy-file-path)
  (register-command! 'copy-line-number cmd-copy-line-number)
  (register-command! 'copy-word cmd-copy-word)
  (register-command! 'count-buffers cmd-count-buffers)
  (register-command! 'count-lines-buffer cmd-count-lines-buffer)
  (register-command! 'count-occurrences cmd-count-occurrences)
  (register-command! 'decrease-font-size cmd-decrease-font-size)
  (register-command! 'delete-frame cmd-delete-frame)
  (register-command! 'delete-to-beginning-of-line cmd-delete-to-beginning-of-line)
  (register-command! 'delete-to-end-of-line cmd-delete-to-end-of-line)
  (register-command! 'delete-trailing-lines cmd-delete-trailing-lines)
  (register-command! 'describe-mode cmd-describe-mode)
  (register-command! 'display-line-numbers-relative cmd-display-line-numbers-relative)
  (register-command! 'downcase-char cmd-downcase-char)
  (register-command! 'eval-and-insert cmd-eval-and-insert)
  (register-command! 'find-grep cmd-find-grep)
  (register-command! 'goto-column cmd-goto-column)
  (register-command! 'goto-definition cmd-goto-definition)
  (register-command! 'goto-line-relative cmd-goto-line-relative)
  (register-command! 'highlight-word-at-point cmd-highlight-word-at-point)
  (register-command! 'increase-font-size cmd-increase-font-size)
  (register-command! 'insert-box-comment cmd-insert-box-comment)
  (register-command! 'insert-buffer-filename cmd-insert-buffer-filename)
  (register-command! 'insert-cond cmd-insert-cond)
  (register-command! 'insert-defun cmd-insert-defun)
  (register-command! 'insert-export cmd-insert-export)
  (register-command! 'insert-header-guard cmd-insert-header-guard)
  (register-command! 'insert-import cmd-insert-import)
  (register-command! 'insert-include cmd-insert-include)
  (register-command! 'insert-lambda cmd-insert-lambda)
  (register-command! 'insert-let cmd-insert-let)
  (register-command! 'insert-line-number cmd-insert-line-number)
  (register-command! 'insert-match cmd-insert-match)
  (register-command! 'insert-path-separator cmd-insert-path-separator)
  (register-command! 'insert-timestamp cmd-insert-timestamp)
  (register-command! 'insert-unless cmd-insert-unless)
  (register-command! 'insert-when cmd-insert-when)
  (register-command! 'kebab-to-camel cmd-kebab-to-camel)
  (register-command! 'kill-matching-buffers cmd-kill-matching-buffers)
  (register-command! 'list-directory cmd-list-directory)
  (register-command! 'list-recent-files cmd-list-recent-files)
  (register-command! 'make-frame cmd-make-frame)
  (register-command! 'mark-lines-matching cmd-mark-lines-matching)
  (register-command! 'move-to-window-bottom cmd-move-to-window-bottom)
  (register-command! 'move-to-window-middle cmd-move-to-window-middle)
  (register-command! 'move-to-window-top cmd-move-to-window-top)
  (register-command! 'narrow-to-defun cmd-narrow-to-defun)
  (register-command! 'number-region cmd-number-region)
  (register-command! 'pipe-region cmd-pipe-region)
  (register-command! 'prefix-lines cmd-prefix-lines)
  (register-command! 'project-compile cmd-project-compile)
  (register-command! 'project-find-file cmd-project-find-file)
  (register-command! 'project-grep cmd-project-grep)
  (register-command! 'recentf-open cmd-recentf-open)
  (register-command! 'recover-session cmd-recover-session)
  (register-command! 'reindent-buffer cmd-reindent-buffer)
  (register-command! 'remove-blank-lines cmd-remove-blank-lines)
  (register-command! 'replace-in-region cmd-replace-in-region)
  (register-command! 'reset-font-size cmd-reset-font-size)
  (register-command! 'reverse-word cmd-reverse-word)
  (register-command! 'savehist-load cmd-savehist-load)
  (register-command! 'savehist-save cmd-savehist-save)
  (register-command! 'scroll-left cmd-scroll-left)
  (register-command! 'scroll-right cmd-scroll-right)
  (register-command! 'search-backward-word cmd-search-backward-word)
  (register-command! 'search-forward-word cmd-search-forward-word)
  (register-command! 'shell-command-insert cmd-shell-command-insert)
  (register-command! 'show-char-count cmd-show-char-count)
  (register-command! 'show-column-number cmd-show-column-number)
  (register-command! 'show-file-info cmd-show-file-info)
  (register-command! 'show-git-blame cmd-show-git-blame)
  (register-command! 'show-git-diff cmd-show-git-diff)
  (register-command! 'show-git-log cmd-show-git-log)
  (register-command! 'show-git-status cmd-show-git-status)
  (register-command! 'show-keybinding-for cmd-show-keybinding-for)
  (register-command! 'show-tab-count cmd-show-tab-count)
  (register-command! 'show-trailing-whitespace-count cmd-show-trailing-whitespace-count)
  (register-command! 'show-word-count cmd-show-word-count)
  (register-command! 'snake-to-camel cmd-snake-to-camel)
  (register-command! 'sort-imports cmd-sort-imports)
  (register-command! 'sort-words cmd-sort-words)
  (register-command! 'strip-line-numbers cmd-strip-line-numbers)
  (register-command! 'suffix-lines cmd-suffix-lines)
  (register-command! 'suspend-frame cmd-suspend-frame)
  (register-command! 'toggle-auto-complete cmd-toggle-auto-complete)
  (register-command! 'toggle-auto-pair-mode cmd-toggle-auto-pair-mode)
  (register-command! 'toggle-auto-revert-global cmd-toggle-auto-revert-global)
  (register-command! 'toggle-backup-files cmd-toggle-backup-files)
  (register-command! 'toggle-case-at-point cmd-toggle-case-at-point)
  (register-command! 'toggle-electric-indent cmd-toggle-electric-indent)
  (register-command! 'toggle-eol-conversion cmd-toggle-eol-conversion)
  (register-command! 'toggle-flymake cmd-toggle-flymake)
  (register-command! 'toggle-flyspell cmd-toggle-flyspell)
  (register-command! 'toggle-global-whitespace cmd-toggle-global-whitespace)
  (register-command! 'toggle-line-comment cmd-toggle-line-comment)
  (register-command! 'toggle-lsp cmd-toggle-lsp)
  (register-command! 'toggle-menu-bar cmd-toggle-menu-bar)
  (register-command! 'toggle-narrow-indicator cmd-toggle-narrow-indicator)
  (register-command! 'toggle-scroll-bar cmd-toggle-scroll-bar)
  (register-command! 'toggle-tool-bar cmd-toggle-tool-bar)
  (register-command! 'trim-lines cmd-trim-lines)
  (register-command! 'uncomment-region cmd-uncomment-region)
  (register-command! 'upcase-char cmd-upcase-char)
  (register-command! 'widen-all cmd-widen-all)
  (register-command! 'wrap-lines-at-column cmd-wrap-lines-at-column)
  (register-command! 'write-region cmd-write-region)
  (register-command! 'yank-whole-line cmd-yank-whole-line)
  ;;; From editor-cmds-c.ss
  (register-command! 'abbrev-mode cmd-abbrev-mode)
  (register-command! 'align-current cmd-align-current)
  (register-command! 'ansi-term cmd-ansi-term)
  (register-command! 'append-to-register cmd-append-to-register)
  (register-command! 'auto-revert-mode cmd-auto-revert-mode)
  (register-command! 'backward-sentence cmd-backward-sentence)
  (register-command! 'bookmark-load cmd-bookmark-load)
  (register-command! 'bookmark-save cmd-bookmark-save)
  (register-command! 'buffer-disable-undo cmd-buffer-disable-undo)
  (register-command! 'buffer-enable-undo cmd-buffer-enable-undo)
  (register-command! 'bury-buffer cmd-bury-buffer)
  (register-command! 'center-region cmd-center-region)
  (register-command! 'clear-rectangle cmd-clear-rectangle)
  (register-command! 'complete-filename cmd-complete-filename)
  (register-command! 'convert-line-endings-dos cmd-convert-line-endings-dos)
  (register-command! 'convert-line-endings-unix cmd-convert-line-endings-unix)
  (register-command! 'copy-file cmd-copy-file)
  (register-command! 'copy-matching-lines cmd-copy-matching-lines)
  (register-command! 'copy-symbol-at-point cmd-copy-symbol-at-point)
  (register-command! 'copy-word-at-point cmd-copy-word-at-point)
  (register-command! 'customize-face cmd-customize-face)
  (register-command! 'dedent-rigidly cmd-dedent-rigidly)
  (register-command! 'define-abbrev cmd-define-abbrev)
  (register-command! 'delete-file cmd-delete-file)
  (register-command! 'delete-matching-lines cmd-delete-matching-lines)
  (register-command! 'delete-non-matching-lines cmd-delete-non-matching-lines)
  (register-command! 'delete-window-below cmd-delete-window-below)
  (register-command! 'describe-face cmd-describe-face)
  (register-command! 'describe-function cmd-describe-function)
  (register-command! 'describe-key-briefly cmd-describe-key-briefly)
  (register-command! 'describe-syntax cmd-describe-syntax)
  (register-command! 'describe-variable cmd-describe-variable)
  (register-command! 'diff-backup cmd-diff-backup)
  (register-command! 'dired cmd-dired)
  (register-command! 'dired-create-directory cmd-dired-create-directory)
  (register-command! 'dired-do-chmod cmd-dired-do-chmod)
  (register-command! 'dired-do-copy cmd-dired-do-copy)
  (register-command! 'dired-do-delete cmd-dired-do-delete)
  (register-command! 'dired-do-rename cmd-dired-do-rename)
  (register-command! 'display-fill-column-indicator cmd-display-fill-column-indicator)
  (register-command! 'electric-newline-and-indent cmd-electric-newline-and-indent)
  (register-command! 'emacs-version cmd-emacs-version)
  (register-command! 'expand-abbrev cmd-expand-abbrev)
  (register-command! 'fill-individual-paragraphs cmd-fill-individual-paragraphs)
  (register-command! 'find-file-literally cmd-find-file-literally)
  (register-command! 'first-error cmd-first-error)
  (register-command! 'fit-window-to-buffer cmd-fit-window-to-buffer)
  (register-command! 'fold-all cmd-fold-all)
  (register-command! 'fold-level cmd-fold-level)
  (register-command! 'font-lock-mode cmd-font-lock-mode)
  (register-command! 'forward-sentence cmd-forward-sentence)
  (register-command! 'getenv cmd-getenv)
  (register-command! 'goto-word-at-point cmd-goto-word-at-point)
  (register-command! 'imenu cmd-imenu)
  (register-command! 'indent-rigidly cmd-indent-rigidly)
  (register-command! 'info cmd-info)
  (register-command! 'info-elisp-manual cmd-info-elisp-manual)
  (register-command! 'info-emacs-manual cmd-info-emacs-manual)
  (register-command! 'insert-file-header cmd-insert-file-header)
  (register-command! 'insert-kbd-macro cmd-insert-kbd-macro)
  (register-command! 'insert-time cmd-insert-time)
  (register-command! 'isearch-backward-word cmd-isearch-backward-word)
  (register-command! 'isearch-forward-symbol cmd-isearch-forward-symbol)
  (register-command! 'isearch-forward-word cmd-isearch-forward-word)
  (register-command! 'ispell-buffer cmd-ispell-buffer)
  (register-command! 'ispell-region cmd-ispell-region)
  (register-command! 'ispell-word cmd-ispell-word)
  (register-command! 'ispell-change-dictionary cmd-ispell-change-dictionary)
  (register-command! 'list-abbrevs cmd-list-abbrevs)
  (register-command! 'list-colors cmd-list-colors)
  (register-command! 'load-theme cmd-load-theme)
  (register-command! 'lock-buffer cmd-lock-buffer)
  (register-command! 'make-directory cmd-make-directory)
  (register-command! 'mark-page cmd-mark-page)
  (register-command! 'maximize-window cmd-maximize-window)
  (register-command! 'memory-report cmd-memory-report)
  (register-command! 'minimize-window cmd-minimize-window)
  (register-command! 'multi-occur cmd-multi-occur)
  (register-command! 'name-last-kbd-macro cmd-name-last-kbd-macro)
  (register-command! 'profiler-start cmd-profiler-start)
  (register-command! 'profiler-stop cmd-profiler-stop)
  (register-command! 'query-replace-regexp cmd-query-replace-regexp)
  (register-command! 'quick-calc cmd-quick-calc)
  (register-command! 'rename-uniquely cmd-rename-uniquely)
  (register-command! 'report-bug cmd-report-bug)
  (register-command! 'resize-window-width cmd-resize-window-width)
  (register-command! 'revert-buffer-with-coding cmd-revert-buffer-with-coding)
  (register-command! 'rotate-windows cmd-rotate-windows)
  (register-command! 'set-buffer-file-coding cmd-set-buffer-file-coding)
  (register-command! 'setenv cmd-setenv)
  (register-command! 'set-language-environment cmd-set-language-environment)
  (register-command! 'show-environment cmd-show-environment)
  (register-command! 'shrink-window-if-larger-than-buffer cmd-shrink-window-if-larger-than-buffer)
  (register-command! 'split-window-below cmd-split-window-below)
  (register-command! 'sudo-find-file cmd-sudo-find-file)
  (register-command! 'swap-windows cmd-swap-windows)
  (register-command! 'toggle-debug-on-quit cmd-toggle-debug-on-quit)
  (register-command! 'toggle-fold cmd-toggle-fold)
  (register-command! 'toggle-frame-fullscreen cmd-toggle-frame-fullscreen)
  (register-command! 'toggle-frame-maximized cmd-toggle-frame-maximized)
  (register-command! 'toggle-menu-bar-mode cmd-toggle-menu-bar-mode)
  (register-command! 'toggle-show-spaces cmd-toggle-show-spaces)
  (register-command! 'toggle-tab-bar-mode cmd-toggle-tab-bar-mode)
  (register-command! 'transpose-paragraphs cmd-transpose-paragraphs)
  (register-command! 'unbury-buffer cmd-unbury-buffer)
  (register-command! 'unfold-all cmd-unfold-all)
  (register-command! 'vc-annotate cmd-vc-annotate)
  (register-command! 'vc-diff-head cmd-vc-diff-head)
  (register-command! 'vc-log-file cmd-vc-log-file)
  (register-command! 'vc-revert cmd-vc-revert)
  (register-command! 'view-echo-area-messages cmd-view-echo-area-messages)
  (register-command! 'view-register cmd-view-register)
  (register-command! 'which-function cmd-which-function)
  (register-command! 'whitespace-mode cmd-whitespace-mode)
  (register-command! 'zap-to-char-inclusive cmd-zap-to-char-inclusive)
  ;;; Aliases for remaining 8 Qt-only commands
  (register-command! 'auto-fill-mode cmd-toggle-auto-fill)
  (register-command! 'centered-cursor-mode cmd-toggle-centered-cursor-mode)
  (register-command! 'ffap cmd-find-file-at-point)
  (register-command! 'kill-ring-save cmd-copy-region)
  (register-command! 'save-file cmd-save-buffer)
  (register-command! 'set-mark-command cmd-set-mark)
  (register-command! 'lsp cmd-toggle-lsp)
  (register-command! 'string-insert-file cmd-insert-file)
  ;;; Canonical Emacs aliases
  (register-command! 'electric-pair-mode cmd-toggle-electric-pair)
  (register-command! 'visual-line-mode cmd-toggle-visual-line-mode)
  (register-command! 'flyspell-mode cmd-flyspell-mode)
  (register-command! 'read-only-mode cmd-toggle-read-only)
  (register-command! 'overwrite-mode cmd-toggle-overwrite-mode)
  (register-command! 'hl-line-mode cmd-toggle-hl-line)
  (register-command! 'whitespace-cleanup-mode cmd-whitespace-cleanup)
  (register-command! 'delete-selection-mode cmd-toggle-transient-mark)
  (register-command! 'show-paren-mode cmd-toggle-highlighting)
  (register-command! 'global-auto-revert-mode cmd-toggle-auto-revert-global)
  (register-command! 'line-number-mode cmd-toggle-line-numbers)
  (register-command! 'column-number-mode cmd-toggle-line-numbers)
  (register-command! 'comment-or-uncomment-region cmd-toggle-comment)
  (register-command! 'isearch-forward cmd-search-forward)
  (register-command! 'isearch-backward cmd-search-backward)
  ;;; New features
  (register-command! 'dired-jump cmd-dired-jump)
  (register-command! 'dired-up-directory cmd-dired-up-directory)
  (register-command! 'dired-do-shell-command cmd-dired-do-shell-command)
  (register-command! 'apropos cmd-apropos-emacs)
  (register-command! 'indent-new-comment-line cmd-indent-new-comment-line)
  (register-command! 'isearch-backward-regexp cmd-isearch-backward-regexp)
  (register-command! 'replace-regexp cmd-replace-regexp)
  (register-command! 'org-capture cmd-org-capture)
  (register-command! 'org-refile cmd-org-refile)
  (register-command! 'org-time-stamp cmd-org-time-stamp)
  (register-command! 'org-insert-link cmd-org-insert-link)
  (register-command! 'org-narrow-to-subtree cmd-org-narrow-to-subtree)
  (register-command! 'org-sort cmd-org-sort)
  (register-command! 'project-switch-to-buffer cmd-project-switch-to-buffer)
  (register-command! 'project-kill-buffers cmd-project-kill-buffers)
  (register-command! 'vc-next-action cmd-vc-next-action)
  ;;; Batch 2: more canonical aliases for standard Emacs names
  (register-command! 'keyboard-escape-quit cmd-keyboard-quit)
  (register-command! 'buffer-menu cmd-list-buffers)
  (register-command! 'move-beginning-of-line cmd-beginning-of-line)
  (register-command! 'move-end-of-line cmd-end-of-line)
  (register-command! 'scroll-other-window-down cmd-scroll-other-window-up)
  (register-command! 'kmacro-start-macro cmd-start-kbd-macro)
  (register-command! 'kmacro-end-macro cmd-end-kbd-macro)
  (register-command! 'kmacro-name-last cmd-name-last-kbd-macro)
  (register-command! 'tab-bar-mode cmd-toggle-tab-bar-mode)
  (register-command! 'clipboard-yank cmd-yank)
  (register-command! 'clipboard-kill-region cmd-kill-region)
  (register-command! 'comment-line cmd-toggle-comment)
  (register-command! 'indent-for-tab-command cmd-indent-or-complete)
  (register-command! 'linum-mode cmd-toggle-line-numbers)
  (register-command! 'sort-numeric-fields cmd-sort-numeric-fields)
  (register-command! 'find-dired cmd-find-dired)
  (register-command! 'find-name-dired cmd-find-name-dired)
  (register-command! 'dired-hide-details-mode cmd-dired-hide-details)
  (register-command! 'desktop-save-mode cmd-desktop-save-mode)
  (register-command! 'org-babel-execute-src-block cmd-org-babel-execute-src-block)
  (register-command! 'org-babel-tangle cmd-org-babel-tangle)
  (register-command! 'other-frame cmd-other-frame)
  (register-command! 'register-to-point cmd-point-to-register)
  (register-command! 'winum-mode cmd-winum-mode)
  (register-command! 'help-with-tutorial cmd-help-with-tutorial)
  (register-command! 'flyspell-prog-mode cmd-toggle-flyspell)
  (register-command! 'cua-mode cmd-cua-mode)
  ;; Batch 4: new commands + aliases
  (register-command! 'check-parens cmd-check-parens)
  (register-command! 'count-lines-page cmd-count-lines-page)
  (register-command! 'how-many cmd-how-many)
  (register-command! 'move-to-window-line-top-bottom cmd-move-to-window-line)
  (register-command! 'binary-overwrite-mode cmd-toggle-overwrite-mode)
  (register-command! 'highlight-symbol-at-point cmd-highlight-symbol)
  (register-command! 'ediff-regions-linewise cmd-ediff-buffers)
  ;; Batch 5: aliases in editor-cmds scope
  (register-command! 'rename-file cmd-rename-file-and-buffer)
  (register-command! 'delete-directory cmd-delete-directory)
  (register-command! 'copy-to-buffer cmd-copy-region-as-kill)
  (register-command! 'dired-do-flagged-delete cmd-dired-do-delete)
  (register-command! 'set-file-modes cmd-set-file-modes)
  (register-command! 'dired-do-chown cmd-dired-do-chown)
  (register-command! 'butterfly cmd-butterfly)
  ;; Batch 7: isearch/case/kmacro/debug aliases
  (register-command! 'isearch-forward-symbol-at-point cmd-isearch-forward-symbol)
  (register-command! 'isearch-yank-word-or-char cmd-search-forward)
  (register-command! 'isearch-query-replace cmd-query-replace)
  (register-command! 'capitalize-dwim cmd-capitalize-word)
  (register-command! 'upcase-dwim cmd-upcase-word)
  (register-command! 'downcase-dwim cmd-downcase-word)
  (register-command! 'describe-personal-keybindings cmd-describe-bindings)
  (register-command! 'whitespace-newline-mode cmd-whitespace-mode)
  (register-command! 'debug-on-entry cmd-debug-on-entry)
  (register-command! 'cancel-debug-on-entry cmd-cancel-debug-on-entry)
  ;; Batch 8: window/help/rectangle/vc/coding aliases
  ;; Window/frame (no real frames — alias to window equivalents)
  (register-command! 'delete-other-frames cmd-delete-other-windows)
  (register-command! 'make-frame-command cmd-split-window-right)
  (register-command! 'balance-windows-area cmd-balance-windows)
  ;; Text manipulation
  (register-command! 'sort-columns cmd-sort-lines)
  (register-command! 'align cmd-align-regexp)
  ;; Navigation
  (register-command! 'find-file-other-frame cmd-find-file-other-window)
  ;; Help/describe
  (register-command! 'describe-char cmd-what-cursor-position)
  (register-command! 'describe-syntax cmd-describe-mode)
  (register-command! 'describe-categories cmd-describe-mode)
  (register-command! 'describe-current-coding-system cmd-describe-mode)
  (register-command! 'describe-input-method cmd-describe-mode)
  (register-command! 'describe-language-environment cmd-describe-mode)
  (register-command! 'describe-coding-system cmd-describe-mode)
  (register-command! 'command-history cmd-view-lossage)
  (register-command! 'list-command-history cmd-view-lossage)
  (register-command! 'apropos-value cmd-apropos-emacs)
  (register-command! 'apropos-library cmd-apropos-emacs)
  (register-command! 'apropos-user-option cmd-apropos-emacs)
  (register-command! 'list-timers cmd-list-processes)
  ;; Rectangle/register
  (register-command! 'string-insert-rectangle cmd-string-rectangle)
  (register-command! 'close-rectangle cmd-kill-rectangle)
  (register-command! 'number-to-register cmd-copy-to-register)
  (register-command! 'increment-register cmd-copy-to-register)
  (register-command! 'frameset-to-register cmd-copy-to-register)
  (register-command! 'window-configuration-to-register cmd-copy-to-register)
  (register-command! 'bookmark-write cmd-bookmark-save)
  (register-command! 'bookmark-insert-location cmd-bookmark-jump)
  (register-command! 'bookmark-rename cmd-bookmark-set)
  (register-command! 'bookmark-insert cmd-bookmark-jump)
  (register-command! 'bookmark-bmenu-list cmd-bookmark-list)
  ;; VC (editor-cmds-c scope: cmd-vc-annotate, cmd-vc-diff-head, cmd-vc-log-file, cmd-vc-revert)
  (register-command! 'vc-root-diff cmd-vc-diff-head)
  (register-command! 'vc-log-incoming cmd-vc-log-file)
  (register-command! 'vc-log-outgoing cmd-vc-log-file)
  (register-command! 'vc-revision-other-window cmd-vc-diff-head)
  (register-command! 'vc-region-history cmd-vc-log-file)
  (register-command! 'vc-ignore cmd-vc-annotate)
  (register-command! 'vc-update cmd-vc-revert)
  (register-command! 'magit-revert cmd-vc-revert)
  ;; Coding/encoding
  (register-command! 'set-buffer-file-coding-system cmd-set-language-environment)
  (register-command! 'revert-buffer-with-coding-system cmd-revert-buffer)
  (register-command! 'set-terminal-coding-system cmd-set-language-environment)
  (register-command! 'set-keyboard-coding-system cmd-set-language-environment)
  (register-command! 'universal-coding-system-argument cmd-set-language-environment)
  (register-command! 'recode-region cmd-set-language-environment)
  (register-command! 'decode-coding-region cmd-set-language-environment)
  (register-command! 'encode-coding-region cmd-set-language-environment)
  (register-command! 'set-input-method cmd-toggle-input-method)
  ;; Batch 9: org/babel/misc/abbrev aliases
  ;; Org capture
  (register-command! 'org-capture-finalize cmd-org-capture-finalize)
  (register-command! 'org-capture-refile cmd-org-capture)
  (register-command! 'org-capture-abort cmd-org-capture-abort)
  (register-command! 'org-capture-kill cmd-org-capture-abort)
  ;; Org babel
  (register-command! 'org-babel-execute-maybe cmd-org-babel-execute-src-block)
  (register-command! 'org-babel-next-src-block cmd-next-error)
  (register-command! 'org-babel-previous-src-block cmd-previous-error)
  (register-command! 'org-babel-mark-block cmd-mark-paragraph)
  (register-command! 'org-babel-kill-session cmd-org-babel-kill-session)
  ;; Error navigation
  (register-command! 'next-error-no-select cmd-next-error)
  ;; Search/find
  (register-command! 'lgrep cmd-grep)
  (register-command! 'locate cmd-find-dired)
  (register-command! 'locate-with-filter cmd-find-name-dired)
  ;; Abbrevs
  (register-command! 'dabbrev-completion cmd-dabbrev-expand)
  (register-command! 'edit-abbrevs cmd-list-abbrevs)
  (register-command! 'write-abbrev-file cmd-list-abbrevs)
  (register-command! 'read-abbrev-file cmd-list-abbrevs)
  ;; Minibuffer/escape
  (register-command! 'abort-recursive-edit cmd-keyboard-quit)
  (register-command! 'top-level cmd-keyboard-quit)
  ;; Batch 10: project/diff aliases (editor-cmds scope)
  (register-command! 'project-compile cmd-project-compile)
  (register-command! 'project-async-shell-command cmd-async-shell-command)
  (register-command! 'ediff-merge-buffers cmd-ediff-buffers)
  (register-command! 'ediff-revision cmd-vc-diff-head)
  (register-command! 'emerge-buffers cmd-ediff-buffers)
  ;; Batch 11: package/treesit/flymake/mc aliases
  (register-command! 'customize-option cmd-customize-face)
  (register-command! 'customize-save-customized cmd-customize-face)
  (register-command! 'helm-projectile cmd-project-find-file)
  ;; EWW bookmarks
  (register-command! 'eww-add-bookmark cmd-eww-add-bookmark)
  (register-command! 'eww-list-bookmarks cmd-eww-list-bookmarks)
  ;; Forge (GitHub via gh CLI)
  (register-command! 'forge-list-prs cmd-forge-list-prs)
  (register-command! 'forge-list-issues cmd-forge-list-issues)
  (register-command! 'forge-view-pr cmd-forge-view-pr)
  (register-command! 'forge-create-pr cmd-forge-create-pr)
)

;;;============================================================================
;;; New feature implementations (TUI)
;;;============================================================================

;;; --- Dired navigation ---
(def (cmd-dired-jump app)
  "Jump to dired for the current file's directory (C-x C-j)."
  (let* ((fr (app-state-frame app)) (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (and buf (buffer-file-path buf)))
         (dir (if path (path-directory path) ".")))
    (with-catch
      (lambda (e) (echo-message! (app-state-echo app)
                    (string-append "Error: " (with-output-to-string (lambda () (display-exception e))))))
      (lambda ()
        (let* ((proc (open-process (list path: "ls" arguments: ["-la" dir]
                        stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
               (output (read-line proc #f)))
          (close-port proc)
          (open-output-buffer app (string-append "*Dired: " dir "*") (or output "")))))))

(def (cmd-dired-up-directory app)
  "Go up to parent directory in dired."
  (let* ((fr (app-state-frame app)) (win (current-window fr))
         (buf (edit-window-buffer win))
         (name (and buf (buffer-name buf)))
         (dir (if (and name (string-prefix? "*Dired: " name))
                (let ((d (substring name 8 (- (string-length name) 1))))
                  (path-directory (if (string-suffix? "/" d) (substring d 0 (- (string-length d) 1)) d)))
                "..")))
    (with-catch
      (lambda (e) (echo-message! (app-state-echo app) "Cannot go up"))
      (lambda ()
        (let* ((proc (open-process (list path: "ls" arguments: ["-la" dir]
                        stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
               (output (read-line proc #f)))
          (close-port proc)
          (open-output-buffer app (string-append "*Dired: " dir "*") (or output "")))))))

(def (cmd-dired-do-shell-command app)
  "Run shell command on marked files in dired."
  (let ((cmd (app-read-string app "Shell command: ")))
    (when (and cmd (not (string-empty? cmd)))
      (with-catch
        (lambda (e) (echo-message! (app-state-echo app) "Shell command error"))
        (lambda ()
          (let* ((proc (open-process (list path: "/bin/sh" arguments: ["-c" cmd]
                          stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                 (output (read-line proc #f)))
            (process-status proc) (close-port proc)
            (open-output-buffer app "*Shell Command*" (or output ""))))))))

;;; --- Help/Apropos ---
(def (cmd-apropos-emacs app)
  "Search commands by keyword (C-h a)."
  (let ((pattern (app-read-string app "Apropos: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((cmds (hash->list *all-commands*))
             (matches (filter (lambda (p) (string-contains (symbol->string (car p)) pattern)) cmds))
             (lines (map (lambda (p) (symbol->string (car p))) matches)))
        (if (null? lines)
          (echo-message! (app-state-echo app) (string-append "No matches for: " pattern))
          (open-output-buffer app "*Apropos*"
            (string-append "Commands matching \"" pattern "\":\n\n"
              (string-join (sort lines string<?) "\n"))))))))

;;; --- Comment ---
(def (cmd-indent-new-comment-line app)
  "Continue comment on new line (M-j)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (text (editor-get-line ed line))
         (trimmed (string-trim text)))
    (if (and (> (string-length trimmed) 0)
             (or (string-prefix? ";;" trimmed) (string-prefix? "//" trimmed)
                 (string-prefix? "#" trimmed) (string-prefix? "--" trimmed)))
      (let ((prefix (cond ((string-prefix? ";;" trimmed) ";; ")
                          ((string-prefix? "//" trimmed) "// ")
                          ((string-prefix? "#" trimmed) "# ")
                          ((string-prefix? "--" trimmed) "-- ")
                          (else ""))))
        (cmd-newline app)
        (let ((ed2 (current-editor app)))
          (editor-insert-text ed2 (editor-get-current-pos ed2) prefix)))
      (cmd-newline app))))


;;; --- Search ---
(def (cmd-isearch-backward-regexp app)
  "Regexp search backward (C-M-r)."
  (cmd-search-backward app))

(def (cmd-replace-regexp app)
  "Replace using regexp."
  (cmd-query-replace-regexp app))

;;; --- Org mode ---

(def (cmd-org-capture app)
  "Capture a note with template selection and interactive editing.
   Select a template (t=TODO, n=Note, j=Journal), edit in *Org Capture* buffer,
   then C-c C-c to finalize or C-c C-k to abort."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (menu (org-capture-menu-string))
         (key (echo-read-string echo (string-append "Template (" menu "): ") row width)))
    (when (and key (> (string-length key) 0))
      (let* ((buf-info (current-buffer-from-app app))
             (source-file (or (buffer-name buf-info) ""))
             (source-path (or (buffer-file-path buf-info) ""))
             (tmpl-str (org-capture-template-template
                         (or (find (lambda (t) (string=? (org-capture-template-key t) key))
                                   *org-capture-templates*)
                             (car *org-capture-templates*))))
             (cursor-pos (org-capture-cursor-position tmpl-str))
             (expanded (org-capture-start key source-file source-path)))
        (if (not expanded)
          (echo-error! echo (string-append "Unknown template: " key))
          (let* ((ed (current-editor app))
                 (buf (buffer-create! "*Org Capture*" ed)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (editor-insert-text ed 0 expanded)
            (when cursor-pos
              (editor-goto-pos ed (min cursor-pos (string-length expanded))))
            (echo-message! echo "Edit then C-c C-c to save, C-c C-k to abort")))))))

(def (cmd-org-capture-finalize app)
  "Finalize org capture: save buffer content to target file."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf)))
    (if (not (string=? name "*Org Capture*"))
      (echo-error! (app-state-echo app) "Not in a capture buffer")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed)))
        (if (org-capture-finalize text)
          (begin
            (execute-command! app 'kill-buffer-cmd)
            (echo-message! (app-state-echo app) "Capture saved"))
          (echo-error! (app-state-echo app) "Capture failed — no active session"))))))

(def (cmd-org-capture-abort app)
  "Abort org capture: discard buffer without saving."
  (let* ((buf (current-buffer-from-app app))
         (name (buffer-name buf)))
    (if (not (string=? name "*Org Capture*"))
      (echo-error! (app-state-echo app) "Not in a capture buffer")
      (begin
        (org-capture-abort)
        (execute-command! app 'kill-buffer-cmd)
        (echo-message! (app-state-echo app) "Capture aborted")))))

(def (cmd-org-refile app)
  "Refile current heading to another location with interactive selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (lines (string-split text #\newline))
         (total (length lines))
         (cur-line (editor-line-from-position ed pos))
         (heading-line (let loop ((l cur-line))
                         (cond
                           ((< l 0) #f)
                           ((and (< l total) (org-heading-line? (list-ref lines l))) l)
                           (else (loop (- l 1)))))))
    (if (not heading-line)
      (echo-error! echo "Not on an org heading")
      (let* ((level (org-heading-stars-of-line (list-ref lines heading-line)))
             (subtree-end (let loop ((i (+ heading-line 1)))
                            (cond
                              ((>= i total) total)
                              ((and (org-heading-line? (list-ref lines i))
                                    (<= (org-heading-stars-of-line (list-ref lines i)) level)) i)
                              (else (loop (+ i 1))))))
             (subtree-lines (let loop ((i heading-line) (acc '()))
                              (if (>= i subtree-end) (reverse acc)
                                (loop (+ i 1) (cons (list-ref lines i) acc)))))
             (targets (let loop ((i 0) (acc '()))
                        (if (>= i total) (reverse acc)
                          (if (and (or (< i heading-line) (>= i subtree-end))
                                   (org-heading-line? (list-ref lines i)))
                            (let* ((hline (list-ref lines i))
                                   (nstars (org-heading-stars-of-line hline))
                                   (label (string-append
                                            (make-string nstars #\*)
                                            (substring hline nstars (string-length hline)))))
                              (loop (+ i 1) (cons (cons label i) acc)))
                            (loop (+ i 1) acc)))))
             (labels (map car targets)))
        (if (null? labels)
          (echo-error! echo "No refile targets found")
          (let ((chosen (app-read-string app "Refile to: ")))
            (when (and chosen (not (string-empty? chosen)))
              ;; Find the best matching target
              (let ((target-pair (or (assoc chosen targets)
                                     (find (lambda (p) (string-contains (car p) chosen)) targets))))
                (if (not target-pair)
                  (echo-error! echo "No matching heading found")
                  (let* ((target-line (cdr target-pair))
                         (before (let loop ((i 0) (acc '()))
                                   (if (>= i heading-line) (reverse acc)
                                     (loop (+ i 1) (cons (list-ref lines i) acc)))))
                         (after (let loop ((i subtree-end) (acc '()))
                                  (if (>= i total) (reverse acc)
                                    (loop (+ i 1) (cons (list-ref lines i) acc)))))
                         (rest-lines (append before after))
                         (removed-count (- subtree-end heading-line))
                         (adj-target (if (> target-line heading-line)
                                       (- target-line removed-count) target-line))
                         (adj-total (length rest-lines))
                         (target-level (org-heading-stars-of-line (list-ref rest-lines adj-target)))
                         (insert-at (let loop ((i (+ adj-target 1)))
                                      (cond
                                        ((>= i adj-total) adj-total)
                                        ((and (org-heading-line? (list-ref rest-lines i))
                                              (<= (org-heading-stars-of-line (list-ref rest-lines i))
                                                  target-level)) i)
                                        (else (loop (+ i 1))))))
                         (pre (let loop ((i 0) (acc '()))
                                (if (>= i insert-at) (reverse acc)
                                  (loop (+ i 1) (cons (list-ref rest-lines i) acc)))))
                         (post (let loop ((i insert-at) (acc '()))
                                 (if (>= i adj-total) (reverse acc)
                                   (loop (+ i 1) (cons (list-ref rest-lines i) acc)))))
                         (new-text (string-join (append pre subtree-lines post) "\n")))
                    (with-undo-action ed
                      (editor-set-text ed new-text))
                    (echo-message! echo
                      (string-append "Refiled to: " chosen))))))))))))

(def (cmd-org-time-stamp app)
  "Insert org timestamp."
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "Timestamp error"))
    (lambda ()
      (let* ((proc (open-process (list path: "date" arguments: '("+<%Y-%m-%d %a>")
                      stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (ts (read-line proc)))
        (process-status proc) (close-port proc)
        (when (string? ts)
          (let ((ed (current-editor app)))
            (editor-insert-text ed (editor-get-current-pos ed) ts)))))))

(def (cmd-org-insert-link app)
  "Insert org link [[url][description]]."
  (let ((url (app-read-string app "Link URL: ")))
    (when (and url (not (string-empty? url)))
      (let ((desc (app-read-string app "Description: ")))
        (let* ((ed (current-editor app))
               (link (if (and desc (not (string-empty? desc)))
                       (string-append "[[" url "][" desc "]]")
                       (string-append "[[" url "]]"))))
          (editor-insert-text ed (editor-get-current-pos ed) link))))))

(def (cmd-org-narrow-to-subtree app)
  "Narrow to current org subtree."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (text (editor-get-text ed))
         (lines (string-split text #\newline)))
    ;; Find heading at or before current line
    (let loop ((l line))
      (if (< l 0)
        (echo-message! (app-state-echo app) "No heading found")
        (let ((ln (if (< l (length lines)) (list-ref lines l) "")))
          (if (and (> (string-length ln) 0) (char=? (string-ref ln 0) #\*))
            ;; Found heading; find end of subtree
            (let* ((level (let lp ((i 0)) (if (and (< i (string-length ln)) (char=? (string-ref ln i) #\*)) (lp (+ i 1)) i)))
                   (end (let lp2 ((el (+ l 1)))
                          (if (>= el (length lines)) el
                            (let ((eln (list-ref lines el)))
                              (if (and (> (string-length eln) 0) (char=? (string-ref eln 0) #\*)
                                       (<= (let lp3 ((j 0)) (if (and (< j (string-length eln)) (char=? (string-ref eln j) #\*)) (lp3 (+ j 1)) j)) level))
                                el (lp2 (+ el 1)))))))
                   (start-pos (editor-position-from-line ed l))
                   (end-pos (if (>= end (length lines)) (string-length text) (editor-position-from-line ed end))))
              (cmd-narrow-to-region app) ;; Uses mark-based narrowing
              (echo-message! (app-state-echo app) (string-append "Narrowed to subtree: " (string-trim ln))))
            (loop (- l 1))))))))

(def (cmd-org-sort app)
  "Sort org entries."
  (echo-message! (app-state-echo app) "Org sort: use M-x sort-lines on region"))

;;; --- Project ---
(def (cmd-project-switch-to-buffer app)
  "Switch to a buffer in the current project."
  (let* ((fr (app-state-frame app)) (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (and buf (buffer-file-path buf)))
         (root (if path (find-project-root (path-directory path)) #f)))
    (if (not root)
      (cmd-switch-buffer app)
      (let* ((bufs (filter (lambda (b)
                             (let ((fp (buffer-file-path b)))
                               (and fp (string-prefix? root fp))))
                           *buffer-list*))
             (names (map buffer-name bufs)))
        (if (null? names)
          (echo-message! (app-state-echo app) "No project buffers")
          (let ((name (app-read-string app (string-append "Project buffer [" root "]: "))))
            (when (and name (not (string-empty? name)))
              (let ((target (find (lambda (b) (string=? (buffer-name b) name)) bufs)))
                (if target
                  (begin (buffer-attach! (current-editor app) target)
                         (set! (edit-window-buffer win) target))
                  (echo-message! (app-state-echo app) "Buffer not found"))))))))))

(def (find-project-root dir)
  "Find project root by looking for .git, gerbil.pkg, Makefile, etc."
  (let loop ((d (if (string-suffix? "/" dir) dir (string-append dir "/"))))
    (cond
      ((or (string=? d "/") (string=? d "")) #f)
      ((or (file-exists? (string-append d ".git"))
           (file-exists? (string-append d "gerbil.pkg"))
           (file-exists? (string-append d "Makefile"))
           (file-exists? (string-append d "package.json")))
       d)
      (else (loop (path-directory (substring d 0 (- (string-length d) 1))))))))

(def (cmd-project-kill-buffers app)
  "Kill all buffers in the current project."
  (let* ((fr (app-state-frame app)) (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (and buf (buffer-file-path buf)))
         (root (if path (find-project-root (path-directory path)) #f)))
    (if (not root)
      (echo-message! (app-state-echo app) "Not in a project")
      (let* ((bufs (filter (lambda (b)
                             (let ((fp (buffer-file-path b)))
                               (and fp (string-prefix? root fp))))
                           *buffer-list*))
             (count (length bufs)))
        (for-each (lambda (b) (set! *buffer-list* (remq b *buffer-list*))) bufs)
        (echo-message! (app-state-echo app) (string-append "Killed " (number->string count) " project buffers"))))))

;;; --- Version control ---
(def (cmd-vc-next-action app)
  "Do the next logical VCS action (C-x v v)."
  (let* ((buf (edit-window-buffer (current-window (app-state-frame app))))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-message! (app-state-echo app) "No file for VC")
      (with-catch
        (lambda (e) (echo-message! (app-state-echo app) "VC error"))
        (lambda ()
          (let* ((dir (path-directory path))
                 (proc (open-process (list path: "git" arguments: ["status" "--porcelain" "--" path]
                           directory: dir stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                 (status-line (read-line proc)))
            (process-status proc) (close-port proc)
            (cond
              ((eof-object? status-line)
               (echo-message! (app-state-echo app) "File is clean (no changes)"))
              ((or (string-prefix? "??" status-line) (string-prefix? "A " status-line))
               ;; Untracked or added — stage it
               (let ((p2 (open-process (list path: "git" arguments: ["add" "--" path]
                              directory: dir stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t))))
                 (process-status p2) (close-port p2)
                 (echo-message! (app-state-echo app) (string-append "Staged: " (path-strip-directory path)))))
              ((or (string-prefix? " M" status-line) (string-prefix? "M " status-line)
                   (string-prefix? "MM" status-line))
               ;; Modified — stage it
               (let ((p2 (open-process (list path: "git" arguments: ["add" "--" path]
                              directory: dir stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t))))
                 (process-status p2) (close-port p2)
                 (echo-message! (app-state-echo app) (string-append "Staged: " (path-strip-directory path)))))
              (else
               (echo-message! (app-state-echo app)
                 (string-append "Status: " (string-trim status-line)))))))))))

;;;============================================================================
;;; Batch 2: New feature implementations
;;;============================================================================

;;; --- Sort numeric ---
(def (cmd-sort-numeric-fields app)
  "Sort lines by numeric value of first number on each line."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (numbered (map (lambda (l)
                          (let ((nums (pregexp-match "[0-9]+" l)))
                            (cons (if nums (string->number (car nums)) 0) l)))
                        lines))
         (sorted (sort numbered (lambda (a b) (< (car a) (car b)))))
         (result (string-join (map cdr sorted) "\n")))
    (editor-set-text ed result)
    (echo-message! (app-state-echo app)
      (string-append "Sorted " (number->string (length lines)) " lines numerically"))))

;;; --- Find in dired ---
(def (cmd-find-dired app)
  "Find files matching pattern in directory (find-dired)."
  (let ((dir (app-read-string app "Directory: ")))
    (when (and dir (not (string-empty? dir)))
      (let ((args (app-read-string app "Find arguments: ")))
        (when (and args (not (string-empty? args)))
          (with-catch
            (lambda (e) (echo-message! (app-state-echo app) "find error"))
            (lambda ()
              (let* ((cmd-str (string-append "find " dir " " args))
                     (output (run-process ["bash" "-c" cmd-str] coprocess: read-all-as-string)))
                (open-output-buffer app "*Find*" (or output ""))))))))))

(def (cmd-find-name-dired app)
  "Find files by name pattern in directory (find-name-dired)."
  (let ((dir (app-read-string app "Directory: ")))
    (when (and dir (not (string-empty? dir)))
      (let ((pattern (app-read-string app "Filename pattern: ")))
        (when (and pattern (not (string-empty? pattern)))
          (with-catch
            (lambda (e) (echo-message! (app-state-echo app) "find error"))
            (lambda ()
              (let* ((cmd-str (string-append "find " dir " -name " (string-append "'" pattern "'")))
                     (output (run-process ["bash" "-c" cmd-str] coprocess: read-all-as-string)))
                (open-output-buffer app "*Find*" (or output ""))))))))))

;;; --- Dired details ---
(def *dired-hide-details* #f)
(def (cmd-dired-hide-details app)
  "Toggle dired details display."
  (set! *dired-hide-details* (not *dired-hide-details*))
  (echo-message! (app-state-echo app)
    (if *dired-hide-details* "Details hidden" "Details shown")))

;;; --- Desktop save mode ---
(def *desktop-save-mode* #f)
(def (cmd-desktop-save-mode app)
  "Toggle desktop-save-mode (auto save/restore session)."
  (set! *desktop-save-mode* (not *desktop-save-mode*))
  (echo-message! (app-state-echo app)
    (if *desktop-save-mode* "Desktop save mode enabled" "Desktop save mode disabled")))

;;; --- Org babel commands ---
(def (cmd-org-babel-execute-src-block app)
  "Execute the org source block at point (C-c C-c)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (lines (string-split text #\newline)))
    (let-values (((lang header-args body begin-line end-line block-name)
                  (org-babel-find-src-block lines line-num)))
      (if (not lang)
        (echo-message! (app-state-echo app) "Not in a source block")
        (with-catch
          (lambda (e) (echo-message! (app-state-echo app)
                        (string-append "Babel error: "
                          (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((output (org-babel-execute lang body header-args
                            buffer-text: text)))
              (org-babel-insert-result ed end-line output
                (or (hash-get header-args "results") "output"))
              (echo-message! (app-state-echo app)
                (string-append "Executed " lang " block")))))))))

(def (cmd-org-babel-tangle app)
  "Tangle the current org buffer — extract code blocks to files."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed)))
    (with-catch
      (lambda (e) (echo-message! (app-state-echo app)
                    (string-append "Tangle error: "
                      (with-output-to-string (lambda () (display-exception e))))))
      (lambda ()
        (let ((files (org-babel-tangle-to-files text)))
          (echo-message! (app-state-echo app)
            (if (null? files)
              "No :tangle blocks found"
              (string-append "Tangled to: "
                (string-join (map car files) ", ")))))))))

;;; --- Org babel sessions ---
(def (cmd-org-babel-kill-session app)
  "Kill all active org-babel sessions."
  (org-babel-kill-all-sessions)
  (echo-message! (app-state-echo app) "All babel sessions killed"))

;;; --- Other frame (stub) ---
(def (cmd-other-frame app)
  "Switch to next frame (stub — gemacs is single-frame)."
  (echo-message! (app-state-echo app) "Only one frame"))

;;; --- Winum mode (stub) ---
(def *winum-mode* #f)
(def (cmd-winum-mode app)
  "Toggle window-numbering mode."
  (set! *winum-mode* (not *winum-mode*))
  (echo-message! (app-state-echo app)
    (if *winum-mode* "Winum mode enabled (use M-1..M-9)" "Winum mode disabled")))

;;; --- Help with tutorial ---
(def (cmd-help-with-tutorial app)
  "Show the gemacs tutorial (C-h t)."
  (let ((text (string-append
    "=== Gemacs Tutorial ===\n\n"
    "Welcome to Gemacs, a Gerbil Scheme Emacs replacement.\n\n"
    "== Basic Movement ==\n"
    "  C-f / C-b    Forward / backward character\n"
    "  M-f / M-b    Forward / backward word\n"
    "  C-n / C-p    Next / previous line\n"
    "  C-a / C-e    Beginning / end of line\n"
    "  M-< / M->    Beginning / end of buffer\n"
    "  C-v / M-v    Scroll down / up\n"
    "  C-l          Recenter\n\n"
    "== Editing ==\n"
    "  C-d          Delete character\n"
    "  M-d          Kill word\n"
    "  C-k          Kill to end of line\n"
    "  C-w          Kill region\n"
    "  M-w          Copy region\n"
    "  C-y          Yank (paste)\n"
    "  M-y          Yank pop (cycle kill ring)\n"
    "  C-/          Undo\n"
    "  C-x u        Undo\n\n"
    "== Files & Buffers ==\n"
    "  C-x C-f      Find file\n"
    "  C-x C-s      Save buffer\n"
    "  C-x s        Save all buffers\n"
    "  C-x b        Switch buffer\n"
    "  C-x k        Kill buffer\n"
    "  C-x C-b      List buffers\n\n"
    "== Windows ==\n"
    "  C-x 2        Split horizontally\n"
    "  C-x 3        Split vertically\n"
    "  C-x 1        Delete other windows\n"
    "  C-x 0        Delete this window\n"
    "  C-x o        Other window\n\n"
    "== Search & Replace ==\n"
    "  C-s          Search forward\n"
    "  C-r          Search backward\n"
    "  M-%          Query replace\n\n"
    "== Commands ==\n"
    "  M-x          Execute command by name\n"
    "  C-g          Keyboard quit\n"
    "  C-h k        Describe key\n"
    "  C-h f        Describe function\n\n"
    "== Org Mode ==\n"
    "  TAB          Cycle visibility\n"
    "  M-RET        Insert heading\n"
    "  C-c C-t      Toggle TODO\n"
    "  C-c C-c      Execute src block\n\n"
    "== Gemacs-Specific ==\n"
    "  M-x magit-status   Git integration\n"
    "  M-x treemacs       File tree\n"
    "  M-x shell          Shell\n"
    "  M-x eshell         Gerbil shell\n"
    "  M-x term           Terminal\n")))
    (open-output-buffer app "*Tutorial*" text)))

;;; --- CUA mode (stub) ---
(def *cua-mode* #f)
(def (cmd-cua-mode app)
  "Toggle CUA keybindings (C-c/C-x/C-v for copy/cut/paste)."
  (set! *cua-mode* (not *cua-mode*))
  (echo-message! (app-state-echo app)
    (if *cua-mode* "CUA mode enabled" "CUA mode disabled")))

;;; --- Org archive subtree ---
(def (cmd-org-archive-subtree app)
  "Archive the current org subtree."
  (echo-message! (app-state-echo app) "Archive subtree: not yet implemented"))

;;; --- Org toggle heading ---
(def (cmd-org-toggle-heading app)
  "Toggle between heading and normal text."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-text (editor-get-line ed line-num))
         (trimmed (string-trim line-text)))
    (if (and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\*))
      ;; Remove heading prefix
      (let* ((stars (let lp ((i 0))
                      (if (and (< i (string-length trimmed))
                               (char=? (string-ref trimmed i) #\*))
                        (lp (+ i 1)) i)))
             (rest (string-trim (substring trimmed stars (string-length trimmed))))
             (start (editor-position-from-line ed line-num))
             (line-len (string-length line-text)))
        ;; Delete the line content and insert replacement
        (editor-delete-range ed start line-len)
        (editor-insert-text ed start rest))
      ;; Add heading prefix
      (let* ((start (editor-position-from-line ed line-num)))
        (editor-insert-text ed start "* ")))))

;;; --- Magit init ---
(def (cmd-magit-init app)
  "Initialize a new git repository."
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "Git init failed"))
    (lambda ()
      (let* ((fr (app-state-frame app))
             (buf (edit-window-buffer (current-window fr)))
             (path (and buf (buffer-file-path buf)))
             (dir (if path (path-directory path) ".")))
        (run-process ["git" "init" dir] coprocess: void)
        (echo-message! (app-state-echo app)
          (string-append "Initialized git repo in " dir))))))

;;; --- Magit tag ---
(def (cmd-magit-tag app)
  "Create a git tag."
  (let ((tag (app-read-string app "Tag name: ")))
    (when (and tag (not (string-empty? tag)))
      (with-catch
        (lambda (e) (echo-message! (app-state-echo app) "Tag failed"))
        (lambda ()
          (run-process ["git" "tag" tag] coprocess: void)
          (echo-message! (app-state-echo app)
            (string-append "Created tag: " tag)))))))

;;;============================================================================
;;; Batch 4: check-parens, count-lines-page, how-many
;;;============================================================================

;;; --- Check parens ---
(def (cmd-check-parens app)
  "Check for unbalanced parentheses in the current buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (len (string-length text))
         (stk '())
         (pairs '((#\( . #\)) (#\[ . #\]) (#\{ . #\}))))
    (let lp ((i 0) (stk '()))
      (cond
        ((>= i len)
         (if (null? stk)
           (echo-message! (app-state-echo app) "Parentheses are balanced")
           (let* ((pos (car stk))
                  (line (editor-line-from-position ed pos)))
             (echo-message! (app-state-echo app)
               (string-append "Unmatched opener at line " (number->string (+ line 1)))))))
        (else
         (let ((ch (string-ref text i)))
           (cond
             ((assoc ch pairs)
              (lp (+ i 1) (cons i stk)))
             ((find (lambda (p) (char=? ch (cdr p))) pairs)
              => (lambda (p)
                   (if (and (pair? stk)
                            (char=? (string-ref text (car stk)) (car p)))
                     (lp (+ i 1) (cdr stk))
                     (let ((line (editor-line-from-position ed i)))
                       (echo-message! (app-state-echo app)
                         (string-append "Unmatched " (string ch) " at line "
                           (number->string (+ line 1))))))))
             (else (lp (+ i 1) stk)))))))))

;;; --- Count lines page ---
(def (cmd-count-lines-page app)
  "Count lines on the current page (delimited by form-feed)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text))
         ;; Find page boundaries (form-feed = \f = char 12)
         (page-start (let lp ((i (- pos 1)))
                       (cond ((<= i 0) 0)
                             ((char=? (string-ref text i) (integer->char 12)) (+ i 1))
                             (else (lp (- i 1))))))
         (page-end (let lp ((i pos))
                     (cond ((>= i len) len)
                           ((char=? (string-ref text i) (integer->char 12)) i)
                           (else (lp (+ i 1))))))
         ;; Count lines
         (count-lines (lambda (start end)
                        (let lp ((i start) (n 0))
                          (cond ((>= i end) n)
                                ((char=? (string-ref text i) #\newline) (lp (+ i 1) (+ n 1)))
                                (else (lp (+ i 1) n))))))
         (before (count-lines page-start pos))
         (after (count-lines pos page-end))
         (total (+ before after)))
    (echo-message! (app-state-echo app)
      (string-append "Page has " (number->string total) " lines ("
        (number->string before) " + " (number->string after) ")"))))

;;; --- How many ---
(def (cmd-how-many app)
  "Count regexp matches from point to end of buffer."
  (let ((pattern (app-read-string app "How many (regexp): ")))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (pos (editor-get-current-pos ed))
             (rest (substring text pos (string-length text)))
             (rx (with-catch (lambda (e) #f) (lambda () (pregexp pattern)))))
        (if (not rx)
          (echo-message! (app-state-echo app) "Invalid regexp")
          (let lp ((s rest) (count 0))
            (let ((m (pregexp-match rx s)))
              (if (not m)
                (echo-message! (app-state-echo app)
                  (string-append (number->string count) " occurrences"))
                (let* ((match-str (car m))
                       (match-len (string-length match-str))
                       (idx (string-contains s match-str)))
                  (if (or (not idx) (= match-len 0))
                    (echo-message! (app-state-echo app)
                      (string-append (number->string count) " occurrences"))
                    (lp (substring s (+ idx (max 1 match-len)) (string-length s))
                        (+ count 1))))))))))))

;;;============================================================================
;;; Batch 5: delete-directory, set-file-modes, dired-do-chown, butterfly
;;;============================================================================

;;; --- Delete directory ---
(def (cmd-delete-directory app)
  "Delete a directory (must be empty)."
  (let ((dir (app-read-string app "Delete directory: ")))
    (when (and dir (not (string-empty? dir)))
      (with-catch
        (lambda (e) (echo-message! (app-state-echo app)
                      (string-append "Cannot delete: " dir)))
        (lambda ()
          (delete-directory dir)
          (echo-message! (app-state-echo app)
            (string-append "Deleted directory: " dir)))))))

;;; --- Set file modes (chmod) ---
(def (cmd-set-file-modes app)
  "Set file permissions (chmod)."
  (let* ((fr (app-state-frame app))
         (buf (edit-window-buffer (current-window fr)))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-message! (app-state-echo app) "No file in current buffer")
      (let ((mode (app-read-string app (string-append "chmod " path " to: "))))
        (when (and mode (not (string-empty? mode)))
          (with-catch
            (lambda (e) (echo-message! (app-state-echo app) "chmod failed"))
            (lambda ()
              (run-process ["chmod" mode path] coprocess: void)
              (echo-message! (app-state-echo app)
                (string-append "Set " path " to mode " mode)))))))))

;;; --- Dired do chown ---
(def (cmd-dired-do-chown app)
  "Change file owner in dired."
  (let* ((fr (app-state-frame app))
         (buf (edit-window-buffer (current-window fr)))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-message! (app-state-echo app) "No file in current buffer")
      (let ((owner (app-read-string app (string-append "chown " path " to: "))))
        (when (and owner (not (string-empty? owner)))
          (with-catch
            (lambda (e) (echo-message! (app-state-echo app) "chown failed"))
            (lambda ()
              (run-process ["chown" owner path] coprocess: void)
              (echo-message! (app-state-echo app)
                (string-append "Changed owner of " path " to " owner)))))))))

;;; --- Butterfly ---
(def (cmd-butterfly app)
  "A butterfly flapping its wings causes a gentle breeze..."
  (echo-message! (app-state-echo app)
    "The butterflies have set the universe in motion."))

;;;============================================================================
;;; Batch 7: debug stubs
;;;============================================================================

(def (cmd-debug-on-entry app)
  "Set debug-on-entry for a function (stub)."
  (echo-message! (app-state-echo app) "debug-on-entry: not yet implemented"))

(def (cmd-cancel-debug-on-entry app)
  "Cancel debug-on-entry (stub)."
  (echo-message! (app-state-echo app) "cancel-debug-on-entry: not yet implemented"))

;;;============================================================================
;;; Batch 12: Emacs-standard alias registrations (editor-cmds chain scope)
;;;============================================================================

(def (register-batch12-aliases!)
  ;; Undo/redo aliases
  (register-command! 'undo-redo cmd-redo)
  (register-command! 'undo-only cmd-undo)
  ;; Display/mode aliases
  (register-command! 'display-time-mode cmd-display-time)
  ;; Outline/folding aliases
  (register-command! 'outline-hide-all cmd-fold-all)
  (register-command! 'outline-show-all cmd-unfold-all)
  (register-command! 'outline-cycle cmd-toggle-fold)
  ;; Dired aliases
  (register-command! 'dired-do-touch cmd-dired-create-directory)
  (register-command! 'dired-copy-filename-as-kill cmd-copy-buffer-name)
  (register-command! 'dired-hide-dotfiles cmd-dired-hide-details)
  ;; Emacs base mode-name aliases → toggle commands (editor-cmds scope)
  (register-command! 'delete-trailing-whitespace-mode cmd-toggle-delete-trailing-whitespace-on-save)
  (register-command! 'menu-bar-mode cmd-toggle-menu-bar-mode))

;;;============================================================================
;;; iedit-mode: rename symbol at point across buffer
;;;============================================================================

(def (iedit-word-char? ch)
  "Return #t if ch is a word character (alphanumeric, underscore, hyphen)."
  (or (char-alphabetic? ch) (char-numeric? ch)
      (char=? ch #\_) (char=? ch #\-)))

(def (iedit-get-word-at-point ed)
  "Get word boundaries at cursor. Returns (values word start end) or (values #f 0 0)."
  (let* ((pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (or (>= pos len) (= len 0))
      (values #f 0 0)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (iedit-word-char? (string-ref text (- i 1)))))
                        i
                        (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (iedit-word-char? (string-ref text i))))
                      i
                      (loop (+ i 1)))))
             (word (substring text start end)))
        (if (> (string-length word) 0)
          (values word start end)
          (values #f 0 0))))))

(def (iedit-count-whole-word text word)
  "Count whole-word occurrences of word in text."
  (let ((wlen (string-length word))
        (tlen (string-length text)))
    (let loop ((i 0) (count 0))
      (if (> (+ i wlen) tlen) count
        (if (and (string=? (substring text i (+ i wlen)) word)
                 ;; Check word boundary before
                 (or (= i 0)
                     (not (iedit-word-char? (string-ref text (- i 1)))))
                 ;; Check word boundary after
                 (or (= (+ i wlen) tlen)
                     (not (iedit-word-char? (string-ref text (+ i wlen))))))
          (loop (+ i wlen) (+ count 1))
          (loop (+ i 1) count))))))

(def (cmd-iedit-mode app)
  "Rename symbol at point across the buffer (iedit-mode).
   Gets the word at point, prompts for a replacement, and replaces all
   whole-word occurrences."
  (let* ((ed  (current-editor app))
         (echo (app-state-echo app))
         (fr   (app-state-frame app))
         (row  (- (frame-height fr) 1))
         (width (frame-width fr)))
    (let-values (((word _start _end) (iedit-get-word-at-point ed)))
      (if (not word)
        (echo-message! echo "No symbol at point")
        (let* ((text (editor-get-text ed))
               (count (iedit-count-whole-word text word))
               (prompt (string-append
                        "iedit (" (number->string count)
                        " of " word "): Replace with: "))
               (replacement (echo-read-string echo prompt row width)))
          (if (or (not replacement)
                  (string=? replacement "")
                  (string=? replacement word))
            (echo-message! echo "iedit: cancelled or no change")
            (let ((replaced 0))
              (with-undo-action ed
                (send-message ed SCI_SETTARGETSTART 0 0)
                (send-message ed SCI_SETTARGETEND (editor-get-text-length ed) 0)
                (send-message ed SCI_SETSEARCHFLAGS SCFIND_WHOLEWORD 0)
                (let loop ()
                  (let ((found (send-message/string ed SCI_SEARCHINTARGET word)))
                    (when (>= found 0)
                      (send-message/string ed SCI_REPLACETARGET replacement)
                      (set! replaced (+ replaced 1))
                      (send-message ed SCI_SETTARGETSTART
                        (+ found (string-length replacement)) 0)
                      (send-message ed SCI_SETTARGETEND
                        (editor-get-text-length ed) 0)
                      (loop)))))
              (echo-message! echo
                (string-append "iedit: replaced "
                  (number->string replaced) " occurrences")))))))))

;;;============================================================================
;;; Multi-terminal management (multi-vterm parity)
;;;============================================================================

(def (get-tui-terminal-buffers)
  "Return list of terminal buffers."
  (filter terminal-buffer? *buffer-list*))

(def (tui-switch-to-terminal! app buf)
  "Switch to terminal buffer BUF in the current window."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)))

(def (cmd-term-list app)
  "Switch between terminal buffers via completion."
  (let ((terms (get-tui-terminal-buffers)))
    (if (null? terms)
      (echo-message! (app-state-echo app) "No terminal buffers")
      (let* ((echo (app-state-echo app))
             (fr (app-state-frame app))
             (row (- (frame-height fr) 1))
             (width (frame-width fr))
             (names (map buffer-name terms))
             (choice (echo-read-string-with-completion
                       echo "Terminal: " names row width)))
        (when (and choice (not (string=? choice "")))
          (let ((target (find (lambda (b) (string=? (buffer-name b) choice)) terms)))
            (when target
              (tui-switch-to-terminal! app target))))))))

(def (cmd-term-next app)
  "Switch to the next terminal buffer, cycling around."
  (let* ((terms (get-tui-terminal-buffers))
         (cur (current-buffer-from-app app)))
    (cond
      ((null? terms)
       (echo-message! (app-state-echo app) "No terminal buffers"))
      ((not (terminal-buffer? cur))
       (tui-switch-to-terminal! app (car terms)))
      (else
       (let loop ((rest terms))
         (cond
           ((null? rest)
            (tui-switch-to-terminal! app (car terms)))
           ((eq? (car rest) cur)
            (if (null? (cdr rest))
              (tui-switch-to-terminal! app (car terms))
              (tui-switch-to-terminal! app (cadr rest))))
           (else (loop (cdr rest)))))))))

(def (cmd-term-prev app)
  "Switch to the previous terminal buffer, cycling around."
  (let* ((terms (get-tui-terminal-buffers))
         (cur (current-buffer-from-app app))
         (last-term (and (pair? terms) (list-ref terms (- (length terms) 1)))))
    (cond
      ((null? terms)
       (echo-message! (app-state-echo app) "No terminal buffers"))
      ((not (terminal-buffer? cur))
       (tui-switch-to-terminal! app last-term))
      (else
       (let loop ((rest terms) (prev last-term))
         (cond
           ((null? rest)
            (tui-switch-to-terminal! app prev))
           ((eq? (car rest) cur)
            (tui-switch-to-terminal! app prev))
           (else (loop (cdr rest) (car rest)))))))))

;;;============================================================================
;;; EWW Bookmarks
;;;============================================================================

(def *eww-bookmarks* '())  ; list of (title . url) pairs
(def *eww-bookmarks-file* (path-expand ".gemacs-eww-bookmarks" (user-info-home (user-info (user-name)))))

(def (eww-load-bookmarks!)
  "Load EWW bookmarks from disk."
  (when (file-exists? *eww-bookmarks-file*)
    (with-exception-catcher
      (lambda (e) #f)
      (lambda ()
        (set! *eww-bookmarks*
          (with-input-from-file *eww-bookmarks-file*
            (lambda ()
              (let loop ((result '()))
                (let ((line (read-line)))
                  (if (eof-object? line)
                    (reverse result)
                    (let ((tab-pos (string-index line #\tab)))
                      (if tab-pos
                        (loop (cons (cons (substring line 0 tab-pos)
                                         (substring line (+ tab-pos 1) (string-length line)))
                                    result))
                        (loop result)))))))))))))

(def (eww-save-bookmarks!)
  "Persist EWW bookmarks to disk."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (with-output-to-file *eww-bookmarks-file*
        (lambda ()
          (for-each (lambda (bm)
                      (display (car bm))
                      (display "\t")
                      (display (cdr bm))
                      (newline))
            *eww-bookmarks*))))))

(def (cmd-eww-add-bookmark app)
  "Bookmark the current EWW page."
  (let ((echo (app-state-echo app)))
    (if (not *eww-current-url*)
      (echo-message! echo "No page to bookmark")
      (let* ((fr (app-state-frame app))
             (row (- (frame-height fr) 1))
             (width (frame-width fr))
             (title (echo-read-string echo "Bookmark title: " row width)))
        (when (and title (not (string=? title "")))
          (eww-load-bookmarks!)
          (set! *eww-bookmarks*
            (cons (cons title *eww-current-url*) *eww-bookmarks*))
          (eww-save-bookmarks!)
          (echo-message! echo (string-append "Bookmarked: " title)))))))

(def (cmd-eww-list-bookmarks app)
  "Show EWW bookmarks in a buffer. Navigate and press RET to open."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app)))
    (eww-load-bookmarks!)
    (if (null? *eww-bookmarks*)
      (echo-message! echo "No bookmarks saved")
      (let* ((lines (let loop ((bms *eww-bookmarks*) (i 1) (acc '()))
                      (if (null? bms)
                        (reverse acc)
                        (let ((bm (car bms)))
                          (loop (cdr bms) (+ i 1)
                            (cons (string-append (number->string i) ". "
                                    (car bm) " — " (cdr bm))
                              acc))))))
             (text (string-append "EWW Bookmarks:\n\n"
                     (string-join lines "\n") "\n")))
        (forge-show-in-buffer! app "*EWW Bookmarks*" text)
        (echo-message! echo
          (string-append (number->string (length *eww-bookmarks*)) " bookmarks"))))))

;;;============================================================================
;;; Forge (GitHub integration via gh CLI)
;;;============================================================================

(def (forge-run-gh args)
  "Run gh CLI command and return output string, or #f on failure."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (let ((proc (open-process
                    (list path: "gh"
                          arguments: args
                          stdin-redirection: #f
                          stdout-redirection: #t
                          stderr-redirection: #t))))
        (let ((output (read-all-as-string proc)))
          (process-status proc)
          (if (zero? (process-status proc))
            output
            #f))))))

(def (forge-show-in-buffer! app buf-name text)
  "Create or reuse a buffer, fill with text, switch to it."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (existing (buffer-by-name buf-name))
         (buf (or existing (buffer-create! buf-name ed))))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer (current-window fr)) buf)
    (send-message ed SCI_CLEARALL 0)
    (editor-insert-text ed 0 text)
    (send-message ed SCI_GOTOPOS 0)))

(def (cmd-forge-list-prs app)
  "List open pull requests for the current project."
  (let* ((echo (app-state-echo app))
         (output (forge-run-gh ["pr" "list" "--limit" "20"])))
    (if (not output)
      (echo-error! echo "forge: failed to list PRs (is gh installed?)")
      (begin
        (forge-show-in-buffer! app "*Forge PRs*"
          (string-append "Pull Requests:\n\n" output))
        (echo-message! echo "Forge: PRs loaded")))))

(def (cmd-forge-list-issues app)
  "List open issues for the current project."
  (let* ((echo (app-state-echo app))
         (output (forge-run-gh ["issue" "list" "--limit" "20"])))
    (if (not output)
      (echo-error! echo "forge: failed to list issues (is gh installed?)")
      (begin
        (forge-show-in-buffer! app "*Forge Issues*"
          (string-append "Issues:\n\n" output))
        (echo-message! echo "Forge: issues loaded")))))

(def (cmd-forge-view-pr app)
  "View details of a specific PR by number."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (num (echo-read-string echo "PR number: " row width)))
    (when (and num (not (string=? num "")))
      (let ((output (forge-run-gh ["pr" "view" num])))
        (if (not output)
          (echo-error! echo (string-append "forge: failed to view PR #" num))
          (begin
            (forge-show-in-buffer! app (string-append "*Forge PR #" num "*") output)
            (echo-message! echo (string-append "Forge: PR #" num))))))))

(def (cmd-forge-create-pr app)
  "Create a new PR via gh CLI."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (title (echo-read-string echo "PR title: " row width)))
    (when (and title (not (string=? title "")))
      (let ((output (forge-run-gh ["pr" "create" "--title" title "--fill"])))
        (if (not output)
          (echo-error! echo "forge: failed to create PR")
          (echo-message! echo (string-append "Created: " (string-trim output))))))))
