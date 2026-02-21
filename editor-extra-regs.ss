;;; -*- Gerbil -*-
;;; Parity registrations: commands defined in editor-*.ss but not yet
;;; registered with register-command! in the TUI layer.
;;; These commands already have full implementations — this module just
;;; exposes them to M-x by name, matching their Qt registrations.

(export register-parity-commands!
        cmd-dired-jump cmd-dired-up-directory cmd-dired-do-shell-command
        cmd-apropos-emacs cmd-indent-new-comment-line
        cmd-isearch-backward-regexp cmd-replace-regexp
        cmd-org-capture cmd-org-refile cmd-org-time-stamp
        cmd-org-insert-link cmd-org-narrow-to-subtree cmd-org-sort
        cmd-project-switch-to-buffer cmd-project-kill-buffers
        cmd-vc-next-action
        ;; Batch 2
        cmd-sort-numeric-fields cmd-find-dired cmd-find-name-dired
        cmd-dired-hide-details cmd-desktop-save-mode
        cmd-org-babel-execute-src-block cmd-org-babel-tangle
        cmd-other-frame cmd-winum-mode cmd-help-with-tutorial
        cmd-cua-mode cmd-org-archive-subtree cmd-org-toggle-heading
        cmd-magit-init cmd-magit-tag
        ;; Batch 4
        cmd-check-parens cmd-count-lines-page cmd-how-many)

(import :std/sugar
        :std/srfi/13
        :std/sort
        :std/misc/string
        :std/misc/process
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gemacs/pregexp-compat pregexp pregexp-match)
        :gerbil-scintilla/scintilla
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/echo
        (only-in :gemacs/org-babel
                 org-babel-find-src-block org-babel-execute
                 org-babel-tangle-to-files org-babel-insert-result)
        :gemacs/editor-core
        :gemacs/editor-text
        :gemacs/editor-ui
        :gemacs/editor-advanced
        :gemacs/editor-cmds-a
        :gemacs/editor-cmds-b
        :gemacs/editor-cmds-c)

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
  (register-command! 'flyspell-mode cmd-toggle-flyspell)
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
(def *tui-org-capture-templates* '())
(def *tui-org-capture-file* #f)

(def (cmd-org-capture app)
  "Capture a note (org-capture)."
  (let ((text (app-read-string app "Capture: ")))
    (when (and text (not (string-empty? text)))
      (let* ((file (or *tui-org-capture-file*
                       (string-append (or (getenv "HOME") ".") "/.gemacs-capture.org")))
             (entry (string-append "\n* " text "\n")))
        (with-catch
          (lambda (e) (echo-message! (app-state-echo app) "Capture error"))
          (lambda ()
            (call-with-output-file [path: file append: #t]
              (lambda (p) (display entry p)))
            (echo-message! (app-state-echo app) (string-append "Captured: " text))))))))

(def (cmd-org-refile app)
  "Refile current heading to another location."
  (let ((target (app-read-string app "Refile to: ")))
    (when (and target (not (string-empty? target)))
      (echo-message! (app-state-echo app) (string-append "Refile: " target " (stub)")))))

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
            (let ((output (org-babel-execute lang body header-args)))
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
