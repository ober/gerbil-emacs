;;; -*- Gerbil -*-
;;; Extra TUI editor commands for gerbil-emacs
;;; Facade module: imports sub-modules and registers all commands.

(export register-extra-commands!
        winner-save-config!)

(import :gerbil-emacs/core
        :gerbil-emacs/editor-extra-helpers
        :gerbil-emacs/editor-extra-org
        :gerbil-emacs/editor-extra-web
        :gerbil-emacs/editor-extra-vcs
        :gerbil-emacs/editor-extra-editing
        :gerbil-emacs/editor-extra-tools
        :gerbil-emacs/editor-extra-tools2
        :gerbil-emacs/editor-extra-media
        :gerbil-emacs/editor-extra-modes
        :gerbil-emacs/editor-extra-final)

;;;============================================================================
;;; Register extra commands
;;;============================================================================

(def (register-extra-commands!)
  ;; Task #46: org-mode, windmove, winner, VC, mail, sessions, etc.
  ;; Org-mode stubs
  (register-command! 'org-mode cmd-org-mode)
  (register-command! 'org-todo cmd-org-todo)
  (register-command! 'org-schedule cmd-org-schedule)
  (register-command! 'org-deadline cmd-org-deadline)
  (register-command! 'org-agenda cmd-org-agenda)
  (register-command! 'org-export cmd-org-export)
  (register-command! 'org-table-create cmd-org-table-create)
  (register-command! 'org-link cmd-org-link)
  (register-command! 'org-store-link cmd-org-store-link)
  (register-command! 'org-open-at-point cmd-org-open-at-point)
  (register-command! 'org-cycle cmd-org-cycle)
  (register-command! 'org-shift-tab cmd-org-shift-tab)
  (register-command! 'org-promote cmd-org-promote)
  (register-command! 'org-demote cmd-org-demote)
  (register-command! 'org-move-subtree-up cmd-org-move-subtree-up)
  (register-command! 'org-move-subtree-down cmd-org-move-subtree-down)
  (register-command! 'org-toggle-checkbox cmd-org-toggle-checkbox)
  (register-command! 'org-priority cmd-org-priority)
  (register-command! 'org-set-tags cmd-org-set-tags)
  (register-command! 'org-insert-heading cmd-org-insert-heading)
  (register-command! 'org-insert-src-block cmd-org-insert-src-block)
  (register-command! 'org-clock-in cmd-org-clock-in)
  (register-command! 'org-clock-out cmd-org-clock-out)
  (register-command! 'org-template-expand cmd-org-template-expand)
  ;; Calendar/diary
  (register-command! 'calendar cmd-calendar)
  (register-command! 'diary-view-entries cmd-diary-view-entries)
  ;; EWW browser
  (register-command! 'eww cmd-eww)
  (register-command! 'eww-browse-url cmd-eww-browse-url)
  (register-command! 'browse-url-at-point cmd-browse-url-at-point)
  ;; Windmove
  (register-command! 'windmove-left cmd-windmove-left)
  (register-command! 'windmove-right cmd-windmove-right)
  (register-command! 'windmove-up cmd-windmove-up)
  (register-command! 'windmove-down cmd-windmove-down)
  ;; Winner mode
  (register-command! 'winner-undo cmd-winner-undo)
  (register-command! 'winner-redo cmd-winner-redo)
  ;; Tab-bar
  (register-command! 'tab-new cmd-tab-new)
  (register-command! 'tab-close cmd-tab-close)
  (register-command! 'tab-next cmd-tab-next)
  (register-command! 'tab-previous cmd-tab-previous)
  (register-command! 'tab-rename cmd-tab-rename)
  (register-command! 'tab-move cmd-tab-move)
  ;; VC extras
  (register-command! 'vc-register cmd-vc-register)
  (register-command! 'vc-dir cmd-vc-dir)
  (register-command! 'vc-pull cmd-vc-pull)
  (register-command! 'vc-push cmd-vc-push)
  (register-command! 'vc-create-tag cmd-vc-create-tag)
  (register-command! 'vc-print-log cmd-vc-print-log)
  (register-command! 'vc-stash cmd-vc-stash)
  (register-command! 'vc-stash-pop cmd-vc-stash-pop)
  ;; Mail
  (register-command! 'compose-mail cmd-compose-mail)
  (register-command! 'rmail cmd-rmail)
  (register-command! 'gnus cmd-gnus)
  ;; Sessions
  (register-command! 'desktop-save cmd-desktop-save)
  (register-command! 'desktop-read cmd-desktop-read)
  (register-command! 'desktop-clear cmd-desktop-clear)
  ;; Man pages
  (register-command! 'man cmd-man)
  (register-command! 'woman cmd-woman)
  ;; Macro extras
  (register-command! 'apply-macro-to-region-lines cmd-apply-macro-to-region-lines)
  (register-command! 'edit-kbd-macro cmd-edit-kbd-macro)
  ;; Compilation
  (register-command! 'recompile cmd-recompile)
  (register-command! 'kill-compilation cmd-kill-compilation)
  ;; Flyspell
  (register-command! 'flyspell-auto-correct-word cmd-flyspell-auto-correct-word)
  (register-command! 'flyspell-goto-next-error cmd-flyspell-goto-next-error)
  ;; Multiple cursors
  (register-command! 'mc-mark-next-like-this cmd-mc-mark-next-like-this)
  (register-command! 'mc-mark-previous-like-this cmd-mc-mark-previous-like-this)
  (register-command! 'mc-mark-all-like-this cmd-mc-mark-all-like-this)
  (register-command! 'mc-edit-lines cmd-mc-edit-lines)
  ;; Package management
  (register-command! 'package-list-packages cmd-package-list-packages)
  (register-command! 'package-install cmd-package-install)
  (register-command! 'package-delete cmd-package-delete)
  (register-command! 'package-refresh-contents cmd-package-refresh-contents)
  ;; Custom
  (register-command! 'customize-group cmd-customize-group)
  (register-command! 'customize-variable cmd-customize-variable)
  (register-command! 'customize-themes cmd-customize-themes)
  ;; Diff mode
  (register-command! 'diff-mode cmd-diff-mode)
  (register-command! 'diff-apply-hunk cmd-diff-apply-hunk)
  (register-command! 'diff-revert-hunk cmd-diff-revert-hunk)
  (register-command! 'diff-goto-source cmd-diff-goto-source)
  ;; Artist mode
  (register-command! 'artist-mode cmd-artist-mode)
  ;; Tramp
  (register-command! 'tramp-cleanup-all-connections cmd-tramp-cleanup-all-connections)
  ;; Process
  (register-command! 'proced cmd-proced)
  ;; Paredit
  (register-command! 'paredit-wrap-round cmd-paredit-wrap-round)
  (register-command! 'paredit-wrap-square cmd-paredit-wrap-square)
  (register-command! 'paredit-wrap-curly cmd-paredit-wrap-curly)
  (register-command! 'paredit-splice-sexp cmd-paredit-splice-sexp)
  (register-command! 'paredit-raise-sexp cmd-paredit-raise-sexp)
  ;; Remote editing
  (register-command! 'find-file-ssh cmd-find-file-ssh)
  ;; Text manipulation
  (register-command! 'string-inflection-cycle cmd-string-inflection-cycle)
  ;; Ediff
  (register-command! 'ediff-files cmd-ediff-files)
  (register-command! 'ediff-regions cmd-ediff-regions)
  ;; Undo tree
  (register-command! 'undo-tree-visualize cmd-undo-tree-visualize)
  ;; Server
  (register-command! 'server-start cmd-server-start)
  (register-command! 'server-edit cmd-server-edit)
  ;; Navigation
  (register-command! 'pop-global-mark cmd-pop-global-mark)
  (register-command! 'set-goal-column cmd-set-goal-column)
  (register-command! 'cd cmd-cd)
  ;; Misc
  (register-command! 'display-prefix cmd-display-prefix)
  (register-command! 'digit-argument cmd-digit-argument)
  (register-command! 'negative-argument cmd-negative-argument)
  (register-command! 'suspend-emacs cmd-suspend-emacs)
  (register-command! 'save-buffers-kill-emacs cmd-save-buffers-kill-emacs)
  (register-command! 'view-mode cmd-view-mode)
  (register-command! 'doc-view-mode cmd-doc-view-mode)
  (register-command! 'speedbar cmd-speedbar)
  (register-command! 'world-clock cmd-world-clock)
  (register-command! 'display-battery cmd-display-battery)
  (register-command! 'uptime cmd-uptime)
  (register-command! 'kmacro-set-counter cmd-kmacro-set-counter)
  (register-command! 'kmacro-insert-counter cmd-kmacro-insert-counter)
  (register-command! 'whitespace-report cmd-whitespace-report)
  (register-command! 'describe-coding-system cmd-describe-coding-system)
  (register-command! 'set-terminal-coding-system cmd-set-terminal-coding-system)
  (register-command! 'overwrite-mode cmd-overwrite-mode)
  ;; Task #47: xref, ibuffer, which-key, markdown, auto-insert, and more
  ;; Xref
  (register-command! 'xref-find-definitions cmd-xref-find-definitions)
  (register-command! 'xref-find-references cmd-xref-find-references)
  (register-command! 'xref-find-apropos cmd-xref-find-apropos)
  (register-command! 'xref-go-back cmd-xref-go-back)
  (register-command! 'xref-go-forward cmd-xref-go-forward)
  ;; Ibuffer
  (register-command! 'ibuffer cmd-ibuffer)
  (register-command! 'ibuffer-mark cmd-ibuffer-mark)
  (register-command! 'ibuffer-delete cmd-ibuffer-delete)
  (register-command! 'ibuffer-do-kill cmd-ibuffer-do-kill)
  ;; Which-key
  (register-command! 'which-key cmd-which-key)
  ;; Markdown
  (register-command! 'markdown-mode cmd-markdown-mode)
  (register-command! 'markdown-preview cmd-markdown-preview)
  (register-command! 'markdown-insert-header cmd-markdown-insert-header)
  (register-command! 'markdown-insert-bold cmd-markdown-insert-bold)
  (register-command! 'markdown-insert-italic cmd-markdown-insert-italic)
  (register-command! 'markdown-insert-code cmd-markdown-insert-code)
  (register-command! 'markdown-insert-link cmd-markdown-insert-link)
  (register-command! 'markdown-insert-image cmd-markdown-insert-image)
  (register-command! 'markdown-insert-code-block cmd-markdown-insert-code-block)
  (register-command! 'markdown-insert-list-item cmd-markdown-insert-list-item)
  ;; Auto-insert
  (register-command! 'auto-insert cmd-auto-insert)
  (register-command! 'auto-insert-mode cmd-auto-insert-mode)
  ;; Text scale
  (register-command! 'text-scale-increase cmd-text-scale-increase)
  (register-command! 'text-scale-decrease cmd-text-scale-decrease)
  (register-command! 'text-scale-reset cmd-text-scale-reset)
  ;; Browse kill ring
  (register-command! 'browse-kill-ring cmd-browse-kill-ring)
  ;; Flycheck
  (register-command! 'flycheck-mode cmd-flycheck-mode)
  (register-command! 'flycheck-next-error cmd-flycheck-next-error)
  (register-command! 'flycheck-previous-error cmd-flycheck-previous-error)
  (register-command! 'flycheck-list-errors cmd-flycheck-list-errors)
  ;; Treemacs
  (register-command! 'treemacs cmd-treemacs)
  (register-command! 'treemacs-find-file cmd-treemacs-find-file)
  ;; Magit
  (register-command! 'magit-status cmd-magit-status)
  (register-command! 'magit-log cmd-magit-log)
  (register-command! 'magit-diff cmd-magit-diff)
  (register-command! 'magit-commit cmd-magit-commit)
  (register-command! 'magit-stage-file cmd-magit-stage-file)
  (register-command! 'magit-unstage-file cmd-magit-unstage-file)
  (register-command! 'magit-branch cmd-magit-branch)
  (register-command! 'magit-checkout cmd-magit-checkout)
  ;; Minibuffer
  (register-command! 'minibuffer-complete cmd-minibuffer-complete)
  (register-command! 'minibuffer-keyboard-quit cmd-minibuffer-keyboard-quit)
  ;; Abbrev extras
  (register-command! 'define-global-abbrev cmd-define-global-abbrev)
  (register-command! 'define-mode-abbrev cmd-define-mode-abbrev)
  (register-command! 'unexpand-abbrev cmd-unexpand-abbrev)
  ;; Hippie expand
  (register-command! 'hippie-expand-undo cmd-hippie-expand-undo)
  ;; Compilation
  (register-command! 'next-error-function cmd-next-error-function)
  (register-command! 'previous-error-function cmd-previous-error-function)
  ;; Bookmark extras
  (register-command! 'bookmark-bmenu-list cmd-bookmark-bmenu-list)
  ;; Rectangle extras
  (register-command! 'rectangle-mark-mode cmd-rectangle-mark-mode)
  (register-command! 'number-to-register cmd-number-to-register)
  ;; Isearch extras
  (register-command! 'isearch-toggle-case-fold cmd-isearch-toggle-case-fold)
  (register-command! 'isearch-toggle-regexp cmd-isearch-toggle-regexp)
  ;; Semantic / imenu / tags
  (register-command! 'semantic-mode cmd-semantic-mode)
  (register-command! 'imenu-anywhere cmd-imenu-anywhere)
  (register-command! 'tags-search cmd-tags-search)
  (register-command! 'tags-query-replace cmd-tags-query-replace)
  (register-command! 'visit-tags-table cmd-visit-tags-table)
  ;; Whitespace extras
  (register-command! 'whitespace-toggle-options cmd-whitespace-toggle-options)
  ;; Highlight
  (register-command! 'highlight-regexp cmd-highlight-regexp)
  (register-command! 'unhighlight-regexp cmd-unhighlight-regexp)
  ;; Server extras
  (register-command! 'server-force-delete cmd-server-force-delete)
  ;; Help extras
  (register-command! 'help-for-help cmd-help-for-help)
  (register-command! 'help-quick cmd-help-quick)
  ;; Theme
  (register-command! 'disable-theme cmd-disable-theme)
  (register-command! 'describe-theme cmd-describe-theme)
  ;; Ediff extras
  (register-command! 'ediff-merge cmd-ediff-merge)
  (register-command! 'ediff-directories cmd-ediff-directories)
  ;; Window extras
  (register-command! 'window-divider-mode cmd-window-divider-mode)
  (register-command! 'scroll-bar-mode cmd-scroll-bar-mode)
  (register-command! 'menu-bar-open cmd-menu-bar-open)
  ;; Programming
  (register-command! 'toggle-prettify-symbols cmd-toggle-prettify-symbols)
  (register-command! 'subword-mode cmd-subword-mode)
  (register-command! 'superword-mode cmd-superword-mode)
  (register-command! 'glasses-mode cmd-glasses-mode)
  ;; Calculator
  (register-command! 'calculator cmd-calculator)
  ;; Text info
  (register-command! 'count-words-line cmd-count-words-line)
  (register-command! 'display-column-number cmd-display-column-number)
  (register-command! 'what-tab-width cmd-what-tab-width)
  (register-command! 'set-tab-width cmd-set-tab-width)
  (register-command! 'display-cursor-position cmd-display-cursor-position)
  ;; Display toggles
  (register-command! 'toggle-line-spacing cmd-toggle-line-spacing)
  (register-command! 'toggle-selection-mode cmd-toggle-selection-mode)
  (register-command! 'toggle-virtual-space cmd-toggle-virtual-space)
  (register-command! 'toggle-caret-style cmd-toggle-caret-style)
  ;; Compare
  (register-command! 'compare-windows cmd-compare-windows)
  ;; Frame
  (register-command! 'iconify-frame cmd-iconify-frame)
  (register-command! 'raise-frame cmd-raise-frame)
  ;; Face/font
  (register-command! 'set-face-attribute cmd-set-face-attribute)
  (register-command! 'list-faces-display cmd-list-faces-display)
  ;; Eshell extras
  (register-command! 'eshell-here cmd-eshell-here)
  ;; Calendar extras
  (register-command! 'calendar-goto-date cmd-calendar-goto-date)
  (register-command! 'calendar-holidays cmd-calendar-holidays)
  ;; ERC
  (register-command! 'erc cmd-erc)
  ;; TRAMP extras
  (register-command! 'tramp-cleanup-connections cmd-tramp-cleanup-connections)
  ;; LSP
  (register-command! 'lsp-find-declaration cmd-lsp-find-declaration)
  (register-command! 'lsp-find-implementation cmd-lsp-find-implementation)
  (register-command! 'lsp-rename cmd-lsp-rename)
  (register-command! 'lsp-format-buffer cmd-lsp-format-buffer)
  (register-command! 'lsp-code-actions cmd-lsp-code-actions)
  (register-command! 'lsp-describe-thing-at-point cmd-lsp-describe-thing-at-point)
  ;; DAP (Debug Adapter Protocol)
  (register-command! 'dap-debug cmd-dap-debug)
  (register-command! 'dap-breakpoint-toggle cmd-dap-breakpoint-toggle)
  (register-command! 'dap-continue cmd-dap-continue)
  (register-command! 'dap-step-over cmd-dap-step-over)
  (register-command! 'dap-step-in cmd-dap-step-in)
  (register-command! 'dap-step-out cmd-dap-step-out)
  ;; Snippets
  (register-command! 'yas-insert-snippet cmd-yas-insert-snippet)
  (register-command! 'yas-new-snippet cmd-yas-new-snippet)
  (register-command! 'yas-visit-snippet-file cmd-yas-visit-snippet-file)
  ;; Task #48: EWW, EMMS, PDF tools, Calc, ace-jump, expand-region, etc.
  ;; EWW extras
  (register-command! 'eww-back cmd-eww-back)
  (register-command! 'eww-forward cmd-eww-forward)
  (register-command! 'eww-reload cmd-eww-reload)
  (register-command! 'eww-download cmd-eww-download)
  (register-command! 'eww-copy-page-url cmd-eww-copy-page-url)
  ;; EMMS
  (register-command! 'emms cmd-emms)
  (register-command! 'emms-play-file cmd-emms-play-file)
  (register-command! 'emms-pause cmd-emms-pause)
  (register-command! 'emms-stop cmd-emms-stop)
  (register-command! 'emms-next cmd-emms-next)
  (register-command! 'emms-previous cmd-emms-previous)
  ;; PDF tools
  (register-command! 'pdf-view-mode cmd-pdf-view-mode)
  (register-command! 'pdf-view-next-page cmd-pdf-view-next-page)
  (register-command! 'pdf-view-previous-page cmd-pdf-view-previous-page)
  (register-command! 'pdf-view-goto-page cmd-pdf-view-goto-page)
  ;; Calc stack
  (register-command! 'calc-push cmd-calc-push)
  (register-command! 'calc-pop cmd-calc-pop)
  (register-command! 'calc-dup cmd-calc-dup)
  (register-command! 'calc-swap cmd-calc-swap)
  ;; Ace-jump/Avy
  (register-command! 'avy-goto-char cmd-avy-goto-char)
  (register-command! 'avy-goto-word cmd-avy-goto-word)
  (register-command! 'avy-goto-line cmd-avy-goto-line)
  ;; Expand-region
  (register-command! 'expand-region cmd-expand-region)
  (register-command! 'contract-region cmd-contract-region)
  ;; Smartparens
  (register-command! 'sp-forward-slurp-sexp cmd-sp-forward-slurp-sexp)
  (register-command! 'sp-forward-barf-sexp cmd-sp-forward-barf-sexp)
  (register-command! 'sp-backward-slurp-sexp cmd-sp-backward-slurp-sexp)
  (register-command! 'sp-backward-barf-sexp cmd-sp-backward-barf-sexp)
  ;; Project.el extras
  (register-command! 'project-switch-project cmd-project-switch-project)
  (register-command! 'project-find-regexp cmd-project-find-regexp)
  (register-command! 'project-shell cmd-project-shell)
  (register-command! 'project-dired cmd-project-dired)
  (register-command! 'project-eshell cmd-project-eshell)
  ;; JSON/XML
  (register-command! 'json-pretty-print cmd-json-pretty-print)
  (register-command! 'xml-format cmd-xml-format)
  ;; Notifications
  (register-command! 'notifications-list cmd-notifications-list)
  ;; Profiler
  (register-command! 'profiler-report cmd-profiler-report)
  ;; Narrowing extras
  (register-command! 'narrow-to-page cmd-narrow-to-page)
  ;; Encoding
  (register-command! 'describe-current-coding-system cmd-describe-current-coding-system)
  ;; File-local variables
  (register-command! 'add-file-local-variable cmd-add-file-local-variable)
  (register-command! 'add-dir-local-variable cmd-add-dir-local-variable)
  ;; Hippie expand
  (register-command! 'hippie-expand-file cmd-hippie-expand-file)
  ;; Register extras
  (register-command! 'frameset-to-register cmd-frameset-to-register)
  (register-command! 'window-configuration-to-register cmd-window-configuration-to-register)
  ;; Kmacro extras
  (register-command! 'kmacro-add-counter cmd-kmacro-add-counter)
  (register-command! 'kmacro-set-format cmd-kmacro-set-format)
  ;; Line number display
  (register-command! 'display-line-numbers-absolute cmd-display-line-numbers-absolute)
  (register-command! 'display-line-numbers-none cmd-display-line-numbers-none)
  ;; Scratch
  (register-command! 'scratch-buffer cmd-scratch-buffer)
  ;; Recentf
  (register-command! 'recentf-cleanup cmd-recentf-cleanup)
  ;; Hooks
  (register-command! 'add-hook cmd-add-hook)
  (register-command! 'remove-hook cmd-remove-hook)
  ;; Package archives
  (register-command! 'package-archives cmd-package-archives)
  ;; Auto-save
  (register-command! 'auto-save-mode cmd-auto-save-mode)
  (register-command! 'recover-file cmd-recover-file)
  ;; TRAMP
  (register-command! 'tramp-version cmd-tramp-version)
  ;; HL line
  (register-command! 'hl-line-mode cmd-hl-line-mode)
  ;; Occur
  (register-command! 'occur-rename-buffer cmd-occur-rename-buffer)
  ;; Printing
  (register-command! 'print-buffer cmd-print-buffer)
  (register-command! 'print-region cmd-print-region)
  ;; Char info
  (register-command! 'describe-char-at-point cmd-describe-char-at-point)
  ;; Debug
  (register-command! 'toggle-debug-on-signal cmd-toggle-debug-on-signal)
  (register-command! 'toggle-word-boundary cmd-toggle-word-boundary)
  ;; Indent
  (register-command! 'indent-tabs-mode cmd-indent-tabs-mode)
  (register-command! 'electric-indent-local-mode cmd-electric-indent-local-mode)
  ;; Display
  (register-command! 'visual-fill-column-mode cmd-visual-fill-column-mode)
  (register-command! 'adaptive-wrap-prefix-mode cmd-adaptive-wrap-prefix-mode)
  (register-command! 'display-fill-column cmd-display-fill-column)
  (register-command! 'set-selective-display cmd-set-selective-display)
  (register-command! 'toggle-indicate-empty-lines cmd-toggle-indicate-empty-lines)
  (register-command! 'toggle-indicate-buffer-boundaries cmd-toggle-indicate-buffer-boundaries)
  ;; Face
  (register-command! 'facemenu-set-foreground cmd-facemenu-set-foreground)
  (register-command! 'facemenu-set-background cmd-facemenu-set-background)
  ;; Games
  (register-command! 'tetris cmd-tetris)
  (register-command! 'snake cmd-snake)
  (register-command! 'dunnet cmd-dunnet)
  (register-command! 'hanoi cmd-hanoi)
  (register-command! 'life cmd-life)
  (register-command! 'doctor cmd-doctor)
  ;; Proced extras
  (register-command! 'proced-send-signal cmd-proced-send-signal)
  (register-command! 'proced-filter cmd-proced-filter)
  ;; Ediff extras
  (register-command! 'ediff-show-registry cmd-ediff-show-registry)
  ;; Task #49: elisp, scheme, regex builder, color picker, etc.
  ;; Emacs Lisp
  (register-command! 'emacs-lisp-mode cmd-emacs-lisp-mode)
  (register-command! 'eval-last-sexp cmd-eval-last-sexp)
  (register-command! 'eval-defun cmd-eval-defun)
  (register-command! 'eval-print-last-sexp cmd-eval-print-last-sexp)
  ;; Scheme/Gerbil
  (register-command! 'scheme-mode cmd-scheme-mode)
  (register-command! 'gerbil-mode cmd-gerbil-mode)
  (register-command! 'run-scheme cmd-run-scheme)
  (register-command! 'scheme-send-region cmd-scheme-send-region)
  (register-command! 'scheme-send-buffer cmd-scheme-send-buffer)
  ;; Regex builder
  (register-command! 're-builder cmd-re-builder)
  ;; Colors
  (register-command! 'list-colors-display cmd-list-colors-display)
  ;; IDO
  (register-command! 'ido-mode cmd-ido-mode)
  (register-command! 'ido-find-file cmd-ido-find-file)
  (register-command! 'ido-switch-buffer cmd-ido-switch-buffer)
  ;; Helm/Ivy/Vertico
  (register-command! 'helm-mode cmd-helm-mode)
  (register-command! 'ivy-mode cmd-ivy-mode)
  (register-command! 'vertico-mode cmd-vertico-mode)
  (register-command! 'consult-line cmd-consult-line)
  (register-command! 'consult-grep cmd-consult-grep)
  (register-command! 'consult-buffer cmd-consult-buffer)
  ;; Company
  (register-command! 'company-mode cmd-company-mode)
  (register-command! 'company-complete cmd-company-complete)
  ;; Flyspell extras
  (register-command! 'flyspell-buffer cmd-flyspell-buffer)
  (register-command! 'flyspell-correct-word cmd-flyspell-correct-word)
  ;; Bibliography
  (register-command! 'citar-insert-citation cmd-citar-insert-citation)
  ;; Docker
  (register-command! 'docker cmd-docker)
  (register-command! 'docker-containers cmd-docker-containers)
  (register-command! 'docker-images cmd-docker-images)
  ;; Restclient
  (register-command! 'restclient-mode cmd-restclient-mode)
  (register-command! 'restclient-http-send cmd-restclient-http-send)
  ;; Config file modes
  (register-command! 'yaml-mode cmd-yaml-mode)
  (register-command! 'toml-mode cmd-toml-mode)
  (register-command! 'dockerfile-mode cmd-dockerfile-mode)
  ;; SQL
  (register-command! 'sql-mode cmd-sql-mode)
  (register-command! 'sql-connect cmd-sql-connect)
  (register-command! 'sql-send-region cmd-sql-send-region)
  ;; Language modes
  (register-command! 'python-mode cmd-python-mode)
  (register-command! 'c-mode cmd-c-mode)
  (register-command! 'c++-mode cmd-c++-mode)
  (register-command! 'java-mode cmd-java-mode)
  (register-command! 'rust-mode cmd-rust-mode)
  (register-command! 'go-mode cmd-go-mode)
  (register-command! 'js-mode cmd-js-mode)
  (register-command! 'typescript-mode cmd-typescript-mode)
  (register-command! 'html-mode cmd-html-mode)
  (register-command! 'css-mode cmd-css-mode)
  (register-command! 'lua-mode cmd-lua-mode)
  (register-command! 'ruby-mode cmd-ruby-mode)
  (register-command! 'shell-script-mode cmd-shell-script-mode)
  ;; Generic modes
  (register-command! 'prog-mode cmd-prog-mode)
  (register-command! 'text-mode cmd-text-mode)
  (register-command! 'fundamental-mode cmd-fundamental-mode)
  ;; Completion
  (register-command! 'completion-at-point cmd-completion-at-point)
  ;; Eldoc
  (register-command! 'eldoc-mode cmd-eldoc-mode)
  ;; Which-function
  (register-command! 'which-function-mode cmd-which-function-mode)
  ;; Compilation
  (register-command! 'compilation-mode cmd-compilation-mode)
  ;; GDB/GUD
  (register-command! 'gdb cmd-gdb)
  (register-command! 'gud-break cmd-gud-break)
  (register-command! 'gud-remove cmd-gud-remove)
  (register-command! 'gud-cont cmd-gud-cont)
  (register-command! 'gud-next cmd-gud-next)
  (register-command! 'gud-step cmd-gud-step)
  ;; Hippie expand
  (register-command! 'try-expand-dabbrev cmd-try-expand-dabbrev)
  ;; Mode line
  (register-command! 'toggle-mode-line cmd-toggle-mode-line)
  (register-command! 'mode-line-other-buffer cmd-mode-line-other-buffer)
  ;; Timer
  (register-command! 'run-with-timer cmd-run-with-timer)
  ;; Global modes
  (register-command! 'global-auto-revert-mode cmd-global-auto-revert-mode)
  (register-command! 'save-place-mode cmd-save-place-mode)
  (register-command! 'winner-mode cmd-winner-mode)
  (register-command! 'global-whitespace-mode cmd-global-whitespace-mode)
  ;; Cursor
  (register-command! 'blink-cursor-mode cmd-blink-cursor-mode)
  ;; Task #50: push to 1000+
  ;; Lisp interaction
  (register-command! 'lisp-interaction-mode cmd-lisp-interaction-mode)
  (register-command! 'inferior-lisp cmd-inferior-lisp)
  (register-command! 'slime cmd-slime)
  (register-command! 'sly cmd-sly)
  ;; Folding extras
  (register-command! 'fold-this cmd-fold-this)
  (register-command! 'fold-this-all cmd-fold-this-all)
  (register-command! 'origami-mode cmd-origami-mode)
  ;; Indent guides
  (register-command! 'indent-guide-mode cmd-indent-guide-mode)
  (register-command! 'highlight-indent-guides-mode cmd-highlight-indent-guides-mode)
  ;; Rainbow
  (register-command! 'rainbow-delimiters-mode cmd-rainbow-delimiters-mode)
  (register-command! 'rainbow-mode cmd-rainbow-mode)
  ;; Git gutter
  (register-command! 'git-gutter-mode cmd-git-gutter-mode)
  (register-command! 'git-gutter-next-hunk cmd-git-gutter-next-hunk)
  (register-command! 'git-gutter-previous-hunk cmd-git-gutter-previous-hunk)
  (register-command! 'git-gutter-revert-hunk cmd-git-gutter-revert-hunk)
  (register-command! 'git-gutter-stage-hunk cmd-git-gutter-stage-hunk)
  ;; Minimap
  (register-command! 'minimap-mode cmd-minimap-mode)
  ;; Zen modes
  (register-command! 'writeroom-mode cmd-writeroom-mode)
  (register-command! 'focus-mode cmd-focus-mode)
  (register-command! 'olivetti-mode cmd-olivetti-mode)
  ;; Golden ratio
  (register-command! 'golden-ratio-mode cmd-golden-ratio-mode)
  ;; Rotate
  (register-command! 'rotate-window cmd-rotate-window)
  (register-command! 'rotate-frame cmd-rotate-frame)
  ;; Modern completion
  (register-command! 'corfu-mode cmd-corfu-mode)
  (register-command! 'orderless-mode cmd-orderless-mode)
  (register-command! 'marginalia-mode cmd-marginalia-mode)
  (register-command! 'embark-act cmd-embark-act)
  (register-command! 'embark-dwim cmd-embark-dwim)
  (register-command! 'cape-dabbrev cmd-cape-dabbrev)
  (register-command! 'cape-file cmd-cape-file)
  ;; Doom
  (register-command! 'doom-themes cmd-doom-themes)
  (register-command! 'doom-modeline-mode cmd-doom-modeline-mode)
  ;; Which-key
  (register-command! 'which-key-mode cmd-which-key-mode)
  ;; Helpful
  (register-command! 'helpful-callable cmd-helpful-callable)
  (register-command! 'helpful-variable cmd-helpful-variable)
  (register-command! 'helpful-key cmd-helpful-key)
  ;; Diff-hl
  (register-command! 'diff-hl-mode cmd-diff-hl-mode)
  ;; Wgrep
  (register-command! 'wgrep-change-to-wgrep-mode cmd-wgrep-change-to-wgrep-mode)
  (register-command! 'wgrep-finish-edit cmd-wgrep-finish-edit)
  ;; Symbol overlay
  (register-command! 'symbol-overlay-put cmd-symbol-overlay-put)
  (register-command! 'symbol-overlay-remove-all cmd-symbol-overlay-remove-all)
  ;; Perspective
  (register-command! 'persp-switch cmd-persp-switch)
  (register-command! 'persp-add-buffer cmd-persp-add-buffer)
  (register-command! 'persp-remove-buffer cmd-persp-remove-buffer)
  ;; Popper
  (register-command! 'popper-toggle-latest cmd-popper-toggle-latest)
  (register-command! 'popper-cycle cmd-popper-cycle)
  ;; Icons
  (register-command! 'all-the-icons-install-fonts cmd-all-the-icons-install-fonts)
  (register-command! 'nerd-icons-install-fonts cmd-nerd-icons-install-fonts)
  ;; Page break lines
  (register-command! 'page-break-lines-mode cmd-page-break-lines-mode)
  ;; Undo-fu
  (register-command! 'undo-fu-only-undo cmd-undo-fu-only-undo)
  (register-command! 'undo-fu-only-redo cmd-undo-fu-only-redo)
  ;; Vundo
  (register-command! 'vundo cmd-vundo)
  ;; Dash/Devdocs
  (register-command! 'dash-at-point cmd-dash-at-point)
  (register-command! 'devdocs-lookup cmd-devdocs-lookup)
  ;; AI
  (register-command! 'copilot-mode cmd-copilot-mode)
  (register-command! 'copilot-accept-completion cmd-copilot-accept-completion)
  (register-command! 'copilot-next-completion cmd-copilot-next-completion)
  (register-command! 'gptel cmd-gptel)
  (register-command! 'gptel-send cmd-gptel-send)
  ;; Modal editing
  (register-command! 'evil-mode cmd-evil-mode)
  (register-command! 'meow-mode cmd-meow-mode)
  ;; Terminals
  (register-command! 'eat cmd-eat)
  (register-command! 'vterm cmd-vterm)
  ;; Notes
  (register-command! 'denote cmd-denote)
  (register-command! 'denote-link cmd-denote-link)
  (register-command! 'org-roam-node-find cmd-org-roam-node-find)
  (register-command! 'org-roam-node-insert cmd-org-roam-node-insert)
  (register-command! 'org-roam-buffer-toggle cmd-org-roam-buffer-toggle)
  ;; Dirvish
  (register-command! 'dirvish cmd-dirvish)
  ;; Jinx
  (register-command! 'jinx-mode cmd-jinx-mode)
  (register-command! 'jinx-correct cmd-jinx-correct)
  ;; HL-todo
  (register-command! 'hl-todo-mode cmd-hl-todo-mode)
  (register-command! 'hl-todo-next cmd-hl-todo-next)
  (register-command! 'hl-todo-previous cmd-hl-todo-previous)
  ;; Editorconfig
  (register-command! 'editorconfig-mode cmd-editorconfig-mode)
  ;; Envrc
  (register-command! 'envrc-mode cmd-envrc-mode)
  ;; Apheleia
  (register-command! 'apheleia-mode cmd-apheleia-mode)
  (register-command! 'apheleia-format-buffer cmd-apheleia-format-buffer)
  ;; Magit extras
  (register-command! 'magit-stash cmd-magit-stash)
  (register-command! 'magit-blame cmd-magit-blame)
  (register-command! 'magit-fetch cmd-magit-fetch)
  (register-command! 'magit-pull cmd-magit-pull)
  (register-command! 'magit-push cmd-magit-push)
  (register-command! 'magit-rebase cmd-magit-rebase)
  (register-command! 'magit-merge cmd-magit-merge)
  (register-command! 'git-log-file cmd-git-log-file)
  ;; Task #51: Additional commands to cross 1000
  (register-command! 'native-compile-file cmd-native-compile-file)
  (register-command! 'native-compile-async cmd-native-compile-async)
  (register-command! 'tab-line-mode cmd-tab-line-mode)
  (register-command! 'pixel-scroll-precision-mode cmd-pixel-scroll-precision-mode)
  (register-command! 'so-long-mode cmd-so-long-mode)
  (register-command! 'repeat-mode cmd-repeat-mode)
  (register-command! 'context-menu-mode cmd-context-menu-mode)
  (register-command! 'savehist-mode cmd-savehist-mode)
  (register-command! 'recentf-mode cmd-recentf-mode)
  (register-command! 'winner-undo-2 cmd-winner-undo-2)
  (register-command! 'global-subword-mode cmd-global-subword-mode)
  (register-command! 'display-fill-column-indicator-mode cmd-display-fill-column-indicator-mode)
  (register-command! 'global-display-line-numbers-mode cmd-global-display-line-numbers-mode)
  (register-command! 'indent-bars-mode cmd-indent-bars-mode)
  (register-command! 'global-hl-line-mode cmd-global-hl-line-mode)
  (register-command! 'delete-selection-mode cmd-delete-selection-mode)
  (register-command! 'electric-indent-mode cmd-electric-indent-mode)
  (register-command! 'show-paren-mode cmd-show-paren-mode)
  (register-command! 'column-number-mode cmd-column-number-mode)
  (register-command! 'size-indication-mode cmd-size-indication-mode)
  (register-command! 'minibuffer-depth-indicate-mode cmd-minibuffer-depth-indicate-mode)
  (register-command! 'file-name-shadow-mode cmd-file-name-shadow-mode)
  (register-command! 'midnight-mode cmd-midnight-mode)
  (register-command! 'cursor-intangible-mode cmd-cursor-intangible-mode)
  (register-command! 'auto-compression-mode cmd-auto-compression-mode)
  ;; Window resize
  (register-command! 'enlarge-window cmd-enlarge-window)
  (register-command! 'shrink-window cmd-shrink-window)
  (register-command! 'enlarge-window-horizontally cmd-enlarge-window-horizontally)
  (register-command! 'shrink-window-horizontally cmd-shrink-window-horizontally)
  ;; Regex search
  (register-command! 'isearch-forward-regexp cmd-search-forward-regexp)
  (register-command! 'query-replace-regexp-interactive cmd-query-replace-regexp-interactive)
  ;; Real multi-cursor (Scintilla multi-selection)
  (register-command! 'mc-add-next cmd-mc-real-add-next)
  (register-command! 'mc-add-all cmd-mc-real-add-all)
  (register-command! 'mc-skip-and-add-next cmd-mc-skip-and-add-next)
  (register-command! 'mc-cursors-on-lines cmd-mc-cursors-on-lines)
  (register-command! 'mc-unmark-last cmd-mc-unmark-last)
  (register-command! 'mc-rotate cmd-mc-rotate)
  ;; Paredit advanced
  (register-command! 'paredit-slurp-forward cmd-paredit-slurp-forward)
  (register-command! 'paredit-barf-forward cmd-paredit-barf-forward)
  (register-command! 'paredit-split-sexp cmd-paredit-split-sexp)
  (register-command! 'paredit-join-sexps cmd-paredit-join-sexps)
  ;; Number increment/decrement
  (register-command! 'increment-number cmd-increment-number)
  (register-command! 'decrement-number cmd-decrement-number)
  ;; Grep/compilation navigation
  (register-command! 'grep-goto cmd-grep-goto)
  (register-command! 'next-error cmd-next-error)
  ;; Occur goto
  (register-command! 'occur-goto cmd-occur-goto)
  ;; Markdown mode
  (register-command! 'markdown-bold cmd-markdown-bold)
  (register-command! 'markdown-italic cmd-markdown-italic)
  (register-command! 'markdown-code cmd-markdown-code)
  (register-command! 'markdown-code-block cmd-markdown-code-block)
  (register-command! 'markdown-heading cmd-markdown-heading)
  (register-command! 'markdown-link cmd-markdown-link)
  (register-command! 'markdown-image cmd-markdown-image)
  (register-command! 'markdown-hr cmd-markdown-hr)
  (register-command! 'markdown-list-item cmd-markdown-list-item)
  (register-command! 'markdown-checkbox cmd-markdown-checkbox)
  (register-command! 'markdown-toggle-checkbox cmd-markdown-toggle-checkbox)
  (register-command! 'markdown-table cmd-markdown-table)
  (register-command! 'markdown-preview-outline cmd-markdown-preview-outline)
  ;; Dired operations
  (register-command! 'dired-mark cmd-dired-mark)
  (register-command! 'dired-unmark cmd-dired-unmark)
  (register-command! 'dired-unmark-all cmd-dired-unmark-all)
  (register-command! 'dired-delete-marked cmd-dired-delete-marked)
  (register-command! 'dired-refresh cmd-dired-refresh)
  ;; Diff two files
  (register-command! 'diff-two-files cmd-diff-two-files)
  ;; Encoding / line endings
  (register-command! 'set-buffer-encoding cmd-set-buffer-encoding)
  (register-command! 'convert-line-endings cmd-convert-line-endings)
  ;; Statistics
  (register-command! 'buffer-statistics cmd-buffer-statistics)
  ;; Fuzzy M-x
  (register-command! 'execute-extended-command-fuzzy cmd-execute-extended-command-fuzzy)
  ;; Scratch with mode
  (register-command! 'scratch-with-mode cmd-scratch-with-mode)
  ;; Buffer navigation
  (register-command! 'switch-to-buffer-other-window cmd-switch-to-buffer-other-window)
  ;; Editorconfig
  (register-command! 'editorconfig-apply cmd-editorconfig-apply)
  ;; Format buffer
  (register-command! 'format-buffer cmd-format-buffer)
  ;; Git blame
  (register-command! 'git-blame-line cmd-git-blame-line)
  ;; M-x with history
  (register-command! 'execute-extended-command-with-history cmd-execute-extended-command-with-history)
  ;; Word completion from buffer
  (register-command! 'complete-word-from-buffer cmd-complete-word-from-buffer)
  ;; URL handling
  (register-command! 'open-url-at-point cmd-open-url-at-point)
  ;; MRU buffer switching
  (register-command! 'switch-buffer-mru cmd-switch-buffer-mru)
  ;; Shell on region with replace
  (register-command! 'shell-command-on-region-replace cmd-shell-command-on-region-replace)
  ;; Named macros
  (register-command! 'execute-named-macro cmd-execute-named-macro)
  ;; Apply macro to region lines
  (register-command! 'apply-macro-to-region cmd-apply-macro-to-region)
  ;; Diff summary
  (register-command! 'diff-summary cmd-diff-summary)
  ;; Revert without confirm
  (register-command! 'revert-buffer-no-confirm cmd-revert-buffer-no-confirm)
  ;; Sudo save
  (register-command! 'sudo-save-buffer cmd-sudo-save-buffer)
  ;; URL encode/decode
  (register-command! 'url-encode-region cmd-url-encode-region)
  (register-command! 'url-decode-region cmd-url-decode-region)
  ;; JSON
  (register-command! 'json-format-buffer cmd-json-format-buffer)
  (register-command! 'json-minify-buffer cmd-json-minify-buffer)
  (register-command! 'json-sort-keys cmd-json-sort-keys)
  (register-command! 'jq-filter cmd-jq-filter)
  ;; HTML entities
  (register-command! 'html-encode-region cmd-html-encode-region)
  (register-command! 'html-decode-region cmd-html-decode-region)
  ;; File warnings
  (register-command! 'find-file-with-warnings cmd-find-file-with-warnings)
  ;; Encoding
  (register-command! 'detect-encoding cmd-detect-encoding)
  ;; CSV
  (register-command! 'csv-align-columns cmd-csv-align-columns)
  ;; Epoch conversion
  (register-command! 'epoch-to-date cmd-epoch-to-date)
)
