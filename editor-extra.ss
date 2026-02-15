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
  ;; Batch 25 - line manipulation, calc, tables, etc.
  (register-command! 'reverse-lines cmd-reverse-lines)
  (register-command! 'shuffle-lines cmd-shuffle-lines)
  (register-command! 'calc-eval-region cmd-calc-eval-region)
  (register-command! 'table-insert cmd-table-insert)
  (register-command! 'list-timers cmd-list-timers)
  (register-command! 'toggle-aggressive-indent cmd-toggle-aggressive-indent)
  (register-command! 'smart-open-line-above cmd-smart-open-line-above)
  (register-command! 'smart-open-line-below cmd-smart-open-line-below)
  (register-command! 'quick-run cmd-quick-run)
  (register-command! 'toggle-ws-butler-mode cmd-toggle-ws-butler-mode)
  (register-command! 'copy-as-formatted cmd-copy-as-formatted)
  (register-command! 'wrap-region-with cmd-wrap-region-with)
  (register-command! 'unwrap-region cmd-unwrap-region)
  (register-command! 'toggle-quotes cmd-toggle-quotes)
  (register-command! 'word-frequency-analysis cmd-word-frequency-analysis)
  (register-command! 'selection-info cmd-selection-info)
  (register-command! 'increment-hex-at-point cmd-increment-hex-at-point)
  (register-command! 'describe-char cmd-describe-char)
  (register-command! 'narrow-to-region-simple cmd-narrow-to-region-simple)
  (register-command! 'widen-simple cmd-widen-simple)
  (register-command! 'toggle-buffer-read-only cmd-toggle-buffer-read-only)
  ;; Batch 26 - comment-box, format-region, rename, etc.
  (register-command! 'comment-box cmd-comment-box)
  (register-command! 'format-region cmd-format-region)
  (register-command! 'rename-symbol cmd-rename-symbol)
  (register-command! 'isearch-occur cmd-isearch-occur)
  (register-command! 'helm-mini cmd-helm-mini)
  (register-command! 'toggle-comment-style cmd-toggle-comment-style)
  (register-command! 'toggle-flymake-mode cmd-toggle-flymake-mode)
  (register-command! 'indent-for-tab cmd-indent-for-tab)
  (register-command! 'dedent-region cmd-dedent-region)
  (register-command! 'duplicate-and-comment cmd-duplicate-and-comment)
  (register-command! 'insert-scratch-message cmd-insert-scratch-message)
  (register-command! 'count-lines-region cmd-count-lines-region)
  (register-command! 'cycle-spacing cmd-cycle-spacing)
  ;; Batch 27 - focus mode, zen mode, killed buffers, etc.
  (register-command! 'toggle-focus-mode cmd-toggle-focus-mode)
  (register-command! 'toggle-zen-mode cmd-toggle-zen-mode)
  (register-command! 'reopen-killed-buffer cmd-reopen-killed-buffer)
  (register-command! 'copy-file-name-only cmd-copy-file-name-only)
  (register-command! 'open-containing-folder cmd-open-containing-folder)
  (register-command! 'new-empty-buffer cmd-new-empty-buffer)
  (register-command! 'toggle-window-dedicated cmd-toggle-window-dedicated)
  (register-command! 'toggle-which-key-mode cmd-toggle-which-key-mode)
  (register-command! 'which-key-describe-prefix cmd-which-key-describe-prefix)
  (register-command! 'transpose-windows cmd-transpose-windows)
  (register-command! 'fold-toggle-at-point cmd-fold-toggle-at-point)
  (register-command! 'imenu-list cmd-imenu-list)
  ;; Batch 28 - regex builder, web search, conversions, etc.
  (register-command! 'regex-builder cmd-regex-builder)
  (register-command! 'eww-search-web cmd-eww-search-web)
  (register-command! 'goto-last-edit cmd-goto-last-edit)
  (register-command! 'eval-region-and-replace cmd-eval-region-and-replace)
  (register-command! 'hex-to-decimal cmd-hex-to-decimal)
  (register-command! 'decimal-to-hex cmd-decimal-to-hex)
  (register-command! 'encode-hex-string cmd-encode-hex-string)
  (register-command! 'decode-hex-string cmd-decode-hex-string)
  (register-command! 'copy-buffer-file-name cmd-copy-buffer-file-name)
  (register-command! 'insert-date-formatted cmd-insert-date-formatted)
  (register-command! 'prepend-to-buffer cmd-prepend-to-buffer)
  (register-command! 'save-persistent-scratch cmd-save-persistent-scratch)
  (register-command! 'load-persistent-scratch cmd-load-persistent-scratch)
  ;; Batch 29 - memory, password, tabs, shell output, modes, etc.
  (register-command! 'memory-usage cmd-memory-usage)
  (register-command! 'generate-password cmd-generate-password)
  (register-command! 'insert-sequential-numbers cmd-insert-sequential-numbers)
  (register-command! 'insert-env-var cmd-insert-env-var)
  (register-command! 'untabify-region cmd-untabify-region)
  (register-command! 'tabify-region cmd-tabify-region)
  (register-command! 'shell-command-to-string cmd-shell-command-to-string)
  (register-command! 'toggle-highlight-changes cmd-toggle-highlight-changes)
  (register-command! 'window-save-layout cmd-window-save-layout)
  (register-command! 'window-restore-layout cmd-window-restore-layout)
  (register-command! 'set-buffer-mode cmd-set-buffer-mode)
  (register-command! 'canonically-space-region cmd-canonically-space-region)
  (register-command! 'list-packages cmd-list-packages)
  ;; Batch 30 - TODO/FIXME, cursor, modeline, indent guides, etc.
  (register-command! 'insert-todo cmd-insert-todo)
  (register-command! 'insert-fixme cmd-insert-fixme)
  (register-command! 'toggle-cursor-type cmd-toggle-cursor-type)
  (register-command! 'toggle-modeline cmd-toggle-modeline)
  (register-command! 'toggle-indent-guide cmd-toggle-indent-guide)
  (register-command! 'toggle-rainbow-mode cmd-toggle-rainbow-mode)
  (register-command! 'goto-scratch cmd-goto-scratch)
  (register-command! 'display-prefix-help cmd-display-prefix-help)
  (register-command! 'toggle-electric-quote cmd-toggle-electric-quote)
  (register-command! 'calculator-inline cmd-calculator-inline)
  (register-command! 'toggle-visible-mark cmd-toggle-visible-mark)
  (register-command! 'open-recent-dir cmd-open-recent-dir)
  (register-command! 'toggle-fringe cmd-toggle-fringe)
  ;; Batch 31 - titlecase, bracket match, block comment, etc.
  (register-command! 'titlecase-region cmd-titlecase-region)
  (register-command! 'goto-matching-bracket cmd-goto-matching-bracket)
  (register-command! 'toggle-block-comment cmd-toggle-block-comment)
  (register-command! 'move-to-window-center cmd-move-to-window-center)
  (register-command! 'reverse-region-chars cmd-reverse-region-chars)
  (register-command! 'toggle-relative-line-numbers cmd-toggle-relative-line-numbers)
  (register-command! 'toggle-cua-mode cmd-toggle-cua-mode)
  (register-command! 'exchange-dot-and-mark cmd-exchange-dot-and-mark)
  (register-command! 'sort-paragraphs cmd-sort-paragraphs)
  (register-command! 'insert-mode-line cmd-insert-mode-line)
  (register-command! 'push-mark-command cmd-push-mark-command)
  ;; Batch 32 - delete selection, word count, column ruler, etc.
  (register-command! 'toggle-delete-selection cmd-toggle-delete-selection)
  (register-command! 'toggle-word-count cmd-toggle-word-count)
  (register-command! 'toggle-column-ruler cmd-toggle-column-ruler)
  (register-command! 'shell-here cmd-shell-here)
  (register-command! 'toggle-soft-wrap cmd-toggle-soft-wrap)
  (register-command! 'toggle-whitespace-cleanup-on-save cmd-toggle-whitespace-cleanup-on-save)
  (register-command! 'insert-random-line cmd-insert-random-line)
  (register-command! 'smart-backspace cmd-smart-backspace)
  (register-command! 'toggle-line-move-visual cmd-toggle-line-move-visual)
  ;; Batch 33 - char-by-code, subword, bidi, fill-column indicator, etc.
  (register-command! 'insert-char-by-code cmd-insert-char-by-code)
  (register-command! 'toggle-subword-mode cmd-toggle-subword-mode)
  (register-command! 'toggle-auto-composition cmd-toggle-auto-composition)
  (register-command! 'toggle-bidi-display cmd-toggle-bidi-display)
  (register-command! 'toggle-display-fill-column-indicator cmd-toggle-display-fill-column-indicator)
  (register-command! 'insert-current-file-name cmd-insert-current-file-name)
  (register-command! 'toggle-pixel-scroll-2 cmd-toggle-pixel-scroll)
  (register-command! 'insert-lorem-ipsum cmd-insert-lorem-ipsum)
  (register-command! 'toggle-auto-highlight-symbol cmd-toggle-auto-highlight-symbol)
  (register-command! 'copy-rectangle-to-clipboard cmd-copy-rectangle-to-clipboard)
  (register-command! 'insert-file-contents cmd-insert-file-contents-at-point)
  ;; Batch 34 - cursor blink, other-window scroll, separator, etc.
  (register-command! 'toggle-cursor-blink cmd-toggle-cursor-blink)
  (register-command! 'recenter-other-window cmd-recenter-other-window)
  (register-command! 'scroll-up-other-window cmd-scroll-up-other-window)
  (register-command! 'scroll-down-other-window cmd-scroll-down-other-window)
  (register-command! 'toggle-header-line cmd-toggle-header-line)
  (register-command! 'toggle-auto-save-visited cmd-toggle-auto-save-visited)
  (register-command! 'goto-random-line cmd-goto-random-line)
  (register-command! 'reverse-words-in-region cmd-reverse-words-in-region)
  (register-command! 'insert-separator-line cmd-insert-separator-line)
  (register-command! 'toggle-hl-todo cmd-toggle-hl-todo)
  (register-command! 'sort-words-in-line cmd-sort-words-in-line)
  ;; Batch 35 - auto-complete, which-function, line numbers, etc.
  (register-command! 'toggle-global-auto-complete cmd-toggle-global-auto-complete)
  (register-command! 'toggle-which-function cmd-toggle-which-function)
  (register-command! 'toggle-display-line-numbers cmd-toggle-display-line-numbers)
  (register-command! 'toggle-selective-display cmd-toggle-selective-display)
  (register-command! 'toggle-global-font-lock cmd-toggle-global-font-lock)
  (register-command! 'insert-register-content cmd-insert-register-content)
  (register-command! 'insert-date-iso cmd-insert-date-iso)
  (register-command! 'toggle-word-wrap-column cmd-toggle-word-wrap-column)
  (register-command! 'clone-indirect-buffer cmd-clone-indirect-buffer)
  (register-command! 'toggle-auto-dim-other-buffers cmd-toggle-auto-dim-other-buffers)
  (register-command! 'toggle-global-eldoc cmd-toggle-global-eldoc)
  (register-command! 'open-line-below cmd-open-line-below)
  ;; Batch 36 - show-paren style, UUID, visual line, scroll, etc.
  (register-command! 'toggle-show-paren-style cmd-toggle-show-paren-style)
  (register-command! 'insert-uuid-v4 cmd-insert-uuid-v4)
  (register-command! 'toggle-auto-insert-mode cmd-toggle-auto-insert-mode)
  (register-command! 'toggle-global-visual-line cmd-toggle-global-visual-line)
  (register-command! 'toggle-scroll-conservatively cmd-toggle-scroll-conservatively)
  (register-command! 'toggle-show-keystroke cmd-toggle-show-keystroke)
  (register-command! 'toggle-auto-revert-tail cmd-toggle-auto-revert-tail)
  (register-command! 'toggle-flyspell-prog cmd-toggle-flyspell-prog)
  (register-command! 'toggle-auto-save-buffers cmd-toggle-auto-save-buffers)
  (register-command! 'insert-backslash cmd-insert-backslash)
  (register-command! 'toggle-global-linum cmd-toggle-global-linum)
  ;; Batch 37 - highlight-indentation, hungry-delete, type-break, etc.
  (register-command! 'toggle-highlight-indentation cmd-toggle-highlight-indentation)
  (register-command! 'toggle-hungry-delete cmd-toggle-hungry-delete)
  (register-command! 'toggle-type-break cmd-toggle-type-break)
  (register-command! 'insert-zero-width-space cmd-insert-zero-width-space)
  (register-command! 'toggle-delete-trailing-on-save cmd-toggle-delete-trailing-on-save)
  (register-command! 'toggle-cursor-in-non-selected-windows cmd-toggle-cursor-in-non-selected-windows)
  (register-command! 'toggle-blink-matching-paren cmd-toggle-blink-matching-paren)
  (register-command! 'toggle-next-error-follow cmd-toggle-next-error-follow)
  (register-command! 'insert-page-break cmd-insert-page-break)
  ;; Batch 38 - auto-compression, image mode, backups, encryption, etc.
  (register-command! 'toggle-auto-compression cmd-toggle-auto-compression)
  (register-command! 'toggle-image-mode cmd-toggle-image-mode)
  (register-command! 'toggle-save-silently cmd-toggle-save-silently)
  (register-command! 'toggle-confirm-kill-emacs cmd-toggle-confirm-kill-emacs)
  (register-command! 'toggle-auto-window-vscroll cmd-toggle-auto-window-vscroll)
  (register-command! 'toggle-fast-but-imprecise-scrolling cmd-toggle-fast-but-imprecise-scrolling)
  (register-command! 'toggle-mouse-avoidance cmd-toggle-mouse-avoidance)
  (register-command! 'toggle-make-backup-files cmd-toggle-make-backup-files)
  (register-command! 'toggle-lock-file-create cmd-toggle-lock-file-create)
  (register-command! 'toggle-auto-encryption cmd-toggle-auto-encryption)
  ;; Batch 39 - read-only dirs, uniquify, so-long, tooltips, etc.
  (register-command! 'toggle-read-only-directories cmd-toggle-read-only-directories)
  (register-command! 'toggle-auto-revert-verbose cmd-toggle-auto-revert-verbose)
  (register-command! 'toggle-uniquify-buffer-names cmd-toggle-uniquify-buffer-names)
  (register-command! 'toggle-global-so-long cmd-toggle-global-so-long)
  (register-command! 'toggle-minibuffer-depth-indicate cmd-toggle-minibuffer-depth-indicate)
  (register-command! 'toggle-context-menu-mode cmd-toggle-context-menu-mode)
  (register-command! 'toggle-tooltip-mode cmd-toggle-tooltip-mode)
  (register-command! 'toggle-file-name-shadow cmd-toggle-file-name-shadow)
  (register-command! 'toggle-minibuffer-electric-default cmd-toggle-minibuffer-electric-default)
  (register-command! 'toggle-history-delete-duplicates cmd-toggle-history-delete-duplicates)
  ;; Batch 40 - delete-pair-blink, recursive minibuffers, short answers, etc.
  (register-command! 'toggle-delete-pair-blink cmd-toggle-delete-pair-blink)
  (register-command! 'toggle-show-paren-when-point-inside cmd-toggle-show-paren-when-point-inside)
  (register-command! 'toggle-enable-recursive-minibuffers cmd-toggle-enable-recursive-minibuffers)
  (register-command! 'toggle-use-dialog-box cmd-toggle-use-dialog-box)
  (register-command! 'toggle-use-short-answers cmd-toggle-use-short-answers)
  (register-command! 'toggle-ring-bell-function cmd-toggle-ring-bell-function)
  (register-command! 'toggle-sentence-end-double-space cmd-toggle-sentence-end-double-space)
  (register-command! 'toggle-colon-double-space cmd-toggle-colon-double-space)
  (register-command! 'toggle-comment-auto-fill cmd-toggle-comment-auto-fill)
  ;; batch 41
  (register-command! 'toggle-company-mode cmd-toggle-company-mode)
  (register-command! 'toggle-ivy-mode cmd-toggle-ivy-mode)
  (register-command! 'toggle-helm-mode cmd-toggle-helm-mode)
  (register-command! 'toggle-projectile-mode cmd-toggle-projectile-mode)
  (register-command! 'toggle-evil-mode cmd-toggle-evil-mode)
  (register-command! 'toggle-doom-modeline cmd-toggle-doom-modeline)
  (register-command! 'toggle-treesit-mode cmd-toggle-treesit-mode)
  (register-command! 'toggle-eglot-mode cmd-toggle-eglot-mode)
  (register-command! 'toggle-display-time cmd-toggle-display-time)
  (register-command! 'toggle-display-battery cmd-toggle-display-battery)
  ;; batch 42
  (register-command! 'toggle-auto-fill-comments cmd-toggle-auto-fill-comments)
  (register-command! 'toggle-electric-indent-mode cmd-toggle-electric-indent-mode)
  (register-command! 'toggle-truncate-partial-width-windows cmd-toggle-truncate-partial-width-windows)
  (register-command! 'toggle-inhibit-startup-screen cmd-toggle-inhibit-startup-screen)
  (register-command! 'toggle-visible-cursor cmd-toggle-visible-cursor)
  (register-command! 'toggle-transient-mark-mode cmd-toggle-transient-mark-mode)
  (register-command! 'insert-form-feed cmd-insert-form-feed)
  (register-command! 'toggle-global-whitespace-mode cmd-toggle-global-whitespace-mode)
  (register-command! 'toggle-hide-ifdef-mode cmd-toggle-hide-ifdef-mode)
  (register-command! 'toggle-allout-mode cmd-toggle-allout-mode)
  ;; batch 43
  (register-command! 'toggle-desktop-save-mode cmd-toggle-desktop-save-mode)
  (register-command! 'toggle-recentf-mode cmd-toggle-recentf-mode)
  (register-command! 'toggle-savehist-mode cmd-toggle-savehist-mode)
  (register-command! 'toggle-winner-mode cmd-toggle-winner-mode)
  (register-command! 'toggle-midnight-mode cmd-toggle-midnight-mode)
  (register-command! 'toggle-global-undo-tree cmd-toggle-global-undo-tree)
  (register-command! 'toggle-diff-hl-mode cmd-toggle-diff-hl-mode)
  (register-command! 'toggle-volatile-highlights cmd-toggle-volatile-highlights)
  (register-command! 'toggle-vertico-mode cmd-toggle-vertico-mode)
  (register-command! 'toggle-marginalia-mode cmd-toggle-marginalia-mode)
  ;; batch 44
  (register-command! 'toggle-consult-mode cmd-toggle-consult-mode)
  (register-command! 'toggle-orderless-mode cmd-toggle-orderless-mode)
  (register-command! 'toggle-embark-mode cmd-toggle-embark-mode)
  (register-command! 'toggle-undo-fu-session cmd-toggle-undo-fu-session)
  (register-command! 'toggle-auto-package-mode cmd-toggle-auto-package-mode)
  (register-command! 'toggle-corfu-mode cmd-toggle-corfu-mode)
  (register-command! 'toggle-cape-mode cmd-toggle-cape-mode)
  (register-command! 'toggle-nerd-icons-mode cmd-toggle-nerd-icons-mode)
  (register-command! 'toggle-all-the-icons cmd-toggle-all-the-icons)
  (register-command! 'toggle-doom-themes cmd-toggle-doom-themes)
  ;; batch 45
  (register-command! 'toggle-modus-themes cmd-toggle-modus-themes)
  (register-command! 'toggle-ef-themes cmd-toggle-ef-themes)
  (register-command! 'toggle-nano-theme cmd-toggle-nano-theme)
  (register-command! 'toggle-ligature-mode cmd-toggle-ligature-mode)
  (register-command! 'toggle-pixel-scroll-precision cmd-toggle-pixel-scroll-precision)
  (register-command! 'toggle-repeat-mode cmd-toggle-repeat-mode)
  (register-command! 'toggle-tab-line-mode cmd-toggle-tab-line-mode)
  (register-command! 'toggle-scroll-bar-mode cmd-toggle-scroll-bar-mode)
  (register-command! 'toggle-tool-bar-mode cmd-toggle-tool-bar-mode)
  ;; batch 46
  (register-command! 'insert-date-time-stamp cmd-insert-date-time-stamp)
  (register-command! 'toggle-auto-rename-tag cmd-toggle-auto-rename-tag)
  (register-command! 'toggle-global-prettify-symbols cmd-toggle-global-prettify-symbols)
  (register-command! 'toggle-global-subword cmd-toggle-global-subword)
  (register-command! 'toggle-global-superword cmd-toggle-global-superword)
  (register-command! 'toggle-delete-by-moving-to-trash cmd-toggle-delete-by-moving-to-trash)
  (register-command! 'toggle-create-lockfiles cmd-toggle-create-lockfiles)
  (register-command! 'toggle-mode-line-compact cmd-toggle-mode-line-compact)
  (register-command! 'toggle-use-file-dialog cmd-toggle-use-file-dialog)
  (register-command! 'toggle-xterm-mouse-mode cmd-toggle-xterm-mouse-mode)
  ;; batch 47
  (register-command! 'toggle-auto-save-default cmd-toggle-auto-save-default)
  (register-command! 'toggle-make-pointer-invisible cmd-toggle-make-pointer-invisible)
  (register-command! 'toggle-kill-whole-line cmd-toggle-kill-whole-line)
  (register-command! 'toggle-set-mark-command-repeat-pop cmd-toggle-set-mark-command-repeat-pop)
  (register-command! 'toggle-enable-local-variables cmd-toggle-enable-local-variables)
  (register-command! 'toggle-enable-dir-local-variables cmd-toggle-enable-dir-local-variables)
  (register-command! 'toggle-ad-activate-all cmd-toggle-ad-activate-all)
  (register-command! 'toggle-global-hi-lock-mode cmd-toggle-global-hi-lock-mode)
  (register-command! 'toggle-next-line-add-newlines cmd-toggle-next-line-add-newlines)
  ;; batch 48
  (register-command! 'toggle-auto-save-on-idle cmd-toggle-auto-save-on-idle)
  (register-command! 'toggle-delete-active-region cmd-toggle-delete-active-region)
  (register-command! 'toggle-shift-select-mode cmd-toggle-shift-select-mode)
  (register-command! 'toggle-cua-selection-mode cmd-toggle-cua-selection-mode)
  (register-command! 'toggle-global-goto-address cmd-toggle-global-goto-address)
  (register-command! 'toggle-global-reveal-mode cmd-toggle-global-reveal-mode)
  (register-command! 'toggle-global-auto-composition cmd-toggle-global-auto-composition)
  (register-command! 'toggle-global-display-line-numbers cmd-toggle-global-display-line-numbers)
  (register-command! 'toggle-blink-cursor-mode cmd-toggle-blink-cursor-mode)
  ;; batch 49
  (register-command! 'toggle-indent-guide-global cmd-toggle-indent-guide-global)
  (register-command! 'toggle-rainbow-delimiters-global cmd-toggle-rainbow-delimiters-global)
  (register-command! 'toggle-global-display-fill-column cmd-toggle-global-display-fill-column)
  (register-command! 'toggle-global-flycheck cmd-toggle-global-flycheck)
  (register-command! 'toggle-global-company cmd-toggle-global-company)
  (register-command! 'toggle-global-diff-hl cmd-toggle-global-diff-hl)
  (register-command! 'toggle-global-git-gutter cmd-toggle-global-git-gutter)
  (register-command! 'toggle-global-page-break-lines cmd-toggle-global-page-break-lines)
  (register-command! 'toggle-global-anzu cmd-toggle-global-anzu)
  ;; batch 50
  (register-command! 'toggle-global-prettify cmd-toggle-global-prettify)
  (register-command! 'toggle-global-hl-todo cmd-toggle-global-hl-todo)
  (register-command! 'toggle-global-color-identifiers cmd-toggle-global-color-identifiers)
  (register-command! 'toggle-global-aggressive-indent cmd-toggle-global-aggressive-indent)
  (register-command! 'toggle-global-origami cmd-toggle-global-origami)
  (register-command! 'toggle-global-centered-cursor cmd-toggle-global-centered-cursor)
  (register-command! 'toggle-global-beacon cmd-toggle-global-beacon)
  (register-command! 'toggle-global-dimmer cmd-toggle-global-dimmer)
  (register-command! 'toggle-global-focus cmd-toggle-global-focus)
  ;; batch 51
  (register-command! 'toggle-global-auto-revert-non-file cmd-toggle-global-auto-revert-non-file)
  (register-command! 'toggle-global-tree-sitter cmd-toggle-global-tree-sitter)
  (register-command! 'toggle-global-copilot cmd-toggle-global-copilot)
  (register-command! 'toggle-global-lsp-mode cmd-toggle-global-lsp-mode)
  (register-command! 'toggle-global-format-on-save cmd-toggle-global-format-on-save)
  (register-command! 'toggle-global-yas cmd-toggle-global-yas)
  (register-command! 'toggle-global-smartparens cmd-toggle-global-smartparens)
  ;; batch 52
  (register-command! 'toggle-global-cwarn cmd-toggle-global-cwarn)
  (register-command! 'toggle-global-hideshow cmd-toggle-global-hideshow)
  (register-command! 'toggle-global-abbrev cmd-toggle-global-abbrev)
  (register-command! 'toggle-global-diff-auto-refine cmd-toggle-global-diff-auto-refine)
  (register-command! 'toggle-global-eldoc-box cmd-toggle-global-eldoc-box)
  (register-command! 'toggle-global-flyspell-lazy cmd-toggle-global-flyspell-lazy)
  (register-command! 'toggle-global-so-clean cmd-toggle-global-so-clean)
  ;; batch 53
  (register-command! 'toggle-global-whitespace-newline cmd-toggle-global-whitespace-newline)
  (register-command! 'toggle-global-highlight-indent cmd-toggle-global-highlight-indent)
  (register-command! 'toggle-global-rainbow-mode cmd-toggle-global-rainbow-mode)
  (register-command! 'toggle-global-auto-highlight cmd-toggle-global-auto-highlight)
  (register-command! 'toggle-global-symbol-overlay cmd-toggle-global-symbol-overlay)
  (register-command! 'toggle-global-highlight-parentheses cmd-toggle-global-highlight-parentheses)
  (register-command! 'toggle-global-pulse-line cmd-toggle-global-pulse-line)
  ;; batch 54
  (register-command! 'toggle-global-visual-regexp cmd-toggle-global-visual-regexp)
  (register-command! 'toggle-global-move-dup cmd-toggle-global-move-dup)
  (register-command! 'toggle-global-expand-region cmd-toggle-global-expand-region)
  (register-command! 'toggle-global-multiple-cursors cmd-toggle-global-multiple-cursors)
  (register-command! 'toggle-global-undo-propose cmd-toggle-global-undo-propose)
  (register-command! 'toggle-global-goto-chg cmd-toggle-global-goto-chg)
  (register-command! 'toggle-global-avy cmd-toggle-global-avy)
  ;; batch 55
  (register-command! 'toggle-global-wgrep cmd-toggle-global-wgrep)
  (register-command! 'toggle-global-deadgrep cmd-toggle-global-deadgrep)
  (register-command! 'toggle-global-ripgrep cmd-toggle-global-ripgrep)
  (register-command! 'toggle-global-projectile-ripgrep cmd-toggle-global-projectile-ripgrep)
  (register-command! 'toggle-global-counsel cmd-toggle-global-counsel)
  (register-command! 'toggle-global-swiper cmd-toggle-global-swiper)
  (register-command! 'toggle-global-prescient cmd-toggle-global-prescient)
  ;; batch 56
  (register-command! 'toggle-global-which-key cmd-toggle-global-which-key)
  (register-command! 'toggle-global-hydra cmd-toggle-global-hydra)
  (register-command! 'toggle-global-transient cmd-toggle-global-transient)
  (register-command! 'toggle-global-general cmd-toggle-global-general)
  (register-command! 'toggle-global-use-package cmd-toggle-global-use-package)
  (register-command! 'toggle-global-diminish cmd-toggle-global-diminish)
  (register-command! 'toggle-global-delight cmd-toggle-global-delight)
  ;; batch 57
  (register-command! 'toggle-global-envrc cmd-toggle-global-envrc)
  (register-command! 'toggle-global-direnv cmd-toggle-global-direnv)
  (register-command! 'toggle-global-editorconfig cmd-toggle-global-editorconfig)
  (register-command! 'toggle-global-dtrt-indent cmd-toggle-global-dtrt-indent)
  (register-command! 'toggle-global-ws-trim cmd-toggle-global-ws-trim)
  (register-command! 'toggle-global-auto-compile cmd-toggle-global-auto-compile)
  (register-command! 'toggle-global-no-littering cmd-toggle-global-no-littering)
  ;; batch 58
  (register-command! 'toggle-global-golden-ratio cmd-toggle-global-golden-ratio)
  (register-command! 'toggle-global-zoom-window cmd-toggle-global-zoom-window)
  (register-command! 'toggle-global-shackle cmd-toggle-global-shackle)
  (register-command! 'toggle-global-popwin cmd-toggle-global-popwin)
  (register-command! 'toggle-global-popper cmd-toggle-global-popper)
  (register-command! 'toggle-global-posframe cmd-toggle-global-posframe)
  (register-command! 'toggle-global-childframe cmd-toggle-global-childframe)
  ;; batch 59
  (register-command! 'toggle-global-helpful cmd-toggle-global-helpful)
  (register-command! 'toggle-global-elisp-demos cmd-toggle-global-elisp-demos)
  (register-command! 'toggle-global-suggest cmd-toggle-global-suggest)
  (register-command! 'toggle-global-buttercup cmd-toggle-global-buttercup)
  (register-command! 'toggle-global-ert-runner cmd-toggle-global-ert-runner)
  (register-command! 'toggle-global-undercover cmd-toggle-global-undercover)
  (register-command! 'toggle-global-benchmark-init cmd-toggle-global-benchmark-init)
  ;; batch 60
  (register-command! 'toggle-global-native-compile cmd-toggle-global-native-compile)
  (register-command! 'toggle-global-gcmh cmd-toggle-global-gcmh)
  (register-command! 'toggle-global-esup cmd-toggle-global-esup)
  (register-command! 'toggle-global-explain-pause cmd-toggle-global-explain-pause)
  (register-command! 'toggle-global-keyfreq cmd-toggle-global-keyfreq)
  (register-command! 'toggle-global-command-log cmd-toggle-global-command-log)
  (register-command! 'toggle-global-interaction-log cmd-toggle-global-interaction-log)
  ;; batch 61
  (register-command! 'toggle-global-treemacs-icons cmd-toggle-global-treemacs-icons)
  (register-command! 'toggle-global-all-the-icons-dired cmd-toggle-global-all-the-icons-dired)
  (register-command! 'toggle-global-centaur-tabs cmd-toggle-global-centaur-tabs)
  (register-command! 'toggle-global-awesome-tab cmd-toggle-global-awesome-tab)
  (register-command! 'toggle-global-tab-bar cmd-toggle-global-tab-bar)
  (register-command! 'toggle-global-mini-frame cmd-toggle-global-mini-frame)
  (register-command! 'toggle-global-vertico-posframe cmd-toggle-global-vertico-posframe)
  ;; batch 62
  (register-command! 'toggle-global-solaire cmd-toggle-global-solaire)
  (register-command! 'toggle-global-spaceline cmd-toggle-global-spaceline)
  (register-command! 'toggle-global-doom-modeline-env cmd-toggle-global-doom-modeline-env)
  (register-command! 'toggle-global-minions cmd-toggle-global-minions)
  (register-command! 'toggle-global-moody cmd-toggle-global-moody)
  (register-command! 'toggle-global-rich-minority cmd-toggle-global-rich-minority)
  (register-command! 'toggle-global-smart-mode-line cmd-toggle-global-smart-mode-line)
  ;; batch 63
  (register-command! 'toggle-global-nyan-cat cmd-toggle-global-nyan-cat)
  (register-command! 'toggle-global-parrot cmd-toggle-global-parrot)
  (register-command! 'toggle-global-zone cmd-toggle-global-zone)
  (register-command! 'toggle-global-fireplace cmd-toggle-global-fireplace)
  (register-command! 'toggle-global-snow cmd-toggle-global-snow)
  (register-command! 'toggle-global-power-mode cmd-toggle-global-power-mode)
  (register-command! 'toggle-global-animate-typing cmd-toggle-global-animate-typing)
  ;; batch 64
  (register-command! 'toggle-global-org-roam cmd-toggle-global-org-roam)
  (register-command! 'toggle-global-org-journal cmd-toggle-global-org-journal)
  (register-command! 'toggle-global-org-super-agenda cmd-toggle-global-org-super-agenda)
  (register-command! 'toggle-global-org-noter cmd-toggle-global-org-noter)
  (register-command! 'toggle-global-org-download cmd-toggle-global-org-download)
  (register-command! 'toggle-global-org-cliplink cmd-toggle-global-org-cliplink)
  (register-command! 'toggle-global-org-present cmd-toggle-global-org-present)
  ;; batch 65
  (register-command! 'toggle-global-lsp-ui cmd-toggle-global-lsp-ui)
  (register-command! 'toggle-global-lsp-treemacs cmd-toggle-global-lsp-treemacs)
  (register-command! 'toggle-global-lsp-ivy cmd-toggle-global-lsp-ivy)
  (register-command! 'toggle-global-dap-mode cmd-toggle-global-dap-mode)
  (register-command! 'toggle-global-lsp-headerline cmd-toggle-global-lsp-headerline)
  (register-command! 'toggle-global-lsp-lens cmd-toggle-global-lsp-lens)
  (register-command! 'toggle-global-lsp-semantic-tokens cmd-toggle-global-lsp-semantic-tokens)
)
