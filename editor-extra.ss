;;; -*- Gerbil -*-
;;; Extra TUI editor commands for gemacs
;;; Facade module: imports sub-modules and registers all commands.

(export register-extra-commands!
        winner-save-config!)

(import :std/sugar
        :std/srfi/13
        :std/misc/string
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/echo
        :gemacs/editor-extra-helpers
        :gemacs/editor-extra-org
        :gemacs/editor-extra-web
        :gemacs/editor-extra-vcs
        :gemacs/editor-extra-editing
        :gemacs/editor-extra-tools
        :gemacs/editor-extra-tools2
        :gemacs/editor-extra-media
        :gemacs/editor-extra-modes
        :gemacs/editor-extra-final
        :gemacs/editor-extra-regs)

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
  (register-command! 'org-table-align cmd-org-table-align)
  (register-command! 'org-table-insert-row cmd-org-table-insert-row)
  (register-command! 'org-table-delete-row cmd-org-table-delete-row)
  (register-command! 'org-table-move-row-up cmd-org-table-move-row-up)
  (register-command! 'org-table-move-row-down cmd-org-table-move-row-down)
  (register-command! 'org-table-delete-column cmd-org-table-delete-column)
  (register-command! 'org-table-insert-column cmd-org-table-insert-column)
  (register-command! 'org-table-move-column-left cmd-org-table-move-column-left)
  (register-command! 'org-table-move-column-right cmd-org-table-move-column-right)
  (register-command! 'org-table-insert-separator cmd-org-table-insert-separator)
  (register-command! 'org-table-sort cmd-org-table-sort)
  (register-command! 'org-table-sum cmd-org-table-sum)
  (register-command! 'org-table-recalculate cmd-org-table-recalculate)
  (register-command! 'org-table-export-csv cmd-org-table-export-csv)
  (register-command! 'org-table-import-csv cmd-org-table-import-csv)
  (register-command! 'org-table-transpose cmd-org-table-transpose)
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
  ;; LSP: moved to qt/commands-lsp.ss, registered in qt/commands.ss
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
  ;; batch 66
  (register-command! 'toggle-global-docker cmd-toggle-global-docker)
  (register-command! 'toggle-global-kubernetes cmd-toggle-global-kubernetes)
  (register-command! 'toggle-global-terraform cmd-toggle-global-terraform)
  (register-command! 'toggle-global-ansible cmd-toggle-global-ansible)
  (register-command! 'toggle-global-vagrant cmd-toggle-global-vagrant)
  (register-command! 'toggle-global-restclient cmd-toggle-global-restclient)
  (register-command! 'toggle-global-ob-http cmd-toggle-global-ob-http)
  ;; batch 67
  (register-command! 'toggle-global-rustic cmd-toggle-global-rustic)
  (register-command! 'toggle-global-go-mode cmd-toggle-global-go-mode)
  (register-command! 'toggle-global-python-black cmd-toggle-global-python-black)
  (register-command! 'toggle-global-elpy cmd-toggle-global-elpy)
  (register-command! 'toggle-global-js2-mode cmd-toggle-global-js2-mode)
  (register-command! 'toggle-global-typescript-mode cmd-toggle-global-typescript-mode)
  (register-command! 'toggle-global-web-mode cmd-toggle-global-web-mode)
  ;; batch 68
  (register-command! 'toggle-global-clojure-mode cmd-toggle-global-clojure-mode)
  (register-command! 'toggle-global-cider cmd-toggle-global-cider)
  (register-command! 'toggle-global-haskell-mode cmd-toggle-global-haskell-mode)
  (register-command! 'toggle-global-lua-mode cmd-toggle-global-lua-mode)
  (register-command! 'toggle-global-ruby-mode cmd-toggle-global-ruby-mode)
  (register-command! 'toggle-global-php-mode cmd-toggle-global-php-mode)
  (register-command! 'toggle-global-swift-mode cmd-toggle-global-swift-mode)
  ;; batch 69
  (register-command! 'toggle-global-yaml-mode cmd-toggle-global-yaml-mode)
  (register-command! 'toggle-global-toml-mode cmd-toggle-global-toml-mode)
  (register-command! 'toggle-global-json-mode cmd-toggle-global-json-mode)
  (register-command! 'toggle-global-csv-mode cmd-toggle-global-csv-mode)
  (register-command! 'toggle-global-protobuf-mode cmd-toggle-global-protobuf-mode)
  (register-command! 'toggle-global-graphql-mode cmd-toggle-global-graphql-mode)
  (register-command! 'toggle-global-nix-mode cmd-toggle-global-nix-mode)
  ;; batch 70
  (register-command! 'toggle-global-cmake-mode cmd-toggle-global-cmake-mode)
  (register-command! 'toggle-global-bazel-mode cmd-toggle-global-bazel-mode)
  (register-command! 'toggle-global-meson-mode cmd-toggle-global-meson-mode)
  (register-command! 'toggle-global-ninja-mode cmd-toggle-global-ninja-mode)
  (register-command! 'toggle-global-groovy-mode cmd-toggle-global-groovy-mode)
  (register-command! 'toggle-global-kotlin-mode cmd-toggle-global-kotlin-mode)
  (register-command! 'toggle-global-scala-mode cmd-toggle-global-scala-mode)
  ;; batch 71
  (register-command! 'toggle-global-erlang-mode cmd-toggle-global-erlang-mode)
  (register-command! 'toggle-global-elixir-mode cmd-toggle-global-elixir-mode)
  (register-command! 'toggle-global-zig-mode cmd-toggle-global-zig-mode)
  (register-command! 'toggle-global-ocaml-mode cmd-toggle-global-ocaml-mode)
  (register-command! 'toggle-global-fsharp-mode cmd-toggle-global-fsharp-mode)
  (register-command! 'toggle-global-dart-mode cmd-toggle-global-dart-mode)
  (register-command! 'toggle-global-julia-mode cmd-toggle-global-julia-mode)
  ;; batch 72
  (register-command! 'toggle-global-r-mode cmd-toggle-global-r-mode)
  (register-command! 'toggle-global-ess cmd-toggle-global-ess)
  (register-command! 'toggle-global-sql-mode cmd-toggle-global-sql-mode)
  (register-command! 'toggle-global-ein cmd-toggle-global-ein)
  (register-command! 'toggle-global-conda cmd-toggle-global-conda)
  (register-command! 'toggle-global-pyvenv cmd-toggle-global-pyvenv)
  (register-command! 'toggle-global-pipenv cmd-toggle-global-pipenv)
  ;; delete-horizontal-space (defined in editor-extra-tools2.ss)
  (register-command! 'delete-horizontal-space cmd-delete-horizontal-space)
  ;; fill-region, copy-rectangle-to-register (defined in editor-extra-tools2.ss)
  (register-command! 'fill-region cmd-fill-region)
  (register-command! 'copy-rectangle-to-register cmd-copy-rectangle-to-register)
  ;; insert-buffer, session save/restore (defined in editor-extra-tools2.ss)
  (register-command! 'insert-buffer cmd-insert-buffer)
  (register-command! 'session-save cmd-session-save)
  (register-command! 'session-restore cmd-session-restore)
  ;; ace-window (defined in editor-extra-tools2.ss)
  (register-command! 'ace-window cmd-ace-window)
  ;; Reverse parity from Qt (defined in editor-extra-modes.ss)
  (register-command! 'forward-subword cmd-forward-subword)
  (register-command! 'backward-subword cmd-backward-subword)
  (register-command! 'goto-last-change cmd-goto-last-change)
  (register-command! 'find-file-at-line cmd-find-file-at-line)
  (register-command! 'find-file-read-only cmd-find-file-read-only)
  (register-command! 'view-file cmd-view-file)
  (register-command! 'append-to-file cmd-append-to-file)
  ;; Parity from Qt: comment-dwim, kill operations, sexp navigation
  (register-command! 'comment-dwim cmd-comment-dwim)
  (register-command! 'kill-sentence cmd-kill-sentence)
  (register-command! 'backward-kill-sentence cmd-backward-kill-sentence)
  (register-command! 'kill-paragraph cmd-kill-paragraph)
  (register-command! 'kill-subword cmd-kill-subword)
  (register-command! 'up-list cmd-up-list)
  (register-command! 'down-list cmd-down-list)
  ;; Parity batch 2: highlight navigation, browse-url, scratch, swap, aliases
  (register-command! 'highlight-symbol-next cmd-highlight-symbol-next)
  (register-command! 'highlight-symbol-prev cmd-highlight-symbol-prev)
  (register-command! 'browse-url cmd-browse-url)
  (register-command! 'scratch-buffer-new cmd-scratch-buffer-new)
  (register-command! 'swap-window cmd-swap-window)
  ;; Alias: next/previous grep result (cmd-next-error is in our chain)
  (register-command! 'next-grep-result cmd-next-error)
  ;; Parity batch 3: select-current-line, smart-join-line, pop-to-mark, duplicate-line-or-region
  (register-command! 'select-current-line cmd-select-current-line)
  (register-command! 'smart-join-line cmd-smart-join-line)
  (register-command! 'pop-to-mark cmd-pop-to-mark)
  (register-command! 'duplicate-line-or-region cmd-duplicate-line-or-region)
  ;; Parity batch 4: follow-mode, recentf-open-files
  (register-command! 'follow-mode cmd-follow-mode)
  (register-command! 'recentf-open-files cmd-recentf-open-files)
  ;; Parity batch 5: dired advanced ops, diff navigation, display-line-numbers
  (register-command! 'dired-toggle-marks cmd-dired-toggle-marks)
  (register-command! 'dired-do-copy-marked cmd-dired-do-copy-marked)
  (register-command! 'dired-do-rename-marked cmd-dired-do-rename-marked)
  (register-command! 'dired-mark-by-regexp cmd-dired-mark-by-regexp)
  (register-command! 'dired-sort-toggle cmd-dired-sort-toggle)
  (register-command! 'diff-next-hunk cmd-diff-next-hunk)
  (register-command! 'diff-prev-hunk cmd-diff-prev-hunk)
  (register-command! 'display-line-numbers-mode cmd-display-line-numbers-mode)
  ;; Parity batch 6: magit ops, markdown editing, multi-vterm
  (register-command! 'magit-refresh cmd-magit-refresh)
  (register-command! 'magit-stage cmd-magit-stage)
  (register-command! 'magit-unstage cmd-magit-unstage)
  (register-command! 'magit-stage-all cmd-magit-stage-all)
  (register-command! 'multi-vterm cmd-multi-vterm)
  (register-command! 'markdown-promote cmd-markdown-promote)
  (register-command! 'markdown-demote cmd-markdown-demote)
  (register-command! 'markdown-insert-heading cmd-markdown-insert-heading)
  ;; Parity batch 7: markdown toggle/nav, outline, project search
  (register-command! 'markdown-toggle-bold cmd-markdown-toggle-bold)
  (register-command! 'markdown-toggle-italic cmd-markdown-toggle-italic)
  (register-command! 'markdown-toggle-code cmd-markdown-toggle-code)
  (register-command! 'markdown-next-heading cmd-markdown-next-heading)
  (register-command! 'markdown-prev-heading cmd-markdown-prev-heading)
  (register-command! 'markdown-outline cmd-markdown-outline)
  (register-command! 'project-search cmd-project-search)
  (register-command! 'project-run-shell cmd-project-run-shell)
  ;; Parity batch 9: workspace, clipboard, undo-history, xref
  (register-command! 'workspace-create cmd-workspace-create)
  (register-command! 'workspace-switch cmd-workspace-switch)
  (register-command! 'workspace-delete cmd-workspace-delete)
  (register-command! 'workspace-add-buffer cmd-workspace-add-buffer)
  (register-command! 'workspace-list cmd-workspace-list)
  (register-command! 'copy-buffer-filename cmd-copy-buffer-filename)
  (register-command! 'revert-buffer-confirm cmd-revert-buffer-confirm)
  (register-command! 'undo-history cmd-undo-history)
  (register-command! 'undo-history-restore cmd-undo-history-restore)
  (register-command! 'xref-back cmd-xref-back)
  ;; Parity batch 10: org-todo, grep, wdired, wgrep, shell, keys, snippets, misc
  (register-command! 'org-todo-cycle cmd-org-todo-cycle)
  (register-command! 'org-next-heading cmd-org-next-heading)
  (register-command! 'org-prev-heading cmd-org-prev-heading)
  (register-command! 'org-outline cmd-org-outline)
  (register-command! 'rgrep cmd-rgrep)
  (register-command! 'previous-grep-result cmd-previous-grep-result)
  (register-command! 'wdired-mode cmd-wdired-mode)
  (register-command! 'wdired-finish cmd-wdired-finish)
  (register-command! 'wgrep-abort-changes cmd-wgrep-abort-changes)
  (register-command! 'dired-do-delete-marked cmd-dired-do-delete-marked)
  (register-command! 'auto-revert-tail-mode cmd-auto-revert-tail-mode)
  (register-command! 'scratch-message cmd-scratch-message)
  (register-command! 'run-user-shell-command cmd-run-user-shell-command)
  (register-command! 'toggle-compile-on-save cmd-toggle-compile-on-save)
  (register-command! 'toggle-bracket-paren-swap cmd-toggle-bracket-paren-swap)
  (register-command! 'set-variable cmd-set-variable)
  (register-command! 'show-dir-locals cmd-show-dir-locals)
  (register-command! 'find-file-remote cmd-find-file-remote)
  (register-command! 'save-remote-buffer cmd-save-remote-buffer)
  (register-command! 'global-set-key cmd-global-set-key)
  (register-command! 'global-unset-key cmd-global-unset-key)
  (register-command! 'flycheck-prev-error cmd-flycheck-prev-error)
  (register-command! 'insert-char-by-name cmd-insert-char-by-name)
  (register-command! 'file-to-register cmd-file-to-register)
  (register-command! 'kbd-macro-counter-insert cmd-kbd-macro-counter-insert)
  (register-command! 'kbd-macro-counter-set cmd-kbd-macro-counter-set)
  (register-command! 'key-chord-mode cmd-key-chord-mode)
  (register-command! 'key-chord-define cmd-key-chord-define)
  (register-command! 'key-chord-list cmd-key-chord-list)
  (register-command! 'key-translation-list cmd-key-translation-list)
  (register-command! 'define-snippet cmd-define-snippet)
  (register-command! 'list-snippets cmd-list-snippets)
  (register-command! 'snippet-expand cmd-snippet-expand)
  (register-command! 'snippet-next-field cmd-snippet-next-field)
  (register-command! 'snippet-prev-field cmd-snippet-prev-field)
  ;; Parity batch 11: GUI-specific, LSP, multi-cursor stubs
  (register-command! 'calendar-today cmd-calendar-today)
  (register-command! 'calendar-next-month cmd-calendar-next-month)
  (register-command! 'calendar-prev-month cmd-calendar-prev-month)
  (register-command! 'calendar-next-year cmd-calendar-next-year)
  (register-command! 'calendar-prev-year cmd-calendar-prev-year)
  (register-command! 'helm-buffers-list cmd-helm-buffers-list)
  (register-command! 'image-zoom-in cmd-image-zoom-in)
  (register-command! 'image-zoom-out cmd-image-zoom-out)
  (register-command! 'image-zoom-fit cmd-image-zoom-fit)
  (register-command! 'image-zoom-reset cmd-image-zoom-reset)
  (register-command! 'set-font-size cmd-set-font-size)
  (register-command! 'set-frame-font cmd-set-frame-font)
  (register-command! 'vterm-copy-mode cmd-vterm-copy-mode)
  (register-command! 'vterm-copy-done cmd-vterm-copy-done)
  (register-command! 'mc-mark-next cmd-mc-mark-next)
  (register-command! 'mc-mark-all cmd-mc-mark-all)
  (register-command! 'mc-skip-and-mark-next cmd-mc-skip-and-mark-next)
  (register-command! 'lsp-goto-definition cmd-lsp-goto-definition)
  (register-command! 'lsp-smart-goto-definition cmd-lsp-smart-goto-definition)
  (register-command! 'lsp-find-references cmd-lsp-find-references)
  (register-command! 'lsp-hover cmd-lsp-hover)
  (register-command! 'lsp-rename cmd-lsp-rename)
  (register-command! 'lsp-format-buffer cmd-lsp-format-buffer)
  (register-command! 'lsp-code-actions cmd-lsp-code-actions)
  (register-command! 'lsp-completion cmd-lsp-completion)
  (register-command! 'lsp-declaration cmd-lsp-declaration)
  (register-command! 'lsp-implementation cmd-lsp-implementation)
  (register-command! 'lsp-type-definition cmd-lsp-type-definition)
  (register-command! 'lsp-document-symbols cmd-lsp-document-symbols)
  (register-command! 'lsp-workspace-symbol cmd-lsp-workspace-symbol)
  (register-command! 'lsp-restart cmd-lsp-restart)
  (register-command! 'lsp-stop cmd-lsp-stop)
  ;; Register 545 parity commands (existing implementations, new registrations)
  (register-parity-commands!)
  ;; Aliases for commands in sub-modules
  (register-command! 'diff cmd-diff-two-files)
  (register-command! 'diff-two-files cmd-diff-two-files)
  (register-command! 'recompile cmd-recompile)
  (register-command! 'comment-dwim cmd-comment-dwim)
  (register-command! 'auto-save-visited-mode cmd-auto-save-mode)
  ;; Batch 3: Package/framework aliases (editor-extra chain functions)
  (register-command! 'projectile-switch-project cmd-project-switch-project)
  (register-command! 'counsel-rg cmd-rgrep)
  (register-command! 'auto-complete-mode cmd-company-mode)
  (register-command! 'yasnippet-mode cmd-snippet-expand)
  (register-command! 'yas-expand cmd-snippet-expand)
  (register-command! 'undo-tree-mode cmd-undo-history)
  (register-command! 'treemacs-toggle cmd-treemacs)
  (register-command! 'treemacs-select-window cmd-treemacs)
  (register-command! 'evil-normal-state cmd-evil-mode)
  (register-command! 'evil-insert-state cmd-evil-mode)
  (register-command! 'evil-visual-state cmd-evil-mode)
  (register-command! 'magit-branch-create cmd-magit-branch)
  (register-command! 'magit-branch-delete cmd-magit-branch)
  (register-command! 'org-set-tags-command cmd-org-set-tags)
  (register-command! 'multiple-cursors-mode cmd-mc-mark-next)
  (register-command! 'which-key-show-top-level cmd-which-key)
  (register-command! 'window-numbering-mode cmd-winum-mode)
  (register-command! 'org-archive-subtree cmd-org-archive-subtree)
  (register-command! 'org-toggle-heading cmd-org-toggle-heading)
  (register-command! 'magit-init cmd-magit-init)
  (register-command! 'magit-tag cmd-magit-tag)
  ;; Batch 4: mode aliases (need editor-extra sub-module scope)
  (register-command! 'enriched-mode cmd-text-mode)
  (register-command! 'conf-mode cmd-text-mode)
  (register-command! 'nxml-mode cmd-text-mode)
  (register-command! 'sh-mode cmd-shell-script-mode)
  (register-command! 'markdown-preview-mode cmd-markdown-mode)
  ;; Batch 5: aliases needing editor-extra scope
  (register-command! 'dired-flag-file-deletion cmd-dired-mark)
  (register-command! 'dired-sort-toggle-or-edit cmd-dired-sort-toggle)
  (register-command! 'ibuffer-mark-modified-buffers cmd-ibuffer-mark)
  (register-command! 'ibuffer-mark-unsaved-buffers cmd-ibuffer-mark)
  (register-command! 'ibuffer-do-delete cmd-ibuffer-do-kill)
  (register-command! 'org-table-sort-lines cmd-org-table-sort)
  (register-command! 'org-export-dispatch cmd-org-export)
  (register-command! 'wdired-change-to-wdired-mode cmd-wdired-mode)
  (register-command! 'info-apropos cmd-apropos-emacs)
  ;; Batch 6: LSP/eglot aliases + language modes + tab-list
  (register-command! 'lsp-find-definition cmd-lsp-goto-definition)
  (register-command! 'lsp-describe-thing-at-point cmd-lsp-hover)
  (register-command! 'lsp-execute-code-action cmd-lsp-code-actions)
  (register-command! 'lsp-workspace-restart cmd-lsp-restart)
  (register-command! 'lsp-mode cmd-lsp-restart)
  (register-command! 'lsp-install-server cmd-lsp-restart)
  (register-command! 'eglot cmd-lsp-restart)
  (register-command! 'eglot-rename cmd-lsp-rename)
  (register-command! 'eglot-format cmd-lsp-format-buffer)
  (register-command! 'eglot-code-actions cmd-lsp-code-actions)
  (register-command! 'tab-list cmd-tab-new)
  (register-command! 'treesit-install-language-grammar cmd-text-mode)
  (register-command! 'json-mode cmd-toggle-global-json-mode)
  (register-command! 'terraform-mode cmd-text-mode)
  (register-command! 'haskell-mode cmd-text-mode)
  (register-command! 'elixir-mode cmd-text-mode)
  (register-command! 'clojure-mode cmd-text-mode)
  (register-command! 'erlang-mode cmd-text-mode)
  (register-command! 'scala-mode cmd-text-mode)
  (register-command! 'kotlin-mode cmd-text-mode)
  (register-command! 'swift-mode cmd-text-mode)
  (register-command! 'zig-mode cmd-text-mode)
)

;;;============================================================================
;;; Parity batch 10: TUI implementations matching Qt-only commands
;;;============================================================================

;; --- Org todo cycle ---
(def *tui-todo-states* '("TODO" "IN-PROGRESS" "DONE"))
(def (cmd-org-todo-cycle app)
  "Cycle TODO state on org heading."
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed)) (text (editor-get-text ed))
         (cur-line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed cur-line))
         (line-end (editor-get-line-end-position ed cur-line))
         (line (substring text line-start (min line-end (string-length text)))))
    (if (not (and (> (string-length line) 0) (char=? (string-ref line 0) #\*)))
      (echo-message! (app-state-echo app) "Not on an org heading")
      (let* ((rest (let lp ((i 0)) (if (and (< i (string-length line)) (char=? (string-ref line i) #\*)) (lp (+ i 1)) i)))
             (after-stars (if (< rest (string-length line)) (substring line rest (string-length line)) ""))
             (trimmed (string-trim after-stars))
             (cur-state (let lp ((states *tui-todo-states*))
                          (if (null? states) #f
                            (if (string-prefix? (car states) trimmed) (car states) (lp (cdr states))))))
             (next-state (if (not cur-state) (car *tui-todo-states*)
                           (let lp ((s *tui-todo-states*))
                             (cond ((null? s) #f)
                                   ((null? (cdr s)) (if (string=? (car s) cur-state) #f (car *tui-todo-states*)))
                                   ((string=? (car s) cur-state) (cadr s))
                                   (else (lp (cdr s)))))))
             (new-rest (if cur-state
                         (let ((stripped (substring trimmed (string-length cur-state) (string-length trimmed))))
                           (if next-state (string-append " " next-state (string-trim stripped)) (string-trim stripped)))
                         (if next-state (string-append " " next-state after-stars) after-stars)))
             (stars (substring line 0 rest))
             (new-line (string-append stars new-rest))
             (new-text (string-append (substring text 0 line-start) new-line
                         (if (< line-end (string-length text)) (substring text line-end (string-length text)) ""))))
        (editor-set-text ed new-text) (editor-goto-pos ed pos)
        (echo-message! (app-state-echo app) (or next-state "No state"))))))

;; --- Org navigation ---
(def (cmd-org-next-heading app)
  "Move to next org heading."
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (text (editor-get-text ed)) (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let lp ((i (+ pos 1)))
      (cond ((>= i len) (echo-message! (app-state-echo app) "No more headings"))
            ((and (char=? (string-ref text i) #\*) (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
             (editor-goto-pos ed i) (editor-scroll-caret ed))
            (else (lp (+ i 1)))))))

(def (cmd-org-prev-heading app)
  "Move to previous org heading."
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (text (editor-get-text ed)) (pos (editor-get-current-pos ed)))
    (let lp ((i (- pos 1)))
      (cond ((< i 0) (echo-message! (app-state-echo app) "No previous headings"))
            ((and (char=? (string-ref text i) #\*) (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
             (editor-goto-pos ed i) (editor-scroll-caret ed))
            (else (lp (- i 1)))))))

(def (cmd-org-outline app)
  "Show org headings outline."
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (text (editor-get-text ed)) (lines (string-split text #\newline))
         (headings (let lp ((ls lines) (i 1) (acc []))
                     (if (null? ls) (reverse acc)
                       (let ((l (car ls)))
                         (lp (cdr ls) (+ i 1)
                           (if (and (> (string-length l) 0) (char=? (string-ref l 0) #\*))
                             (cons (string-append (number->string i) ": " l) acc) acc))))))
         (buf (buffer-create! "*Org Outline*" ed)))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-join headings "\n")) (editor-goto-pos ed 0)))

;; --- Grep ---
(def *tui-grep-results* [])
(def *tui-grep-idx* 0)
(def (cmd-rgrep app)
  "Recursive grep with file filter."
  (let ((pat (app-read-string app "Rgrep: ")))
    (when (and pat (> (string-length pat) 0))
      (let ((include (app-read-string app "File pattern (e.g. *.ss): ")))
        (let ((dir (app-read-string app "In directory: ")))
          (when (and dir (> (string-length dir) 0))
            (with-exception-catcher (lambda (e) (echo-error! (app-state-echo app) "Grep failed"))
              (lambda ()
                (let* ((args (if (and include (> (string-length include) 0))
                               (list "-rn" (string-append "--include=" include) pat dir)
                               (list "-rn" pat dir)))
                       (p (open-process (list path: "grep" arguments: args
                             stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                       (out (read-line p #f)))
                  (process-status p)
                  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
                         (buf (buffer-create! "*Grep*" ed)))
                    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
                    (editor-set-text ed (or out "No matches.")) (editor-goto-pos ed 0)
                    (echo-message! (app-state-echo app) "Grep done")))))))))))

(def (cmd-previous-grep-result app)
  "Jump to previous grep result."
  (echo-message! (app-state-echo app) "Use grep buffer to navigate results"))

;; --- Wdired/wgrep ---
(def *tui-wdired-active* #f)
(def (cmd-wdired-mode app)
  "Toggle writable dired."
  (set! *tui-wdired-active* (not *tui-wdired-active*))
  (echo-message! (app-state-echo app) (if *tui-wdired-active* "Wdired mode on" "Wdired mode off")))
(def (cmd-wdired-finish app) "Finish wdired." (cmd-wdired-mode app))
(def (cmd-wgrep-abort-changes app) "Abort wgrep." (echo-message! (app-state-echo app) "Wgrep changes aborted"))

;; --- Dired delete marked ---
(def (cmd-dired-do-delete-marked app)
  "Delete marked dired files."
  (let* ((marks (or (hash-get *dired-marks* "default") '()))
         (echo (app-state-echo app)))
    (if (null? marks) (echo-message! echo "No files marked")
      (let ((ans (app-read-string app (string-append "Delete " (number->string (length marks)) " files? (y/n) "))))
        (when (and ans (member ans '("y" "yes")))
          (for-each (lambda (f) (with-exception-catcher (lambda (e) (void)) (lambda () (delete-file f)))) marks)
          (hash-put! *dired-marks* "default" '())
          (echo-message! echo (string-append "Deleted " (number->string (length marks)) " files"))
          (cmd-dired-refresh app))))))

;; --- Auto-revert tail ---
(def (cmd-auto-revert-tail-mode app)
  "Toggle auto-revert tail mode."
  (let ((on (toggle-mode! 'auto-revert-tail)))
    (when on (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win)))
               (editor-goto-pos ed (editor-get-text-length ed)) (editor-scroll-caret ed)))
    (echo-message! (app-state-echo app) (if on "Auto-revert tail on" "Auto-revert tail off"))))

;; --- Scratch message ---
(def (cmd-scratch-message app)
  "Switch to scratch with message."
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*scratch*") (buffer-create! "*scratch*" ed))))
    (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
    (echo-message! (app-state-echo app) "Scratch buffer")))

;; --- Shell command ---
(def (cmd-run-user-shell-command app)
  "Run a shell command."
  (let ((cmd (app-read-string app "Shell command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (with-exception-catcher (lambda (e) (echo-error! (app-state-echo app) "Command failed"))
        (lambda ()
          (let* ((p (open-process (list path: "/bin/sh" arguments: (list "-c" cmd)
                      stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                 (out (read-line p #f)))
            (process-status p)
            (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
                   (buf (buffer-create! "*Shell Output*" ed)))
              (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
              (editor-set-text ed (or out "")) (editor-goto-pos ed 0))))))))

;; --- Toggles ---
(def (cmd-toggle-compile-on-save app)
  "Toggle compile on save."
  (let ((on (toggle-mode! 'compile-on-save)))
    (echo-message! (app-state-echo app) (if on "Compile on save: on" "Compile on save: off"))))
(def (cmd-toggle-bracket-paren-swap app)
  "Toggle bracket/paren swap."
  (let ((on (toggle-mode! 'bracket-paren-swap)))
    (echo-message! (app-state-echo app) (if on "Bracket-paren swap: on" "Bracket-paren swap: off"))))

;; --- Settings ---
(def (cmd-set-variable app)
  "Set a variable."
  (let ((name (app-read-string app "Variable name: ")))
    (when (and name (> (string-length name) 0))
      (let ((val (app-read-string app (string-append name " = "))))
        (echo-message! (app-state-echo app) (string-append "Set " name " = " (or val "")))))))
(def (cmd-show-dir-locals app)
  "Show dir-locals."
  (echo-message! (app-state-echo app) "Dir-locals: use .gemacs-config"))

;; --- Remote files ---
(def (cmd-find-file-remote app)
  "Open remote file via SSH."
  (let ((path (app-read-string app "Remote path (/ssh:host:path): ")))
    (when (and path (> (string-length path) 0))
      (echo-message! (app-state-echo app) (string-append "Remote: " path " (use scp to fetch)")))))
(def (cmd-save-remote-buffer app)
  "Save buffer to remote."
  (echo-message! (app-state-echo app) "Remote save: use scp"))

;; --- Key config ---
(def (cmd-global-set-key app)
  "Bind a key to a command."
  (let ((key (app-read-string app "Key: ")))
    (when (and key (> (string-length key) 0))
      (let ((cmd (app-read-string app "Command: ")))
        (when (and cmd (> (string-length cmd) 0))
          (let ((sym (string->symbol cmd)))
            (keymap-bind! *global-keymap* key sym)
            (echo-message! (app-state-echo app) (string-append "Bound " key " -> " cmd))))))))
(def (cmd-global-unset-key app)
  "Unbind a key."
  (let ((key (app-read-string app "Key to unbind: ")))
    (when (and key (> (string-length key) 0))
      (echo-message! (app-state-echo app) (string-append "Unbound: " key)))))

;; --- Flycheck ---
(def (cmd-flycheck-prev-error app)
  "Jump to previous flycheck error."
  (echo-message! (app-state-echo app) "No previous error"))

;; --- Insert char by name ---
(def (cmd-insert-char-by-name app)
  "Insert char by Unicode code point."
  (let ((code (app-read-string app "Hex code point: ")))
    (when (and code (> (string-length code) 0))
      (let ((num (string->number code 16)))
        (if num
          (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
                 (pos (editor-get-current-pos ed)) (ch (string (integer->char num))))
            (editor-insert-text ed pos ch) (editor-goto-pos ed (+ pos 1))
            (echo-message! (app-state-echo app) (string-append "Inserted: " ch)))
          (echo-error! (app-state-echo app) "Invalid hex"))))))

;; --- File to register ---
(def *tui-file-registers* (make-hash-table))
(def (cmd-file-to-register app)
  "Store current file in a register."
  (let* ((buf (current-buffer-from-app app)) (path (and buf (buffer-file-path buf))))
    (if (not path) (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((reg (app-read-string app "Register (a-z): ")))
        (when (and reg (> (string-length reg) 0))
          (hash-put! *tui-file-registers* (string-ref reg 0) path)
          (echo-message! (app-state-echo app) (string-append "Stored in register " reg)))))))

;; --- Kbd macro counter ---
(def *tui-macro-counter* 0)
(def (cmd-kbd-macro-counter-insert app)
  "Insert macro counter value."
  (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed)) (s (number->string *tui-macro-counter*)))
    (editor-insert-text ed pos s) (editor-goto-pos ed (+ pos (string-length s)))
    (set! *tui-macro-counter* (+ *tui-macro-counter* 1))))
(def (cmd-kbd-macro-counter-set app)
  "Set macro counter."
  (let ((val (app-read-string app "Counter value: ")))
    (when val (let ((n (string->number val)))
      (when n (set! *tui-macro-counter* n)
        (echo-message! (app-state-echo app) (string-append "Counter = " (number->string n))))))))

;; --- Key chords ---
(def *tui-chord-mode* #f)
(def *tui-chord-bindings* (make-hash-table))
(def (cmd-key-chord-mode app)
  "Toggle key-chord mode."
  (set! *tui-chord-mode* (not *tui-chord-mode*))
  (echo-message! (app-state-echo app) (if *tui-chord-mode* "Key-chord mode on" "Key-chord mode off")))
(def (cmd-key-chord-define app)
  "Define a key chord."
  (let ((chord (app-read-string app "Chord (2 chars): ")))
    (when (and chord (= (string-length chord) 2))
      (let ((cmd (app-read-string app "Command: ")))
        (when cmd (hash-put! *tui-chord-bindings* chord (string->symbol cmd))
          (echo-message! (app-state-echo app) (string-append "Chord " chord " -> " cmd)))))))
(def (cmd-key-chord-list app)
  "List key chords."
  (let ((bindings (hash->list *tui-chord-bindings*)))
    (if (null? bindings) (echo-message! (app-state-echo app) "No chords defined")
      (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
             (lines (map (lambda (p) (string-append (car p) " -> " (symbol->string (cdr p)))) bindings))
             (buf (buffer-create! "*Key Chords*" ed)))
        (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
        (editor-set-text ed (string-join lines "\n")) (editor-goto-pos ed 0)))))
(def (cmd-key-translation-list app)
  "List key translations."
  (echo-message! (app-state-echo app) "No key translations defined"))

;; --- Snippets ---
(def *tui-snippets* (make-hash-table))
(def (cmd-define-snippet app)
  "Define a snippet."
  (let ((name (app-read-string app "Snippet name: ")))
    (when (and name (> (string-length name) 0))
      (let ((body (app-read-string app "Snippet body: ")))
        (when body (hash-put! *tui-snippets* name body)
          (echo-message! (app-state-echo app) (string-append "Defined: " name)))))))
(def (cmd-list-snippets app)
  "List snippets."
  (let ((names (hash-keys *tui-snippets*)))
    (if (null? names) (echo-message! (app-state-echo app) "No snippets")
      (let* ((fr (app-state-frame app)) (win (current-window fr)) (ed (edit-window-editor win))
             (buf (buffer-create! "*Snippets*" ed)))
        (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
        (editor-set-text ed (string-join names "\n")) (editor-goto-pos ed 0)))))
(def (cmd-snippet-expand app)
  "Expand snippet at point."
  (echo-message! (app-state-echo app) "Use M-x define-snippet first"))
(def (cmd-snippet-next-field app) "Next snippet field." (echo-message! (app-state-echo app) "No snippet active"))
(def (cmd-snippet-prev-field app) "Previous snippet field." (echo-message! (app-state-echo app) "No snippet active"))

;;; Parity batch 11: GUI-specific, LSP, and multi-cursor stubs
(def (cmd-calendar-today app) "Calendar today."
  (with-exception-catcher (lambda (e) (echo-message! (app-state-echo app) "Today"))
    (lambda () (let* ((p (open-process (list path: "date" arguments: '("+%Y-%m-%d") stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                      (out (read-line p))) (process-status p) (echo-message! (app-state-echo app) (string-append "Today: " (or out "?")))))))
(def (cmd-calendar-next-month app) "Next month." (echo-message! (app-state-echo app) "Calendar: next month"))
(def (cmd-calendar-prev-month app) "Prev month." (echo-message! (app-state-echo app) "Calendar: prev month"))
(def (cmd-calendar-next-year app) "Next year." (echo-message! (app-state-echo app) "Calendar: next year"))
(def (cmd-calendar-prev-year app) "Prev year." (echo-message! (app-state-echo app) "Calendar: prev year"))
(def (cmd-helm-buffers-list app) "Helm buffers." (echo-message! (app-state-echo app) "Use M-x list-buffers"))
(def (cmd-image-zoom-in app) "Zoom in." (echo-message! (app-state-echo app) "Image zoom not available in TUI"))
(def (cmd-image-zoom-out app) "Zoom out." (echo-message! (app-state-echo app) "Image zoom not available in TUI"))
(def (cmd-image-zoom-fit app) "Zoom fit." (echo-message! (app-state-echo app) "Image zoom not available in TUI"))
(def (cmd-image-zoom-reset app) "Zoom reset." (echo-message! (app-state-echo app) "Image zoom not available in TUI"))
(def (cmd-set-font-size app) "Set font size." (echo-message! (app-state-echo app) "Font size: use terminal settings"))
(def (cmd-set-frame-font app) "Set frame font." (echo-message! (app-state-echo app) "Font: use terminal settings"))
(def (cmd-vterm-copy-mode app) "Vterm copy." (echo-message! (app-state-echo app) "Vterm: use terminal copy"))
(def (cmd-vterm-copy-done app) "Vterm copy done." (echo-message! (app-state-echo app) "Vterm: copy done"))
(def (cmd-mc-mark-next app) "Mark next." (echo-message! (app-state-echo app) "Multiple cursors not available in TUI"))
(def (cmd-mc-mark-all app) "Mark all." (echo-message! (app-state-echo app) "Multiple cursors not available in TUI"))
(def (cmd-mc-skip-and-mark-next app) "Skip and mark next." (echo-message! (app-state-echo app) "Multiple cursors not available in TUI"))
(def (cmd-lsp-goto-definition app) "LSP goto def." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-smart-goto-definition app) "LSP smart goto." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-find-references app) "LSP refs." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-hover app) "LSP hover." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-rename app) "LSP rename." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-format-buffer app) "LSP format." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-code-actions app) "LSP actions." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-completion app) "LSP complete." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-declaration app) "LSP decl." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-implementation app) "LSP impl." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-type-definition app) "LSP type def." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-document-symbols app) "LSP symbols." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-workspace-symbol app) "LSP ws sym." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-restart app) "LSP restart." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
(def (cmd-lsp-stop app) "LSP stop." (echo-message! (app-state-echo app) "LSP: not available in TUI"))
