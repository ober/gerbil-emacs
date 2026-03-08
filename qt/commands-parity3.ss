;;; -*- Gerbil -*-
;;; Qt parity commands (part 3) — bulk toggle command registration.
;;; Chain position: after commands-parity2, before commands-aliases.
;;; Registers 339 toggle commands that match TUI parity.

(export #t)

(import :std/sugar
        :std/srfi/13
        :std/misc/string
        :gemacs/core
        :gemacs/qt/echo
        :gemacs/qt/commands-core
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-search2
        :gemacs/qt/commands-file
        :gemacs/qt/commands-file2
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-sexp2
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-ide2
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-vcs2
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-shell2
        :gemacs/qt/commands-modes
        :gemacs/qt/commands-modes2
        :gemacs/qt/commands-config
        :gemacs/qt/commands-config2
        :gemacs/qt/commands-parity
        :gemacs/qt/commands-parity2)

;;;============================================================================
;;; Bulk toggle registration infrastructure
;;;============================================================================

;; Shared state for all bulk-registered toggles
(def *qt-toggle-states* (make-hash-table))

(def (toggle-name->display name)
  "Convert toggle symbol name to human-readable display string.
   e.g. 'toggle-aggressive-indent -> \"Aggressive Indent\""
  (let* ((s (symbol->string name))
         ;; Remove 'toggle-' prefix
         (base (if (string-prefix? "toggle-" s)
                 (substring s 7 (string-length s))
                 s))
         ;; Replace hyphens with spaces and capitalize words
         (words (string-split base #\-)))
    (string-join
     (map (lambda (w)
            (if (> (string-length w) 0)
              (string-append (string (char-upcase (string-ref w 0)))
                             (substring w 1 (string-length w)))
              w))
          words)
     " ")))

(def (make-toggle-command name)
  "Create a toggle command closure for the given name symbol."
  (let ((display-name (toggle-name->display name)))
    (lambda (app)
      (let* ((cur (hash-get *qt-toggle-states* name))
             (new (not cur)))
        (hash-put! *qt-toggle-states* name new)
        (echo-message! (app-state-echo app)
          (string-append display-name (if new " ON" " OFF")))))))

(def (qt-register-parity3-toggles!)
  "Register all bulk toggle commands for Qt parity."
  (for-each
    (lambda (name)
      (register-command! name (make-toggle-command name)))
    *parity3-toggle-names*))

;;;============================================================================
;;; Toggle command names — all 339 missing toggles
;;;============================================================================

(def *parity3-toggle-names*
  '(toggle-ad-activate-all
    toggle-aggressive-indent
    toggle-allout-mode
    toggle-all-the-icons
    toggle-auto-composition
    toggle-auto-compression
    toggle-auto-dim-other-buffers
    toggle-auto-encryption
    toggle-auto-fill-comments
    toggle-auto-highlight-symbol
    toggle-auto-insert-mode
    toggle-auto-package-mode
    toggle-auto-rename-tag
    toggle-auto-revert-tail
    toggle-auto-revert-verbose
    toggle-auto-save-buffers
    toggle-auto-save-default
    toggle-auto-save-on-idle
    toggle-auto-save-visited
    toggle-auto-window-vscroll
    toggle-bidi-display
    toggle-blink-cursor-mode
    toggle-blink-matching-paren
    toggle-block-comment
    toggle-buffer-read-only
    toggle-cape-mode
    toggle-caret-style
    toggle-colon-double-space
    toggle-column-ruler
    toggle-comment-auto-fill
    toggle-comment-style
    toggle-company-mode
    toggle-confirm-kill-emacs
    toggle-consult-mode
    toggle-context-menu-mode
    toggle-corfu-mode
    toggle-create-lockfiles
    toggle-cua-mode
    toggle-cua-selection-mode
    toggle-cursor-blink
    toggle-cursor-in-non-selected-windows
    toggle-cursor-type
    toggle-debug-on-signal
    toggle-delete-active-region
    toggle-delete-by-moving-to-trash
    toggle-delete-pair-blink
    toggle-delete-selection
    toggle-delete-trailing-on-save
    toggle-desktop-save-mode
    toggle-diff-hl-mode
    toggle-display-battery
    toggle-display-fill-column-indicator
    toggle-display-line-numbers
    toggle-display-time
    toggle-doom-modeline
    toggle-doom-themes
    toggle-ef-themes
    toggle-eglot-mode
    toggle-electric-indent-mode
    toggle-electric-quote
    toggle-embark-mode
    toggle-enable-dir-local-variables
    toggle-enable-local-variables
    toggle-fast-but-imprecise-scrolling
    toggle-file-name-shadow
    toggle-flymake-mode
    toggle-flyspell-prog
    toggle-focus-mode
    toggle-fringe
    toggle-global-abbrev
    toggle-global-aggressive-indent
    toggle-global-all-the-icons-dired
    toggle-global-animate-typing
    toggle-global-ansible
    toggle-global-anzu
    toggle-global-auto-compile
    toggle-global-auto-complete
    toggle-global-auto-composition
    toggle-global-auto-highlight
    toggle-global-auto-revert-non-file
    toggle-global-avy
    toggle-global-awesome-tab
    toggle-global-bazel-mode
    toggle-global-beacon
    toggle-global-benchmark-init
    toggle-global-buttercup
    toggle-global-centaur-tabs
    toggle-global-centered-cursor
    toggle-global-childframe
    toggle-global-cider
    toggle-global-clojure-mode
    toggle-global-cmake-mode
    toggle-global-color-identifiers
    toggle-global-command-log
    toggle-global-company
    toggle-global-conda
    toggle-global-copilot
    toggle-global-counsel
    toggle-global-csv-mode
    toggle-global-cwarn
    toggle-global-dap-mode
    toggle-global-dart-mode
    toggle-global-deadgrep
    toggle-global-delight
    toggle-global-diff-auto-refine
    toggle-global-diff-hl
    toggle-global-diminish
    toggle-global-dimmer
    toggle-global-direnv
    toggle-global-display-fill-column
    toggle-global-display-line-numbers
    toggle-global-docker
    toggle-global-doom-modeline-env
    toggle-global-dtrt-indent
    toggle-global-editorconfig
    toggle-global-ein
    toggle-global-eldoc-box
    toggle-global-elisp-demos
    toggle-global-elixir-mode
    toggle-global-elpy
    toggle-global-envrc
    toggle-global-erlang-mode
    toggle-global-ert-runner
    toggle-global-ess
    toggle-global-esup
    toggle-global-expand-region
    toggle-global-explain-pause
    toggle-global-fireplace
    toggle-global-flycheck
    toggle-global-flyspell-lazy
    toggle-global-focus
    toggle-global-font-lock
    toggle-global-format-on-save
    toggle-global-fsharp-mode
    toggle-global-gcmh
    toggle-global-general
    toggle-global-git-gutter
    toggle-global-golden-ratio
    toggle-global-go-mode
    toggle-global-goto-address
    toggle-global-goto-chg
    toggle-global-graphql-mode
    toggle-global-groovy-mode
    toggle-global-haskell-mode
    toggle-global-helpful
    toggle-global-hideshow
    toggle-global-highlight-indent
    toggle-global-highlight-parentheses
    toggle-global-hi-lock-mode
    toggle-global-hl-todo
    toggle-global-hydra
    toggle-global-interaction-log
    toggle-global-js2-mode
    toggle-global-json-mode
    toggle-global-julia-mode
    toggle-global-keyfreq
    toggle-global-kotlin-mode
    toggle-global-kubernetes
    toggle-global-linum
    toggle-global-lsp-headerline
    toggle-global-lsp-ivy
    toggle-global-lsp-lens
    toggle-global-lsp-mode
    toggle-global-lsp-semantic-tokens
    toggle-global-lsp-treemacs
    toggle-global-lsp-ui
    toggle-global-lua-mode
    toggle-global-meson-mode
    toggle-global-mini-frame
    toggle-global-minions
    toggle-global-moody
    toggle-global-move-dup
    toggle-global-multiple-cursors
    toggle-global-native-compile
    toggle-global-ninja-mode
    toggle-global-nix-mode
    toggle-global-no-littering
    toggle-global-nyan-cat
    toggle-global-ob-http
    toggle-global-ocaml-mode
    toggle-global-org-cliplink
    toggle-global-org-download
    toggle-global-org-journal
    toggle-global-org-noter
    toggle-global-org-present
    toggle-global-org-roam
    toggle-global-org-super-agenda
    toggle-global-origami
    toggle-global-page-break-lines
    toggle-global-parrot
    toggle-global-php-mode
    toggle-global-pipenv
    toggle-global-popper
    toggle-global-popwin
    toggle-global-posframe
    toggle-global-power-mode
    toggle-global-prescient
    toggle-global-prettify
    toggle-global-prettify-symbols
    toggle-global-projectile-ripgrep
    toggle-global-protobuf-mode
    toggle-global-pulse-line
    toggle-global-python-black
    toggle-global-pyvenv
    toggle-global-rainbow-mode
    toggle-global-restclient
    toggle-global-reveal-mode
    toggle-global-rich-minority
    toggle-global-ripgrep
    toggle-global-r-mode
    toggle-global-ruby-mode
    toggle-global-rustic
    toggle-global-scala-mode
    toggle-global-shackle
    toggle-global-smart-mode-line
    toggle-global-smartparens
    toggle-global-snow
    toggle-global-so-clean
    toggle-global-solaire
    toggle-global-so-long
    toggle-global-spaceline
    toggle-global-sql-mode
    toggle-global-subword
    toggle-global-suggest
    toggle-global-superword
    toggle-global-swift-mode
    toggle-global-swiper
    toggle-global-symbol-overlay
    toggle-global-tab-bar
    toggle-global-terraform
    toggle-global-toml-mode
    toggle-global-transient
    toggle-global-treemacs-icons
    toggle-global-tree-sitter
    toggle-global-typescript-mode
    toggle-global-undercover
    toggle-global-undo-propose
    toggle-global-undo-tree
    toggle-global-use-package
    toggle-global-vagrant
    toggle-global-vertico-posframe
    toggle-global-visual-line
    toggle-global-visual-regexp
    toggle-global-web-mode
    toggle-global-wgrep
    toggle-global-which-key
    toggle-global-whitespace-mode
    toggle-global-whitespace-newline
    toggle-global-ws-trim
    toggle-global-yaml-mode
    toggle-global-yas
    toggle-global-zig-mode
    toggle-global-zone
    toggle-global-zoom-window
    toggle-header-line
    toggle-hide-ifdef-mode
    toggle-highlight-changes
    toggle-highlight-indentation
    toggle-history-delete-duplicates
    toggle-hl-todo
    toggle-hungry-delete
    toggle-image-mode
    toggle-indent-guide
    toggle-indent-guide-global
    toggle-indicate-buffer-boundaries
    toggle-indicate-empty-lines
    toggle-inhibit-startup-screen
    toggle-ivy-mode
    toggle-kill-whole-line
    toggle-ligature-mode
    toggle-line-move-visual
    toggle-line-spacing
    toggle-lock-file-create
    toggle-make-backup-files
    toggle-make-pointer-invisible
    toggle-marginalia-mode
    toggle-midnight-mode
    toggle-minibuffer-depth-indicate
    toggle-minibuffer-electric-default
    toggle-mode-line
    toggle-modeline
    toggle-mode-line-compact
    toggle-modus-themes
    toggle-mouse-avoidance
    toggle-nano-theme
    toggle-nerd-icons-mode
    toggle-next-error-follow
    toggle-next-line-add-newlines
    toggle-orderless-mode
    toggle-pixel-scroll-2
    toggle-pixel-scroll-precision
    toggle-prettify-symbols
    toggle-projectile-mode
    toggle-quotes
    toggle-read-only-directories
    toggle-recentf-mode
    toggle-relative-line-numbers
    toggle-ring-bell-function
    toggle-savehist-mode
    toggle-save-silently
    toggle-scroll-bar-mode
    toggle-scroll-conservatively
    toggle-selection-mode
    toggle-selective-display
    toggle-sentence-end-double-space
    toggle-set-mark-command-repeat-pop
    toggle-shift-select-mode
    toggle-show-keystroke
    toggle-show-paren-style
    toggle-show-paren-when-point-inside
    toggle-soft-wrap
    toggle-subword-mode
    toggle-tab-line-mode
    toggle-tool-bar-mode
    toggle-tooltip-mode
    toggle-transient-mark-mode
    toggle-treesit-mode
    toggle-truncate-partial-width-windows
    toggle-type-break
    toggle-undo-fu-session
    toggle-uniquify-buffer-names
    toggle-use-dialog-box
    toggle-use-file-dialog
    toggle-use-short-answers
    toggle-vertico-mode
    toggle-virtual-space
    toggle-visible-cursor
    toggle-visible-mark
    toggle-volatile-highlights
    toggle-which-function
    toggle-which-key-mode
    toggle-whitespace-cleanup-on-save
    toggle-winner-mode
    toggle-word-boundary
    toggle-word-count
    toggle-word-wrap-column
    toggle-ws-butler-mode
    toggle-xterm-mouse-mode
    toggle-zen-mode))
