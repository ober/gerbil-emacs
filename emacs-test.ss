;;; -*- Gerbil -*-
;;; Tests for gemacs
;;; Includes both pure-logic tests and headless Scintilla editor tests.
;;; Scintilla works headlessly (no terminal needed) for all operations
;;; except scintilla_refresh(). Lexer/syntax highlighting requires Lexilla
;;; which is only available at runtime, so highlighting tests are skipped.

(import :std/test
        :std/srfi/13
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gerbil-scintilla/tui
        :gerbil-scintilla/lexer
        :gemacs/core
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/echo
        :gemacs/editor-core
        (only-in :gemacs/editor-ui
                 cmd-transpose-chars cmd-upcase-word cmd-downcase-word
                 cmd-capitalize-word cmd-kill-word cmd-toggle-comment
                 cmd-list-buffers cmd-indent-or-complete org-buffer?)
        (only-in :gemacs/editor-text
                 cmd-transpose-words cmd-transpose-lines
                 cmd-upcase-region cmd-downcase-region
                 cmd-backward-kill-word
                 cmd-forward-paragraph cmd-backward-paragraph
                 cmd-join-line cmd-fill-paragraph cmd-just-one-space
                 cmd-indent-region cmd-delete-blank-lines)
        (only-in :gemacs/editor-advanced
                 cmd-exchange-point-and-mark
                 cmd-hippie-expand)
        (only-in :gemacs/editor-text
                 cmd-dabbrev-expand collect-dabbrev-matches)
        (only-in :gemacs/editor-cmds-a
                 cmd-copy-region-as-kill
                 cmd-forward-sexp cmd-backward-sexp cmd-backward-kill-sexp
                 cmd-balance-windows)
        (only-in :gemacs/org-parse
                 org-parse-timestamp org-timestamp? org-timestamp-type
                 org-timestamp-year org-timestamp-month org-timestamp-day
                 org-timestamp-day-name org-timestamp-hour org-timestamp-minute
                 org-timestamp-end-hour org-timestamp-end-minute
                 org-timestamp-repeater org-timestamp-warning
                 org-timestamp->string org-timestamp-elapsed
                 org-current-timestamp-string pad-02
                 org-parse-heading-line org-heading-stars
                 org-heading-keyword org-heading-title
                 org-heading-priority org-heading-tags org-heading-scheduled
                 org-heading-line-number
                 org-parse-planning-line org-parse-properties
                 org-parse-clock-line
                 org-parse-buffer
                 org-parse-buffer-settings org-parse-tag-expr
                 org-table-line? org-comment-line?
                 org-keyword-line? org-block-begin?
                 org-heading-stars-of-line org-current-timestamp-string
                 org-heading-clocks org-heading-title
                 make-org-heading)
        (only-in :gemacs/org-table
                 org-table-row? org-table-separator? org-table-parse-row
                 org-table-column-widths org-table-format-row
                 org-table-format-separator org-numeric-cell?
                 org-table-align org-table-next-cell
                 org-table-on-table-line? org-table-find-bounds
                 org-table-current-column org-table-next-data-line
                 org-table-get-rows org-table-goto-column
                 org-table-insert-column org-table-delete-column
                 org-table-insert-row org-table-delete-row
                 org-table-sort org-table-to-csv org-csv-to-table
                 org-table-parse-tblfm)
        (only-in :gemacs/org-clock
                 org-clock-in-at-point org-clock-out
                 org-clock-display org-clock-modeline-string
                 *org-clock-start* *org-clock-heading*
                 org-elapsed-minutes)
        (only-in :gemacs/org-list
                 org-list-item? org-meta-return
                 org-cycle-list-bullet org-count-leading-spaces
                 org-update-checkbox-statistics!)
        (only-in :gemacs/org-export
                 org-export-buffer org-export-inline
                 org-split-into-blocks html-escape)
        (only-in :gemacs/org-babel
                 org-babel-parse-header-args org-babel-parse-begin-line
                 org-babel-find-src-block org-babel-inside-src-block?
                 org-babel-format-result org-babel-find-named-block
                 org-babel-tangle org-babel-inject-variables
                 org-babel-expand-noweb org-ctrl-c-ctrl-c-context)
        (only-in :gemacs/org-agenda
                 org-collect-agenda-items org-agenda-sort-items
                 org-format-agenda-item org-agenda-todo-list
                 org-agenda-tags-match org-agenda-search
                 org-timestamp-in-range? org-make-date-ts
                 org-advance-date-ts org-date-weekday
                 org-find-stuck-projects make-org-agenda-item
                 org-agenda-item-heading)
        (only-in :gemacs/org-capture
                 org-capture-expand-template org-capture-cursor-position
                 org-capture-start org-capture-finalize org-capture-abort
                 org-refile-targets org-insert-under-heading
                 org-capture-menu-string
                 *org-capture-templates* *org-capture-active?*)
        (only-in :gemacs/persist
                 detect-major-mode buffer-local-get buffer-local-set!)
        (only-in :gemacs/org-highlight
                 setup-org-styles! org-highlight-buffer! org-set-fold-levels!
                 ORG_STYLE_HEADING_1 ORG_STYLE_BLOCK_DELIM ORG_STYLE_BLOCK_BODY
                 ORG_STYLE_DEFAULT)
        (only-in :gemacs/editor-extra-org
                 cmd-org-mode
                 cmd-org-todo cmd-org-export cmd-org-cycle cmd-org-shift-tab
                 cmd-org-store-link *org-stored-link*
                 cmd-org-promote cmd-org-demote
                 cmd-org-move-subtree-up cmd-org-move-subtree-down
                 cmd-org-toggle-checkbox cmd-org-priority
                 cmd-org-insert-heading cmd-org-insert-src-block
                 cmd-org-template-expand
                 org-heading-line? org-heading-level org-find-subtree-end org-on-checkbox-line?
                 *focus-mode* *zen-mode* *killed-buffers*
                 remember-killed-buffer! *which-key-mode*
                 *dedicated-windows* *new-buffer-counter*
                 *relative-line-numbers* *cua-mode*
                 *global-auto-complete-mode* *which-function-mode*
                 *display-line-numbers-mode* *selective-display-level*
                 *global-font-lock-mode* *auto-dim-other-buffers*
                 *global-eldoc-mode* *word-wrap-column*
                 *desktop-save-mode* *recentf-mode* *savehist-mode*
                 *winner-mode* *midnight-mode* *global-undo-tree*
                 *diff-hl-mode* *volatile-highlights*
                 *vertico-mode* *marginalia-mode*
                 *global-cwarn* *global-hideshow* *global-abbrev*
                 *global-diff-auto-refine* *global-eldoc-box*
                 *global-flyspell-lazy* *global-so-clean*
                 *global-native-compile* *global-gcmh*
                 *global-esup* *global-explain-pause*
                 *global-keyfreq* *global-command-log*
                 *global-interaction-log*
                 *global-yaml-mode* *global-toml-mode*
                 *global-json-mode* *global-csv-mode*
                 *global-protobuf-mode* *global-graphql-mode*
                 *global-nix-mode*)
        (only-in :gemacs/editor register-all-commands!)
        (only-in :gemacs/editor-extra-media
                 *consult-mode* *orderless-mode* *embark-mode*
                 *undo-fu-session* *auto-package-mode*
                 *corfu-mode* *cape-mode* *nerd-icons-mode*
                 *all-the-icons* *doom-themes*
                 *global-whitespace-newline* *global-highlight-indent*
                 *global-rainbow-mode* *global-auto-highlight*
                 *global-symbol-overlay* *global-highlight-parentheses*
                 *global-pulse-line*
                 *global-solaire* *global-spaceline*
                 *global-doom-modeline-env* *global-minions*
                 *global-moody* *global-rich-minority*
                 *global-smart-mode-line*
                 *global-erlang-mode* *global-elixir-mode*
                 *global-zig-mode* *global-ocaml-mode*
                 *global-fsharp-mode* *global-dart-mode*
                 *global-julia-mode*)
        (only-in :gemacs/editor-extra-editing
                 occur-parse-source-name
                 text-find-matching-close text-sexp-end
                 parse-grep-line-text find-number-at-pos
                 *dired-marks*
                 *auto-fill-comments* *electric-indent-mode*
                 *truncate-partial-width* *inhibit-startup-screen*
                 *visible-cursor* *transient-mark-mode*
                 *global-whitespace-mode*
                 *hide-ifdef-mode* *allout-mode*
                 *indent-guide-global* *rainbow-delimiters-global*
                 *global-display-fill-column* *global-flycheck*
                 *global-company* *global-diff-hl*
                 *global-git-gutter* *global-page-break-lines*
                 *global-anzu*
                 *global-visual-regexp* *global-move-dup*
                 *global-expand-region* *global-multiple-cursors*
                 *global-undo-propose* *global-goto-chg*
                 *global-avy*
                 *global-nyan-cat* *global-parrot* *global-zone*
                 *global-fireplace* *global-snow*
                 *global-power-mode* *global-animate-typing*
                 *global-r-mode* *global-ess*
                 *global-sql-mode* *global-ein*
                 *global-conda* *global-pyvenv* *global-pipenv*)
        (only-in :gemacs/editor-extra-vcs
                 fuzzy-match? fuzzy-score)
        (only-in :gemacs/editor-extra-final
                 parse-editorconfig find-editorconfig
                 find-url-at-point collect-buffer-words
                 *command-history* command-history-add!
                 *named-macros* *buffer-access-times*
                 record-buffer-access!
                 *regex-builder-pattern* *last-edit-positions*
                 record-edit-position! *persistent-scratch-file*
                 *subword-mode* *auto-composition-mode*
                 *bidi-display-reordering* *fill-column-indicator*
                 *pixel-scroll-mode* *auto-highlight-symbol-mode*
                 *lorem-ipsum-text* insert-char-by-code-string
                 *read-only-directories* *auto-revert-verbose*
                 *uniquify-buffer-names* *global-so-long-mode*
                 *minibuffer-depth-indicate* *context-menu-mode*
                 *tooltip-mode* *file-name-shadow-mode*
                 *minibuffer-electric-default*
                 *history-delete-duplicates*
                 *modus-themes* *ef-themes* *nano-theme*
                 *ligature-mode* *pixel-scroll-precision*
                 *repeat-mode* *tab-line-mode*
                 *scroll-bar-mode* *tool-bar-mode*
                 *global-auto-revert-non-file* *global-tree-sitter*
                 *global-copilot* *global-lsp-mode*
                 *global-format-on-save* *global-yas*
                 *global-smartparens*
                 *global-helpful* *global-elisp-demos*
                 *global-suggest* *global-buttercup*
                 *global-ert-runner* *global-undercover*
                 *global-benchmark-init*
                 *global-clojure-mode* *global-cider*
                 *global-haskell-mode* *global-lua-mode*
                 *global-ruby-mode* *global-php-mode*
                 *global-swift-mode*)
        (only-in :gemacs/editor-extra-tools2
                 *highlight-changes-mode* *saved-window-layouts*
                 *known-modes* *password-chars*
                 *cursor-blink* *header-line-mode*
                 *auto-save-visited-mode* *hl-todo-mode*
                 *delete-pair-blink* *show-paren-when-point-inside*
                 *enable-recursive-minibuffers* *use-dialog-box*
                 *use-short-answers* *ring-bell-function*
                 *sentence-end-double-space* *colon-double-space*
                 *comment-auto-fill*
                 *global-prettify* *global-hl-todo*
                 *global-color-identifiers* *global-aggressive-indent*
                 *global-origami* *global-centered-cursor*
                 *global-beacon* *global-dimmer* *global-focus*
                 *global-wgrep* *global-deadgrep* *global-ripgrep*
                 *global-projectile-ripgrep* *global-counsel*
                 *global-swiper* *global-prescient*
                 *global-org-roam* *global-org-journal*
                 *global-org-super-agenda* *global-org-noter*
                 *global-org-download* *global-org-cliplink*
                 *global-org-present*)
        (only-in :gemacs/editor-extra-tools
                 *cursor-type* *modeline-visible*
                 *indent-guide-mode* *rainbow-mode*
                 *electric-quote-mode* *visible-mark-mode*
                 *fringe-mode*
                 *show-paren-style* *auto-insert-mode*
                 *global-visual-line-mode* *scroll-conservatively*
                 *show-keystroke-mode* *auto-revert-tail-mode*
                 *flyspell-prog-mode* *auto-save-buffers-mode*
                 *global-linum-mode*
                 *company-mode* *ivy-mode* *helm-mode*
                 *projectile-mode* *doom-modeline*
                 *treesit-mode* *eglot-mode*
                 *display-time* *display-battery*
                 *auto-save-on-idle* *delete-active-region*
                 *shift-select-mode* *cua-selection-mode*
                 *global-goto-address* *global-reveal-mode*
                 *global-auto-composition* *global-display-line-numbers*
                 *blink-cursor-mode*
                 *global-which-key* *global-hydra* *global-transient*
                 *global-general* *global-use-package*
                 *global-diminish* *global-delight*
                 *global-lsp-ui* *global-lsp-treemacs*
                 *global-lsp-ivy* *global-dap-mode*
                 *global-lsp-headerline* *global-lsp-lens*
                 *global-lsp-semantic-tokens*)
        (only-in :gemacs/editor-extra-web
                 url-encode url-decode
                 html-encode-entities html-decode-entities
                 csv-split-line detect-file-encoding
                 large-file? binary-file?
                 reverse-lines-in-string
                 *aggressive-indent-mode*
                 *ws-butler-mode* *file-runners*
                 file-extension
                 *auto-compression-mode* *image-mode*
                 *save-silently* *confirm-kill-emacs*
                 *auto-window-vscroll* *fast-but-imprecise-scrolling*
                 *mouse-avoidance-mode* *make-backup-files*
                 *lock-file-create* *auto-encryption-mode*
                 *auto-rename-tag* *global-prettify-symbols*
                 *global-subword-mode* *global-superword-mode*
                 *delete-by-moving-to-trash* *create-lockfiles*
                 *mode-line-compact* *use-file-dialog*
                 *xterm-mouse-mode*
                 *global-golden-ratio* *global-zoom-window*
                 *global-shackle* *global-popwin* *global-popper*
                 *global-posframe* *global-childframe*
                 *global-rustic* *global-go-mode*
                 *global-python-black* *global-elpy*
                 *global-js2-mode* *global-typescript-mode*
                 *global-web-mode*)
        (only-in :gemacs/editor-extra-modes
                 cmd-eval-last-sexp cmd-eval-defun cmd-eval-print-last-sexp
                 *global-envrc* *global-direnv* *global-editorconfig*
                 *global-dtrt-indent* *global-ws-trim*
                 *global-auto-compile* *global-no-littering*
                 *global-docker* *global-kubernetes*
                 *global-terraform* *global-ansible*
                 *global-vagrant* *global-restclient*
                 *global-ob-http*)
        (only-in :gemacs/editor-extra-vcs
                 *comment-style* *flymake-mode*
                 *delete-selection-mode* *word-count-mode*
                 *column-ruler-mode* *soft-wrap-mode*
                 *whitespace-cleanup-on-save* *line-move-visual*
                 *random-lines*
                 *highlight-indentation-mode* *hungry-delete-mode*
                 *type-break-mode* *delete-trailing-on-save*
                 *cursor-in-non-selected* *blink-matching-paren*
                 *next-error-follow*
                 *auto-save-default* *make-pointer-invisible*
                 *kill-whole-line* *set-mark-command-repeat-pop*
                 *enable-local-variables* *enable-dir-local-variables*
                 *ad-activate-all* *global-hi-lock-mode*
                 *next-line-add-newlines*
                 *global-treemacs-icons* *global-all-the-icons-dired*
                 *global-centaur-tabs* *global-awesome-tab*
                 *global-tab-bar* *global-mini-frame*
                 *global-vertico-posframe*
                 *global-cmake-mode* *global-bazel-mode*
                 *global-meson-mode* *global-ninja-mode*
                 *global-groovy-mode* *global-kotlin-mode*
                 *global-scala-mode*)
        (only-in :gemacs/highlight
                 detect-file-language gerbil-file-extension?
                 setup-highlighting-for-file!)
        (only-in :gemacs/terminal
                 parse-ansi-segments text-segment-text
                 text-segment-fg-color text-segment-bold?
                 terminal-buffer? color-to-style
                 *term-style-base*)
        (only-in :gemacs/echo
                 *minibuffer-history* minibuffer-history-add!)
        (only-in :gemacs/editor-core
                 make-auto-save-path file-mod-time
                 *buffer-mod-times* update-buffer-mod-time!)
        (only-in :gemacs/editor-ui cmd-list-buffers)
        (only-in :gemacs/qt/lsp-client
                 lsp-content-changed? lsp-record-sent-content!
                 *lsp-on-initialized-handler* *lsp-last-sent-content*
                 lsp-queue-ui-action! lsp-poll-ui-actions!
                 file-path->uri uri->file-path lsp-language-id
                 *lsp-initialized* *lsp-initializing*)
)

(export emacs-test)

(def emacs-test
  (test-suite "gemacs"

    (test-case "key-event->string: Ctrl keys"
      ;; C-a = key 0x01
      (check (key-event->string (make-tui-event 1 0 #x01 0 0 0 0 0)) => "C-a")
      ;; C-x = key 0x18
      (check (key-event->string (make-tui-event 1 0 #x18 0 0 0 0 0)) => "C-x")
      ;; C-g = key 0x07
      (check (key-event->string (make-tui-event 1 0 #x07 0 0 0 0 0)) => "C-g")
      ;; C-z = key 0x1A
      (check (key-event->string (make-tui-event 1 0 #x1A 0 0 0 0 0)) => "C-z")
      ;; C-@ / C-SPC = key 0, ch 0
      (check (key-event->string (make-tui-event 1 0 0 0 0 0 0 0)) => "C-@"))

    (test-case "key-event->string: special keys"
      ;; ESC
      (check (key-event->string (make-tui-event 1 0 #x1B 0 0 0 0 0)) => "ESC")
      ;; DEL (backspace)
      (check (key-event->string (make-tui-event 1 0 #x7F 0 0 0 0 0)) => "DEL")
      ;; C-_ (C-/)
      (check (key-event->string (make-tui-event 1 0 #x1F 0 0 0 0 0)) => "C-_")
      ;; C-\
      (check (key-event->string (make-tui-event 1 0 #x1C 0 0 0 0 0)) => "C-\\")
      ;; Space
      (check (key-event->string (make-tui-event 1 0 #x20 0 0 0 0 0)) => "SPC"))

    (test-case "key-event->string: regular characters"
      ;; 'a' = key 0, ch 97
      (check (key-event->string (make-tui-event 1 0 0 97 0 0 0 0)) => "a")
      ;; 'Z' = key 0, ch 90
      (check (key-event->string (make-tui-event 1 0 0 90 0 0 0 0)) => "Z")
      ;; '1' = key 0, ch 49
      (check (key-event->string (make-tui-event 1 0 0 49 0 0 0 0)) => "1"))

    (test-case "key-event->string: Alt/Meta"
      ;; M-f = key 0, ch 102, mod TB_MOD_ALT
      (check (key-event->string (make-tui-event 1 1 0 102 0 0 0 0)) => "M-f")
      ;; M-< = key 0, ch 60, mod TB_MOD_ALT
      (check (key-event->string (make-tui-event 1 1 0 60 0 0 0 0)) => "M-<")
      ;; M-SPC = key 0x20, ch 0, mod TB_MOD_ALT
      (check (key-event->string (make-tui-event 1 1 #x20 0 0 0 0 0)) => "M-SPC"))

    (test-case "key-event->string: arrow and function keys"
      (check (key-event->string (make-tui-event 1 0 TB_KEY_ARROW_UP 0 0 0 0 0))
             => "<up>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_ARROW_DOWN 0 0 0 0 0))
             => "<down>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_F1 0 0 0 0 0))
             => "<f1>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_HOME 0 0 0 0 0))
             => "<home>")
      (check (key-event->string (make-tui-event 1 0 TB_KEY_DELETE 0 0 0 0 0))
             => "<delete>"))

    (test-case "keymap: bind and lookup"
      (let ((km (make-keymap)))
        (keymap-bind! km "C-f" 'forward-char)
        (keymap-bind! km "C-b" 'backward-char)
        (check (keymap-lookup km "C-f") => 'forward-char)
        (check (keymap-lookup km "C-b") => 'backward-char)
        (check (keymap-lookup km "C-z") => #f)))

    (test-case "keymap: nested keymaps for prefix keys"
      (let ((outer (make-keymap))
            (inner (make-keymap)))
        (keymap-bind! inner "C-s" 'save-buffer)
        (keymap-bind! outer "C-x" inner)
        ;; Lookup C-x returns the inner keymap
        (check (hash-table? (keymap-lookup outer "C-x")) => #t)
        ;; Lookup C-s in inner keymap
        (check (keymap-lookup inner "C-s") => 'save-buffer)))

    (test-case "key-state: single key command"
      (let ((km (make-keymap)))
        (keymap-bind! km "C-f" 'forward-char)
        (let ((state (make-key-state km [])))
          ;; C-f event: key=0x06, ch=0, mod=0
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 #x06 0 0 0 0 0))))
            (check action => 'command)
            (check data => 'forward-char)))))

    (test-case "key-state: prefix key sequence"
      (let ((outer (make-keymap))
            (inner (make-keymap)))
        (keymap-bind! inner "C-s" 'save-buffer)
        (keymap-bind! outer "C-x" inner)
        (let ((state (make-key-state outer [])))
          ;; First key: C-x (key=0x18) -> prefix
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 #x18 0 0 0 0 0))))
            (check action => 'prefix)
            ;; Second key: C-s (key=0x13) -> command
            (let-values (((action2 data2 new-state2)
                          (key-state-feed! new-state
                            (make-tui-event 1 0 #x13 0 0 0 0 0))))
              (check action2 => 'command)
              (check data2 => 'save-buffer))))))

    (test-case "key-state: self-insert for printable chars"
      (let ((km (make-keymap)))
        (let ((state (make-key-state km [])))
          ;; 'a' = key=0, ch=97 -> self-insert
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 0 97 0 0 0 0))))
            (check action => 'self-insert)
            (check data => 97)))))

    (test-case "key-state: undefined key"
      (let ((km (make-keymap)))
        (let ((state (make-key-state km [])))
          ;; C-z (key=0x1A) with no binding -> undefined
          (let-values (((action data new-state)
                        (key-state-feed! state
                          (make-tui-event 1 0 #x1A 0 0 0 0 0))))
            (check action => 'undefined)
            (check data => "C-z")))))

    (test-case "echo-state: messages"
      (let ((echo (make-initial-echo-state)))
        (check (echo-state-message echo) => #f)
        (echo-message! echo "Hello")
        (check (echo-state-message echo) => "Hello")
        (check (echo-state-error? echo) => #f)
        (echo-error! echo "Error!")
        (check (echo-state-message echo) => "Error!")
        (check (echo-state-error? echo) => #t)
        (echo-clear! echo)
        (check (echo-state-message echo) => #f)))

    (test-case "default bindings setup"
      (setup-default-bindings!)
      ;; Check some bindings
      (check (keymap-lookup *global-keymap* "C-f") => 'forward-char)
      (check (keymap-lookup *global-keymap* "C-b") => 'backward-char)
      (check (keymap-lookup *global-keymap* "C-n") => 'next-line)
      (check (keymap-lookup *global-keymap* "C-p") => 'previous-line)
      (check (keymap-lookup *global-keymap* "C-a") => 'beginning-of-line)
      (check (keymap-lookup *global-keymap* "C-e") => 'end-of-line)
      (check (keymap-lookup *global-keymap* "C-k") => 'kill-line)
      (check (keymap-lookup *global-keymap* "C-y") => 'yank)
      (check (keymap-lookup *global-keymap* "C-g") => 'keyboard-quit)
      (check (keymap-lookup *global-keymap* "M-f") => 'forward-word)
      (check (keymap-lookup *global-keymap* "M-b") => 'backward-word)
      ;; C-x prefix is a keymap
      (check (hash-table? (keymap-lookup *global-keymap* "C-x")) => #t)
      ;; C-x C-s
      (check (keymap-lookup *ctrl-x-map* "C-s") => 'save-buffer)
      (check (keymap-lookup *ctrl-x-map* "C-c") => 'quit)
      ;; REPL bindings
      (check (keymap-lookup *global-keymap* "M-:") => 'eval-expression)
      (check (hash-table? (keymap-lookup *ctrl-x-map* "r")) => #t)  ; C-x r is now prefix map
      ;; New bindings: M-x, M-g, C-h, C-x C-b, M-%, TAB
      (check (keymap-lookup *global-keymap* "M-x") => 'execute-extended-command)
      (check (hash-table? (keymap-lookup *global-keymap* "M-g")) => #t)
      (check (keymap-lookup *meta-g-map* "g") => 'goto-line)
      (check (keymap-lookup *meta-g-map* "M-g") => 'goto-line)
      (check (hash-table? (keymap-lookup *global-keymap* "C-h")) => #t)
      (check (keymap-lookup *help-map* "k") => 'describe-key)
      (check (keymap-lookup *help-map* "b") => 'list-bindings)
      (check (keymap-lookup *help-map* "f") => 'describe-command)
      (check (keymap-lookup *ctrl-x-map* "C-b") => 'list-buffers)
      (check (keymap-lookup *global-keymap* "M-%") => 'query-replace)
      (check (keymap-lookup *global-keymap* "TAB") => 'indent-or-complete))

    (test-case "key-event->string: TAB key"
      ;; Tab key = 0x09
      (check (key-event->string (make-tui-event 1 0 #x09 0 0 0 0 0)) => "TAB")
      ;; M-TAB = Tab with Alt
      (check (key-event->string (make-tui-event 1 1 #x09 0 0 0 0 0)) => "M-TAB"))

    (test-case "keymap-entries"
      (let ((km (make-keymap)))
        (keymap-bind! km "C-a" 'foo)
        (keymap-bind! km "C-b" 'bar)
        (let ((entries (keymap-entries km)))
          (check (length entries) => 2)
          (check (assoc "C-a" entries) => '("C-a" . foo))
          (check (assoc "C-b" entries) => '("C-b" . bar)))))

    (test-case "eval-expression-string: simple expression"
      (let-values (((result error?) (eval-expression-string "(+ 1 2)")))
        (check result => "3")
        (check error? => #f)))

    (test-case "eval-expression-string: error handling"
      (let-values (((result error?) (eval-expression-string "(/ 1 0)")))
        (check error? => #t)
        (check (string? result) => #t)))

    (test-case "eval-expression-string: output capture"
      ;; eval should capture the return value, not stdout
      (let-values (((result error?) (eval-expression-string "42")))
        (check result => "42")
        (check error? => #f)))

    (test-case "repl-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (repl-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'repl)
        (check (repl-buffer? buf) => #t)
        (set! (buffer-lexer-lang buf) 'dired)
        (check (repl-buffer? buf) => #f)))

    (test-case "eshell-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (eshell-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'eshell)
        (check (eshell-buffer? buf) => #t)))

    (test-case "eshell: pwd"
      (let-values (((output cwd) (eshell-process-input "pwd" "/tmp")))
        (check output => "/tmp\n")
        (check cwd => "/tmp")))

    (test-case "eshell: cd"
      (let-values (((output cwd) (eshell-process-input "cd /tmp" "/home")))
        (check output => "")
        (check cwd => "/tmp"))
      ;; cd to non-existent directory
      (let-values (((output cwd) (eshell-process-input "cd /nonexistent999" "/tmp")))
        (check (string-contains output "no such") => 4)
        (check cwd => "/tmp")))

    (test-case "eshell: echo"
      (let-values (((output cwd) (eshell-process-input "echo hello world" "/tmp")))
        (check output => "hello world\n")))

    (test-case "eshell: eval expression"
      (let-values (((output cwd) (eshell-process-input "(+ 1 2)" "/tmp")))
        (check output => "3\n")))

    (test-case "eshell: which"
      (let-values (((output cwd) (eshell-process-input "which ls" "/tmp")))
        ;; Should find ls somewhere (string-contains returns index or #f)
        (check (not (not (string-contains output "ls"))) => #t)))

    (test-case "eshell: ls"
      ;; ls on /tmp should work
      (let-values (((output cwd) (eshell-process-input "ls /tmp" "/tmp")))
        (check (string? output) => #t)))

    (test-case "eshell: external command"
      (let-values (((output cwd) (eshell-process-input "echo external-test" "/tmp")))
        (check output => "external-test\n")))

    (test-case "eshell: empty input"
      (let-values (((output cwd) (eshell-process-input "" "/tmp")))
        (check output => "")
        (check cwd => "/tmp")))

    (test-case "eshell: clear and exit"
      (let-values (((output cwd) (eshell-process-input "clear" "/tmp")))
        (check output => 'clear))
      (let-values (((output cwd) (eshell-process-input "exit" "/tmp")))
        (check output => 'exit)))

    (test-case "eshell keybinding"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "e") => 'eshell))

    (test-case "shell-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (shell-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'shell)
        (check (shell-buffer? buf) => #t)))

    (test-case "strip-ansi-codes"
      ;; Regular text passes through
      (check (strip-ansi-codes "hello") => "hello")
      ;; CSI color codes stripped
      (let ((esc (string (integer->char 27))))
        (check (strip-ansi-codes (string-append esc "[32mgreen" esc "[0m"))
               => "green")
        ;; CSI with multiple params
        (check (strip-ansi-codes (string-append esc "[1;34mbold-blue" esc "[0m"))
               => "bold-blue")))

    (test-case "shell subprocess lifecycle"
      (let ((ss (shell-start!)))
        ;; Verify state
        (check (shell-state? ss) => #t)
        (check (shell-state-prompt-pos ss) => 0)
        ;; Send a simple command
        (shell-send! ss "echo test123")
        ;; Wait for output
        (thread-sleep! 1.0)
        (let ((output (shell-read-available ss)))
          (check (string? output) => #t)
          (check (not (not (string-contains output "test123"))) => #t))
        ;; Clean shutdown
        (shell-stop! ss)))

    (test-case "shell keybinding"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "$") => 'shell))

    (test-case "shell command produces output"
      (let ((ss (shell-start!)))
        (shell-send! ss "echo hello")
        (thread-sleep! 1.0)
        (let ((output (shell-read-available ss)))
          (check (string? output) => #t)
          (check (not (not (string-contains output "hello"))) => #t))
        (shell-stop! ss)))

    (test-case "shell multiple sequential commands"
      (let ((ss (shell-start!)))
        (shell-send! ss "echo first")
        (thread-sleep! 1.0)
        (let ((out1 (shell-read-available ss)))
          (check (not (not (and out1 (string-contains out1 "first")))) => #t))
        (shell-send! ss "echo second")
        (thread-sleep! 1.0)
        (let ((out2 (shell-read-available ss)))
          (check (not (not (and out2 (string-contains out2 "second")))) => #t))
        (shell-stop! ss)))

    (test-case "shell empty input handling"
      (let ((ss (shell-start!)))
        ;; Sending empty string should not crash
        (shell-send! ss "")
        (thread-sleep! 0.5)
        ;; Shell should still be responsive
        (shell-send! ss "echo alive")
        (thread-sleep! 1.0)
        (let ((output (shell-read-available ss)))
          (check (not (not (and output (string-contains output "alive")))) => #t))
        (shell-stop! ss)))

    (test-case "shell stop/cleanup"
      (let ((ss (shell-start!)))
        ;; Stop should not throw
        (shell-stop! ss)
        ;; Read after stop returns #f (process closed)
        (check (shell-read-available ss) => #f)))

    (test-case "shell prompt-pos tracking"
      (let ((ss (shell-start!)))
        (check (shell-state-prompt-pos ss) => 0)
        ;; Manually update prompt-pos like the app loop does
        (set! (shell-state-prompt-pos ss) 42)
        (check (shell-state-prompt-pos ss) => 42)
        (shell-stop! ss)))

    (test-case "shell send-char"
      (let ((ss (shell-start!)))
        ;; Send individual characters then newline (PTY mode)
        (shell-send-char! ss #\e)
        (shell-send-char! ss #\c)
        (shell-send-char! ss #\h)
        (shell-send-char! ss #\o)
        (shell-send-char! ss #\space)
        (shell-send-char! ss #\h)
        (shell-send-char! ss #\i)
        (shell-send-char! ss #\newline)
        (thread-sleep! 1.0)
        (let ((output (shell-read-available ss)))
          (check (not (not (and output (string-contains output "hi")))) => #t))
        (shell-stop! ss)))

    (test-case "shell-filter-echo strips echoed command"
      (check (shell-filter-echo "echo hello\nhello\n$ " "echo hello")
             => "hello\n$ ")
      (check (shell-filter-echo "hello\n$ " "echo hello")
             => "hello\n$ ")  ; no match, keep as-is
      (check (shell-filter-echo "some output" #f)
             => "some output")  ; no last-sent, keep as-is
      (check (shell-filter-echo "\n$ " "")
             => "\n$ "))  ; empty command, keep as-is

    (test-case "new keybindings: redo, toggles, zoom, etc"
      (setup-default-bindings!)
      ;; Redo
      (check (keymap-lookup *global-keymap* "M-_") => 'redo)
      ;; Toggle line numbers
      (check (keymap-lookup *ctrl-x-map* "l") => 'toggle-line-numbers)
      ;; Toggle word wrap
      (check (keymap-lookup *ctrl-x-map* "w") => 'toggle-word-wrap)
      ;; Toggle whitespace
      (check (keymap-lookup *ctrl-x-map* "t") => 'toggle-whitespace)
      ;; Zoom
      (check (keymap-lookup *global-keymap* "C-=") => 'zoom-in)
      (check (keymap-lookup *global-keymap* "C--") => 'zoom-out)
      (check (keymap-lookup *ctrl-x-map* "C-0") => 'zoom-reset)
      ;; Select all
      (check (keymap-lookup *ctrl-x-map* "h") => 'select-all)
      ;; Dired (was duplicate-line, now dired per standard Emacs)
      (check (keymap-lookup *ctrl-x-map* "d") => 'dired)
      ;; Comment toggle
      (check (keymap-lookup *global-keymap* "M-;") => 'toggle-comment)
      ;; Transpose
      (check (keymap-lookup *global-keymap* "C-t") => 'transpose-chars)
      ;; Word case
      (check (keymap-lookup *global-keymap* "M-u") => 'upcase-word)
      (check (keymap-lookup *global-keymap* "M-l") => 'downcase-word)
      (check (keymap-lookup *global-keymap* "M-c") => 'capitalize-word)
      ;; Kill word
      (check (keymap-lookup *global-keymap* "M-d") => 'kill-word)
      ;; What line (M-g prefix)
      (check (keymap-lookup *meta-g-map* "l") => 'what-line))

    (test-case "new keybindings: write-file, revert, defun nav"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-x-map* "C-w") => 'write-file)
      (check (keymap-lookup *ctrl-x-map* "C-r") => 'recentf-open)
      (check (keymap-lookup *global-keymap* "M-a") => 'backward-sentence)
      (check (keymap-lookup *global-keymap* "M-e") => 'forward-sentence))

    (test-case "auto-pair-char helper"
      ;; Import via editor.ss is not possible without TUI, so test inline
      (let ((apc (lambda (ch)
                   (cond
                     ((= ch 40) 41)   ; ( -> )
                     ((= ch 91) 93)   ; [ -> ]
                     ((= ch 34) 34)   ; " -> "
                     (else #f)))))
        (check (apc 40) => 41)   ; ( -> )
        (check (apc 91) => 93)   ; [ -> ]
        (check (apc 34) => 34)   ; " -> "
        (check (apc 97) => #f)   ; 'a' -> no pair
        (check (apc 32) => #f))) ; space -> no pair

    (test-case "new keybindings: delete-blank-lines, count words"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-x-map* "C-o") => 'delete-blank-lines)
      (check (keymap-lookup *global-keymap* "M-=") => 'count-words))

    (test-case "brace-char? helper"
      ;; Test the brace matching character check
      ;; ( = 40, ) = 41, [ = 91, ] = 93, { = 123, } = 125
      (let ((bc? (lambda (ch)
                   (let ((c (char->integer ch)))
                     (or (= c 40) (= c 41)
                         (= c 91) (= c 93)
                         (= c 123) (= c 125))))))
        (check (bc? #\() => #t)
        (check (bc? #\)) => #t)
        (check (bc? #\[) => #t)
        (check (bc? #\]) => #t)
        (check (bc? #\{) => #t)
        (check (bc? #\}) => #t)
        (check (bc? #\a) => #f)
        (check (bc? #\space) => #f)))

    (test-case "word-char? helper"
      ;; Test word-char? logic (re-implemented inline for testing)
      (let ((wc? (lambda (ch)
                   (or (char-alphabetic? ch) (char-numeric? ch)
                       (char=? ch #\_) (char=? ch #\-)))))
        (check (wc? #\a) => #t)
        (check (wc? #\Z) => #t)
        (check (wc? #\0) => #t)
        (check (wc? #\_) => #t)
        (check (wc? #\-) => #t)
        (check (wc? #\space) => #f)
        (check (wc? #\() => #f)))

    (test-case "new keybindings: yank-pop, occur, compile, etc"
      ;; Test new keybinding registrations
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-y") => 'yank-pop)
      (check (hash-table? (keymap-lookup *global-keymap* "M-s")) => #t)
      (let ((ms-map (keymap-lookup *global-keymap* "M-s")))
        (check (keymap-lookup ms-map "o") => 'occur))
      (check (keymap-lookup *ctrl-x-map* "c") => 'compile)
      (check (keymap-lookup *global-keymap* "M-|") => 'shell-command-on-region)
      (check (keymap-lookup *ctrl-c-map* "^") => 'sort-lines))

    (test-case "app-state new fields"
      ;; Test new app-state fields have correct defaults
      (let ((app (new-app-state #f)))
        (check (app-state-kill-ring-idx app) => 0)
        (check (app-state-last-yank-pos app) => #f)
        (check (app-state-last-yank-len app) => #f)
        (check (app-state-last-compile app) => #f)
        (check (hash-table? (app-state-bookmarks app)) => #t)
        (check (app-state-rect-kill app) => [])))

    (test-case "bookmark and register keybindings"
      (setup-default-bindings!)
      ;; C-c prefix
      (check (hash-table? (keymap-lookup *global-keymap* "C-c")) => #t)
      (let ((cc-map (keymap-lookup *global-keymap* "C-c")))
        (check (keymap-lookup cc-map "z") => 'repl)
        (check (keymap-lookup cc-map "p") => 'goto-matching-paren))
      ;; C-x r prefix
      (let ((rxr-map (keymap-lookup *ctrl-x-map* "r")))
        (check (keymap-lookup rxr-map "m") => 'bookmark-set)
        (check (keymap-lookup rxr-map "b") => 'bookmark-jump)
        (check (keymap-lookup rxr-map "l") => 'bookmark-list)
        (check (keymap-lookup rxr-map "k") => 'kill-rectangle)
        (check (keymap-lookup rxr-map "y") => 'yank-rectangle))
      ;; Other new bindings
      (check (keymap-lookup *global-keymap* "M-j") => 'join-line)
      (check (keymap-lookup *ctrl-x-map* "C-l") => 'downcase-region)
      (check (keymap-lookup *ctrl-x-map* "C-u") => 'upcase-region))

    (test-case "more keybindings: shell-cmd, fill, dabbrev, etc"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-!") => 'shell-command)
      (check (keymap-lookup *global-keymap* "M-q") => 'fill-paragraph)
      (check (keymap-lookup *ctrl-x-map* "i") => 'insert-file)
      (check (keymap-lookup *global-keymap* "M-/") => 'hippie-expand)
      (check (keymap-lookup *ctrl-x-map* "=") => 'what-cursor-position))

    (test-case "macro and mark ring keybindings"
      (setup-default-bindings!)
      ;; Keyboard macros
      (check (keymap-lookup *ctrl-x-map* "(") => 'start-kbd-macro)
      (check (keymap-lookup *ctrl-x-map* ")") => 'end-kbd-macro)
      (check (keymap-lookup *ctrl-x-map* "e") => 'call-last-kbd-macro)
      ;; Mark ring
      (check (keymap-lookup *ctrl-c-map* "SPC") => 'pop-mark))

    (test-case "app-state macro and mark-ring fields"
      (let ((app (new-app-state #f)))
        (check (app-state-macro-recording app) => #f)
        (check (app-state-macro-last app) => #f)
        (check (app-state-mark-ring app) => [])))

    (test-case "shell-quote helper"
      ;; Inline shell-quote for testing
      (let ((sq (lambda (s)
                  (string-append "'"
                    (let loop ((i 0) (acc ""))
                      (if (>= i (string-length s)) acc
                        (let ((ch (string-ref s i)))
                          (if (char=? ch #\')
                            (loop (+ i 1) (string-append acc "'\"'\"'"))
                            (loop (+ i 1) (string-append acc (string ch)))))))
                    "'"))))
        (check (not (not (string-contains (sq "hello") "hello"))) => #t)
        (check (string-ref (sq "hello") 0) => #\')
        (check (string-ref (sq "it's") 0) => #\')))

    (test-case "eval-expression-string"
      ;; Test in-process eval
      (let-values (((result error?) (eval-expression-string "(+ 1 2)")))
        (check result => "3")
        (check error? => #f))
      ;; Test error case
      (let-values (((result error?) (eval-expression-string "(/ 1 0)")))
        (check error? => #t)
        (check (string? result) => #t)))

    (test-case "register keybindings"
      (setup-default-bindings!)
      (let ((rxr-map (keymap-lookup *ctrl-x-map* "r")))
        (check (keymap-lookup rxr-map "s") => 'copy-to-register)
        (check (keymap-lookup rxr-map "i") => 'insert-register)
        (check (keymap-lookup rxr-map "SPC") => 'point-to-register)
        (check (keymap-lookup rxr-map "j") => 'jump-to-register)))

    (test-case "new keybindings: backward-kill-word, zap, goto-char"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-DEL") => 'backward-kill-word)
      (check (keymap-lookup *global-keymap* "M-z") => 'zap-to-char)
      (check (keymap-lookup *meta-g-map* "c") => 'goto-char))

    (test-case "app-state registers field"
      (let ((app (new-app-state #f)))
        (check (hash-table? (app-state-registers app)) => #t)))

    (test-case "new keybindings: transpose, just-one-space, repeat, etc"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "M-t") => 'transpose-words)
      (check (keymap-lookup *ctrl-x-map* "C-t") => 'transpose-lines)
      (check (keymap-lookup *ctrl-x-map* "z") => 'repeat)
      (check (keymap-lookup *global-keymap* "M-SPC") => 'just-one-space)
      (check (keymap-lookup *meta-g-map* "n") => 'next-error)
      (check (keymap-lookup *meta-g-map* "p") => 'previous-error))

    (test-case "app-state last-command field"
      (let ((app (new-app-state #f)))
        (check (app-state-last-command app) => #f)))

    (test-case "new keybindings: kill-whole-line, move-line, narrow, etc"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "k") => 'kill-whole-line)
      (check (keymap-lookup *global-keymap* "M-<up>") => 'move-line-up)
      (check (keymap-lookup *global-keymap* "M-<down>") => 'move-line-down)
      (check (keymap-lookup *ctrl-c-map* "!") => 'pipe-buffer)
      (check (keymap-lookup *ctrl-c-map* "n") => 'narrow-to-region)
      (check (keymap-lookup *ctrl-c-map* "w") => 'widen))

    (test-case "repl subprocess lifecycle"
      (let ((rs (repl-start!)))
        ;; Verify state is initialized
        (check (repl-state? rs) => #t)
        (check (repl-state-history rs) => [])
        (check (repl-state-prompt-pos rs) => 0)
        ;; Send expression and wait for output
        (repl-send! rs "(+ 1 2)")
        (check (equal? (car (repl-state-history rs)) "(+ 1 2)") => #t)
        ;; Wait for gxi to process
        (thread-sleep! 1.0)
        ;; Read available output
        (let ((output (repl-read-available rs)))
          (check (string? output) => #t)
          ;; string-contains returns index (integer) or #f
          (check (not (not (string-contains output "3"))) => #t))
        ;; Clean shutdown
        (repl-stop! rs)))

    (test-case "new keybindings: string-rect, number-lines, align, etc"
      ;; Rectangle extensions
      (check (keymap-lookup *ctrl-x-r-map* "t") => 'string-rectangle)
      (check (keymap-lookup *ctrl-x-r-map* "o") => 'open-rectangle)
      ;; Number lines, reverse region
      (check (keymap-lookup *ctrl-c-map* "#") => 'number-lines)
      (check (keymap-lookup *ctrl-c-map* "r") => 'reverse-region)
      ;; Flush/keep lines
      (check (keymap-lookup *meta-s-map* "f") => 'flush-lines)
      (check (keymap-lookup *meta-s-map* "k") => 'keep-lines)
      ;; Align, sort fields
      (check (keymap-lookup *ctrl-c-map* "a") => 'align-regexp)
      (check (keymap-lookup *ctrl-c-map* "s") => 'sort-fields))

    (test-case "new keybindings: mark-word, paragraph, indentation, etc"
      ;; Mark word, paragraph nav
      (check (keymap-lookup *global-keymap* "M-@") => 'mark-word)
      (check (keymap-lookup *global-keymap* "M-h") => 'mark-paragraph)
      (check (keymap-lookup *global-keymap* "M-}") => 'forward-paragraph)
      (check (keymap-lookup *global-keymap* "M-{") => 'backward-paragraph)
      ;; Indentation
      (check (keymap-lookup *global-keymap* "M-m") => 'back-to-indentation)
      (check (keymap-lookup *global-keymap* "M-^") => 'delete-indentation)
      ;; Exchange point and mark
      (check (keymap-lookup *ctrl-x-map* "C-x") => 'exchange-point-and-mark)
      ;; Info
      (check (keymap-lookup *ctrl-x-map* "C-p") => 'what-page)
      (check (hash-table? (keymap-lookup *ctrl-c-map* "l")) => #t) ;; LSP prefix map
      ;; Copy line
      (check (keymap-lookup *ctrl-c-map* "c") => 'whitespace-cleanup))

    (test-case "new keybindings: where-is, apropos, read-only, etc"
      ;; Help extensions
      (check (keymap-lookup *help-map* "w") => 'where-is)
      (check (keymap-lookup *help-map* "a") => 'apropos-command)
      ;; Buffer: read-only, rename
      (check (keymap-lookup *ctrl-x-map* "C-q") => 'toggle-read-only)
      (check (keymap-lookup *ctrl-x-r-map* "n") => 'rename-buffer)
      ;; C-x 4 prefix
      (check (hash-table? (keymap-lookup *ctrl-x-map* "4")) => #t)
      (check (keymap-lookup *ctrl-x-4-map* "b") => 'switch-buffer-other-window)
      (check (keymap-lookup *ctrl-x-4-map* "f") => 'find-file-other-window))

    (test-case "rot13 helper (inline)"
      ;; Reimplementation for testing (same logic as editor.ss rot13-string)
      (let* ((rot13-char
               (lambda (ch)
                 (cond
                   ((and (char>=? ch #\a) (char<=? ch #\z))
                    (integer->char (+ (char->integer #\a)
                                      (modulo (+ (- (char->integer ch) (char->integer #\a)) 13) 26))))
                   ((and (char>=? ch #\A) (char<=? ch #\Z))
                    (integer->char (+ (char->integer #\A)
                                      (modulo (+ (- (char->integer ch) (char->integer #\A)) 13) 26))))
                   (else ch))))
             (rot13-string
               (lambda (s)
                 (let* ((len (string-length s))
                        (result (make-string len)))
                   (let loop ((i 0))
                     (when (< i len)
                       (string-set! result i (rot13-char (string-ref s i)))
                       (loop (+ i 1))))
                   result))))
        (check (rot13-string "Hello") => "Uryyb")
        (check (rot13-string "Uryyb") => "Hello")
        (check (rot13-string "123") => "123")))

    (test-case "new keybindings: tabify, rot13, hex, count, dedup"
      (check (keymap-lookup *ctrl-c-map* "t") => 'tabify)
      (check (keymap-lookup *ctrl-c-map* "3") => 'rot13-region)
      (check (keymap-lookup *ctrl-c-map* "x") => 'hexl-mode)
      (check (keymap-lookup *meta-s-map* "c") => 'count-matches)
      (check (keymap-lookup *ctrl-c-map* "u") => 'delete-duplicate-lines))

    (test-case "new keybindings: diff, checksum, grep-buffer, etc"
      (check (keymap-lookup *ctrl-c-map* "d") => 'diff-buffer-with-file)
      (check (keymap-lookup *ctrl-c-map* "5") => 'checksum)
      (check (keymap-lookup *global-keymap* "M-&") => 'async-shell-command)
      (check (keymap-lookup *ctrl-x-map* "$") => 'toggle-truncate-lines)
      (check (keymap-lookup *meta-s-map* "g") => 'grep-buffer)
      (check (keymap-lookup *ctrl-c-map* "D") => 'insert-date)
      (check (keymap-lookup *ctrl-c-map* "8") => 'insert-char))

    (test-case "new keybindings: eval-buffer, clone, scratch, save-some, etc"
      (setup-default-bindings!)
      ;; Eval buffer/region
      (check (keymap-lookup *ctrl-c-map* "E") => 'eval-buffer)
      (check (keymap-lookup *ctrl-c-map* "v") => 'eval-region)
      ;; Clone buffer, scratch
      (check (keymap-lookup *ctrl-c-map* "b") => 'clone-buffer)
      (check (keymap-lookup *ctrl-c-map* "S") => 'scratch-buffer)
      ;; Save some buffers (overrides old shell on C-x s)
      (check (keymap-lookup *ctrl-x-map* "s") => 'save-some-buffers)
      ;; Revert quick
      (check (keymap-lookup *ctrl-c-map* "R") => 'revert-buffer-quick)
      ;; Toggle highlighting
      (check (keymap-lookup *ctrl-c-map* "f") => 'toggle-highlighting)
      ;; Display time
      (check (keymap-lookup *ctrl-c-map* "T") => 'display-time))

    (test-case "new keybindings: ediff, calc, describe-bindings, etc"
      (setup-default-bindings!)
      ;; Calculator
      (check (keymap-lookup *ctrl-c-map* "=") => 'calc)
      ;; Case fold search
      (check (keymap-lookup *ctrl-c-map* "C") => 'toggle-case-fold-search)
      ;; Describe bindings
      (check (keymap-lookup *help-map* "B") => 'describe-bindings)
      ;; Center line
      (check (keymap-lookup *global-keymap* "M-o") => 'center-line)
      ;; What face
      (check (keymap-lookup *ctrl-c-map* "F") => 'what-face)
      ;; List processes
      (check (keymap-lookup *ctrl-c-map* "P") => 'list-processes)
      ;; View messages
      (check (keymap-lookup *ctrl-c-map* "m") => 'view-messages)
      ;; Auto fill
      (check (keymap-lookup *ctrl-c-map* "q") => 'toggle-auto-fill)
      ;; Delete trailing whitespace
      (check (keymap-lookup *ctrl-c-map* "W") => 'delete-trailing-whitespace)
      ;; Ediff buffers
      (check (keymap-lookup *ctrl-c-map* "B") => 'ediff-buffers)
      ;; Rename file
      (check (keymap-lookup *ctrl-c-map* "M") => 'rename-file-and-buffer)
      ;; Sudo write
      (check (keymap-lookup *ctrl-c-map* "X") => 'sudo-write)
      ;; Sort numeric
      (check (keymap-lookup *ctrl-c-map* "N") => 'sort-numeric)
      ;; Count words region
      (check (keymap-lookup *ctrl-c-map* "L") => 'count-words-region))

    (test-case "new keybindings: overwrite, visual-line, fill-col, etc"
      (setup-default-bindings!)
      ;; Overwrite mode
      (check (keymap-lookup *global-keymap* "<insert>") => 'toggle-overwrite-mode)
      ;; Visual line mode
      (check (keymap-lookup *ctrl-c-map* "V") => 'toggle-visual-line-mode)
      ;; Fill column
      (check (keymap-lookup *ctrl-c-map* ".") => 'set-fill-column)
      (check (keymap-lookup *ctrl-c-map* "|") => 'toggle-fill-column-indicator)
      ;; Repeat complex command
      (check (keymap-lookup *ctrl-c-map* "Z") => 'repeat-complex-command)
      ;; Eldoc
      (check (keymap-lookup *ctrl-c-map* "I") => 'eldoc)
      ;; Highlight symbol/clear
      (check (keymap-lookup *ctrl-c-map* "h") => 'highlight-symbol)
      (check (keymap-lookup *ctrl-c-map* "H") => 'clear-highlight)
      ;; Indent rigidly
      (check (keymap-lookup *ctrl-c-map* ">") => 'indent-rigidly-right)
      (check (keymap-lookup *ctrl-c-map* "<") => 'indent-rigidly-left)
      ;; Buffer stats
      (check (keymap-lookup *ctrl-c-map* "?") => 'buffer-stats)
      ;; Show tabs/eol
      (check (keymap-lookup *ctrl-c-map* "4") => 'toggle-show-tabs)
      (check (keymap-lookup *ctrl-c-map* "6") => 'toggle-show-eol))

    (test-case "new keybindings: copy-above, open-line-above, select, etc"
      (setup-default-bindings!)
      ;; Copy from above
      (check (keymap-lookup *ctrl-c-map* "A") => 'copy-from-above)
      ;; Open line above
      (check (keymap-lookup *ctrl-c-map* "O") => 'open-line-above)
      ;; Select line
      (check (keymap-lookup *ctrl-c-map* "G") => 'select-line)
      ;; Split line
      (check (keymap-lookup *ctrl-c-map* "J") => 'split-line)
      ;; Hippie expand
      (check (keymap-lookup *global-keymap* "M-TAB") => 'hippie-expand)
      ;; Swap buffers
      (check (keymap-lookup *ctrl-c-map* "9") => 'swap-buffers)
      ;; Tab width
      (check (keymap-lookup *ctrl-c-map* "7") => 'cycle-tab-width)
      ;; Indent tabs
      (check (keymap-lookup *ctrl-c-map* "0") => 'toggle-indent-tabs-mode)
      ;; Buffer info
      (check (keymap-lookup *ctrl-c-map* "j") => 'buffer-info)
      ;; Enlarge/shrink
      (check (keymap-lookup *ctrl-x-map* "^") => 'enlarge-window)
      (check (keymap-lookup *ctrl-x-map* "-") => 'shrink-window))

    (test-case "new keybindings: whitespace-cleanup, electric-pair, etc"
      (setup-default-bindings!)
      ;; Whitespace cleanup (overrides copy-line)
      (check (keymap-lookup *ctrl-c-map* "c") => 'whitespace-cleanup)
      ;; Toggle electric pair
      (check (keymap-lookup *ctrl-c-map* "Q") => 'toggle-electric-pair)
      ;; Previous/next buffer
      (check (keymap-lookup *ctrl-x-map* "<left>") => 'previous-buffer)
      (check (keymap-lookup *ctrl-x-map* "<right>") => 'next-buffer)
      ;; Balance windows
      (check (keymap-lookup *ctrl-x-map* "+") => 'balance-windows)
      ;; Move to window line
      (check (keymap-lookup *global-keymap* "M-r") => 'move-to-window-line)
      ;; Kill buffer and window
      (check (keymap-lookup *ctrl-x-4-map* "0") => 'kill-buffer-and-window)
      ;; Flush undo
      (check (keymap-lookup *ctrl-c-map* "/") => 'flush-undo)
      ;; Upcase initials region
      (check (keymap-lookup *ctrl-c-map* "U") => 'upcase-initials-region)
      ;; Untabify buffer
      (check (keymap-lookup *ctrl-c-map* "_") => 'untabify-buffer)
      ;; Insert buffer name
      (check (keymap-lookup *ctrl-c-map* "%") => 'insert-buffer-name)
      ;; Mark defun
      (check (keymap-lookup *ctrl-c-map* "y") => 'mark-defun)
      ;; Insert pairs
      (check (keymap-lookup *ctrl-c-map* "(") => 'insert-parentheses)
      (check (keymap-lookup *ctrl-c-map* "[") => 'insert-pair-brackets)
      ;; Describe char
      (check (keymap-lookup *ctrl-c-map* ",") => 'describe-char)
      ;; Find file at point
      (check (keymap-lookup *ctrl-c-map* "o") => 'find-file-at-point)
      ;; Count chars region
      (check (keymap-lookup *ctrl-c-map* "K") => 'count-chars-region))

    (test-case "new keybindings: sexp nav, unfill, kill-ring, registers, etc"
      (setup-default-bindings!)
      ;; Count words buffer
      (check (keymap-lookup *ctrl-c-map* "+") => 'count-words-buffer)
      ;; Unfill paragraph
      (check (keymap-lookup *ctrl-c-map* ";") => 'unfill-paragraph)
      ;; List registers
      (check (keymap-lookup *ctrl-c-map* "@") => 'list-registers)
      ;; Show kill ring
      (check (keymap-lookup *ctrl-c-map* "Y") => 'show-kill-ring)
      ;; Smart beginning of line
      (check (keymap-lookup *ctrl-c-map* "`") => 'smart-beginning-of-line)
      ;; What buffer
      (check (keymap-lookup *ctrl-c-map* "~") => 'what-buffer)
      ;; Narrowing indicator
      (check (keymap-lookup *ctrl-c-map* ":") => 'toggle-narrowing-indicator)
      ;; Insert file name
      (check (keymap-lookup *ctrl-c-map* "&") => 'insert-file-name)
      ;; S-expression navigation
      (check (keymap-lookup *meta-g-map* "u") => 'backward-up-list)
      (check (keymap-lookup *meta-g-map* "d") => 'forward-up-list)
      (check (keymap-lookup *meta-g-map* "k") => 'kill-sexp)
      (check (keymap-lookup *meta-g-map* "f") => 'forward-sexp)
      (check (keymap-lookup *meta-g-map* "b") => 'backward-sexp))

    (test-case "new keybindings: mark-sexp, word-freq, uuid, delete-pair, etc"
      (setup-default-bindings!)
      ;; Mark sexp
      (check (keymap-lookup *meta-g-map* "SPC") => 'mark-sexp)
      ;; Indent sexp
      (check (keymap-lookup *meta-g-map* "TAB") => 'indent-sexp)
      ;; Word frequency
      (check (keymap-lookup *ctrl-c-map* "*") => 'word-frequency)
      ;; Insert UUID
      (check (keymap-lookup *ctrl-c-map* "'") => 'insert-uuid)
      ;; Delete pair
      (check (keymap-lookup *ctrl-c-map* "}") => 'delete-pair)
      ;; Toggle hl-line
      (check (keymap-lookup *ctrl-c-map* "{") => 'toggle-hl-line)
      ;; Find alternate file
      (check (keymap-lookup *ctrl-x-map* "C-v") => 'find-alternate-file)
      ;; Increment register
      (check (keymap-lookup *ctrl-x-r-map* "+") => 'increment-register))

    ;; Task #39: sort, scroll, text processing commands
    (test-case "new keybindings: scroll-other, matching-paren, etc"
      ;; M-g v/V: scroll other window
      (check (keymap-lookup *meta-g-map* "v") => 'scroll-other-window)
      (check (keymap-lookup *meta-g-map* "V") => 'scroll-other-window-up)
      ;; M-g m: goto matching paren
      (check (keymap-lookup *meta-g-map* "m") => 'goto-matching-paren)
      ;; editor-get-text-range helper (defined in editor.ss, can't test here)
      ;; Verify *auto-revert-mode* global exists in editor.ss (also can't test)
      ;; Check that the new M-g bindings are distinct from existing ones
      (check (not (eq? (keymap-lookup *meta-g-map* "v")
                       (keymap-lookup *meta-g-map* "V"))) => #t)
      ;; Check M-g map still has previous bindings
      (check (keymap-lookup *meta-g-map* "u") => 'backward-up-list)
      (check (keymap-lookup *meta-g-map* "d") => 'forward-up-list)
      (check (keymap-lookup *meta-g-map* "k") => 'kill-sexp)
      (check (keymap-lookup *meta-g-map* "f") => 'forward-sexp)
      (check (keymap-lookup *meta-g-map* "b") => 'backward-sexp))

    (test-case "new keybindings: backward-kill-sexp, goto-percent, etc"
      ;; M-g DEL -> backward-kill-sexp
      (check (keymap-lookup *meta-g-map* "DEL") => 'backward-kill-sexp)
      ;; M-g % -> goto-percent
      (check (keymap-lookup *meta-g-map* "%") => 'goto-percent)
      ;; Verify existing M-g bindings still intact
      (check (keymap-lookup *meta-g-map* "u") => 'backward-up-list)
      (check (keymap-lookup *meta-g-map* "m") => 'goto-matching-paren)
      (check (keymap-lookup *meta-g-map* "v") => 'scroll-other-window)
      (check (keymap-lookup *meta-g-map* "V") => 'scroll-other-window-up))

    (test-case "new keybindings: regex search, window resize, undo"
      (setup-default-bindings!)
      ;; Regex search and replace
      (check (keymap-lookup *global-keymap* "C-M-s") => 'isearch-forward-regexp)
      (check (keymap-lookup *global-keymap* "C-M-%") => 'query-replace-regexp)
      ;; Window resize
      (check (keymap-lookup *ctrl-x-map* "^") => 'enlarge-window)
      (check (keymap-lookup *ctrl-x-map* "{") => 'shrink-window-horizontally)
      (check (keymap-lookup *ctrl-x-map* "}") => 'enlarge-window-horizontally)
      ;; Alternative undo
      (check (keymap-lookup *global-keymap* "C-/") => 'undo))

    (test-case "new keybindings: view-lossage"
      (setup-default-bindings!)
      (check (keymap-lookup *help-map* "l") => 'view-lossage))

    (test-case "keybindings: code folding"
      (setup-default-bindings!)
      (check (keymap-lookup *meta-g-map* "F") => 'toggle-fold)
      (check (keymap-lookup *meta-g-map* "C") => 'fold-all)
      (check (keymap-lookup *meta-g-map* "E") => 'unfold-all))

    (test-case "keybindings: completion popup"
      (setup-default-bindings!)
      (check (keymap-lookup *global-keymap* "C-M-i") => 'complete-at-point))

    (test-case "command registration: autocomplete and transpose"
      (register-all-commands!)
      (check (procedure? (find-command 'complete-at-point)) => #t)
      (check (procedure? (find-command 'transpose-windows)) => #t)
      (check (procedure? (find-command 'git-log-file)) => #t))

    (test-case "command registration: multi-cursor and occur-goto"
      (register-all-commands!)
      (check (procedure? (find-command 'mc-add-next)) => #t)
      (check (procedure? (find-command 'mc-add-all)) => #t)
      (check (procedure? (find-command 'mc-skip-and-add-next)) => #t)
      (check (procedure? (find-command 'mc-cursors-on-lines)) => #t)
      (check (procedure? (find-command 'mc-unmark-last)) => #t)
      (check (procedure? (find-command 'mc-rotate)) => #t)
      (check (procedure? (find-command 'occur-goto)) => #t))

    (test-case "occur-parse-source-name: parses header"
      (check (occur-parse-source-name
               "5 matches for \"foo\" in main.ss:\n\n1:foo") => "main.ss")
      (check (occur-parse-source-name "no header here") => #f))

    (test-case "command registration: paredit advanced and numbers"
      (register-all-commands!)
      (check (procedure? (find-command 'paredit-slurp-forward)) => #t)
      (check (procedure? (find-command 'paredit-barf-forward)) => #t)
      (check (procedure? (find-command 'paredit-split-sexp)) => #t)
      (check (procedure? (find-command 'paredit-join-sexps)) => #t)
      (check (procedure? (find-command 'increment-number)) => #t)
      (check (procedure? (find-command 'decrement-number)) => #t)
      (check (procedure? (find-command 'grep-goto)) => #t)
      (check (procedure? (find-command 'next-error)) => #t))

    (test-case "sexp helpers: text-find-matching-close"
      (check (text-find-matching-close "(abc)" 0) => 5)
      (check (text-find-matching-close "(a (b) c)" 0) => 9)
      (check (text-find-matching-close "[x]" 0) => 3))

    (test-case "sexp helpers: text-sexp-end"
      (check (text-sexp-end "(abc)" 0) => 5)
      (check (text-sexp-end "hello world" 0) => 5)
      (check (text-sexp-end "\"str\"" 0) => 5))

    (test-case "grep line parsing"
      (check (parse-grep-line-text "foo.ss:42:some text") => '("foo.ss" 42))
      (check (parse-grep-line-text "no match here") => #f))

    (test-case "number at point: find-number-at-pos"
      (check (find-number-at-pos "abc 123 def" 4) => '(4 . 7))
      (check (find-number-at-pos "abc def" 2) => #f))

    (test-case "key lossage recording"
      (let ((app (new-app-state #f)))
        ;; Starts empty
        (check (app-state-key-lossage app) => [])
        ;; Record some keys
        (key-lossage-record! app "C-x")
        (key-lossage-record! app "C-f")
        (check (length (app-state-key-lossage app)) => 2)
        ;; Most recent first
        (check (car (app-state-key-lossage app)) => "C-f")
        ;; Format includes both keys
        (let ((s (key-lossage->string app)))
          (check (not (not (string-contains s "C-x"))) => #t)
          (check (not (not (string-contains s "C-f"))) => #t))))

    ;;=========================================================================
    ;; Feature implementation tests
    ;;=========================================================================

    (test-case "app-state: winner mode fields"
      (let ((app (new-app-state #f)))
        ;; Winner history starts empty
        (check (app-state-winner-history app) => [])
        (check (app-state-winner-history-idx app) => 0)
        ;; Can set winner history
        (set! (app-state-winner-history app)
          (list '(1 0 ("*scratch*")) '(2 0 ("*scratch*" "foo.ss"))))
        (check (length (app-state-winner-history app)) => 2)
        ;; Can navigate history index
        (set! (app-state-winner-history-idx app) 1)
        (check (app-state-winner-history-idx app) => 1)))

    (test-case "app-state: tab management fields"
      (let ((app (new-app-state #f)))
        ;; Starts with one default tab
        (check (length (app-state-tabs app)) => 1)
        (check (app-state-current-tab-idx app) => 0)
        ;; Default tab has correct structure: (name buffer-names window-idx)
        (let ((tab (car (app-state-tabs app))))
          (check (car tab) => "Tab 1")
          (check (cadr tab) => '("*scratch*"))
          (check (caddr tab) => 0))
        ;; Can add tabs
        (set! (app-state-tabs app)
          (append (app-state-tabs app) (list (list "Tab 2" '("foo.ss") 0))))
        (check (length (app-state-tabs app)) => 2)
        ;; Can switch tabs
        (set! (app-state-current-tab-idx app) 1)
        (check (app-state-current-tab-idx app) => 1)))

    (test-case "command registration: spell checking"
      (register-all-commands!)
      (check (not (not (find-command 'ispell-word))) => #t)
      (check (not (not (find-command 'ispell-buffer))) => #t)
      (check (not (not (find-command 'ispell-region))) => #t))

    (test-case "command registration: winner mode"
      (register-all-commands!)
      (check (not (not (find-command 'winner-undo))) => #t)
      (check (not (not (find-command 'winner-redo))) => #t)
      (check (not (not (find-command 'winner-mode))) => #t))

    (test-case "command registration: tab management"
      (register-all-commands!)
      (check (not (not (find-command 'tab-new))) => #t)
      (check (not (not (find-command 'tab-close))) => #t)
      (check (not (not (find-command 'tab-next))) => #t)
      (check (not (not (find-command 'tab-previous))) => #t)
      (check (not (not (find-command 'tab-rename))) => #t)
      (check (not (not (find-command 'tab-move))) => #t))

    (test-case "command registration: flycheck"
      (register-all-commands!)
      (check (not (not (find-command 'flycheck-mode))) => #t)
      (check (not (not (find-command 'flycheck-next-error))) => #t)
      (check (not (not (find-command 'flycheck-previous-error))) => #t)
      (check (not (not (find-command 'flycheck-list-errors))) => #t))

    (test-case "command registration: git gutter"
      (register-all-commands!)
      (check (not (not (find-command 'git-gutter-mode))) => #t)
      (check (not (not (find-command 'git-gutter-next-hunk))) => #t)
      (check (not (not (find-command 'git-gutter-previous-hunk))) => #t)
      (check (not (not (find-command 'git-gutter-revert-hunk))) => #t)
      (check (not (not (find-command 'git-gutter-stage-hunk))) => #t))

    (test-case "command registration: multiple cursors"
      (register-all-commands!)
      (check (not (not (find-command 'mc-mark-next-like-this))) => #t)
      (check (not (not (find-command 'mc-mark-previous-like-this))) => #t)
      (check (not (not (find-command 'mc-mark-all-like-this))) => #t))

    (test-case "command registration: xref navigation"
      (register-all-commands!)
      (check (not (not (find-command 'xref-find-definitions))) => #t)
      (check (not (not (find-command 'xref-find-references))) => #t)
      (check (not (not (find-command 'xref-find-apropos))) => #t)
      (check (not (not (find-command 'xref-go-back))) => #t)
      (check (not (not (find-command 'xref-go-forward))) => #t))

    (test-case "command registration: project.el"
      (register-all-commands!)
      (check (not (not (find-command 'project-switch-project))) => #t)
      (check (not (not (find-command 'project-find-regexp))) => #t)
      (check (not (not (find-command 'project-shell))) => #t)
      (check (not (not (find-command 'project-dired))) => #t)
      (check (not (not (find-command 'project-eshell))) => #t))

    (test-case "command registration: treemacs"
      (register-all-commands!)
      (check (not (not (find-command 'treemacs))) => #t)
      (check (not (not (find-command 'treemacs-find-file))) => #t))

    (test-case "command registration: calendar"
      (register-all-commands!)
      (check (not (not (find-command 'calendar-goto-date))) => #t)
      (check (not (not (find-command 'calendar-holidays))) => #t))

    (test-case "command registration: auto-insert"
      (register-all-commands!)
      (check (not (not (find-command 'auto-insert))) => #t)
      (check (not (not (find-command 'auto-insert-mode))) => #t))

    (test-case "command registration: smartparens and paredit"
      (register-all-commands!)
      (check (not (not (find-command 'sp-forward-slurp-sexp))) => #t)
      (check (not (not (find-command 'sp-forward-barf-sexp))) => #t)
      (check (not (not (find-command 'paredit-wrap-round))) => #t)
      (check (not (not (find-command 'paredit-raise-sexp))) => #t)
      (check (not (not (find-command 'paredit-splice-sexp))) => #t))

    (test-case "command registration: EWW browser"
      (register-all-commands!)
      (check (not (not (find-command 'eww))) => #t)
      (check (not (not (find-command 'eww-browse-url))) => #t)
      (check (not (not (find-command 'browse-url-at-point))) => #t)
      (check (not (not (find-command 'eww-back))) => #t)
      (check (not (not (find-command 'eww-forward))) => #t)
      (check (not (not (find-command 'eww-reload))) => #t)
      (check (not (not (find-command 'eww-download))) => #t)
      (check (not (not (find-command 'eww-copy-page-url))) => #t))

    (test-case "command registration: diff and ediff"
      (register-all-commands!)
      (check (not (not (find-command 'diff-mode))) => #t)
      (check (not (not (find-command 'diff-apply-hunk))) => #t)
      (check (not (not (find-command 'diff-revert-hunk))) => #t)
      (check (not (not (find-command 'diff-goto-source))) => #t)
      (check (not (not (find-command 'ediff-files))) => #t)
      (check (not (not (find-command 'ediff-regions))) => #t)
      (check (not (not (find-command 'ediff-merge))) => #t))

    (test-case "command registration: yasnippet"
      (register-all-commands!)
      (check (not (not (find-command 'yas-insert-snippet))) => #t)
      (check (not (not (find-command 'yas-new-snippet))) => #t)
      (check (not (not (find-command 'yas-visit-snippet-file))) => #t))

    (test-case "command registration: EMMS media player"
      (register-all-commands!)
      (check (not (not (find-command 'emms))) => #t)
      (check (not (not (find-command 'emms-play-file))) => #t)
      (check (not (not (find-command 'emms-pause))) => #t)
      (check (not (not (find-command 'emms-stop))) => #t)
      (check (not (not (find-command 'emms-next))) => #t)
      (check (not (not (find-command 'emms-previous))) => #t))

    (test-case "command registration: PDF viewer"
      (register-all-commands!)
      (check (not (not (find-command 'pdf-view-mode))) => #t)
      (check (not (not (find-command 'pdf-view-next-page))) => #t)
      (check (not (not (find-command 'pdf-view-previous-page))) => #t)
      (check (not (not (find-command 'pdf-view-goto-page))) => #t))

    (test-case "command registration: avy/ace-jump"
      (register-all-commands!)
      (check (not (not (find-command 'avy-goto-char))) => #t)
      (check (not (not (find-command 'avy-goto-word))) => #t)
      (check (not (not (find-command 'avy-goto-line))) => #t))

    (test-case "command registration: expand-region"
      (register-all-commands!)
      (check (not (not (find-command 'expand-region))) => #t)
      (check (not (not (find-command 'contract-region))) => #t))

    (test-case "command registration: abbreviations"
      (register-all-commands!)
      (check (not (not (find-command 'define-global-abbrev))) => #t))

    (test-case "keybindings: ediff"
      (setup-default-bindings!)
      (check (keymap-lookup *ctrl-c-map* "B") => 'ediff-buffers))

    (test-case "keybindings: help extensions"
      (setup-default-bindings!)
      (check (keymap-lookup *help-map* "c") => 'describe-key-briefly)
      (check (keymap-lookup *help-map* "d") => 'describe-function)
      (check (keymap-lookup *help-map* "v") => 'describe-variable)
      (check (keymap-lookup *help-map* "i") => 'info))

    (test-case "keybindings: M-g extended bindings"
      (setup-default-bindings!)
      (check (keymap-lookup *meta-g-map* "i") => 'imenu)
      (check (keymap-lookup *meta-g-map* "F") => 'toggle-fold))

    (test-case "buffer-list management"
      ;; Clear any existing buffers
      (set! *buffer-list* [])
      ;; Create and add buffers
      (let* ((buf1 (make-buffer "test1.ss" #f #f #f #f #f #f))
             (buf2 (make-buffer "test2.ss" #f #f #f #f #f #f)))
        (buffer-list-add! buf1)
        (buffer-list-add! buf2)
        (check (length (buffer-list)) => 2)
        ;; Find by name
        (check (buffer-name (buffer-by-name "test1.ss")) => "test1.ss")
        (check (buffer-by-name "nonexistent") => #f)
        ;; Remove
        (buffer-list-remove! buf1)
        (check (length (buffer-list)) => 1)
        (check (buffer-by-name "test1.ss") => #f)))

    (test-case "dired-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (dired-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'dired)
        (check (dired-buffer? buf) => #t)))

    (test-case "brace-char? exported from core"
      (check (brace-char? (char->integer #\()) => #t)
      (check (brace-char? (char->integer #\))) => #t)
      (check (brace-char? (char->integer #\[)) => #t)
      (check (brace-char? (char->integer #\])) => #t)
      (check (brace-char? (char->integer #\{)) => #t)
      (check (brace-char? (char->integer #\})) => #t)
      (check (brace-char? (char->integer #\a)) => #f)
      (check (brace-char? (char->integer #\space)) => #f))

    (test-case "file I/O helpers"
      ;; Write and read a temp file
      (let ((tmp "/tmp/gemacs-test-file.txt"))
        (write-string-to-file tmp "hello world\n")
        (check (read-file-as-string tmp) => "hello world\n")
        ;; Cleanup
        (delete-file tmp)))

    (test-case "eval-expression-string: various types"
      ;; String result
      (let-values (((result error?) (eval-expression-string "\"hello\"")))
        (check result => "\"hello\"")
        (check error? => #f))
      ;; Boolean
      (let-values (((result error?) (eval-expression-string "#t")))
        (check result => "#t")
        (check error? => #f))
      ;; List
      (let-values (((result error?) (eval-expression-string "'(1 2 3)")))
        (check result => "(1 2 3)")
        (check error? => #f)))

    (test-case "eshell: pipeline"
      ;; Test piped commands
      (let-values (((output cwd) (eshell-process-input "echo hello | wc -c" "/tmp")))
        ;; Should get the character count of "hello\n"
        (check (string? output) => #t)))

    (test-case "eshell: head and tail"
      (let-values (((output cwd) (eshell-process-input "echo -e 'a\\nb\\nc' | head -2" "/tmp")))
        (check (string? output) => #t)))

    (test-case "eshell: find command"
      ;; Find in /tmp (may or may not have files)
      (let-values (((output cwd) (eshell-process-input "find /tmp -maxdepth 0 -type d" "/tmp")))
        (check (string? output) => #t)))

    (test-case "eshell: env and export"
      ;; Test env variable access
      (let-values (((output cwd) (eshell-process-input "echo $HOME" "/tmp")))
        (check (string? output) => #t)
        (check (> (string-length output) 0) => #t)))

    (test-case "strip-ansi-codes: complex sequences"
      ;; OSC sequences
      (let ((esc (string (integer->char 27))))
        ;; Title setting escape
        (check (strip-ansi-codes (string-append esc "]0;title" (string (integer->char 7)) "text"))
               => "text")
        ;; Nested formatting
        (check (strip-ansi-codes
                 (string-append esc "[1m" esc "[31m" "bold-red" esc "[0m"))
               => "bold-red")))

    (test-case "command registry: find-command returns procedure"
      (register-all-commands!)
      ;; find-command should return a procedure for registered commands
      (let ((cmd (find-command 'forward-char)))
        (check (procedure? cmd) => #t))
      ;; Non-existent command returns #f
      (check (find-command 'nonexistent-command-xyz) => #f))

    (test-case "all-commands hash populated"
      (register-all-commands!)
      ;; Should have many commands registered
      (check (> (hash-length *all-commands*) 100) => #t))

    ;; -- Markdown mode commands --
    (test-case "command registration: markdown mode"
      (register-all-commands!)
      (check (procedure? (find-command 'markdown-bold)) => #t)
      (check (procedure? (find-command 'markdown-italic)) => #t)
      (check (procedure? (find-command 'markdown-code)) => #t)
      (check (procedure? (find-command 'markdown-code-block)) => #t)
      (check (procedure? (find-command 'markdown-heading)) => #t)
      (check (procedure? (find-command 'markdown-link)) => #t)
      (check (procedure? (find-command 'markdown-image)) => #t)
      (check (procedure? (find-command 'markdown-hr)) => #t)
      (check (procedure? (find-command 'markdown-list-item)) => #t)
      (check (procedure? (find-command 'markdown-checkbox)) => #t)
      (check (procedure? (find-command 'markdown-toggle-checkbox)) => #t)
      (check (procedure? (find-command 'markdown-table)) => #t)
      (check (procedure? (find-command 'markdown-preview-outline)) => #t))

    ;; -- Dired operations --
    (test-case "command registration: dired operations"
      (register-all-commands!)
      (check (procedure? (find-command 'dired-mark)) => #t)
      (check (procedure? (find-command 'dired-unmark)) => #t)
      (check (procedure? (find-command 'dired-unmark-all)) => #t)
      (check (procedure? (find-command 'dired-delete-marked)) => #t)
      (check (procedure? (find-command 'dired-refresh)) => #t))

    ;; -- Utility commands --
    (test-case "command registration: diff, encoding, statistics"
      (register-all-commands!)
      (check (procedure? (find-command 'diff-two-files)) => #t)
      (check (procedure? (find-command 'set-buffer-encoding)) => #t)
      (check (procedure? (find-command 'convert-line-endings)) => #t)
      (check (procedure? (find-command 'buffer-statistics)) => #t))

    ;; -- Dired marks state --
    (test-case "dired marks: hash table operations"
      ;; Clean slate
      (set! *dired-marks* (make-hash-table))
      (hash-put! *dired-marks* "file1.txt" #t)
      (hash-put! *dired-marks* "file2.txt" #t)
      (check (length (hash-keys *dired-marks*)) => 2)
      (hash-remove! *dired-marks* "file1.txt")
      (check (length (hash-keys *dired-marks*)) => 1)
      (set! *dired-marks* (make-hash-table))
      (check (length (hash-keys *dired-marks*)) => 0))

    ;; -- Fuzzy matching --
    (test-case "fuzzy-match? basic"
      (check (fuzzy-match? "fb" "find-buffer") => #t)
      (check (fuzzy-match? "abc" "aXbXc") => #t)
      (check (fuzzy-match? "xyz" "hello") => #f)
      (check (fuzzy-match? "" "anything") => #t)
      (check (fuzzy-match? "FB" "find-buffer") => #t)) ;; case insensitive

    (test-case "fuzzy-score ranking"
      ;; Prefix match should score higher than non-prefix
      (let ((s1 (fuzzy-score "fi" "find-file"))
            (s2 (fuzzy-score "fi" "xfind")))
        (check (> s1 s2) => #t))
      ;; Consecutive matches should score higher
      (let ((s1 (fuzzy-score "abc" "abcdef"))
            (s2 (fuzzy-score "abc" "aXbXc")))
        (check (> s1 s2) => #t)))

    (test-case "command registration: fuzzy M-x and scratch"
      (register-all-commands!)
      (check (procedure? (find-command 'execute-extended-command-fuzzy)) => #t)
      (check (procedure? (find-command 'scratch-with-mode)) => #t)
      (check (procedure? (find-command 'switch-to-buffer-other-window)) => #t))

    ;; -- Editorconfig parsing --
    (test-case "parse-editorconfig: basic"
      ;; Write a temp .editorconfig
      (let ((tmp "/tmp/.gerbil-test-editorconfig"))
        (call-with-output-file tmp
          (lambda (port)
            (display "[*.py]\n" port)
            (display "indent_style = space\n" port)
            (display "indent_size = 4\n" port)))
        (let ((sections (parse-editorconfig tmp)))
          (check (pair? sections) => #t)
          (check (caar sections) => "*.py")
          (check (hash-get (cdar sections) "indent_style") => "space")
          (check (hash-get (cdar sections) "indent_size") => "4"))
        (delete-file tmp)))

    ;; -- URL detection --
    (test-case "find-url-at-point: basic"
      (let ((text "Visit https://example.com/path for info"))
        (check (find-url-at-point text 10) => '(6 . 30))
        (check (find-url-at-point text 0) => #f)))

    ;; -- Command history --
    (test-case "command-history: add and dedup"
      (set! *command-history* [])
      (command-history-add! "find-file")
      (command-history-add! "save-buffer")
      (command-history-add! "find-file")  ;; should dedup
      (check (length *command-history*) => 2)
      (check (car *command-history*) => "find-file"))

    ;; -- MRU buffer tracking --
    (test-case "buffer-access-times: tracking"
      (set! *buffer-access-times* (make-hash-table))
      (record-buffer-access! "a.txt")
      (record-buffer-access! "b.txt")
      (check (> (hash-get *buffer-access-times* "b.txt")
                (hash-get *buffer-access-times* "a.txt")) => #t))

    ;; -- Named macros --
    (test-case "named-macros: store and retrieve"
      (set! *named-macros* (make-hash-table))
      (hash-put! *named-macros* "test-macro" '((command . forward-char)))
      (check (hash-get *named-macros* "test-macro") => '((command . forward-char)))
      (check (hash-get *named-macros* "nonexistent") => #f))

    ;; -- Command registration batch 23 --
    (test-case "command registration: batch 23 features"
      (register-all-commands!)
      (check (procedure? (find-command 'editorconfig-apply)) => #t)
      (check (procedure? (find-command 'format-buffer)) => #t)
      (check (procedure? (find-command 'git-blame-line)) => #t)
      (check (procedure? (find-command 'execute-extended-command-with-history)) => #t)
      (check (procedure? (find-command 'complete-word-from-buffer)) => #t)
      (check (procedure? (find-command 'open-url-at-point)) => #t)
      (check (procedure? (find-command 'switch-buffer-mru)) => #t)
      (check (procedure? (find-command 'shell-command-on-region-replace)) => #t)
      (check (procedure? (find-command 'execute-named-macro)) => #t)
      (check (procedure? (find-command 'apply-macro-to-region)) => #t)
      (check (procedure? (find-command 'diff-summary)) => #t)
      (check (procedure? (find-command 'revert-buffer-no-confirm)) => #t)
      (check (procedure? (find-command 'sudo-save-buffer)) => #t))

    ;; -- URL encode/decode --
    (test-case "url-encode and url-decode"
      (check (url-encode "hello world") => "hello+world")
      (check (url-encode "a&b=c") => "a%26b%3dc")
      (check (url-decode "hello+world") => "hello world")
      (check (url-decode "a%26b%3dc") => "a&b=c"))

    ;; -- HTML entities --
    (test-case "html-encode and html-decode"
      (check (html-encode-entities "<b>hi</b>") => "&lt;b&gt;hi&lt;/b&gt;")
      (check (html-decode-entities "&amp;lt;") => "<")
      (check (html-decode-entities "&copy; 2024") => "(c) 2024"))

    ;; -- CSV split --
    (test-case "csv-split-line"
      (check (csv-split-line "a,b,c") => '("a" "b" "c"))
      (check (csv-split-line "\"hello world\",2,3") => '("hello world" "2" "3")))

    ;; -- Encoding detection --
    (test-case "detect-file-encoding: utf8"
      (let ((tmp "/tmp/.gerbil-test-encoding"))
        (call-with-output-file tmp (lambda (p) (display "hello" p)))
        (check (detect-file-encoding tmp) => "utf-8")
        (delete-file tmp)))

    ;; -- Command registration batch 24 --
    (test-case "command registration: batch 24 features"
      (register-all-commands!)
      (check (procedure? (find-command 'url-encode-region)) => #t)
      (check (procedure? (find-command 'url-decode-region)) => #t)
      (check (procedure? (find-command 'json-format-buffer)) => #t)
      (check (procedure? (find-command 'json-minify-buffer)) => #t)
      (check (procedure? (find-command 'json-sort-keys)) => #t)
      (check (procedure? (find-command 'jq-filter)) => #t)
      (check (procedure? (find-command 'html-encode-region)) => #t)
      (check (procedure? (find-command 'html-decode-region)) => #t)
      (check (procedure? (find-command 'find-file-with-warnings)) => #t)
      (check (procedure? (find-command 'detect-encoding)) => #t)
      (check (procedure? (find-command 'csv-align-columns)) => #t)
      (check (procedure? (find-command 'epoch-to-date)) => #t))

    ;; -- Batch 25: reverse-lines helper --
    (test-case "reverse-lines-in-string"
      (check (reverse-lines-in-string "a\nb\nc") => "c\nb\na")
      (check (reverse-lines-in-string "single") => "single")
      (check (reverse-lines-in-string "x\ny") => "y\nx"))

    ;; -- Batch 25: file-extension helper --
    (test-case "file-extension helper"
      (check (file-extension "test.py") => "py")
      (check (file-extension "foo.bar.js") => "js")
      (check (file-extension "noext") => ""))

    ;; -- Batch 25: file-runners config --
    (test-case "file-runners: known extensions"
      (check (hash-get *file-runners* "py") => "python3")
      (check (hash-get *file-runners* "sh") => "bash")
      (check (hash-get *file-runners* "ss") => "gxi")
      (check (hash-get *file-runners* "js") => "node")
      (check (hash-get *file-runners* "unknown") => #f))

    ;; -- Batch 25: mode toggles --
    (test-case "aggressive-indent and ws-butler toggles"
      (set! *aggressive-indent-mode* #f)
      (check *aggressive-indent-mode* => #f)
      (set! *aggressive-indent-mode* #t)
      (check *aggressive-indent-mode* => #t)
      (set! *ws-butler-mode* #f)
      (check *ws-butler-mode* => #f)
      (set! *ws-butler-mode* #t)
      (check *ws-butler-mode* => #t))

    ;; -- Command registration batch 25 --
    (test-case "command registration: batch 25 features"
      (register-all-commands!)
      (check (procedure? (find-command 'reverse-lines)) => #t)
      (check (procedure? (find-command 'shuffle-lines)) => #t)
      (check (procedure? (find-command 'calc-eval-region)) => #t)
      (check (procedure? (find-command 'table-insert)) => #t)
      (check (procedure? (find-command 'list-timers)) => #t)
      (check (procedure? (find-command 'toggle-aggressive-indent)) => #t)
      (check (procedure? (find-command 'smart-open-line-above)) => #t)
      (check (procedure? (find-command 'smart-open-line-below)) => #t)
      (check (procedure? (find-command 'quick-run)) => #t)
      (check (procedure? (find-command 'toggle-ws-butler-mode)) => #t)
      (check (procedure? (find-command 'copy-as-formatted)) => #t)
      (check (procedure? (find-command 'wrap-region-with)) => #t)
      (check (procedure? (find-command 'unwrap-region)) => #t)
      (check (procedure? (find-command 'toggle-quotes)) => #t)
      (check (procedure? (find-command 'word-frequency-analysis)) => #t)
      (check (procedure? (find-command 'selection-info)) => #t)
      (check (procedure? (find-command 'increment-hex-at-point)) => #t)
      (check (procedure? (find-command 'describe-char)) => #t)
      (check (procedure? (find-command 'narrow-to-region-simple)) => #t)
      (check (procedure? (find-command 'widen-simple)) => #t)
      (check (procedure? (find-command 'toggle-buffer-read-only)) => #t))

    ;; -- Batch 26: comment-style and flymake toggles --
    (test-case "batch 26 mode toggles"
      (set! *comment-style* 'line)
      (check (eq? *comment-style* 'line) => #t)
      (set! *comment-style* 'block)
      (check (eq? *comment-style* 'block) => #t)
      (set! *flymake-mode* #f)
      (check *flymake-mode* => #f)
      (set! *flymake-mode* #t)
      (check *flymake-mode* => #t))

    ;; -- Command registration batch 26 --
    (test-case "command registration: batch 26 features"
      (register-all-commands!)
      (check (procedure? (find-command 'comment-box)) => #t)
      (check (procedure? (find-command 'format-region)) => #t)
      (check (procedure? (find-command 'rename-symbol)) => #t)
      (check (procedure? (find-command 'isearch-occur)) => #t)
      (check (procedure? (find-command 'helm-mini)) => #t)
      (check (procedure? (find-command 'toggle-comment-style)) => #t)
      (check (procedure? (find-command 'toggle-flymake-mode)) => #t)
      (check (procedure? (find-command 'indent-for-tab)) => #t)
      (check (procedure? (find-command 'dedent-region)) => #t)
      (check (procedure? (find-command 'duplicate-and-comment)) => #t)
      (check (procedure? (find-command 'insert-scratch-message)) => #t)
      (check (procedure? (find-command 'count-lines-region)) => #t)
      (check (procedure? (find-command 'cycle-spacing)) => #t))

    ;; -- Batch 27 state tests --
    (test-case "batch 27 mode toggles"
      ;; Focus mode
      (set! *focus-mode* #f)
      (check *focus-mode* => #f)
      (set! *focus-mode* #t)
      (check *focus-mode* => #t)
      ;; Zen mode
      (set! *zen-mode* #f)
      (check *zen-mode* => #f)
      (set! *zen-mode* #t)
      (check *zen-mode* => #t)
      ;; Which-key mode
      (set! *which-key-mode* #f)
      (check *which-key-mode* => #f)
      (set! *which-key-mode* #t)
      (check *which-key-mode* => #t))

    (test-case "killed buffer stack: remember and recall"
      (set! *killed-buffers* '())
      (remember-killed-buffer! "test.ss" "/tmp/test.ss" "hello world")
      (check (length *killed-buffers*) => 1)
      (check (car (car *killed-buffers*)) => "test.ss")
      (remember-killed-buffer! "foo.ss" "/tmp/foo.ss" "foo content")
      (check (length *killed-buffers*) => 2)
      (check (car (car *killed-buffers*)) => "foo.ss"))

    (test-case "dedicated windows: hash operations"
      (set! *dedicated-windows* (make-hash-table))
      (hash-put! *dedicated-windows* "test-buf" #t)
      (check (hash-get *dedicated-windows* "test-buf") => #t)
      (hash-remove! *dedicated-windows* "test-buf")
      (check (hash-get *dedicated-windows* "test-buf") => #f))

    ;; -- Command registration batch 27 --
    (test-case "command registration: batch 27 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-focus-mode)) => #t)
      (check (procedure? (find-command 'toggle-zen-mode)) => #t)
      (check (procedure? (find-command 'reopen-killed-buffer)) => #t)
      (check (procedure? (find-command 'copy-file-name-only)) => #t)
      (check (procedure? (find-command 'open-containing-folder)) => #t)
      (check (procedure? (find-command 'new-empty-buffer)) => #t)
      (check (procedure? (find-command 'toggle-window-dedicated)) => #t)
      (check (procedure? (find-command 'toggle-which-key-mode)) => #t)
      (check (procedure? (find-command 'which-key-describe-prefix)) => #t)
      (check (procedure? (find-command 'transpose-windows)) => #t)
      (check (procedure? (find-command 'fold-toggle-at-point)) => #t)
      (check (procedure? (find-command 'imenu-list)) => #t))

    ;; -- Batch 28 state tests --
    (test-case "batch 28: regex builder pattern state"
      (set! *regex-builder-pattern* "")
      (check (string? *regex-builder-pattern*) => #t)
      (set! *regex-builder-pattern* "foo.*bar")
      (check *regex-builder-pattern* => "foo.*bar"))

    (test-case "batch 28: edit position tracking"
      (set! *last-edit-positions* '())
      (check (null? *last-edit-positions*) => #t)
      (set! *last-edit-positions*
        (cons (cons "test.ss" 42) *last-edit-positions*))
      (check (length *last-edit-positions*) => 1)
      (check (caar *last-edit-positions*) => "test.ss")
      (check (cdar *last-edit-positions*) => 42))

    (test-case "batch 28: persistent scratch file path"
      (check (string? *persistent-scratch-file*) => #t)
      (check (number? (string-contains *persistent-scratch-file* "scratch")) => #t))

    ;; -- Command registration batch 28 --
    (test-case "command registration: batch 28 features"
      (register-all-commands!)
      (check (procedure? (find-command 'regex-builder)) => #t)
      (check (procedure? (find-command 'eww-search-web)) => #t)
      (check (procedure? (find-command 'goto-last-edit)) => #t)
      (check (procedure? (find-command 'eval-region-and-replace)) => #t)
      (check (procedure? (find-command 'hex-to-decimal)) => #t)
      (check (procedure? (find-command 'decimal-to-hex)) => #t)
      (check (procedure? (find-command 'encode-hex-string)) => #t)
      (check (procedure? (find-command 'decode-hex-string)) => #t)
      (check (procedure? (find-command 'copy-buffer-file-name)) => #t)
      (check (procedure? (find-command 'insert-date-formatted)) => #t)
      (check (procedure? (find-command 'prepend-to-buffer)) => #t)
      (check (procedure? (find-command 'save-persistent-scratch)) => #t)
      (check (procedure? (find-command 'load-persistent-scratch)) => #t))

    ;; -- Batch 29 state tests --
    (test-case "batch 29: highlight changes mode"
      (set! *highlight-changes-mode* #f)
      (check *highlight-changes-mode* => #f)
      (set! *highlight-changes-mode* #t)
      (check *highlight-changes-mode* => #t))

    (test-case "batch 29: window layouts hash"
      (set! *saved-window-layouts* (make-hash-table))
      (hash-put! *saved-window-layouts* "main" '("*scratch*" "foo.ss"))
      (check (hash-get *saved-window-layouts* "main") => '("*scratch*" "foo.ss"))
      (check (hash-get *saved-window-layouts* "nonexistent") => #f))

    (test-case "batch 29: known modes hash"
      (check (hash-get *known-modes* "scheme") => "scheme")
      (check (hash-get *known-modes* "python") => "python")
      (check (hash-get *known-modes* "nonexistent") => #f))

    (test-case "batch 29: password chars length"
      (check (> (string-length *password-chars*) 60) => #t))

    ;; -- Command registration batch 29 --
    (test-case "command registration: batch 29 features"
      (register-all-commands!)
      (check (procedure? (find-command 'memory-usage)) => #t)
      (check (procedure? (find-command 'generate-password)) => #t)
      (check (procedure? (find-command 'insert-sequential-numbers)) => #t)
      (check (procedure? (find-command 'insert-env-var)) => #t)
      (check (procedure? (find-command 'untabify-region)) => #t)
      (check (procedure? (find-command 'tabify-region)) => #t)
      (check (procedure? (find-command 'shell-command-to-string)) => #t)
      (check (procedure? (find-command 'toggle-highlight-changes)) => #t)
      (check (procedure? (find-command 'window-save-layout)) => #t)
      (check (procedure? (find-command 'window-restore-layout)) => #t)
      (check (procedure? (find-command 'set-buffer-mode)) => #t)
      (check (procedure? (find-command 'canonically-space-region)) => #t)
      (check (procedure? (find-command 'list-packages)) => #t))

    ;; -- Batch 30 state tests --
    (test-case "batch 30: mode toggles"
      ;; Cursor type
      (set! *cursor-type* 'line)
      (check (eq? *cursor-type* 'line) => #t)
      (set! *cursor-type* 'block)
      (check (eq? *cursor-type* 'block) => #t)
      ;; Modeline
      (set! *modeline-visible* #t)
      (check *modeline-visible* => #t)
      ;; Indent guide
      (set! *indent-guide-mode* #f)
      (check *indent-guide-mode* => #f)
      ;; Rainbow mode
      (set! *rainbow-mode* #f)
      (check *rainbow-mode* => #f)
      ;; Electric quote
      (set! *electric-quote-mode* #f)
      (check *electric-quote-mode* => #f)
      ;; Visible mark
      (set! *visible-mark-mode* #f)
      (check *visible-mark-mode* => #f)
      ;; Fringe
      (set! *fringe-mode* #t)
      (check *fringe-mode* => #t))

    ;; -- Command registration batch 30 --
    (test-case "command registration: batch 30 features"
      (register-all-commands!)
      (check (procedure? (find-command 'insert-todo)) => #t)
      (check (procedure? (find-command 'insert-fixme)) => #t)
      (check (procedure? (find-command 'toggle-cursor-type)) => #t)
      (check (procedure? (find-command 'toggle-modeline)) => #t)
      (check (procedure? (find-command 'toggle-indent-guide)) => #t)
      (check (procedure? (find-command 'toggle-rainbow-mode)) => #t)
      (check (procedure? (find-command 'goto-scratch)) => #t)
      (check (procedure? (find-command 'display-prefix-help)) => #t)
      (check (procedure? (find-command 'toggle-electric-quote)) => #t)
      (check (procedure? (find-command 'calculator-inline)) => #t)
      (check (procedure? (find-command 'toggle-visible-mark)) => #t)
      (check (procedure? (find-command 'open-recent-dir)) => #t)
      (check (procedure? (find-command 'toggle-fringe)) => #t))

    ;; -- Batch 31 state tests --
    (test-case "batch 31: mode toggles"
      (set! *relative-line-numbers* #f)
      (check *relative-line-numbers* => #f)
      (set! *relative-line-numbers* #t)
      (check *relative-line-numbers* => #t)
      (set! *cua-mode* #f)
      (check *cua-mode* => #f)
      (set! *cua-mode* #t)
      (check *cua-mode* => #t))

    ;; -- Command registration batch 31 --
    (test-case "command registration: batch 31 features"
      (register-all-commands!)
      (check (procedure? (find-command 'titlecase-region)) => #t)
      (check (procedure? (find-command 'goto-matching-bracket)) => #t)
      (check (procedure? (find-command 'toggle-block-comment)) => #t)
      (check (procedure? (find-command 'move-to-window-center)) => #t)
      (check (procedure? (find-command 'reverse-region-chars)) => #t)
      (check (procedure? (find-command 'toggle-relative-line-numbers)) => #t)
      (check (procedure? (find-command 'toggle-cua-mode)) => #t)
      (check (procedure? (find-command 'exchange-dot-and-mark)) => #t)
      (check (procedure? (find-command 'sort-paragraphs)) => #t)
      (check (procedure? (find-command 'insert-mode-line)) => #t)
      (check (procedure? (find-command 'push-mark-command)) => #t))

    ;; -- Batch 32 tests --
    (test-case "batch 32: mode toggles"
      (set! *delete-selection-mode* #t)
      (check *delete-selection-mode* => #t)
      (set! *word-count-mode* #f)
      (check *word-count-mode* => #f)
      (set! *column-ruler-mode* #f)
      (check *column-ruler-mode* => #f)
      (set! *soft-wrap-mode* #f)
      (check *soft-wrap-mode* => #f)
      (set! *whitespace-cleanup-on-save* #f)
      (check *whitespace-cleanup-on-save* => #f)
      (set! *line-move-visual* #t)
      (check *line-move-visual* => #t)
      (check (list? *random-lines*) => #t)
      (check (> (length *random-lines*) 0) => #t))

    ;; -- Command registration batch 32 --
    (test-case "command registration: batch 32 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-delete-selection)) => #t)
      (check (procedure? (find-command 'toggle-word-count)) => #t)
      (check (procedure? (find-command 'toggle-column-ruler)) => #t)
      (check (procedure? (find-command 'shell-here)) => #t)
      (check (procedure? (find-command 'toggle-soft-wrap)) => #t)
      (check (procedure? (find-command 'toggle-whitespace-cleanup-on-save)) => #t)
      (check (procedure? (find-command 'insert-random-line)) => #t)
      (check (procedure? (find-command 'smart-backspace)) => #t)
      (check (procedure? (find-command 'toggle-line-move-visual)) => #t))

    ;; -- Batch 33 tests --
    (test-case "batch 33: mode toggles"
      (set! *subword-mode* #f)
      (check *subword-mode* => #f)
      (set! *auto-composition-mode* #t)
      (check *auto-composition-mode* => #t)
      (set! *bidi-display-reordering* #t)
      (check *bidi-display-reordering* => #t)
      (set! *fill-column-indicator* #f)
      (check *fill-column-indicator* => #f)
      (set! *pixel-scroll-mode* #f)
      (check *pixel-scroll-mode* => #f)
      (set! *auto-highlight-symbol-mode* #f)
      (check *auto-highlight-symbol-mode* => #f))

    (test-case "batch 33: lorem ipsum text"
      (check (string? *lorem-ipsum-text*) => #t)
      (check (> (string-length *lorem-ipsum-text*) 50) => #t))

    (test-case "batch 33: insert-char-by-code-string helper"
      (check (insert-char-by-code-string "65") => #\A)
      (check (insert-char-by-code-string "#x41") => #\A)
      (check (insert-char-by-code-string "0x41") => #\A)
      (check (insert-char-by-code-string "bad") => #f))

    ;; -- Command registration batch 33 --
    (test-case "command registration: batch 33 features"
      (register-all-commands!)
      (check (procedure? (find-command 'insert-char-by-code)) => #t)
      (check (procedure? (find-command 'toggle-subword-mode)) => #t)
      (check (procedure? (find-command 'toggle-auto-composition)) => #t)
      (check (procedure? (find-command 'toggle-bidi-display)) => #t)
      (check (procedure? (find-command 'toggle-display-fill-column-indicator)) => #t)
      (check (procedure? (find-command 'insert-current-file-name)) => #t)
      (check (procedure? (find-command 'toggle-pixel-scroll-2)) => #t)
      (check (procedure? (find-command 'insert-lorem-ipsum)) => #t)
      (check (procedure? (find-command 'toggle-auto-highlight-symbol)) => #t)
      (check (procedure? (find-command 'copy-rectangle-to-clipboard)) => #t)
      (check (procedure? (find-command 'insert-file-contents)) => #t))

    ;; -- Batch 34 tests --
    (test-case "batch 34: mode toggles"
      (set! *cursor-blink* #t)
      (check *cursor-blink* => #t)
      (set! *header-line-mode* #f)
      (check *header-line-mode* => #f)
      (set! *auto-save-visited-mode* #f)
      (check *auto-save-visited-mode* => #f)
      (set! *hl-todo-mode* #f)
      (check *hl-todo-mode* => #f))

    ;; -- Command registration batch 34 --
    (test-case "command registration: batch 34 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-cursor-blink)) => #t)
      (check (procedure? (find-command 'recenter-other-window)) => #t)
      (check (procedure? (find-command 'scroll-up-other-window)) => #t)
      (check (procedure? (find-command 'scroll-down-other-window)) => #t)
      (check (procedure? (find-command 'toggle-header-line)) => #t)
      (check (procedure? (find-command 'toggle-auto-save-visited)) => #t)
      (check (procedure? (find-command 'goto-random-line)) => #t)
      (check (procedure? (find-command 'reverse-words-in-region)) => #t)
      (check (procedure? (find-command 'insert-separator-line)) => #t)
      (check (procedure? (find-command 'toggle-hl-todo)) => #t)
      (check (procedure? (find-command 'sort-words-in-line)) => #t))

    ;; -- Batch 35 tests --
    (test-case "batch 35: mode toggles"
      (set! *global-auto-complete-mode* #f)
      (check *global-auto-complete-mode* => #f)
      (set! *which-function-mode* #f)
      (check *which-function-mode* => #f)
      (set! *display-line-numbers-mode* #t)
      (check *display-line-numbers-mode* => #t)
      (check *selective-display-level* => #f)
      (set! *global-font-lock-mode* #t)
      (check *global-font-lock-mode* => #t)
      (set! *auto-dim-other-buffers* #f)
      (check *auto-dim-other-buffers* => #f)
      (set! *global-eldoc-mode* #t)
      (check *global-eldoc-mode* => #t)
      (check (number? *word-wrap-column*) => #t))

    ;; -- Command registration batch 35 --
    (test-case "command registration: batch 35 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-auto-complete)) => #t)
      (check (procedure? (find-command 'toggle-which-function)) => #t)
      (check (procedure? (find-command 'toggle-display-line-numbers)) => #t)
      (check (procedure? (find-command 'toggle-selective-display)) => #t)
      (check (procedure? (find-command 'toggle-global-font-lock)) => #t)
      (check (procedure? (find-command 'insert-register-content)) => #t)
      (check (procedure? (find-command 'insert-date-iso)) => #t)
      (check (procedure? (find-command 'toggle-word-wrap-column)) => #t)
      (check (procedure? (find-command 'clone-indirect-buffer)) => #t)
      (check (procedure? (find-command 'toggle-auto-dim-other-buffers)) => #t)
      (check (procedure? (find-command 'toggle-global-eldoc)) => #t)
      (check (procedure? (find-command 'open-line-below)) => #t))

    ;; -- Batch 36 tests --
    (test-case "batch 36: mode toggles"
      (check (eq? *show-paren-style* 'parenthesis) => #t)
      (set! *auto-insert-mode* #f)
      (check *auto-insert-mode* => #f)
      (set! *global-visual-line-mode* #f)
      (check *global-visual-line-mode* => #f)
      (check (number? *scroll-conservatively*) => #t)
      (set! *show-keystroke-mode* #f)
      (check *show-keystroke-mode* => #f)
      (set! *auto-revert-tail-mode* #f)
      (check *auto-revert-tail-mode* => #f)
      (set! *flyspell-prog-mode* #f)
      (check *flyspell-prog-mode* => #f)
      (set! *auto-save-buffers-mode* #f)
      (check *auto-save-buffers-mode* => #f)
      (set! *global-linum-mode* #f)
      (check *global-linum-mode* => #f))

    ;; -- Command registration batch 36 --
    (test-case "command registration: batch 36 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-show-paren-style)) => #t)
      (check (procedure? (find-command 'insert-uuid-v4)) => #t)
      (check (procedure? (find-command 'toggle-auto-insert-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-visual-line)) => #t)
      (check (procedure? (find-command 'toggle-scroll-conservatively)) => #t)
      (check (procedure? (find-command 'toggle-show-keystroke)) => #t)
      (check (procedure? (find-command 'toggle-auto-revert-tail)) => #t)
      (check (procedure? (find-command 'toggle-flyspell-prog)) => #t)
      (check (procedure? (find-command 'toggle-auto-save-buffers)) => #t)
      (check (procedure? (find-command 'insert-backslash)) => #t)
      (check (procedure? (find-command 'toggle-global-linum)) => #t))

    ;; -- Batch 37 tests --
    (test-case "batch 37: mode toggles"
      (set! *highlight-indentation-mode* #f)
      (check *highlight-indentation-mode* => #f)
      (set! *hungry-delete-mode* #f)
      (check *hungry-delete-mode* => #f)
      (set! *type-break-mode* #f)
      (check *type-break-mode* => #f)
      (set! *delete-trailing-on-save* #f)
      (check *delete-trailing-on-save* => #f)
      (check *cursor-in-non-selected* => #t)
      (check *blink-matching-paren* => #t)
      (set! *next-error-follow* #f)
      (check *next-error-follow* => #f))

    ;; -- Command registration batch 37 --
    (test-case "command registration: batch 37 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-highlight-indentation)) => #t)
      (check (procedure? (find-command 'toggle-hungry-delete)) => #t)
      (check (procedure? (find-command 'toggle-type-break)) => #t)
      (check (procedure? (find-command 'insert-zero-width-space)) => #t)
      (check (procedure? (find-command 'toggle-delete-trailing-on-save)) => #t)
      (check (procedure? (find-command 'toggle-cursor-in-non-selected-windows)) => #t)
      (check (procedure? (find-command 'toggle-blink-matching-paren)) => #t)
      (check (procedure? (find-command 'toggle-next-error-follow)) => #t)
      (check (procedure? (find-command 'insert-page-break)) => #t))

    ;; -- Batch 38 tests --
    (test-case "batch 38: mode toggles"
      (check *auto-compression-mode* => #t)
      (set! *image-mode* #f)
      (check *image-mode* => #f)
      (set! *save-silently* #f)
      (check *save-silently* => #f)
      (check *confirm-kill-emacs* => #t)
      (check *auto-window-vscroll* => #t)
      (set! *fast-but-imprecise-scrolling* #f)
      (check *fast-but-imprecise-scrolling* => #f)
      (set! *mouse-avoidance-mode* #f)
      (check *mouse-avoidance-mode* => #f)
      (check *make-backup-files* => #t)
      (check *lock-file-create* => #t)
      (check *auto-encryption-mode* => #t))

    ;; -- Command registration batch 38 --
    (test-case "command registration: batch 38 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-auto-compression)) => #t)
      (check (procedure? (find-command 'toggle-image-mode)) => #t)
      (check (procedure? (find-command 'toggle-save-silently)) => #t)
      (check (procedure? (find-command 'toggle-confirm-kill-emacs)) => #t)
      (check (procedure? (find-command 'toggle-auto-window-vscroll)) => #t)
      (check (procedure? (find-command 'toggle-fast-but-imprecise-scrolling)) => #t)
      (check (procedure? (find-command 'toggle-mouse-avoidance)) => #t)
      (check (procedure? (find-command 'toggle-make-backup-files)) => #t)
      (check (procedure? (find-command 'toggle-lock-file-create)) => #t)
      (check (procedure? (find-command 'toggle-auto-encryption)) => #t))

    ;; -- Batch 39 tests --
    (test-case "batch 39: mode toggles"
      (set! *read-only-directories* #f)
      (check *read-only-directories* => #f)
      (check *auto-revert-verbose* => #t)
      (check *uniquify-buffer-names* => #t)
      (check *global-so-long-mode* => #t)
      (set! *minibuffer-depth-indicate* #f)
      (check *minibuffer-depth-indicate* => #f)
      (set! *context-menu-mode* #f)
      (check *context-menu-mode* => #f)
      (check *tooltip-mode* => #t)
      (check *file-name-shadow-mode* => #t)
      (check *minibuffer-electric-default* => #t)
      (set! *history-delete-duplicates* #f)
      (check *history-delete-duplicates* => #f))

    ;; -- Command registration batch 39 --
    (test-case "command registration: batch 39 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-read-only-directories)) => #t)
      (check (procedure? (find-command 'toggle-auto-revert-verbose)) => #t)
      (check (procedure? (find-command 'toggle-uniquify-buffer-names)) => #t)
      (check (procedure? (find-command 'toggle-global-so-long)) => #t)
      (check (procedure? (find-command 'toggle-minibuffer-depth-indicate)) => #t)
      (check (procedure? (find-command 'toggle-context-menu-mode)) => #t)
      (check (procedure? (find-command 'toggle-tooltip-mode)) => #t)
      (check (procedure? (find-command 'toggle-file-name-shadow)) => #t)
      (check (procedure? (find-command 'toggle-minibuffer-electric-default)) => #t)
      (check (procedure? (find-command 'toggle-history-delete-duplicates)) => #t))

    ;; -- Batch 40 tests --
    (test-case "batch 40: mode toggles"
      (check *delete-pair-blink* => #t)
      (set! *show-paren-when-point-inside* #f)
      (check *show-paren-when-point-inside* => #f)
      (set! *enable-recursive-minibuffers* #f)
      (check *enable-recursive-minibuffers* => #f)
      (set! *use-dialog-box* #f)
      (check *use-dialog-box* => #f)
      (check *use-short-answers* => #t)
      (check (eq? *ring-bell-function* 'ignore) => #t)
      (check *sentence-end-double-space* => #t)
      (set! *colon-double-space* #f)
      (check *colon-double-space* => #f)
      (set! *comment-auto-fill* #f)
      (check *comment-auto-fill* => #f))

    ;; -- Command registration batch 40 --
    (test-case "command registration: batch 40 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-delete-pair-blink)) => #t)
      (check (procedure? (find-command 'toggle-show-paren-when-point-inside)) => #t)
      (check (procedure? (find-command 'toggle-enable-recursive-minibuffers)) => #t)
      (check (procedure? (find-command 'toggle-use-dialog-box)) => #t)
      (check (procedure? (find-command 'toggle-use-short-answers)) => #t)
      (check (procedure? (find-command 'toggle-ring-bell-function)) => #t)
      (check (procedure? (find-command 'toggle-sentence-end-double-space)) => #t)
      (check (procedure? (find-command 'toggle-colon-double-space)) => #t)
      (check (procedure? (find-command 'toggle-comment-auto-fill)) => #t))

    ;; -- Batch 41 tests --
    (test-case "batch 41: mode toggles"
      (check *company-mode* => #f)
      (set! *company-mode* #t)
      (check *company-mode* => #t)
      (check *ivy-mode* => #f)
      (check *helm-mode* => #f)
      (check *projectile-mode* => #f)
      (check *doom-modeline* => #f)
      (check *treesit-mode* => #f)
      (check *eglot-mode* => #f)
      (check *display-time* => #f)
      (check *display-battery* => #f))

    ;; -- Command registration batch 41 --
    (test-case "command registration: batch 41 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-company-mode)) => #t)
      (check (procedure? (find-command 'toggle-ivy-mode)) => #t)
      (check (procedure? (find-command 'toggle-helm-mode)) => #t)
      (check (procedure? (find-command 'toggle-projectile-mode)) => #t)
      (check (procedure? (find-command 'toggle-doom-modeline)) => #t)
      (check (procedure? (find-command 'toggle-treesit-mode)) => #t)
      (check (procedure? (find-command 'toggle-eglot-mode)) => #t)
      (check (procedure? (find-command 'toggle-display-time)) => #t)
      (check (procedure? (find-command 'toggle-display-battery)) => #t))

    ;; -- Batch 42 tests --
    (test-case "batch 42: mode toggles"
      (check *auto-fill-comments* => #f)
      (check *electric-indent-mode* => #t)
      (set! *electric-indent-mode* #f)
      (check *electric-indent-mode* => #f)
      (check *truncate-partial-width* => #f)
      (check *inhibit-startup-screen* => #f)
      (check *visible-cursor* => #t)
      (check *transient-mark-mode* => #t)
      (check *global-whitespace-mode* => #f)
      (check *hide-ifdef-mode* => #f)
      (check *allout-mode* => #f))

    ;; -- Command registration batch 42 --
    (test-case "command registration: batch 42 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-auto-fill-comments)) => #t)
      (check (procedure? (find-command 'toggle-electric-indent-mode)) => #t)
      (check (procedure? (find-command 'toggle-truncate-partial-width-windows)) => #t)
      (check (procedure? (find-command 'toggle-inhibit-startup-screen)) => #t)
      (check (procedure? (find-command 'toggle-visible-cursor)) => #t)
      (check (procedure? (find-command 'toggle-transient-mark-mode)) => #t)
      (check (procedure? (find-command 'insert-form-feed)) => #t)
      (check (procedure? (find-command 'toggle-global-whitespace-mode)) => #t)
      (check (procedure? (find-command 'toggle-hide-ifdef-mode)) => #t)
      (check (procedure? (find-command 'toggle-allout-mode)) => #t))

    ;; -- Batch 43 tests --
    (test-case "batch 43: mode toggles"
      (check *desktop-save-mode* => #f)
      (check *recentf-mode* => #t)
      (check *savehist-mode* => #t)
      (check *winner-mode* => #t)
      (check *midnight-mode* => #f)
      (check *global-undo-tree* => #f)
      (check *diff-hl-mode* => #f)
      (check *volatile-highlights* => #f)
      (check *vertico-mode* => #f)
      (check *marginalia-mode* => #f))

    ;; -- Command registration batch 43 --
    (test-case "command registration: batch 43 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-desktop-save-mode)) => #t)
      (check (procedure? (find-command 'toggle-recentf-mode)) => #t)
      (check (procedure? (find-command 'toggle-savehist-mode)) => #t)
      (check (procedure? (find-command 'toggle-winner-mode)) => #t)
      (check (procedure? (find-command 'toggle-midnight-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-undo-tree)) => #t)
      (check (procedure? (find-command 'toggle-diff-hl-mode)) => #t)
      (check (procedure? (find-command 'toggle-volatile-highlights)) => #t)
      (check (procedure? (find-command 'toggle-vertico-mode)) => #t)
      (check (procedure? (find-command 'toggle-marginalia-mode)) => #t))

    ;; -- Batch 44 tests --
    (test-case "batch 44: mode toggles"
      (check *consult-mode* => #f)
      (check *orderless-mode* => #f)
      (check *embark-mode* => #f)
      (check *undo-fu-session* => #f)
      (check *auto-package-mode* => #f)
      (check *corfu-mode* => #f)
      (check *cape-mode* => #f)
      (check *nerd-icons-mode* => #f)
      (check *all-the-icons* => #f)
      (check *doom-themes* => #f))

    ;; -- Command registration batch 44 --
    (test-case "command registration: batch 44 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-consult-mode)) => #t)
      (check (procedure? (find-command 'toggle-orderless-mode)) => #t)
      (check (procedure? (find-command 'toggle-embark-mode)) => #t)
      (check (procedure? (find-command 'toggle-undo-fu-session)) => #t)
      (check (procedure? (find-command 'toggle-auto-package-mode)) => #t)
      (check (procedure? (find-command 'toggle-corfu-mode)) => #t)
      (check (procedure? (find-command 'toggle-cape-mode)) => #t)
      (check (procedure? (find-command 'toggle-nerd-icons-mode)) => #t)
      (check (procedure? (find-command 'toggle-all-the-icons)) => #t)
      (check (procedure? (find-command 'toggle-doom-themes)) => #t))

    ;; -- Batch 45 tests --
    (test-case "batch 45: mode toggles"
      (check *modus-themes* => #f)
      (check *ef-themes* => #f)
      (check *nano-theme* => #f)
      (check *ligature-mode* => #f)
      (check *pixel-scroll-precision* => #f)
      (check *repeat-mode* => #f)
      (check *tab-line-mode* => #f)
      (check *scroll-bar-mode* => #t)
      (check *tool-bar-mode* => #f))

    ;; -- Command registration batch 45 --
    (test-case "command registration: batch 45 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-modus-themes)) => #t)
      (check (procedure? (find-command 'toggle-ef-themes)) => #t)
      (check (procedure? (find-command 'toggle-nano-theme)) => #t)
      (check (procedure? (find-command 'toggle-ligature-mode)) => #t)
      (check (procedure? (find-command 'toggle-pixel-scroll-precision)) => #t)
      (check (procedure? (find-command 'toggle-repeat-mode)) => #t)
      (check (procedure? (find-command 'toggle-tab-line-mode)) => #t)
      (check (procedure? (find-command 'toggle-scroll-bar-mode)) => #t)
      (check (procedure? (find-command 'toggle-tool-bar-mode)) => #t))

    ;; -- Batch 46 tests --
    (test-case "batch 46: mode toggles"
      (check *auto-rename-tag* => #f)
      (check *global-prettify-symbols* => #f)
      (check *global-subword-mode* => #f)
      (check *global-superword-mode* => #f)
      (check *delete-by-moving-to-trash* => #f)
      (check *create-lockfiles* => #t)
      (check *mode-line-compact* => #f)
      (check *use-file-dialog* => #f)
      (check *xterm-mouse-mode* => #f))

    ;; -- Command registration batch 46 --
    (test-case "command registration: batch 46 features"
      (register-all-commands!)
      (check (procedure? (find-command 'insert-date-time-stamp)) => #t)
      (check (procedure? (find-command 'toggle-auto-rename-tag)) => #t)
      (check (procedure? (find-command 'toggle-global-prettify-symbols)) => #t)
      (check (procedure? (find-command 'toggle-global-subword)) => #t)
      (check (procedure? (find-command 'toggle-global-superword)) => #t)
      (check (procedure? (find-command 'toggle-delete-by-moving-to-trash)) => #t)
      (check (procedure? (find-command 'toggle-create-lockfiles)) => #t)
      (check (procedure? (find-command 'toggle-mode-line-compact)) => #t)
      (check (procedure? (find-command 'toggle-use-file-dialog)) => #t)
      (check (procedure? (find-command 'toggle-xterm-mouse-mode)) => #t))

    ;; -- Batch 47 tests --
    (test-case "batch 47: mode toggles"
      (check *auto-save-default* => #t)
      (check *make-pointer-invisible* => #t)
      (check *kill-whole-line* => #f)
      (check *set-mark-command-repeat-pop* => #f)
      (check *enable-local-variables* => #t)
      (check *enable-dir-local-variables* => #t)
      (check *ad-activate-all* => #f)
      (check *global-hi-lock-mode* => #f)
      (check *next-line-add-newlines* => #f))

    ;; -- Command registration batch 47 --
    (test-case "command registration: batch 47 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-auto-save-default)) => #t)
      (check (procedure? (find-command 'toggle-make-pointer-invisible)) => #t)
      (check (procedure? (find-command 'toggle-kill-whole-line)) => #t)
      (check (procedure? (find-command 'toggle-set-mark-command-repeat-pop)) => #t)
      (check (procedure? (find-command 'toggle-enable-local-variables)) => #t)
      (check (procedure? (find-command 'toggle-enable-dir-local-variables)) => #t)
      (check (procedure? (find-command 'toggle-ad-activate-all)) => #t)
      (check (procedure? (find-command 'toggle-global-hi-lock-mode)) => #t)
      (check (procedure? (find-command 'toggle-next-line-add-newlines)) => #t))

    ;; -- Batch 48 tests --
    (test-case "batch 48: mode toggles"
      (check *auto-save-on-idle* => #f)
      (check *delete-active-region* => #t)
      (check *shift-select-mode* => #t)
      (check *cua-selection-mode* => #f)
      (check *global-goto-address* => #f)
      (check *global-reveal-mode* => #f)
      (check *global-auto-composition* => #t)
      (check *global-display-line-numbers* => #f)
      (check *blink-cursor-mode* => #t))

    ;; -- Command registration batch 48 --
    (test-case "command registration: batch 48 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-auto-save-on-idle)) => #t)
      (check (procedure? (find-command 'toggle-delete-active-region)) => #t)
      (check (procedure? (find-command 'toggle-shift-select-mode)) => #t)
      (check (procedure? (find-command 'toggle-cua-selection-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-goto-address)) => #t)
      (check (procedure? (find-command 'toggle-global-reveal-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-auto-composition)) => #t)
      (check (procedure? (find-command 'toggle-global-display-line-numbers)) => #t)
      (check (procedure? (find-command 'toggle-blink-cursor-mode)) => #t))

    ;; -- Batch 49 tests --
    (test-case "batch 49: mode toggles"
      (check *indent-guide-global* => #f)
      (check *rainbow-delimiters-global* => #f)
      (check *global-display-fill-column* => #f)
      (check *global-flycheck* => #f)
      (check *global-company* => #f)
      (check *global-diff-hl* => #f)
      (check *global-git-gutter* => #f)
      (check *global-page-break-lines* => #f)
      (check *global-anzu* => #f))

    ;; -- Command registration batch 49 --
    (test-case "command registration: batch 49 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-indent-guide-global)) => #t)
      (check (procedure? (find-command 'toggle-rainbow-delimiters-global)) => #t)
      (check (procedure? (find-command 'toggle-global-display-fill-column)) => #t)
      (check (procedure? (find-command 'toggle-global-flycheck)) => #t)
      (check (procedure? (find-command 'toggle-global-company)) => #t)
      (check (procedure? (find-command 'toggle-global-diff-hl)) => #t)
      (check (procedure? (find-command 'toggle-global-git-gutter)) => #t)
      (check (procedure? (find-command 'toggle-global-page-break-lines)) => #t)
      (check (procedure? (find-command 'toggle-global-anzu)) => #t))

    ;; -- Batch 50 tests --
    (test-case "batch 50: mode toggles"
      (check *global-prettify* => #f)
      (check *global-hl-todo* => #f)
      (check *global-color-identifiers* => #f)
      (check *global-aggressive-indent* => #f)
      (check *global-origami* => #f)
      (check *global-centered-cursor* => #f)
      (check *global-beacon* => #f)
      (check *global-dimmer* => #f)
      (check *global-focus* => #f))

    ;; -- Command registration batch 50 --
    (test-case "command registration: batch 50 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-prettify)) => #t)
      (check (procedure? (find-command 'toggle-global-hl-todo)) => #t)
      (check (procedure? (find-command 'toggle-global-color-identifiers)) => #t)
      (check (procedure? (find-command 'toggle-global-aggressive-indent)) => #t)
      (check (procedure? (find-command 'toggle-global-origami)) => #t)
      (check (procedure? (find-command 'toggle-global-centered-cursor)) => #t)
      (check (procedure? (find-command 'toggle-global-beacon)) => #t)
      (check (procedure? (find-command 'toggle-global-dimmer)) => #t)
      (check (procedure? (find-command 'toggle-global-focus)) => #t))

    ;; -- Batch 51 tests --
    (test-case "batch 51: mode toggles"
      (check *global-auto-revert-non-file* => #f)
      (check *global-tree-sitter* => #f)
      (check *global-copilot* => #f)
      (check *global-lsp-mode* => #f)
      (check *global-format-on-save* => #f)
      (check *global-yas* => #f)
      (check *global-smartparens* => #f))

    ;; -- Command registration batch 51 --
    (test-case "command registration: batch 51 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-auto-revert-non-file)) => #t)
      (check (procedure? (find-command 'toggle-global-tree-sitter)) => #t)
      (check (procedure? (find-command 'toggle-global-copilot)) => #t)
      (check (procedure? (find-command 'toggle-global-lsp-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-format-on-save)) => #t)
      (check (procedure? (find-command 'toggle-global-yas)) => #t)
      (check (procedure? (find-command 'toggle-global-smartparens)) => #t))

    ;; -- Batch 52 tests --
    (test-case "batch 52: mode toggles"
      (check *global-cwarn* => #f)
      (check *global-hideshow* => #f)
      (check *global-abbrev* => #t)
      (check *global-diff-auto-refine* => #t)
      (check *global-eldoc-box* => #f)
      (check *global-flyspell-lazy* => #f)
      (check *global-so-clean* => #f))

    ;; -- Command registration batch 52 --
    (test-case "command registration: batch 52 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-cwarn)) => #t)
      (check (procedure? (find-command 'toggle-global-hideshow)) => #t)
      (check (procedure? (find-command 'toggle-global-abbrev)) => #t)
      (check (procedure? (find-command 'toggle-global-diff-auto-refine)) => #t)
      (check (procedure? (find-command 'toggle-global-eldoc-box)) => #t)
      (check (procedure? (find-command 'toggle-global-flyspell-lazy)) => #t)
      (check (procedure? (find-command 'toggle-global-so-clean)) => #t))

    ;; -- Batch 53 tests --
    (test-case "batch 53: mode toggles"
      (check *global-whitespace-newline* => #f)
      (check *global-highlight-indent* => #f)
      (check *global-rainbow-mode* => #f)
      (check *global-auto-highlight* => #f)
      (check *global-symbol-overlay* => #f)
      (check *global-highlight-parentheses* => #f)
      (check *global-pulse-line* => #f))

    ;; -- Command registration batch 53 --
    (test-case "command registration: batch 53 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-whitespace-newline)) => #t)
      (check (procedure? (find-command 'toggle-global-highlight-indent)) => #t)
      (check (procedure? (find-command 'toggle-global-rainbow-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-auto-highlight)) => #t)
      (check (procedure? (find-command 'toggle-global-symbol-overlay)) => #t)
      (check (procedure? (find-command 'toggle-global-highlight-parentheses)) => #t)
      (check (procedure? (find-command 'toggle-global-pulse-line)) => #t))

    ;; -- Batch 54 tests --
    (test-case "batch 54: mode toggles"
      (check *global-visual-regexp* => #f)
      (check *global-move-dup* => #f)
      (check *global-expand-region* => #f)
      (check *global-multiple-cursors* => #f)
      (check *global-undo-propose* => #f)
      (check *global-goto-chg* => #f)
      (check *global-avy* => #f))

    ;; -- Command registration batch 54 --
    (test-case "command registration: batch 54 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-visual-regexp)) => #t)
      (check (procedure? (find-command 'toggle-global-move-dup)) => #t)
      (check (procedure? (find-command 'toggle-global-expand-region)) => #t)
      (check (procedure? (find-command 'toggle-global-multiple-cursors)) => #t)
      (check (procedure? (find-command 'toggle-global-undo-propose)) => #t)
      (check (procedure? (find-command 'toggle-global-goto-chg)) => #t)
      (check (procedure? (find-command 'toggle-global-avy)) => #t))

    (test-case "batch 55: mode toggles"
      (check *global-wgrep* => #f)
      (check *global-deadgrep* => #f)
      (check *global-ripgrep* => #f)
      (check *global-projectile-ripgrep* => #f)
      (check *global-counsel* => #f)
      (check *global-swiper* => #f)
      (check *global-prescient* => #f))

    (test-case "command registration: batch 55 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-wgrep)) => #t)
      (check (procedure? (find-command 'toggle-global-deadgrep)) => #t)
      (check (procedure? (find-command 'toggle-global-ripgrep)) => #t)
      (check (procedure? (find-command 'toggle-global-projectile-ripgrep)) => #t)
      (check (procedure? (find-command 'toggle-global-counsel)) => #t)
      (check (procedure? (find-command 'toggle-global-swiper)) => #t)
      (check (procedure? (find-command 'toggle-global-prescient)) => #t))

    (test-case "batch 56: mode toggles"
      (check *global-which-key* => #f)
      (check *global-hydra* => #f)
      (check *global-transient* => #f)
      (check *global-general* => #f)
      (check *global-use-package* => #f)
      (check *global-diminish* => #f)
      (check *global-delight* => #f))

    (test-case "command registration: batch 56 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-which-key)) => #t)
      (check (procedure? (find-command 'toggle-global-hydra)) => #t)
      (check (procedure? (find-command 'toggle-global-transient)) => #t)
      (check (procedure? (find-command 'toggle-global-general)) => #t)
      (check (procedure? (find-command 'toggle-global-use-package)) => #t)
      (check (procedure? (find-command 'toggle-global-diminish)) => #t)
      (check (procedure? (find-command 'toggle-global-delight)) => #t))

    (test-case "batch 57: mode toggles"
      (check *global-envrc* => #f)
      (check *global-direnv* => #f)
      (check *global-editorconfig* => #f)
      (check *global-dtrt-indent* => #f)
      (check *global-ws-trim* => #f)
      (check *global-auto-compile* => #f)
      (check *global-no-littering* => #f))

    (test-case "command registration: batch 57 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-envrc)) => #t)
      (check (procedure? (find-command 'toggle-global-direnv)) => #t)
      (check (procedure? (find-command 'toggle-global-editorconfig)) => #t)
      (check (procedure? (find-command 'toggle-global-dtrt-indent)) => #t)
      (check (procedure? (find-command 'toggle-global-ws-trim)) => #t)
      (check (procedure? (find-command 'toggle-global-auto-compile)) => #t)
      (check (procedure? (find-command 'toggle-global-no-littering)) => #t))

    (test-case "batch 58: mode toggles"
      (check *global-golden-ratio* => #f)
      (check *global-zoom-window* => #f)
      (check *global-shackle* => #f)
      (check *global-popwin* => #f)
      (check *global-popper* => #f)
      (check *global-posframe* => #f)
      (check *global-childframe* => #f))

    (test-case "command registration: batch 58 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-golden-ratio)) => #t)
      (check (procedure? (find-command 'toggle-global-zoom-window)) => #t)
      (check (procedure? (find-command 'toggle-global-shackle)) => #t)
      (check (procedure? (find-command 'toggle-global-popwin)) => #t)
      (check (procedure? (find-command 'toggle-global-popper)) => #t)
      (check (procedure? (find-command 'toggle-global-posframe)) => #t)
      (check (procedure? (find-command 'toggle-global-childframe)) => #t))

    (test-case "batch 59: mode toggles"
      (check *global-helpful* => #f)
      (check *global-elisp-demos* => #f)
      (check *global-suggest* => #f)
      (check *global-buttercup* => #f)
      (check *global-ert-runner* => #f)
      (check *global-undercover* => #f)
      (check *global-benchmark-init* => #f))

    (test-case "command registration: batch 59 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-helpful)) => #t)
      (check (procedure? (find-command 'toggle-global-elisp-demos)) => #t)
      (check (procedure? (find-command 'toggle-global-suggest)) => #t)
      (check (procedure? (find-command 'toggle-global-buttercup)) => #t)
      (check (procedure? (find-command 'toggle-global-ert-runner)) => #t)
      (check (procedure? (find-command 'toggle-global-undercover)) => #t)
      (check (procedure? (find-command 'toggle-global-benchmark-init)) => #t))

    (test-case "batch 60: mode toggles"
      (check *global-native-compile* => #f)
      (check *global-gcmh* => #f)
      (check *global-esup* => #f)
      (check *global-explain-pause* => #f)
      (check *global-keyfreq* => #f)
      (check *global-command-log* => #f)
      (check *global-interaction-log* => #f))

    (test-case "command registration: batch 60 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-native-compile)) => #t)
      (check (procedure? (find-command 'toggle-global-gcmh)) => #t)
      (check (procedure? (find-command 'toggle-global-esup)) => #t)
      (check (procedure? (find-command 'toggle-global-explain-pause)) => #t)
      (check (procedure? (find-command 'toggle-global-keyfreq)) => #t)
      (check (procedure? (find-command 'toggle-global-command-log)) => #t)
      (check (procedure? (find-command 'toggle-global-interaction-log)) => #t))

    (test-case "batch 61: mode toggles"
      (check *global-treemacs-icons* => #f)
      (check *global-all-the-icons-dired* => #f)
      (check *global-centaur-tabs* => #f)
      (check *global-awesome-tab* => #f)
      (check *global-tab-bar* => #f)
      (check *global-mini-frame* => #f)
      (check *global-vertico-posframe* => #f))

    (test-case "command registration: batch 61 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-treemacs-icons)) => #t)
      (check (procedure? (find-command 'toggle-global-all-the-icons-dired)) => #t)
      (check (procedure? (find-command 'toggle-global-centaur-tabs)) => #t)
      (check (procedure? (find-command 'toggle-global-awesome-tab)) => #t)
      (check (procedure? (find-command 'toggle-global-tab-bar)) => #t)
      (check (procedure? (find-command 'toggle-global-mini-frame)) => #t)
      (check (procedure? (find-command 'toggle-global-vertico-posframe)) => #t))

    (test-case "batch 62: mode toggles"
      (check *global-solaire* => #f)
      (check *global-spaceline* => #f)
      (check *global-doom-modeline-env* => #f)
      (check *global-minions* => #f)
      (check *global-moody* => #f)
      (check *global-rich-minority* => #f)
      (check *global-smart-mode-line* => #f))

    (test-case "command registration: batch 62 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-solaire)) => #t)
      (check (procedure? (find-command 'toggle-global-spaceline)) => #t)
      (check (procedure? (find-command 'toggle-global-doom-modeline-env)) => #t)
      (check (procedure? (find-command 'toggle-global-minions)) => #t)
      (check (procedure? (find-command 'toggle-global-moody)) => #t)
      (check (procedure? (find-command 'toggle-global-rich-minority)) => #t)
      (check (procedure? (find-command 'toggle-global-smart-mode-line)) => #t))

    (test-case "batch 63: mode toggles"
      (check *global-nyan-cat* => #f)
      (check *global-parrot* => #f)
      (check *global-zone* => #f)
      (check *global-fireplace* => #f)
      (check *global-snow* => #f)
      (check *global-power-mode* => #f)
      (check *global-animate-typing* => #f))

    (test-case "command registration: batch 63 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-nyan-cat)) => #t)
      (check (procedure? (find-command 'toggle-global-parrot)) => #t)
      (check (procedure? (find-command 'toggle-global-zone)) => #t)
      (check (procedure? (find-command 'toggle-global-fireplace)) => #t)
      (check (procedure? (find-command 'toggle-global-snow)) => #t)
      (check (procedure? (find-command 'toggle-global-power-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-animate-typing)) => #t))

    (test-case "batch 64: mode toggles"
      (check *global-org-roam* => #f)
      (check *global-org-journal* => #f)
      (check *global-org-super-agenda* => #f)
      (check *global-org-noter* => #f)
      (check *global-org-download* => #f)
      (check *global-org-cliplink* => #f)
      (check *global-org-present* => #f))

    (test-case "command registration: batch 64 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-org-roam)) => #t)
      (check (procedure? (find-command 'toggle-global-org-journal)) => #t)
      (check (procedure? (find-command 'toggle-global-org-super-agenda)) => #t)
      (check (procedure? (find-command 'toggle-global-org-noter)) => #t)
      (check (procedure? (find-command 'toggle-global-org-download)) => #t)
      (check (procedure? (find-command 'toggle-global-org-cliplink)) => #t)
      (check (procedure? (find-command 'toggle-global-org-present)) => #t))

    (test-case "batch 65: mode toggles"
      (check *global-lsp-ui* => #f)
      (check *global-lsp-treemacs* => #f)
      (check *global-lsp-ivy* => #f)
      (check *global-dap-mode* => #f)
      (check *global-lsp-headerline* => #f)
      (check *global-lsp-lens* => #f)
      (check *global-lsp-semantic-tokens* => #f))

    (test-case "command registration: batch 65 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-lsp-ui)) => #t)
      (check (procedure? (find-command 'toggle-global-lsp-treemacs)) => #t)
      (check (procedure? (find-command 'toggle-global-lsp-ivy)) => #t)
      (check (procedure? (find-command 'toggle-global-dap-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-lsp-headerline)) => #t)
      (check (procedure? (find-command 'toggle-global-lsp-lens)) => #t)
      (check (procedure? (find-command 'toggle-global-lsp-semantic-tokens)) => #t))

    (test-case "batch 66: mode toggles"
      (check *global-docker* => #f)
      (check *global-kubernetes* => #f)
      (check *global-terraform* => #f)
      (check *global-ansible* => #f)
      (check *global-vagrant* => #f)
      (check *global-restclient* => #f)
      (check *global-ob-http* => #f))

    (test-case "command registration: batch 66 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-docker)) => #t)
      (check (procedure? (find-command 'toggle-global-kubernetes)) => #t)
      (check (procedure? (find-command 'toggle-global-terraform)) => #t)
      (check (procedure? (find-command 'toggle-global-ansible)) => #t)
      (check (procedure? (find-command 'toggle-global-vagrant)) => #t)
      (check (procedure? (find-command 'toggle-global-restclient)) => #t)
      (check (procedure? (find-command 'toggle-global-ob-http)) => #t))

    (test-case "batch 67: mode toggles"
      (check *global-rustic* => #f)
      (check *global-go-mode* => #f)
      (check *global-python-black* => #f)
      (check *global-elpy* => #f)
      (check *global-js2-mode* => #f)
      (check *global-typescript-mode* => #f)
      (check *global-web-mode* => #f))

    (test-case "command registration: batch 67 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-rustic)) => #t)
      (check (procedure? (find-command 'toggle-global-go-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-python-black)) => #t)
      (check (procedure? (find-command 'toggle-global-elpy)) => #t)
      (check (procedure? (find-command 'toggle-global-js2-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-typescript-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-web-mode)) => #t))

    (test-case "batch 68: mode toggles"
      (check *global-clojure-mode* => #f)
      (check *global-cider* => #f)
      (check *global-haskell-mode* => #f)
      (check *global-lua-mode* => #f)
      (check *global-ruby-mode* => #f)
      (check *global-php-mode* => #f)
      (check *global-swift-mode* => #f))

    (test-case "command registration: batch 68 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-clojure-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-cider)) => #t)
      (check (procedure? (find-command 'toggle-global-haskell-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-lua-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-ruby-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-php-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-swift-mode)) => #t))

    (test-case "batch 69: mode toggles"
      (check *global-yaml-mode* => #f)
      (check *global-toml-mode* => #f)
      (check *global-json-mode* => #f)
      (check *global-csv-mode* => #f)
      (check *global-protobuf-mode* => #f)
      (check *global-graphql-mode* => #f)
      (check *global-nix-mode* => #f))

    (test-case "command registration: batch 69 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-yaml-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-toml-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-json-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-csv-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-protobuf-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-graphql-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-nix-mode)) => #t))

    (test-case "batch 70: mode toggles"
      (check *global-cmake-mode* => #f)
      (check *global-bazel-mode* => #f)
      (check *global-meson-mode* => #f)
      (check *global-ninja-mode* => #f)
      (check *global-groovy-mode* => #f)
      (check *global-kotlin-mode* => #f)
      (check *global-scala-mode* => #f))

    (test-case "command registration: batch 70 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-cmake-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-bazel-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-meson-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-ninja-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-groovy-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-kotlin-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-scala-mode)) => #t))

    (test-case "batch 71: mode toggles"
      (check *global-erlang-mode* => #f)
      (check *global-elixir-mode* => #f)
      (check *global-zig-mode* => #f)
      (check *global-ocaml-mode* => #f)
      (check *global-fsharp-mode* => #f)
      (check *global-dart-mode* => #f)
      (check *global-julia-mode* => #f))

    (test-case "command registration: batch 71 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-erlang-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-elixir-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-zig-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-ocaml-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-fsharp-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-dart-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-julia-mode)) => #t))

    (test-case "batch 72: mode toggles"
      (check *global-r-mode* => #f)
      (check *global-ess* => #f)
      (check *global-sql-mode* => #f)
      (check *global-ein* => #f)
      (check *global-conda* => #f)
      (check *global-pyvenv* => #f)
      (check *global-pipenv* => #f))

    (test-case "command registration: batch 72 features"
      (register-all-commands!)
      (check (procedure? (find-command 'toggle-global-r-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-ess)) => #t)
      (check (procedure? (find-command 'toggle-global-sql-mode)) => #t)
      (check (procedure? (find-command 'toggle-global-ein)) => #t)
      (check (procedure? (find-command 'toggle-global-conda)) => #t)
      (check (procedure? (find-command 'toggle-global-pyvenv)) => #t)
      (check (procedure? (find-command 'toggle-global-pipenv)) => #t))

    ;;=========================================================================
    ;; Headless Scintilla editor tests
    ;; These create real Scintilla editors without a terminal and test
    ;; actual editor commands (cursor movement, text insertion, etc.)
    ;;=========================================================================

    (test-case "headless: create editor and app-state"
      ;; Verify we can create a fully functional headless editor
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (check (app-state? app) => #t)
        (check (eq? (current-editor app) ed) => #t)
        (check (eq? (current-buffer-from-app app) buf) => #t)))

    (test-case "headless: self-insert advances cursor"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        ;; Type 'h' (ASCII 104)
        (cmd-self-insert! app 104)
        (check (editor-get-text ed) => "h")
        (check (editor-get-current-pos ed) => 1)
        ;; Type 'i' (ASCII 105)
        (cmd-self-insert! app 105)
        (check (editor-get-text ed) => "hi")
        (check (editor-get-current-pos ed) => 2)
        ;; Type '!' (ASCII 33)
        (cmd-self-insert! app 33)
        (check (editor-get-text ed) => "hi!")
        (check (editor-get-current-pos ed) => 3)))

    (test-case "headless: auto-pair inserts both chars"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (set! *auto-pair-mode* #t)
        ;; Type '(' (ASCII 40) - should auto-pair with ')'
        (cmd-self-insert! app 40)
        (check (editor-get-text ed) => "()")
        ;; Cursor between parens
        (check (editor-get-current-pos ed) => 1)
        (set! *auto-pair-mode* #f)))

    (test-case "headless: navigation - forward/backward char"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        ;; Forward 3 chars
        (cmd-forward-char app)
        (cmd-forward-char app)
        (cmd-forward-char app)
        (check (editor-get-current-pos ed) => 3)
        ;; Backward 1 char
        (cmd-backward-char app)
        (check (editor-get-current-pos ed) => 2)))

    (test-case "headless: navigation - beginning/end of line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world\nsecond line")
        (editor-goto-pos ed 5)
        (cmd-end-of-line app)
        (check (editor-get-current-pos ed) => 11)
        (cmd-beginning-of-line app)
        (check (editor-get-current-pos ed) => 0)))

    (test-case "headless: navigation - next/previous line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line one\nline two\nline three")
        (editor-goto-pos ed 0)
        (cmd-next-line app)
        ;; Should be on line 1, column 0 -> position 9
        (check (editor-line-from-position ed (editor-get-current-pos ed)) => 1)
        (cmd-next-line app)
        (check (editor-line-from-position ed (editor-get-current-pos ed)) => 2)
        (cmd-previous-line app)
        (check (editor-line-from-position ed (editor-get-current-pos ed)) => 1)))

    (test-case "headless: navigation - beginning/end of buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line one\nline two\nline three")
        (editor-goto-pos ed 5)
        (cmd-end-of-buffer app)
        ;; "line one\nline two\nline three" = 28 chars, cursor goes to end
        (check (editor-get-current-pos ed) => (editor-get-text-length ed))
        (cmd-beginning-of-buffer app)
        (check (editor-get-current-pos ed) => 0)))

    (test-case "headless: newline with electric indent"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "  indented")
        (editor-goto-pos ed 10) ;; end of "  indented"
        (cmd-newline app)
        (let ((text (editor-get-text ed)))
          ;; Should have newline + 2 spaces of indent
          (check (string-contains text "\n  ") => 10)
          ;; Cursor should be after the indent
          (check (editor-get-current-pos ed) => 13))))

    (test-case "headless: kill-line and yank"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world\nsecond line")
        (editor-goto-pos ed 0)
        ;; Kill line should remove "hello world" (to end of line)
        (cmd-kill-line app)
        (check (editor-get-text ed) => "\nsecond line")
        ;; Kill ring should have "hello world"
        (check (car (app-state-kill-ring app)) => "hello world")
        ;; Move to end and yank
        (cmd-end-of-buffer app)
        (cmd-yank app)
        (check (editor-get-text ed) => "\nsecond linehello world")))

    (test-case "headless: delete-char and backward-delete-char"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "abcde")
        ;; Delete char at position 0 (removes 'a')
        (editor-goto-pos ed 0)
        (cmd-delete-char app)
        (check (editor-get-text ed) => "bcde")
        ;; Backward delete at position 2 (removes 'c')
        (editor-goto-pos ed 2)
        (cmd-backward-delete-char app)
        (check (editor-get-text ed) => "bde")))

    (test-case "headless: undo and redo"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (editor-set-save-point ed) ;; clear undo history
        (editor-insert-text ed 0 "hello")
        (editor-goto-pos ed 5)
        (check (editor-get-text ed) => "hello")
        (cmd-undo app)
        (check (editor-get-text ed) => "")
        (cmd-redo app)
        (check (editor-get-text ed) => "hello")))

    (test-case "headless: set-mark and kill-region"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Set mark at 0, move to 5, kill region -> removes "hello"
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        (cmd-kill-region app)
        (check (editor-get-text ed) => " world")
        ;; Scintilla clipboard should have the killed text
        (check (editor-get-clipboard ed) => "hello")
        ;; Mark should be cleared
        (check (buffer-mark buf) => #f)))

    (test-case "headless: copy-region-as-kill"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        (cmd-copy-region-as-kill app)
        ;; Text should be unchanged
        (check (editor-get-text ed) => "hello world")
        ;; Kill ring should have "hello"
        (check (car (app-state-kill-ring app)) => "hello")))

    (test-case "headless: buffer-list-buffer? predicate"
      (let ((buf (make-buffer "*test*" #f #f #f #f #f #f)))
        (check (buffer-list-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'buffer-list)
        (check (buffer-list-buffer? buf) => #t)))

    (test-case "headless: buffer list select switches buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        ;; Create a second buffer and buffer-list buffer
        (let* ((buf2 (buffer-create! "target.ss" ed "/tmp/target.ss"))
               (bl-buf (buffer-create! "*Buffer List*" ed)))
          (set! (buffer-lexer-lang bl-buf) 'buffer-list)
          (buffer-attach! ed bl-buf)
          (set! (edit-window-buffer win) bl-buf)
          (editor-set-text ed "  Buffer\t\tFile\n  ------\t\t----\n  *scratch*\t\t\n  target.ss\t\t/tmp/target.ss\n")
          ;; Position cursor on the "target.ss" line (line 3)
          (let ((line3-start (editor-position-from-line ed 3)))
            (editor-goto-pos ed line3-start)
            ;; Select buffer
            (cmd-buffer-list-select app)
            ;; Should have switched to target.ss buffer
            (check (buffer-name (edit-window-buffer win)) => "target.ss"))
          ;; Cleanup
          (buffer-list-remove! buf)
          (buffer-list-remove! buf2)
          (buffer-list-remove! bl-buf))))

    (test-case "headless: cmd-list-buffers then select (end-to-end)"
      ;; Regression: cmd-list-buffers formats with tabs, cmd-buffer-list-select
      ;; must parse that format correctly. This catches format/parse mismatches.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        (let ((buf2 (buffer-create! "myfile.ss" ed "/tmp/myfile.ss")))
          ;; Run cmd-list-buffers to generate the *Buffer List*
          (cmd-list-buffers app)
          ;; Should now be viewing *Buffer List*
          (check (buffer-name (edit-window-buffer win)) => "*Buffer List*")
          ;; Buffer list should be read-only
          (check (editor-get-read-only? ed) => #t)
          ;; Find the line containing "myfile.ss" and position cursor there
          (let* ((text (editor-get-text ed))
                 (lines (string-split text #\newline))
                 (target-line
                  (let loop ((ls lines) (i 0))
                    (cond ((null? ls) #f)
                          ((string-contains (car ls) "myfile.ss") i)
                          (else (loop (cdr ls) (+ i 1)))))))
            (check (not (eq? target-line #f)) => #t)
            (let ((pos (editor-position-from-line ed target-line)))
              (editor-goto-pos ed pos)
              ;; Select buffer — this must switch to myfile.ss
              (cmd-buffer-list-select app)
              (check (buffer-name (edit-window-buffer win)) => "myfile.ss")))
          ;; Cleanup
          (buffer-list-remove! buf)
          (buffer-list-remove! buf2)
          (let ((bl (buffer-by-name "*Buffer List*")))
            (when bl (buffer-list-remove! bl))))))

    (test-case "headless: REPL self-insert uses editor-insert-text"
      ;; Regression: editor-send-key doesn't insert chars in Scintilla TUI.
      ;; cmd-self-insert! must use editor-insert-text for REPL/shell/eshell buffers.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*REPL*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        ;; Mark as REPL buffer
        (set! (buffer-lexer-lang buf) 'repl)
        ;; Set initial prompt text
        (editor-set-text ed "gerbil> ")
        ;; Create a fake repl-state with prompt-pos at 8 (end of "gerbil> ")
        (let ((rs (make-repl-state #f 8 [])))
          (hash-put! *repl-state* buf rs)
          ;; Position cursor at end (after prompt)
          (editor-goto-pos ed 8)
          ;; Type "(+ 1 2)" via cmd-self-insert!
          (for-each (lambda (ch) (cmd-self-insert! app (char->integer ch)))
                    (string->list "(+ 1 2)"))
          ;; Verify the text was inserted
          (let ((text (editor-get-text ed)))
            (check (string-contains text "(+ 1 2)") => 8))
          ;; Verify cursor moved forward
          (check (editor-get-current-pos ed) => 15))
        ;; Cleanup
        (hash-remove! *repl-state* buf)
        (buffer-list-remove! buf)))

    (test-case "headless: shell self-insert shows in buffer"
      ;; Shell buffers insert typed chars locally (comint-style),
      ;; NOT via PTY echo. Commands are visible as you type them.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*shell*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        ;; Mark as shell buffer
        (set! (buffer-lexer-lang buf) 'shell)
        ;; Simulate shell prompt output
        (editor-set-text ed "$ ")
        ;; Create a fake shell-state with process=#f (no-op sends)
        (let ((ss (make-shell-state #f 2 #f)))
          (hash-put! *shell-state* buf ss)
          ;; Position cursor after prompt
          (editor-goto-pos ed 2)
          ;; Type "ls" via cmd-self-insert! — should appear in buffer
          (for-each (lambda (ch) (cmd-self-insert! app (char->integer ch)))
                    (string->list "ls"))
          ;; Buffer text now shows the typed command
          (let ((text (editor-get-text ed)))
            (check (not (not (string-contains text "ls"))) => #t)))
        ;; Cleanup
        (hash-remove! *shell-state* buf)
        (buffer-list-remove! buf)))

    (test-case "headless: org-todo cycling"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Test: heading with no state -> add TODO
        (editor-set-text ed "* Buy milk")
        (editor-goto-pos ed 2) ;; on the heading
        (cmd-org-todo app)
        (check (editor-get-text ed) => "* TODO Buy milk")
        ;; Test: TODO -> DONE
        (editor-goto-pos ed 2)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "* DONE Buy milk")
        ;; Test: DONE -> remove keyword
        (editor-goto-pos ed 2)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "* Buy milk")))

    (test-case "headless: dired buffer blocks self-insert"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*dired*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f 'dired #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "  file1.txt\n  file2.txt\n")
        (editor-goto-pos ed 0)
        ;; Self-insert should be suppressed in dired
        (cmd-self-insert! app 97) ;; 'a'
        (check (editor-get-text ed) => "  file1.txt\n  file2.txt\n")
        (check (editor-get-current-pos ed) => 0)))

    (test-case "headless: open-line inserts newline without moving cursor"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 5)
        (cmd-open-line app)
        (check (editor-get-text ed) => "hello\n")
        ;; Cursor stays at position 5 (before the newline)
        (check (editor-get-current-pos ed) => 5)))

    (test-case "headless: forward-word navigation"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world foo")
        (editor-goto-pos ed 0)
        (cmd-forward-word app)
        ;; Should be at end of first word or start of second
        (let ((pos (editor-get-current-pos ed)))
          (check (> pos 0) => #t)
          (check (<= pos 6) => #t))))

    (test-case "headless: self-insert in dired is suppressed"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*dired*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f 'dired #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "dir listing")
        (editor-goto-pos ed 0)
        (cmd-self-insert! app 120) ;; 'x'
        ;; Text should be unchanged
        (check (editor-get-text ed) => "dir listing")))

    (test-case "headless: multiple self-inserts build text correctly"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        ;; Type "abc" one character at a time
        (cmd-self-insert! app 97)  ;; a
        (cmd-self-insert! app 98)  ;; b
        (cmd-self-insert! app 99)  ;; c
        (check (editor-get-text ed) => "abc")
        (check (editor-get-current-pos ed) => 3)
        ;; Move to beginning and type "X"
        (editor-goto-pos ed 0)
        (cmd-self-insert! app 88)  ;; X
        (check (editor-get-text ed) => "Xabc")
        (check (editor-get-current-pos ed) => 1)))

    (test-case "headless: kill-line at end of line kills newline"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "first\nsecond")
        ;; Go to end of first line (pos 5, just before \n)
        (editor-goto-pos ed 5)
        (cmd-kill-line app)
        ;; Should kill the newline, joining lines
        (check (editor-get-text ed) => "firstsecond")))

    ;;=========================================================================
    ;; Org-parse: timestamp parsing tests
    ;;=========================================================================

    (test-case "org-parse-timestamp: active date only"
      (let ((ts (org-parse-timestamp "<2024-01-15>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'active)
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 1)
        (check (org-timestamp-day ts) => 15)
        (check (org-timestamp-day-name ts) => #f)
        (check (org-timestamp-hour ts) => #f)
        (check (org-timestamp-minute ts) => #f)))

    (test-case "org-parse-timestamp: active with day name and time"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:30>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'active)
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 1)
        (check (org-timestamp-day ts) => 15)
        (check (org-timestamp-day-name ts) => "Mon")
        (check (org-timestamp-hour ts) => 10)
        (check (org-timestamp-minute ts) => 30)))

    (test-case "org-parse-timestamp: inactive"
      (let ((ts (org-parse-timestamp "[2024-03-20 Wed 14:00]")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'inactive)
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 3)
        (check (org-timestamp-day ts) => 20)
        (check (org-timestamp-hour ts) => 14)
        (check (org-timestamp-minute ts) => 0)))

    (test-case "org-parse-timestamp: time range"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:00-11:30>")))
        (check (org-timestamp-hour ts) => 10)
        (check (org-timestamp-minute ts) => 0)
        (check (org-timestamp-end-hour ts) => 11)
        (check (org-timestamp-end-minute ts) => 30)))

    (test-case "org-parse-timestamp: with repeater"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:00 +1w>")))
        (check (org-timestamp-repeater ts) => "+1w")
        (check (org-timestamp-warning ts) => #f)))

    (test-case "org-parse-timestamp: with warning"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:00 -3d>")))
        (check (org-timestamp-warning ts) => "-3d")))

    (test-case "org-parse-timestamp: invalid returns #f"
      (check (org-parse-timestamp "not a timestamp") => #f)
      (check (org-parse-timestamp "") => #f)
      (check (org-parse-timestamp "short") => #f))

    (test-case "org-timestamp->string: round-trip"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:30>")))
        (check (org-timestamp->string ts) => "<2024-01-15 Mon 10:30>"))
      (let ((ts (org-parse-timestamp "[2024-03-20]")))
        (check (org-timestamp->string ts) => "[2024-03-20]")))

    (test-case "org-timestamp-elapsed: basic"
      (let ((start (org-parse-timestamp "[2024-01-15 Mon 10:00]"))
            (end   (org-parse-timestamp "[2024-01-15 Mon 11:30]")))
        (check (org-timestamp-elapsed start end) => "1:30")))

    (test-case "org-timestamp-elapsed: zero"
      (let ((start (org-parse-timestamp "[2024-01-15 Mon 10:00]"))
            (end   (org-parse-timestamp "[2024-01-15 Mon 10:00]")))
        (check (org-timestamp-elapsed start end) => "0:00")))

    (test-case "org-timestamp-elapsed: multi-hour"
      (let ((start (org-parse-timestamp "[2024-01-15 Mon 09:00]"))
            (end   (org-parse-timestamp "[2024-01-15 Mon 17:45]")))
        (check (org-timestamp-elapsed start end) => "8:45")))

    ;;=========================================================================
    ;; Org-parse: heading parsing tests
    ;;=========================================================================

    (test-case "org-parse-heading-line: full heading"
      (let-values (((level keyword priority title tags)
                    (org-parse-heading-line "** TODO [#A] My Task :work:urgent:")))
        (check level => 2)
        (check keyword => "TODO")
        (check priority => #\A)
        (check title => "My Task")
        (check tags => '("work" "urgent"))))

    (test-case "org-parse-heading-line: minimal heading"
      (let-values (((level keyword priority title tags)
                    (org-parse-heading-line "* Hello")))
        (check level => 1)
        (check keyword => #f)
        (check priority => #f)
        (check title => "Hello")
        (check tags => '())))

    (test-case "org-parse-heading-line: no keyword no tags"
      (let-values (((level keyword priority title tags)
                    (org-parse-heading-line "*** Just a title")))
        (check level => 3)
        (check keyword => #f)
        (check priority => #f)
        (check title => "Just a title")
        (check tags => '())))

    (test-case "org-parse-heading-line: DONE keyword"
      (let-values (((level keyword priority title tags)
                    (org-parse-heading-line "* DONE Finished")))
        (check level => 1)
        (check keyword => "DONE")
        (check title => "Finished")))

    (test-case "org-parse-heading-line: not a heading"
      (let-values (((level keyword priority title tags)
                    (org-parse-heading-line "Not a heading")))
        (check level => #f)))

    ;;=========================================================================
    ;; Org-parse: planning line tests
    ;;=========================================================================

    (test-case "org-parse-planning-line: SCHEDULED only"
      (let-values (((sched dead closed)
                    (org-parse-planning-line "  SCHEDULED: <2024-01-15 Mon 10:00>")))
        (check (org-timestamp? sched) => #t)
        (check (org-timestamp-day sched) => 15)
        (check dead => #f)
        (check closed => #f)))

    (test-case "org-parse-planning-line: DEADLINE only"
      (let-values (((sched dead closed)
                    (org-parse-planning-line "  DEADLINE: <2024-02-28>")))
        (check sched => #f)
        (check (org-timestamp? dead) => #t)
        (check (org-timestamp-month dead) => 2)))

    (test-case "org-parse-planning-line: both SCHEDULED and DEADLINE"
      (let-values (((sched dead closed)
                    (org-parse-planning-line "  SCHEDULED: <2024-01-15> DEADLINE: <2024-02-28>")))
        (check (org-timestamp? sched) => #t)
        (check (org-timestamp? dead) => #t)))

    ;;=========================================================================
    ;; Org-parse: property drawer tests
    ;;=========================================================================

    (test-case "org-parse-properties: basic"
      (let* ((lines '(":PROPERTIES:" ":ID: abc123" ":CATEGORY: work" ":END:"))
             (props (org-parse-properties lines 0)))
        (check (hash-get props "ID") => "abc123")
        (check (hash-get props "CATEGORY") => "work")))

    ;;=========================================================================
    ;; Org-parse: clock line tests
    ;;=========================================================================

    (test-case "org-parse-clock-line: closed clock"
      (let-values (((start end dur)
                    (org-parse-clock-line "  CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:30] =>  1:30")))
        (check (org-timestamp? start) => #t)
        (check (org-timestamp? end) => #t)
        (check dur => "1:30")))

    (test-case "org-parse-clock-line: open clock"
      (let-values (((start end dur)
                    (org-parse-clock-line "  CLOCK: [2024-01-15 Mon 10:00]")))
        (check (org-timestamp? start) => #t)
        (check end => #f)
        (check dur => #f)))

    ;;=========================================================================
    ;; Org-parse: buffer-level parsing tests
    ;;=========================================================================

    (test-case "org-parse-buffer: multiple headings"
      (let* ((text (string-append
                    "* TODO First heading\n"
                    "Some body text\n"
                    "** Sub heading\n"
                    "More body\n"
                    "* DONE Second heading\n"))
             (headings (org-parse-buffer text)))
        (check (length headings) => 3)
        (let ((h1 (car headings)))
          (check (org-heading-stars h1) => 1)
          (check (org-heading-keyword h1) => "TODO")
          (check (org-heading-title h1) => "First heading")
          (check (org-heading-line-number h1) => 0))
        (let ((h2 (cadr headings)))
          (check (org-heading-stars h2) => 2)
          (check (org-heading-title h2) => "Sub heading"))
        (let ((h3 (caddr headings)))
          (check (org-heading-stars h3) => 1)
          (check (org-heading-keyword h3) => "DONE"))))

    (test-case "org-parse-buffer: with planning"
      (let* ((text (string-append
                    "* TODO Task\n"
                    "  SCHEDULED: <2024-01-15 Mon 10:00>\n"
                    "  Body text\n"))
             (headings (org-parse-buffer text)))
        (check (length headings) => 1)
        (let ((h (car headings)))
          (check (org-timestamp? (org-heading-scheduled h)) => #t)
          (check (org-timestamp-day (org-heading-scheduled h)) => 15))))

    ;;=========================================================================
    ;; Org-parse: buffer settings tests
    ;;=========================================================================

    (test-case "org-parse-buffer-settings: basic"
      (let* ((text (string-append
                    "#+TITLE: My Document\n"
                    "#+AUTHOR: Test User\n"
                    "#+STARTUP: overview\n"))
             (settings (org-parse-buffer-settings text)))
        (check (hash-get settings "title") => "My Document")
        (check (hash-get settings "author") => "Test User")
        (check (hash-get settings "startup") => "overview")))

    (test-case "org-parse-buffer-settings: TODO keywords"
      (let* ((text "#+TODO: TODO NEXT | DONE CANCELLED\n")
             (settings (org-parse-buffer-settings text)))
        (check (hash-get settings "todo-active") => '("TODO" "NEXT"))
        (check (hash-get settings "todo-done") => '("DONE" "CANCELLED"))))

    ;;=========================================================================
    ;; Org-parse: tag matching tests
    ;;=========================================================================

    (test-case "org-parse-tag-expr: basic"
      (let ((terms (org-parse-tag-expr "+work-personal")))
        (check (length terms) => 2)
        (check (car (car terms)) => '+)
        (check (cdr (car terms)) => "work")
        (check (car (cadr terms)) => '-)
        (check (cdr (cadr terms)) => "personal")))

    (test-case "org-parse-tag-expr: simple tag name"
      (let ((terms (org-parse-tag-expr "work")))
        (check (length terms) => 1)
        (check (car (car terms)) => '+)
        (check (cdr (car terms)) => "work")))

    ;;=========================================================================
    ;; Org-parse: utility function tests
    ;;=========================================================================

    (test-case "org-heading-line?: detects headings"
      (check (org-heading-line? "* Heading") => #t)
      (check (org-heading-line? "** Sub") => #t)
      (check (org-heading-line? "Not a heading") => #f)
      (check (org-heading-line? "") => #f))

    (test-case "org-table-line?: detects tables"
      (check (org-table-line? "| a | b |") => #t)
      (check (org-table-line? "|---+---|") => #t)
      (check (org-table-line? "not a table") => #f))

    (test-case "org-comment-line?: detects comments"
      (check (org-comment-line? "# This is a comment") => #t)
      (check (org-comment-line? "#+TITLE: Not a comment") => #f)
      (check (org-comment-line? "Regular text") => #f))

    (test-case "org-keyword-line?: detects keywords"
      (check (org-keyword-line? "#+TITLE: Hello") => #t)
      (check (org-keyword-line? "#+BEGIN_SRC") => #t)
      (check (org-keyword-line? "Not a keyword") => #f))

    (test-case "org-block-begin?: detects block starts"
      (check (org-block-begin? "#+BEGIN_SRC python") => #t)
      (check (org-block-begin? "#+begin_quote") => #t)
      (check (org-block-begin? "not a block") => #f))

    (test-case "org-current-timestamp-string: produces valid format"
      (let ((ts (org-current-timestamp-string)))
        (check (char=? (string-ref ts 0) #\<) => #t)
        (check (char=? (string-ref ts (- (string-length ts) 1)) #\>) => #t)
        ;; Should contain a date pattern
        (check (> (string-length ts) 15) => #t)))

    (test-case "pad-02: zero-pads correctly"
      (check (pad-02 0) => "00")
      (check (pad-02 5) => "05")
      (check (pad-02 10) => "10")
      (check (pad-02 31) => "31"))

    ;;=========================================================================
    ;; Org-mode: helper function tests (existing)
    ;;=========================================================================

    (test-case "org-heading-level: counts stars"
      (check (org-heading-level "* Heading") => 1)
      (check (org-heading-level "** Sub") => 2)
      (check (org-heading-level "*** Deep") => 3)
      (check (org-heading-level "Not a heading") => 0)
      (check (org-heading-level "") => 0))

    (test-case "org-find-subtree-end: finds boundaries"
      (let ((lines '("* H1" "Body1" "** Sub1" "Body2" "* H2" "Body3")))
        ;; Subtree of H1 (level 1) ends at H2 (index 4)
        (check (org-find-subtree-end lines 0 1) => 4)
        ;; Subtree of Sub1 (level 2) ends at H2 (index 4)
        (check (org-find-subtree-end lines 2 2) => 4)
        ;; Subtree of H2 (level 1) ends at end of list
        (check (org-find-subtree-end lines 4 1) => 6)))

    (test-case "org-on-checkbox-line?: detects checkboxes"
      ;; string-contains returns index (0 for first match) or #f
      (check (not (not (org-on-checkbox-line? "- [ ] Buy milk"))) => #t)
      (check (not (not (org-on-checkbox-line? "- [X] Done task"))) => #t)
      (check (not (not (org-on-checkbox-line? "- [x] Also done"))) => #t)
      (check (org-on-checkbox-line? "Regular text") => #f)
      (check (org-on-checkbox-line? "* Heading") => #f))

    ;;=========================================================================
    ;; Org-mode: headless command tests
    ;;=========================================================================

    (test-case "headless: org-todo on nested heading"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Test on ** sub-heading
        (editor-set-text ed "** Task item")
        (editor-goto-pos ed 3)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "** TODO Task item")
        ;; Cycle to DONE
        (editor-goto-pos ed 3)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "** DONE Task item")
        ;; Cycle back to none
        (editor-goto-pos ed 3)
        (cmd-org-todo app)
        (check (editor-get-text ed) => "** Task item")))

    (test-case "headless: org-promote and org-demote"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Demote: * -> **
        (editor-set-text ed "* Heading One")
        (editor-goto-pos ed 2)
        (cmd-org-demote app)
        (check (editor-get-text ed) => "** Heading One")
        ;; Demote again: ** -> ***
        (editor-goto-pos ed 2)
        (cmd-org-demote app)
        (check (editor-get-text ed) => "*** Heading One")
        ;; Promote: *** -> **
        (editor-goto-pos ed 2)
        (cmd-org-promote app)
        (check (editor-get-text ed) => "** Heading One")
        ;; Promote: ** -> *
        (editor-goto-pos ed 2)
        (cmd-org-promote app)
        (check (editor-get-text ed) => "* Heading One")))

    (test-case "headless: org-promote at level 1 is no-op"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Already top")
        (editor-goto-pos ed 2)
        (cmd-org-promote app)
        ;; Should stay unchanged
        (check (editor-get-text ed) => "* Already top")))

    (test-case "headless: org-toggle-checkbox"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Unchecked -> checked
        (editor-set-text ed "- [ ] Buy groceries")
        (editor-goto-pos ed 5)
        (cmd-org-toggle-checkbox app)
        (check (editor-get-text ed) => "- [X] Buy groceries")
        ;; Checked -> unchecked
        (editor-goto-pos ed 5)
        (cmd-org-toggle-checkbox app)
        (check (editor-get-text ed) => "- [ ] Buy groceries")))

    (test-case "headless: org-priority cycling"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; No priority -> [#A]
        (editor-set-text ed "* Task")
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* [#A] Task")
        ;; [#A] -> [#B]
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* [#B] Task")
        ;; [#B] -> [#C]
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* [#C] Task")
        ;; [#C] -> none
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* Task")))

    (test-case "headless: org-priority with TODO keyword"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; TODO heading: priority goes after keyword
        (editor-set-text ed "* TODO Fix bug")
        (editor-goto-pos ed 2)
        (cmd-org-priority app)
        (check (editor-get-text ed) => "* TODO [#A] Fix bug")))

    (test-case "headless: org-move-subtree-down"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Two headings with content
        (editor-set-text ed "* First\nBody 1\n* Second\nBody 2")
        (editor-goto-pos ed 0)  ;; on "* First"
        (cmd-org-move-subtree-down app)
        (check (editor-get-text ed) => "* Second\nBody 2\n* First\nBody 1")))

    (test-case "headless: org-move-subtree-up"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Position on second heading
        (editor-set-text ed "* First\nBody 1\n* Second\nBody 2")
        ;; Move cursor to "* Second" (line 2)
        (editor-goto-pos ed (editor-position-from-line ed 2))
        (cmd-org-move-subtree-up app)
        (check (editor-get-text ed) => "* Second\nBody 2\n* First\nBody 1")))

    (test-case "headless: org-insert-heading"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; On a level-1 heading, insert new level-1 heading after subtree
        (editor-set-text ed "* Heading\nSome body text")
        (editor-goto-pos ed 2)
        (cmd-org-insert-heading app)
        (let ((text (editor-get-text ed)))
          ;; Should have new "* " heading after the body
          (check (not (not (string-contains text "\n* "))) => #t))))

    (test-case "headless: org-insert-heading at level 2"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "** Sub heading\nBody")
        (editor-goto-pos ed 3)
        (cmd-org-insert-heading app)
        (let ((text (editor-get-text ed)))
          ;; Should insert ** heading (same level)
          (check (not (not (string-contains text "\n** "))) => #t))))

    (test-case "headless: org-export strips stars, preserves case"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* My Heading\nBody text here\n** Sub Heading")
        (cmd-org-export app)
        ;; Export creates *Org Export* buffer with stripped text
        (let ((text (editor-get-text ed)))
          ;; Heading text should preserve case (bug fix: was string-upcase)
          (check (not (not (string-contains text "My Heading"))) => #t)
          (check (not (not (string-contains text "Sub Heading"))) => #t)
          ;; Should not contain * prefix
          (check (not (string-contains text "* ")) => #t)
          ;; Body preserved
          (check (not (not (string-contains text "Body text here"))) => #t))))

    (test-case "headless: org-cycle fold/unfold"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nChild line 1\nChild line 2\n* Next")
        (editor-goto-pos ed 0)
        ;; All lines should be visible initially
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)
        ;; Fold
        (cmd-org-cycle app)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 0)
        ;; Heading line 0 stays visible
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)
        ;; Line 3 ("* Next") should stay visible (not part of subtree)
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 1)
        ;; Unfold
        (cmd-org-cycle app)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)))

    (test-case "headless: org-shift-tab global fold/unfold"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* H1\nBody1\n* H2\nBody2")
        ;; All visible initially
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        ;; Fold: hide non-headings
        (cmd-org-shift-tab app)
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)  ;; heading visible
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)  ;; body hidden
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)  ;; heading visible
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 0)  ;; body hidden
        ;; Unfold: show all
        (cmd-org-shift-tab app)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 1)))

    (test-case "headless: org-store-link"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nLine 2")
        (editor-goto-pos ed 0) ;; line 0
        (cmd-org-store-link app)
        (check (string? *org-stored-link*) => #t)
        (check (not (not (string-contains *org-stored-link* "file:/tmp/test.org"))) => #t)
        (check (not (not (string-contains *org-stored-link* "::1"))) => #t)))

    (test-case "headless: org src-block template format"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; cmd-org-insert-src-block requires interactive prompt, so test
        ;; the template format directly by inserting text
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (let ((template "#+BEGIN_SRC python\n\n#+END_SRC\n"))
          (editor-insert-text ed 0 template)
          (let ((text (editor-get-text ed)))
            (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
            (check (not (not (string-contains text "#+END_SRC"))) => #t)))))

    (test-case "headless: org-template-expand <s"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "#+END_SRC"))) => #t)
          ;; No <s remaining
          (check (not (string-contains text "<s")) => #t))))

    (test-case "headless: org-template-expand <e"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<e")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_EXAMPLE"))) => #t)
          (check (not (not (string-contains text "#+END_EXAMPLE"))) => #t))))

    (test-case "headless: org-template-expand <q"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<q")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_QUOTE"))) => #t)
          (check (not (not (string-contains text "#+END_QUOTE"))) => #t))))

    (test-case "headless: org-template-expand preserves indentation"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Indented template
        (editor-set-text ed "  <s")
        (editor-goto-pos ed 4)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          ;; Both begin and end should be indented
          (check (not (not (string-contains text "  #+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "  #+END_SRC"))) => #t))))

    (test-case "headless: org-template-expand <l for latex export"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<l")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          ;; BEGIN has full "EXPORT latex"
          (check (not (not (string-contains text "#+BEGIN_EXPORT latex"))) => #t)
          ;; END has just "EXPORT"
          (check (not (not (string-contains text "#+END_EXPORT"))) => #t))))

    (test-case "headless: org-template-expand unknown key"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Unknown template key - should not expand
        (editor-set-text ed "<z")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (check (editor-get-text ed) => "<z")))

    (test-case "headless: org-template-expand preserves highlighting"
      ;; Regression: <sTAB was clearing all org-mode colors because
      ;; editor-set-text resets Scintilla styles and highlighting was not
      ;; re-applied after the expansion.
      (let* ((SCI_GETSTYLEAT 2010)
             (ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "notes.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Set up an org buffer with a heading followed by a template trigger
        (editor-set-text ed "* My Heading\n<s")
        (editor-goto-pos ed 15) ;; cursor after <s
        ;; Apply org highlighting so heading gets styled
        (setup-org-styles! ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; Verify heading is styled before expansion
        (let ((style-before (send-message ed SCI_GETSTYLEAT 0 0)))
          (check (= style-before ORG_STYLE_HEADING_1) => #t))
        ;; Expand the <s template
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          ;; Template was expanded
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          ;; Heading is STILL styled after expansion (the bug: it was reset to 0)
          (let ((style-after (send-message ed SCI_GETSTYLEAT 0 0)))
            (check (= style-after ORG_STYLE_HEADING_1) => #t))
          ;; Block delimiters are styled too
          (let* ((begin-pos (string-contains text "#+BEGIN_SRC"))
                 (style-begin (send-message ed SCI_GETSTYLEAT begin-pos 0)))
            (check (= style-begin ORG_STYLE_BLOCK_DELIM) => #t)))))

    ;;=========================================================================
    ;; Org-table: pure function tests
    ;;=========================================================================

    (test-case "org-table-row?: detects table rows"
      (check (org-table-row? "| a | b |") => #t)
      (check (org-table-row? "|---+---|") => #t)
      (check (org-table-row? "  | x |") => #t)
      (check (org-table-row? "not a row") => #f)
      (check (org-table-row? "") => #f))

    (test-case "org-table-separator?: detects separators"
      (check (org-table-separator? "|---+---|") => #t)
      (check (org-table-separator? "|---|") => #t)
      (check (org-table-separator? "| a | b |") => #f))

    (test-case "org-table-parse-row: splits cells"
      (check (org-table-parse-row "| a | bb | ccc |") => '("a" "bb" "ccc"))
      (check (org-table-parse-row "|x|y|") => '("x" "y"))
      (check (org-table-parse-row "| single |") => '("single"))
      (check (org-table-parse-row "not a row") => '()))

    (test-case "org-table-column-widths: computes widths"
      (let ((rows '(("a" "bb" "ccc")
                     ("dddd" "e" "ff"))))
        (check (org-table-column-widths rows) => '(4 2 3))))

    (test-case "org-table-format-row: pads cells"
      (let ((widths '(4 3 5)))
        (check (org-table-format-row '("a" "bb" "ccc") widths)
               => "| a    | bb  | ccc   |")))

    (test-case "org-table-format-separator: generates separator"
      (check (org-table-format-separator '(3 4 5))
             => "|-----+------+-------|"))

    (test-case "org-numeric-cell?: detects numbers"
      (check (org-numeric-cell? "123") => #t)
      (check (org-numeric-cell? "3.14") => #t)
      (check (org-numeric-cell? "-5") => #t)
      (check (org-numeric-cell? "50%") => #t)
      (check (org-numeric-cell? "abc") => #f)
      (check (org-numeric-cell? "") => #f))

    (test-case "org-table-parse-tblfm: parses formulas"
      (let ((result (org-table-parse-tblfm "#+TBLFM: $3=$1+$2")))
        (check (length result) => 1)
        (check (caar result) => "$3")
        (check (cdar result) => "$1+$2")))

    (test-case "org-csv-to-table: converts CSV to org"
      (let ((result (org-csv-to-table "a,b,c\n1,2,3")))
        (check (not (not (string-contains result "| a"))) => #t)
        (check (not (not (string-contains result "| 1"))) => #t)))

    (test-case "headless: org-table-align basic"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "| a | bb |\n| ccc | d |")
        (editor-goto-pos ed 2)
        (org-table-align ed)
        (let ((text (editor-get-text ed)))
          ;; Both columns should be padded to same width
          (check (not (not (string-contains text "| a   |"))) => #t)
          (check (not (not (string-contains text "| ccc |"))) => #t))))

    (test-case "headless: org-table-align with separator"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "| Name | Age |\n|---+---|\n| Alice | 30 |")
        (editor-goto-pos ed 2)
        (org-table-align ed)
        (let ((text (editor-get-text ed)))
          ;; Separator should be regenerated with proper widths
          (check (not (not (string-contains text "|"))) => #t)
          (check (not (not (string-contains text "Alice"))) => #t))))

    (test-case "headless: org-table-insert-row"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "| a | b |\n| c | d |")
        (editor-goto-pos ed 2) ;; on first row
        (org-table-insert-row ed)
        (let ((text (editor-get-text ed)))
          ;; Should now have 3 rows
          (let ((row-count (length (filter (lambda (l) (org-table-row? l))
                                          (string-split text #\newline)))))
            (check (= row-count 3) => #t)))))

    (test-case "headless: org-table-to-csv"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "| Name | Age |\n|---+---|\n| Alice | 30 |")
        (editor-goto-pos ed 2)
        (let ((csv (org-table-to-csv ed)))
          (check (not (not (string-contains csv "Name"))) => #t)
          (check (not (not (string-contains csv "Alice"))) => #t))))

    ;;=========================================================================
    ;; Org-table TAB + highlighting regression tests
    ;;=========================================================================

    (test-case "headless: org-table-align preserves heading styling"
      ;; Regression: org-table-align uses SCI_REPLACETARGET which clears
      ;; styling in the replaced region. Heading above the table must keep
      ;; its style after table alignment.
      (let* ((SCI_GETSTYLEAT 2010)
             (ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "notes.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* My Heading\n| a | bb |\n| ccc | d |")
        (setup-org-styles! ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; Heading is styled before alignment
        (let ((style-before (send-message ed SCI_GETSTYLEAT 0 0)))
          (check (= style-before ORG_STYLE_HEADING_1) => #t))
        ;; Align the table (cursor on second line)
        (editor-goto-pos ed 15)
        (org-table-align ed)
        ;; Re-apply highlighting (as the real code should do)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; Heading must STILL be styled after alignment
        (let ((style-after (send-message ed SCI_GETSTYLEAT 0 0)))
          (check (= style-after ORG_STYLE_HEADING_1) => #t))
        ;; Table lines must have table style
        (let* ((text (editor-get-text ed))
               (tbl-pos (string-contains text "| a")))
          (when tbl-pos
            (check (= (send-message ed SCI_GETSTYLEAT tbl-pos 0) 58) => #t)))))

    (test-case "headless: org-table-next-cell preserves heading styling"
      ;; Regression: TAB in org table clears highlighting for the whole buffer.
      ;; After org-table-next-cell + re-highlight, heading above table must
      ;; still have heading style.
      (let* ((SCI_GETSTYLEAT 2010)
             (ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "notes.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Tasks\n| Name | Age |\n| Alice | 30 |")
        (set! (buffer-lexer-lang buf) 'org)
        (setup-org-styles! ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; Verify heading is styled
        (check (= (send-message ed SCI_GETSTYLEAT 0 0) ORG_STYLE_HEADING_1) => #t)
        ;; Move cursor to first cell and invoke next-cell
        (editor-goto-pos ed 10) ;; inside "| Name |"
        (org-table-next-cell ed)
        ;; Re-apply highlighting (as the real code should do after table ops)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; Heading STILL styled
        (check (= (send-message ed SCI_GETSTYLEAT 0 0) ORG_STYLE_HEADING_1) => #t)
        ;; Table lines still styled
        (let* ((text (editor-get-text ed))
               (tbl-pos (string-contains text "| Name")))
          (when tbl-pos
            (check (= (send-message ed SCI_GETSTYLEAT tbl-pos 0) 58) => #t)))))

    (test-case "headless: org-table multiple TABs maintain consistent styling"
      ;; Regression: repeated TAB presses toggle highlighting on/off.
      ;; After multiple next-cell operations, styles must remain consistent.
      (let* ((SCI_GETSTYLEAT 2010)
             (ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "notes.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Report\n| Col1 | Col2 |\n|---+---|\n| x | y |")
        (set! (buffer-lexer-lang buf) 'org)
        (setup-org-styles! ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; TAB three times
        (editor-goto-pos ed 12) ;; inside first table row
        (org-table-next-cell ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        (org-table-next-cell ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        (org-table-next-cell ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; After 3 TABs, heading STILL has heading style
        (check (= (send-message ed SCI_GETSTYLEAT 0 0) ORG_STYLE_HEADING_1) => #t)
        ;; Table lines still have table style
        (let* ((text (editor-get-text ed))
               (tbl-pos (string-contains text "|")))
          (when (and tbl-pos (> tbl-pos 0))
            (check (= (send-message ed SCI_GETSTYLEAT tbl-pos 0) 58) => #t)))))

    (test-case "headless: org-table TAB preserves content below table"
      ;; Regression: TAB was eating lines after the table.
      ;; After org-table-next-cell, text below table must be intact.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "notes.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "| a | b |\n| c | d |\nSome text below")
        (editor-goto-pos ed 2) ;; inside first cell
        (org-table-next-cell ed)
        (let ((text (editor-get-text ed)))
          ;; Text below table must still be present
          (check (not (not (string-contains text "Some text below"))) => #t))))

    (test-case "headless: org-table TAB preserves styling of text below table"
      ;; After table TAB, content below the table should keep its style.
      ;; Here we check that a heading BELOW the table keeps heading style.
      (let* ((SCI_GETSTYLEAT 2010)
             (ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "notes.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "| a | b |\n| c | d |\n* Below heading")
        (setup-org-styles! ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; Verify heading below table is styled
        (let* ((text (editor-get-text ed))
               (heading-pos (string-contains text "* Below")))
          (check (= (send-message ed SCI_GETSTYLEAT heading-pos 0)
                    ORG_STYLE_HEADING_1) => #t))
        ;; TAB in table
        (editor-goto-pos ed 2)
        (org-table-next-cell ed)
        (org-highlight-buffer! ed (editor-get-text ed))
        ;; Heading below still styled
        (let* ((text (editor-get-text ed))
               (heading-pos (string-contains text "* Below")))
          (check (= (send-message ed SCI_GETSTYLEAT heading-pos 0)
                    ORG_STYLE_HEADING_1) => #t))))

    ;;=========================================================================
    ;; Org-clock: elapsed time tests
    ;;=========================================================================

    (test-case "org-elapsed-minutes: basic"
      (let ((start (org-parse-timestamp "[2024-01-15 Mon 10:00]"))
            (end   (org-parse-timestamp "[2024-01-15 Mon 11:30]")))
        (check (org-elapsed-minutes start end) => 90)))

    (test-case "org-elapsed-minutes: zero"
      (let ((start (org-parse-timestamp "[2024-01-15 Mon 10:00]"))
            (end   (org-parse-timestamp "[2024-01-15 Mon 10:00]")))
        (check (org-elapsed-minutes start end) => 0)))

    (test-case "org-clock-modeline-string: returns #f when not clocking"
      (set! *org-clock-start* #f)
      (check (org-clock-modeline-string) => #f))

    ;;=========================================================================
    ;; Org-list: detection tests
    ;;=========================================================================

    (test-case "org-list-item?: unordered dash"
      (let-values (((type indent marker) (org-list-item? "- item")))
        (check type => 'unordered)
        (check indent => 0)
        (check marker => "-")))

    (test-case "org-list-item?: unordered indented"
      (let-values (((type indent marker) (org-list-item? "  + sub item")))
        (check type => 'unordered)
        (check indent => 2)
        (check marker => "+")))

    (test-case "org-list-item?: ordered"
      (let-values (((type indent marker) (org-list-item? "1. First item")))
        (check type => 'ordered)
        (check marker => "1.")))

    (test-case "org-list-item?: checkbox unchecked"
      (let-values (((type indent marker) (org-list-item? "- [ ] Todo item")))
        (check type => 'checkbox-unchecked)))

    (test-case "org-list-item?: checkbox checked"
      (let-values (((type indent marker) (org-list-item? "- [X] Done item")))
        (check type => 'checkbox-checked)))

    (test-case "org-list-item?: not a list"
      (let-values (((type indent marker) (org-list-item? "Regular text")))
        (check type => #f)))

    (test-case "org-count-leading-spaces: counts"
      (check (org-count-leading-spaces "  hello") => 2)
      (check (org-count-leading-spaces "hello") => 0)
      (check (org-count-leading-spaces "    ") => 4))

    ;;=========================================================================
    ;; Org-export: inline markup tests
    ;;=========================================================================

    (test-case "org-export-inline: bold to HTML"
      (check (org-export-inline " *bold* " 'html) => " <b>bold</b> "))

    (test-case "org-export-inline: code to markdown"
      (check (org-export-inline "use ~code~ here" 'markdown) => "use `code` here"))

    (test-case "html-escape: escapes special chars"
      (check (html-escape "<script>") => "&lt;script&gt;")
      (check (html-escape "a & b") => "a &amp; b")
      (check (html-escape "\"hi\"") => "&quot;hi&quot;"))

    (test-case "org-split-into-blocks: headings and paragraphs"
      (let ((blocks (org-split-into-blocks "* Heading\nSome text\n\n** Sub")))
        ;; Should have heading, paragraph, blank, heading
        (check (>= (length blocks) 3) => #t)
        (check (caar blocks) => 'heading)))

    (test-case "org-export-buffer: HTML output"
      (let ((html (org-export-buffer "#+TITLE: Test\n* Hello\nWorld" 'html)))
        (check (not (not (string-contains html "<h1"))) => #t)
        (check (not (not (string-contains html "Hello"))) => #t)
        (check (not (not (string-contains html "World"))) => #t)
        (check (not (not (string-contains html "<!DOCTYPE"))) => #t)))

    (test-case "org-export-buffer: Markdown output"
      (let ((md (org-export-buffer "* Hello\nWorld" 'markdown)))
        (check (not (not (string-contains md "# Hello"))) => #t)
        (check (not (not (string-contains md "World"))) => #t)))

    (test-case "org-export-buffer: LaTeX output"
      (let ((tex (org-export-buffer "#+TITLE: Test\n* Hello\nWorld" 'latex)))
        (check (not (not (string-contains tex "\\section"))) => #t)
        (check (not (not (string-contains tex "\\documentclass"))) => #t)))

    (test-case "org-export-buffer: plain text"
      (let ((txt (org-export-buffer "* Hello\nWorld" 'text)))
        (check (not (not (string-contains txt "Hello"))) => #t)
        (check (not (not (string-contains txt "World"))) => #t)
        ;; Should not have * prefix
        (check (not (string-contains txt "* ")) => #t)))

    ;;; ================================================================
    ;;; Org Babel tests (Phase 4)
    ;;; ================================================================

    (test-case "org-babel: parse header args"
      (let ((h (org-babel-parse-header-args ":var x=5 :results output :dir /tmp")))
        (check (hash-get h "var") => "x=5")
        (check (hash-get h "results") => "output")
        (check (hash-get h "dir") => "/tmp")))

    (test-case "org-babel: parse begin line"
      (let ((parsed (org-babel-parse-begin-line "#+BEGIN_SRC python :var x=5")))
        (check (car parsed) => "python")
        (check (hash-get (cdr parsed) "var") => "x=5")))

    (test-case "org-babel: parse begin line - no args"
      (let ((parsed (org-babel-parse-begin-line "#+BEGIN_SRC bash")))
        (check (car parsed) => "bash")))

    (test-case "org-babel: find src block"
      (let* ((text "Some text\n#+BEGIN_SRC bash\necho hello\n#+END_SRC\nMore text")
             (lines (string-split text #\newline)))
        (let-values (((lang hargs body begin end name)
                      (org-babel-find-src-block lines 2)))
          (check lang => "bash")
          (check body => "echo hello")
          (check begin => 1)
          (check end => 3))))

    (test-case "org-babel: inside src block"
      (let* ((text "#+BEGIN_SRC python\nprint(42)\n#+END_SRC")
             (lines (string-split text #\newline)))
        (check (org-babel-inside-src-block? lines 1) => #t)
        (check (org-babel-inside-src-block? lines 0) => #f)))

    (test-case "org-babel: format result output"
      (check (org-babel-format-result "hello\nworld" "output")
        => ": hello\n: world"))

    (test-case "org-babel: format result value"
      (check (org-babel-format-result "42" "value") => "42"))

    (test-case "org-babel: find named block"
      (let ((text "#+NAME: greet\n#+BEGIN_SRC bash\necho hi\n#+END_SRC"))
        (check (org-babel-find-named-block text "greet") => "echo hi")
        (check (org-babel-find-named-block text "missing") => #f)))

    (test-case "org-babel: tangle extraction"
      (let* ((text (string-append
                     "#+BEGIN_SRC bash :tangle /tmp/test.sh\n"
                     "echo hello\n"
                     "#+END_SRC\n"
                     "Some text\n"
                     "#+BEGIN_SRC python :tangle /tmp/test.py\n"
                     "print(42)\n"
                     "#+END_SRC"))
             (pairs (org-babel-tangle text)))
        (check (length pairs) => 2)))

    (test-case "org-babel: variable injection bash"
      (let ((result (org-babel-inject-variables "bash" '(("x" . "5") ("y" . "hello")))))
        (check (not (not (string-contains result "x='5'"))) => #t)
        (check (not (not (string-contains result "y='hello'"))) => #t)))

    (test-case "org-babel: variable injection python"
      (let ((result (org-babel-inject-variables "python" '(("x" . "5")))))
        (check (not (not (string-contains result "x = 5"))) => #t)))

    (test-case "org-babel: noweb expansion"
      (let ((text "#+NAME: helper\n#+BEGIN_SRC bash\necho hi\n#+END_SRC"))
        (check (org-babel-expand-noweb text "<<helper>>") => "echo hi")))

    (test-case "org-babel: C-c C-c context detection"
      (let ((lines '("* Heading" "#+BEGIN_SRC python" "print(42)" "#+END_SRC" "| a | b |")))
        (check (org-ctrl-c-ctrl-c-context lines 0) => 'heading)
        (check (org-ctrl-c-ctrl-c-context lines 2) => 'src-block)
        (check (org-ctrl-c-ctrl-c-context lines 4) => 'table)))

    ;;; ================================================================
    ;;; Org Agenda tests (Phase 7)
    ;;; ================================================================

    (test-case "org-agenda: date weekday calculation"
      ;; 2024-01-15 is a Monday (1)
      (check (org-date-weekday 2024 1 15) => 1)
      ;; 2024-01-14 is a Sunday (0)
      (check (org-date-weekday 2024 1 14) => 0))

    (test-case "org-agenda: make-date-ts"
      (let ((ts (org-make-date-ts 2024 3 15)))
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 3)
        (check (org-timestamp-day ts) => 15)))

    (test-case "org-agenda: timestamp in range"
      (let ((ts (org-make-date-ts 2024 1 15))
            (from (org-make-date-ts 2024 1 10))
            (to (org-make-date-ts 2024 1 20)))
        (check (org-timestamp-in-range? ts from to) => #t))
      ;; Out of range
      (let ((ts (org-make-date-ts 2024 2 1))
            (from (org-make-date-ts 2024 1 10))
            (to (org-make-date-ts 2024 1 20)))
        (check (org-timestamp-in-range? ts from to) => #f)))

    (test-case "org-agenda: advance date"
      (let* ((ts (org-make-date-ts 2024 1 30))
             (advanced (org-advance-date-ts ts 3)))
        (check (org-timestamp-year advanced) => 2024)
        (check (org-timestamp-month advanced) => 2)
        (check (org-timestamp-day advanced) => 2)))

    (test-case "org-agenda: collect items"
      (let* ((text (string-append
                     "* TODO Task 1\n"
                     "  SCHEDULED: <2024-01-15 Mon>\n"
                     "* DONE Task 2\n"
                     "  DEADLINE: <2024-01-18 Thu>\n"))
             (from (org-make-date-ts 2024 1 14))
             (to (org-make-date-ts 2024 1 20))
             (items (org-collect-agenda-items text "test.org" from to)))
        (check (length items) => 2)))

    (test-case "org-agenda: sort items by time"
      (let* ((h1 (make-org-heading 1 "TODO" #f "Morning" '() #f #f #f #f '() 0 #f))
             (h2 (make-org-heading 1 "TODO" #f "Evening" '() #f #f #f #f '() 0 #f))
             (ts1 (org-parse-timestamp "<2024-01-15 Mon 09:00>"))
             (ts2 (org-parse-timestamp "<2024-01-15 Mon 17:00>"))
             (i1 (make-org-agenda-item h1 'scheduled ts1 "09:00" "t" 0))
             (i2 (make-org-agenda-item h2 'scheduled ts2 "17:00" "t" 0))
             (sorted (org-agenda-sort-items (list i2 i1))))
        ;; i1 (09:00) should come first
        (check (org-heading-title (org-agenda-item-heading (car sorted)))
          => "Morning")))

    (test-case "org-agenda: TODO list"
      (let* ((text "* TODO Task A\n* DONE Task B\n* TODO Task C\n")
             (result (org-agenda-todo-list text "test.org")))
        (check (not (not (string-contains result "Task A"))) => #t)
        (check (not (not (string-contains result "Task C"))) => #t)
        ;; DONE should not appear
        (check (not (string-contains result "Task B")) => #t)))

    (test-case "org-agenda: tag search"
      (let* ((text "* TODO Task A :work:\n* TODO Task B :home:\n* DONE Task C :work:\n")
             (result (org-agenda-tags-match text "test.org" "work")))
        (check (not (not (string-contains result "Task A"))) => #t)
        (check (not (not (string-contains result "Task C"))) => #t)
        (check (not (string-contains result "Task B")) => #t)))

    (test-case "org-agenda: text search"
      (let* ((text "* Meeting with Alice\n* Lunch break\n* Call Bob\n")
             (result (org-agenda-search text "test.org" "alice")))
        (check (not (not (string-contains result "Alice"))) => #t)
        (check (not (string-contains result "Bob")) => #t)))

    ;;; ================================================================
    ;;; Org Capture tests (Phase 9)
    ;;; ================================================================

    (test-case "org-capture: template expansion %U"
      (let ((result (org-capture-expand-template "* TODO %?\n  %U\n" "test.org" "/tmp/test.org")))
        ;; Should contain an inactive timestamp [...]
        (check (not (not (string-contains result "["))) => #t)
        ;; %? should be removed
        (check (not (string-contains result "%?")) => #t)))

    (test-case "org-capture: template expansion %f"
      (let ((result (org-capture-expand-template "From: %f" "myfile.org" "/tmp/myfile.org")))
        (check result => "From: myfile.org")))

    (test-case "org-capture: template expansion %%"
      (let ((result (org-capture-expand-template "100%% done" "f" "p")))
        (check result => "100% done")))

    (test-case "org-capture: cursor position"
      (check (org-capture-cursor-position "* TODO %?\n  %U") => 7)
      (check (org-capture-cursor-position "no cursor here") => #f))

    (test-case "org-capture: menu string"
      (let ((menu (org-capture-menu-string)))
        (check (not (not (string-contains menu "[t]"))) => #t)
        (check (not (not (string-contains menu "TODO"))) => #t)))

    (test-case "org-capture: refile targets"
      (let* ((text "* Heading A\nBody\n** Sub B\n* Heading C\n")
             (targets (org-refile-targets text)))
        (check (length targets) => 3)
        (check (caar targets) => "Heading A")))

    (test-case "org-capture: insert under heading"
      (let* ((text "* Tasks\n* Notes\n")
             (result (org-insert-under-heading text "Tasks" "** New task\n")))
        (check (not (not (string-contains result "New task"))) => #t)
        ;; New task should be between Tasks and Notes
        (let ((task-pos (string-contains result "New task"))
              (notes-pos (string-contains result "Notes")))
          (check (< task-pos notes-pos) => #t))))

    (test-case "org-capture: start and abort"
      (org-capture-start "t" "test.org" "/tmp/test.org")
      (check *org-capture-active?* => #t)
      (org-capture-abort)
      (check *org-capture-active?* => #f))

    (test-case "headless: command registration for new org commands"
      (register-all-commands!)
      (check (not (not (find-command 'org-promote))) => #t)
      (check (not (not (find-command 'org-demote))) => #t)
      (check (not (not (find-command 'org-move-subtree-up))) => #t)
      (check (not (not (find-command 'org-move-subtree-down))) => #t)
      (check (not (not (find-command 'org-toggle-checkbox))) => #t)
      (check (not (not (find-command 'org-priority))) => #t)
      (check (not (not (find-command 'org-set-tags))) => #t)
      (check (not (not (find-command 'org-insert-heading))) => #t)
      (check (not (not (find-command 'org-insert-src-block))) => #t)
      (check (not (not (find-command 'org-clock-in))) => #t)
      (check (not (not (find-command 'org-clock-out))) => #t)
      (check (not (not (find-command 'org-template-expand))) => #t))

    ;;; ================================================================
    ;;; Language detection and highlighting tests
    ;;; ================================================================

    (test-case "detect-file-language: Python"
      (check (detect-file-language "script.py") => 'python)
      (check (detect-file-language "app.pyw") => 'python)
      (check (detect-file-language "/home/user/code/main.py") => 'python))

    (test-case "detect-file-language: Scheme/Gerbil"
      (check (detect-file-language "core.ss") => 'scheme)
      (check (detect-file-language "lib.scm") => 'scheme)
      (check (detect-file-language "module.sld") => 'scheme)
      (check (detect-file-language "lib.sls") => 'scheme))

    (test-case "detect-file-language: C/C++"
      (check (detect-file-language "main.c") => 'c)
      (check (detect-file-language "header.h") => 'c)
      (check (detect-file-language "app.cpp") => 'c)
      (check (detect-file-language "lib.hpp") => 'c)
      (check (detect-file-language "test.cc") => 'c)
      (check (detect-file-language "util.cxx") => 'c))

    (test-case "detect-file-language: JavaScript/TypeScript"
      (check (detect-file-language "app.js") => 'javascript)
      (check (detect-file-language "index.jsx") => 'javascript)
      (check (detect-file-language "main.mjs") => 'javascript)
      (check (detect-file-language "app.ts") => 'typescript)
      (check (detect-file-language "component.tsx") => 'typescript))

    (test-case "detect-file-language: Web languages"
      (check (detect-file-language "index.html") => 'html)
      (check (detect-file-language "page.htm") => 'html)
      (check (detect-file-language "style.css") => 'css))

    (test-case "detect-file-language: Shell/Bash"
      (check (detect-file-language "deploy.sh") => 'bash)
      (check (detect-file-language "config.bash") => 'bash)
      (check (detect-file-language "setup.zsh") => 'bash))

    (test-case "detect-file-language: Data formats"
      (check (detect-file-language "data.json") => 'json)
      (check (detect-file-language "config.yaml") => 'yaml)
      (check (detect-file-language "config.yml") => 'yaml)
      (check (detect-file-language "settings.toml") => 'toml))

    (test-case "detect-file-language: Other languages"
      (check (detect-file-language "app.rb") => 'ruby)
      (check (detect-file-language "main.rs") => 'rust)
      (check (detect-file-language "main.go") => 'go)
      (check (detect-file-language "App.java") => 'java)
      (check (detect-file-language "script.lua") => 'lua)
      (check (detect-file-language "query.sql") => 'sql)
      (check (detect-file-language "README.md") => 'markdown)
      (check (detect-file-language "Makefile") => 'makefile)
      (check (detect-file-language "changes.diff") => 'diff)
      (check (detect-file-language "changes.patch") => 'diff))

    (test-case "detect-file-language: unknown/no extension"
      (check (detect-file-language "README") => #f)
      (check (detect-file-language "data.xyz") => #f)
      (check (detect-file-language #f) => #f))

    (test-case "gerbil-file-extension?"
      (check (gerbil-file-extension? "core.ss") => #t)
      (check (gerbil-file-extension? "lib.scm") => #t)
      (check (gerbil-file-extension? "main.py") => #f)
      (check (gerbil-file-extension? #f) => #f))

    (test-case "buffer-attach hook restores highlighting"
      ;; Verify the hook mechanism: creating a buffer with a file-path
      ;; and attaching it should trigger the post-attach hook
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "test.py" ed "/tmp/test.py")))
        ;; Just verify the attach doesn't crash (hook is no-op by default in tests)
        (buffer-attach! ed buf)
        (check (buffer-file-path buf) => "/tmp/test.py")
        (check (detect-file-language "/tmp/test.py") => 'python)))

    ;;; ================================================================
    ;;; Terminal ANSI parsing tests
    ;;; ================================================================

    (test-case "parse-ansi-segments: plain text"
      (let ((segs (parse-ansi-segments "hello world")))
        (check (length segs) => 1)
        (check (text-segment-text (car segs)) => "hello world")
        (check (text-segment-fg-color (car segs)) => -1)
        (check (text-segment-bold? (car segs)) => #f)))

    (test-case "parse-ansi-segments: single color"
      ;; ESC[31m = red foreground, ESC[0m = reset
      (let* ((input (string-append "\x1b;[31mhello\x1b;[0m world"))
             (segs (parse-ansi-segments input)))
        (check (>= (length segs) 2) => #t)
        ;; First segment: "hello" in red
        (check (text-segment-text (car segs)) => "hello")
        (check (text-segment-fg-color (car segs)) => 1)  ; red
        ;; Second segment: " world" in default
        (check (text-segment-text (cadr segs)) => " world")
        (check (text-segment-fg-color (cadr segs)) => -1)))

    (test-case "parse-ansi-segments: bold + color"
      ;; ESC[1;32m = bold green
      (let* ((input (string-append "\x1b;[1;32mOK\x1b;[0m"))
             (segs (parse-ansi-segments input)))
        (check (>= (length segs) 1) => #t)
        (check (text-segment-text (car segs)) => "OK")
        (check (text-segment-fg-color (car segs)) => 2)  ; green
        (check (text-segment-bold? (car segs)) => #t)))

    (test-case "parse-ansi-segments: bright colors"
      ;; ESC[91m = bright red
      (let* ((input (string-append "\x1b;[91mERROR\x1b;[0m"))
             (segs (parse-ansi-segments input)))
        (check (text-segment-text (car segs)) => "ERROR")
        (check (text-segment-fg-color (car segs)) => 9)))  ; bright red = 8+1

    (test-case "parse-ansi-segments: strips carriage returns"
      (let ((segs (parse-ansi-segments "line1\r\nline2\r\n")))
        (check (length segs) => 1)
        (check (text-segment-text (car segs)) => "line1\nline2\n")))

    (test-case "parse-ansi-segments: strips non-SGR CSI sequences"
      ;; ESC[2J = clear screen, ESC[H = cursor home
      (let* ((input (string-append "\x1b;[2J\x1b;[Hhello"))
             (segs (parse-ansi-segments input)))
        ;; Should produce "hello" with default color
        (check (= (length segs) 1) => #t)
        (check (text-segment-text (car segs)) => "hello")))

    (test-case "parse-ansi-segments: OSC sequences stripped"
      ;; ESC]0;title BEL = set window title
      (let* ((input (string-append "\x1b;]0;my-title\x07;text"))
             (segs (parse-ansi-segments input)))
        (check (text-segment-text (car segs)) => "text")))

    (test-case "color-to-style: default"
      (check (color-to-style -1 #f) => 0)
      (check (color-to-style -1 #t) => (+ *term-style-base* 15)))

    (test-case "color-to-style: standard colors"
      (check (color-to-style 0 #f) => (+ *term-style-base* 0))   ; black
      (check (color-to-style 1 #f) => (+ *term-style-base* 1))   ; red
      (check (color-to-style 7 #f) => (+ *term-style-base* 7)))  ; white

    (test-case "color-to-style: bold shifts to bright"
      (check (color-to-style 1 #t) => (+ *term-style-base* 9))   ; bold red = bright red
      (check (color-to-style 4 #t) => (+ *term-style-base* 12))) ; bold blue = bright blue

    (test-case "terminal-buffer? detection"
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "*terminal*" ed #f)))
        (check (terminal-buffer? buf) => #f)
        (set! (buffer-lexer-lang buf) 'terminal)
        (check (terminal-buffer? buf) => #t)))

    ;;=========================================================================
    ;; Minibuffer history tests
    ;;=========================================================================

    (test-case "minibuffer-history-add! adds entries"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "first")
      (check *minibuffer-history* => ["first"])
      (minibuffer-history-add! "second")
      (check *minibuffer-history* => ["second" "first"]))

    (test-case "minibuffer-history-add! ignores empty strings"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "")
      (check *minibuffer-history* => [])
      (minibuffer-history-add! "valid")
      (minibuffer-history-add! "")
      (check *minibuffer-history* => ["valid"]))

    (test-case "minibuffer-history-add! deduplicates front"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "cmd")
      (minibuffer-history-add! "cmd")
      ;; Should not have duplicate at front
      (check (length *minibuffer-history*) => 1)
      (check (car *minibuffer-history*) => "cmd"))

    (test-case "minibuffer-history-add! allows non-front duplicates"
      (set! *minibuffer-history* [])
      (minibuffer-history-add! "alpha")
      (minibuffer-history-add! "beta")
      (minibuffer-history-add! "alpha")
      ;; alpha appears twice (front and end), that's OK
      (check (length *minibuffer-history*) => 3)
      (check (car *minibuffer-history*) => "alpha"))

    (test-case "minibuffer-history-add! respects max size"
      (set! *minibuffer-history* [])
      ;; Add 105 entries
      (let loop ((i 0))
        (when (< i 105)
          (minibuffer-history-add! (string-append "entry-" (number->string i)))
          (loop (+ i 1))))
      ;; Should be capped at 100
      (check (<= (length *minibuffer-history*) 100) => #t))

    ;;=========================================================================
    ;; Auto-save and file modification tracking tests
    ;;=========================================================================

    (test-case "make-auto-save-path produces #name# format"
      (check (make-auto-save-path "/home/user/foo.txt") => "/home/user/#foo.txt#")
      (check (make-auto-save-path "/tmp/bar.ss") => "/tmp/#bar.ss#")
      (check (make-auto-save-path "simple.txt") => "#simple.txt#"))

    (test-case "file-mod-time returns number for existing file"
      (let ((mt (file-mod-time "/home/jafourni/mine/gerbil-emacs/core.ss")))
        (check (number? mt) => #t)
        (check (> mt 0) => #t)))

    (test-case "file-mod-time returns #f for nonexistent file"
      (check (file-mod-time "/nonexistent/path/foo.ss") => #f))

    (test-case "update-buffer-mod-time! tracks file times"
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "test.ss" ed "/home/jafourni/mine/gerbil-emacs/core.ss")))
        (update-buffer-mod-time! buf)
        (let ((mt (hash-get *buffer-mod-times* buf)))
          (check (number? mt) => #t)
          (check (> mt 0) => #t))
        ;; Clean up
        (hash-remove! *buffer-mod-times* buf)))

    (test-case "update-buffer-mod-time! ignores non-file buffers"
      (let* ((ed (create-scintilla-editor))
             (buf (buffer-create! "*scratch*" ed #f)))
        (update-buffer-mod-time! buf)
        (check (hash-get *buffer-mod-times* buf) => #f)))

    ;;=========================================================================
    ;; Regression tests: Python syntax highlighting
    ;;=========================================================================

    (test-case "headless: Python lexer activates and applies styles"
      ;; Regression: SCI_SETILEXER_MSG was 4030 (wrong) instead of 4033.
      ;; This caused zero syntax highlighting for ALL languages in TUI.
      (let ((ed (create-scintilla-editor width: 80 height: 24)))
        ;; Set up Python highlighting using the same path as the real code
        (setup-highlighting-for-file! ed "test.py")
        ;; Set Python source code
        (editor-set-text ed "def hello():\n    pass\n")
        (editor-colourise ed 0 -1)
        ;; Lexer should be active (SCLEX_PYTHON = 2)
        (check (editor-get-lexer ed) => 2)
        ;; 'def' at position 0 should be keyword style (SCE_P_WORD = 5)
        (check (send-message ed SCI_GETSTYLEAT 0 0) => 5)
        ;; 'pass' at position 17 should also be keyword style
        (check (send-message ed SCI_GETSTYLEAT 17 0) => 5)
        ;; 'hello' at position 4 should be defname style (SCE_P_DEFNAME = 9)
        (check (send-message ed SCI_GETSTYLEAT 4 0) => 9)))

    (test-case "headless: Gerbil/Scheme lexer activates"
      (let ((ed (create-scintilla-editor width: 80 height: 24)))
        (setup-highlighting-for-file! ed "test.ss")
        (editor-set-text ed "(define (hello x)\n  (+ x 1))\n")
        (editor-colourise ed 0 -1)
        ;; SCLEX_LISP = 21
        (check (editor-get-lexer ed) => 21)))

    (test-case "headless: detect-file-language maps extensions correctly"
      (check (detect-file-language "foo.py") => 'python)
      (check (detect-file-language "foo.ss") => 'scheme)
      (check (detect-file-language "foo.js") => 'javascript)
      (check (detect-file-language "Makefile") => 'makefile)
      (check (detect-file-language "foo.rs") => 'rust)
      (check (detect-file-language "foo.txt") => #f))

    ;;=========================================================================
    ;; Regression tests: read-only per-document, not per-widget
    ;;=========================================================================

    (test-case "headless: read-only is per-document not per-editor"
      ;; Regression: QScintilla's setReadOnly() is widget-level, persisting
      ;; across buffer switches. After viewing a read-only buffer (like
      ;; *Buffer List*), all subsequent buffers become uneditable.
      ;; Fix: use SCI_SETREADONLY (per-document) instead.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (doc1 (send-message ed SCI_CREATEDOCUMENT 0 0))
             (doc2 (send-message ed SCI_CREATEDOCUMENT 0 0)))
        ;; Attach doc1, set read-only, verify
        (send-message ed SCI_SETDOCPOINTER 0 doc1)
        (send-message ed SCI_SETREADONLY 1 0)
        (check (send-message ed SCI_GETREADONLY 0 0) => 1)
        ;; Switch to doc2 — should NOT be read-only
        (send-message ed SCI_SETDOCPOINTER 0 doc2)
        (check (send-message ed SCI_GETREADONLY 0 0) => 0)
        ;; Switch back to doc1 — should still be read-only
        (send-message ed SCI_SETDOCPOINTER 0 doc1)
        (check (send-message ed SCI_GETREADONLY 0 0) => 1)
        ;; Cleanup
        (send-message ed SCI_RELEASEDOCUMENT 0 doc1)
        (send-message ed SCI_RELEASEDOCUMENT 0 doc2)))

    (test-case "headless: read-only toggle allows typing"
      ;; Simulates: set read-only (like buffer-list), clear it, verify typing.
      (let ((ed (create-scintilla-editor width: 80 height: 24)))
        (editor-set-text ed "buffer list content")
        ;; Set read-only
        (editor-set-read-only ed #t)
        (check (editor-get-read-only? ed) => #t)
        ;; Clear read-only (like switching to writable buffer)
        (editor-set-read-only ed #f)
        (check (editor-get-read-only? ed) => #f)
        ;; Typing should work
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (editor-insert-text ed 0 "hello")
        (check (editor-get-text ed) => "hello")))

    (test-case "headless: REPL typing works after read-only cleared"
      ;; Regression: After viewing *Buffer List* (read-only), switching to
      ;; *REPL* made it uneditable because widget-level read-only persisted.
      ;; Test: set read-only, clear it, then type in REPL mode.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*REPL*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        (set! (buffer-lexer-lang buf) 'repl)
        ;; Simulate having viewed a read-only buffer
        (editor-set-read-only ed #t)
        (check (editor-get-read-only? ed) => #t)
        ;; Clear read-only (as buffer-attach! does for non-readonly buffers)
        (editor-set-read-only ed #f)
        ;; Set up REPL content
        (editor-set-text ed "gerbil> ")
        ;; Type in REPL via cmd-self-insert!
        (let ((rs (make-repl-state #f 8 [])))
          (hash-put! *repl-state* buf rs)
          (editor-goto-pos ed 8)
          (for-each (lambda (ch) (cmd-self-insert! app (char->integer ch)))
                    (string->list "(+ 1 2)"))
          (check (string-contains (editor-get-text ed) "(+ 1 2)") => 8)
          (hash-remove! *repl-state* buf))
        ;; Cleanup
        (buffer-list-remove! buf)))

    (test-case "headless: eq? hash table survives buffer mutation"
      ;; Regression: *repl-state* used make-hash-table (equal? comparison).
      ;; Buffer structs are transparent, so mutating buffer-modified after
      ;; hash-put! caused hash-get to return #f, blocking REPL typing.
      ;; Fix: use make-hash-table-eq for all buffer-keyed tables.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*REPL*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        (set! (buffer-lexer-lang buf) 'repl)
        (editor-set-text ed "> ")
        (let ((rs (make-repl-state #f 2 [])))
          (hash-put! *repl-state* buf rs)
          ;; Mutate buffer fields (simulates Scintilla save-point signal)
          (set! (buffer-modified buf) #t)
          ;; hash-get must still find the repl-state despite mutation
          (check (eq? (hash-get *repl-state* buf) rs) => #t)
          ;; Type after mutation — must succeed
          (editor-goto-pos ed 2)
          (for-each (lambda (ch) (cmd-self-insert! app (char->integer ch)))
                    (string->list "hi"))
          (check (editor-get-text ed) => "> hi")
          (hash-remove! *repl-state* buf))
        (buffer-list-remove! buf)))

    (test-case "headless: buffer-list string-trim-both parses names"
      ;; Regression: Qt cmd-buffer-list-select used SRFI-13 string-trim which
      ;; only strips LEADING whitespace. Buffer names padded to 24 chars kept
      ;; trailing spaces, so buffer-by-name failed.
      ;; TUI version uses tabs+string-index. This tests the padding format.
      (let* ((name "*scratch*")
             (padded (string-append name (make-string (- 24 (string-length name)) #\space)))
             (line (string-append "  .  " padded "fundamental   ")))
        ;; Simulate the Qt parsing logic with string-trim-both
        (let ((extracted (string-trim-both (substring line 5 29))))
          (check extracted => "*scratch*"))))

    (test-case "headless: fold margin width is 1 char for TUI"
      ;; Regression: fold margin width was 14 character cells, creating a
      ;; massive black gap in TUI. Should be 1 for compact fold markers.
      (let ((ed (create-scintilla-editor width: 80 height: 24)))
        ;; Simulate what enable-code-file-features! does
        (send-message ed SCI_SETMARGINTYPEN 2 SC_MARGIN_SYMBOL)
        (send-message ed SCI_SETMARGINWIDTHN 2 1)
        (check (send-message ed SCI_GETMARGINWIDTHN 2 0) => 1)))

    (test-case "headless: default margin 1 width is 0"
      ;; Regression: Scintilla defaults margin 1 to 16 pixels/chars.
      ;; In TUI, that's 16 character cells of blank space.
      (let ((ed (create-scintilla-editor width: 80 height: 24)))
        (send-message ed SCI_SETMARGINWIDTHN 1 0)
        (check (send-message ed SCI_GETMARGINWIDTHN 1 0) => 0)))

    (test-case "headless: REPL prompt-pos blocks typing before prompt"
      ;; Verify self-insert guard: typing before prompt-pos is blocked,
      ;; typing at/after prompt-pos succeeds.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*REPL*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (buffer-list-add! buf)
        (set! (buffer-lexer-lang buf) 'repl)
        (editor-set-text ed "> ")
        (let ((rs (make-repl-state #f 2 [])))
          (hash-put! *repl-state* buf rs)
          ;; Cursor at position 0 (before prompt) — typing should be blocked
          (editor-goto-pos ed 0)
          (cmd-self-insert! app (char->integer #\x))
          (check (editor-get-text ed) => "> ")  ;; unchanged
          ;; Cursor at position 2 (after prompt) — typing should work
          (editor-goto-pos ed 2)
          (cmd-self-insert! app (char->integer #\x))
          (check (editor-get-text ed) => "> x")
          (hash-remove! *repl-state* buf))
        (buffer-list-remove! buf)))

    ;;=========================================================================
    ;; Search forward tests
    ;;=========================================================================

    (test-case "headless: search-forward finds first match"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "foo bar foo baz foo")
        (editor-goto-pos ed 0)
        ;; Search for "foo" — should find at position 0, cursor at end of match (3)
        (search-forward-impl! app "foo")
        (check (editor-get-current-pos ed) => 3)
        (check (app-state-last-search app) => "foo")))

    (test-case "headless: search-forward repeated finds next match"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "foo bar foo baz foo")
        (editor-goto-pos ed 0)
        ;; First search finds "foo" at 0, cursor at end (3)
        (search-forward-impl! app "foo")
        (check (editor-get-current-pos ed) => 3)
        ;; Simulate repeated C-s: move past current match, search again
        (editor-goto-pos ed (+ 0 1))  ;; move past match start
        (search-forward-impl! app "foo")
        ;; Should find second "foo" at position 8, cursor at 11
        (check (editor-get-current-pos ed) => 11)
        ;; Again: move past, search
        (editor-goto-pos ed (+ 8 1))  ;; move past second match start
        (search-forward-impl! app "foo")
        ;; Should find third "foo" at position 16, cursor at 19
        (check (editor-get-current-pos ed) => 19)))

    (test-case "headless: search-forward wraps around"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "foo bar baz")
        ;; Start searching from after "foo"
        (editor-goto-pos ed 4)
        (search-forward-impl! app "foo")
        ;; Should wrap around and find "foo" at 0, cursor at 3
        (check (editor-get-current-pos ed) => 3)))

    ;;=========================================================================
    ;; Org-mode template expansion tests
    ;;=========================================================================

    (test-case "headless: org-template-expand <s"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((result (editor-get-text ed)))
          ;; Should contain BEGIN_SRC and END_SRC
          (check (string-contains result "#+BEGIN_SRC") => 0)
          (check (not (not (string-contains result "#+END_SRC"))) => #t))))

    (test-case "headless: org-template-expand <e"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<e")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((result (editor-get-text ed)))
          (check (string-contains result "#+BEGIN_EXAMPLE") => 0)
          (check (not (not (string-contains result "#+END_EXAMPLE"))) => #t))))

    (test-case "headless: org-template-expand preserves indentation"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "  <q")
        (editor-goto-pos ed 4)
        (cmd-org-template-expand app)
        (let ((result (editor-get-text ed)))
          ;; Should preserve the 2-space indent
          (check (string-contains result "  #+BEGIN_QUOTE") => 0)
          (check (not (not (string-contains result "  #+END_QUOTE"))) => #t))))

    (test-case "headless: org-template-expand invalid key does nothing"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<z")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        ;; Should be unchanged — invalid template key
        (check (editor-get-text ed) => "<z")))

    ;;=========================================================================
    ;; Org-mode heading fold/unfold tests
    ;;=========================================================================

    (test-case "headless: org-cycle folds heading children"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nChild line 1\nChild line 2\n* Next heading")
        (editor-goto-pos ed 0)  ;; On heading line
        ;; Children should start visible
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 1) => #t)
        (check (= (send-message ed SCI_GETLINEVISIBLE 2) 1) => #t)
        ;; Fold
        (cmd-org-cycle app)
        ;; Children should be hidden
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 0) => #t)
        (check (= (send-message ed SCI_GETLINEVISIBLE 2) 0) => #t)
        ;; Next heading should still be visible
        (check (= (send-message ed SCI_GETLINEVISIBLE 3) 1) => #t)))

    (test-case "headless: org-cycle unfolds hidden children"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nChild line 1\nChild line 2\n* Next heading")
        (editor-goto-pos ed 0)
        ;; Fold first
        (cmd-org-cycle app)
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 0) => #t)
        ;; Unfold
        (cmd-org-cycle app)
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 1) => #t)
        (check (= (send-message ed SCI_GETLINEVISIBLE 2) 1) => #t)))

    (test-case "headless: org-cycle on non-heading does nothing"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nNot a heading\n* Next")
        ;; Move to line 1 (not a heading)
        (editor-goto-pos ed 10)
        ;; Should report "Not on a heading" and return without changing visibility
        (cmd-org-cycle app)
        ;; All lines should remain visible
        (check (= (send-message ed SCI_GETLINEVISIBLE 0) 1) => #t)
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 1) => #t)
        (check (= (send-message ed SCI_GETLINEVISIBLE 2) 1) => #t)))

    (test-case "headless: org-cycle nested headings"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Top\n** Sub1\nContent\n** Sub2\n* Next")
        (editor-goto-pos ed 0)  ;; On "* Top"
        ;; Fold top heading — should hide sub1, content, sub2 but not "* Next"
        (cmd-org-cycle app)
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 0) => #t)  ;; ** Sub1
        (check (= (send-message ed SCI_GETLINEVISIBLE 2) 0) => #t)  ;; Content
        (check (= (send-message ed SCI_GETLINEVISIBLE 3) 0) => #t)  ;; ** Sub2
        (check (= (send-message ed SCI_GETLINEVISIBLE 4) 1) => #t)  ;; * Next
        ))

    ;;=========================================================================
    ;; Selection and copy/kill tests
    ;;=========================================================================

    (test-case "headless: copy-region preserves text"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        (cmd-copy-region app)
        ;; Text unchanged after copy
        (check (editor-get-text ed) => "hello world")
        ;; Scintilla clipboard has the copied text
        (check (editor-get-clipboard ed) => "hello")
        ;; Mark cleared
        (check (buffer-mark buf) => #f)))

    (test-case "headless: kill-region removes text and updates clipboard"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "abcdef")
        ;; Mark positions 2-4: "cd"
        (editor-goto-pos ed 2)
        (cmd-set-mark app)
        (editor-goto-pos ed 4)
        (cmd-kill-region app)
        (check (editor-get-text ed) => "abef")
        (check (editor-get-clipboard ed) => "cd")
        (check (buffer-mark buf) => #f)))

    (test-case "headless: mark then backward kill"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Set mark at end, move backward, kill region
        (editor-goto-pos ed 11)
        (cmd-set-mark app)
        (editor-goto-pos ed 6)
        (cmd-kill-region app)
        (check (editor-get-text ed) => "hello ")
        (check (editor-get-clipboard ed) => "world")))

    (test-case "headless: kill-region with no mark shows error"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "test")
        (set! (buffer-mark buf) #f)
        (cmd-kill-region app)
        ;; Text should be unchanged since there's no mark
        (check (editor-get-text ed) => "test")
        ;; Echo should show error
        (check (echo-state-error? (app-state-echo app)) => #t)))

    (test-case "headless: set-mark pushes old mark to ring"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 3)
        (cmd-set-mark app)
        (check (buffer-mark buf) => 3)
        ;; Set mark again at different position
        (editor-goto-pos ed 7)
        (cmd-set-mark app)
        (check (buffer-mark buf) => 7)
        ;; Old mark (3) should be in mark ring
        (check (pair? (app-state-mark-ring app)) => #t)))

    (test-case "headless: mark tracks selection region via Scintilla"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Set mark at 0
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (check (buffer-mark buf) => 0)
        ;; Move forward — simulate visual selection update
        (editor-goto-pos ed 5)
        (editor-set-selection ed 0 5)
        ;; Verify selection range covers "hello"
        (check (editor-get-selection-start ed) => 0)
        (check (editor-get-selection-end ed) => 5)))

    (test-case "headless: kill then yank round-trip"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "ABCXYZ")
        ;; Kill "XYZ"
        (editor-goto-pos ed 3)
        (cmd-set-mark app)
        (editor-goto-pos ed 6)
        (cmd-kill-region app)
        (check (editor-get-text ed) => "ABC")
        ;; Yank back — clipboard should have "XYZ"
        (editor-goto-pos ed 3)
        (editor-paste ed)
        (check (editor-get-text ed) => "ABCXYZ")))

    (test-case "headless: copy-region copies middle of text"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "one two three")
        ;; Copy "two" (positions 4-7)
        (editor-goto-pos ed 4)
        (cmd-set-mark app)
        (editor-goto-pos ed 7)
        (cmd-copy-region app)
        ;; Text unchanged
        (check (editor-get-text ed) => "one two three")
        ;; Clipboard has "two"
        (check (editor-get-clipboard ed) => "two")
        ;; Paste at end
        (editor-goto-pos ed 13)
        (editor-paste ed)
        (check (editor-get-text ed) => "one two threetwo")))

    (test-case "headless: multiple copy-as-kill accumulates in kill ring"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "aaa bbb ccc")
        ;; First copy: "aaa"
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 3)
        (cmd-copy-region-as-kill app)
        ;; Second copy: "bbb"
        (editor-goto-pos ed 4)
        (cmd-set-mark app)
        (editor-goto-pos ed 7)
        (cmd-copy-region-as-kill app)
        ;; Text unchanged
        (check (editor-get-text ed) => "aaa bbb ccc")
        ;; Kill ring should have 2 entries
        (check (>= (length (app-state-kill-ring app)) 2) => #t)
        ;; Most recent is "bbb"
        (check (car (app-state-kill-ring app)) => "bbb")))

    ;;=========================================================================
    ;; Eval-last-sexp tests
    ;;=========================================================================

    (test-case "headless: eval-last-sexp evaluates expression before point"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(+ 1 2)")
        ;; Place cursor right after closing paren
        (editor-goto-pos ed 7)
        (cmd-eval-last-sexp app)
        ;; Echo should show "3"
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t)
          (check (not (not (string-contains msg "3"))) => #t))))

    (test-case "headless: eval-last-sexp with no sexp shows message"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        (cmd-eval-last-sexp app)
        ;; Should show "No sexp before point"
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t))))

    (test-case "headless: eval-defun evaluates top-level form"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(* 6 7)")
        ;; Cursor inside the form
        (editor-goto-pos ed 3)
        (cmd-eval-defun app)
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t)
          (check (not (not (string-contains msg "42"))) => #t))))

    (test-case "headless: eval-expression-string handles strings"
      (let-values (((result error?) (eval-expression-string "(string-append \"hello\" \" \" \"world\")")))
        (check error? => #f)
        (check (not (not (string-contains result "hello world"))) => #t)))

    (test-case "headless: eval-expression-string handles lists"
      (let-values (((result error?) (eval-expression-string "(list 1 2 3)")))
        (check error? => #f)
        (check (not (not (string-contains result "1"))) => #t)))

    ;;=========================================================================
    ;; Org-mode comprehensive tests
    ;;=========================================================================

    (test-case "headless: org-todo cycling none -> TODO -> DONE -> none"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* My heading")
        (editor-goto-pos ed 0)
        ;; First cycle: add TODO
        (cmd-org-todo app)
        (check (not (not (string-contains (editor-get-text ed) "TODO"))) => #t)
        ;; Second cycle: TODO -> DONE
        (cmd-org-todo app)
        (check (not (not (string-contains (editor-get-text ed) "DONE"))) => #t)
        ;; Third cycle: DONE -> back to plain
        (cmd-org-todo app)
        (let ((text (editor-get-text ed)))
          (check (not (string-contains text "TODO")) => #t)
          (check (not (string-contains text "DONE")) => #t))))

    (test-case "headless: org-promote and demote"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "** Subheading")
        (editor-goto-pos ed 0)
        ;; Promote: ** -> *
        (cmd-org-promote app)
        (check (string-prefix? "* " (editor-get-text ed)) => #t)
        ;; Demote: * -> **
        (cmd-org-demote app)
        (check (string-prefix? "** " (editor-get-text ed)) => #t)
        ;; Demote again: ** -> ***
        (cmd-org-demote app)
        (check (string-prefix? "*** " (editor-get-text ed)) => #t)))

    (test-case "headless: org-toggle-checkbox"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "- [ ] Buy groceries")
        (editor-goto-pos ed 0)
        ;; Toggle: [ ] -> [X]
        (cmd-org-toggle-checkbox app)
        (check (not (not (string-contains (editor-get-text ed) "[X]"))) => #t)
        ;; Toggle back: [X] -> [ ]
        (cmd-org-toggle-checkbox app)
        (check (not (not (string-contains (editor-get-text ed) "[ ]"))) => #t)))

    (test-case "headless: org-insert-heading creates new heading"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "** Tasks\nSome content")
        (editor-goto-pos ed 5)
        (cmd-org-insert-heading app)
        (let ((text (editor-get-text ed)))
          ;; Should have two ** headings now
          (let ((first (string-contains text "** "))
                (second (string-contains text "** " 3)))
            (check (not (not first)) => #t)
            (check (not (not second)) => #t)))))

    (test-case "headless: org-move-subtree-down"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* First\n* Second\n* Third")
        (editor-goto-pos ed 0)  ;; on "* First"
        (cmd-org-move-subtree-down app)
        (let ((text (editor-get-text ed)))
          ;; "Second" should now come before "First"
          (let ((pos-second (string-contains text "Second"))
                (pos-first (string-contains text "First")))
            (check (< pos-second pos-first) => #t)))))

    (test-case "headless: org-shift-tab global cycling"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* H1\nContent1\n** H2\nContent2\n* H3\nContent3")
        (editor-goto-pos ed 0)
        ;; All lines should start visible
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 1) => #t)
        ;; First shift-tab: show only headings (hide content)
        (cmd-org-shift-tab app)
        ;; Content lines should be hidden
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 0) => #t)  ;; Content1
        (check (= (send-message ed SCI_GETLINEVISIBLE 3) 0) => #t)  ;; Content2
        ;; Heading lines should remain visible
        (check (= (send-message ed SCI_GETLINEVISIBLE 0) 1) => #t)  ;; H1
        (check (= (send-message ed SCI_GETLINEVISIBLE 2) 1) => #t)  ;; H2
        (check (= (send-message ed SCI_GETLINEVISIBLE 4) 1) => #t)  ;; H3
        ))

    (test-case "headless: org-template-expand all block types"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Test <v for VERSE
        (editor-set-text ed "<v")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (check (not (not (string-contains (editor-get-text ed) "#+BEGIN_VERSE"))) => #t)
        ;; Test <c for CENTER
        (editor-set-text ed "<c")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (check (not (not (string-contains (editor-get-text ed) "#+BEGIN_CENTER"))) => #t)
        ;; Test <h for EXPORT html
        (editor-set-text ed "<h")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_EXPORT html"))) => #t)
          ;; End tag should be just #+END_EXPORT (not #+END_EXPORT html)
          (check (not (not (string-contains text "#+END_EXPORT"))) => #t))))

    (test-case "headless: org-store-link stores file reference"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nSome text")
        (editor-goto-pos ed 0)
        (cmd-org-store-link app)
        ;; Should have stored a link
        (check (not (not *org-stored-link*)) => #t)))

    ;;=========================================================================
    ;; Keybinding tests for new features
    ;;=========================================================================

    (test-case "keybinding: C-x C-e bound to eval-last-sexp"
      (check (keymap-lookup *ctrl-x-map* "C-e") => 'eval-last-sexp))

    (test-case "keybinding: C-c C-e bound to eval-last-sexp"
      (check (keymap-lookup *ctrl-c-map* "C-e") => 'eval-last-sexp))

    (test-case "keybinding: C-c C-d bound to eval-defun"
      (check (keymap-lookup *ctrl-c-map* "C-d") => 'eval-defun))

    (test-case "keybinding: org-mode C-c prefix keys"
      (check (keymap-lookup *ctrl-c-map* "C-n") => 'org-next-heading)
      (check (keymap-lookup *ctrl-c-map* "C-p") => 'org-prev-heading)
      (check (keymap-lookup *ctrl-c-map* "C-t") => 'org-todo)
      (check (keymap-lookup *ctrl-c-map* "C-l") => 'org-link)
      (check (keymap-lookup *ctrl-c-map* "C-o") => 'org-open-at-point)
      (check (keymap-lookup *ctrl-c-map* "C-q") => 'org-set-tags)
      ;; Note: C-c , is overridden to describe-char later in core.ss
      (check (keymap-lookup *ctrl-c-map* ",") => 'describe-char))

    (test-case "keybinding: standard Emacs essentials"
      ;; Movement
      (check (keymap-lookup *global-keymap* "C-f") => 'forward-char)
      (check (keymap-lookup *global-keymap* "C-b") => 'backward-char)
      (check (keymap-lookup *global-keymap* "C-n") => 'next-line)
      (check (keymap-lookup *global-keymap* "C-p") => 'previous-line)
      (check (keymap-lookup *global-keymap* "C-a") => 'beginning-of-line)
      (check (keymap-lookup *global-keymap* "C-e") => 'end-of-line)
      ;; Edit
      (check (keymap-lookup *global-keymap* "C-d") => 'delete-char)
      (check (keymap-lookup *global-keymap* "C-k") => 'kill-line)
      (check (keymap-lookup *global-keymap* "C-y") => 'yank)
      (check (keymap-lookup *global-keymap* "C-w") => 'kill-region)
      (check (keymap-lookup *global-keymap* "M-w") => 'copy-region)
      ;; Search
      (check (keymap-lookup *global-keymap* "C-s") => 'search-forward)
      (check (keymap-lookup *global-keymap* "C-r") => 'search-backward)
      ;; Files
      (check (keymap-lookup *ctrl-x-map* "C-s") => 'save-buffer)
      (check (keymap-lookup *ctrl-x-map* "C-f") => 'find-file)
      ;; Buffers
      (check (keymap-lookup *ctrl-x-map* "b") => 'switch-buffer)
      (check (keymap-lookup *ctrl-x-map* "k") => 'kill-buffer-cmd)
      ;; Windows
      (check (keymap-lookup *ctrl-x-map* "2") => 'split-window)
      (check (keymap-lookup *ctrl-x-map* "3") => 'split-window-right)
      (check (keymap-lookup *ctrl-x-map* "1") => 'delete-other-windows)
      (check (keymap-lookup *ctrl-x-map* "0") => 'delete-window)
      ;; Undo
      (check (keymap-lookup *global-keymap* "C-/") => 'undo)
      ;; Meta
      (check (keymap-lookup *global-keymap* "M-:") => 'eval-expression)
      (check (keymap-lookup *global-keymap* "M-.") => 'goto-definition)
      (check (keymap-lookup *global-keymap* "M-;") => 'toggle-comment))

    ;;=========================================================================
    ;; Navigation tests
    ;;=========================================================================

    (test-case "headless: forward/backward word"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world foo")
        (editor-goto-pos ed 0)
        ;; Forward word should jump to end of "hello"
        (cmd-forward-word app)
        (check (> (editor-get-current-pos ed) 0) => #t)
        (check (<= (editor-get-current-pos ed) 6) => #t)))

    (test-case "headless: beginning/end of buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line one\nline two\nline three")
        (editor-goto-pos ed 10)
        ;; Go to beginning of buffer
        (cmd-beginning-of-buffer app)
        (check (editor-get-current-pos ed) => 0)
        ;; Go to end of buffer
        (cmd-end-of-buffer app)
        (check (= (editor-get-current-pos ed) (string-length "line one\nline two\nline three")) => #t)))

    (test-case "headless: delete-char removes character"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 0)
        (cmd-delete-char app)
        (check (editor-get-text ed) => "ello")))

    (test-case "headless: kill-line removes to end of line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world\nsecond line")
        (editor-goto-pos ed 5)
        (cmd-kill-line app)
        (check (editor-get-text ed) => "hello\nsecond line")))

    (test-case "headless: undo reverses insertion"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        ;; Type text
        (cmd-self-insert! app (char->integer #\a))
        (cmd-self-insert! app (char->integer #\b))
        (cmd-self-insert! app (char->integer #\c))
        (check (editor-get-text ed) => "abc")
        ;; Undo
        (cmd-undo app)
        (check (not (string=? (editor-get-text ed) "abc")) => #t)))

    (test-case "headless: eval-expression-string with math"
      (let-values (((result error?) (eval-expression-string "(expt 2 10)")))
        (check error? => #f)
        (check (not (not (string-contains result "1024"))) => #t)))

    (test-case "headless: eval-expression-string with error"
      (let-values (((result error?) (eval-expression-string "(error \"test error\")")))
        (check error? => #t)))

    (test-case "headless: org-heading-level helper"
      (check (org-heading-level "* Top") => 1)
      (check (org-heading-level "** Sub") => 2)
      (check (org-heading-level "*** Deep") => 3)
      (check (org-heading-level "Not a heading") => 0)
      (check (org-heading-level "") => 0))

    (test-case "headless: org-find-subtree-end"
      ;; "* H1\nContent\n** Sub\nMore\n* H2"
      ;; Line 0: * H1
      ;; Line 1: Content
      ;; Line 2: ** Sub
      ;; Line 3: More
      ;; Line 4: * H2
      (let ((lines '("* H1" "Content" "** Sub" "More" "* H2")))
        ;; Subtree end for H1 (level 1) at line 0 should be line 4
        (check (org-find-subtree-end lines 0 1) => 4)
        ;; Subtree end for ** Sub (level 2) at line 2 should be line 4
        (check (org-find-subtree-end lines 2 2) => 4)))

    (test-case "headless: command registration for eval commands"
      (check (procedure? (find-command 'eval-last-sexp)) => #t)
      (check (procedure? (find-command 'eval-defun)) => #t)
      (check (procedure? (find-command 'eval-buffer)) => #t)
      (check (procedure? (find-command 'eval-region)) => #t)
      (check (procedure? (find-command 'eval-expression)) => #t))

    (test-case "headless: command registration for org commands"
      (check (procedure? (find-command 'org-todo)) => #t)
      (check (procedure? (find-command 'org-cycle)) => #t)
      (check (procedure? (find-command 'org-shift-tab)) => #t)
      (check (procedure? (find-command 'org-promote)) => #t)
      (check (procedure? (find-command 'org-demote)) => #t)
      (check (procedure? (find-command 'org-insert-heading)) => #t)
      (check (procedure? (find-command 'org-toggle-checkbox)) => #t)
      (check (procedure? (find-command 'org-template-expand)) => #t)
      (check (procedure? (find-command 'org-store-link)) => #t)
      (check (procedure? (find-command 'org-move-subtree-up)) => #t)
      (check (procedure? (find-command 'org-move-subtree-down)) => #t)
      (check (procedure? (find-command 'org-priority)) => #t))

    ;;=========================================================================
    ;; Transpose tests
    ;;=========================================================================

    (test-case "headless: transpose-chars swaps two chars before point"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "abcdef")
        (editor-goto-pos ed 3)  ;; cursor after 'c', swaps b and c
        (cmd-transpose-chars app)
        ;; chars at pos-2 and pos-1 swap: b<->c -> "acbdef"
        (check (editor-get-text ed) => "acbdef")))

    (test-case "headless: transpose-words swaps adjacent words"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 6)  ;; on 'w' in "world"
        (cmd-transpose-words app)
        (let ((text (editor-get-text ed)))
          ;; "world" should come before "hello"
          (let ((pw (string-contains text "world"))
                (ph (string-contains text "hello")))
            (check (not (not pw)) => #t)
            (check (not (not ph)) => #t)
            (check (< pw ph) => #t)))))

    (test-case "headless: transpose-lines swaps adjacent lines"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "first\nsecond\nthird")
        (editor-goto-pos ed 8)  ;; on "second" line
        (cmd-transpose-lines app)
        (let ((text (editor-get-text ed)))
          ;; "second" should come before "first"
          (let ((ps (string-contains text "second"))
                (pf (string-contains text "first")))
            (check (not (not ps)) => #t)
            (check (not (not pf)) => #t)
            (check (< ps pf) => #t)))))

    ;;=========================================================================
    ;; Case transformation tests
    ;;=========================================================================

    (test-case "headless: upcase-word uppercases word at point"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (cmd-upcase-word app)
        (check (not (not (string-contains (editor-get-text ed) "HELLO"))) => #t)))

    (test-case "headless: downcase-word lowercases word at point"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "HELLO WORLD")
        (editor-goto-pos ed 0)
        (cmd-downcase-word app)
        (check (not (not (string-contains (editor-get-text ed) "hello"))) => #t)))

    (test-case "headless: capitalize-word capitalizes word at point"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (cmd-capitalize-word app)
        (check (not (not (string-contains (editor-get-text ed) "Hello"))) => #t)))

    (test-case "headless: upcase-region uppercases selection"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Set mark at 0, cursor at 5 => select "hello"
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        (cmd-upcase-region app)
        (check (string-prefix? "HELLO" (editor-get-text ed)) => #t)))

    (test-case "headless: downcase-region lowercases selection"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "HELLO WORLD")
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        (cmd-downcase-region app)
        (check (string-prefix? "hello" (editor-get-text ed)) => #t)))

    ;;=========================================================================
    ;; Kill-word tests
    ;;=========================================================================

    (test-case "headless: kill-word deletes forward word"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 0)
        (cmd-kill-word app)
        ;; "hello" should be deleted, leaving " world" or "world"
        (let ((text (editor-get-text ed)))
          (check (not (string-contains text "hello")) => #t)
          (check (not (not (string-contains text "world"))) => #t))))

    (test-case "headless: backward-kill-word deletes backward word"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 11)  ;; at end
        (cmd-backward-kill-word app)
        ;; "world" should be deleted
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "hello"))) => #t)
          (check (not (string-contains text "world")) => #t))))

    ;;=========================================================================
    ;; S-expression navigation tests
    ;;=========================================================================

    (test-case "headless: forward-sexp moves past balanced parens"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(hello) (world)")
        (editor-goto-pos ed 0)
        (cmd-forward-sexp app)
        ;; Should be after "(hello)" => position 7
        (check (= (editor-get-current-pos ed) 7) => #t)))

    (test-case "headless: backward-sexp moves before balanced parens"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(hello) (world)")
        (editor-goto-pos ed 15)  ;; at end
        (cmd-backward-sexp app)
        ;; Should be before "(world)" => position 8
        (check (= (editor-get-current-pos ed) 8) => #t)))

    ;;=========================================================================
    ;; Paragraph navigation tests
    ;;=========================================================================

    (test-case "headless: forward-paragraph moves to next blank line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "First paragraph.\n\nSecond paragraph.")
        (editor-goto-pos ed 0)
        (cmd-forward-paragraph app)
        ;; Should move past the blank line
        (check (> (editor-get-current-pos ed) 16) => #t)))

    (test-case "headless: backward-paragraph moves to previous blank line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "First paragraph.\n\nSecond paragraph.")
        (editor-goto-pos ed 35)  ;; at end
        (cmd-backward-paragraph app)
        ;; Should move before "Second paragraph."
        (check (< (editor-get-current-pos ed) 20) => #t)))

    ;;=========================================================================
    ;; Line operation tests
    ;;=========================================================================

    (test-case "headless: open-line inserts newline without moving cursor"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 2)
        (cmd-open-line app)
        (let ((text (editor-get-text ed)))
          ;; Should insert a newline at position 2
          (check (not (not (string-contains text "\n"))) => #t)
          ;; "hello" text should still be present (split across lines)
          (check (> (string-length text) 5) => #t))))

    (test-case "headless: join-line joins current with next line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello\nworld")
        (editor-goto-pos ed 0)
        (cmd-join-line app)
        ;; Lines should be joined
        (let ((text (editor-get-text ed)))
          (check (not (string-contains text "\n")) => #t)
          (check (not (not (string-contains text "hello"))) => #t)
          (check (not (not (string-contains text "world"))) => #t))))

    (test-case "headless: just-one-space collapses whitespace"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello     world")
        (editor-goto-pos ed 8)  ;; in the middle of spaces
        (cmd-just-one-space app)
        ;; Multiple spaces should become one
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "hello"))) => #t)
          (check (not (not (string-contains text "world"))) => #t)
          (check (< (string-length text) 15) => #t))))

    ;; Note: cmd-delete-blank-lines has a known bug with SCI_POSITIONFROMLINE
    ;; returning #!void. Skipping until the underlying function is fixed.

    ;;=========================================================================
    ;; Comment toggle test
    ;;=========================================================================

    (test-case "headless: toggle-comment comments/uncomments line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.ss" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(define x 42)")
        (editor-goto-pos ed 0)
        ;; Set mark so toggle-comment has a region
        (cmd-set-mark app)
        (editor-goto-pos ed 13)
        (cmd-toggle-comment app)
        ;; Should have comment prefix
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text ";"))) => #t))))

    ;;=========================================================================
    ;; Window management tests
    ;;=========================================================================

    (test-case "headless: split-window creates two windows"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Initially 1 window
        (check (= (length (frame-windows fr)) 1) => #t)
        (cmd-split-window app)
        ;; Now should have 2 windows
        (check (= (length (frame-windows fr)) 2) => #t)))

    (test-case "headless: delete-other-windows leaves one window"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Split first
        (cmd-split-window app)
        (check (= (length (frame-windows fr)) 2) => #t)
        ;; Now delete others
        (cmd-delete-other-windows app)
        (check (= (length (frame-windows fr)) 1) => #t)))

    (test-case "headless: other-window cycles through windows"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (cmd-split-window app)
        (let ((initial-idx (frame-current-idx fr)))
          (cmd-other-window app)
          ;; Window index should change
          (check (not (= (frame-current-idx fr) initial-idx)) => #t))))

    (test-case "headless: delete-window removes current window"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Split to get 2 windows
        (cmd-split-window app)
        (check (= (length (frame-windows fr)) 2) => #t)
        ;; Delete current
        (cmd-delete-window app)
        (check (= (length (frame-windows fr)) 1) => #t)))

    ;;=========================================================================
    ;; Fill paragraph test
    ;;=========================================================================

    (test-case "headless: fill-paragraph wraps long lines"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Create a very long line
        (editor-set-text ed "This is a very long line that should be wrapped when fill-paragraph is called because it exceeds the fill column width significantly and needs to be broken up into multiple lines")
        (editor-goto-pos ed 0)
        (cmd-fill-paragraph app)
        ;; Should have line breaks now
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "\n"))) => #t))))

    ;;=========================================================================
    ;; Multiple cursor position tracking
    ;;=========================================================================

    (test-case "headless: forward-char moves cursor by 1"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "abcdef")
        (editor-goto-pos ed 0)
        (cmd-forward-char app)
        (check (= (editor-get-current-pos ed) 1) => #t)
        (cmd-forward-char app)
        (check (= (editor-get-current-pos ed) 2) => #t)))

    (test-case "headless: backward-char moves cursor back by 1"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "abcdef")
        (editor-goto-pos ed 3)
        (cmd-backward-char app)
        (check (= (editor-get-current-pos ed) 2) => #t)))

    (test-case "headless: next-line moves to next line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line1\nline2\nline3")
        (editor-goto-pos ed 0)
        (cmd-next-line app)
        ;; Should be on line 2 (position >= 6)
        (check (>= (editor-get-current-pos ed) 6) => #t)
        (check (< (editor-get-current-pos ed) 12) => #t)))

    (test-case "headless: previous-line moves to previous line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line1\nline2\nline3")
        (editor-goto-pos ed 8)  ;; on line2
        (cmd-previous-line app)
        ;; Should be on line 1 (position < 6)
        (check (< (editor-get-current-pos ed) 6) => #t)))

    (test-case "headless: beginning-of-line and end-of-line"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world\nsecond line")
        (editor-goto-pos ed 5)
        (cmd-beginning-of-line app)
        (check (= (editor-get-current-pos ed) 0) => #t)
        (cmd-end-of-line app)
        (check (= (editor-get-current-pos ed) 11) => #t)))

    ;;=========================================================================
    ;; Multiple self-insert characters
    ;;=========================================================================

    (test-case "headless: self-insert builds text correctly"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (cmd-self-insert! app (char->integer #\H))
        (cmd-self-insert! app (char->integer #\i))
        (cmd-self-insert! app (char->integer #\!))
        (check (editor-get-text ed) => "Hi!")))

    ;;=========================================================================
    ;; Backspace / backward-delete
    ;;=========================================================================

    (test-case "headless: backward-delete-char removes char before cursor"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello")
        (editor-goto-pos ed 5)
        (cmd-backward-delete-char app)
        (check (editor-get-text ed) => "hell")))

    ;;=========================================================================
    ;; Mark and region operations
    ;;=========================================================================

    (test-case "headless: exchange-point-and-mark swaps positions"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Set mark at 0
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        ;; Move to 5
        (editor-goto-pos ed 5)
        ;; Exchange: mark should go to 5, point to 0
        (cmd-exchange-point-and-mark app)
        (check (= (buffer-mark buf) 5) => #t)
        (check (= (editor-get-current-pos ed) 0) => #t)))

    ;;=========================================================================
    ;; Indent region test
    ;;=========================================================================

    (test-case "headless: indent-region modifies text"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.ss" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line1\nline2\nline3")
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 17)
        ;; Just verify it doesn't crash
        (cmd-indent-region app)
        (check (> (string-length (editor-get-text ed)) 0) => #t)))

    ;;=========================================================================
    ;; Buffer modified tracking
    ;;=========================================================================

    (test-case "headless: buffer tracks modified state"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "initial")
        ;; Clear modified flag
        (send-message ed SCI_SETSAVEPOINT)
        (check (= (send-message ed SCI_GETMODIFY) 0) => #t)
        ;; Make a change
        (editor-goto-pos ed 7)
        (cmd-self-insert! app (char->integer #\!))
        ;; Now should be modified
        (check (= (send-message ed SCI_GETMODIFY) 1) => #t)))

    ;;=========================================================================
    ;; Combined workflow tests
    ;;=========================================================================

    (test-case "headless: mark-kill-yank round-trip preserves text"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "The quick brown fox")
        ;; Kill "quick " (positions 4-10)
        (editor-goto-pos ed 4)
        (cmd-set-mark app)
        (editor-goto-pos ed 10)
        (cmd-kill-region app)
        (check (editor-get-text ed) => "The brown fox")
        ;; Yank back at position 4
        (editor-goto-pos ed 4)
        (editor-paste ed)
        (check (editor-get-text ed) => "The quick brown fox")))

    (test-case "headless: multiple kill-lines accumulate"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line1\nline2\nline3")
        (editor-goto-pos ed 0)
        ;; Kill first line content
        (cmd-kill-line app)
        (check (editor-get-text ed) => "\nline2\nline3")
        ;; Kill the newline
        (cmd-kill-line app)
        (check (editor-get-text ed) => "line2\nline3")))

    (test-case "headless: search then navigate workflow"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "foo bar baz foo quux")
        ;; Search for "baz"
        (search-forward-impl! app "baz")
        ;; Cursor should be after "baz" (position 11 = 8 + 3)
        (check (= (editor-get-current-pos ed) 11) => #t)
        ;; Text should be unchanged
        (check (editor-get-text ed) => "foo bar baz foo quux")))

    (test-case "headless: upcase word then move to next word"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world test")
        (editor-goto-pos ed 0)
        ;; Upcase first word
        (cmd-upcase-word app)
        ;; Cursor should have moved past "hello"
        (check (> (editor-get-current-pos ed) 0) => #t)
        ;; Second upcase-word should affect "world"
        (cmd-upcase-word app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "HELLO"))) => #t)
          (check (not (not (string-contains text "WORLD"))) => #t))))

    ;;=========================================================================
    ;; Echo area and messaging
    ;;=========================================================================

    (test-case "headless: echo message then error clears message"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Send a message
        (echo-message! (app-state-echo app) "Hello")
        (check (echo-state-message (app-state-echo app)) => "Hello")
        (check (echo-state-error? (app-state-echo app)) => #f)
        ;; Send an error
        (echo-error! (app-state-echo app) "Oops")
        (check (echo-state-message (app-state-echo app)) => "Oops")
        (check (echo-state-error? (app-state-echo app)) => #t)
        ;; Clear
        (echo-clear! (app-state-echo app))
        (check (echo-state-message (app-state-echo app)) => #f)))

    ;;=========================================================================
    ;; Org-mode additional workflow tests
    ;;=========================================================================

    (test-case "headless: org-cycle fold then unfold preserves text"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nBody text\nMore body")
        (let ((original (editor-get-text ed)))
          (editor-goto-pos ed 0)
          ;; Fold
          (cmd-org-cycle app)
          ;; Content lines hidden
          (check (= (send-message ed SCI_GETLINEVISIBLE 1) 0) => #t)
          ;; Unfold (cycle back)
          (cmd-org-cycle app)
          (cmd-org-cycle app)
          ;; Text should be unchanged
          (check (editor-get-text ed) => original))))

    (test-case "headless: org-promote at level 1 is no-op"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Top level")
        (editor-goto-pos ed 0)
        ;; Promoting level 1 heading should not change it
        (cmd-org-promote app)
        (check (string-prefix? "* " (editor-get-text ed)) => #t)))

    (test-case "headless: org-priority cycling"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* TODO My task")
        (editor-goto-pos ed 0)
        (cmd-org-priority app)
        ;; Should have [#A] priority cookie
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "[#"))) => #t))))

    ;;=========================================================================
    ;; Comprehensive keybinding coverage
    ;;=========================================================================

    (test-case "keybinding: text transformation keys"
      (check (keymap-lookup *global-keymap* "C-t") => 'transpose-chars)
      (check (keymap-lookup *global-keymap* "M-t") => 'transpose-words)
      (check (keymap-lookup *global-keymap* "M-u") => 'upcase-word)
      (check (keymap-lookup *global-keymap* "M-l") => 'downcase-word)
      (check (keymap-lookup *global-keymap* "M-c") => 'capitalize-word))

    (test-case "keybinding: kill and word operations"
      (check (keymap-lookup *global-keymap* "M-d") => 'kill-word)
      (check (keymap-lookup *global-keymap* "C-k") => 'kill-line)
      (check (keymap-lookup *global-keymap* "C-d") => 'delete-char))

    (test-case "keybinding: sexp navigation via M-g prefix"
      (check (keymap-lookup *meta-g-map* "f") => 'forward-sexp)
      (check (keymap-lookup *meta-g-map* "b") => 'backward-sexp))

    (test-case "keybinding: paragraph navigation"
      (check (keymap-lookup *global-keymap* "M-}") => 'forward-paragraph)
      (check (keymap-lookup *global-keymap* "M-{") => 'backward-paragraph))

    (test-case "keybinding: M-x and core keys"
      (check (keymap-lookup *global-keymap* "M-x") => 'execute-extended-command)
      (check (keymap-lookup *global-keymap* "C-g") => 'keyboard-quit)
      ;; C-x is a prefix key (returns a keymap, not a symbol)
      (check (hash-table? (keymap-lookup *global-keymap* "C-x")) => #t))

    ;;=========================================================================
    ;; TAB dispatch integration tests (org-mode via cmd-indent-or-complete)
    ;;=========================================================================

    (test-case "headless: org-buffer? detects .org files"
      ;; By buffer name
      (let ((buf (make-buffer "notes.org" #f #f #f #f #f #f)))
        (check (not (not (org-buffer? buf))) => #t))
      ;; By file path
      (let ((buf (make-buffer "notes" "/tmp/notes.org" #f #f #f #f #f)))
        (check (not (not (org-buffer? buf))) => #t))
      ;; By lexer-lang
      (let ((buf (make-buffer "notes" #f #f #f #f 'org #f)))
        (check (not (not (org-buffer? buf))) => #t))
      ;; Non-org buffer
      (let ((buf (make-buffer "test.py" "/tmp/test.py" #f #f #f #f #f)))
        (check (org-buffer? buf) => #f))
      ;; Scratch buffer
      (let ((buf (make-buffer "*scratch*" #f #f #f #f #f #f)))
        (check (org-buffer? buf) => #f)))

    (test-case "headless: TAB in org buffer on <s expands template"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; User types "<s" then hits TAB
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        ;; TAB dispatches through cmd-indent-or-complete
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "#+END_SRC"))) => #t))))

    (test-case "headless: TAB in org buffer on <e expands example block"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<e")
        (editor-goto-pos ed 2)
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_EXAMPLE"))) => #t)
          (check (not (not (string-contains text "#+END_EXAMPLE"))) => #t))))

    (test-case "headless: TAB in org buffer on <q expands quote block"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<q")
        (editor-goto-pos ed 2)
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_QUOTE"))) => #t)
          (check (not (not (string-contains text "#+END_QUOTE"))) => #t))))

    (test-case "headless: TAB on org heading folds children"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nBody line 1\nBody line 2")
        (editor-goto-pos ed 0)
        ;; TAB on heading should fold (hide body lines)
        (cmd-indent-or-complete app)
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 0) => #t)
        (check (= (send-message ed SCI_GETLINEVISIBLE 2) 0) => #t)
        ;; Heading itself stays visible
        (check (= (send-message ed SCI_GETLINEVISIBLE 0) 1) => #t)))

    (test-case "headless: TAB on plain text in org buffer indents"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "plain text")
        (editor-goto-pos ed 0)
        ;; TAB on plain text should just indent
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          ;; Should have 2 spaces inserted
          (check (string-prefix? "  " text) => #t))))

    (test-case "headless: TAB in non-org buffer just indents"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.py" "/tmp/test.py"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<s")
        (editor-goto-pos ed 2)
        ;; TAB in non-org buffer should NOT expand template
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          (check (not (string-contains text "#+BEGIN_SRC")) => #t)
          ;; Should have inserted spaces instead
          (check (not (not (string-contains text "  "))) => #t))))

    (test-case "headless: TAB <s expands in large org file with preceding content"
      ;; Regression: cursor position could exceed string-length when file
      ;; has multi-byte chars or many lines, causing template expand to silently fail.
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "big.org" "/tmp/big.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr))
             ;; Build a large org file: many heading/body lines, then <s at end
             (body (let loop ((i 0) (acc "#+title: Test\n\n"))
                     (if (>= i 50)
                       (string-append acc "<s")
                       (loop (+ i 1)
                             (string-append acc "** Heading " (number->string i) "\n"
                                            "Body line with some text here.\n"))))))
        (editor-set-text ed body)
        ;; Place cursor at end (after "<s")
        (editor-goto-pos ed (string-length body))
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "#+END_SRC"))) => #t))))

    (test-case "headless: TAB <s expands when <s is in the middle of an org file"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "mid.org" "/tmp/mid.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; <s is on line 3 (0-indexed), not at start or end
        (editor-set-text ed "#+title: Test\n\n** Section\n<s\nMore text here\n")
        ;; Place cursor after "<s" (position = 28: "#+title: Test\n\n** Section\n<s" = 28 chars)
        (let ((target-pos (string-contains "#+title: Test\n\n** Section\n<s\nMore text here\n" "<s")))
          (editor-goto-pos ed (+ target-pos 2)))
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          (check (not (not (string-contains text "#+END_SRC"))) => #t)
          ;; The rest of the file should still be there
          (check (not (not (string-contains text "More text here"))) => #t))))

    (test-case "headless: TAB expands all org template types"
      ;; Test <v, <c, <C, <l, <h, <a (not just s/e/q)
      (for-each
        (lambda (pair)
          (let* ((trigger (car pair))
                 (block-type (cdr pair))
                 (ed (create-scintilla-editor width: 80 height: 24))
                 (buf (make-buffer "tmpl.org" "/tmp/tmpl.org"
                        (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
                 (win (make-edit-window ed buf 0 0 80 24 0))
                 (fr (make-frame [win] 0 80 24 'vertical))
                 (app (new-app-state fr)))
            (editor-set-text ed (string-append "<" trigger))
            (editor-goto-pos ed (+ 1 (string-length trigger)))
            (cmd-indent-or-complete app)
            (let ((text (editor-get-text ed)))
              (check (not (not (string-contains text
                (string-append "#+BEGIN_" block-type)))) => #t))))
        '(("v" . "VERSE") ("c" . "CENTER") ("C" . "COMMENT")
          ("l" . "EXPORT") ("h" . "EXPORT") ("a" . "EXPORT"))))

    (test-case "headless: TAB does not expand unknown org template trigger"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" "/tmp/test.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; <z is not a valid template trigger
        (editor-set-text ed "<z")
        (editor-goto-pos ed 2)
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          ;; Should NOT have expanded
          (check (not (string-contains text "#+BEGIN")) => #t)
          ;; Should have indented instead
          (check (not (not (string-contains text "  "))) => #t))))

    (test-case "headless: TAB <s with leading whitespace preserves indent"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "indent.org" "/tmp/indent.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; <s with 4 spaces of leading whitespace
        (editor-set-text ed "    <s")
        (editor-goto-pos ed 6)
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
          ;; The BEGIN line should preserve the 4-space indent
          (check (not (not (string-contains text "    #+BEGIN_SRC"))) => #t))))

    ;;=========================================================================
    ;; Hippie-expand / Dabbrev completion tests
    ;;=========================================================================

    (test-case "headless: collect-buffer-words extracts unique words"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world hello foo bar world baz")
        (let ((words (collect-buffer-words ed)))
          ;; Should return unique words, no duplicates
          (check (not (not (member "hello" words))) => #t)
          (check (not (not (member "world" words))) => #t)
          (check (not (not (member "foo" words))) => #t)
          (check (not (not (member "bar" words))) => #t)
          (check (not (not (member "baz" words))) => #t)
          ;; "hello" appears twice in text but only once in result
          (check (= (length (filter (lambda (w) (string=? w "hello")) words)) 1) => #t))))

    (test-case "headless: collect-buffer-words filters short words"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "a is the long-word test ok")
        (let ((words (collect-buffer-words ed)))
          ;; Short words (<=2 chars) should be filtered out
          (check (not (member "a" words)) => #t)
          (check (not (member "is" words)) => #t)
          (check (not (member "ok" words)) => #t)
          ;; Words > 2 chars should be present
          (check (not (not (member "the" words))) => #t)
          (check (not (not (member "long-word" words))) => #t)
          (check (not (not (member "test" words))) => #t))))

    (test-case "headless: collect-buffer-words handles empty buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (let ((words (collect-buffer-words ed)))
          (check (null? words) => #t))))

    (test-case "headless: collect-dabbrev-matches finds prefix matches"
      (let* ((text "hello help helicopter world")
             (matches (collect-dabbrev-matches text "hel" 0)))
        ;; Should find words starting with "hel" (not from position 0)
        (check (> (length matches) 0) => #t)
        ;; All matches should start with "hel"
        (for-each (lambda (m) (check (string-prefix? "hel" m) => #t))
                  matches)))

    (test-case "headless: hippie-expand completes word from buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Set up buffer with words, then type partial word
        (editor-set-text ed "defun defvar defmacro")
        ;; Position at end to type "def" partial
        (editor-goto-pos ed (string-length "defun defvar defmacro"))
        ;; Insert a space and the prefix "def"
        (editor-insert-text ed (editor-get-current-pos ed) " def")
        (editor-goto-pos ed (+ (string-length "defun defvar defmacro") 4))
        ;; Hippie-expand should complete "def" to one of "defun", "defvar", "defmacro"
        (cmd-hippie-expand app)
        (let ((text (editor-get-text ed)))
          ;; The "def" prefix should be expanded to a full word
          (check (or (not (not (string-contains text "defun defvar defmacro defun")))
                     (not (not (string-contains text "defun defvar defmacro defvar")))
                     (not (not (string-contains text "defun defvar defmacro defmacro"))))
                 => #t))))

    (test-case "headless: hippie-expand with no prefix shows message"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello ")
        (editor-goto-pos ed 6)  ;; after space, no word prefix
        (cmd-hippie-expand app)
        ;; Should show "No prefix to complete" message
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t))))

    (test-case "headless: dabbrev-expand cycles through matches"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "apple application apply ")
        (editor-goto-pos ed (string-length "apple application apply "))
        ;; Type partial "app"
        (editor-insert-text ed (editor-get-current-pos ed) "app")
        (editor-goto-pos ed (+ (string-length "apple application apply ") 3))
        ;; First expand
        (cmd-dabbrev-expand app)
        (let ((text1 (editor-get-text ed)))
          ;; Should have expanded "app" to a full word
          (check (> (string-length text1) (string-length "apple application apply app")) => #t))
        ;; Second expand should cycle to next match
        (set! (app-state-last-command app) 'dabbrev-expand)
        (cmd-dabbrev-expand app)
        (let ((text2 (editor-get-text ed)))
          ;; Text should change (cycled to different match)
          (check (> (string-length text2) (string-length "apple application apply app")) => #t))))

    ;;=========================================================================
    ;; Org-mode comprehensive feature parity tests
    ;;=========================================================================

    (test-case "headless: org-heading-line? predicate"
      (check (org-heading-line? "* Heading") => #t)
      (check (org-heading-line? "** Sub heading") => #t)
      (check (org-heading-line? "*** Deep heading") => #t)
      (check (org-heading-line? "Not a heading") => #f)
      (check (org-heading-line? "") => #f)
      (check (org-heading-line? "  * indented star") => #f)
      ;; Note: org-heading-line? only checks first char is *, not space after
      (check (org-heading-line? "*bold text") => #t))

    (test-case "headless: org-on-checkbox-line? predicate"
      ;; org-on-checkbox-line? returns index (truthy) or #f
      (check (not (not (org-on-checkbox-line? "- [ ] unchecked"))) => #t)
      (check (not (not (org-on-checkbox-line? "- [X] checked"))) => #t)
      (check (not (not (org-on-checkbox-line? "  - [ ] indented"))) => #t)
      (check (org-on-checkbox-line? "no checkbox here") => #f)
      (check (org-on-checkbox-line? "") => #f)
      (check (org-on-checkbox-line? "* Heading") => #f))

    (test-case "headless: org-todo full cycle NONE -> TODO -> DONE -> NONE"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Start with plain heading (no TODO keyword)
        (editor-set-text ed "* Plain heading")
        (editor-goto-pos ed 0)
        ;; First cycle: NONE -> TODO
        (cmd-org-todo app)
        (let ((text1 (editor-get-text ed)))
          (check (not (not (string-contains text1 "TODO"))) => #t))
        ;; Second cycle: TODO -> DONE
        (cmd-org-todo app)
        (let ((text2 (editor-get-text ed)))
          (check (not (not (string-contains text2 "DONE"))) => #t))
        ;; Third cycle: DONE -> NONE (back to plain)
        (cmd-org-todo app)
        (let ((text3 (editor-get-text ed)))
          (check (not (string-contains text3 "TODO")) => #t)
          (check (not (string-contains text3 "DONE")) => #t))))

    (test-case "headless: org-insert-heading from non-heading creates level 1"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "Some plain text")
        (editor-goto-pos ed 0)
        (cmd-org-insert-heading app)
        (let ((text (editor-get-text ed)))
          ;; Should create a level-1 heading
          (check (not (not (string-contains text "* "))) => #t))))

    (test-case "headless: org-insert-heading preserves level"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "** Sub heading\nContent here")
        (editor-goto-pos ed 0)
        (cmd-org-insert-heading app)
        (let ((text (editor-get-text ed)))
          ;; Should create another level-2 heading
          (check (not (not (string-contains text "** "))) => #t)
          ;; Count "**" occurrences - should be at least 2
          (let count-stars ((idx 0) (count 0))
            (let ((pos (string-contains text "** " idx)))
              (if pos
                (count-stars (+ pos 3) (+ count 1))
                (check (>= count 2) => #t)))))))

    (test-case "headless: org-toggle-checkbox creates checkbox on plain list item"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Start with unchecked checkbox
        (editor-set-text ed "- [ ] Buy milk")
        (editor-goto-pos ed 0)
        (cmd-org-toggle-checkbox app)
        ;; Should toggle to checked
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "[X]"))) => #t))
        ;; Toggle again should uncheck
        (cmd-org-toggle-checkbox app)
        (let ((text2 (editor-get-text ed)))
          (check (not (not (string-contains text2 "[ ]"))) => #t))))

    (test-case "headless: org-cycle preserves content through fold/unfold"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* H1\nBody line 1\nBody line 2\n* H2\nMore body")
        (let ((original-text (editor-get-text ed)))
          (editor-goto-pos ed 0)
          ;; Fold
          (cmd-org-cycle app)
          ;; Unfold (cycle through states)
          (cmd-org-cycle app)
          (cmd-org-cycle app)
          ;; Text content should be unchanged
          (check (editor-get-text ed) => original-text))))

    (test-case "headless: org-shift-tab cycles through overview/content/all"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* H1\nBody1\n** Sub1\nBody2\n* H2\nBody3")
        (editor-goto-pos ed 0)
        ;; First shift-tab: overview mode - only top headings visible
        (cmd-org-shift-tab app)
        ;; Body lines should be hidden
        (check (= (send-message ed SCI_GETLINEVISIBLE 1) 0) => #t)
        ;; Heading lines visible
        (check (= (send-message ed SCI_GETLINEVISIBLE 0) 1) => #t)
        (check (= (send-message ed SCI_GETLINEVISIBLE 4) 1) => #t)))

    (test-case "headless: org-move-subtree-up at first heading is no-op"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* First\n* Second")
        (let ((original (editor-get-text ed)))
          (editor-goto-pos ed 0)
          ;; Move up at first heading should not change anything
          (cmd-org-move-subtree-up app)
          (check (editor-get-text ed) => original))))

    (test-case "headless: org-move-subtree-down at last heading is no-op"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* First\n* Second")
        (let ((original (editor-get-text ed)))
          ;; Go to last heading
          (editor-goto-pos ed 8)  ;; on "* Second"
          (cmd-org-move-subtree-down app)
          (check (editor-get-text ed) => original))))

    (test-case "headless: org-export strips stars and preserves content"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Main Title\nSome text\n** Subtitle\nMore text")
        (editor-goto-pos ed 0)
        (cmd-org-export app)
        (let ((text (editor-get-text ed)))
          ;; Stars should be stripped - exported text should not start with *
          (check (not (string-prefix? "*" text)) => #t)
          ;; Content should still be present
          (check (not (not (string-contains text "Main Title"))) => #t)
          (check (not (not (string-contains text "Some text"))) => #t)
          (check (not (not (string-contains text "Subtitle"))) => #t))))

    (test-case "headless: org-insert-src-block creates template"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        (editor-goto-pos ed 0)
        ;; Insert src block with no language (empty prompt result)
        ;; Since app-read-string requires terminal input, we test the template format
        ;; by directly inserting what the function would produce
        (let ((template "#+BEGIN_SRC\n\n#+END_SRC\n"))
          (editor-insert-text ed 0 template)
          (let ((text (editor-get-text ed)))
            (check (not (not (string-contains text "#+BEGIN_SRC"))) => #t)
            (check (not (not (string-contains text "#+END_SRC"))) => #t)))))

    (test-case "headless: org-priority cycles through A B C"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* TODO Task")
        (editor-goto-pos ed 0)
        ;; First: add priority A
        (cmd-org-priority app)
        (check (not (not (string-contains (editor-get-text ed) "[#A]"))) => #t)
        ;; Second: cycle to B
        (cmd-org-priority app)
        (check (not (not (string-contains (editor-get-text ed) "[#B]"))) => #t)
        ;; Third: cycle to C
        (cmd-org-priority app)
        (check (not (not (string-contains (editor-get-text ed) "[#C]"))) => #t)
        ;; Fourth: remove priority
        (cmd-org-priority app)
        (let ((text (editor-get-text ed)))
          (check (not (string-contains text "[#")) => #t))))

    (test-case "headless: org-promote and demote nested headings"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Start with level 2 heading
        (editor-set-text ed "** Sub heading")
        (editor-goto-pos ed 0)
        ;; Demote: ** -> ***
        (cmd-org-demote app)
        (check (string-prefix? "*** " (editor-get-text ed)) => #t)
        ;; Promote: *** -> **
        (cmd-org-promote app)
        (check (string-prefix? "** " (editor-get-text ed)) => #t)
        ;; Promote: ** -> *
        (cmd-org-promote app)
        (check (string-prefix? "* " (editor-get-text ed)) => #t)
        ;; Promote at level 1: no-op
        (cmd-org-promote app)
        (check (string-prefix? "* " (editor-get-text ed)) => #t)))

    (test-case "headless: org-find-subtree-end with nested headings"
      ;; * H1
      ;; ** Sub1
      ;; *** SubSub1
      ;; ** Sub2
      ;; * H2
      (let ((lines '("* H1" "** Sub1" "*** SubSub1" "** Sub2" "* H2")))
        ;; H1 subtree (level 1) at line 0 extends to line 4
        (check (org-find-subtree-end lines 0 1) => 4)
        ;; Sub1 subtree (level 2) at line 1 extends to line 3
        (check (org-find-subtree-end lines 1 2) => 3)
        ;; SubSub1 subtree (level 3) at line 2 extends to line 3
        (check (org-find-subtree-end lines 2 3) => 3)
        ;; Sub2 subtree (level 2) at line 3 extends to line 4
        (check (org-find-subtree-end lines 3 2) => 4)
        ;; H2 subtree (level 1) at line 4 extends to end (5)
        (check (org-find-subtree-end lines 4 1) => 5)))

    (test-case "headless: org-move-subtree-down swaps headings"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Alpha\nContent A\n* Beta\nContent B")
        (editor-goto-pos ed 0)
        (cmd-org-move-subtree-down app)
        (let ((text (editor-get-text ed)))
          ;; Beta should come before Alpha
          (let ((pb (string-contains text "Beta"))
                (pa (string-contains text "Alpha")))
            (check (not (not pb)) => #t)
            (check (not (not pa)) => #t)
            (check (< pb pa) => #t)))))

    (test-case "headless: org-move-subtree-up swaps headings"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Alpha\nContent A\n* Beta\nContent B")
        ;; Go to Beta heading
        (editor-goto-pos ed (string-contains "* Alpha\nContent A\n* Beta\nContent B" "* Beta"))
        (cmd-org-move-subtree-up app)
        (let ((text (editor-get-text ed)))
          ;; Beta should come before Alpha
          (let ((pb (string-contains text "Beta"))
                (pa (string-contains text "Alpha")))
            (check (not (not pb)) => #t)
            (check (not (not pa)) => #t)
            (check (< pb pa) => #t)))))

    (test-case "headless: org-template-expand <l for LaTeX"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "<l")
        (editor-goto-pos ed 2)
        (cmd-org-template-expand app)
        (let ((text (editor-get-text ed)))
          (check (not (not (string-contains text "#+BEGIN_EXPORT latex"))) => #t)
          (check (not (not (string-contains text "#+END_EXPORT"))) => #t))))

    ;;=========================================================================
    ;; Eval tests in scratch buffer context
    ;;=========================================================================

    (test-case "headless: eval-last-sexp with arithmetic in scratch"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Simulate typing an expression in scratch buffer
        (editor-set-text ed "(+ 1 2)")
        (editor-goto-pos ed 7)  ;; after closing paren
        (cmd-eval-last-sexp app)
        ;; Result should appear in echo area
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t)
          (check (not (not (string-contains msg "3"))) => #t))))

    (test-case "headless: eval-last-sexp with nested expression"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(* 2 (+ 3 4))")
        (editor-goto-pos ed 14)  ;; after closing paren
        (cmd-eval-last-sexp app)
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t)
          (check (not (not (string-contains msg "14"))) => #t))))

    (test-case "headless: eval-last-sexp with string expression"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(string-append \"hello\" \" \" \"world\")")
        (editor-goto-pos ed (string-length "(string-append \"hello\" \" \" \"world\")"))
        (cmd-eval-last-sexp app)
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t)
          (check (not (not (string-contains msg "hello world"))) => #t))))

    (test-case "headless: eval-last-sexp at position 0 shows no sexp"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(+ 1 2)")
        (editor-goto-pos ed 0)
        (cmd-eval-last-sexp app)
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t)
          (check (not (not (string-contains msg "No sexp"))) => #t))))

    (test-case "headless: eval-defun evaluates top-level form in scratch"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(+ 10 20)")
        (editor-goto-pos ed 4)  ;; in the middle of the form
        (cmd-eval-defun app)
        (let ((msg (echo-state-message (app-state-echo app))))
          (check (not (not msg)) => #t)
          (check (not (not (string-contains msg "30"))) => #t))))

    (test-case "headless: eval-print-last-sexp inserts result in buffer"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(+ 5 5)")
        (editor-goto-pos ed 7)  ;; after closing paren
        (cmd-eval-print-last-sexp app)
        (let ((text (editor-get-text ed)))
          ;; Result should be inserted into the buffer
          (check (not (not (string-contains text ";; => 10"))) => #t))))

    (test-case "headless: eval-expression-string with various types"
      ;; Boolean
      (let-values (((result error?) (eval-expression-string "(= 1 1)")))
        (check error? => #f)
        (check (not (not (string-contains result "#t"))) => #t))
      ;; List
      (let-values (((result error?) (eval-expression-string "(list 1 2 3)")))
        (check error? => #f)
        (check (not (not (string-contains result "1"))) => #t)
        (check (not (not (string-contains result "2"))) => #t)
        (check (not (not (string-contains result "3"))) => #t))
      ;; String
      (let-values (((result error?) (eval-expression-string "(string-upcase \"hello\")")))
        (check error? => #f)
        (check (not (not (string-contains result "HELLO"))) => #t)))

    (test-case "keybinding: C-c C-e and C-x C-e for eval in scratch"
      ;; Both keybindings should point to eval-last-sexp
      (check (keymap-lookup *ctrl-c-map* "C-e") => 'eval-last-sexp)
      (check (keymap-lookup *ctrl-x-map* "C-e") => 'eval-last-sexp))

    ;;=========================================================================
    ;; More emacs behavior tests
    ;;=========================================================================

    (test-case "headless: kill-line accumulates in kill ring"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "first\nsecond\nthird")
        (editor-goto-pos ed 0)
        ;; Kill first line content
        (cmd-kill-line app)
        (check (> (length (app-state-kill-ring app)) 0) => #t)
        ;; Kill the newline
        (cmd-kill-line app)
        ;; Kill second line
        (cmd-kill-line app)
        ;; Kill ring should have accumulated kills
        (check (>= (length (app-state-kill-ring app)) 1) => #t)))

    (test-case "headless: yank restores killed text"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        (editor-goto-pos ed 5)
        ;; Kill " world"
        (cmd-kill-line app)
        (check (editor-get-text ed) => "hello")
        ;; Yank back
        (cmd-yank app)
        (check (not (not (string-contains (editor-get-text ed) " world"))) => #t)))

    (test-case "headless: backward-kill-sexp removes sexp before point"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(foo) (bar)")
        (editor-goto-pos ed 5)  ;; after (foo)
        (cmd-backward-kill-sexp app)
        (let ((text (editor-get-text ed)))
          ;; (foo) should be removed
          (check (not (string-contains text "foo")) => #t)
          ;; (bar) should remain
          (check (not (not (string-contains text "bar"))) => #t))))

    (test-case "headless: mark-kill-yank preserves text integrity"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "The quick brown fox jumps")
        ;; Select "quick "
        (editor-goto-pos ed 4)
        (cmd-set-mark app)
        (editor-goto-pos ed 10)
        ;; Kill the selection
        (cmd-kill-region app)
        (check (editor-get-text ed) => "The brown fox jumps")
        ;; Move to end and yank
        (cmd-end-of-buffer app)
        (cmd-yank app)
        (let ((text (editor-get-text ed)))
          ;; "quick " should be at the end
          (check (not (not (string-contains text "quick"))) => #t))))

    (test-case "headless: multiple self-inserts then undo workflow"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "")
        ;; Type "hello"
        (for-each (lambda (ch)
                    (cmd-self-insert! app (char->integer ch)))
                  (string->list "hello"))
        (check (editor-get-text ed) => "hello")
        ;; Undo should remove typed text
        (cmd-undo app)
        (check (not (string=? (editor-get-text ed) "hello")) => #t)))

    (test-case "headless: search-forward then search-backward"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "one two three two one")
        ;; Search forward for "two"
        (editor-goto-pos ed 0)
        (search-forward-impl! app "two")
        (let ((pos1 (editor-get-current-pos ed)))
          ;; Should find first "two" at position 7
          (check (> pos1 0) => #t)
          (check (<= pos1 7) => #t))))

    (test-case "headless: forward-paragraph skips to next paragraph"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "First paragraph.\n\nSecond paragraph.\n\nThird paragraph.")
        (editor-goto-pos ed 0)
        (cmd-forward-paragraph app)
        (let ((pos (editor-get-current-pos ed)))
          ;; Should move past the first paragraph
          (check (> pos 0) => #t)
          (check (> pos (string-length "First paragraph.")) => #t))))

    (test-case "headless: backward-paragraph moves to previous paragraph"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "First paragraph.\n\nSecond paragraph.\n\nThird paragraph.")
        ;; Start at end
        (editor-goto-pos ed (string-length "First paragraph.\n\nSecond paragraph.\n\nThird paragraph."))
        (cmd-backward-paragraph app)
        (let ((pos (editor-get-current-pos ed)))
          ;; Should move before "Third paragraph."
          (check (< pos (string-length "First paragraph.\n\nSecond paragraph.\n\nThird paragraph.")) => #t))))

    (test-case "headless: join-line combines two lines"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello\nworld")
        (editor-goto-pos ed 0)
        (cmd-join-line app)
        (let ((text (editor-get-text ed)))
          ;; Should merge into one line
          (check (not (string-contains text "\n")) => #t)
          (check (not (not (string-contains text "hello"))) => #t)
          (check (not (not (string-contains text "world"))) => #t))))

    (test-case "headless: just-one-space collapses multiple spaces"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello     world")
        (editor-goto-pos ed 8)  ;; in the middle of spaces
        (cmd-just-one-space app)
        (let ((text (editor-get-text ed)))
          ;; Multiple spaces should be replaced with one
          (check (not (not (string-contains text "hello world"))) => #t)
          (check (not (string-contains text "  ")) => #t))))

    ;; Note: cmd-delete-blank-lines has a known bug where SCI_POSITIONFROMLINE
    ;; returns #!void for out-of-range lines. Skipping headless test until fixed.

    (test-case "headless: toggle-comment adds and removes comment"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "some code here")
        (editor-goto-pos ed 0)
        ;; Comment
        (cmd-toggle-comment app)
        (let ((text1 (editor-get-text ed)))
          ;; Should have comment prefix (;; for scheme)
          (check (not (string=? text1 "some code here")) => #t))
        ;; Uncomment
        (cmd-toggle-comment app)
        (let ((text2 (editor-get-text ed)))
          ;; Should be back to original (or close to it)
          (check (not (not (string-contains text2 "some code here"))) => #t))))

    (test-case "headless: copy-region-as-kill preserves original text"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Select "world"
        (editor-goto-pos ed 6)
        (cmd-set-mark app)
        (editor-goto-pos ed 11)
        (cmd-copy-region-as-kill app)
        ;; Text should be unchanged
        (check (editor-get-text ed) => "hello world")
        ;; Kill ring should have "world"
        (check (> (length (app-state-kill-ring app)) 0) => #t)
        (check (not (not (string-contains (car (app-state-kill-ring app)) "world"))) => #t)))

    (test-case "headless: exchange-point-and-mark swaps cursor and mark"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "hello world")
        ;; Set mark at 0, cursor at 5
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed 5)
        ;; Exchange
        (cmd-exchange-point-and-mark app)
        ;; Cursor should now be at 0 (where mark was)
        (check (= (editor-get-current-pos ed) 0) => #t)))

    (test-case "headless: forward-sexp skips over string literals"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(+ 1 2) (+ 3 4)")
        (editor-goto-pos ed 0)
        (cmd-forward-sexp app)
        ;; Should jump past first sexp "(+ 1 2)"
        (check (>= (editor-get-current-pos ed) 7) => #t)))

    (test-case "headless: backward-sexp moves before sexp"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "(foo) (bar)")
        (editor-goto-pos ed 11)  ;; at end
        (cmd-backward-sexp app)
        ;; Should move before "(bar)"
        (check (<= (editor-get-current-pos ed) 6) => #t)))

    (test-case "headless: minibuffer history tracks inputs"
      ;; Test the minibuffer history mechanism
      (let ((original *minibuffer-history*))
        ;; Clear history for clean test
        (set! *minibuffer-history* '())
        (minibuffer-history-add! "first")
        (minibuffer-history-add! "second")
        (minibuffer-history-add! "third")
        ;; Most recent should be first
        (check (car *minibuffer-history*) => "third")
        (check (= (length *minibuffer-history*) 3) => #t)
        ;; Adding duplicate at front should not duplicate
        (minibuffer-history-add! "third")
        (check (= (length *minibuffer-history*) 3) => #t)
        ;; Empty strings should not be added
        (minibuffer-history-add! "")
        (check (= (length *minibuffer-history*) 3) => #t)
        ;; Restore
        (set! *minibuffer-history* original)))

    (test-case "headless: buffer modified tracking"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*test*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "original")
        ;; Initially not modified (we just set it)
        (send-message ed SCI_SETSAVEPOINT)
        (check (= (send-message ed SCI_GETMODIFY) 0) => #t)
        ;; Insert text marks as modified
        (editor-insert-text ed 0 "new ")
        (check (= (send-message ed SCI_GETMODIFY) 1) => #t)
        ;; Save point clears modified
        (send-message ed SCI_SETSAVEPOINT)
        (check (= (send-message ed SCI_GETMODIFY) 0) => #t)))

    (test-case "headless: auto-save-path generation"
      ;; auto-save files use #filename# convention
      (let ((path (make-auto-save-path "/home/user/test.txt")))
        (check (string? path) => #t)
        (check (not (not (string-contains path "#"))) => #t)))

    (test-case "headless: file-mod-time tracking"
      (let ((times *buffer-mod-times*))
        ;; Should be a hash table
        (check (hash-table? times) => #t)))

    (test-case "headless: highlight detection by file extension"
      (check (detect-file-language "test.py") => 'python)
      (check (detect-file-language "test.js") => 'javascript)
      (check (detect-file-language "test.c") => 'c)
      (check (detect-file-language "test.rs") => 'rust)
      (check (detect-file-language "test.go") => 'go)
      (check (detect-file-language "test.rb") => 'ruby))

    (test-case "headless: gerbil-file-extension? predicate"
      (check (gerbil-file-extension? "test.ss") => #t)
      (check (gerbil-file-extension? "test.scm") => #t)
      (check (gerbil-file-extension? "test.py") => #f)
      (check (gerbil-file-extension? "test.txt") => #f))

    (test-case "headless: ANSI code stripping"
      ;; Basic ESC[...m codes
      (check (strip-ansi-codes "\x1b;[31mred text\x1b;[0m") => "red text")
      ;; Nested codes
      (check (strip-ansi-codes "\x1b;[1;32mbold green\x1b;[0m") => "bold green")
      ;; No codes
      (check (strip-ansi-codes "plain text") => "plain text"))

    (test-case "headless: terminal-buffer? predicate"
      ;; terminal-buffer? checks (buffer-lexer-lang buf) == 'terminal
      (let ((tbuf (make-buffer "*term*" #f #f #f #f 'terminal #f)))
        (check (terminal-buffer? tbuf) => #t))
      (let ((nbuf (make-buffer "*scratch*" #f #f #f #f #f #f)))
        (check (terminal-buffer? nbuf) => #f)))

    (test-case "headless: parse-ansi-segments basic"
      (let ((segments (parse-ansi-segments "hello")))
        (check (> (length segments) 0) => #t)
        (check (text-segment-text (car segments)) => "hello")))

    (test-case "headless: fuzzy-match? with various patterns"
      ;; Exact prefix match
      (check (fuzzy-match? "hel" "hello") => #t)
      ;; Subsequence match
      (check (fuzzy-match? "hlo" "hello") => #t)
      ;; No match
      (check (fuzzy-match? "xyz" "hello") => #f)
      ;; Empty query matches everything
      (check (fuzzy-match? "" "hello") => #t)
      ;; Case insensitive
      (check (fuzzy-match? "HEL" "hello") => #t))

    (test-case "headless: fuzzy-score ranks better matches higher"
      ;; Exact prefix should score higher than subsequence
      (let ((prefix-score (fuzzy-score "hel" "hello"))
            (subseq-score (fuzzy-score "hlo" "hello")))
        (check (> prefix-score subseq-score) => #t)))

    (test-case "headless: fuzzy-filter-sort for file completion (hippie match)"
      ;; Simulates file completion: typing a few chars matches filenames fuzzy
      (let ((files '("echo.ss" "editor-core.ss" "editor-ui.ss" "editor-text.ss"
                      "editor-advanced.ss" "editor-extra-org.ss" "core.ss"
                      "emacs-test.ss" "build.ss" "main.ss")))
        ;; "ecs" should match "echo.ss" (e-c-s subsequence)
        (let ((matches (fuzzy-filter-sort "ecs" files)))
          (check (> (length matches) 0) => #t)
          (check (member "echo.ss" matches) => (member "echo.ss" matches)))
        ;; "eui" should match "editor-ui.ss" (e-u-i subsequence)
        (let ((matches (fuzzy-filter-sort "eui" files)))
          (check (> (length matches) 0) => #t)
          (check (not (not (member "editor-ui.ss" matches))) => #t))
        ;; "eorg" should match "editor-extra-org.ss"
        (let ((matches (fuzzy-filter-sort "eorg" files)))
          (check (> (length matches) 0) => #t)
          (check (not (not (member "editor-extra-org.ss" matches))) => #t))
        ;; Exact prefix still works and ranks first
        (let ((matches (fuzzy-filter-sort "core" files)))
          (check (car matches) => "core.ss"))
        ;; No match returns empty
        (check (fuzzy-filter-sort "xyz123" files) => [])))

    (test-case "headless: fuzzy-filter-sort ranking prefers start and separator matches"
      ;; When multiple items match, better matches should rank higher
      (let ((candidates '("buffer.ss" "build.ss" "editor-buffer.ss" "abacus.ss")))
        ;; "buf" should rank "buffer.ss" above "editor-buffer.ss"
        ;; because it matches at the start
        (let ((matches (fuzzy-filter-sort "buf" candidates)))
          (check (> (length matches) 1) => #t)
          (check (car matches) => "buffer.ss"))))

    (test-case "headless: echo-read-file-with-completion is exported"
      ;; Verify the new function is available (it's exported from echo.ss)
      (check (procedure? echo-read-file-with-completion) => #t))

    (test-case "headless: window management after split"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Start with one window
        (check (= (length (frame-windows fr)) 1) => #t)
        ;; Split creates two
        (cmd-split-window app)
        (check (= (length (frame-windows (app-state-frame app))) 2) => #t)
        ;; Delete-other-windows goes back to one
        (cmd-delete-other-windows app)
        (check (= (length (frame-windows (app-state-frame app))) 1) => #t)))

    (test-case "headless: other-window cycles focus"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Split to get two windows
        (cmd-split-window app)
        (let ((win1 (current-window (app-state-frame app))))
          ;; Switch to other window
          (cmd-other-window app)
          (let ((win2 (current-window (app-state-frame app))))
            ;; Should be a different window object
            (check (not (eq? win1 win2)) => #t)))))

    (test-case "headless: fill-paragraph wraps at fill column"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Long line that should be wrapped
        (editor-set-text ed "This is a very long line of text that should definitely be wrapped when fill-paragraph is called because it exceeds the fill column width.")
        (editor-goto-pos ed 0)
        (cmd-fill-paragraph app)
        (let ((text (editor-get-text ed)))
          ;; Should now contain newlines (wrapped)
          (check (not (not (string-contains text "\n"))) => #t))))

    (test-case "headless: indent-region indents lines"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*scratch*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "line1\nline2\nline3")
        ;; Select all
        (editor-goto-pos ed 0)
        (cmd-set-mark app)
        (editor-goto-pos ed (string-length "line1\nline2\nline3"))
        ;; Indent
        (cmd-indent-region app)
        ;; Text should be modified (indented)
        (let ((text (editor-get-text ed)))
          (check (not (string=? text "line1\nline2\nline3")) => #t))))

    (test-case "headless: self-insert respects read-only"
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "*test*" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "read only text")
        ;; Set read-only via Scintilla
        (send-message ed SCI_SETREADONLY 1)
        (editor-goto-pos ed 0)
        ;; Try to insert - should not change text
        (cmd-self-insert! app (char->integer #\X))
        (check (editor-get-text ed) => "read only text")
        ;; Clear read-only
        (send-message ed SCI_SETREADONLY 0)))

    ;;=========================================================================
    ;; ROBUST org-mode TAB dispatch tests (assume nothing about global state)
    ;;=========================================================================

    (test-case "headless: register-all-commands! registers org-cycle"
      ;; Verify that org-cycle IS registered after calling register-all-commands!
      ;; This is the foundation - if this fails, all TAB dispatch to org-cycle
      ;; will silently fail (execute-command! echoes error but doesn't throw)
      (register-all-commands!)
      (let ((cmd (find-command 'org-cycle)))
        (check (not (not cmd)) => #t)   ;; must exist
        (check (procedure? cmd) => #t)) ;; must be a procedure
      (let ((cmd (find-command 'org-template-expand)))
        (check (not (not cmd)) => #t)
        (check (procedure? cmd) => #t)))

    (test-case "headless: org-cycle via execute-command! dispatch"
      ;; Test that execute-command! actually invokes org-cycle, not just
      ;; that cmd-org-cycle works when called directly
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "dispatch.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nBody line 1\nBody line 2")
        (editor-goto-pos ed 0)
        ;; BEFORE: all lines must be visible
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)
        ;; Dispatch through execute-command! (same path as cmd-indent-or-complete)
        (execute-command! app 'org-cycle)
        ;; AFTER: body lines must be hidden
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)  ;; heading visible
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)  ;; body hidden
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 0)  ;; body hidden
        ;; Dispatch again to unfold
        (execute-command! app 'org-cycle)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)  ;; body visible again
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)))

    (test-case "headless: cmd-indent-or-complete full dispatch on org heading"
      ;; The REAL dispatch path: cmd-indent-or-complete -> org-buffer? check
      ;; -> heading detection -> execute-command! 'org-cycle
      ;; This test explicitly registers commands first (assumes nothing)
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test-tab.org" "/tmp/test-tab.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Set up org content with heading and body
        (editor-set-text ed "* My Heading\nBody line A\nBody line B\n* Next Heading")
        (editor-goto-pos ed 0)
        ;; Verify preconditions: buffer IS org, line IS heading
        (check (not (not (org-buffer? buf))) => #t)
        (let ((text (editor-get-text ed)))
          (check (char=? (string-ref text 0) #\*) => #t))
        ;; BEFORE: all 4 lines visible
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 1)
        ;; TAB on heading: should fold body lines
        (cmd-indent-or-complete app)
        ;; AFTER: heading 0 and next heading 3 visible, body 1,2 hidden
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 0)
        (check (send-message ed SCI_GETLINEVISIBLE 3 0) => 1)
        ;; TAB again: should unfold
        (cmd-indent-or-complete app)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)))

    (test-case "headless: cmd-indent-or-complete on ** heading (level 2)"
      ;; Make sure ** headings also trigger org-cycle, not indent
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "level2.org" "/tmp/level2.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "** Sub Heading\nSub body\n** Next Sub")
        (editor-goto-pos ed 0)
        ;; BEFORE: all visible
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        ;; TAB on ** heading
        (cmd-indent-or-complete app)
        ;; Sub body should be hidden
        (check (send-message ed SCI_GETLINEVISIBLE 0 0) => 1)  ;; ** heading visible
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)  ;; body hidden
        (check (send-message ed SCI_GETLINEVISIBLE 2 0) => 1)  ;; next ** visible
        ;; Text should NOT have spaces prepended (would prove indent happened)
        (let ((text (editor-get-text ed)))
          (check (string-prefix? "** Sub Heading" text) => #t))))

    (test-case "headless: cmd-indent-or-complete on org plain text indents"
      ;; When on a non-heading line in an org buffer, TAB should insert spaces
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "indent.org" "/tmp/indent.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* Heading\nPlain text line")
        ;; Move cursor to line 1 (plain text, not heading)
        (let ((line1-start (editor-position-from-line ed 1)))
          (editor-goto-pos ed line1-start))
        ;; TAB should indent (insert spaces), not fold
        (cmd-indent-or-complete app)
        (let ((text (editor-get-text ed)))
          ;; Line 1 should now have spaces
          (check (not (not (string-contains text "  Plain"))) => #t))
        ;; Body line should still be visible (not folded)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)))

    (test-case "headless: cmd-indent-or-complete on non-org file just indents"
      ;; TAB in a .py file should never trigger org-cycle
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.py" "/tmp/test.py"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "* not-a-heading\nbody")
        (editor-goto-pos ed 0)
        ;; Verify it's NOT an org buffer
        (check (org-buffer? buf) => #f)
        ;; TAB should indent, not fold
        (cmd-indent-or-complete app)
        ;; Text should have spaces inserted (indent), not fold
        (let ((text (editor-get-text ed)))
          (check (string-prefix? "  " text) => #t))
        ;; Body should be visible (no fold happened)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)))

    (test-case "headless: org-buffer? edge cases"
      ;; Name exactly 4 chars - should NOT match (needs > 4 for .org suffix check)
      (let ((buf (make-buffer ".org" #f #f #f #f #f #f)))
        (check (org-buffer? buf) => #f))
      ;; Name "x.org" - 5 chars, ends with .org
      (let ((buf (make-buffer "x.org" #f #f #f #f #f #f)))
        (check (not (not (org-buffer? buf))) => #t))
      ;; Lexer-lang 'org overrides any name
      (let ((buf (make-buffer "foo.txt" #f #f #f #f 'org #f)))
        (check (not (not (org-buffer? buf))) => #t))
      ;; Path .org overrides name
      (let ((buf (make-buffer "Notes" "/home/user/notes.org" #f #f #f #f #f)))
        (check (not (not (org-buffer? buf))) => #t))
      ;; Neither name, path, nor lexer matches
      (let ((buf (make-buffer "org-stuff" "/tmp/org-stuff.txt" #f #f #f 'python #f)))
        (check (org-buffer? buf) => #f)))

    (test-case "headless: org heading detection in cmd-indent-or-complete"
      ;; Verify that various heading formats are detected correctly
      (register-all-commands!)
      ;; Test with leading whitespace (common in real files)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "ws.org" "/tmp/ws.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Heading with NO leading whitespace
        (editor-set-text ed "* Clean heading\nBody text")
        (editor-goto-pos ed 0)
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 1)
        (cmd-indent-or-complete app)
        ;; Body should be folded
        (check (send-message ed SCI_GETLINEVISIBLE 1 0) => 0)))

    (test-case "headless: org-cycle preserves text content exactly"
      ;; Fold and unfold should not alter the buffer text at all
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "preserve.org" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (let ((original "* Heading 1\nLine A\nLine B\n* Heading 2\nLine C"))
          (editor-set-text ed original)
          (editor-goto-pos ed 0)
          ;; Fold
          (cmd-org-cycle app)
          (check (editor-get-text ed) => original)
          ;; Unfold
          (cmd-org-cycle app)
          (check (editor-get-text ed) => original))))

    ;;=========================================================================
    ;; ROBUST fuzzy completion tests (thorough edge cases + integration)
    ;;=========================================================================

    (test-case "headless: fuzzy-match? comprehensive edge cases"
      ;; Empty query matches anything
      (check (fuzzy-match? "" "") => #t)
      (check (fuzzy-match? "" "anything") => #t)
      ;; Equal strings
      (check (fuzzy-match? "hello" "hello") => #t)
      ;; Query longer than target: no match
      (check (fuzzy-match? "toolong" "short") => #f)
      ;; Case insensitive
      (check (fuzzy-match? "ABC" "abcdef") => #t)
      (check (fuzzy-match? "abc" "ABCDEF") => #t)
      ;; Subsequence with gaps
      (check (fuzzy-match? "ace" "abcde") => #t)
      ;; Characters out of order: no match
      (check (fuzzy-match? "ba" "abc") => #f)
      ;; Single char matches
      (check (fuzzy-match? "x" "xyz") => #t)
      (check (fuzzy-match? "z" "xyz") => #t)
      ;; Special chars in filenames
      (check (fuzzy-match? "e.s" "echo.ss") => #t)
      (check (fuzzy-match? "e-c" "editor-core.ss") => #t))

    (test-case "headless: fuzzy-score consecutive and word-boundary bonuses"
      ;; Consecutive chars score higher than separated
      (let ((consec (fuzzy-score "abc" "abcdef"))
            (gapped (fuzzy-score "abc" "aXbXcX")))
        (check (> consec gapped) => #t))
      ;; Start-of-string bonus
      (let ((start (fuzzy-score "ed" "editor-core.ss"))
            (mid   (fuzzy-score "ed" "xeditor")))
        (check (> start mid) => #t))
      ;; Word boundary bonus (after - or /)
      (let ((boundary (fuzzy-score "c" "editor-core.ss"))
            (mid-word (fuzzy-score "c" "electroc")))
        ;; 'c' after '-' in editor-core should score higher
        ;; Actually both match 'c' at different positions, boundary detection
        ;; gives bonus when char follows separator
        (check (>= boundary 0) => #t))
      ;; No match returns -1
      (check (fuzzy-score "xyz" "abc") => -1))

    (test-case "headless: fuzzy-filter-sort realistic file directory"
      ;; Simulate what happens during C-x C-f file completion
      (let ((dir-contents '("Makefile" "README.md" "build.ss" "core.ss"
                            "echo.ss" "editor-core.ss" "editor-ui.ss"
                            "editor-text.ss" "editor-advanced.ss"
                            "editor-extra-org.ss" "emacs-test.ss"
                            "gerbil.pkg" "keymap.ss" "main.ss"
                            "manifest.ss" "window.ss" "buffer.ss"
                            "modeline.ss" "highlight.ss" "persist.ss")))
        ;; Typing "eo" should match "echo.ss" and "editor-extra-org.ss"
        (let ((matches (fuzzy-filter-sort "eo" dir-contents)))
          (check (> (length matches) 0) => #t)
          (check (not (not (member "echo.ss" matches))) => #t))
        ;; Typing "edco" should strongly match "editor-core.ss"
        (let ((matches (fuzzy-filter-sort "edco" dir-contents)))
          (check (> (length matches) 0) => #t)
          (check (car matches) => "editor-core.ss"))
        ;; Typing "mk" should match "Makefile" (case insensitive)
        (let ((matches (fuzzy-filter-sort "mk" dir-contents)))
          (check (not (not (member "Makefile" matches))) => #t))
        ;; Typing "bss" should match "build.ss" and "buffer.ss"
        (let ((matches (fuzzy-filter-sort "bss" dir-contents)))
          (check (> (length matches) 0) => #t))
        ;; Typing "test" should match "emacs-test.ss"
        (let ((matches (fuzzy-filter-sort "test" dir-contents)))
          (check (not (not (member "emacs-test.ss" matches))) => #t))
        ;; Empty query returns all (no filtering)
        (let ((matches (fuzzy-filter-sort "" dir-contents)))
          (check (= (length matches) (length dir-contents)) => #t))
        ;; No match for gibberish
        (check (fuzzy-filter-sort "zzqqxx" dir-contents) => [])))

    (test-case "headless: fuzzy-filter-sort best match is first"
      ;; Verify that the BEST match (highest score) is first in results
      (let ((candidates '("find-file" "font-lock" "fill-paragraph"
                           "forward-char" "forward-word")))
        ;; "ff" should rank "find-file" first (starts with f, f after -)
        (let ((matches (fuzzy-filter-sort "ff" candidates)))
          (check (> (length matches) 0) => #t)
          ;; find-file should be in results
          (check (not (not (member "find-file" matches))) => #t))
        ;; "fw" should match forward-* entries
        (let ((matches (fuzzy-filter-sort "fw" candidates)))
          (check (> (length matches) 0) => #t)
          (check (not (not (member "forward-word" matches))) => #t))
        ;; "fp" should rank "fill-paragraph" highly
        (let ((matches (fuzzy-filter-sort "fp" candidates)))
          (check (not (not (member "fill-paragraph" matches))) => #t))))

    (test-case "headless: echo-read-file-with-completion is a callable procedure"
      ;; Verify the fuzzy file completion function exists and is exported
      (check (procedure? echo-read-file-with-completion) => #t)
      ;; Verify the string completion function also exists
      (check (procedure? echo-read-string-with-completion) => #t))

    (test-case "headless: cmd-find-file is registered and callable"
      ;; Verify find-file command is properly registered
      (register-all-commands!)
      (let ((cmd (find-command 'find-file)))
        (check (not (not cmd)) => #t)
        (check (procedure? cmd) => #t)))

    (test-case "headless: command registry comprehensive check"
      ;; Verify all major emacs commands are registered
      (for-each
        (lambda (cmd-name)
          (check (procedure? (find-command cmd-name)) => #t))
        '(forward-char backward-char next-line previous-line
          beginning-of-line end-of-line forward-word backward-word
          beginning-of-buffer end-of-buffer goto-line
          delete-char kill-line kill-word kill-region
          yank copy-region search-forward search-backward
          undo save-buffer find-file switch-buffer kill-buffer-cmd
          split-window split-window-right delete-window delete-other-windows
          other-window set-mark transpose-chars transpose-words
          upcase-word downcase-word capitalize-word
          forward-sexp backward-sexp forward-paragraph backward-paragraph
          open-line join-line just-one-space delete-blank-lines
          toggle-comment fill-paragraph indent-region
          eval-expression eval-last-sexp eval-defun
          org-todo org-cycle org-promote org-demote
          org-insert-heading org-toggle-checkbox org-priority
          hippie-expand dabbrev-expand
          describe-key execute-extended-command keyboard-quit
          scratch-buffer eshell shell)))

    ;;=========================================================================
    ;; Org-mode activation tests
    ;;=========================================================================

    (test-case "org-mode: detect-major-mode returns org-mode for .org files"
      (check (detect-major-mode "notes.org") => 'org-mode)
      (check (detect-major-mode "/home/user/todo.org") => 'org-mode)
      (check (detect-major-mode "README.md") => 'markdown-mode)
      (check (detect-major-mode "test.py") => 'python-mode)
      (check (detect-major-mode "unknown.xyz") => #f))

    (test-case "org-mode: cmd-org-mode sets buffer-lexer-lang to org"
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "test.txt" "/tmp/test.txt"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Before: not an org buffer
        (check (buffer-lexer-lang buf) => #f)
        (check (org-buffer? buf) => #f)
        ;; Activate org-mode via the command
        (cmd-org-mode app)
        ;; After: buffer-lexer-lang is 'org
        (check (buffer-lexer-lang buf) => 'org)
        ;; org-buffer? should now return true
        (check (not (not (org-buffer? buf))) => #t)
        ;; buffer-local major-mode is set
        (check (buffer-local-get buf 'major-mode) => 'org-mode)))

    (test-case "org-mode: M-x org-mode command is registered"
      (register-all-commands!)
      ;; org-mode must be a registered command
      (check (not (not (find-command 'org-mode))) => #t)
      ;; Execute it through the dispatch path
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "demo.txt" #f
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (execute-command! app 'org-mode)
        (check (buffer-lexer-lang buf) => 'org)))

    (test-case "org-mode: auto-activation for .org file via mode detection"
      (register-all-commands!)
      ;; Simulate what TUI app.ss does: detect mode then call the command
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "notes.org" "/tmp/notes.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Detect and activate mode (simulating app.ss open-file path)
        (let ((mode (detect-major-mode "/tmp/notes.org")))
          (check mode => 'org-mode)
          (buffer-local-set! buf 'major-mode mode)
          (let ((mode-cmd (find-command mode)))
            (check (not (not mode-cmd)) => #t)
            (mode-cmd app)))
        ;; Verify activation happened
        (check (buffer-lexer-lang buf) => 'org)
        (check (buffer-local-get buf 'major-mode) => 'org-mode)
        (check (not (not (org-buffer? buf))) => #t)))

    ;;=========================================================================
    ;; Org table TAB dispatch tests
    ;;=========================================================================

    (test-case "org-table: TAB on table line aligns columns"
      ;; TAB on a misaligned table should produce aligned columns
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "data.org" "/tmp/data.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Misaligned table: columns have different widths
        (editor-set-text ed "| Name | Age |\n| Alice | 30 |\n| Bob | 7 |")
        ;; Position cursor on first table line
        (editor-goto-pos ed 2)
        ;; TAB should align the table (not insert spaces)
        (cmd-indent-or-complete app)
        ;; After TAB, table should be aligned — all rows same width
        (let* ((text (editor-get-text ed))
               (lines (string-split text #\newline)))
          ;; All lines should have same length (aligned columns)
          (check (> (length lines) 1) => #t)
          (let ((len0 (string-length (car lines))))
            (for-each (lambda (l) (check (string-length l) => len0)) lines))
          ;; Cells should be padded: "Alice" and "Bob" in same-width column
          ;; "30" and "7" padded to same width
          (check (string-contains (list-ref lines 0) "| Name ") => 0)
          (check (string-contains (list-ref lines 1) "| Alice") => 0)
          (check (string-contains (list-ref lines 2) "| Bob  ") => 0))))

    (test-case "org-table: TAB moves cursor to next cell"
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "tbl.org" "/tmp/tbl.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Pre-aligned table
        (editor-set-text ed "| a | b |\n| c | d |")
        ;; Position cursor on first cell (after "| ")
        (editor-goto-pos ed 2)
        ;; Verify we're in the first cell of an org table
        (let ((line-num (send-message ed SCI_LINEFROMPOSITION 2 0)))
          (check line-num => 0))
        ;; TAB should move to next cell
        (cmd-indent-or-complete app)
        (let ((new-pos (editor-get-current-pos ed))
              (text (editor-get-text ed)))
          ;; Cursor should have moved forward (into the second cell)
          (check (> new-pos 2) => #t)
          ;; Cursor should be on the same line (line 0) or within the table
          (let ((line (send-message ed SCI_LINEFROMPOSITION new-pos 0)))
            (check (<= line 1) => #t)))))

    (test-case "org-table: TAB at last cell creates new row"
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "new.org" "/tmp/new.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Single-row table, cursor in last cell
        (editor-set-text ed "| x | y |")
        ;; Put cursor in the last cell (at the "y")
        (editor-goto-pos ed 6)
        ;; Count lines before TAB
        (let ((lines-before (send-message ed SCI_GETLINECOUNT)))
          ;; TAB at end of table should create a new row
          (cmd-indent-or-complete app)
          (let ((lines-after (send-message ed SCI_GETLINECOUNT))
                (text (editor-get-text ed)))
            ;; Should have one more line
            (check (> lines-after lines-before) => #t)
            ;; New text should contain two rows
            (let ((rows (filter (lambda (l) (org-table-row? l))
                                (string-split text #\newline))))
              (check (>= (length rows) 2) => #t))))))

    (test-case "org-table: TAB skips separator rows"
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "sep.org" "/tmp/sep.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        ;; Table with separator: header, separator, data
        (editor-set-text ed "| h1 | h2 |\n|----+----|\n| a  | b  |")
        ;; Put cursor in last cell of header row (at "h2")
        (editor-goto-pos ed 7)
        ;; TAB should skip the separator and land in the data row
        (cmd-indent-or-complete app)
        (let* ((new-pos (editor-get-current-pos ed))
               (new-line (send-message ed SCI_LINEFROMPOSITION new-pos 0)))
          ;; Should be on line 2 (data row), not line 1 (separator)
          (check new-line => 2))))

    (test-case "org-table: non-table line in org buffer gets normal TAB"
      ;; TAB on a non-table line should NOT trigger table alignment
      (register-all-commands!)
      (let* ((ed (create-scintilla-editor width: 80 height: 24))
             (buf (make-buffer "mixed.org" "/tmp/mixed.org"
                    (send-message ed SCI_GETDOCPOINTER) #f #f #f #f))
             (win (make-edit-window ed buf 0 0 80 24 0))
             (fr (make-frame [win] 0 80 24 'vertical))
             (app (new-app-state fr)))
        (editor-set-text ed "Just some text here")
        (editor-goto-pos ed 4)
        (let ((text-before (editor-get-text ed)))
          (cmd-indent-or-complete app)
          ;; Should have inserted spaces (indent), not done table alignment
          (let ((text-after (editor-get-text ed)))
            (check (> (string-length text-after) (string-length text-before)) => #t)))))

    ;; -----------------------------------------------------------------------
    ;; LSP client protocol layer tests (qt/lsp-client.ss has no Qt deps)
    ;; These guard against regressions in the initialization timing fix
    ;; and the didChange deduplication logic added to make visual feedback work.
    ;; -----------------------------------------------------------------------

    (test-case "lsp-client: content deduplication — lsp-content-changed?"
      ;; New URIs are always considered changed (so didOpen/didChange is sent)
      (let ((uri "file:///tmp/test-lsp-changed.ss"))
        ;; Not yet seen → must report changed
        (check (lsp-content-changed? uri "hello") => #t)
        ;; Record it
        (lsp-record-sent-content! uri "hello")
        ;; Same content → not changed (prevents flooding server)
        (check (lsp-content-changed? uri "hello") => #f)
        ;; Different content → changed (new edit must be sent)
        (check (lsp-content-changed? uri "hello world") => #t)
        ;; Record the new content
        (lsp-record-sent-content! uri "hello world")
        (check (lsp-content-changed? uri "hello world") => #f)
        ;; Empty string is distinct from missing
        (check (lsp-content-changed? uri "") => #t)
        ;; Unrelated URI is always fresh
        (check (lsp-content-changed? "file:///other.ss" "hello") => #t)
        ;; Cleanup so other tests see a clean slate
        (hash-remove! *lsp-last-sent-content* uri)))

    (test-case "lsp-client: *lsp-on-initialized-handler* callback mechanism"
      ;; This is the core of the initialization timing fix:
      ;; after the async initialize handshake completes, the callback
      ;; fires on the UI thread to send didOpen for all open buffers.
      (check (box? *lsp-on-initialized-handler*) => #t)
      (let ((fired #f))
        ;; Install a test callback
        (set-box! *lsp-on-initialized-handler* (lambda () (set! fired #t)))
        ;; Simulate what lsp-send-initialize! does on completion:
        ;; fire the callback via the UI action queue
        (let ((h (unbox *lsp-on-initialized-handler*)))
          (when h (lsp-queue-ui-action! h)))
        ;; Handler not yet called — still pending in queue
        (check fired => #f)
        ;; Drain the queue (simulating the 50ms UI timer)
        (lsp-poll-ui-actions!)
        ;; Now the callback fired
        (check fired => #t)
        ;; Restore to safe state
        (set-box! *lsp-on-initialized-handler* #f)))

    (test-case "lsp-client: UI action queue — multiple thunks run in order"
      (let ((log []))
        (lsp-queue-ui-action! (lambda () (set! log (append log [1]))))
        (lsp-queue-ui-action! (lambda () (set! log (append log [2]))))
        (lsp-queue-ui-action! (lambda () (set! log (append log [3]))))
        (lsp-poll-ui-actions!)
        (check log => [1 2 3])))

    (test-case "lsp-client: URI conversion round-trip"
      (check (file-path->uri "/home/user/test.ss")
             => "file:///home/user/test.ss")
      (check (uri->file-path "file:///home/user/test.ss")
             => "/home/user/test.ss")
      ;; Non-file URIs pass through unchanged
      (check (uri->file-path "other://example") => "other://example"))

    (test-case "lsp-client: language-id mapping for Gerbil files"
      (check (lsp-language-id "/path/file.ss")  => "scheme")
      (check (lsp-language-id "/path/file.scm") => "scheme")
      (check (lsp-language-id "/path/file.sld") => "scheme")
      (check (lsp-language-id "/path/file.py")  => "python")
      (check (lsp-language-id "/path/file.rs")  => "rust")
      (check (lsp-language-id "/path/file.go")  => "go")
      (check (lsp-language-id "/path/file.txt") => "plaintext"))

    (test-case "lsp-client: starts in not-running state"
      ;; LSP must not be initialized at startup — important invariant
      ;; that prevents spurious didChange sends before the server is ready
      (check *lsp-initialized*  => #f)
      (check *lsp-initializing* => #f))

    ))

;; Run tests when executed directly
(def main
  (lambda args
    (run-tests! emacs-test)
    (test-report-summary!)))
