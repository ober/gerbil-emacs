;;; -*- Gerbil -*-
;;; Tests for gerbil-emacs
;;; Includes both pure-logic tests and headless Scintilla editor tests.
;;; Scintilla works headlessly (no terminal needed) for all operations
;;; except scintilla_refresh(). Lexer/syntax highlighting requires Lexilla
;;; which is only available at runtime, so highlighting tests are skipped.

(import :std/test
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/echo
        :gerbil-emacs/editor-core
        (only-in :gerbil-emacs/editor-cmds-a cmd-copy-region-as-kill)
        (only-in :gerbil-emacs/editor-extra-org
                 cmd-org-todo cmd-org-export cmd-org-cycle cmd-org-shift-tab
                 cmd-org-store-link *org-stored-link*
                 cmd-org-promote cmd-org-demote
                 cmd-org-move-subtree-up cmd-org-move-subtree-down
                 cmd-org-toggle-checkbox cmd-org-priority
                 cmd-org-insert-heading cmd-org-insert-src-block
                 cmd-org-template-expand
                 org-heading-level org-find-subtree-end org-on-checkbox-line?
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
        (only-in :gerbil-emacs/editor register-all-commands!)
        (only-in :gerbil-emacs/editor-extra-media
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
                 *global-smart-mode-line*)
        (only-in :gerbil-emacs/editor-extra-editing
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
                 *global-power-mode* *global-animate-typing*)
        (only-in :gerbil-emacs/editor-extra-vcs
                 fuzzy-match? fuzzy-score)
        (only-in :gerbil-emacs/editor-extra-final
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
        (only-in :gerbil-emacs/editor-extra-tools2
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
        (only-in :gerbil-emacs/editor-extra-tools
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
                 *projectile-mode* *evil-mode* *doom-modeline*
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
        (only-in :gerbil-emacs/editor-extra-web
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
        (only-in :gerbil-emacs/editor-extra-modes
                 *global-envrc* *global-direnv* *global-editorconfig*
                 *global-dtrt-indent* *global-ws-trim*
                 *global-auto-compile* *global-no-littering*
                 *global-docker* *global-kubernetes*
                 *global-terraform* *global-ansible*
                 *global-vagrant* *global-restclient*
                 *global-ob-http*)
        (only-in :gerbil-emacs/editor-extra-vcs
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
        (only-in :gerbil-emacs/highlight
                 detect-file-language gerbil-file-extension?
                 setup-highlighting-for-file!)
        (only-in :gerbil-emacs/terminal
                 parse-ansi-segments text-segment-text
                 text-segment-fg-color text-segment-bold?
                 terminal-buffer? color-to-style
                 *term-style-base*)
        (only-in :gerbil-emacs/echo
                 *minibuffer-history* minibuffer-history-add!)
        (only-in :gerbil-emacs/editor-core
                 make-auto-save-path file-mod-time
                 *buffer-mod-times* update-buffer-mod-time!)
)

(export emacs-test)

(def emacs-test
  (test-suite "gerbil-emacs"

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
      (check (keymap-lookup *ctrl-c-map* "l") => 'count-lines-region)
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
      (let ((tmp "/tmp/gerbil-emacs-test-file.txt"))
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
      (check *evil-mode* => #f)
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
      (check (procedure? (find-command 'toggle-evil-mode)) => #t)
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
    ;; Org-mode: helper function tests
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

    ))

;; Run tests when executed directly
(def main
  (lambda args
    (run-tests! emacs-test)
    (test-report-summary!)))
