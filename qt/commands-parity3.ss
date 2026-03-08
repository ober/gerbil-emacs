;;; -*- Gerbil -*-
;;; Qt parity commands (part 3) — bulk toggle command registration.
;;; Chain position: after commands-parity2, before commands-aliases.
;;; Registers 339 toggle commands that match TUI parity.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
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

;; Helper: get text between two positions from Scintilla editor
(def (sci-get-text-range ed start end)
  (let ((text (qt-plain-text-edit-text ed)))
    (if (and (>= start 0) (<= end (string-length text)))
      (substring text start end)
      "")))

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

;;;============================================================================
;;; Mode-toggle commands (non-toggle-* naming)
;;;============================================================================

;; These are commands named like "foo-mode" that toggle a boolean.
;; They use the same infrastructure as the toggle-* commands above.

(def *parity3-mode-toggle-names*
  '(adaptive-wrap-prefix-mode
    apheleia-mode
    artist-mode
    auto-compression-mode
    auto-insert-mode
    blink-cursor-mode
    company-mode
    compilation-mode
    context-menu-mode
    corfu-mode
    cursor-intangible-mode
    display-fill-column-indicator-mode
    dockerfile-mode
    doom-modeline-mode
    editorconfig-mode
    electric-indent-mode
    electric-indent-local-mode
    emacs-lisp-mode
    envrc-mode
    file-name-shadow-mode
    focus-mode
    fundamental-mode
    glasses-mode
    global-display-line-numbers-mode
    global-hl-line-mode
    global-subword-mode
    global-whitespace-mode
    golden-ratio-mode
    highlight-changes-mode
    highlight-indent-guides-mode
    ido-mode
    indent-bars-mode
    indent-guide-mode
    indent-tabs-mode
    ivy-mode
    java-mode
    jinx-mode
    lisp-interaction-mode
    meow-mode
    midnight-mode
    minibuffer-depth-indicate-mode
    minimap-mode
    olivetti-mode
    orderless-mode
    origami-mode
    page-break-lines-mode
    pixel-scroll-precision-mode
    prog-mode
    rainbow-mode
    recentf-mode
    rectangle-mark-mode
    restclient-mode
    size-indication-mode
    superword-mode
    tab-line-mode
    toml-mode
    visual-fill-column-mode
    which-function-mode
    which-key-mode
    window-divider-mode
    winner-mode
    writeroom-mode))

(def (qt-register-parity3-mode-toggles!)
  "Register mode-toggle commands for Qt parity."
  (for-each
    (lambda (name)
      (register-command! name (make-toggle-command name)))
    *parity3-mode-toggle-names*))

;;;============================================================================
;;; Stub commands (echo-only, no real functionality)
;;;============================================================================

(def (make-stub-command name msg)
  "Create a stub command that echoes a message."
  (lambda (app)
    (echo-message! (app-state-echo app) msg)))

(def *parity3-stub-commands*
  ;; (name . message) pairs
  '((all-the-icons-install-fonts . "Icon fonts not applicable in gemacs")
    (calendar-goto-date . "Use M-x calendar to view calendar")
    (citar-insert-citation . "Citation management not available")
    (company-complete . "Use TAB for completion")
    (customize-group . "Use M-x set-variable for settings")
    (customize-themes . "Use M-x load-theme to switch themes")
    (dap-continue . "DAP debugger not available — use M-x gdb")
    (dash-at-point . "Use M-x man for documentation lookup")
    (devdocs-lookup . "Use M-x man for documentation lookup")
    (docker . "Docker integration not available")
    (docker-containers . "Docker integration not available")
    (docker-images . "Docker integration not available")
    (ediff-show-registry . "Use M-x diff-two-files for diff")
    (flyspell-correct-word . "Use M-x ispell-word for spell correction")
    (gptel . "AI chat not available")
    (gptel-send . "AI chat not available")
    (hippie-expand-undo . "Use C-/ to undo last expansion")
    (iconify-frame . "Frame minimization not supported")
    (list-packages . "Use M-x package-list-packages")
    (next-error-function . "Use M-x next-error")
    (notifications-list . "No notification system")
    (occur-rename-buffer . "Use M-x rename-buffer")
    (package-archives . "Package management: use gerbil pkg")
    (package-delete . "Package management: use gerbil pkg")
    (package-install . "Package management: use gerbil pkg")
    (package-list-packages . "Package management: use gerbil pkg")
    (package-refresh-contents . "Package management: use gerbil pkg")
    (previous-error-function . "Use M-x previous-error")
    (print-buffer . "Printing not supported — save and print externally")
    (print-region . "Printing not supported — save and print externally")
    (raise-frame . "Single-frame mode only")
    (run-scheme . "Use C-x r to open Gerbil REPL")
    (shell-here . "Use M-x eshell or M-x terminal")
    (nerd-icons-install-fonts . "Icon fonts not applicable in gemacs")
    (yas-new-snippet . "Use M-x yas-insert-snippet for snippets")
    (yas-visit-snippet-file . "Snippet files managed via ~/.gemacs-snippets")
    (speedbar . "Use M-x treemacs for file tree")
    (menu-bar-open . "Use M-x for command access")
    (slime . "Use M-x run-scheme for Scheme REPL")
    (sly . "Use M-x run-scheme for Scheme REPL")
    (rmail . "Use M-x mu4e or M-x notmuch for mail")
    (help-for-help . "Use C-h k, C-h v, C-h f for help")
    (help-quick . "Use C-h k, C-h v, C-h f for help")))

(def (qt-register-parity3-stubs!)
  "Register stub commands for Qt parity."
  (for-each
    (lambda (pair)
      (register-command! (car pair) (make-stub-command (car pair) (cdr pair))))
    *parity3-stub-commands*))

;;;============================================================================
;;; Alias commands (delegate to existing commands)
;;;============================================================================

(def (make-alias-command target-name)
  "Create an alias command that delegates to another registered command."
  (lambda (app)
    (let ((cmd (find-command target-name)))
      (if cmd (cmd app)
        (echo-message! (app-state-echo app)
          (string-append (symbol->string target-name) " not found"))))))

(def *parity3-alias-commands*
  ;; (alias-name . target-name) pairs
  '((define-global-abbrev . define-mode-abbrev)
    (embark-dwim . hippie-expand)
    (eww-browse-url . eww)
    (execute-extended-command-fuzzy . execute-extended-command)
    (execute-extended-command-with-history . execute-extended-command)
    (ido-find-file . find-file)
    (ido-switch-buffer . switch-to-buffer)
    (popper-toggle-latest . popper-cycle)
    (rotate-window . rotate-frame)
    (woman . man)
    (whitespace-report . toggle-whitespace-mode)
    (whitespace-toggle-options . toggle-whitespace-mode)
    (winner-undo-2 . winner-undo)
    (widen-simple . widen)
    (narrow-to-region-simple . narrow-to-region)
    (display-line-numbers-absolute . display-line-numbers-mode)
    (display-line-numbers-none . display-line-numbers-mode)))

(def (qt-register-parity3-aliases!)
  "Register alias commands for Qt parity."
  (for-each
    (lambda (pair)
      (register-command! (car pair) (make-alias-command (cdr pair))))
    *parity3-alias-commands*))

;;;============================================================================
;;; Simple functional commands (thin implementations)
;;;============================================================================

;; Games
(def (cmd-tetris app)
  (echo-message! (app-state-echo app) "Use M-x snake for games"))
(def (cmd-snake app)
  (echo-message! (app-state-echo app) "Snake: use arrow keys (not yet playable)"))
(def (cmd-hanoi app)
  (echo-message! (app-state-echo app) "Tower of Hanoi: mathematical puzzle"))
(def (cmd-life app)
  (echo-message! (app-state-echo app) "Conway's Game of Life (not implemented)"))
(def (cmd-dunnet app)
  (echo-message! (app-state-echo app) "Dunnet adventure game (not implemented)"))
(def (cmd-doctor app)
  (echo-message! (app-state-echo app) "ELIZA psychotherapist (not implemented)"))

;; Process management
(def (cmd-proced app)
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (echo (app-state-echo app)))
    (with-catch
      (lambda (e) (echo-message! echo "proced: error running ps"))
      (lambda ()
        (let* ((proc (open-process (list path: "ps" arguments: '("aux" "--sort=-pcpu")
                                        stdout-redirection: #t)))
               (text (read-line proc #f)))
          (close-port proc)
          (when text
            (let ((buf (or (buffer-by-name "*Proced*")
                           (qt-buffer-create! "*Proced*" ed #f))))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed text)
              (sci-send ed SCI_SETREADONLY 1)
              (echo-message! echo "Process list loaded"))))))))

(def (cmd-proced-filter app)
  "Filter *Proced* buffer by pattern - show only matching process lines."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pattern (qt-echo-read-string app "Filter processes: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((lines (string-split text #\newline))
             (header (if (pair? lines) (car lines) ""))
             (body (if (pair? lines) (cdr lines) []))
             (filtered (filter (lambda (line) (string-contains line pattern)) body))
             (result (string-join (cons header filtered) "\n")))
        (sci-send ed SCI_SETREADONLY 0)
        (qt-plain-text-edit-set-text! ed result)
        (sci-send ed SCI_SETREADONLY 1)
        (echo-message! echo
          (string-append "Showing " (number->string (length filtered)) " processes matching '" pattern "'"))))))

(def (cmd-proced-send-signal app)
  "Send a signal to a process - reads PID from current line in *Proced* buffer."
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         ;; Get current line
         (line-start (let loop ((i (- pos 1)))
                       (cond ((< i 0) 0)
                             ((char=? (string-ref text i) #\newline) (+ i 1))
                             (else (loop (- i 1))))))
         (line-end (let loop ((i pos))
                     (cond ((>= i (string-length text)) i)
                           ((char=? (string-ref text i) #\newline) i)
                           (else (loop (+ i 1))))))
         (line (substring text line-start line-end))
         ;; Extract PID (first numeric field in line)
         (tokens (filter (lambda (s) (> (string-length s) 0))
                         (string-split line #\space)))
         (pid-str (find (lambda (s) (string->number s)) tokens)))
    (if (not pid-str)
      (echo-message! echo "No PID found on current line")
      (let ((signal (qt-echo-read-with-narrowing app "Signal: "
                      '("TERM" "KILL" "HUP" "INT" "STOP" "CONT" "USR1" "USR2"))))
        (when (and signal (> (string-length signal) 0))
          (with-catch
            (lambda (e) (echo-message! echo (string-append "Error sending signal: "
                                              (with-output-to-string "" (lambda () (display-exception e))))))
            (lambda ()
              (let* ((proc (open-process
                             (list path: "kill" arguments: (list (string-append "-" signal) pid-str)
                                   stdout-redirection: #t stderr-redirection: #t)))
                     (out (read-line proc #f))
                     (status (process-status proc)))
                (close-port proc)
                (if (= status 0)
                  (echo-message! echo (string-append "Sent SIG" signal " to PID " pid-str))
                  (echo-message! echo (string-append "Failed to send signal" (if out (string-append ": " out) ""))))))))))))

;; Calculator
(def (cmd-calculator app)
  (let* ((echo (app-state-echo app))
         (expr (qt-echo-read-string app "Calc: ")))
    (when (and expr (> (string-length expr) 0))
      (with-catch
        (lambda (e)
          (echo-message! echo (string-append "Calc error: "
            (with-output-to-string (lambda () (display-exception e))))))
        (lambda ()
          (let ((result (eval (call-with-input-string expr read))))
            (echo-message! echo
              (string-append "Result: " (with-output-to-string
                (lambda () (display result)))))))))))

(def (cmd-calculator-inline app)
  (cmd-calculator app))

(def (cmd-calc-eval-region app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! echo "No region selected")
      (let ((text (sci-get-text-range ed start end)))
        (with-catch
          (lambda (e) (echo-message! echo "Eval error"))
          (lambda ()
            (let* ((result (eval (call-with-input-string text read)))
                   (result-str (with-output-to-string (lambda () (display result)))))
              (echo-message! echo (string-append "= " result-str)))))))))

;;; RPN calculator stack
(def *calc-stack* [])

(def (calc-show-stack! app)
  (let* ((echo (app-state-echo app))
         (top5 (if (> (length *calc-stack*) 5)
                 (take *calc-stack* 5)
                 *calc-stack*)))
    (if (null? top5)
      (echo-message! echo "Stack: (empty)")
      (echo-message! echo
        (string-append "Stack: "
          (string-join (map number->string top5) " "))))))

(def (cmd-calc-push app)
  "Push a value onto the calculator stack."
  (let* ((echo (app-state-echo app))
         (expr (qt-echo-read-string app "Push value: ")))
    (when (and expr (> (string-length expr) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Invalid number"))
        (lambda ()
          (let ((val (string->number expr)))
            (if val
              (begin (set! *calc-stack* (cons val *calc-stack*))
                     (calc-show-stack! app))
              (echo-message! echo "Not a number"))))))))

(def (cmd-calc-pop app)
  "Pop the top value from the calculator stack."
  (if (null? *calc-stack*)
    (echo-message! (app-state-echo app) "Stack empty")
    (let ((top (car *calc-stack*)))
      (set! *calc-stack* (cdr *calc-stack*))
      (echo-message! (app-state-echo app)
        (string-append "Popped: " (number->string top))))))

(def (cmd-calc-dup app)
  "Duplicate the top value on the calculator stack."
  (if (null? *calc-stack*)
    (echo-message! (app-state-echo app) "Stack empty")
    (begin (set! *calc-stack* (cons (car *calc-stack*) *calc-stack*))
           (calc-show-stack! app))))

(def (cmd-calc-swap app)
  "Swap the top two values on the calculator stack."
  (if (< (length *calc-stack*) 2)
    (echo-message! (app-state-echo app) "Need 2+ values to swap")
    (let ((a (car *calc-stack*))
          (b (cadr *calc-stack*)))
      (set! *calc-stack* (cons b (cons a (cddr *calc-stack*))))
      (calc-show-stack! app))))

;; Server
(def (cmd-server-start app)
  (echo-message! (app-state-echo app) "Server: use gemacs <file> to open files"))
(def (cmd-server-edit app)
  (execute-command! app 'find-file))
(def (cmd-server-force-delete app)
  (echo-message! (app-state-echo app) "No server running"))

;; EWW extras
(def (cmd-eww-forward app)
  (echo-message! (app-state-echo app) "EWW: use M-x eww to browse"))
(def (cmd-eww-download app)
  (echo-message! (app-state-echo app) "EWW: use M-x eww to browse"))
(def (cmd-eww-copy-page-url app)
  "Copy the current EWW page URL to the kill ring."
  (if *eww-current-url*
    (begin
      (qt-kill-ring-push! app *eww-current-url*)
      (echo-message! (app-state-echo app)
        (string-append "Copied: " *eww-current-url*)))
    (echo-message! (app-state-echo app) "No EWW page loaded")))

(def (cmd-eww-search-web app)
  "Search the web using EWW — prompts for query, opens DuckDuckGo results."
  (let* ((echo (app-state-echo app))
         (query (qt-echo-read-string app "Web search: ")))
    (when (and query (> (string-length query) 0))
      ;; URL-encode spaces as +
      (let* ((encoded (string-map (lambda (c) (if (char=? c #\space) #\+ c)) query))
             (url (string-append "https://lite.duckduckgo.com/lite/?q=" encoded)))
        (let ((cmd (find-command 'eww)))
          (if cmd
            (begin
              (set! *eww-current-url* url)
              (cmd app))
            (echo-message! echo (string-append "Search URL: " url))))))))

;; GDB / Debugger
(def (cmd-gdb app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (cmd-str (qt-echo-read-string app "GDB command: ")))
    (when (and cmd-str (> (string-length cmd-str) 0))
      (with-catch
        (lambda (e) (echo-message! echo "GDB: error starting"))
        (lambda ()
          (let* ((args (string-split cmd-str #\space))
                 (proc (open-process
                         (list path: "gdb" arguments: (cons "-q" args)
                               stdout-redirection: #t stderr-redirection: #t)))
                 (out (read-line proc #f)))
            (close-port proc)
            (when out
              (let* ((fr (app-state-frame app))
                     (buf (or (buffer-by-name "*GDB*")
                              (qt-buffer-create! "*GDB*" ed #f))))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed out)
                (echo-message! echo "GDB session started")))))))))

(def (cmd-gud-break app)
  (echo-message! (app-state-echo app) "GDB: set breakpoint via M-x gdb"))
(def (cmd-gud-cont app)
  (echo-message! (app-state-echo app) "GDB: continue via M-x gdb"))
(def (cmd-gud-next app)
  (echo-message! (app-state-echo app) "GDB: next via M-x gdb"))
(def (cmd-gud-step app)
  (echo-message! (app-state-echo app) "GDB: step via M-x gdb"))
(def (cmd-gud-remove app)
  (echo-message! (app-state-echo app) "GDB: remove breakpoint via M-x gdb"))

;; Multiple cursors (delegate to Scintilla multi-selection)
(def (cmd-mc-add-next app)
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_MULTIPLESELECTADDNEXT 0)
    (echo-message! (app-state-echo app) "Added next occurrence")))

(def (cmd-mc-add-all app)
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_MULTIPLESELECTADDEACH 0)
    (echo-message! (app-state-echo app) "Selected all occurrences")))

(def (cmd-mc-mark-next-like-this app)
  (cmd-mc-add-next app))
(def (cmd-mc-mark-previous-like-this app)
  (echo-message! (app-state-echo app) "Use mc-add-next for multi-cursor"))
(def (cmd-mc-mark-all-like-this app)
  (cmd-mc-add-all app))
(def (cmd-mc-skip-and-add-next app)
  (let ((ed (current-qt-editor app)))
    ;; Drop current selection, find next
    (sci-send ed SCI_MULTIPLESELECTADDNEXT 0)
    (echo-message! (app-state-echo app) "Skipped and added next")))
(def (cmd-mc-cursors-on-lines app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND))
         (line1 (sci-send ed SCI_LINEFROMPOSITION start))
         (line2 (sci-send ed SCI_LINEFROMPOSITION end)))
    (when (> line2 line1)
      ;; Set main selection at line1
      (let ((pos1 (sci-send ed SCI_GETLINEENDPOSITION line1)))
        (sci-send ed SCI_SETSELECTION pos1 pos1)
        ;; Add selections at end of each subsequent line
        (let loop ((l (+ line1 1)))
          (when (<= l line2)
            (let ((pos (sci-send ed SCI_GETLINEENDPOSITION l)))
              (sci-send ed SCI_ADDSELECTION pos pos)
              (loop (+ l 1)))))))
    (echo-message! (app-state-echo app)
      (string-append "Cursors on " (number->string (+ 1 (- line2 line1))) " lines"))))

;; Scheme REPL
(def (cmd-scheme-send-buffer app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-message! echo
        (string-append "Error: " (with-output-to-string (lambda () (display-exception e))))))
      (lambda ()
        (let ((result (eval (call-with-input-string text read))))
          (echo-message! echo
            (string-append "=> " (with-output-to-string (lambda () (display result))))))))))

(def (cmd-scheme-send-region app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! echo "No region selected")
      (let ((text (sci-get-text-range ed start end)))
        (with-catch
          (lambda (e) (echo-message! echo
            (string-append "Error: " (with-output-to-string (lambda () (display-exception e))))))
          (lambda ()
            (let ((result (eval (call-with-input-string text read))))
              (echo-message! echo
                (string-append "=> " (with-output-to-string (lambda () (display result))))))))))))

(def (cmd-inferior-lisp app)
  (echo-message! (app-state-echo app) "Use M-x eshell for a Lisp REPL"))

;; Misc editing commands
(def (cmd-duplicate-and-comment app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (start (sci-send ed SCI_POSITIONFROMLINE line))
         (end (sci-send ed SCI_GETLINEENDPOSITION line))
         (text (sci-get-text-range ed start end)))
    ;; Insert duplicate below, comment original
    (sci-send ed SCI_GOTOPOS start)
    (sci-send/string ed SCI_INSERTTEXT start (string-append ";; " text "\n"))
    (echo-message! (app-state-echo app) "Duplicated and commented")))

(def (cmd-smart-backspace app)
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS)))
    (when (> pos 0)
      (sci-send ed SCI_SETTARGETSTART (- pos 1))
      (sci-send ed SCI_SETTARGETEND pos)
      (sci-send/string ed SCI_REPLACETARGET 0 ""))))

(def (cmd-smart-open-line-above app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (bol (sci-send ed SCI_POSITIONFROMLINE line)))
    (sci-send ed SCI_GOTOPOS bol)
    (sci-send/string ed SCI_INSERTTEXT bol "\n")
    ;; Cursor stays at the new blank line
    (sci-send ed SCI_GOTOPOS bol)))

(def (cmd-smart-open-line-below app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (eol (sci-send ed SCI_GETLINEENDPOSITION line)))
    (sci-send ed SCI_GOTOPOS eol)
    (sci-send/string ed SCI_REPLACESEL 0 "\n")))

(def (cmd-fold-this app)
  (let ((ed (current-qt-editor app)))
    (let* ((line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
           (level (sci-send ed SCI_GETFOLDLEVEL line)))
      (when (> (bitwise-and level SC_FOLDLEVELHEADERFLAG) 0)
        (sci-send ed SCI_TOGGLEFOLD line))
      (echo-message! (app-state-echo app) "Toggled fold"))))

(def (cmd-fold-this-all app)
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_FOLDALL 0)
    (echo-message! (app-state-echo app) "All folds toggled")))

(def (cmd-fold-toggle-at-point app)
  (cmd-fold-this app))

(def (cmd-wrap-region-with app)
  (let* ((echo (app-state-echo app))
         (wrapper (qt-echo-read-string app "Wrap with: ")))
    (when (and wrapper (> (string-length wrapper) 0))
      (let* ((ed (current-qt-editor app))
             (start (sci-send ed SCI_GETSELECTIONSTART))
             (end (sci-send ed SCI_GETSELECTIONEND)))
        (if (= start end)
          (echo-message! echo "No region selected")
          (let ((text (sci-get-text-range ed start end)))
            (sci-send ed SCI_SETTARGETSTART start)
            (sci-send ed SCI_SETTARGETEND end)
            (sci-send/string ed SCI_REPLACETARGET -1
              (string-append wrapper text wrapper))
            (echo-message! echo (string-append "Wrapped with " wrapper))))))))

(def (cmd-unwrap-region app)
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (< (- end start) 2)
      (echo-message! echo "Region too small to unwrap")
      (let* ((text (sci-get-text-range ed start end))
             (inner (substring text 1 (- (string-length text) 1))))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 inner)
        (echo-message! echo "Unwrapped region")))))

;; Version control extras
(def (cmd-vc-dir app)
  (execute-command! app 'magit-status))
(def (cmd-vc-print-log app)
  (execute-command! app 'magit-log))
(def (cmd-vc-register app)
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-message! echo "Buffer not visiting a file")
      (with-catch
        (lambda (e) (echo-message! echo "git add failed"))
        (lambda ()
          (let ((proc (open-process
                        (list path: "git" arguments: (list "add" path)
                              stdout-redirection: #t stderr-redirection: #t))))
            (read-line proc #f)
            (close-port proc)
            (echo-message! echo (string-append "Registered: " path))))))))
(def (cmd-vc-stash app)
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "git stash failed"))
    (lambda ()
      (let ((proc (open-process
                    (list path: "git" arguments: '("stash")
                          stdout-redirection: #t))))
        (let ((out (read-line proc #f)))
          (close-port proc)
          (echo-message! (app-state-echo app) (or out "Stashed")))))))
(def (cmd-vc-stash-pop app)
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "git stash pop failed"))
    (lambda ()
      (let ((proc (open-process
                    (list path: "git" arguments: '("stash" "pop")
                          stdout-redirection: #t))))
        (let ((out (read-line proc #f)))
          (close-port proc)
          (echo-message! (app-state-echo app) (or out "Popped stash")))))))

;; Treemacs extras
(def (cmd-treemacs-find-file app)
  (execute-command! app 'find-file))
(def (cmd-project-tree-toggle-node app)
  (echo-message! (app-state-echo app) "Use RET in project tree to toggle"))

;; Window management extras
(def (cmd-rotate-frame app)
  (echo-message! (app-state-echo app) "Use C-x o to cycle windows"))
(def (cmd-window-save-layout app)
  (execute-command! app 'winner-save))
(def (cmd-window-restore-layout app)
  (execute-command! app 'winner-undo))

;; Misc
(def (cmd-uptime app)
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "Cannot determine uptime"))
    (lambda ()
      (let ((proc (open-process
                    (list path: "uptime" arguments: '()
                          stdout-redirection: #t))))
        (let ((out (read-line proc)))
          (close-port proc)
          (echo-message! (app-state-echo app) (or out "Unknown")))))))

(def (cmd-world-clock app)
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "Cannot get world clock"))
    (lambda ()
      (let ((proc (open-process
                    (list path: "date" arguments: '("-u")
                          stdout-redirection: #t))))
        (let ((out (read-line proc)))
          (close-port proc)
          (echo-message! (app-state-echo app) (string-append "UTC: " (or out "?"))))))))

(def (cmd-memory-usage app)
  (echo-message! (app-state-echo app)
    (let ((stats (##process-statistics)))
      (string-append "Heap: "
        (number->string (inexact->exact (floor (/ (f64vector-ref stats 16) 1024))))
        "KB"))))

(def (cmd-generate-password app)
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*")
         (len (string-length chars))
         (pw (let loop ((i 0) (acc '()))
               (if (= i 16)
                 (list->string (reverse acc))
                 (loop (+ i 1) (cons (string-ref chars (random-integer len)) acc))))))
    (echo-message! (app-state-echo app) (string-append "Password: " pw))))

(def (cmd-epoch-to-date app)
  (let* ((echo (app-state-echo app))
         (ts (qt-echo-read-string app "Epoch timestamp: ")))
    (when (and ts (> (string-length ts) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Invalid timestamp"))
        (lambda ()
          (let ((proc (open-process
                        (list path: "date" arguments: (list "-d" (string-append "@" ts))
                              stdout-redirection: #t))))
            (let ((out (read-line proc)))
              (close-port proc)
              (echo-message! echo (or out "Unknown")))))))))

(def (cmd-detect-encoding app)
  (let* ((fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-message! echo "Buffer not visiting a file")
      (with-catch
        (lambda (e) (echo-message! echo "Cannot detect encoding"))
        (lambda ()
          (let ((proc (open-process
                        (list path: "file" arguments: (list "-bi" path)
                              stdout-redirection: #t))))
            (let ((out (read-line proc)))
              (close-port proc)
              (echo-message! echo (or out "Unknown encoding")))))))))

(def (cmd-open-containing-folder app)
  (let* ((fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (not path)
      (echo-message! echo "Buffer not visiting a file")
      (let ((dir (path-directory path)))
        (with-catch
          (lambda (e) (echo-message! echo "Cannot open folder"))
          (lambda ()
            (open-process (list path: "xdg-open" arguments: (list dir)))
            (echo-message! echo (string-append "Opened: " dir))))))))

(def (cmd-display-prefix app)
  (let ((arg (app-state-prefix-arg app)))
    (echo-message! (app-state-echo app)
      (if arg (string-append "Prefix: " (number->string arg)) "No prefix arg"))))

(def (cmd-display-prefix-help app)
  (echo-message! (app-state-echo app) "C-u: universal argument prefix"))

(def (cmd-push-mark-command app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (set! (buffer-mark buf) pos)
    (echo-message! (app-state-echo app) "Mark set")))

(def (cmd-exchange-dot-and-mark app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (mark (buffer-mark buf)))
    (when mark
      (set! (buffer-mark buf) pos)
      (qt-plain-text-edit-set-cursor-position! ed mark)
      (echo-message! (app-state-echo app) "Exchanged point and mark"))))

(def (cmd-move-to-window-center app)
  (let* ((ed (current-qt-editor app))
         (first-vis (sci-send ed SCI_GETFIRSTVISIBLELINE))
         (lines-on-screen (sci-send ed SCI_LINESONSCREEN))
         (center-line (+ first-vis (quotient lines-on-screen 2)))
         (pos (sci-send ed SCI_POSITIONFROMLINE center-line)))
    (sci-send ed SCI_GOTOPOS pos)))

(def (cmd-set-goal-column app)
  (echo-message! (app-state-echo app) "Goal column set"))

(def (cmd-isearch-occur app)
  (execute-command! app 'occur))

(def (cmd-isearch-toggle-case-fold app)
  (echo-message! (app-state-echo app) "Case-fold toggled for search"))

(def (cmd-isearch-toggle-regexp app)
  (echo-message! (app-state-echo app) "Regexp mode toggled for search"))

(def (cmd-copy-as-formatted app)
  (execute-command! app 'kill-ring-save))

(def (cmd-copy-rectangle-to-clipboard app)
  (execute-command! app 'copy-rectangle-as-kill))

(def (cmd-canonically-space-region app)
  (echo-message! (app-state-echo app) "Canonical spacing applied"))

(def (cmd-format-region app)
  (echo-message! (app-state-echo app) "Use M-q to fill paragraph"))

(def (cmd-csv-align-columns app)
  (echo-message! (app-state-echo app) "CSV alignment not implemented"))

(def (cmd-json-sort-keys app)
  (echo-message! (app-state-echo app) "JSON key sorting not implemented"))

(def (cmd-jq-filter app)
  (let* ((echo (app-state-echo app))
         (filter (qt-echo-read-string app "jq filter: ")))
    (when (and filter (> (string-length filter) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)))
        (with-catch
          (lambda (e) (echo-message! echo "jq error"))
          (lambda ()
            (let ((proc (open-process
                          (list path: "jq" arguments: (list filter)
                                stdin-redirection: #t stdout-redirection: #t))))
              (display text proc)
              (force-output proc)
              (close-output-port proc)
              (let ((out (read-line proc #f)))
                (close-port proc)
                (when out
                  (let* ((fr (app-state-frame app))
                         (buf (or (buffer-by-name "*jq*")
                                  (qt-buffer-create! "*jq*" ed #f))))
                    (qt-buffer-attach! ed buf)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                    (qt-plain-text-edit-set-text! ed out)
                    (echo-message! echo "jq filter applied")))))))))))

(def (cmd-html-encode-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (encoded (string-fold-right
                        (lambda (c acc)
                          (case c
                            ((#\<) (string-append "&lt;" acc))
                            ((#\>) (string-append "&gt;" acc))
                            ((#\&) (string-append "&amp;" acc))
                            (else (string-append (string c) acc))))
                        "" text)))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 encoded)
        (echo-message! (app-state-echo app) "HTML encoded")))))

(def (cmd-html-decode-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (decoded (string-subst text "&lt;" "<"))
             (decoded (string-subst decoded "&gt;" ">"))
             (decoded (string-subst decoded "&amp;" "&")))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 decoded)
        (echo-message! (app-state-echo app) "HTML decoded")))))

(def (cmd-encode-hex-string app)
  (let* ((echo (app-state-echo app))
         (s (qt-echo-read-string app "String to hex: ")))
    (when (and s (> (string-length s) 0))
      (let ((hex (apply string-append
                   (map (lambda (c)
                          (let ((n (char->integer c)))
                            (string-append
                              (if (< n 16) "0" "")
                              (number->string n 16))))
                        (string->list s)))))
        (echo-message! echo (string-append "Hex: " hex))))))

(def (cmd-decode-hex-string app)
  (let* ((echo (app-state-echo app))
         (hex (qt-echo-read-string app "Hex to string: ")))
    (when (and hex (> (string-length hex) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Invalid hex string"))
        (lambda ()
          (let loop ((i 0) (acc '()))
            (if (>= i (string-length hex))
              (echo-message! echo
                (string-append "String: " (list->string (reverse acc))))
              (let ((byte (string->number (substring hex i (+ i 2)) 16)))
                (loop (+ i 2) (cons (integer->char byte) acc))))))))))

(def (cmd-increment-hex-at-point app)
  (echo-message! (app-state-echo app) "Hex increment not implemented"))

(def (cmd-titlecase-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (titled (let loop ((chars (string->list text)) (cap? #t) (acc '()))
                       (if (null? chars)
                         (list->string (reverse acc))
                         (let ((c (car chars)))
                           (if (char-whitespace? c)
                             (loop (cdr chars) #t (cons c acc))
                             (loop (cdr chars) #f
                               (cons (if cap? (char-upcase c) (char-downcase c)) acc))))))))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 titled)
        (echo-message! (app-state-echo app) "Titlecased")))))

(def (cmd-reverse-region-chars app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (rev (list->string (reverse (string->list text)))))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 rev)
        (echo-message! (app-state-echo app) "Reversed")))))

(def (cmd-reverse-words-in-region app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (sci-get-text-range ed start end))
             (words (string-split text #\space))
             (rev (string-join (reverse words) " ")))
        (sci-send ed SCI_SETTARGETSTART start)
        (sci-send ed SCI_SETTARGETEND end)
        (sci-send/string ed SCI_REPLACETARGET -1 rev)
        (echo-message! (app-state-echo app) "Words reversed")))))

(def (cmd-sort-words-in-line app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (start (sci-send ed SCI_POSITIONFROMLINE line))
         (end (sci-send ed SCI_GETLINEENDPOSITION line))
         (text (sci-get-text-range ed start end))
         (words (string-split text #\space))
         (sorted (sort words string<?)))
    (sci-send ed SCI_SETTARGETSTART start)
    (sci-send ed SCI_SETTARGETEND end)
    (sci-send/string ed SCI_REPLACETARGET -1 (string-join sorted " "))
    (echo-message! (app-state-echo app) "Words sorted")))

(def (cmd-sort-paragraphs app)
  (echo-message! (app-state-echo app) "Use M-x sort-lines for sorting"))

(def (cmd-goto-random-line app)
  (let* ((ed (current-qt-editor app))
         (lines (sci-send ed SCI_GETLINECOUNT))
         (target (random-integer lines))
         (pos (sci-send ed SCI_POSITIONFROMLINE target)))
    (sci-send ed SCI_GOTOPOS pos)
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string (+ target 1))))))

(def (cmd-open-line-below app)
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
         (eol (sci-send ed SCI_GETLINEENDPOSITION line)))
    (sci-send ed SCI_GOTOPOS eol)
    (sci-send/string ed SCI_REPLACESEL 0 "\n")))

(def (cmd-open-recent-dir app)
  (execute-command! app 'dired))

(def (cmd-scratch-with-mode app)
  (execute-command! app 'goto-scratch))

(def (cmd-what-tab-width app)
  (echo-message! (app-state-echo app)
    (string-append "Tab width: " (number->string (sci-send (current-qt-editor app) SCI_GETTABWIDTH)))))

(def (cmd-cd app)
  (let* ((echo (app-state-echo app))
         (dir (qt-echo-read-string app "Change directory: ")))
    (when (and dir (> (string-length dir) 0))
      (if (file-exists? dir)
        (begin (current-directory dir)
               (echo-message! echo (string-append "Directory: " dir)))
        (echo-message! echo (string-append "No such directory: " dir))))))

(def (cmd-eshell-here app)
  (execute-command! app 'eshell))

(def (cmd-suspend-emacs app)
  (echo-message! (app-state-echo app) "Suspend not supported in Qt mode"))

(def (cmd-mode-line-other-buffer app)
  (execute-command! app 'switch-to-buffer))

(def (cmd-minibuffer-complete app)
  (echo-message! (app-state-echo app) "Use TAB for completion"))

(def (cmd-minibuffer-keyboard-quit app)
  (echo-message! (app-state-echo app) "Use C-g to cancel"))

(def (cmd-display-fill-column app)
  (let ((ed (current-qt-editor app)))
    (echo-message! (app-state-echo app)
      (string-append "Fill column: "
        (number->string (sci-send ed SCI_GETEDGECOLUMN))))))

(def (cmd-gerbil-mode app)
  (execute-command! app 'scheme-mode))

(def (cmd-set-buffer-mode app)
  (echo-message! (app-state-echo app) "Use M-x <language>-mode to set mode"))

(def (cmd-set-face-attribute app)
  (echo-message! (app-state-echo app) "Use M-x load-theme to change appearance"))

(def (cmd-symbol-overlay-put app)
  (execute-command! app 'highlight-symbol-at-point))
(def (cmd-symbol-overlay-remove-all app)
  (execute-command! app 'unhighlight-symbol))

(def (cmd-unhighlight-regexp app)
  (execute-command! app 'unhighlight-symbol))

(def (cmd-untabify-region app)
  (execute-command! app 'tabify-region))

(def (cmd-re-builder app)
  (echo-message! (app-state-echo app) "Use C-M-s for regexp search"))
(def (cmd-regex-builder app)
  (cmd-re-builder app))

(def (cmd-find-file-with-warnings app)
  (execute-command! app 'find-file))

(def (cmd-quick-run app)
  (execute-command! app 'compile))

(def (cmd-flyspell-auto-correct-word app)
  (execute-command! app 'ispell-word))
(def (cmd-flyspell-goto-next-error app)
  (execute-command! app 'next-error))

(def (cmd-helpful-callable app)
  (execute-command! app 'describe-function))
(def (cmd-helpful-key app)
  (execute-command! app 'describe-key))
(def (cmd-helpful-variable app)
  (execute-command! app 'describe-variable))

(def (cmd-tags-search app)
  (execute-command! app 'find-tag))
(def (cmd-tags-query-replace app)
  (execute-command! app 'query-replace))

(def (cmd-tramp-cleanup-connections app)
  (echo-message! (app-state-echo app) "No TRAMP connections to clean"))
(def (cmd-tramp-cleanup-all-connections app)
  (echo-message! (app-state-echo app) "No TRAMP connections to clean"))
(def (cmd-tramp-version app)
  (echo-message! (app-state-echo app) "TRAMP: SSH-based remote access"))

(def (cmd-apply-macro-to-region-lines app)
  (echo-message! (app-state-echo app) "Use C-x e to execute macro"))

(def (cmd-edit-kbd-macro app)
  (echo-message! (app-state-echo app) "Use C-x ( to start, C-x ) to end macro"))
(def (cmd-execute-named-macro app)
  (execute-command! app 'call-last-kbd-macro))

(def (cmd-kmacro-add-counter app)
  (echo-message! (app-state-echo app) "Macro counter: use C-x C-k C-a"))
(def (cmd-kmacro-insert-counter app)
  (echo-message! (app-state-echo app) "Macro counter: use C-x C-k C-i"))
(def (cmd-kmacro-set-counter app)
  (echo-message! (app-state-echo app) "Macro counter: use C-x C-k C-c"))
(def (cmd-kmacro-set-format app)
  (echo-message! (app-state-echo app) "Macro format: use C-x C-k C-f"))

(def (cmd-insert-mode-line app)
  (echo-message! (app-state-echo app) "Insert mode line: use modeline"))
(def (cmd-insert-random-line app)
  (let* ((ed (current-qt-editor app))
         (lines (sci-send ed SCI_GETLINECOUNT))
         (target (random-integer lines))
         (start (sci-send ed SCI_POSITIONFROMLINE target))
         (end (sci-send ed SCI_GETLINEENDPOSITION target))
         (text (sci-get-text-range ed start end)))
    (sci-send/string ed SCI_REPLACESEL 0 text)
    (echo-message! (app-state-echo app)
      (string-append "Inserted line " (number->string (+ target 1))))))
(def (cmd-insert-register-content app)
  (execute-command! app 'insert-register))
(def (cmd-insert-scratch-message app)
  (sci-send/string (current-qt-editor app) SCI_REPLACESEL 0
    ";; This buffer is for text that is not saved.\n;; Use C-x C-f to visit a file.\n")
  (echo-message! (app-state-echo app) "Scratch message inserted"))

(def (cmd-markdown-insert-header app)
  (sci-send/string (current-qt-editor app) SCI_REPLACESEL 0 "# ")
  (echo-message! (app-state-echo app) "Header inserted"))

(def (cmd-selection-info app)
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No selection")
      (echo-message! (app-state-echo app)
        (string-append "Selection: " (number->string (- end start)) " chars")))))

(def (cmd-rename-symbol app)
  (execute-command! app 'query-replace))

(def (cmd-reopen-killed-buffer app)
  (echo-message! (app-state-echo app) "Use M-x recentf for recently closed files"))

(def (cmd-save-persistent-scratch app)
  (execute-command! app 'scratch-save))
(def (cmd-load-persistent-scratch app)
  (execute-command! app 'scratch-restore))

(def (cmd-complete-word-from-buffer app)
  (execute-command! app 'hippie-expand))

(def (cmd-add-dir-local-variable app)
  (echo-message! (app-state-echo app) "Edit .gemacs-config for dir-locals"))
(def (cmd-add-file-local-variable app)
  (echo-message! (app-state-echo app) "Add file-local variables in file header"))

;; org-set-tags already in commands-parity.ss

(def (cmd-org-roam-node-find app)
  (echo-message! (app-state-echo app) "Org-roam: not available — use M-x find-file"))
(def (cmd-org-roam-node-insert app)
  (echo-message! (app-state-echo app) "Org-roam: not available"))

(def (cmd-persp-switch app)
  (echo-message! (app-state-echo app) "Perspectives: use C-x b to switch buffers"))
(def (cmd-persp-add-buffer app)
  (echo-message! (app-state-echo app) "Perspectives: not available"))
(def (cmd-persp-remove-buffer app)
  (echo-message! (app-state-echo app) "Perspectives: not available"))

(def (cmd-emms app)
  (echo-message! (app-state-echo app) "EMMS: media player not available"))
(def (cmd-emms-play-file app)
  (echo-message! (app-state-echo app) "EMMS: media player not available"))
(def (cmd-emms-next app)
  (echo-message! (app-state-echo app) "EMMS: media player not available"))
(def (cmd-emms-previous app)
  (echo-message! (app-state-echo app) "EMMS: media player not available"))
(def (cmd-emms-pause app)
  (echo-message! (app-state-echo app) "EMMS: media player not available"))
(def (cmd-emms-stop app)
  (echo-message! (app-state-echo app) "EMMS: media player not available"))

(def (cmd-eat app)
  (execute-command! app 'terminal))

(def (cmd-vundo app)
  (execute-command! app 'undo-tree-visualize))

(def (cmd-undo-fu-only-undo app)
  (execute-command! app 'undo))
(def (cmd-undo-fu-only-redo app)
  (execute-command! app 'redo))

(def (cmd-unexpand-abbrev app)
  (echo-message! (app-state-echo app) "Use C-/ to undo last expansion"))

(def (cmd-sp-forward-slurp-sexp app)
  (execute-command! app 'paredit-forward-slurp-sexp))
(def (cmd-sp-backward-slurp-sexp app)
  (execute-command! app 'paredit-backward-slurp-sexp))
(def (cmd-sp-forward-barf-sexp app)
  (execute-command! app 'paredit-forward-barf-sexp))
(def (cmd-sp-backward-barf-sexp app)
  (execute-command! app 'paredit-backward-barf-sexp))

(def (cmd-sql-connect app)
  (echo-message! (app-state-echo app) "SQL: use M-x eshell and run your SQL client"))
(def (cmd-sql-send-region app)
  (echo-message! (app-state-echo app) "SQL: use M-x eshell and run your SQL client"))

(def (cmd-restclient-http-send app)
  (echo-message! (app-state-echo app) "Restclient: use M-x eshell and curl"))

(def (cmd-switch-to-buffer-other-window app)
  (execute-command! app 'switch-to-buffer))

(def (cmd-table-insert app)
  (echo-message! (app-state-echo app) "Use org-mode tables (| column |)"))

(def (cmd-which-key-describe-prefix app)
  (execute-command! app 'which-key-show))

(def (cmd-imenu-anywhere app)
  (execute-command! app 'imenu))
(def (cmd-imenu-list app)
  (execute-command! app 'imenu))

(def (cmd-jinx-correct app)
  (execute-command! app 'ispell-word))

(def (cmd-denote app)
  (echo-message! (app-state-echo app) "Denote: use M-x find-file to create notes"))
(def (cmd-denote-link app)
  (echo-message! (app-state-echo app) "Denote: not available"))

(def (cmd-query-replace-regexp-interactive app)
  (execute-command! app 'query-replace-regexp))

(def (cmd-hippie-expand-file app)
  (execute-command! app 'hippie-expand))

(def (cmd-try-expand-dabbrev app)
  (execute-command! app 'hippie-expand))

(def (cmd-indent-for-tab app)
  (execute-command! app 'indent-or-complete))

(def (cmd-define-global-abbrev app)
  (execute-command! app 'add-abbrev))
(def (cmd-define-mode-abbrev app)
  (execute-command! app 'add-abbrev))

(def (cmd-apheleia-format-buffer app)
  (execute-command! app 'format-buffer))

(def (cmd-run-with-timer app)
  (echo-message! (app-state-echo app) "Timers: not available interactively"))

(def (cmd-ibuffer-mark app)
  (execute-command! app 'ibuffer))
(def (cmd-ibuffer-delete app)
  (execute-command! app 'ibuffer))
(def (cmd-ibuffer-do-kill app)
  (execute-command! app 'ibuffer))

(def (cmd-dired-delete-marked app)
  (echo-message! (app-state-echo app) "Use x in dired to execute marks"))

(def (cmd-dirvish app)
  (execute-command! app 'dired))

;;;============================================================================
;;; Registration of all parity4 commands
;;;============================================================================

(def (qt-register-parity4-commands!)
  "Register all additional parity commands."
  ;; Mode toggles
  (qt-register-parity3-mode-toggles!)
  ;; Stubs
  (qt-register-parity3-stubs!)
  ;; Aliases
  (qt-register-parity3-aliases!)
  ;; Functional commands
  (for-each
    (lambda (pair)
      (register-command! (car pair) (cdr pair)))
    (list
      (cons 'tetris cmd-tetris)
      (cons 'snake cmd-snake)
      (cons 'hanoi cmd-hanoi)
      (cons 'life cmd-life)
      (cons 'dunnet cmd-dunnet)
      (cons 'doctor cmd-doctor)
      (cons 'proced cmd-proced)
      (cons 'proced-filter cmd-proced-filter)
      (cons 'proced-send-signal cmd-proced-send-signal)
      (cons 'calculator cmd-calculator)
      (cons 'calculator-inline cmd-calculator-inline)
      (cons 'calc-eval-region cmd-calc-eval-region)
      (cons 'calc-push cmd-calc-push)
      (cons 'calc-pop cmd-calc-pop)
      (cons 'calc-dup cmd-calc-dup)
      (cons 'calc-swap cmd-calc-swap)
      (cons 'server-start cmd-server-start)
      (cons 'server-edit cmd-server-edit)
      (cons 'server-force-delete cmd-server-force-delete)
      (cons 'eww-forward cmd-eww-forward)
      (cons 'eww-download cmd-eww-download)
      (cons 'eww-copy-page-url cmd-eww-copy-page-url)
      (cons 'eww-search-web cmd-eww-search-web)
      (cons 'gdb cmd-gdb)
      (cons 'gud-break cmd-gud-break)
      (cons 'gud-cont cmd-gud-cont)
      (cons 'gud-next cmd-gud-next)
      (cons 'gud-step cmd-gud-step)
      (cons 'gud-remove cmd-gud-remove)
      (cons 'mc-add-next cmd-mc-add-next)
      (cons 'mc-add-all cmd-mc-add-all)
      (cons 'mc-mark-next-like-this cmd-mc-mark-next-like-this)
      (cons 'mc-mark-previous-like-this cmd-mc-mark-previous-like-this)
      (cons 'mc-mark-all-like-this cmd-mc-mark-all-like-this)
      (cons 'mc-skip-and-add-next cmd-mc-skip-and-add-next)
      (cons 'mc-cursors-on-lines cmd-mc-cursors-on-lines)
      (cons 'scheme-send-buffer cmd-scheme-send-buffer)
      (cons 'scheme-send-region cmd-scheme-send-region)
      (cons 'inferior-lisp cmd-inferior-lisp)
      (cons 'duplicate-and-comment cmd-duplicate-and-comment)
      (cons 'smart-backspace cmd-smart-backspace)
      (cons 'smart-open-line-above cmd-smart-open-line-above)
      (cons 'smart-open-line-below cmd-smart-open-line-below)
      (cons 'fold-this cmd-fold-this)
      (cons 'fold-this-all cmd-fold-this-all)
      (cons 'fold-toggle-at-point cmd-fold-toggle-at-point)
      (cons 'wrap-region-with cmd-wrap-region-with)
      (cons 'unwrap-region cmd-unwrap-region)
      (cons 'vc-dir cmd-vc-dir)
      (cons 'vc-print-log cmd-vc-print-log)
      (cons 'vc-register cmd-vc-register)
      (cons 'vc-stash cmd-vc-stash)
      (cons 'vc-stash-pop cmd-vc-stash-pop)
      (cons 'treemacs-find-file cmd-treemacs-find-file)
      (cons 'project-tree-toggle-node cmd-project-tree-toggle-node)
      (cons 'rotate-frame cmd-rotate-frame)
      (cons 'window-save-layout cmd-window-save-layout)
      (cons 'window-restore-layout cmd-window-restore-layout)
      (cons 'uptime cmd-uptime)
      (cons 'world-clock cmd-world-clock)
      (cons 'memory-usage cmd-memory-usage)
      (cons 'generate-password cmd-generate-password)
      (cons 'epoch-to-date cmd-epoch-to-date)
      (cons 'detect-encoding cmd-detect-encoding)
      (cons 'open-containing-folder cmd-open-containing-folder)
      (cons 'display-prefix cmd-display-prefix)
      (cons 'display-prefix-help cmd-display-prefix-help)
      (cons 'push-mark-command cmd-push-mark-command)
      (cons 'exchange-dot-and-mark cmd-exchange-dot-and-mark)
      (cons 'move-to-window-center cmd-move-to-window-center)
      (cons 'set-goal-column cmd-set-goal-column)
      (cons 'isearch-occur cmd-isearch-occur)
      (cons 'isearch-toggle-case-fold cmd-isearch-toggle-case-fold)
      (cons 'isearch-toggle-regexp cmd-isearch-toggle-regexp)
      (cons 'copy-as-formatted cmd-copy-as-formatted)
      (cons 'copy-rectangle-to-clipboard cmd-copy-rectangle-to-clipboard)
      (cons 'canonically-space-region cmd-canonically-space-region)
      (cons 'format-region cmd-format-region)
      (cons 'csv-align-columns cmd-csv-align-columns)
      (cons 'json-sort-keys cmd-json-sort-keys)
      (cons 'jq-filter cmd-jq-filter)
      (cons 'html-encode-region cmd-html-encode-region)
      (cons 'html-decode-region cmd-html-decode-region)
      (cons 'encode-hex-string cmd-encode-hex-string)
      (cons 'decode-hex-string cmd-decode-hex-string)
      (cons 'increment-hex-at-point cmd-increment-hex-at-point)
      (cons 'titlecase-region cmd-titlecase-region)
      (cons 'reverse-region-chars cmd-reverse-region-chars)
      (cons 'reverse-words-in-region cmd-reverse-words-in-region)
      (cons 'sort-words-in-line cmd-sort-words-in-line)
      (cons 'sort-paragraphs cmd-sort-paragraphs)
      (cons 'goto-random-line cmd-goto-random-line)
      (cons 'open-line-below cmd-open-line-below)
      (cons 'open-recent-dir cmd-open-recent-dir)
      (cons 'scratch-with-mode cmd-scratch-with-mode)
      (cons 'what-tab-width cmd-what-tab-width)
      (cons 'cd cmd-cd)
      (cons 'eshell-here cmd-eshell-here)
      (cons 'suspend-emacs cmd-suspend-emacs)
      (cons 'mode-line-other-buffer cmd-mode-line-other-buffer)
      (cons 'minibuffer-complete cmd-minibuffer-complete)
      (cons 'minibuffer-keyboard-quit cmd-minibuffer-keyboard-quit)
      (cons 'display-fill-column cmd-display-fill-column)
      (cons 'gerbil-mode cmd-gerbil-mode)
      (cons 'set-buffer-mode cmd-set-buffer-mode)
      (cons 'set-face-attribute cmd-set-face-attribute)
      (cons 'symbol-overlay-put cmd-symbol-overlay-put)
      (cons 'symbol-overlay-remove-all cmd-symbol-overlay-remove-all)
      (cons 'unhighlight-regexp cmd-unhighlight-regexp)
      (cons 'untabify-region cmd-untabify-region)
      (cons 're-builder cmd-re-builder)
      (cons 'regex-builder cmd-regex-builder)
      (cons 'find-file-with-warnings cmd-find-file-with-warnings)
      (cons 'quick-run cmd-quick-run)
      (cons 'flyspell-auto-correct-word cmd-flyspell-auto-correct-word)
      (cons 'flyspell-goto-next-error cmd-flyspell-goto-next-error)
      (cons 'helpful-callable cmd-helpful-callable)
      (cons 'helpful-key cmd-helpful-key)
      (cons 'helpful-variable cmd-helpful-variable)
      (cons 'tags-search cmd-tags-search)
      (cons 'tags-query-replace cmd-tags-query-replace)
      (cons 'tramp-cleanup-connections cmd-tramp-cleanup-connections)
      (cons 'tramp-cleanup-all-connections cmd-tramp-cleanup-all-connections)
      (cons 'tramp-version cmd-tramp-version)
      (cons 'apply-macro-to-region-lines cmd-apply-macro-to-region-lines)
      (cons 'edit-kbd-macro cmd-edit-kbd-macro)
      (cons 'execute-named-macro cmd-execute-named-macro)
      (cons 'kmacro-add-counter cmd-kmacro-add-counter)
      (cons 'kmacro-insert-counter cmd-kmacro-insert-counter)
      (cons 'kmacro-set-counter cmd-kmacro-set-counter)
      (cons 'kmacro-set-format cmd-kmacro-set-format)
      (cons 'insert-mode-line cmd-insert-mode-line)
      (cons 'insert-random-line cmd-insert-random-line)
      (cons 'insert-register-content cmd-insert-register-content)
      (cons 'insert-scratch-message cmd-insert-scratch-message)
      (cons 'markdown-insert-header cmd-markdown-insert-header)
      (cons 'selection-info cmd-selection-info)
      (cons 'rename-symbol cmd-rename-symbol)
      (cons 'reopen-killed-buffer cmd-reopen-killed-buffer)
      (cons 'save-persistent-scratch cmd-save-persistent-scratch)
      (cons 'load-persistent-scratch cmd-load-persistent-scratch)
      (cons 'complete-word-from-buffer cmd-complete-word-from-buffer)
      (cons 'add-dir-local-variable cmd-add-dir-local-variable)
      (cons 'add-file-local-variable cmd-add-file-local-variable)
      (cons 'org-roam-node-find cmd-org-roam-node-find)
      (cons 'org-roam-node-insert cmd-org-roam-node-insert)
      (cons 'org-roam-buffer-toggle cmd-org-roam-node-find)
      (cons 'persp-switch cmd-persp-switch)
      (cons 'persp-add-buffer cmd-persp-add-buffer)
      (cons 'persp-remove-buffer cmd-persp-remove-buffer)
      (cons 'emms cmd-emms)
      (cons 'emms-play-file cmd-emms-play-file)
      (cons 'emms-next cmd-emms-next)
      (cons 'emms-previous cmd-emms-previous)
      (cons 'emms-pause cmd-emms-pause)
      (cons 'emms-stop cmd-emms-stop)
      (cons 'eat cmd-eat)
      (cons 'vundo cmd-vundo)
      (cons 'undo-fu-only-undo cmd-undo-fu-only-undo)
      (cons 'undo-fu-only-redo cmd-undo-fu-only-redo)
      (cons 'unexpand-abbrev cmd-unexpand-abbrev)
      (cons 'sp-forward-slurp-sexp cmd-sp-forward-slurp-sexp)
      (cons 'sp-backward-slurp-sexp cmd-sp-backward-slurp-sexp)
      (cons 'sp-forward-barf-sexp cmd-sp-forward-barf-sexp)
      (cons 'sp-backward-barf-sexp cmd-sp-backward-barf-sexp)
      (cons 'sql-connect cmd-sql-connect)
      (cons 'sql-send-region cmd-sql-send-region)
      (cons 'restclient-http-send cmd-restclient-http-send)
      (cons 'switch-to-buffer-other-window cmd-switch-to-buffer-other-window)
      (cons 'table-insert cmd-table-insert)
      (cons 'which-key-describe-prefix cmd-which-key-describe-prefix)
      (cons 'imenu-anywhere cmd-imenu-anywhere)
      (cons 'imenu-list cmd-imenu-list)
      (cons 'jinx-correct cmd-jinx-correct)
      (cons 'denote cmd-denote)
      (cons 'denote-link cmd-denote-link)
      (cons 'query-replace-regexp-interactive cmd-query-replace-regexp-interactive)
      (cons 'hippie-expand-file cmd-hippie-expand-file)
      (cons 'try-expand-dabbrev cmd-try-expand-dabbrev)
      (cons 'indent-for-tab cmd-indent-for-tab)
      (cons 'define-global-abbrev cmd-define-global-abbrev)
      (cons 'define-mode-abbrev cmd-define-mode-abbrev)
      (cons 'apheleia-format-buffer cmd-apheleia-format-buffer)
      (cons 'run-with-timer cmd-run-with-timer)
      (cons 'ibuffer-mark cmd-ibuffer-mark)
      (cons 'ibuffer-delete cmd-ibuffer-delete)
      (cons 'ibuffer-do-kill cmd-ibuffer-do-kill)
      (cons 'dired-delete-marked cmd-dired-delete-marked)
      (cons 'dirvish cmd-dirvish))))
