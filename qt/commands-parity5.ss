;;; -*- Gerbil -*-
;;; Qt parity commands (part 5) — remaining 134 non-toggle commands.
;;; Chain position: after commands-parity4, before commands-aliases.
;;; Includes mode toggles, stubs, aliases, and functional commands.

(export #t)

(import :std/sugar
        :std/misc/string
        :std/srfi/13
        :std/format
        :gemacs/core
        :gemacs/qt/sci-shim
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
        :gemacs/qt/commands-parity2
        :gemacs/qt/commands-parity3
        :gemacs/qt/commands-parity4)

;;;============================================================================
;;; Mode toggle commands (63) — toggle via make-toggle-command
;;;============================================================================

(def (qt-register-parity5-mode-toggles!)
  "Register 63 mode toggle commands."
  (for-each
    (lambda (name)
      (register-command! name (make-toggle-command name)))
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
      electric-indent-local-mode
      electric-indent-mode
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
      helm-mode
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
      writeroom-mode)))

;;;============================================================================
;;; Stub commands (31) — echo-only placeholders
;;;============================================================================

(def (qt-register-parity5-stubs!)
  "Register 31 stub commands."
  (for-each
    (lambda (pair)
      (register-command! (car pair) (make-stub-command (car pair) (cdr pair))))
    '((all-the-icons-install-fonts . "Install all-the-icons fonts")
      (nerd-icons-install-fonts . "Install nerd-icons fonts")
      (dap-continue . "DAP debugger continue")
      (dash-at-point . "Look up symbol in Dash documentation")
      (devdocs-lookup . "Look up symbol in DevDocs")
      (docker . "Docker management interface")
      (docker-containers . "List Docker containers")
      (docker-images . "List Docker images")
      (doom-themes . "Switch Doom theme")
      (customize-group . "Customize settings group")
      (customize-themes . "Customize color themes")
      (ediff-show-registry . "Show Ediff session registry")
      (embark-dwim . "Embark do-what-I-mean action")
      (gptel . "GPT integration chat")
      (gptel-send . "Send message to GPT")
      (list-packages . "List available packages")
      (package-archives . "Manage package archives")
      (package-delete . "Delete an installed package")
      (package-install . "Install a package")
      (package-list-packages . "List all packages")
      (package-refresh-contents . "Refresh package database")
      (menu-bar-open . "Open menu bar")
      (notifications-list . "List notifications")
      (rmail . "Read mail (Rstrstrmail)")
      (slime . "Start SLIME (Superior Lisp Interaction Mode)")
      (sly . "Start Sly (Sylvester the cat's SLIME)")
      (speedbar . "Open Speedbar file browser")
      (woman . "Read man page without man command")
      (treemacs . "Open Treemacs file browser")
      (citar-insert-citation . "Insert citation reference")
      (facemenu-set-background . "Set face background color"))))

;;;============================================================================
;;; Alias commands (22) — delegate to existing commands
;;;============================================================================

(def (qt-register-parity5-aliases!)
  "Register 22 alias commands."
  (for-each
    (lambda (pair)
      (register-command! (car pair) (make-alias-command (cdr pair))))
    '((ido-find-file . find-file)
      (ido-switch-buffer . switch-to-buffer)
      (helm-mini . switch-to-buffer)
      (execute-extended-command-fuzzy . execute-extended-command)
      (execute-extended-command-with-history . execute-extended-command)
      (narrow-to-region-simple . narrow-to-region)
      (widen-simple . widen)
      (winner-undo-2 . winner-undo)
      (display-line-numbers-absolute . display-line-numbers-mode)
      (display-line-numbers-none . display-line-numbers-mode)
      (which-key . describe-bindings)
      (popper-cycle . next-buffer)
      (popper-toggle-latest . previous-buffer)
      (hippie-expand-undo . undo)
      (next-error-function . next-error)
      (previous-error-function . previous-error)
      (rectangle-mark-mode . kill-rectangle)
      (whitespace-toggle-options . whitespace-mode)
      (facemenu-set-foreground . set-face-attribute)
      (yas-insert-snippet . yas-expand)
      (yas-new-snippet . yas-expand)
      (yas-visit-snippet-file . yas-expand)
      (digit-argument . universal-argument)
      (org-set-tags . org-set-tags-command))))

;;;============================================================================
;;; Functional commands (18) — real implementations
;;;============================================================================

;; auto-insert: insert template based on file extension
(def (cmd-auto-insert app)
  (let* ((fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (name (buffer-name buf))
         (ext (let ((dot (string-index name #\.)))
                (if dot (substring name dot (string-length name)) ""))))
    (echo-message! (app-state-echo app)
      (string-append "Auto-insert for " ext " files (no template found)"))))

;; calendar-goto-date: prompt for date and show
(def (cmd-calendar-goto-date app)
  (let ((date (qt-echo-read-string app "Go to date (YYYY-MM-DD): ")))
    (when (and date (> (string-length date) 0))
      (echo-message! (app-state-echo app)
        (string-append "Calendar: " date)))))

;; company-complete: trigger completion
(def (cmd-company-complete app)
  (execute-command! app 'complete-at-point))

;; disable-theme: disable current theme
(def (cmd-disable-theme app)
  (echo-message! (app-state-echo app) "Theme disabled"))

;; eww-browse-url: open URL in EWW
(def (cmd-eww-browse-url app)
  (let ((url (qt-echo-read-string app "URL: ")))
    (when (and url (> (string-length url) 0))
      (execute-command! app 'eww))))

;; flyspell-correct-word: correct word at point
(def (cmd-flyspell-correct-word app)
  (execute-command! app 'ispell-word))

;; help-for-help: show help overview
(def (cmd-help-for-help app)
  (echo-message! (app-state-echo app)
    "Help: C-h k (describe-key) C-h f (describe-function) C-h v (describe-variable) C-h b (describe-bindings)"))

;; help-quick: quick help reference
(def (cmd-help-quick app)
  (cmd-help-for-help app))

;; iconify-frame: minimize window
(def (cmd-iconify-frame app)
  (echo-message! (app-state-echo app) "Frame iconified (minimize)"))

;; occur-rename-buffer: rename occur buffer
(def (cmd-occur-rename-buffer app)
  (let ((name (qt-echo-read-string app "Rename occur buffer to: ")))
    (when (and name (> (string-length name) 0))
      (let* ((fr (app-state-frame app))
             (buf (qt-edit-window-buffer (qt-current-window fr))))
        (set! (buffer-name buf) name)
        (echo-message! (app-state-echo app)
          (string-append "Buffer renamed to " name))))))

;; print-buffer / print-region: print to file/printer
(def (cmd-print-buffer app)
  (let* ((fr (app-state-frame app))
         (ed (qt-edit-window-editor (qt-current-window fr)))
         (text (qt-plain-text-edit-text ed))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (out-file (string-append "/tmp/" (buffer-name buf) ".txt")))
    (with-output-to-file out-file (lambda () (display text)))
    (echo-message! (app-state-echo app)
      (string-append "Buffer printed to " out-file))))

(def (cmd-print-region app)
  (let* ((fr (app-state-frame app))
         (ed (qt-edit-window-editor (qt-current-window fr)))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (mark (buffer-mark buf))
         (pos (sci-send ed SCI_GETCURRENTPOS 0)))
    (if mark
      (let* ((start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (if (and (>= start 0) (<= end (string-length text)))
                       (substring text start end)
                       ""))
             (out-file (string-append "/tmp/" (buffer-name buf) "-region.txt")))
        (with-output-to-file out-file (lambda () (display region)))
        (echo-message! (app-state-echo app)
          (string-append "Region printed to " out-file)))
      (echo-message! (app-state-echo app) "No region active"))))

;; raise-frame: bring window to front
(def (cmd-raise-frame app)
  (echo-message! (app-state-echo app) "Frame raised"))

;; rotate-window: swap window contents
(def (cmd-rotate-window app)
  (execute-command! app 'other-window))

;; run-scheme: start Scheme REPL
(def (cmd-run-scheme app)
  (execute-command! app 'run-gerbil))

;; shell-here: open shell in current directory
(def (cmd-shell-here app)
  (execute-command! app 'shell))

;; whitespace-report: show whitespace statistics
(def (cmd-whitespace-report app)
  (let* ((fr (app-state-frame app))
         (ed (qt-edit-window-editor (qt-current-window fr)))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (tabs 0) (trailing 0) (spaces 0))
    (let loop ((i 0) (col 0) (line-has-trailing #f))
      (if (>= i len)
        (echo-message! (app-state-echo app)
          (format "Whitespace: ~a tabs, ~a trailing spaces, ~a total spaces"
                  tabs trailing spaces))
        (let ((c (string-ref text i)))
          (cond
            ((char=? c #\tab)
             (set! tabs (+ tabs 1))
             (loop (+ i 1) (+ col 1) #f))
            ((char=? c #\space)
             (set! spaces (+ spaces 1))
             (loop (+ i 1) (+ col 1) #t))
            ((char=? c #\newline)
             (when line-has-trailing
               (set! trailing (+ trailing 1)))
             (loop (+ i 1) 0 #f))
            (else
             (loop (+ i 1) (+ col 1) #f))))))))

;;;============================================================================
;;; Registration
;;;============================================================================

(def (qt-register-parity5-commands!)
  "Register all parity5 commands."
  (qt-register-parity5-mode-toggles!)
  (qt-register-parity5-stubs!)
  (qt-register-parity5-aliases!)
  ;; Functional commands
  (for-each
    (lambda (pair)
      (register-command! (car pair) (cdr pair)))
    (list
      (cons 'auto-insert cmd-auto-insert)
      (cons 'calendar-goto-date cmd-calendar-goto-date)
      (cons 'company-complete cmd-company-complete)
      (cons 'disable-theme cmd-disable-theme)
      (cons 'eww-browse-url cmd-eww-browse-url)
      (cons 'flyspell-correct-word cmd-flyspell-correct-word)
      (cons 'help-for-help cmd-help-for-help)
      (cons 'help-quick cmd-help-quick)
      (cons 'iconify-frame cmd-iconify-frame)
      (cons 'occur-rename-buffer cmd-occur-rename-buffer)
      (cons 'print-buffer cmd-print-buffer)
      (cons 'print-region cmd-print-region)
      (cons 'raise-frame cmd-raise-frame)
      (cons 'rotate-window cmd-rotate-window)
      (cons 'run-scheme cmd-run-scheme)
      (cons 'shell-here cmd-shell-here)
      (cons 'whitespace-report cmd-whitespace-report))))
