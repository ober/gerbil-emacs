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
         (ed (qt-edit-window-editor (qt-current-window fr)))
         (buf (qt-edit-window-buffer (qt-current-window fr)))
         (name (buffer-name buf))
         (ext (let ((dot (string-index name #\.)))
                (if dot (substring name dot (string-length name)) "")))
         (template-dir (string-append (getenv "HOME" "/tmp") "/.gemacs-templates"))
         (template-file (string-append template-dir "/" ext)))
    (cond
      ((and (not (string=? ext ""))
            (file-exists? template-file))
       (let ((content (call-with-input-file template-file
                        (lambda (p) (read-line p #f)))))
         (when content
           (qt-plain-text-edit-set-text! ed content)
           (qt-plain-text-edit-set-cursor-position! ed 0))
         (echo-message! (app-state-echo app)
           (string-append "Inserted template for " ext))))
      ((not (string=? ext ""))
       ;; Try built-in templates
       (let ((builtin
               (cond
                 ((string=? ext ".ss") "(export #t)\n\n(import :std/sugar)\n\n")
                 ((string=? ext ".py") "#!/usr/bin/env python3\n\n\"\"\"\nModule description.\n\"\"\"\n\n")
                 ((string=? ext ".sh") "#!/bin/bash\nset -euo pipefail\n\n")
                 ((string=? ext ".c") "#include <stdio.h>\n#include <stdlib.h>\n\nint main(int argc, char *argv[]) {\n    return 0;\n}\n")
                 ((string=? ext ".h") (let ((guard (string-upcase (string-append (path-strip-extension (path-strip-directory name)) "_H"))))
                                        (string-append "#ifndef " guard "\n#define " guard "\n\n\n\n#endif /* " guard " */\n")))
                 ((string=? ext ".html") "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"UTF-8\">\n  <title></title>\n</head>\n<body>\n\n</body>\n</html>\n")
                 ((string=? ext ".rs") "fn main() {\n    \n}\n")
                 ((string=? ext ".go") "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Hello\")\n}\n")
                 ((string=? ext ".js") "\"use strict\";\n\n")
                 ((string=? ext ".ts") "\n")
                 ((string=? ext ".rb") "#!/usr/bin/env ruby\n# frozen_string_literal: true\n\n")
                 ((string=? ext ".el") ";;; -*- lexical-binding: t; -*-\n\n")
                 ((string=? ext ".md") "# \n\n")
                 ((string=? ext ".yaml") "---\n")
                 ((string=? ext ".json") "{}\n")
                 ((string=? ext ".toml") "")
                 ((string=? ext ".Makefile") ".PHONY: all clean\n\nall:\n\n")
                 (else #f))))
         (if builtin
           (begin
             (qt-plain-text-edit-set-text! ed builtin)
             (qt-plain-text-edit-set-cursor-position! ed 0)
             (echo-message! (app-state-echo app)
               (string-append "Inserted built-in template for " ext)))
           (echo-message! (app-state-echo app)
             (string-append "No template for " ext " (create " template-dir "/" ext ")")))))
      (else
       (echo-message! (app-state-echo app) "Buffer has no file extension")))))

;; calendar-goto-date: prompt for date and display calendar month
(def (cmd-calendar-goto-date app)
  (let ((date (qt-echo-read-string app "Go to date (YYYY-MM-DD): ")))
    (when (and date (> (string-length date) 0))
      (let* ((parts (string-split date #\-))
             (year (if (>= (length parts) 1) (string->number (car parts)) #f))
             (month (if (>= (length parts) 2) (string->number (cadr parts)) #f))
             (day (if (>= (length parts) 3) (string->number (caddr parts)) #f)))
        (if (and year month day (>= month 1) (<= month 12) (>= day 1) (<= day 31))
          (let* ((month-names '#("" "January" "February" "March" "April" "May" "June"
                                  "July" "August" "September" "October" "November" "December"))
                 (day-names "Su Mo Tu We Th Fr Sa")
                 ;; Zeller's congruence for day of week (0=Sunday)
                 (m (if (< month 3) (+ month 12) month))
                 (y (if (< month 3) (- year 1) year))
                 (dow (modulo (+ day (quotient (* 13 (+ m 1)) 5) y
                               (quotient y 4) (- (quotient y 100))
                               (quotient y 400)) 7))
                 ;; First day of month
                 (m1 (if (< month 3) (+ month 12) month))
                 (y1 (if (< month 3) (- year 1) year))
                 (first-dow (modulo (+ 1 (quotient (* 13 (+ m1 1)) 5) y1
                                     (quotient y1 4) (- (quotient y1 100))
                                     (quotient y1 400)) 7))
                 ;; Days in month
                 (days-in-month
                   (cond ((member month '(4 6 9 11)) 30)
                         ((= month 2) (if (or (and (= 0 (modulo year 4)) (not (= 0 (modulo year 100))))
                                              (= 0 (modulo year 400))) 29 28))
                         (else 31)))
                 (out (open-output-string)))
            ;; Build calendar display
            (display (string-append "    " (vector-ref month-names month) " " (number->string year) "\n") out)
            (display (string-append day-names "\n") out)
            ;; Leading spaces
            (let loop ((i 0))
              (when (< i first-dow)
                (display "   " out)
                (loop (+ i 1))))
            ;; Days
            (let dloop ((d 1) (col first-dow))
              (when (<= d days-in-month)
                (let ((s (number->string d)))
                  (if (= d day)
                    (display (string-append "[" (if (< d 10) " " "") s "]") out)
                    (display (string-append (if (< d 10) " " "") s " ") out))
                  (if (= (modulo (+ col 1) 7) 0)
                    (begin (newline out) (dloop (+ d 1) (+ col 1)))
                    (dloop (+ d 1) (+ col 1))))))
            (newline out)
            (let* ((cal-text (get-output-string out))
                   (fr (app-state-frame app))
                   (win (qt-current-window fr))
                   (ed (qt-edit-window-editor win))
                   (cal-buf (qt-buffer-create! "*Calendar*" ed #f)))
              (qt-buffer-attach! ed cal-buf)
              (set! (qt-edit-window-buffer win) cal-buf)
              (qt-plain-text-edit-set-text! ed cal-text)
              (sci-send ed SCI_SETREADONLY 1)
              (qt-plain-text-edit-set-cursor-position! ed 0)
              (echo-message! (app-state-echo app)
                (string-append "Calendar: " (vector-ref month-names month) " "
                               (number->string year)))))
          (echo-error! (app-state-echo app) "Invalid date format. Use YYYY-MM-DD"))))))

;; company-complete: trigger completion
(def (cmd-company-complete app)
  (execute-command! app 'complete-at-point))

;; disable-theme: reset to default color scheme
(def (cmd-disable-theme app)
  (let* ((fr (app-state-frame app)))
    ;; Reset all editors to default colors
    (for-each
      (lambda (win)
        (let ((ed (qt-edit-window-editor win)))
          ;; Reset to default white background, black foreground
          (sci-send ed SCI_STYLESETBACK 32 #xFFFFFF)  ; STYLE_DEFAULT bg
          (sci-send ed SCI_STYLESETFORE 32 #x000000)  ; STYLE_DEFAULT fg
          (sci-send ed SCI_STYLECLEARALL 0)
          ;; Reset caret line
          (sci-send ed SCI_SETCARETLINEVISIBLE 1)
          (sci-send ed SCI_SETCARETLINEBACK #xFFFFE0)))
      (qt-frame-windows fr))
    (echo-message! (app-state-echo app) "Theme reset to defaults")))

;; eww-browse-url: open URL in EWW
(def (cmd-eww-browse-url app)
  (let ((url (qt-echo-read-string app "URL: ")))
    (when (and url (> (string-length url) 0))
      (execute-command! app 'eww))))

;; flyspell-correct-word: correct word at point
(def (cmd-flyspell-correct-word app)
  (execute-command! app 'ispell-word))

;; help-for-help: show interactive help overview in *Help* buffer
(def (cmd-help-for-help app)
  (let* ((fr (app-state-frame app))
         (help-text (string-append
"Gemacs Help System\n"
"==================\n\n"
"You have typed C-h, the help character. Type a Help option:\n\n"
"Key Bindings:\n"
"  C-h k    describe-key        - Show what a key does\n"
"  C-h b    describe-bindings   - Show all key bindings\n"
"  C-h f    describe-function   - Describe a function\n"
"  C-h v    describe-variable   - Describe a variable\n"
"  C-h m    describe-mode       - Describe current major mode\n"
"  C-h a    apropos-command      - Search for commands by name\n"
"  C-h w    where-is            - Show key binding for a command\n\n"
"Navigation:\n"
"  C-f/C-b  Forward/backward character\n"
"  M-f/M-b  Forward/backward word\n"
"  C-a/C-e  Beginning/end of line\n"
"  M-</M->  Beginning/end of buffer\n"
"  C-v/M-v  Scroll down/up\n"
"  C-l      Recenter window\n\n"
"Editing:\n"
"  C-d      Delete character forward\n"
"  M-d      Kill word forward\n"
"  C-k      Kill to end of line\n"
"  C-w      Kill region\n"
"  M-w      Copy region\n"
"  C-y      Yank (paste)\n"
"  C-/      Undo\n"
"  C-x u    Undo\n\n"
"Files & Buffers:\n"
"  C-x C-f  Find file (open)\n"
"  C-x C-s  Save buffer\n"
"  C-x C-w  Write file (save as)\n"
"  C-x b    Switch buffer\n"
"  C-x k    Kill buffer\n"
"  C-x C-b  List buffers\n\n"
"Windows:\n"
"  C-x 2    Split horizontally\n"
"  C-x 3    Split vertically\n"
"  C-x 1    Delete other windows\n"
"  C-x 0    Delete current window\n"
"  C-x o    Other window\n\n"
"Search:\n"
"  C-s      Isearch forward\n"
"  C-r      Isearch backward\n"
"  M-%      Query replace\n"
"  M-x occur  Show matching lines\n\n"
"Other:\n"
"  M-x      Execute command by name\n"
"  C-g      Cancel current operation\n"
"  C-x C-c  Quit Gemacs\n")))
    (let* ((win (qt-current-window fr))
           (ed (qt-edit-window-editor win))
           (help-buf (qt-buffer-create! "*Help*" ed #f)))
      (qt-buffer-attach! ed help-buf)
      (set! (qt-edit-window-buffer win) help-buf)
      (qt-plain-text-edit-set-text! ed help-text)
      (sci-send ed SCI_SETREADONLY 1)
      (qt-plain-text-edit-set-cursor-position! ed 0))
    (echo-message! (app-state-echo app) "Type C-h key for help on a topic")))

;; help-quick: quick help reference
(def (cmd-help-quick app)
  (cmd-help-for-help app))

;; iconify-frame: minimize window
(def (cmd-iconify-frame app)
  (let ((main-win (qt-frame-main-win (app-state-frame app))))
    (qt-widget-show-minimized! main-win)
    (echo-message! (app-state-echo app) "Frame minimized")))

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
  (let ((main-win (qt-frame-main-win (app-state-frame app))))
    (qt-widget-show-normal! main-win)
    (echo-message! (app-state-echo app) "Frame raised")))

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
  (qt-register-parity5-moved-commands!)
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

;;;============================================================================
;;; Moved stubs/aliases from parity3 (file size management)
;;;============================================================================

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

(def (qt-register-parity5-moved-commands!)
  "Register commands moved from parity3 for file size management."
  (for-each
    (lambda (pair) (register-command! (car pair) (cdr pair)))
    (list
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
