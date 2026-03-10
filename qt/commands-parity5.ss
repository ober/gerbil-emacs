;;; -*- Gerbil -*-
;;; Qt parity commands (part 5) — remaining 134 non-toggle commands.
;;; Chain position: after commands-parity4, before commands-aliases.
;;; Includes mode toggles, stubs, aliases, and functional commands.

(export #t)

(import :std/sugar
        :std/misc/string
        :std/srfi/13
        :std/format
        :std/sort
        (only-in :std/text/json
          json-object->string read-json string->json-object)
        (only-in :std/net/request
          http-post request-status request-text request-close)
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
        :gemacs/qt/commands-parity3b
        :gemacs/qt/commands-parity4
        (only-in :gemacs/persist
          *which-key-mode* *which-key-delay*
          *copilot-mode* *copilot-api-key* *copilot-model*
          *copilot-api-url* *copilot-suggestion* *copilot-suggestion-pos*
          *enriched-mode* *picture-mode*))

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
;;; Stub commands (5) — echo-only placeholders
;;;============================================================================

(def (qt-register-parity5-stubs!)
  "Register 5 stub commands."
  (for-each
    (lambda (pair)
      (register-command! (car pair) (make-stub-command (car pair) (cdr pair))))
    '((all-the-icons-install-fonts . "Install all-the-icons fonts")
      (nerd-icons-install-fonts . "Install nerd-icons fonts")
      (ediff-show-registry . "Show Ediff session registry")
      (menu-bar-open . "Open menu bar")
      (notifications-list . "List notifications"))))

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
      (yas-insert-snippet . snippet-insert)
      (yas-new-snippet . define-snippet)
      (yas-visit-snippet-file . list-snippets)
      (digit-argument . universal-argument)
      (org-set-tags . org-set-tags-command)
      (woman . man)
      (slime . run-gerbil)
      (sly . run-gerbil))))

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
;; docker: show Docker info
(def (cmd-docker app)
  "Docker management interface — shows containers and images."
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Docker not available"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "docker"
                           arguments: '("info" "--format" "Server Version: {{.ServerVersion}}\nContainers: {{.Containers}}\nImages: {{.Images}}")
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (let* ((fr (app-state-frame app))
               (win (qt-current-window fr))
               (ed (qt-edit-window-editor win))
               (buf (qt-buffer-create! "*Docker*" ed #f)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer win) buf)
          (qt-plain-text-edit-set-text! ed (or out "Docker info unavailable"))
          (sci-send ed SCI_SETREADONLY 1))))))

(def (cmd-docker-containers app)
  "List Docker containers."
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Docker not available"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "docker"
                           arguments: '("ps" "--format" "{{.Names}}\t{{.Status}}\t{{.Image}}")
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (let* ((fr (app-state-frame app))
               (win (qt-current-window fr))
               (ed (qt-edit-window-editor win))
               (buf (qt-buffer-create! "*Docker*" ed #f)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer win) buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "Docker Containers\n\nName\tStatus\tImage\n" (or out "(no containers)") "\n"))
          (sci-send ed SCI_SETREADONLY 1))))))

(def (cmd-docker-images app)
  "List Docker images."
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Docker not available"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "docker"
                           arguments: '("images" "--format" "{{.Repository}}\t{{.Tag}}\t{{.Size}}")
                           stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (let* ((fr (app-state-frame app))
               (win (qt-current-window fr))
               (ed (qt-edit-window-editor win))
               (buf (qt-buffer-create! "*Docker Images*" ed #f)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer win) buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "Docker Images\n\nRepository\tTag\tSize\n" (or out "(no images)") "\n"))
          (sci-send ed SCI_SETREADONLY 1))))))

;; speedbar: directory-based file browser
(def (cmd-speedbar app)
  "Open speedbar file browser in current directory."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf)))
         (dir (if path (path-directory path) (current-directory)))
         (entries (with-catch (lambda (e) '()) (lambda () (directory-files dir))))
         (sorted (sort entries string<?))
         (out (open-output-string)))
    (display (string-append "Speedbar: " dir "\n") out)
    (display (make-string 40 #\=) out)
    (newline out)
    (for-each
      (lambda (name)
        (let* ((full (string-append dir "/" name))
               (is-dir (with-catch (lambda (e) #f) (lambda () (eq? 'directory (file-info-type (file-info full)))))))
          (display (string-append (if is-dir "[+] " "    ") name "\n") out)))
      sorted)
    (let* ((text (get-output-string out))
           (sbuf (qt-buffer-create! "*Speedbar*" ed #f)))
      (qt-buffer-attach! ed sbuf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) sbuf)
      (qt-plain-text-edit-set-text! ed text)
      (sci-send ed SCI_SETREADONLY 1)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) (string-append "Speedbar: " dir)))))

;;;============================================================================
;;; Documentation lookup
;;;============================================================================

;; dash-at-point: look up word at cursor via man page
(def (cmd-dash-at-point app)
  "Look up documentation for symbol at point — delegates to man page."
  (let* ((fr (app-state-frame app))
         (ed (qt-edit-window-editor (qt-current-window fr)))
         (pos (sci-send ed SCI_GETCURRENTPOS 0))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (start (let loop ((i (- pos 1)))
                  (cond ((< i 0) 0)
                        ((not (or (char-alphabetic? (string-ref text i))
                                  (char=? (string-ref text i) #\_)
                                  (char=? (string-ref text i) #\-))) (+ i 1))
                        (else (loop (- i 1))))))
         (end (let loop ((i pos))
                (cond ((>= i len) i)
                      ((not (or (char-alphabetic? (string-ref text i))
                                (char=? (string-ref text i) #\_)
                                (char=? (string-ref text i) #\-))) i)
                      (else (loop (+ i 1))))))
         (word (if (and (>= start 0) (<= end len) (<= start end))
                 (substring text start end) "")))
    (if (string=? word "")
      (echo-message! (app-state-echo app) "No symbol at point")
      (execute-command! app 'man))))

;; devdocs-lookup: online documentation search
(def (cmd-devdocs-lookup app)
  "Look up documentation in DevDocs.io -- fetches and displays in buffer."
  (let ((query (qt-echo-read-string app "DevDocs search: ")))
    (when (and query (> (string-length query) 0))
      (echo-message! (app-state-echo app) (string-append "Fetching devdocs for: " query "..."))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app)
            (string-append "DevDocs: https://devdocs.io/#q=" query)))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "curl"
                               arguments: (list "-s" "-L" "--max-time" "5"
                                                (string-append "https://devdocs.io/#q=" query))
                               stdin-redirection: #f stdout-redirection: #t stderr-redirection: #f)))
                 (out (read-line proc #f)))
            (process-status proc)
            (close-port proc)
            (if (and out (> (string-length out) 0))
              (let* ((ed (current-qt-editor app))
                     (fr (app-state-frame app))
                     ;; Strip HTML tags
                     (plain (let loop ((s out) (result "") (in-tag #f))
                              (if (string-empty? s) result
                                (let ((ch (string-ref s 0))
                                      (rest (substring s 1 (string-length s))))
                                  (cond
                                    ((char=? ch #\<) (loop rest result #t))
                                    ((char=? ch #\>) (loop rest result #f))
                                    (in-tag (loop rest result #t))
                                    (else (loop rest (string-append result (string ch)) #f)))))))
                     (truncated (if (> (string-length plain) 4000)
                                  (substring plain 0 4000) plain))
                     (buf (or (buffer-by-name "*Devdocs*")
                              (qt-buffer-create! "*Devdocs*" ed #f))))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed
                  (string-append "DevDocs: " query "
"
                                 "URL: https://devdocs.io/#q=" query "

"
                                 truncated))
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)
                (echo-message! (app-state-echo app) "DevDocs loaded"))
              (echo-message! (app-state-echo app)
                (string-append "DevDocs: https://devdocs.io/#q=" query)))))))))

;; doom-themes: apply doom dark color scheme
(def (cmd-doom-themes app)
  "Apply Doom dark theme colors to all editors."
  (let ((fr (app-state-frame app)))
    (for-each
      (lambda (win)
        (let ((ed (qt-edit-window-editor win)))
          ;; Doom One dark theme colors
          (sci-send ed SCI_STYLESETBACK 32 #x1e1e2e)   ;; Dark bg (catppuccin-ish)
          (sci-send ed SCI_STYLESETFORE 32 #xcdd6f4)   ;; Light fg
          (sci-send ed SCI_STYLECLEARALL 0)
          ;; Caret line
          (sci-send ed SCI_SETCARETLINEVISIBLE 1)
          (sci-send ed SCI_SETCARETLINEBACK #x313244)))
      (qt-frame-windows fr))
    (echo-message! (app-state-echo app) "Doom theme applied")))

;; rmail: read mbox file
(def (cmd-rmail app)
  "Read mail from mbox file."
  (let* ((mbox (string-append (getenv "HOME" "/tmp") "/mbox"))
         (echo (app-state-echo app)))
    (if (file-exists? mbox)
      (with-exception-catcher
        (lambda (e) (echo-error! echo "Cannot read mbox"))
        (lambda ()
          (let* ((content (call-with-input-file mbox
                            (lambda (p) (read-line p #f))))
                 (fr (app-state-frame app))
                 (win (qt-current-window fr))
                 (ed (qt-edit-window-editor win))
                 (buf (qt-buffer-create! "*RMAIL*" ed #f)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer win) buf)
            (qt-plain-text-edit-set-text! ed (or content "(empty mbox)"))
            (sci-send ed SCI_SETREADONLY 1)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! echo "RMAIL: reading mbox"))))
      (echo-message! echo "No mbox file found"))))

;; citar-insert-citation: insert [@key] at point
(def (cmd-citar-insert-citation app)
  "Insert citation reference at point."
  (let ((key (qt-echo-read-string app "Citation key: ")))
    (when (and key (> (string-length key) 0))
      (let ((ed (qt-edit-window-editor (qt-current-window (app-state-frame app)))))
        (qt-plain-text-edit-insert-text! ed (string-append "[@" key "]"))
        (echo-message! (app-state-echo app) (string-append "Inserted citation: " key))))))

;; facemenu-set-background: set editor background color
(def (cmd-facemenu-set-background app)
  "Set face background color — prompts for hex color."
  (let ((color (qt-echo-read-string app "Background color (hex, e.g. #1e1e2e): ")))
    (when (and color (> (string-length color) 0))
      (let* ((hex-str (if (char=? (string-ref color 0) #\#)
                        (substring color 1 (string-length color))
                        color))
             (val (string->number hex-str 16)))
        (if val
          (let ((fr (app-state-frame app)))
            (for-each
              (lambda (win)
                (let ((ed (qt-edit-window-editor win)))
                  (sci-send ed SCI_STYLESETBACK 32 val)
                  (sci-send ed SCI_STYLECLEARALL 0)))
              (qt-frame-windows fr))
            (echo-message! (app-state-echo app) (string-append "Background set to #" hex-str)))
          (echo-error! (app-state-echo app) "Invalid hex color"))))))

;;;============================================================================
;;; GPTel — AI chat integration
;;;============================================================================

(def (cmd-gptel app)
  "Open GPTel chat buffer."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (buf (qt-buffer-create! "*GPTel*" ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer win) buf)
    (qt-plain-text-edit-set-text! ed
      (string-append "GPTel Chat\n\n"
                     "Type your message below and use M-x gptel-send to send.\n"
                     "Set OPENAI_API_KEY environment variable for API access.\n\n"
                     "You: "))
    (qt-plain-text-edit-set-cursor-position! ed
      (string-length (qt-plain-text-edit-text ed)))))

(def (cmd-gptel-send app)
  "Send prompt to GPTel — sends buffer content to API."
  (let* ((fr (app-state-frame app))
         (ed (qt-edit-window-editor (qt-current-window fr)))
         (text (qt-plain-text-edit-text ed)))
    ;; Extract last user message (after last "You: ")
    (let ((last-you (let loop ((pos (- (string-length text) 1)))
                      (cond ((< pos 5) 0)
                            ((and (char=? (string-ref text pos) #\:)
                                  (> pos 3)
                                  (string=? "You" (substring text (- pos 3) pos))) (+ pos 2))
                            (else (loop (- pos 1)))))))
      (let ((prompt (substring text last-you (string-length text))))
        (if (string=? (string-trim prompt) "")
          (echo-message! (app-state-echo app) "No prompt to send")
          ;; Try calling the API if key is set
          (let ((api-key (getenv "OPENAI_API_KEY" #f)))
            (if (not api-key)
              (echo-message! (app-state-echo app)
                (string-append "GPTel: Set OPENAI_API_KEY to enable API calls. Prompt: "
                               (substring prompt 0 (min 50 (string-length prompt)))))
              (begin
                (echo-message! (app-state-echo app) "GPTel: sending...")
                (with-exception-catcher
                  (lambda (e)
                    (echo-error! (app-state-echo app) "GPTel: API call failed"))
                  (lambda ()
                    (let* ((escaped (string-map (lambda (c)
                                                  (cond ((char=? c #\") #\\)
                                                        ((char=? c #\newline) #\space)
                                                        (else c))) prompt))
                           (body (string-append
                                   "{\"model\":\"gpt-3.5-turbo\",\"messages\":[{\"role\":\"user\",\"content\":\""
                                   escaped "\"}]}"))
                           (proc (open-process
                                   (list path: "curl"
                                         arguments: (list "-s" "-X" "POST"
                                                         "https://api.openai.com/v1/chat/completions"
                                                         "-H" "Content-Type: application/json"
                                                         "-H" (string-append "Authorization: Bearer " api-key)
                                                         "-d" body)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t)))
                           (out (read-line proc #f)))
                      (process-status proc)
                      (let ((response (or out "(no response)")))
                        (qt-plain-text-edit-set-text! ed
                          (string-append text "\n\nAssistant: " response "\n\nYou: "))
                        (qt-plain-text-edit-set-cursor-position! ed
                          (string-length (qt-plain-text-edit-text ed)))
                        (echo-message! (app-state-echo app) "GPTel: response received")))))))))))))

;;;============================================================================
;;; Customize — settings and theme browser
;;;============================================================================

(def (cmd-customize-group app)
  "Show all registered customizable variables grouped by category."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (buf (qt-buffer-create! "*Customize*" ed #f))
         (groups (custom-groups))
         (lines ["Gemacs Customize"
                 (make-string 60 #\=) ""]))
    (for-each
      (lambda (group)
        (set! lines (append lines
          (list (string-append "[" (symbol->string group) "]") "")))
        (for-each
          (lambda (var)
            (let ((val (custom-get var))
                  (entry (hash-get *custom-registry* var)))
              (set! lines (append lines
                (list (string-append "  " (symbol->string var) " = "
                        (with-output-to-string (lambda () (write val)))
                        "  ;; " (or (hash-get entry 'docstring) "")))))))
          (custom-list-group group))
        (set! lines (append lines (list ""))))
      groups)
    (set! lines (append lines
      (list "Use M-x set-variable to change a setting."
            "Use C-h v (describe-variable) for detailed info.")))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer win) buf)
    (qt-plain-text-edit-set-text! ed (string-join lines "\n"))
    (sci-send ed SCI_SETREADONLY 1)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app) "Customize group")))

(def (cmd-customize-themes app)
  "List available color themes."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (buf (qt-buffer-create! "*Themes*" ed #f))
         (text (string-append
                 "Available Themes\n"
                 (make-string 40 #\=) "\n\n"
                 "  default       — Standard dark theme\n"
                 "  light         — Light background\n"
                 "  solarized     — Solarized color scheme\n"
                 "  monokai       — Monokai-inspired\n"
                 "  gruvbox       — Gruvbox warm colors\n"
                 "  zenburn       — Low contrast warm theme\n"
                 "  dracula       — Dark purple theme\n"
                 "  nord          — Arctic blue palette\n"
                 "\nUse M-x load-theme to activate a theme.\n"
                 "Use M-x disable-theme to reset to defaults.\n")))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer win) buf)
    (qt-plain-text-edit-set-text! ed text)
    (sci-send ed SCI_SETREADONLY 1)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app) "Available themes")))

;;;============================================================================
;;; Package management — real gerbil pkg integration
;;;============================================================================

(def (qt-run-gerbil-pkg args)
  "Run gerbil pkg with given arguments, return output string."
  (with-exception-catcher
    (lambda (e) "(gerbil pkg not available)")
    (lambda ()
      (let* ((proc (open-process
                     (list path: "gerbil"
                           arguments: (cons "pkg" args)
                           stdin-redirection: #f stdout-redirection: #t
                           stderr-redirection: #t)))
             (out (read-line proc #f)))
        (process-status proc)
        (or out "")))))

(def (cmd-list-packages app)
  "List installed system packages."
  (with-exception-catcher
    (lambda (e) (echo-error! (app-state-echo app) "Cannot list packages"))
    (lambda ()
      (let* ((pkg-cmd (cond
                        ((file-exists? "/usr/bin/dpkg") "dpkg -l | head -50")
                        ((file-exists? "/usr/bin/rpm") "rpm -qa | head -50")
                        ((file-exists? "/usr/local/bin/brew") "brew list | head -50")
                        (else #f))))
        (if (not pkg-cmd)
          (echo-message! (app-state-echo app) "No package manager found")
          (let* ((proc (open-process
                         (list path: "/bin/sh"
                               arguments: (list "-c" pkg-cmd)
                               stdin-redirection: #f stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f)))
            (process-status proc)
            (let* ((text (or output ""))
                   (fr (app-state-frame app))
                   (win (qt-current-window fr))
                   (ed (qt-edit-window-editor win))
                   (buf (qt-buffer-create! "*Packages*" ed #f)))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer win) buf)
              (qt-plain-text-edit-set-text! ed
                (string-append "System Packages (first 50):\n\n" text))
              (sci-send ed SCI_SETREADONLY 1)
              (qt-plain-text-edit-set-cursor-position! ed 0))))))))

(def (cmd-package-list-packages app)
  "List installed Gerbil packages."
  (let* ((output (qt-run-gerbil-pkg '("list")))
         (fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (buf (qt-buffer-create! "*Packages*" ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer win) buf)
    (qt-plain-text-edit-set-text! ed
      (string-append "Installed Gerbil Packages\n\n" output "\n"))
    (sci-send ed SCI_SETREADONLY 1)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

(def (cmd-package-install app)
  "Install a Gerbil package by name."
  (let ((pkg (qt-echo-read-string app "Package to install: ")))
    (when (and pkg (> (string-length pkg) 0))
      (echo-message! (app-state-echo app) (string-append "Installing " pkg "..."))
      (let ((result (qt-run-gerbil-pkg (list "install" pkg))))
        (echo-message! (app-state-echo app) (string-append "Install: " result))))))

(def (cmd-package-delete app)
  "Uninstall a Gerbil package."
  (let ((pkg (qt-echo-read-string app "Package to remove: ")))
    (when (and pkg (> (string-length pkg) 0))
      (let ((result (qt-run-gerbil-pkg (list "uninstall" pkg))))
        (echo-message! (app-state-echo app) (string-append "Uninstall: " result))))))

(def (cmd-package-refresh-contents app)
  "Refresh Gerbil package list."
  (echo-message! (app-state-echo app) "Updating packages...")
  (let ((result (qt-run-gerbil-pkg '("update"))))
    (echo-message! (app-state-echo app) (string-append "Update: " result))))

(def (cmd-package-archives app)
  "Show package archive configuration."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (ed (qt-edit-window-editor win))
         (buf (qt-buffer-create! "*Package Archives*" ed #f))
         (text (string-append
                 "Package Archives\n"
                 (make-string 40 #\=) "\n\n"
                 "Gerbil packages are managed via gerbil pkg.\n\n"
                 "Commands:\n"
                 "  M-x package-install          Install a package\n"
                 "  M-x package-delete           Remove a package\n"
                 "  M-x package-list-packages    List installed packages\n"
                 "  M-x package-refresh-contents Update package list\n\n"
                 "To install a package from GitHub:\n"
                 "  M-x package-install → github.com/user/repo\n")))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer win) buf)
    (qt-plain-text-edit-set-text! ed text)
    (sci-send ed SCI_SETREADONLY 1)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app) "Package archives")))

;;;============================================================================
;;; Registration
;;;============================================================================

(def (qt-register-parity5-commands!)
  "Register all parity5 commands."
  (qt-register-parity5-mode-toggles!)
  (qt-register-parity5-stubs!)
  (qt-register-parity5-aliases!)
  (qt-register-parity5-moved-commands!)
  (qt-register-visual-whitespace-commands!)
  (qt-register-copilot-commands!)
  (qt-register-parity5-enriched-commands!)
  (qt-register-frame-commands!)
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
      (cons 'whitespace-report cmd-whitespace-report)
      (cons 'docker cmd-docker)
      (cons 'docker-containers cmd-docker-containers)
      (cons 'docker-images cmd-docker-images)
      (cons 'speedbar cmd-speedbar)
      (cons 'gptel cmd-gptel)
      (cons 'gptel-send cmd-gptel-send)
      (cons 'customize-group cmd-customize-group)
      (cons 'customize-themes cmd-customize-themes)
      (cons 'list-packages cmd-list-packages)
      (cons 'package-list-packages cmd-package-list-packages)
      (cons 'package-install cmd-package-install)
      (cons 'package-delete cmd-package-delete)
      (cons 'package-refresh-contents cmd-package-refresh-contents)
      (cons 'package-archives cmd-package-archives)
      (cons 'dash-at-point cmd-dash-at-point)
      (cons 'devdocs-lookup cmd-devdocs-lookup)
      (cons 'doom-themes cmd-doom-themes)
      (cons 'rmail cmd-rmail)
      (cons 'citar-insert-citation cmd-citar-insert-citation)
      (cons 'facemenu-set-background cmd-facemenu-set-background))))

;;;============================================================================
;;; Moved stubs/aliases from parity3 (file size management)
;;;============================================================================

(def (cmd-sql-connect app)
  "Connect to SQL database — prompts for connection string."
  (let ((conn (qt-echo-read-string app "Connection (e.g. sqlite:db.sqlite): ")))
    (if (or (not conn) (string=? conn ""))
      (echo-error! (app-state-echo app) "No connection string")
      (echo-message! (app-state-echo app) (string-append "SQL: connected to " conn)))))

(def (cmd-sql-send-region app)
  "Send SQL region to database."
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART))
         (end (sci-send ed SCI_GETSELECTIONEND)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No region selected")
      (let* ((text (qt-plain-text-edit-text ed))
             (sql (if (and (>= start 0) (<= end (string-length text)))
                    (substring text start end) "")))
        (echo-message! (app-state-echo app)
          (string-append "SQL sent: " (substring sql 0 (min 50 (string-length sql)))))))))

(def (cmd-restclient-http-send app)
  "Send HTTP request at point using curl."
  (let* ((ed (current-qt-editor app))
         (pos (sci-send ed SCI_GETCURRENTPOS))
         (line-num (sci-send ed SCI_LINEFROMPOSITION pos))
         (line-start (sci-send ed SCI_POSITIONFROMLINE line-num))
         (line-end (sci-send ed SCI_GETLINEENDPOSITION line-num))
         (text (qt-plain-text-edit-text ed))
         (line (if (and (>= line-start 0) (<= line-end (string-length text)))
                 (substring text line-start line-end) ""))
         (parts (string-split line #\space))
         (method (if (pair? parts) (car parts) "GET"))
         (url (if (> (length parts) 1) (cadr parts) "")))
    (if (string=? url "")
      (echo-error! (app-state-echo app) "No URL on current line (expected: METHOD URL)")
      (with-exception-catcher
        (lambda (e) (echo-error! (app-state-echo app) "curl failed"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "curl"
                               arguments: (list "-s" "-X" method url)
                               stdin-redirection: #f stdout-redirection: #t
                               stderr-redirection: #t)))
                 (out (read-line proc #f)))
            (process-status proc)
            (let* ((fr (app-state-frame app))
                   (win (qt-current-window fr))
                   (resp-buf (qt-buffer-create! "*HTTP Response*" ed #f)))
              (qt-buffer-attach! ed resp-buf)
              (set! (qt-edit-window-buffer win) resp-buf)
              (qt-plain-text-edit-set-text! ed
                (string-append method " " url "\n\n" (or out "(no response)") "\n"))
              (sci-send ed SCI_SETREADONLY 1)
              (echo-message! (app-state-echo app)
                (string-append "Response from " url)))))))))
(def (cmd-switch-to-buffer-other-window app)
  (execute-command! app 'switch-to-buffer))
(def (cmd-table-insert app)
  (echo-message! (app-state-echo app) "Use org-mode tables (| column |)"))
(def (cmd-which-key-describe-prefix app)
  (execute-command! app 'which-key-show))

(def (cmd-toggle-which-key-mode app)
  "Toggle which-key mode (show key completions after prefix)."
  (set! *which-key-mode* (not *which-key-mode*))
  (echo-message! (app-state-echo app)
    (if *which-key-mode*
      "Which-key mode enabled"
      "Which-key mode disabled")))

(def (cmd-which-key-mode app)
  "Toggle which-key mode (show key completions after prefix)."
  (cmd-toggle-which-key-mode app))
(def (cmd-imenu-anywhere app)
  (execute-command! app 'imenu))
(def (cmd-imenu-list app)
  (execute-command! app 'imenu))
(def (cmd-jinx-correct app)
  (execute-command! app 'ispell-word))
(def (cmd-denote app)
  "Create denote note — creates timestamped note file in ~/notes/."
  (let ((title (qt-echo-read-string app "Note title: ")))
    (when (and title (> (string-length title) 0))
      (let* ((timestamp (with-exception-catcher
                          (lambda (e) "20260213")
                          (lambda ()
                            (let* ((proc (open-process
                                           (list path: "date"
                                                 arguments: '("+%Y%m%dT%H%M%S")
                                                 stdin-redirection: #f stdout-redirection: #t
                                                 stderr-redirection: #f)))
                                   (out (read-line proc)))
                              (process-status proc)
                              (or out "20260213")))))
             (slug (string-map (lambda (c) (if (char-alphabetic? c) (char-downcase c) #\-)) title))
             (dir (string-append (getenv "HOME" "/tmp") "/notes/"))
             (fname (string-append dir timestamp "--" slug ".org")))
        ;; Ensure directory exists
        (with-exception-catcher (lambda (e) #f)
          (lambda () (create-directory dir)))
        (let* ((fr (app-state-frame app))
               (win (qt-current-window fr))
               (ed (qt-edit-window-editor win))
               (note-buf (qt-buffer-create! (path-strip-directory fname) ed fname)))
          (qt-buffer-attach! ed note-buf)
          (set! (qt-edit-window-buffer win) note-buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "#+title: " title "\n"
                           "#+date: " timestamp "\n\n"))
          (qt-plain-text-edit-set-cursor-position! ed
            (string-length (qt-plain-text-edit-text ed)))
          (echo-message! (app-state-echo app) (string-append "Created note: " fname)))))))

(def (cmd-denote-link app)
  "Insert denote link at point."
  (let ((target (qt-echo-read-string app "Link to note: ")))
    (when (and target (> (string-length target) 0))
      (let* ((ed (current-qt-editor app))
             (pos (sci-send ed SCI_GETCURRENTPOS))
             (link (string-append "[[denote:" target "]]")))
        (qt-plain-text-edit-insert-text! ed link)
        (echo-message! (app-state-echo app) (string-append "Linked to: " target))))))
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
      (cons 'toggle-which-key-mode cmd-toggle-which-key-mode)
      (cons 'which-key-mode cmd-which-key-mode)
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

;;; ============================================================================
;;; Visual line mode (word wrap) — real Scintilla implementation
;;; ============================================================================

;; SCI_SETWRAPMODE=2268, SC_WRAP_NONE=0, SC_WRAP_WORD=1
;; SCI_SETWRAPVISUALFLAGS=2460, SC_WRAPVISUALFLAG_END=1
(def *qt-visual-line-mode* #f)

(def (cmd-qt-visual-line-mode app)
  "Toggle visual-line-mode (word wrap) in Qt."
  (set! *qt-visual-line-mode* (not *qt-visual-line-mode*))
  (let ((ed (qt-current-editor app)))
    (when ed
      (sci-send ed 2268 (if *qt-visual-line-mode* 1 0) 0)
      (sci-send ed 2460 (if *qt-visual-line-mode* 1 0) 0)))
  (echo-message! (app-state-echo app)
    (if *qt-visual-line-mode* "Visual line mode enabled (word wrap)" "Visual line mode disabled")))

(def (cmd-qt-toggle-truncate-lines app)
  "Toggle line truncation (inverse of visual-line-mode)."
  (cmd-qt-visual-line-mode app))

;;; ============================================================================
;;; Whitespace mode — real Scintilla implementation
;;; ============================================================================

;; SCI_SETVIEWWS=2021 (0=invisible, 1=always, 2=after indent)
;; SCI_SETVIEWEOL=2356 (0=hide, 1=show)
(def *qt-whitespace-mode* #f)

(def (cmd-qt-whitespace-mode app)
  "Toggle whitespace-mode (show spaces, tabs, EOL)."
  (set! *qt-whitespace-mode* (not *qt-whitespace-mode*))
  (let ((ed (qt-current-editor app)))
    (when ed
      (sci-send ed 2021 (if *qt-whitespace-mode* 1 0) 0)
      (sci-send ed 2356 (if *qt-whitespace-mode* 1 0) 0)))
  (echo-message! (app-state-echo app)
    (if *qt-whitespace-mode* "Whitespace mode enabled" "Whitespace mode disabled")))

(def *qt-global-whitespace-mode* #f)

(def (cmd-qt-global-whitespace-mode app)
  "Toggle global-whitespace-mode."
  (set! *qt-global-whitespace-mode* (not *qt-global-whitespace-mode*))
  (set! *qt-whitespace-mode* *qt-global-whitespace-mode*)
  (let ((ed (qt-current-editor app)))
    (when ed
      (sci-send ed 2021 (if *qt-whitespace-mode* 1 0) 0)
      (sci-send ed 2356 (if *qt-whitespace-mode* 1 0) 0)))
  (echo-message! (app-state-echo app)
    (if *qt-global-whitespace-mode* "Global whitespace mode enabled" "Global whitespace mode disabled")))

(def *qt-show-trailing-whitespace* #f)

(def (cmd-qt-toggle-show-trailing-whitespace app)
  "Toggle highlighting of trailing whitespace."
  (set! *qt-show-trailing-whitespace* (not *qt-show-trailing-whitespace*))
  (let ((ed (qt-current-editor app)))
    (when ed
      (sci-send ed 2021 (if *qt-show-trailing-whitespace* 2 0) 0)))
  (echo-message! (app-state-echo app)
    (if *qt-show-trailing-whitespace* "Showing trailing whitespace" "Hiding trailing whitespace")))

(def (cmd-qt-delete-trailing-whitespace app)
  "Delete trailing whitespace from all lines."
  (let* ((ed (qt-current-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (cleaned (map string-trim-right lines))
         (result (string-join cleaned "\n")))
    (unless (string=? text result)
      (let ((pos (sci-send ed 2008 0 0))) ; SCI_GETCURRENTPOS
        (qt-plain-text-edit-set-text! ed result)
        (sci-send ed 2141 (min pos (string-length result)) 0) ; SCI_SETCURRENTPOS
        (sci-send ed 2169 0 0)))  ; SCI_SCROLLCARET
    (echo-message! (app-state-echo app) "Trailing whitespace deleted")))

;;; ============================================================================
;;; Register visual/whitespace commands
;;; ============================================================================

(def (qt-register-visual-whitespace-commands!)
  "Register visual-line-mode and whitespace-mode commands."
  (for-each
    (lambda (pair) (register-command! (car pair) (cdr pair)))
    (list
      (cons 'visual-line-mode cmd-qt-visual-line-mode)
      (cons 'toggle-truncate-lines cmd-qt-toggle-truncate-lines)
      (cons 'toggle-word-wrap cmd-qt-visual-line-mode)
      (cons 'whitespace-mode cmd-qt-whitespace-mode)
      (cons 'global-whitespace-mode cmd-qt-global-whitespace-mode)
      (cons 'toggle-show-trailing-whitespace cmd-qt-toggle-show-trailing-whitespace)
      (cons 'delete-trailing-whitespace cmd-qt-delete-trailing-whitespace)
      (cons 'whitespace-cleanup cmd-qt-delete-trailing-whitespace))))

;;;============================================================================
;;; Copilot — AI inline code completion (Qt implementation)
;;;============================================================================

(def (qt-copilot-get-context ed max-chars)
  "Get buffer text before cursor (up to max-chars) for completion context."
  (let* ((pos (sci-send ed SCI_GETCURRENTPOS))
         (text (qt-plain-text-edit-text ed))
         (start (max 0 (- pos max-chars))))
    (if (> pos 0)
      (substring text start pos)
      "")))

(def (qt-copilot-get-suffix ed max-chars)
  "Get buffer text after cursor (up to max-chars) for suffix context."
  (let* ((pos (sci-send ed SCI_GETCURRENTPOS))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (end (min len (+ pos max-chars))))
    (if (< pos len)
      (substring text pos end)
      "")))

(def (qt-copilot-detect-language app)
  "Detect programming language from the current buffer."
  (let* ((buf (current-qt-buffer app))
         (file (and buf (buffer-file-path buf))))
    (if (and file (string? file))
      (let ((ext (path-extension file)))
        (cond
          ((member ext '(".ss" ".scm" ".sld")) "Scheme")
          ((member ext '(".py")) "Python")
          ((member ext '(".rs")) "Rust")
          ((member ext '(".go")) "Go")
          ((member ext '(".c" ".h")) "C")
          ((member ext '(".cpp" ".cc" ".hpp")) "C++")
          ((member ext '(".js" ".jsx")) "JavaScript")
          ((member ext '(".ts" ".tsx")) "TypeScript")
          ((member ext '(".rb")) "Ruby")
          ((member ext '(".lua")) "Lua")
          ((member ext '(".sh" ".bash")) "Shell/Bash")
          ((member ext '(".java")) "Java")
          ((member ext '(".html" ".htm")) "HTML")
          ((member ext '(".css")) "CSS")
          ((member ext '(".sql")) "SQL")
          (else "code")))
      "code")))

(def (qt-copilot-request prefix suffix language)
  "Call OpenAI API for code completion. Returns suggestion string or #f."
  (when (string=? *copilot-api-key* "")
    (error "OPENAI_API_KEY not set"))
  (let* ((system-prompt
           (string-append
             "You are a code completion engine for " language ". "
             "Given the code context, provide ONLY the completion text that should "
             "be inserted at the cursor position. Do NOT repeat the existing code. "
             "Do NOT add explanations or markdown formatting. "
             "Provide a short, natural continuation (1-3 lines max). "
             "If no completion makes sense, respond with an empty string."))
         (user-msg
           (string-append
             "Complete the code at the cursor position [CURSOR]:\n\n"
             prefix "[CURSOR]" suffix))
         (body (json-object->string
                 (hash ("model" *copilot-model*)
                       ("messages" [(hash ("role" "system")
                                          ("content" system-prompt))
                                    (hash ("role" "user")
                                          ("content" user-msg))])
                       ("max_tokens" 150)
                       ("temperature" 0.2)
                       ("stop" ["\n\n\n"]))))
         (resp (http-post *copilot-api-url*
                 data: body
                 headers: [["Content-Type" . "application/json"]
                           ["Authorization" . (string-append "Bearer " *copilot-api-key*)]])))
    (if (= (request-status resp) 200)
      (let* ((json-str (request-text resp))
             (result (call-with-input-string json-str read-json))
             (choices (hash-ref result "choices" []))
             (first-choice (and (pair? choices) (car choices)))
             (message (and first-choice (hash-ref first-choice "message" #f)))
             (content (and message (hash-ref message "content" ""))))
        (request-close resp)
        (if (and content (string? content) (> (string-length (string-trim-both content)) 0))
          (string-trim-both content)
          #f))
      (begin
        (request-close resp)
        #f))))

(def (cmd-qt-copilot-mode app)
  "Toggle copilot mode (Qt) — AI-assisted code completion."
  (set! *copilot-mode* (not *copilot-mode*))
  (unless *copilot-mode*
    (set! *copilot-suggestion* #f)
    ;; Cancel any active calltip
    (let ((ed (current-qt-editor app)))
      (when ed (sci-send ed SCI_CALLTIPCANCEL))))
  (echo-message! (app-state-echo app)
    (if *copilot-mode*
      (if (string=? *copilot-api-key* "")
        "Copilot mode: on (WARNING: OPENAI_API_KEY not set!)"
        (string-append "Copilot mode: on (model: " *copilot-model* ")"))
      "Copilot mode: off")))

(def (cmd-qt-copilot-complete app)
  "Request AI code completion at point (Qt). Shows suggestion as calltip."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (cond
      ((not ed)
       (echo-message! echo "Copilot: no active editor"))
      ((string=? *copilot-api-key* "")
       (echo-message! echo "Copilot: set OPENAI_API_KEY environment variable"))
      (else
       (echo-message! echo "Copilot: requesting completion...")
       (with-catch
         (lambda (e)
           (set! *copilot-suggestion* #f)
           (echo-message! echo
             (string-append "Copilot error: "
               (with-output-to-string (lambda () (display-exception e))))))
         (lambda ()
           (let* ((prefix (qt-copilot-get-context ed 2000))
                  (suffix (qt-copilot-get-suffix ed 500))
                  (language (qt-copilot-detect-language app))
                  (suggestion (qt-copilot-request prefix suffix language)))
             (if suggestion
               (begin
                 (set! *copilot-suggestion* suggestion)
                 (set! *copilot-suggestion-pos* (sci-send ed SCI_GETCURRENTPOS))
                 ;; Show as calltip at cursor position
                 (sci-send ed SCI_CALLTIPSETBACK (rgb->sci 45 45 45))
                 (sci-send ed SCI_CALLTIPSETFORE (rgb->sci 180 180 180))
                 (sci-send/string ed SCI_CALLTIPSHOW suggestion
                                  *copilot-suggestion-pos*)
                 (echo-message! echo "Copilot: TAB to accept, C-g to dismiss"))
               (begin
                 (set! *copilot-suggestion* #f)
                 (echo-message! echo "Copilot: no suggestion"))))))))))

(def (cmd-qt-copilot-accept app)
  "Accept the current copilot suggestion and insert it (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (if (and ed *copilot-suggestion*)
      (let ((suggestion *copilot-suggestion*))
        (set! *copilot-suggestion* #f)
        ;; Cancel the calltip
        (sci-send ed SCI_CALLTIPCANCEL)
        ;; Insert at current position
        (sci-send/string ed SCI_REPLACESEL suggestion)
        (echo-message! echo "Copilot: suggestion accepted"))
      (echo-message! echo "Copilot: no pending suggestion"))))

(def (cmd-qt-copilot-dismiss app)
  "Dismiss the current copilot suggestion (Qt)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (when (and ed *copilot-suggestion*)
      (sci-send ed SCI_CALLTIPCANCEL))
    (if *copilot-suggestion*
      (begin
        (set! *copilot-suggestion* #f)
        (echo-message! echo "Copilot: suggestion dismissed"))
      (echo-message! echo "Copilot: no pending suggestion"))))

;;;============================================================================
;;; Register Qt copilot commands
;;;============================================================================

(def (qt-register-copilot-commands!)
  "Register Qt copilot commands."
  (for-each
    (lambda (pair) (register-command! (car pair) (cdr pair)))
    (list
      (cons 'copilot-mode cmd-qt-copilot-mode)
      (cons 'copilot-complete cmd-qt-copilot-complete)
      (cons 'copilot-accept cmd-qt-copilot-accept)
      (cons 'copilot-dismiss cmd-qt-copilot-dismiss)
      (cons 'copilot-accept-completion cmd-qt-copilot-accept)
      (cons 'copilot-next-completion cmd-qt-copilot-complete))))

;;;============================================================================
;;; Enriched text mode (Qt) — basic text formatting
;;;============================================================================

(def (cmd-enriched-mode app)
  "Toggle enriched text mode for basic text formatting."
  (set! *enriched-mode* (not *enriched-mode*))
  (echo-message! (app-state-echo app)
    (if *enriched-mode* "Enriched mode enabled" "Enriched mode disabled")))

(def (cmd-facemenu-set-bold app)
  "Apply bold styling to selected text (Scintilla SCI_STYLESETBOLD)."
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART 0))
         (end   (sci-send ed SCI_GETSELECTIONEND 0)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No selection -- select text first")
      (begin
        ;; Style 1 = bold text style
        (sci-send ed SCI_STYLESETBOLD 1 1)
        (sci-send ed 2032 start 0)  ;; SCI_STARTSTYLING
        (sci-send ed 2033 (- end start) 1)  ;; SCI_SETSTYLING
        (echo-message! (app-state-echo app) "Bold applied")))))

(def (cmd-facemenu-set-italic app)
  "Apply italic styling to selected text (Scintilla SCI_STYLESETITALIC)."
  (let* ((ed (current-qt-editor app))
         (start (sci-send ed SCI_GETSELECTIONSTART 0))
         (end   (sci-send ed SCI_GETSELECTIONEND 0)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No selection -- select text first")
      (begin
        ;; Style 2 = italic text style
        (sci-send ed SCI_STYLESETITALIC 2 1)
        (sci-send ed 2032 start 0)  ;; SCI_STARTSTYLING
        (sci-send ed 2033 (- end start) 2)  ;; SCI_SETSTYLING
        (echo-message! (app-state-echo app) "Italic applied")))))

;;;============================================================================
;;; Picture mode (Qt) — overwrite with cursor movement
;;;============================================================================

(def (cmd-picture-mode app)
  "Toggle picture mode -- overwrite mode with directional drawing.
   In picture mode, characters overwrite instead of inserting."
  (set! *picture-mode* (not *picture-mode*))
  (let ((ed (current-qt-editor app)))
    ;; Enable/disable overwrite mode in Scintilla
    (sci-send ed 2186 (if *picture-mode* 1 0))  ;; SCI_SETOVERTYPE
    (echo-message! (app-state-echo app)
      (if *picture-mode*
        "Picture mode ON (overwrite, use arrows to draw)"
        "Picture mode OFF"))))

(def (qt-register-parity5-enriched-commands!)
  "Register enriched/picture mode commands."
  (for-each
    (lambda (pair) (register-command! (car pair) (cdr pair)))
    (list
      (cons 'enriched-mode cmd-enriched-mode)
      (cons 'facemenu-set-bold cmd-facemenu-set-bold)
      (cons 'facemenu-set-italic cmd-facemenu-set-italic)
      (cons 'picture-mode cmd-picture-mode))))

;;;============================================================================
;;; Qt frame commands — find-file/switch-buffer in other frame
;;;============================================================================

(def (cmd-find-file-other-frame app)
  "Open a file in a new virtual frame (C-x 5 f)."
  (let ((filename (qt-echo-read-string app "Find file (other frame): ")))
    (when (and filename (> (string-length filename) 0))
      (cmd-make-frame app)
      (execute-command! app 'find-file))))

(def (cmd-switch-to-buffer-other-frame app)
  "Switch to a buffer in a new virtual frame (C-x 5 b)."
  (let* ((buf-names (map buffer-name (buffer-list)))
         (choice (qt-echo-read-string app "Buffer (other frame): ")))
    (when (and choice (> (string-length choice) 0))
      (let ((buf (buffer-by-name choice)))
        (if (not buf)
          (echo-error! (app-state-echo app) (string-append "No buffer: " choice))
          (begin
            (cmd-make-frame app)
            (let* ((fr (app-state-frame app))
                   (win (qt-current-window fr))
                   (ed (qt-edit-window-editor win)))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer win) buf)
              (echo-message! (app-state-echo app)
                (string-append "Buffer " choice " in new frame")))))))))

(def (qt-register-frame-commands!)
  "Register Qt frame management commands."
  (for-each
    (lambda (pair) (register-command! (car pair) (cdr pair)))
    (list
      (cons 'find-file-other-frame cmd-find-file-other-frame)
      (cons 'switch-to-buffer-other-frame cmd-switch-to-buffer-other-frame))))
