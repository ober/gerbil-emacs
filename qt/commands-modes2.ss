;;; -*- Gerbil -*-
;;; Qt commands modes2 - markdown, format buffer, snippet expansion
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gerbil-litehtml/html
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/async
        :gemacs/editor
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/terminal
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
        (only-in :gemacs/org-agenda
                 *org-agenda-files*
                 org-collect-agenda-items org-agenda-item-heading
                 org-agenda-item-type org-agenda-item-date
                 org-agenda-item-time-string org-agenda-item-file)
        (only-in :gemacs/org-parse
                 org-heading-title make-org-timestamp
                 org-timestamp-day)
        (only-in :gemacs/org-table
                 org-table-row? org-table-separator? org-table-parse-row
                 org-table-column-widths org-table-format-row org-table-format-separator
                 org-table-parse-tblfm org-table-eval-formula org-numeric-cell?
                 org-csv-to-table csv-split-line
                 swap-list-elements list-insert list-remove-at filter-map))

;;; ============================================================================
;;; Markdown mode
;;; ============================================================================

(def (md-heading-level line)
  "Return the heading level (1-6) of LINE, or 0 if not a heading."
  (let ((len (string-length line)))
    (if (= len 0) 0
      (let loop ((i 0))
        (cond
          ((>= i len) 0)
          ((>= i 6) 0) ;; max 6 levels
          ((char=? (string-ref line i) #\#) (loop (+ i 1)))
          ((and (> i 0) (char=? (string-ref line i) #\space)) i)
          (else 0))))))

(def (md-get-current-line ed)
  "Get current line text, start position, and end position."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (line-start (let loop ((i pos))
                       (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                         i (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i len) (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1))))))
    (values (substring text line-start line-end) line-start line-end)))

(def (cmd-markdown-promote app)
  "Decrease heading level (remove a #)."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let ((level (md-heading-level line)))
        (if (<= level 1)
          (echo-error! (app-state-echo app) "Cannot promote further")
          (let* ((text (qt-plain-text-edit-text ed))
                 (new-text (string-append
                             (substring text 0 line-start)
                             (substring line 1 (string-length line))
                             (substring text line-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed line-start)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-markdown-demote app)
  "Increase heading level (add a #)."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let ((level (md-heading-level line)))
        (cond
          ((= level 0)
           ;; Not a heading — make it one
           (let* ((text (qt-plain-text-edit-text ed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              "# " line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed (+ line-start 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed)))
          ((>= level 6)
           (echo-error! (app-state-echo app) "Cannot demote further (max level 6)"))
          (else
           (let* ((text (qt-plain-text-edit-text ed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              "#" line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)
             (qt-plain-text-edit-set-cursor-position! ed (+ line-start level 2))
             (qt-plain-text-edit-ensure-cursor-visible! ed))))))))

(def (cmd-markdown-next-heading app)
  "Jump to the next markdown heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Skip to next line first
    (let ((start (let loop ((i pos))
                   (if (or (>= i len) (char=? (string-ref text i) #\newline))
                     (+ i 1) (loop (+ i 1))))))
      (let loop ((i start))
        (cond
          ((>= i len)
           (echo-message! (app-state-echo app) "No more headings"))
          ((and (char=? (string-ref text i) #\#)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (+ i 1))))))))

(def (cmd-markdown-prev-heading app)
  "Jump to the previous markdown heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Go to start of current line, then one more
    (let ((start (let loop ((i pos))
                   (if (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                     (- i 1) (loop (- i 1))))))
      (let loop ((i (max 0 start)))
        (cond
          ((< i 0)
           (echo-message! (app-state-echo app) "No previous heading"))
          ((and (char=? (string-ref text i) #\#)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (- i 1))))))))

(def (cmd-markdown-insert-heading app)
  "Insert a heading at the same level as the current one."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let* ((level (md-heading-level line))
             (prefix (if (> level 0) (string-append (make-string level #\#) " ") "## "))
             (text (qt-plain-text-edit-text ed))
             (insert-text (string-append "\n" prefix))
             (new-text (string-append
                         (substring text 0 line-end)
                         insert-text
                         (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end (string-length insert-text)))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-markdown-toggle-bold app)
  "Toggle bold (**) around the word at point or selection."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection, insert **cursor**
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "****"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 2)))
      ;; Wrap selection in **
      (let* ((selected (substring text sel-start sel-end))
             ;; Check if already bold
             (already-bold (and (>= (string-length selected) 4)
                                (string-prefix? "**" selected)
                                (string-suffix? "**" selected)))
             (replacement (if already-bold
                            (substring selected 2 (- (string-length selected) 2))
                            (string-append "**" selected "**")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-toggle-italic app)
  "Toggle italic (*) around the word at point or selection."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "**"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
      (let* ((selected (substring text sel-start sel-end))
             (already-italic (and (>= (string-length selected) 2)
                                   (string-prefix? "*" selected)
                                   (string-suffix? "*" selected)
                                   (not (string-prefix? "**" selected))))
             (replacement (if already-italic
                            (substring selected 1 (- (string-length selected) 1))
                            (string-append "*" selected "*")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-toggle-code app)
  "Toggle inline code (`) around selection or insert backticks."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (new-text (string-append
                         (substring text 0 pos)
                         "``"
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1)))
      (let* ((selected (substring text sel-start sel-end))
             (already-code (and (>= (string-length selected) 2)
                                 (string-prefix? "`" selected)
                                 (string-suffix? "`" selected)))
             (replacement (if already-code
                            (substring selected 1 (- (string-length selected) 1))
                            (string-append "`" selected "`")))
             (new-text (string-append
                         (substring text 0 sel-start)
                         replacement
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length replacement)))))))

(def (cmd-markdown-insert-link app)
  "Insert a markdown link [text](url)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed))
         (link-text (if (= sel-start sel-end) ""
                      (substring text sel-start sel-end)))
         (url (qt-echo-read-string app "URL: ")))
    (when (> (string-length url) 0)
      (let* ((md-link (string-append "[" (if (string=? link-text "") "link" link-text) "](" url ")"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         md-link
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length md-link)))))))

(def (cmd-markdown-insert-code-block app)
  "Insert a fenced code block."
  (let* ((ed (current-qt-editor app))
         (lang (qt-echo-read-string app "Language (empty for none): "))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (block (string-append "\n```" lang "\n\n```\n"))
         (new-text (string-append
                     (substring text 0 pos)
                     block
                     (substring text pos (string-length text)))))
    (qt-plain-text-edit-set-text! ed new-text)
    ;; Place cursor inside the code block
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 4 (string-length lang) 1))
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-markdown-toggle-checkbox app)
  "Toggle a markdown checkbox [ ] / [x]."
  (let ((ed (current-qt-editor app)))
    (let-values (((line line-start line-end) (md-get-current-line ed)))
      (let* ((text (qt-plain-text-edit-text ed))
             (trimmed (string-trim line))
             (has-unchecked (string-contains trimmed "[ ]"))
             (has-checked (string-contains trimmed "[x]")))
        (cond
          (has-unchecked
           (let* ((idx (string-contains line "[ ]"))
                  (new-line (string-append
                              (substring line 0 idx) "[x]"
                              (substring line (+ idx 3) (string-length line))))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)))
          (has-checked
           (let* ((idx (string-contains line "[x]"))
                  (new-line (string-append
                              (substring line 0 idx) "[ ]"
                              (substring line (+ idx 3) (string-length line))))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text)))
          (else
           ;; Add checkbox prefix
           (let* ((new-line (string-append "- [ ] " trimmed))
                  (new-text (string-append
                              (substring text 0 line-start)
                              new-line
                              (substring text line-end (string-length text)))))
             (qt-plain-text-edit-set-text! ed new-text))))))))

(def (cmd-markdown-outline app)
  "Show an outline of all headings in the current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (lines
           (let loop ((i 0) (acc []))
             (if (>= i len) (reverse acc)
               (let* ((line-end (let lp ((j i))
                                  (if (or (>= j len) (char=? (string-ref text j) #\newline))
                                    j (lp (+ j 1)))))
                      (line (substring text i line-end)))
                 (if (and (> (string-length line) 0)
                          (char=? (string-ref line 0) #\#))
                   (let ((level (md-heading-level line)))
                     (if (> level 0)
                       (loop (+ line-end 1) (cons (cons i line) acc))
                       (loop (+ line-end 1) acc)))
                   (loop (+ line-end 1) acc)))))))
    (if (null? lines)
      (echo-message! (app-state-echo app) "No headings found")
      (let* ((fr (app-state-frame app))
             (outline-buf (or (buffer-by-name "*MD Outline*")
                              (qt-buffer-create! "*MD Outline*" ed #f)))
             (outline-text
               (string-join
                 (map (lambda (entry)
                        (let* ((pos (car entry))
                               (line (cdr entry))
                               (level (md-heading-level line))
                               (indent (make-string (* 2 (- level 1)) #\space)))
                          (string-append indent (number->string pos) ": " line)))
                      lines)
                 "\n")))
        (qt-buffer-attach! ed outline-buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) outline-buf)
        (qt-plain-text-edit-set-text! ed outline-text)
        (qt-text-document-set-modified! (buffer-doc-pointer outline-buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

(def (cmd-markdown-preview app)
  "Generate and display an HTML preview of the current markdown buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (fr (app-state-frame app))
         (preview-buf (or (buffer-by-name "*MD Preview*")
                          (qt-buffer-create! "*MD Preview*" ed #f))))
    ;; Simple markdown to text conversion for preview
    (let* ((lines (string-split text #\newline))
           (rendered
             (string-join
               (map (lambda (line)
                      (let ((level (md-heading-level line)))
                        (cond
                          ;; Heading: underline with = or -
                          ((> level 0)
                           (let* ((heading-text (substring line (+ level 1) (string-length line)))
                                  (underline (make-string (string-length heading-text)
                                                          (if (= level 1) #\= #\-))))
                             (string-append "\n" heading-text "\n" underline)))
                          ;; Horizontal rule
                          ((or (string-prefix? "---" line) (string-prefix? "***" line)
                               (string-prefix? "___" line))
                           (make-string 72 #\-))
                          ;; Code block markers
                          ((string-prefix? "```" line)
                           (string-append "--- " (substring line 3 (string-length line)) " ---"))
                          ;; List items
                          ((string-prefix? "- " line) line)
                          ((string-prefix? "* " line) (string-append "- " (substring line 2 (string-length line))))
                          ;; Quote blocks
                          ((string-prefix? "> " line) (string-append "  | " (substring line 2 (string-length line))))
                          ;; Regular line
                          (else line))))
                    lines)
               "\n")))
      (qt-buffer-attach! ed preview-buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) preview-buf)
      (qt-plain-text-edit-set-text! ed rendered)
      (qt-text-document-set-modified! (buffer-doc-pointer preview-buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "Markdown preview"))))

;;; ============================================================================
;;; Format buffer with external tool

(def *qt-formatters*
  '((scheme "gerbil" "fmt") (python "black" "-") (go "gofmt")
    (javascript "prettier" "--stdin-filepath" "file.js")
    (rust "rustfmt") (c "clang-format") (shell "shfmt" "-")
    (ruby "rubocop" "--auto-correct" "--stdin" "file.rb")
    (json "prettier" "--stdin-filepath" "file.json")
    (css "prettier" "--stdin-filepath" "file.css")
    (xml "xmllint" "--format" "-")
    (yaml "prettier" "--stdin-filepath" "file.yaml")))

(def (cmd-format-buffer app)
  "Format buffer using language-appropriate external formatter."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (ed (current-qt-editor app)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let* ((lang (detect-language path))
             (entry (and lang (assq lang *qt-formatters*))))
        (if (not entry)
          (echo-error! (app-state-echo app)
            (string-append "No formatter for " (or (and lang (symbol->string lang)) "unknown")))
          (let* ((cmd (cadr entry))
                 (args (cddr entry))
                 (text (qt-plain-text-edit-text ed)))
            (with-catch
              (lambda (e) (echo-error! (app-state-echo app) "Format failed"))
              (lambda ()
                (let* ((proc (open-process
                               (list path: cmd arguments: args
                                     stdin-redirection: #t stdout-redirection: #t
                                     stderr-redirection: #f)))
                       (_ (begin (display text proc) (close-output-port proc)))
                       (formatted (read-line proc #f))
                       (status (process-status proc)))
                  (close-port proc)
                  (when (and formatted (> (string-length formatted) 0)
                             (not (string=? formatted text)))
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (qt-plain-text-edit-set-text! ed formatted)
                      (qt-plain-text-edit-set-cursor-position! ed
                        (min pos (string-length formatted)))
                      (echo-message! (app-state-echo app)
                        (string-append "Formatted with " cmd)))))))))))))

(def (cmd-copy-file-name-only app)
  "Copy just the filename (no directory) to kill ring."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-message! (app-state-echo app) "Buffer has no file")
      (let ((name (path-strip-directory path)))
        (qt-kill-ring-push! app name)
        (echo-message! (app-state-echo app) (string-append "Copied: " name))))))

;;; ============================================================================
;;; Swiper / counsel — interactive search wrappers
;;; ============================================================================

(def (cmd-swiper app)
  "Swiper-style interactive search — delegates to occur."
  (cmd-occur app))

(def (cmd-swiper-isearch app)
  "Swiper isearch — delegates to occur (isearch unavailable at this chain level)."
  (cmd-occur app))

(def (cmd-counsel-M-x app)
  "Counsel M-x — delegates to execute-extended-command."
  (cmd-execute-extended-command app))

(def (cmd-counsel-find-file app)
  "Counsel find-file — use M-x find-file or C-x C-f."
  (echo-message! (app-state-echo app) "Use C-x C-f (find-file) to open files"))

(def (cmd-counsel-rg app)
  "Counsel ripgrep — delegates to rgrep."
  (cmd-rgrep app))

(def (cmd-counsel-recentf app)
  "Counsel recent files — delegates to recentf-open."
  (cmd-recentf-open app))

(def (cmd-counsel-bookmark app)
  "Counsel bookmarks — delegates to bookmark-jump."
  (cmd-bookmark-jump app))

(def (cmd-ivy-resume app)
  "Ivy resume — stub, no session to resume."
  (echo-message! (app-state-echo app) "No ivy session to resume"))

;;; ============================================================================
;;; God mode — Ctrl-free command entry
;;; ============================================================================

(def *qt-god-mode* #f)

(def (cmd-god-mode app)
  "Toggle god mode — Ctrl-free command execution."
  (set! *qt-god-mode* (not *qt-god-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-god-mode* "God mode enabled (prefix keys act as C-)" "God mode disabled")))

(def (cmd-god-local-mode app)
  "Toggle god mode in current buffer."
  (cmd-god-mode app))

(def (cmd-god-execute-with-current-bindings app)
  "Execute next key with current bindings (god-mode helper)."
  (echo-message! (app-state-echo app) "Type a key to execute with C- prefix..."))

;;; ============================================================================
;;; Beacon mode — cursor flash
;;; ============================================================================

(def *qt-beacon-mode* #f)

(def (cmd-beacon-mode app)
  "Toggle beacon mode — flash cursor on large movements."
  (set! *qt-beacon-mode* (not *qt-beacon-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-beacon-mode* "Beacon mode enabled" "Beacon mode disabled")))

;;; ============================================================================
;;; Volatile highlights — flash edited regions
;;; ============================================================================

(def *qt-volatile-highlights* #f)

(def (cmd-volatile-highlights-mode app)
  "Toggle volatile highlights — flash edited regions briefly."
  (set! *qt-volatile-highlights* (not *qt-volatile-highlights*))
  (echo-message! (app-state-echo app)
    (if *qt-volatile-highlights* "Volatile highlights enabled" "Volatile highlights disabled")))

;;; ============================================================================
;;; Smartparens — paren auto-pairing
;;; ============================================================================

(def (cmd-smartparens-strict-mode app)
  "Toggle strict smartparens mode — delegates to paredit-strict-mode."
  (cmd-paredit-strict-mode app))

(def (cmd-smartparens-mode app)
  "Toggle smartparens mode — delegates to auto-pair-mode."
  (cmd-toggle-auto-pair-mode app))

;;; ============================================================================
;;; All-the-icons / nerd-icons — icon display
;;; ============================================================================

(def (cmd-all-the-icons-install-fonts app)
  "Install all-the-icons fonts (informational)."
  (echo-message! (app-state-echo app)
    "Icon fonts: use Unicode glyphs. No separate install needed."))

(def (cmd-nerd-icons-install-fonts app)
  "Install nerd-icons fonts (informational)."
  (echo-message! (app-state-echo app)
    "Nerd icons: install a Nerd Font from nerdfonts.com for glyph support."))

;;; ============================================================================
;;; use-package / straight — package config stubs
;;; ============================================================================

(def (cmd-use-package-report app)
  "Show use-package statistics."
  (echo-message! (app-state-echo app)
    "Gemacs: all packages built-in. No external packages to report."))

(def (cmd-straight-use-package app)
  "Configure straight.el package — N/A in gemacs."
  (echo-message! (app-state-echo app)
    "Gemacs uses built-in packages. straight.el not applicable."))

;;; ============================================================================
;;; Which-key enhancements
;;; ============================================================================

(def (cmd-which-key-show-top-level app)
  "Show all top-level key bindings via which-key."
  (echo-message! (app-state-echo app) "Use C-h b to see all key bindings"))

(def (cmd-which-key-show-major-mode app)
  "Show major-mode key bindings via which-key."
  (echo-message! (app-state-echo app) "Use C-h m to see major-mode bindings"))

;;; ============================================================================
;;; Dimmer — dim non-active windows
;;; ============================================================================

(def *qt-dimmer-mode* #f)

(def (cmd-dimmer-mode app)
  "Toggle dimmer mode — dim non-active windows."
  (set! *qt-dimmer-mode* (not *qt-dimmer-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-dimmer-mode* "Dimmer mode enabled" "Dimmer mode disabled")))

;;; ============================================================================
;;; Nyan mode — fun position indicator
;;; ============================================================================

(def *qt-nyan-mode* #f)

(def (cmd-nyan-mode app)
  "Toggle nyan-mode — show nyan cat position indicator in modeline."
  (set! *qt-nyan-mode* (not *qt-nyan-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-nyan-mode* "Nyan mode enabled =^.^=" "Nyan mode disabled")))

;;; ============================================================================
;;; Centered cursor mode
;;; ============================================================================

(def *qt-centered-cursor* #f)

(def (cmd-centered-cursor-mode app)
  "Toggle centered cursor mode — keep cursor vertically centered."
  (set! *qt-centered-cursor* (not *qt-centered-cursor*))
  (echo-message! (app-state-echo app)
    (if *qt-centered-cursor* "Centered cursor mode enabled" "Centered cursor mode disabled")))

;;; ============================================================================
;;; Format-all — external formatter integration
;;; ============================================================================

(def (cmd-format-all-buffer app)
  "Format current buffer using external formatter."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-message! (app-state-echo app) "Buffer has no file — save first")
      (let* ((ext (path-extension path))
             (formatter (cond
                          ((member ext '("py" "pyw")) "black -q -")
                          ((member ext '("js" "jsx" "ts" "tsx")) "prettier --stdin-filepath dummy.js")
                          ((member ext '("go")) "gofmt")
                          ((member ext '("rs")) "rustfmt")
                          ((member ext '("c" "h" "cpp" "hpp" "cc")) "clang-format")
                          ((member ext '("json")) "jq .")
                          ((member ext '("html" "htm" "xml")) "tidy -q -indent")
                          ((member ext '("sh" "bash")) "shfmt -")
                          ((member ext '("rb")) "rubocop -a --stdin dummy.rb 2>/dev/null")
                          ((member ext '("lua")) "lua-format -i --stdin")
                          (else #f))))
        (if (not formatter)
          (echo-message! (app-state-echo app) (string-append "No formatter for ." ext))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (result (with-catch
                           (lambda (e) (cons 'error (error-message e)))
                           (lambda ()
                             (let ((p (open-input-process
                                        (list path: "/bin/sh"
                                              arguments: (list "-c" formatter)
                                              stdin-redirection: #t
                                              stdout-redirection: #t
                                              stderr-redirection: #t))))
                               (display text p)
                               (force-output p)
                               (close-output-port p)
                               (let ((out (read-line p #f)))
                                 (close-input-port p)
                                 (cons 'ok (or out ""))))))))
            (if (eq? (car result) 'error)
              (echo-message! (app-state-echo app) (string-append "Formatter error: " (cdr result)))
              (let ((formatted (cdr result)))
                (when (and (> (string-length formatted) 0)
                           (not (equal? formatted text)))
                  (qt-plain-text-edit-set-text! ed formatted)
                  (echo-message! (app-state-echo app) "Buffer formatted"))))))))))

;;; ============================================================================
;;; Visual regexp — visual feedback during replace
;;; ============================================================================

(def (cmd-visual-regexp-replace app)
  "Visual regexp replace — delegates to query-replace-regexp."
  (cmd-query-replace-regexp app))

(def (cmd-visual-regexp-query-replace app)
  "Visual regexp query replace."
  (cmd-query-replace-regexp app))

;;; ============================================================================
;;; Anzu — search match count indicator
;;; ============================================================================

(def *qt-anzu-mode* #f)

(def (cmd-anzu-mode app)
  "Toggle anzu mode — show search match count."
  (set! *qt-anzu-mode* (not *qt-anzu-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-anzu-mode* "Anzu mode enabled (match counting)" "Anzu mode disabled")))

;;; ============================================================================
;;; Popwin — popup window management
;;; ============================================================================

(def *qt-popwin-mode* #f)

(def (cmd-popwin-mode app)
  "Toggle popwin mode — manage popup windows."
  (set! *qt-popwin-mode* (not *qt-popwin-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-popwin-mode* "Popwin mode enabled" "Popwin mode disabled")))

(def (cmd-popwin-close-popup app)
  "Close the current popup window."
  (cmd-delete-window app))

;;; ============================================================================
;;; Easy-kill — easy copy of various things at point
;;; ============================================================================

(def (cmd-easy-kill app)
  "Easy kill — copy word at point without moving."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (let loop ((start pos))
      (if (and (> start 0)
               (let ((c (string-ref text (- start 1))))
                 (or (char-alphabetic? c) (char-numeric? c) (eqv? c #\_))))
        (loop (- start 1))
        (let loop2 ((end pos))
          (if (and (< end len)
                   (let ((c (string-ref text end)))
                     (or (char-alphabetic? c) (char-numeric? c) (eqv? c #\_))))
            (loop2 (+ end 1))
            (let ((word (substring text start end)))
              (when (> (string-length word) 0)
                (qt-kill-ring-push! app word)
                (echo-message! (app-state-echo app) (string-append "Copied: " word))))))))))

;;; ============================================================================
;;; Crux extras — useful editing commands
;;; ============================================================================

(def (cmd-crux-open-with app)
  "Open current file with external program."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-message! (app-state-echo app) "Buffer has no file")
      (begin
        (with-catch void (lambda () (open-process (list path: "xdg-open" arguments: (list path)))))
        (echo-message! (app-state-echo app) (string-append "Opening with system handler: " path))))))

(def (cmd-crux-duplicate-current-line app)
  "Duplicate the current line."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (line-start (let loop ((i pos)) (if (or (<= i 0) (eqv? (string-ref text (- i 1)) #\newline)) i (loop (- i 1)))))
         (line-end (let loop ((i pos)) (if (or (>= i len) (eqv? (string-ref text i) #\newline)) i (loop (+ i 1)))))
         (line (substring text line-start line-end))
         (new-text (string-append (substring text 0 line-end) "\n" line (substring text line-end len))))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (+ line-end 1 (- pos line-start)))))

(def (cmd-crux-indent-defun app)
  "Indent the current top-level form."
  (echo-message! (app-state-echo app) "Use C-M-\\ (indent-region) to indent forms"))

(def (cmd-crux-swap-windows app)
  "Swap the contents of the two most recent windows."
  (let ((cmd (find-command 'swap-windows)))
    (if cmd (cmd app)
      (echo-message! (app-state-echo app) "Only one window"))))

(def (cmd-crux-cleanup-buffer-or-region app)
  "Clean up buffer: delete trailing whitespace."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (cleaned (map (lambda (line) (string-trim-right line)) lines))
         (result (string-join cleaned "\n")))
    (when (not (equal? text result))
      (qt-plain-text-edit-set-text! ed result))
    (echo-message! (app-state-echo app) "Buffer cleaned up")))

;;; ============================================================================
;;; Selected — act on region commands
;;; ============================================================================

(def *qt-selected-mode* #f)

(def (cmd-selected-mode app)
  "Toggle selected mode — special keybindings when region is active."
  (set! *qt-selected-mode* (not *qt-selected-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-selected-mode* "Selected mode enabled" "Selected mode disabled")))

;;; ============================================================================
;;; Aggressive fill — auto-fill paragraphs as you type
;;; ============================================================================

(def *qt-aggressive-fill* #f)

(def (cmd-aggressive-fill-paragraph-mode app)
  "Toggle aggressive fill paragraph mode — auto-reflow paragraphs."
  (set! *qt-aggressive-fill* (not *qt-aggressive-fill*))
  (echo-message! (app-state-echo app)
    (if *qt-aggressive-fill* "Aggressive fill-paragraph mode enabled" "Aggressive fill-paragraph mode disabled")))

;;; ============================================================================
;;; Snippet/template expansion system
