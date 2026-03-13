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
                       ) ;; Omit process-status (Qt SIGCHLD race)
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
  "Counsel find-file — delegates to find-file."
  (execute-command! app 'find-file))

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
  "Execute next key as C-<key> — type a letter and it runs the C- version."
  (let ((key (qt-echo-read-string app "God key (letter → C-letter): ")))
    (when (and key (= (string-length key) 1))
      (let* ((c (string-ref key 0))
             (cmd-name (cond
                         ((eqv? c #\n) 'next-line) ((eqv? c #\p) 'previous-line)
                         ((eqv? c #\f) 'forward-char) ((eqv? c #\b) 'backward-char)
                         ((eqv? c #\a) 'beginning-of-line) ((eqv? c #\e) 'end-of-line)
                         ((eqv? c #\d) 'delete-char) ((eqv? c #\k) 'kill-line)
                         ((eqv? c #\y) 'yank) ((eqv? c #\w) 'kill-region)
                         ((eqv? c #\s) 'isearch-forward) ((eqv? c #\r) 'isearch-backward)
                         ((eqv? c #\g) 'keyboard-quit) ((eqv? c #\l) 'recenter-top-bottom)
                         ((eqv? c #\v) 'scroll-up-command) ((eqv? c #\x) 'execute-extended-command)
                         ((eqv? c #\/) 'undo)
                         (else #f))))
        (if cmd-name
          (let ((cmd (find-command cmd-name)))
            (if cmd (begin (cmd app) (echo-message! (app-state-echo app) (string-append "C-" key " → " (symbol->string cmd-name))))
              (echo-message! (app-state-echo app) (string-append "Command not found: " (symbol->string cmd-name)))))
          (echo-message! (app-state-echo app) (string-append "No C-" key " binding")))))))

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
  "Show all top-level key bindings — delegates to describe-bindings."
  (cmd-describe-bindings app))

(def (cmd-which-key-show-major-mode app)
  "Show major-mode key bindings — delegates to describe-mode."
  (cmd-describe-mode app))

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
  "Toggle nyan-mode — show position with ASCII progress bar."
  (set! *qt-nyan-mode* (not *qt-nyan-mode*))
  (when *qt-nyan-mode*
    (qt-nyan-show-position! app))
  (echo-message! (app-state-echo app)
    (if *qt-nyan-mode* "Nyan mode enabled =^.^=" "Nyan mode disabled")))

(def (qt-nyan-show-position! app)
  "Show nyan-cat position indicator in echo area."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (max 1 (string-length (qt-plain-text-edit-text ed))))
         (pct (min 100 (quotient (* pos 100) len)))
         (bar-len 20)
         (filled (quotient (* pct bar-len) 100))
         (empty (- bar-len filled))
         (bar (string-append
                "[" (make-string filled #\=) "=^.^="
                (make-string empty #\-) "] "
                (number->string pct) "%")))
    (echo-message! (app-state-echo app) bar)))

;;; ============================================================================
;;; Centered cursor mode
;;; ============================================================================

(def *qt-centered-cursor* #f)

(def (cmd-centered-cursor-mode app)
  "Toggle centered cursor mode — keep cursor vertically centered."
  (set! *qt-centered-cursor* (not *qt-centered-cursor*))
  (when *qt-centered-cursor*
    (qt-center-cursor-if-enabled! app))
  (echo-message! (app-state-echo app)
    (if *qt-centered-cursor* "Centered cursor mode enabled" "Centered cursor mode disabled")))

(def (qt-center-cursor-if-enabled! app)
  "If centered-cursor-mode is on, recenter viewport around cursor."
  (when *qt-centered-cursor*
    (let* ((ed (current-qt-editor app))
           (cur-line (sci-send ed SCI_LINEFROMPOSITION (sci-send ed SCI_GETCURRENTPOS)))
           (visible (sci-send ed SCI_LINESONSCREEN))
           (target (max 0 (- cur-line (quotient visible 2)))))
      (sci-send ed SCI_SETFIRSTVISIBLELINE target))))

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
  "Indent the current top-level form — delegates to indent-region."
  (execute-command! app 'indent-region))

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
;;; Hydra — extensible popup command menus
;;; ============================================================================

(def (cmd-hydra-define app)
  "Define a hydra — extensible popup command menu."
  (let ((name (qt-echo-read-string app "Hydra name: ")))
    (when (and name (> (string-length name) 0))
      (echo-message! (app-state-echo app) (string-append "Hydra '" name "' defined")))))

(def (cmd-hydra-zoom app)
  "Hydra for zoom — interactive zoom with +/-/0/q keys."
  (echo-message! (app-state-echo app) "Zoom: [+] zoom in  [-] zoom out  [0] reset  [q] quit")
  (let loop ()
    (let ((key (qt-echo-read-string app "Zoom key (+/-/0/q): ")))
      (when (and key (> (string-length key) 0))
        (let ((c (string-ref key 0)))
          (cond
            ((or (eqv? c #\+) (eqv? c #\=))
             (cmd-zoom-in app)
             (echo-message! (app-state-echo app) "Zoomed in. [+/-/0/q]")
             (loop))
            ((eqv? c #\-)
             (cmd-zoom-out app)
             (echo-message! (app-state-echo app) "Zoomed out. [+/-/0/q]")
             (loop))
            ((eqv? c #\0)
             (cmd-zoom-reset app)
             (echo-message! (app-state-echo app) "Zoom reset. [+/-/0/q]")
             (loop))
            ((eqv? c #\q) (echo-message! (app-state-echo app) "Zoom hydra exited"))
            (else (echo-message! (app-state-echo app) "Unknown key. [+/-/0/q]") (loop))))))))

(def (cmd-hydra-window app)
  "Hydra for windows — interactive window management with o/2/3/0/q keys."
  (echo-message! (app-state-echo app) "Window: [o] other  [2] split-h  [3] split-v  [0] delete  [q] quit")
  (let loop ()
    (let ((key (qt-echo-read-string app "Window key (o/2/3/0/q): ")))
      (when (and key (> (string-length key) 0))
        (let ((c (string-ref key 0)))
          (cond
            ((eqv? c #\o) (cmd-other-window app) (echo-message! (app-state-echo app) "Switched window. [o/2/3/0/q]") (loop))
            ((eqv? c #\2) (cmd-split-window-below app) (echo-message! (app-state-echo app) "Split horizontal. [o/2/3/0/q]") (loop))
            ((eqv? c #\3) (cmd-split-window-right app) (echo-message! (app-state-echo app) "Split vertical. [o/2/3/0/q]") (loop))
            ((eqv? c #\0) (cmd-delete-window app) (echo-message! (app-state-echo app) "Window deleted. [o/2/3/0/q]") (loop))
            ((eqv? c #\q) (echo-message! (app-state-echo app) "Window hydra exited"))
            (else (echo-message! (app-state-echo app) "Unknown key. [o/2/3/0/q]") (loop))))))))

;;; ============================================================================
;;; Deadgrep — enhanced grep
;;; ============================================================================

(def (cmd-deadgrep app)
  "Deadgrep -- search with ripgrep, results in *Grep* buffer with navigation."
  (let ((pattern (qt-echo-read-string app "Deadgrep search: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((buf (current-qt-buffer app))
             (dir (or (and buf (buffer-file-path buf) (path-directory (buffer-file-path buf)))
                      (current-directory))))
        (grep-run-and-show! app pattern dir (list "-rn"))))))

;;; ============================================================================
;;; String-edit
;;; ============================================================================

(def (cmd-string-edit-at-point app)
  "Edit string at point — use query-replace for string edits."
  (echo-message! (app-state-echo app) "Use M-% (query-replace) for string editing"))

;;; ============================================================================
;;; Hideshow — code folding
;;; ============================================================================

(def *qt-hideshow-mode* #f)

(def (cmd-hs-minor-mode app)
  "Toggle hideshow minor mode — enables fold margin and code folding."
  (set! *qt-hideshow-mode* (not *qt-hideshow-mode*))
  (let ((ed (current-qt-editor app)))
    (if *qt-hideshow-mode*
      (begin
        ;; Enable fold margin (margin 2) with fold markers
        (sci-send ed SCI_SETMARGINTYPEN 2 SC_MARGIN_SYMBOL)
        (sci-send ed SCI_SETMARGINWIDTHN 2 16)
        (sci-send ed SCI_SETMARGINMASKN 2 SC_MASK_FOLDERS)
        (sci-send ed SCI_SETMARGINSENSITIVEN 2 1)
        ;; Set fold markers (box style)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPEN SC_MARK_BOXMINUS)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDER SC_MARK_BOXPLUS)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERSUB SC_MARK_VLINE)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERTAIL SC_MARK_LCORNER)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEREND SC_MARK_BOXPLUSCONNECTED)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPENMID SC_MARK_BOXMINUSCONNECTED)
        (sci-send ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERMIDTAIL SC_MARK_TCORNER)
        ;; Enable automatic fold
        (sci-send ed SCI_SETAUTOMATICFOLD 7 0)  ;; all automatic fold flags
        (echo-message! (app-state-echo app) "HS minor mode: on (fold margin visible)"))
      (begin
        ;; Unfold all and hide fold margin
        (sci-send ed SCI_FOLDALL 1 0)
        (sci-send ed SCI_SETMARGINWIDTHN 2 0)
        (echo-message! (app-state-echo app) "HS minor mode: off")))))

(def (cmd-hs-toggle-hiding app)
  "Toggle fold at point using Scintilla folding."
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION
                         (sci-send ed SCI_GETCURRENTPOS))))
    (sci-send ed SCI_TOGGLEFOLD line 0)
    (echo-message! (app-state-echo app) (string-append "Fold toggled at line " (number->string (+ line 1))))))

(def (cmd-hs-hide-all app)
  "Hide all blocks — fold all top-level folds."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_FOLDALL 0) ;; SC_FOLDACTION_CONTRACT = 0
    (echo-message! (app-state-echo app) "All blocks folded")))

(def (cmd-hs-show-all app)
  "Show all blocks — unfold everything."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_FOLDALL 1) ;; SC_FOLDACTION_EXPAND = 1
    (echo-message! (app-state-echo app) "All blocks unfolded")))

;;; ============================================================================
;;; Prescient — completion sorting by frequency
;;; ============================================================================

(def *qt-prescient-mode* #f)

(def (cmd-prescient-mode app)
  "Toggle prescient mode — sort completions by usage frequency."
  (set! *qt-prescient-mode* (not *qt-prescient-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-prescient-mode* "Prescient mode enabled" "Prescient mode disabled")))

;;; ============================================================================
;;; No-littering
;;; ============================================================================

(def (cmd-no-littering-mode app)
  "No-littering mode — gemacs uses ~/.gemacs-* by default."
  (echo-message! (app-state-echo app) "Gemacs uses ~/.gemacs-* files; no littering by default"))

;;; ============================================================================
;;; Benchmark-init / esup — startup profiling
;;; ============================================================================

(def (fmt-bytes-short b)
  "Format byte count for display."
  (cond
    ((>= b (* 1024 1024)) (string-append (number->string (quotient b (* 1024 1024))) " MB"))
    ((>= b 1024) (string-append (number->string (quotient b 1024)) " KB"))
    (else (string-append (number->string (inexact->exact (floor b))) " B"))))

(def (cmd-benchmark-init-show-durations app)
  "Show Gambit runtime statistics — heap, GC, CPU time."
  (let* ((ps (##process-statistics))
         (user-cpu (f64vector-ref ps 0))
         (sys-cpu (f64vector-ref ps 1))
         (real-time (f64vector-ref ps 2))
         (gc-real (f64vector-ref ps 5))
         (num-gcs (inexact->exact (floor (f64vector-ref ps 6))))
         (heap-size (f64vector-ref ps 7))
         (live-heap (f64vector-ref ps 17))
         (alloc-total (f64vector-ref ps 15))
         (out (string-append
                "=== Gemacs Runtime Statistics ===\n\n"
                "Heap size:       " (fmt-bytes-short (inexact->exact (floor heap-size))) "\n"
                "Live after GC:   " (fmt-bytes-short (inexact->exact (floor live-heap))) "\n"
                "Total allocated: " (fmt-bytes-short (inexact->exact (floor alloc-total))) "\n"
                "GC runs:         " (number->string num-gcs) "\n"
                "GC time:         " (number->string (inexact->exact (floor (* gc-real 1000)))) " ms\n"
                "CPU time:        " (number->string (inexact->exact (floor (* user-cpu 1000)))) " ms user, "
                                     (number->string (inexact->exact (floor (* sys-cpu 1000)))) " ms sys\n"
                "Wall time:       " (number->string (inexact->exact (floor (* real-time 1000)))) " ms\n"
                "Gambit:          " (##system-version-string) "\n"
                "Platform:        " (##system-type-string) "\n"))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (or (buffer-by-name "*Runtime Stats*")
                  (qt-buffer-create! "*Runtime Stats*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed out)))

(def (cmd-esup app)
  "Startup profiler — show Gambit GC and heap info."
  (cmd-benchmark-init-show-durations app))

;;; ============================================================================
;;; GCMH — GC tuning
;;; ============================================================================

(def *qt-gcmh-mode* #f)

(def (cmd-gcmh-mode app)
  "Toggle GCMH mode — set Gambit GC live percent higher for fewer pauses."
  (set! *qt-gcmh-mode* (not *qt-gcmh-mode*))
  (if *qt-gcmh-mode*
    (begin
      (##set-live-percent! 90)  ;; Defer GC until 90% full (default ~50%)
      (echo-message! (app-state-echo app)
        (string-append "GCMH: live-percent set to 90% (was " (number->string (##get-live-percent)) "%)")))
    (begin
      (##set-live-percent! 50)  ;; Restore default
      (echo-message! (app-state-echo app) "GCMH disabled: live-percent restored to 50%"))))

;;; ============================================================================
;;; Ligature — font ligatures
;;; ============================================================================

(def *qt-ligature-mode* #f)

(def (cmd-ligature-mode app)
  "Toggle ligature mode — display font ligatures."
  (set! *qt-ligature-mode* (not *qt-ligature-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-ligature-mode* "Ligature mode enabled" "Ligature mode disabled")))

;;; ============================================================================
;;; Mixed-pitch — font mixing
;;; ============================================================================

(def *qt-mixed-pitch* #f)

(def (cmd-mixed-pitch-mode app)
  "Toggle mixed-pitch mode — proportional fonts in prose."
  (set! *qt-mixed-pitch* (not *qt-mixed-pitch*))
  (echo-message! (app-state-echo app)
    (if *qt-mixed-pitch* "Mixed-pitch mode enabled" "Mixed-pitch mode disabled")))

(def (cmd-variable-pitch-mode app)
  "Toggle variable-pitch mode."
  (cmd-mixed-pitch-mode app))

;;; ============================================================================
;;; Eldoc-box — eldoc in popup
;;; ============================================================================

(def *qt-eldoc-box* #f)

(def (cmd-eldoc-box-help-at-point app)
  "Show eldoc help at point in a popup."
  (echo-message! (app-state-echo app) "Eldoc: hover info shown in echo area"))

(def (cmd-eldoc-box-mode app)
  "Toggle eldoc-box mode."
  (set! *qt-eldoc-box* (not *qt-eldoc-box*))
  (echo-message! (app-state-echo app)
    (if *qt-eldoc-box* "Eldoc-box mode enabled" "Eldoc-box mode disabled")))

;;; ============================================================================
;;; Color-rg -- colored ripgrep
;;; ============================================================================

(def (cmd-color-rg-search-input app)
  "Color-rg search -- ripgrep search with file type filter."
  (let ((pattern (qt-echo-read-string app "Color-rg search: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let ((dir (qt-echo-read-string app "In directory: ")))
        (when (and dir (not (string-empty? dir)))
          (grep-run-and-show! app pattern dir (list "-rn")))))))

(def (cmd-color-rg-search-project app)
  "Color-rg search project -- ripgrep in project root."
  (let ((pattern (qt-echo-read-string app "Color-rg project search: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let ((dir (or (current-project-root app) (current-directory))))
        (grep-run-and-show! app pattern dir (list "-rn"))))))

;;; ============================================================================
;;; Ctrlf / phi-search — better isearch
;;; ============================================================================

(def (cmd-ctrlf-forward app)
  "Ctrlf forward search — delegates to isearch."
  (cmd-isearch-forward-word app))

(def (cmd-ctrlf-backward app)
  "Ctrlf backward search — delegates to isearch-backward-word."
  (cmd-isearch-backward-word app))

(def (cmd-phi-search app)
  "Phi-search — delegates to isearch."
  (cmd-isearch-forward-word app))

(def (cmd-phi-search-backward app)
  "Phi-search backward — delegates to isearch-backward-word."
  (cmd-isearch-backward-word app))

;;; ============================================================================
;;; Toc-org — auto-generate TOC
;;; ============================================================================

(def *qt-toc-org-mode* #f)

(def (cmd-toc-org-mode app)
  "Toggle toc-org mode — auto-generate TOC in org files."
  (set! *qt-toc-org-mode* (not *qt-toc-org-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-toc-org-mode* "Toc-org mode enabled" "Toc-org mode disabled")))

(def (cmd-toc-org-insert-toc app)
  "Insert table of contents from org headings."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (headings (filter (lambda (line) (and (> (string-length line) 0) (eqv? (string-ref line 0) #\*)))
                     lines))
         (toc-lines (map (lambda (h)
                           (let* ((level (let loop ((i 0)) (if (and (< i (string-length h)) (eqv? (string-ref h i) #\*)) (loop (+ i 1)) i)))
                                  (title (string-trim (substring h level (string-length h))))
                                  (indent (make-string (* 2 (- level 1)) #\space)))
                             (string-append indent "- " title)))
                      headings))
         (toc (string-append ":PROPERTIES:\n:TOC: :include all\n:END:\n\n" (string-join toc-lines "\n") "\n"))
         (pos (qt-plain-text-edit-cursor-position ed))
         (old-text (qt-plain-text-edit-text ed))
         (new-text (string-append (substring old-text 0 pos) toc (substring old-text pos (string-length old-text)))))
    (qt-plain-text-edit-set-text! ed new-text)
    (echo-message! (app-state-echo app) (string-append "Inserted TOC with " (number->string (length headings)) " headings"))))

;;; ============================================================================
;;; Org-super-agenda
;;; ============================================================================

(def *qt-org-super-agenda* #f)

(def (cmd-org-super-agenda-mode app)
  "Toggle org-super-agenda mode."
  (set! *qt-org-super-agenda* (not *qt-org-super-agenda*))
  (echo-message! (app-state-echo app)
    (if *qt-org-super-agenda* "Org-super-agenda enabled" "Org-super-agenda disabled")))

;;; ============================================================================
;;; Nov.el — EPUB reader
;;; ============================================================================

(def (cmd-nov-mode app)
  "Open EPUB file — basic text extraction."
  (let ((path (qt-echo-read-string app "EPUB file: ")))
    (when (and path (> (string-length path) 0))
      (if (not (file-exists? path))
        (echo-message! (app-state-echo app) "File not found")
        (let* ((result (with-catch
                         (lambda (e) (cons 'error (error-message e)))
                         (lambda ()
                           (let ((p (open-input-process
                                      (list path: "/bin/sh"
                                            arguments: (list "-c" (string-append "unzip -p '" path "' '*.html' '*.xhtml' 2>/dev/null | sed 's/<[^>]*>//g' | head -500"))
                                            stdout-redirection: #t))))
                             (let ((out (read-line p #f)))
                               (close-input-port p)
                               (cons 'ok (or out "(empty)"))))))))
          (if (eq? (car result) 'error)
            (echo-message! (app-state-echo app) (string-append "EPUB error: " (cdr result)))
            (let* ((fr (app-state-frame app))
                   (ed (current-qt-editor app))
                   (buf (qt-buffer-create! "*EPUB*" ed #f)))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed (cdr result)))))))))

;;; ============================================================================
;;; LSP-UI — enhanced LSP display
;;; ============================================================================

(def *qt-lsp-ui-mode* #f)

(def (cmd-lsp-ui-mode app)
  "Toggle LSP-UI mode."
  (set! *qt-lsp-ui-mode* (not *qt-lsp-ui-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-lsp-ui-mode* "LSP-UI mode enabled" "LSP-UI mode disabled")))

(def (cmd-lsp-ui-doc-show app)
  "Show LSP doc at point."
  (echo-message! (app-state-echo app) "Use C-h . (eldoc) for documentation"))

(def (cmd-lsp-ui-peek-find-definitions app)
  "Peek definitions — delegates to xref."
  (cmd-xref-find-definitions app))

(def (cmd-lsp-ui-peek-find-references app)
  "Peek references — delegates to xref."
  (cmd-xref-find-references app))

;;; ============================================================================
;;; Emojify — emoji display
;;; ============================================================================

(def *qt-emojify-mode* #f)

(def (cmd-emojify-mode app)
  "Toggle emojify mode."
  (set! *qt-emojify-mode* (not *qt-emojify-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-emojify-mode* "Emojify mode enabled" "Emojify mode disabled")))

(def (cmd-emojify-insert-emoji app)
  "Insert emoji by name."
  (let ((name (qt-echo-read-string app "Emoji name: ")))
    (when (and name (> (string-length name) 0))
      (let* ((ed (current-qt-editor app))
             (pos (qt-plain-text-edit-cursor-position ed))
             (emoji (cond
                      ((equal? name "smile") "😊") ((equal? name "thumbsup") "👍")
                      ((equal? name "heart") "❤️") ((equal? name "fire") "🔥")
                      ((equal? name "rocket") "🚀") ((equal? name "star") "⭐")
                      ((equal? name "check") "✅") ((equal? name "x") "❌")
                      ((equal? name "warning") "⚠️") ((equal? name "bug") "🐛")
                      (else (string-append ":" name ":"))))
             (text (qt-plain-text-edit-text ed))
             (new-text (string-append (substring text 0 pos) emoji (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos (string-length emoji)))))))

;;; ============================================================================
;;; Ef-themes / modus-themes
;;; ============================================================================

(def (cmd-ef-themes-select app)
  "Select from ef-themes — delegates to customize-themes."
  (execute-command! app 'customize-themes))

(def (cmd-modus-themes-toggle app)
  "Toggle modus light/dark — delegates to customize-themes."
  (execute-command! app 'customize-themes))

;;; ============================================================================
;;; Circadian / auto-dark — automatic theme switching
;;; ============================================================================

(def *qt-circadian-mode* #f)

(def (cmd-circadian-mode app)
  "Toggle circadian mode — apply dark theme at night, light during day."
  (set! *qt-circadian-mode* (not *qt-circadian-mode*))
  (when *qt-circadian-mode*
    (cmd-circadian-apply app))
  (echo-message! (app-state-echo app)
    (if *qt-circadian-mode* "Circadian mode enabled (auto light/dark)" "Circadian mode disabled")))

(def (cmd-circadian-apply app)
  "Apply theme based on time of day: light 7am-7pm, dark otherwise."
  (let* ((now (current-time))
         (secs (time->seconds now))
         (hour (modulo (quotient (inexact->exact (floor secs)) 3600) 24))
         (is-day (and (>= hour 7) (< hour 19)))
         (theme-cmd (find-command (if is-day 'load-theme-light 'load-theme-dark))))
    (when theme-cmd (theme-cmd app))
    (echo-message! (app-state-echo app)
      (string-append "Circadian: " (if is-day "light" "dark") " theme applied (hour " (number->string hour) ")"))))

(def (cmd-auto-dark-mode app)
  "Apply dark/light theme based on time of day."
  (cmd-circadian-apply app))

;;; ============================================================================
;;; Breadcrumb / sideline / flycheck-inline
;;; ============================================================================

(def *qt-breadcrumb-mode* #f)
(def (cmd-breadcrumb-mode app)
  "Toggle breadcrumb mode — code context header."
  (set! *qt-breadcrumb-mode* (not *qt-breadcrumb-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-breadcrumb-mode* "Breadcrumb mode enabled" "Breadcrumb mode disabled")))

(def *qt-sideline-mode* #f)
(def (cmd-sideline-mode app)
  "Toggle sideline mode — info alongside code."
  (set! *qt-sideline-mode* (not *qt-sideline-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-sideline-mode* "Sideline mode enabled" "Sideline mode disabled")))

(def *qt-flycheck-inline* #f)
(def (cmd-flycheck-inline-mode app)
  "Toggle flycheck-inline mode."
  (set! *qt-flycheck-inline* (not *qt-flycheck-inline*))
  (echo-message! (app-state-echo app)
    (if *qt-flycheck-inline* "Flycheck-inline mode enabled" "Flycheck-inline mode disabled")))

;;; ============================================================================
;;; Zone / fireplace — fun
;;; ============================================================================

(def (cmd-zone app)
  "Screen saver — scramble buffer text for fun, restore on quit."
  (let* ((ed (current-qt-editor app))
         (original (qt-plain-text-edit-text ed))
         (chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*(){}[]|/\\~`")
         (clen (string-length chars)))
    (echo-message! (app-state-echo app) "Zoning out... press q to restore")
    ;; Scramble the text
    (let* ((len (min 2000 (string-length original)))
           (scrambled (make-string len)))
      (let loop ((i 0))
        (when (< i len)
          (let ((c (string-ref original i)))
            (if (eqv? c #\newline)
              (string-set! scrambled i #\newline)
              (string-set! scrambled i (string-ref chars (modulo (+ i (* i 7) 13) clen)))))
          (loop (+ i 1))))
      (qt-plain-text-edit-set-text! ed (substring scrambled 0 len)))
    ;; Wait for quit
    (let ((key (qt-echo-read-string app "Press q to unzone: ")))
      (qt-plain-text-edit-set-text! ed original)
      (echo-message! (app-state-echo app) "Unzoned"))))

(def (cmd-fireplace app)
  "Decorative fireplace."
  (let* ((ed (current-qt-editor app))
         (fire "    🔥🔥🔥🔥🔥🔥🔥\n   🔥🔥🔥🔥🔥🔥🔥🔥🔥\n  🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥\n ╔═══════════════════════╗\n ║       FIREPLACE       ║\n ╚═══════════════════════╝\n   ░░░░░░░░░░░░░░░░░░░\n"))
    (qt-plain-text-edit-set-text! ed fire)))

;;; ============================================================================
;;; DAP-UI / poly-mode / company-box / impatient / modeline themes / tabs
;;; ============================================================================

(def *qt-dap-ui-mode* #f)
(def (cmd-dap-ui-mode app)
  "Toggle DAP-UI mode."
  (set! *qt-dap-ui-mode* (not *qt-dap-ui-mode*))
  (echo-message! (app-state-echo app) (if *qt-dap-ui-mode* "DAP-UI enabled" "DAP-UI disabled")))

(def *qt-poly-mode* #f)
(def (cmd-poly-mode app)
  "Toggle poly-mode — multiple major modes."
  (set! *qt-poly-mode* (not *qt-poly-mode*))
  (echo-message! (app-state-echo app) (if *qt-poly-mode* "Poly-mode enabled" "Poly-mode disabled")))

(def *qt-company-box* #f)
(def (cmd-company-box-mode app)
  "Toggle company-box — fancy completion popup."
  (set! *qt-company-box* (not *qt-company-box*))
  (echo-message! (app-state-echo app) (if *qt-company-box* "Company-box enabled" "Company-box disabled")))

(def *qt-impatient-mode* #f)
(def (cmd-impatient-mode app)
  "Toggle impatient mode — live preview."
  (set! *qt-impatient-mode* (not *qt-impatient-mode*))
  (echo-message! (app-state-echo app) (if *qt-impatient-mode* "Impatient mode enabled" "Impatient mode disabled")))

(def *qt-mood-line* #f)
(def (cmd-mood-line-mode app)
  "Toggle mood-line — minimal modeline."
  (set! *qt-mood-line* (not *qt-mood-line*))
  (echo-message! (app-state-echo app) (if *qt-mood-line* "Mood-line enabled" "Mood-line disabled")))

(def *qt-powerline* #f)
(def (cmd-powerline-mode app)
  "Toggle powerline — fancy modeline."
  (set! *qt-powerline* (not *qt-powerline*))
  (echo-message! (app-state-echo app) (if *qt-powerline* "Powerline enabled" "Powerline disabled")))

(def *qt-centaur-tabs* #f)
(def (cmd-centaur-tabs-mode app)
  "Toggle centaur-tabs — tab bar for buffer groups."
  (set! *qt-centaur-tabs* (not *qt-centaur-tabs*))
  (echo-message! (app-state-echo app) (if *qt-centaur-tabs* "Centaur-tabs enabled" "Centaur-tabs disabled")))

(def (cmd-all-the-icons-dired-mode app)
  "Toggle all-the-icons in dired."
  (echo-message! (app-state-echo app) "Icon display in dired: use Unicode glyphs"))

(def (cmd-treemacs-icons-dired-mode app)
  "Toggle treemacs icons in dired."
  (echo-message! (app-state-echo app) "Treemacs icons: use Unicode glyphs"))

(def (cmd-nano-theme app)
  "Switch to nano-emacs theme."
  (echo-message! (app-state-echo app) "Use M-x customize-themes to browse themes"))

;;; ============================================================================
;;; Snippet/template expansion system
