;;; -*- Gerbil -*-
;;; Xref, ibuffer, which-key, markdown, flycheck, treemacs,
;;; magit, abbrev, and hippie expand commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/highlight
        :gerbil-emacs/editor-extra-helpers)

;; --- Task #47: xref, ibuffer, which-key, markdown, auto-insert, and more ---

;; Xref cross-reference navigation using grep
;; History stack for navigation

(def *xref-history* '())     ; list of (file line col) for back navigation
(def *xref-forward* '())     ; list of (file line col) for forward navigation

(def (xref-push-location! app)
  "Save current location to xref history."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (ed (edit-window-editor win))
         (file (and buf (buffer-file-path buf)))
         (line (editor-line-from-position ed (editor-get-current-pos ed)))
         (col 0))
    (when file
      (set! *xref-history* (cons (list file line col) *xref-history*))
      (set! *xref-forward* '()))))

(def (xref-get-symbol-at-point app)
  "Get the symbol at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (let-values (((start end) (word-bounds-at ed (editor-get-current-pos ed))))
      (if start
        (let ((text (editor-get-text ed)))
          (substring text start end))
        #f))))

(def (xref-grep-for-pattern pattern dir definition?)
  "Search for pattern using grep. Returns list of (file line text)."
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let* ((grep-pattern (if definition?
                             ;; Look for definition patterns
                             (string-append "(def[a-z]*\\s+" pattern "\\b|"
                                           pattern "\\s*[=:]|"
                                           "function\\s+" pattern "\\b|"
                                           "class\\s+" pattern "\\b)")
                             ;; Look for any occurrence
                             (string-append "\\b" pattern "\\b")))
             (proc (open-process
                     (list path: "grep"
                           arguments: (list "-rn" "-E" grep-pattern dir
                                           "--include=*.ss" "--include=*.scm"
                                           "--include=*.py" "--include=*.js"
                                           "--include=*.go" "--include=*.rs"
                                           "--include=*.c" "--include=*.h"
                                           "--include=*.cpp" "--include=*.hpp")
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f)))
        (process-status proc)
        (if (not output)
          '()
          (let ((lines (string-split output #\newline)))
            (filter-map
              (lambda (line)
                (let ((parts (string-split line #\:)))
                  (if (>= (length parts) 3)
                    (let ((file (car parts))
                          (line-num (string->number (cadr parts)))
                          (text (string-join (cddr parts) ":")))
                      (and line-num (list file line-num (string-trim text))))
                    #f)))
              lines)))))))

(def (xref-show-results app results title symbol)
  "Show xref results in a buffer."
  (if (null? results)
    (echo-message! (app-state-echo app) (string-append "No results for: " symbol))
    (if (= (length results) 1)
      ;; Single result - jump directly
      (let* ((result (car results))
             (file (car result))
             (line (cadr result)))
        (xref-push-location! app)
        (xref-goto-location app file line)
        (echo-message! (app-state-echo app) (string-append "Found: " symbol)))
      ;; Multiple results - show in buffer
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (buf (buffer-create! (string-append "*xref: " symbol "*") ed))
             (text (string-append title "\n\n"
                     (string-join
                       (map (lambda (r)
                              (string-append (car r) ":" (number->string (cadr r)) ": " (caddr r)))
                            results)
                       "\n")
                     "\n\nPress Enter on a line to jump to that location.")))
        (xref-push-location! app)
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed text)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)))))

(def (xref-goto-location app file line)
  "Jump to a file and line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    ;; Open the file
    (when (file-exists? file)
      (let* ((name (path-strip-directory file))
             (buf (or (buffer-by-name name)
                      (buffer-create! name ed file)))
             (text (call-with-input-file file (lambda (p) (read-line p #f)))))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (set! (buffer-file-path buf) file)
        (editor-set-text ed (or text ""))
        (editor-goto-line ed line)))))

(def (cmd-xref-find-definitions app)
  "Find definitions of symbol at point using grep."
  (let ((symbol (xref-get-symbol-at-point app))
        (echo (app-state-echo app)))
    (if (not symbol)
      (echo-message! echo "No symbol at point")
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (buf (edit-window-buffer win))
             (file (and buf (buffer-file-path buf)))
             (dir (if file (path-directory file) (current-directory)))
             (results (xref-grep-for-pattern symbol dir #t)))
        (xref-show-results app results
          (string-append "Definitions of: " symbol) symbol)))))

(def (cmd-xref-find-references app)
  "Find references to symbol at point using grep."
  (let ((symbol (xref-get-symbol-at-point app))
        (echo (app-state-echo app)))
    (if (not symbol)
      (echo-message! echo "No symbol at point")
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (buf (edit-window-buffer win))
             (file (and buf (buffer-file-path buf)))
             (dir (if file (path-directory file) (current-directory)))
             (results (xref-grep-for-pattern symbol dir #f)))
        (xref-show-results app results
          (string-append "References to: " symbol) symbol)))))

(def (cmd-xref-find-apropos app)
  "Find symbols matching prompted pattern."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Find symbol matching: " row width)))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((win (current-window fr))
             (buf (edit-window-buffer win))
             (file (and buf (buffer-file-path buf)))
             (dir (if file (path-directory file) (current-directory)))
             (results (xref-grep-for-pattern pattern dir #f)))
        (xref-show-results app results
          (string-append "Symbols matching: " pattern) pattern)))))

(def (cmd-xref-go-back app)
  "Go back to previous xref location."
  (let ((echo (app-state-echo app)))
    (if (null? *xref-history*)
      (echo-message! echo "No xref history")
      (let* ((loc (car *xref-history*))
             (file (car loc))
             (line (cadr loc)))
        ;; Save current position for forward
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (buf (edit-window-buffer win))
               (ed (edit-window-editor win))
               (cur-file (and buf (buffer-file-path buf)))
               (cur-line (editor-line-from-position ed (editor-get-current-pos ed))))
          (when cur-file
            (set! *xref-forward* (cons (list cur-file cur-line 0) *xref-forward*))))
        (set! *xref-history* (cdr *xref-history*))
        (xref-goto-location app file line)
        (echo-message! echo "Xref: back")))))

(def (cmd-xref-go-forward app)
  "Go forward in xref history."
  (let ((echo (app-state-echo app)))
    (if (null? *xref-forward*)
      (echo-message! echo "No forward xref history")
      (let* ((loc (car *xref-forward*))
             (file (car loc))
             (line (cadr loc)))
        (xref-push-location! app)
        (set! *xref-forward* (cdr *xref-forward*))
        (xref-goto-location app file line)
        (echo-message! echo "Xref: forward")))))

;; Ibuffer - advanced buffer management
(def (cmd-ibuffer app)
  "Open ibuffer - advanced buffer management."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (bufs (buffer-list))
         (lines (map (lambda (b)
                       (string-append
                         (if (buffer-modified b) "* " "  ")
                         (buffer-name b)
                         (if (buffer-file-path b)
                           (string-append "  " (buffer-file-path b))
                           "")))
                     bufs))
         (text (string-join lines "\n"))
         (buf (buffer-create! "*Ibuffer*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Ibuffer\n\n  MR  Buffer              File\n  --  ------              ----\n" text "\n"))
    (editor-set-read-only ed #t)))

(def (cmd-ibuffer-mark app)
  "Mark current buffer line in ibuffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num)))
    ;; Mark by inserting > at start of line
    (send-message ed SCI_SETTARGETSTART line-start 0)
    (send-message ed SCI_SETTARGETEND (+ line-start 1) 0)
    (send-message/string ed SCI_REPLACETARGET ">")
    ;; Move to next line
    (editor-goto-pos ed (editor-position-from-line ed (+ line-num 1)))
    (echo-message! (app-state-echo app) "Marked")))

(def (cmd-ibuffer-delete app)
  "Flag buffer for deletion in ibuffer (mark with D)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num)))
    (send-message ed SCI_SETTARGETSTART line-start 0)
    (send-message ed SCI_SETTARGETEND (+ line-start 1) 0)
    (send-message/string ed SCI_REPLACETARGET "D")
    (editor-goto-pos ed (editor-position-from-line ed (+ line-num 1)))
    (echo-message! (app-state-echo app) "Flagged for deletion")))

(def (cmd-ibuffer-do-kill app)
  "Execute flagged operations in ibuffer (kill D-flagged buffers)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (killed 0))
    ;; Find lines starting with D and extract buffer name
    (for-each
      (lambda (line)
        (when (and (> (string-length line) 2) (char=? (string-ref line 0) #\D))
          (let* ((trimmed (string-trim (substring line 1 (string-length line))))
                 (name (let ((sp (string-index trimmed #\space)))
                         (if sp (substring trimmed 0 sp) trimmed)))
                 (buf (buffer-by-name name)))
            (when buf
              (buffer-list-remove! buf)
              (set! killed (+ killed 1))))))
      lines)
    (echo-message! (app-state-echo app)
      (string-append "Killed " (number->string killed) " buffer(s)"))))

;; Which-key - display available keybindings
(def (cmd-which-key app)
  "Display available keybindings for the global keymap."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (entries (keymap-entries *global-keymap*))
         (lines (map (lambda (e)
                       (string-append "  " (car e) " -> "
                         (cond
                           ((symbol? (cdr e)) (symbol->string (cdr e)))
                           ((hash-table? (cdr e)) "<prefix-map>")
                           (else "???"))))
                     (sort entries (lambda (a b) (string<? (car a) (car b))))))
         (buf (buffer-create! "*Which Key*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Key Bindings\n\n"
                          (string-join lines "\n") "\n"))
    (editor-goto-pos ed 0)
    (editor-set-read-only ed #t)))

;; Markdown mode
(def (cmd-markdown-mode app)
  "Toggle markdown mode — set lexer for markdown files."
  (let* ((on (toggle-mode! 'markdown-mode))
         (fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    (when (and buf on)
      (set! (buffer-lexer-lang buf) "markdown"))
    (echo-message! (app-state-echo app)
      (if on "Markdown mode enabled" "Markdown mode disabled"))))

(def (cmd-markdown-preview app)
  "Preview markdown as rendered text using pandoc or basic conversion."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    ;; Try pandoc, fall back to basic rendering
    (let ((rendered (with-exception-catcher
                      (lambda (e) #f)
                      (lambda ()
                        (let ((proc (open-process
                                      (list path: "pandoc"
                                            arguments: '("-t" "plain")
                                            stdin-redirection: #t stdout-redirection: #t
                                            stderr-redirection: #f))))
                          (display text proc)
                          (close-output-port proc)
                          (let ((out (read-line proc #f)))
                            (process-status proc)
                            out))))))
      (let* ((preview-text (or rendered text))
             (buf (buffer-create! "*Markdown Preview*" ed)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed preview-text)
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)
        (echo-message! echo (if rendered "Preview (via pandoc)" "Preview (raw)"))))))

(def (cmd-markdown-insert-header app)
  "Insert markdown header."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "# ")))

(def (cmd-markdown-insert-bold app)
  "Insert markdown bold markers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (begin
        (editor-insert-text ed (editor-get-current-pos ed) "****")
        (editor-goto-pos ed (+ (editor-get-current-pos ed) -2)))
      (begin
        (editor-insert-text ed end "**")
        (editor-insert-text ed start "**")))))

(def (cmd-markdown-insert-italic app)
  "Insert markdown italic markers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (begin
        (editor-insert-text ed (editor-get-current-pos ed) "**")
        (editor-goto-pos ed (+ (editor-get-current-pos ed) -1)))
      (begin
        (editor-insert-text ed end "*")
        (editor-insert-text ed start "*")))))

(def (cmd-markdown-insert-code app)
  "Insert markdown code markers."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (start (editor-get-selection-start ed))
         (end (editor-get-selection-end ed)))
    (if (= start end)
      (begin
        (editor-insert-text ed (editor-get-current-pos ed) "``")
        (editor-goto-pos ed (+ (editor-get-current-pos ed) -1)))
      (begin
        (editor-insert-text ed end "`")
        (editor-insert-text ed start "`")))))

(def (cmd-markdown-insert-link app)
  "Insert markdown link template."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "[text](url)")))

(def (cmd-markdown-insert-image app)
  "Insert markdown image template."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "![alt](url)")))

(def (cmd-markdown-insert-code-block app)
  "Insert markdown fenced code block."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "```\n\n```")))

(def (cmd-markdown-insert-list-item app)
  "Insert markdown list item."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (editor-insert-text ed (editor-get-current-pos ed) "- ")))

;; Auto-insert templates
;; Templates for common file types

(def *auto-insert-enabled* #t)

(def (auto-insert-get-template ext filename)
  "Get template content for a file extension."
  (let ((base (path-strip-extension filename)))
    (cond
      ((member ext '(".ss" ".scm"))
       (string-append ";;; -*- Gerbil -*-\n"
                      ";;; " filename "\n"
                      ";;;\n\n"
                      "(export )\n\n"
                      "(import :std/sugar)\n\n"
                      ";;;============================================================================\n\n"))
      ((member ext '(".py"))
       (string-append "#!/usr/bin/env python3\n"
                      "\"\"\"" filename "\n\n"
                      "Description here.\n"
                      "\"\"\"\n\n"
                      "def main():\n"
                      "    pass\n\n"
                      "if __name__ == '__main__':\n"
                      "    main()\n"))
      ((member ext '(".sh" ".bash"))
       (string-append "#!/bin/bash\n"
                      "# " filename "\n"
                      "# Description here.\n\n"
                      "set -euo pipefail\n\n"))
      ((member ext '(".c"))
       (string-append "/*\n"
                      " * " filename "\n"
                      " * Description here.\n"
                      " */\n\n"
                      "#include <stdio.h>\n"
                      "#include <stdlib.h>\n\n"
                      "int main(int argc, char *argv[]) {\n"
                      "    return 0;\n"
                      "}\n"))
      ((member ext '(".h"))
       (let ((guard (string-append (string-upcase base) "_H")))
         (string-append "#ifndef " guard "\n"
                        "#define " guard "\n\n"
                        "/* " filename " */\n\n"
                        "#endif /* " guard " */\n")))
      ((member ext '(".go"))
       (string-append "// " filename "\n"
                      "package main\n\n"
                      "func main() {\n"
                      "}\n"))
      ((member ext '(".rs"))
       (string-append "// " filename "\n\n"
                      "fn main() {\n"
                      "    println!(\"Hello, world!\");\n"
                      "}\n"))
      ((member ext '(".js" ".mjs"))
       (string-append "// " filename "\n"
                      "'use strict';\n\n"
                      "function main() {\n"
                      "}\n\n"
                      "main();\n"))
      ((member ext '(".html"))
       (string-append "<!DOCTYPE html>\n"
                      "<html lang=\"en\">\n"
                      "<head>\n"
                      "    <meta charset=\"UTF-8\">\n"
                      "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
                      "    <title>" base "</title>\n"
                      "</head>\n"
                      "<body>\n"
                      "    \n"
                      "</body>\n"
                      "</html>\n"))
      ((member ext '(".css"))
       (string-append "/* " filename " */\n\n"
                      "* {\n"
                      "    box-sizing: border-box;\n"
                      "}\n\n"
                      "body {\n"
                      "    margin: 0;\n"
                      "    padding: 0;\n"
                      "}\n"))
      ((member ext '(".md"))
       (string-append "# " base "\n\n"
                      "## Overview\n\n"
                      "Description here.\n\n"
                      "## Usage\n\n"
                      "```\n"
                      "example\n"
                      "```\n"))
      ((member ext '(".json"))
       "{\n}\n")
      ((member ext '(".yaml" ".yml"))
       (string-append "# " filename "\n---\n\n"))
      (else #f))))

(def (cmd-auto-insert app)
  "Insert file template based on file extension."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file (and buf (buffer-file-path buf)))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (if (not file)
      (echo-message! echo "Buffer has no associated file")
      (let* ((ext (path-extension file))
             (filename (path-strip-directory file))
             (template (auto-insert-get-template ext filename)))
        (if (not template)
          (echo-message! echo (string-append "No template for " ext " files"))
          (begin
            (editor-set-text ed template)
            (editor-goto-pos ed (string-length template))
            (echo-message! echo (string-append "Inserted template for " ext))))))))

(def (cmd-auto-insert-mode app)
  "Toggle auto-insert mode."
  (set! *auto-insert-enabled* (not *auto-insert-enabled*))
  (echo-message! (app-state-echo app)
    (if *auto-insert-enabled* "Auto-insert enabled" "Auto-insert disabled")))

;; Text scale (font size)
(def (cmd-text-scale-increase app)
  "Increase text scale."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETZOOM 0 0)))
    (send-message ed SCI_SETZOOM (+ cur 1) 0)
    (echo-message! (app-state-echo app)
      (string-append "Zoom: " (number->string (+ cur 1))))))

(def (cmd-text-scale-decrease app)
  "Decrease text scale."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cur (send-message ed SCI_GETZOOM 0 0)))
    (send-message ed SCI_SETZOOM (- cur 1) 0)
    (echo-message! (app-state-echo app)
      (string-append "Zoom: " (number->string (- cur 1))))))

(def (cmd-text-scale-reset app)
  "Reset text scale to default."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_SETZOOM 0 0)
    (echo-message! (app-state-echo app) "Zoom: 0 (default)")))

;; Browse kill ring
(def (cmd-browse-kill-ring app)
  "Display kill ring contents."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (kr (app-state-kill-ring app))
         (entries (let loop ((items kr) (i 0) (acc []))
                    (if (or (null? items) (>= i 20))
                      (reverse acc)
                      (let ((entry (car items)))
                        (loop (cdr items) (+ i 1)
                              (cons (string-append
                                      (number->string i) ": "
                                      (if (> (string-length entry) 60)
                                        (string-append (substring entry 0 60) "...")
                                        entry))
                                    acc))))))
         (text (if (null? entries) "(empty)"
                 (string-join entries "\n")))
         (buf (buffer-create! "*Kill Ring*" ed)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Kill Ring\n\n" text "\n"))
    (editor-set-read-only ed #t)))

;; Flycheck / syntax checking
;; Uses external linters based on file extension

(def *flycheck-errors* (make-hash-table)) ; buffer-name -> list of (line col message)
(def *flycheck-error-idx* (make-hash-table)) ; buffer-name -> current error index

(def (flycheck-get-linter file-path)
  "Get linter command and args for a file based on extension."
  (let ((ext (path-extension file-path)))
    (cond
      ((member ext '(".py")) 
       '("python3" "-m" "py_compile"))
      ((member ext '(".js" ".mjs"))
       '("node" "--check"))
      ((member ext '(".sh" ".bash"))
       '("bash" "-n"))
      ((member ext '(".rb"))
       '("ruby" "-c"))
      ((member ext '(".pl" ".pm"))
       '("perl" "-c"))
      ((member ext '(".go"))
       '("gofmt" "-e"))
      ((member ext '(".rs"))
       '("rustfmt" "--check"))
      ((member ext '(".c" ".h"))
       '("gcc" "-fsyntax-only" "-Wall"))
      ((member ext '(".cpp" ".hpp" ".cc" ".cxx"))
       '("g++" "-fsyntax-only" "-Wall"))
      ((member ext '(".json"))
       '("python3" "-m" "json.tool"))
      ((member ext '(".yaml" ".yml"))
       '("python3" "-c" "import yaml,sys; yaml.safe_load(open(sys.argv[1]))"))
      ((member ext '(".xml"))
       '("xmllint" "--noout"))
      (else #f))))

(def (flycheck-parse-errors output file-path)
  "Parse linter output into list of (line col message)."
  (let ((lines (string-split output #\newline))
        (errors '()))
    (for-each
      (lambda (line)
        (when (and (> (string-length line) 0)
                   (or (string-contains line "error")
                       (string-contains line "Error")
                       (string-contains line "warning")
                       (string-contains line "Warning")
                       (string-contains line "line ")
                       (string-contains line ":")))
          ;; Try to extract line number - common format: file:line:col: message
          (let* ((parts (string-split line #\:))
                 (line-num (if (>= (length parts) 2)
                             (string->number (string-trim (cadr parts)))
                             #f))
                 (col-num (if (>= (length parts) 3)
                            (string->number (string-trim (caddr parts)))
                            1))
                 (msg (string-trim line)))
            (when (and line-num (> line-num 0))
              (set! errors (cons (list line-num (or col-num 1) msg) errors))))))
      lines)
    (reverse errors)))

(def (flycheck-run-linter! app)
  "Run the linter for the current buffer and store errors."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file-path (and buf (buffer-file-path buf)))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not file-path)
      (echo-error! echo "No file associated with buffer")
      (let ((linter-cmd (flycheck-get-linter file-path)))
        (if (not linter-cmd)
          (echo-message! echo (string-append "No linter for " (path-extension file-path)))
          (with-exception-catcher
            (lambda (e) 
              (echo-message! echo "Linter not available"))
            (lambda ()
              ;; Save buffer first if modified
              (let* ((ed (edit-window-editor win))
                     (proc (open-process
                             (list path: (car linter-cmd)
                                   arguments: (append (cdr linter-cmd) (list file-path))
                                   stdin-redirection: #f
                                   stdout-redirection: #t
                                   stderr-redirection: #t
                                   merge-stderr-with-stdout: #t)))
                     (output-text (read-line proc #f)))
                (process-status proc)
                (let* ((output (or output-text ""))
                       (errors (flycheck-parse-errors output file-path)))
                  (hash-put! *flycheck-errors* buf-name errors)
                  (hash-put! *flycheck-error-idx* buf-name 0)
                  (if (null? errors)
                    (echo-message! echo "No errors found")
                    (echo-message! echo
                      (string-append (number->string (length errors)) " error(s) found"))))))))))))

(def (cmd-flycheck-mode app)
  "Run syntax check on current buffer using appropriate linter."
  (flycheck-run-linter! app))

(def (cmd-flycheck-next-error app)
  "Jump to next flycheck error in current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((errors (or (hash-get *flycheck-errors* buf-name) '())))
        (if (null? errors)
          (echo-message! echo "No errors (run flycheck-mode first)")
          (let* ((idx (or (hash-get *flycheck-error-idx* buf-name) 0))
                 (new-idx (modulo (+ idx 1) (length errors)))
                 (error (list-ref errors new-idx))
                 (line (car error))
                 (col (cadr error))
                 (msg (caddr error))
                 (ed (edit-window-editor win)))
            (hash-put! *flycheck-error-idx* buf-name new-idx)
            ;; Go to the error line
            (editor-goto-line ed line)
            (echo-message! echo (string-append "Error " (number->string (+ new-idx 1))
                                              "/" (number->string (length errors))
                                              ": " msg))))))))

(def (cmd-flycheck-previous-error app)
  "Jump to previous flycheck error in current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((errors (or (hash-get *flycheck-errors* buf-name) '())))
        (if (null? errors)
          (echo-message! echo "No errors (run flycheck-mode first)")
          (let* ((idx (or (hash-get *flycheck-error-idx* buf-name) 0))
                 (new-idx (modulo (- idx 1) (length errors)))
                 (error (list-ref errors new-idx))
                 (line (car error))
                 (col (cadr error))
                 (msg (caddr error))
                 (ed (edit-window-editor win)))
            (hash-put! *flycheck-error-idx* buf-name new-idx)
            ;; Go to the error line
            (editor-goto-line ed line)
            (echo-message! echo (string-append "Error " (number->string (+ new-idx 1))
                                              "/" (number->string (length errors))
                                              ": " msg))))))))

(def (cmd-flycheck-list-errors app)
  "List all flycheck errors in a buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (buf-name (and buf (buffer-name buf)))
         (echo (app-state-echo app)))
    (if (not buf-name)
      (echo-error! echo "No buffer")
      (let ((errors (or (hash-get *flycheck-errors* buf-name) '())))
        (if (null? errors)
          (echo-message! echo "No errors (run flycheck-mode first)")
          (let* ((ed (edit-window-editor win))
                 (error-buf (buffer-create! "*Flycheck Errors*" ed))
                 (text (string-join
                         (map (lambda (err)
                                (string-append "Line " (number->string (car err))
                                              ": " (caddr err)))
                              errors)
                         "\n")))
            (buffer-attach! ed error-buf)
            (set! (edit-window-buffer win) error-buf)
            (editor-set-text ed (string-append "Flycheck errors for " buf-name ":\n\n" text "\n"))
            (editor-goto-pos ed 0)
            (editor-set-read-only ed #t)))))))

;; Treemacs / file explorer - simple tree-view of directory structure
;; Uses a dedicated buffer showing directory tree

(def *treemacs-root* #f) ; current tree root directory
(def *treemacs-expanded* (make-hash-table)) ; path -> #t if expanded

(def (treemacs-get-entries dir depth)
  "Get directory entries with indentation."
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let* ((entries (directory-files dir))
             (sorted (sort entries string<?))
             (indent (make-string (* depth 2) #\space)))
        (apply append
          (map (lambda (name)
                 (let* ((path (path-expand name dir))
                        (is-dir (directory-exists? path))
                        (expanded (and is-dir (hash-get *treemacs-expanded* path)))
                        (prefix (if is-dir
                                  (if expanded "▼ " "▶ ")
                                  "  ")))
                   (cons (list (string-append indent prefix name) path is-dir)
                         (if (and is-dir expanded)
                           (treemacs-get-entries path (+ depth 1))
                           '()))))
               sorted))))))

(def (treemacs-render app root)
  "Render the tree view in a buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Treemacs*")
                  (buffer-create! "*Treemacs*" ed)))
         (entries (cons (list (string-append "▼ " root) root #t)
                       (treemacs-get-entries root 1)))
         (lines (map car entries)))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed (string-append "Treemacs: " root "\n"
                                       (make-string 40 #\-)
                                       "\n"
                                       (string-join lines "\n")
                                       "\n\n[Enter: open/toggle, q: quit]"))
    (editor-goto-line ed 3)
    (editor-set-read-only ed #t)
    ;; Store entries for navigation
    (set! (buffer-lexer-lang buf) (list 'treemacs entries))))

(def (cmd-treemacs app)
  "Toggle treemacs file explorer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win)))
    ;; If already in treemacs, close it
    (if (and buf (string=? (buffer-name buf) "*Treemacs*"))
      (begin
        ;; Switch back to previous buffer or scratch
        (let ((other (find (lambda (b) (not (string=? (buffer-name b) "*Treemacs*")))
                          (buffer-list))))
          (when other
            (buffer-attach! (edit-window-editor win) other)
            (set! (edit-window-buffer win) other)))
        (echo-message! echo "Treemacs closed"))
      ;; Open treemacs
      (let ((root (or *treemacs-root*
                      (let ((file (and buf (buffer-file-path buf))))
                        (if file
                          (or (project-find-root (path-directory file))
                              (path-directory file))
                          (current-directory))))))
        (set! *treemacs-root* root)
        (treemacs-render app root)
        (echo-message! echo "Treemacs opened")))))

(def (cmd-treemacs-find-file app)
  "Find current file in treemacs and expand to it."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (file (and buf (buffer-file-path buf))))
    (if (not file)
      (echo-message! echo "Current buffer has no file")
      (let* ((root (or *treemacs-root*
                       (project-find-root (path-directory file))
                       (path-directory file))))
        (set! *treemacs-root* root)
        ;; Expand all parent directories
        (let loop ((dir (path-directory file)))
          (when (and dir (string-prefix? root dir))
            (hash-put! *treemacs-expanded* dir #t)
            (unless (string=? dir root)
              (loop (path-directory dir)))))
        (treemacs-render app root)
        (echo-message! echo (string-append "Found: " (path-strip-directory file)))))))

;; Magit-like git operations
(def (git-output args)
  "Run a git command and return its stdout as a string, or #f on error."
  (with-exception-catcher
    (lambda (e) #f)
    (lambda ()
      (let ((p (open-process
                 (list path: "git"
                       arguments: args
                       stdin-redirection: #f stdout-redirection: #t
                       stderr-redirection: #t))))
        (let ((out (read-line p #f)))
          (process-status p)
          out)))))

(def (cmd-magit-status app)
  "Show git status in magit-like interface with sections."
  (let* ((branch (or (git-output '("branch" "--show-current")) "???"))
         (status (or (git-output '("status" "--short")) ""))
         (log (or (git-output '("log" "--oneline" "-10")) ""))
         (stash (or (git-output '("stash" "list" "--oneline")) ""))
         ;; Parse status into staged/unstaged
         (lines (if (string=? status "") [] (string-split status #\newline)))
         (staged [])
         (unstaged []))
    ;; Classify lines
    (for-each (lambda (line)
                (when (>= (string-length line) 2)
                  (let ((ix (string-ref line 0))
                        (wt (string-ref line 1)))
                    (when (and (not (char=? ix #\space)) (not (char=? ix #\?)))
                      (set! staged (cons line staged)))
                    (when (or (char=? wt #\M) (char=? wt #\D) (char=? ix #\?))
                      (set! unstaged (cons line unstaged))))))
              lines)
    (let* ((text (string-append
                   "Head: " branch "\n\n"
                   (if (null? staged) ""
                     (string-append "Staged changes:\n"
                       (string-join (reverse staged) "\n") "\n\n"))
                   (if (null? unstaged) ""
                     (string-append "Unstaged changes:\n"
                       (string-join (reverse unstaged) "\n") "\n\n"))
                   (if (and (null? staged) (null? unstaged))
                     "Working tree clean\n\n" "")
                   (if (string=? stash "") ""
                     (string-append "Stashes:\n" stash "\n\n"))
                   "Recent commits:\n" log "\n"))
           (fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Magit*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed text)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t))))

(def (cmd-magit-log app)
  "Show git log with graph."
  (let ((result (or (git-output
                      '("log" "--oneline" "--graph" "--decorate" "-40"))
                    "(not a git repository)")))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Magit Log*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Git Log\n\n" result "\n"))
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t))))

(def (cmd-magit-diff app)
  "Show git diff."
  (let ((result (or (git-output '("diff" "--stat"))
                    "(not a git repository)")))
    (let* ((full-diff (or (git-output '("diff")) ""))
           (fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Magit Diff*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Git Diff\n\n" result "\n\n" full-diff "\n"))
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t)
      ;; Apply diff highlighting
      (setup-highlighting-for-file! ed "diff.diff"))))


(def (cmd-git-log-file app)
  "Show git log for the current file."
  (let* ((buf (current-buffer-from-app app))
         (fp (buffer-file-path buf)))
    (if (not fp)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((result (or (git-output
                          (list "log" "--oneline" "--follow" "-30" fp))
                        "Not a git repository or file not tracked")))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (log-buf (buffer-create!
                          (string-append "*Log: " (path-strip-directory fp) "*")
                          ed)))
          (buffer-attach! ed log-buf)
          (set! (edit-window-buffer win) log-buf)
          (editor-set-text ed (string-append "File: " fp "\n\n" result "\n"))
          (editor-goto-pos ed 0)
          (editor-set-read-only ed #t))))))

(def (cmd-magit-commit app)
  "Create git commit with message from echo area."
  (let ((msg (app-read-string app "Commit message: ")))
    (when (and msg (not (string-empty? msg)))
      (let ((result (with-exception-catcher
                      (lambda (e) (string-append "Error: " (with-output-to-string (lambda () (display-exception e)))))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "commit" "-m" msg)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out "Committed")))))))
        (echo-message! (app-state-echo app) result)))))

(def (cmd-magit-stage-file app)
  "Stage current file."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (buffer-file-path buf)))
    (if path
      (let ((result (with-exception-catcher
                      (lambda (e) "Error staging file")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "add" path)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (process-status p)
                          (string-append "Staged: " (path-strip-directory path)))))))
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-magit-unstage-file app)
  "Unstage current file."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (path (buffer-file-path buf)))
    (if path
      (let ((result (with-exception-catcher
                      (lambda (e) "Error unstaging file")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "reset" "HEAD" path)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (process-status p)
                          (string-append "Unstaged: " (path-strip-directory path)))))))
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

(def (cmd-magit-branch app)
  "Show or create git branch."
  (let ((result (with-exception-catcher
                  (lambda (e) "Not a git repository")
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git"
                                     arguments: '("branch" "-a")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p)
                        (or out "(no branches)")))))))
    (echo-message! (app-state-echo app) result)))

(def (cmd-magit-checkout app)
  "Switch git branch."
  (let ((branch (app-read-string app "Branch: ")))
    (when (and branch (not (string-empty? branch)))
      (let ((result (with-exception-catcher
                      (lambda (e) "Error switching branch")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git"
                                         arguments: (list "checkout" branch)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out (string-append "Switched to: " branch))))))))
        (echo-message! (app-state-echo app) result)))))

;; Minibuffer commands
(def (cmd-minibuffer-complete app)
  "Complete in minibuffer (trigger TAB completion)."
  (echo-message! (app-state-echo app) "TAB to complete"))

(def (cmd-minibuffer-keyboard-quit app)
  "Quit minibuffer."
  (echo-clear! (app-state-echo app))
  (echo-message! (app-state-echo app) "Quit"))

;; Abbrev mode extras
(def *abbrevs* (make-hash-table)) ; abbrev -> expansion

(def (cmd-define-global-abbrev app)
  "Define a global abbreviation."
  (let ((abbrev (app-read-string app "Abbrev: ")))
    (when (and abbrev (not (string-empty? abbrev)))
      (let ((expansion (app-read-string app (string-append "Expansion for \"" abbrev "\": "))))
        (when (and expansion (not (string-empty? expansion)))
          (hash-put! *abbrevs* abbrev expansion)
          (echo-message! (app-state-echo app)
            (string-append "Abbrev: " abbrev " -> " expansion)))))))

(def (cmd-define-mode-abbrev app)
  "Define a mode-specific abbreviation (stored globally)."
  (cmd-define-global-abbrev app))

(def (cmd-unexpand-abbrev app)
  "Undo last abbreviation expansion."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_UNDO 0 0)
    (echo-message! (app-state-echo app) "Abbrev unexpanded")))

;; Hippie expand
(def (cmd-hippie-expand-undo app)
  "Undo last hippie-expand."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win)))
    (send-message ed SCI_UNDO 0 0)
    (echo-message! (app-state-echo app) "Hippie expand undone")))

;; Compilation extras
(def (cmd-next-error-function app)
  "Navigate to next compilation error (uses flycheck)."
  (cmd-flycheck-next-error app))

(def (cmd-previous-error-function app)
  "Navigate to previous compilation error (uses flycheck)."
  (cmd-flycheck-previous-error app))

;;;============================================================================
;;; Batch 30: TODO/FIXME, cursor, modeline, indent guides, etc.
;;;============================================================================

;;; --- Insert TODO/FIXME comment annotations ---

(def (cmd-insert-todo app)
  "Insert a TODO comment annotation at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (note (app-read-string app "TODO note: ")))
    (when (and note (> (string-length note) 0))
      (let ((text (string-append "TODO: " note)))
        (editor-insert-text ed (editor-get-current-pos ed) text)
        (echo-message! echo "TODO inserted")))))

(def (cmd-insert-fixme app)
  "Insert a FIXME comment annotation at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (note (app-read-string app "FIXME note: ")))
    (when (and note (> (string-length note) 0))
      (let ((text (string-append "FIXME: " note)))
        (editor-insert-text ed (editor-get-current-pos ed) text)
        (echo-message! echo "FIXME inserted")))))

;;; --- Toggle cursor type ---

(def *cursor-type* 'line)  ; 'line, 'block, 'underline

(def (cmd-toggle-cursor-type app)
  "Cycle through cursor types: line -> block -> underline."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *cursor-type*
      (case *cursor-type*
        ((line) 'block)
        ((block) 'underline)
        (else 'line)))
    (let ((caret-style
            (case *cursor-type*
              ((line) 1)       ; CARETSTYLE_LINE
              ((block) 2)      ; CARETSTYLE_BLOCK
              ((underline) 0)  ; CARETSTYLE_INVISIBLE (approx)
              (else 1))))
      (send-message ed SCI_SETCARETSTYLE caret-style 0)
      (echo-message! echo
        (string-append "Cursor: " (symbol->string *cursor-type*))))))

;;; --- Toggle modeline display ---

(def *modeline-visible* #t)

(def (cmd-toggle-modeline app)
  "Toggle the modeline/status bar visibility."
  (let ((echo (app-state-echo app)))
    (set! *modeline-visible* (not *modeline-visible*))
    (echo-message! echo
      (if *modeline-visible*
        "Modeline visible"
        "Modeline hidden"))))

;;; --- Toggle indent guides ---

(def *indent-guide-mode* #f)

(def (cmd-toggle-indent-guide app)
  "Toggle display of indentation guides."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *indent-guide-mode* (not *indent-guide-mode*))
    (send-message ed SCI_SETINDENTATIONGUIDES (if *indent-guide-mode* 3 0) 0)
    (echo-message! echo
      (if *indent-guide-mode*
        "Indent guides on"
        "Indent guides off"))))

;;; --- Toggle rainbow delimiters mode ---

(def *rainbow-mode* #f)

(def (cmd-toggle-rainbow-mode app)
  "Toggle rainbow delimiter/bracket coloring mode."
  (let ((echo (app-state-echo app)))
    (set! *rainbow-mode* (not *rainbow-mode*))
    (echo-message! echo
      (if *rainbow-mode*
        "Rainbow delimiters on"
        "Rainbow delimiters off"))))

;;; --- Quick switch to scratch buffer ---

(def (cmd-goto-scratch app)
  "Switch to the *scratch* buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (scratch (find (lambda (b) (equal? (buffer-name b) "*scratch*"))
                        *buffer-list*)))
    (if scratch
      (begin
        (buffer-attach! ed scratch)
        (set! (edit-window-buffer win) scratch)
        (echo-message! echo "Switched to *scratch*"))
      (let ((buf (buffer-create! "*scratch*" ed)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (echo-message! echo "Created *scratch*")))))

;;; --- Display prefix key help ---

(def (cmd-display-prefix-help app)
  "Show all bindings under a given prefix key."
  (let* ((echo (app-state-echo app))
         (prefix (app-read-string app "Prefix: ")))
    (when (and prefix (> (string-length prefix) 0))
      (let* ((ed (current-editor app))
             (entries (keymap-entries *global-keymap*))
             (matches (filter
                        (lambda (e) (string-prefix? prefix (car e)))
                        entries))
             (text (with-output-to-string
                     (lambda ()
                       (display (string-append "Bindings for '" prefix "':\n"))
                       (display (make-string 50 #\-))
                       (display "\n")
                       (if (null? matches)
                         (display "  (none)\n")
                         (for-each
                           (lambda (e)
                             (display "  ")
                             (display (car e))
                             (display " -> ")
                             (display (cdr e))
                             (display "\n"))
                           (sort matches
                             (lambda (a b) (string<? (car a) (car b))))))
                       (display (make-string 50 #\-))
                       (display "\n")
                       (display (number->string (length matches)))
                       (display " binding(s)\n")))))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (buf (buffer-create! "*Prefix Help*" ed)))
          (buffer-attach! ed buf)
          (set! (edit-window-buffer win) buf)
          (editor-set-text ed text)
          (editor-goto-pos ed 0)
          (editor-set-read-only ed #t))))))

;;; --- Toggle electric quote mode ---

(def *electric-quote-mode* #f)

(def (cmd-toggle-electric-quote app)
  "Toggle electric quote mode (auto-convert straight quotes to smart quotes)."
  (let ((echo (app-state-echo app)))
    (set! *electric-quote-mode* (not *electric-quote-mode*))
    (echo-message! echo
      (if *electric-quote-mode*
        "Electric quote mode on"
        "Electric quote mode off"))))

;;; --- Inline calculator ---

(def (cmd-calculator-inline app)
  "Evaluate math expression at point and show/insert result."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (expr (app-read-string app "Calc: ")))
    (when (and expr (> (string-length expr) 0))
      (with-catch
        (lambda (e) (echo-message! echo "Calc error"))
        (lambda ()
          (let* ((result (eval (with-input-from-string expr read)))
                 (result-str (if (number? result)
                               (number->string result)
                               (with-output-to-string
                                 (lambda () (write result))))))
            (echo-message! echo
              (string-append expr " = " result-str))))))))

;;; --- Toggle visible mark mode ---

(def *visible-mark-mode* #f)

(def (cmd-toggle-visible-mark app)
  "Toggle visible mark mode (show mark position indicator)."
  (let ((echo (app-state-echo app)))
    (set! *visible-mark-mode* (not *visible-mark-mode*))
    (echo-message! echo
      (if *visible-mark-mode*
        "Visible mark mode on"
        "Visible mark mode off"))))

;;; --- Open recent directory ---

(def (cmd-open-recent-dir app)
  "Show a list of directories from recently opened files."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (dirs (let loop ((bufs *buffer-list*) (acc []))
                 (if (null? bufs) (reverse acc)
                   (let* ((b (car bufs))
                          (fp (buffer-file-path b)))
                     (if (not fp) (loop (cdr bufs) acc)
                       (let* ((parts (string-split fp #\/))
                              (dir (if (<= (length parts) 1) "."
                                     (string-join
                                       (let cut ((ls parts) (res []))
                                         (if (null? (cdr ls)) (reverse res)
                                           (cut (cdr ls) (cons (car ls) res))))
                                       "/"))))
                         (if (member dir acc)
                           (loop (cdr bufs) acc)
                           (loop (cdr bufs) (cons dir acc)))))))))
         (text (with-output-to-string
                 (lambda ()
                   (display "Recent Directories:\n")
                   (display (make-string 50 #\-))
                   (display "\n")
                   (for-each
                     (lambda (d)
                       (display "  ")
                       (display d)
                       (display "\n"))
                     dirs)
                   (display (make-string 50 #\-))
                   (display "\n")
                   (display (number->string (length dirs)))
                   (display " directories\n")))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (buf (buffer-create! "*Recent Dirs*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed text)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t))))

;;; --- Toggle fringe indicators ---

(def *fringe-mode* #t)

(def (cmd-toggle-fringe app)
  "Toggle the fringe/margin indicators."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *fringe-mode* (not *fringe-mode*))
    (if *fringe-mode*
      (send-message ed SCI_SETMARGINWIDTHN 2 16)
      (send-message ed SCI_SETMARGINWIDTHN 2 0))
    (echo-message! echo
      (if *fringe-mode*
        "Fringe indicators on"
        "Fringe indicators off"))))

;;; =========================================================================
;;; Batch 36: show-paren style, UUID, visual line, scroll, etc.
;;; =========================================================================

(def *show-paren-style* 'parenthesis)  ;; 'parenthesis, 'expression, or 'mixed
(def *auto-insert-mode* #f)
(def *global-visual-line-mode* #f)
(def *scroll-conservatively* 0)
(def *show-keystroke-mode* #f)
(def *auto-revert-tail-mode* #f)
(def *flyspell-prog-mode* #f)
(def *auto-save-buffers-mode* #f)
(def *global-linum-mode* #f)

(def (cmd-toggle-show-paren-style app)
  "Cycle show-paren-style: parenthesis -> expression -> mixed."
  (let ((echo (app-state-echo app)))
    (set! *show-paren-style*
      (case *show-paren-style*
        ((parenthesis) 'expression)
        ((expression) 'mixed)
        (else 'parenthesis)))
    (echo-message! echo
      (string-append "Show-paren style: " (symbol->string *show-paren-style*)))))

(def (cmd-insert-uuid-v4 app)
  "Insert a UUID v4 at point (random-based)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         ;; Generate 16 random hex pairs
         (hex-chars "0123456789abcdef")
         (rand-hex (lambda ()
                     (string (string-ref hex-chars (random-integer 16))
                             (string-ref hex-chars (random-integer 16)))))
         ;; Build UUID: 8-4-4-4-12
         (p1 (string-append (rand-hex) (rand-hex) (rand-hex) (rand-hex)))
         (p2 (string-append (rand-hex) (rand-hex)))
         (p3 (string-append "4" (substring (rand-hex) 1 2) (rand-hex)))
         (p4-first (string-ref hex-chars (+ 8 (random-integer 4))))
         (p4 (string-append (string p4-first) (substring (rand-hex) 1 2) (rand-hex)))
         (p5 (string-append (rand-hex) (rand-hex) (rand-hex) (rand-hex) (rand-hex) (rand-hex)))
         (uuid (string-append p1 "-" p2 "-" p3 "-" p4 "-" p5)))
    (editor-replace-selection ed uuid)
    (echo-message! echo (string-append "UUID: " uuid))))

(def (cmd-toggle-auto-insert-mode app)
  "Toggle auto-insert-mode (auto templates for new files)."
  (let ((echo (app-state-echo app)))
    (set! *auto-insert-mode* (not *auto-insert-mode*))
    (echo-message! echo (if *auto-insert-mode*
                          "Auto-insert mode ON"
                          "Auto-insert mode OFF"))))

(def (cmd-toggle-global-visual-line app)
  "Toggle global visual-line-mode (word wrap with visual movement)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (set! *global-visual-line-mode* (not *global-visual-line-mode*))
    (if *global-visual-line-mode*
      (begin
        ;; SCI_SETWRAPMODE = 2268, SC_WRAP_WORD = 1
        (send-message ed 2268 1 0)
        (echo-message! echo "Global visual-line mode ON"))
      (begin
        ;; SCI_SETWRAPMODE = 2268, SC_WRAP_NONE = 0
        (send-message ed 2268 0 0)
        (echo-message! echo "Global visual-line mode OFF")))))

(def (cmd-toggle-scroll-conservatively app)
  "Toggle conservative scrolling (scroll just enough to show cursor)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (if (= *scroll-conservatively* 0)
      (begin
        (set! *scroll-conservatively* 101)
        ;; SCI_SETVISIBLEPOLICY = 2394, VISIBLE_STRICT = 4
        (send-message ed 2394 4 2)
        (echo-message! echo "Scroll conservatively ON"))
      (begin
        (set! *scroll-conservatively* 0)
        ;; SCI_SETVISIBLEPOLICY = 2394, VISIBLE_SLOP = 1
        (send-message ed 2394 1 5)
        (echo-message! echo "Scroll conservatively OFF")))))

(def (cmd-toggle-show-keystroke app)
  "Toggle showing keystrokes in the echo area."
  (let ((echo (app-state-echo app)))
    (set! *show-keystroke-mode* (not *show-keystroke-mode*))
    (echo-message! echo (if *show-keystroke-mode*
                          "Show keystrokes ON"
                          "Show keystrokes OFF"))))

(def (cmd-toggle-auto-revert-tail app)
  "Toggle auto-revert-tail-mode (tail -f behavior for log files)."
  (let ((echo (app-state-echo app)))
    (set! *auto-revert-tail-mode* (not *auto-revert-tail-mode*))
    (echo-message! echo (if *auto-revert-tail-mode*
                          "Auto-revert tail mode ON"
                          "Auto-revert tail mode OFF"))))

(def (cmd-toggle-flyspell-prog app)
  "Toggle flyspell-prog-mode (spell-check comments and strings only)."
  (let ((echo (app-state-echo app)))
    (set! *flyspell-prog-mode* (not *flyspell-prog-mode*))
    (echo-message! echo (if *flyspell-prog-mode*
                          "Flyspell-prog mode ON"
                          "Flyspell-prog mode OFF"))))

(def (cmd-toggle-auto-save-buffers app)
  "Toggle auto-save-buffers (periodically save all modified buffers)."
  (let ((echo (app-state-echo app)))
    (set! *auto-save-buffers-mode* (not *auto-save-buffers-mode*))
    (echo-message! echo (if *auto-save-buffers-mode*
                          "Auto-save buffers ON"
                          "Auto-save buffers OFF"))))

(def (cmd-insert-backslash app)
  "Insert a backslash character at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (editor-replace-selection ed "\\")
    (echo-message! echo "Backslash inserted")))

(def (cmd-toggle-global-linum app)
  "Toggle global linum-mode (line numbers in all buffers)."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (set! *global-linum-mode* (not *global-linum-mode*))
    (if *global-linum-mode*
      (begin
        ;; SCI_SETMARGINWIDTHN = 2242
        (send-message ed 2242 0 48)
        (echo-message! echo "Global linum mode ON"))
      (begin
        (send-message ed 2242 0 0)
        (echo-message! echo "Global linum mode OFF")))))

;; ── batch 41: IDE framework toggles ─────────────────────────────────
(def *company-mode* #f)
(def *ivy-mode* #f)
(def *helm-mode* #f)
(def *projectile-mode* #f)
(def *evil-mode* #f)
(def *doom-modeline* #f)
(def *treesit-mode* #f)
(def *eglot-mode* #f)
(def *display-time* #f)
(def *display-battery* #f)

(def (cmd-toggle-company-mode app)
  "Toggle company-mode (auto-completion framework)."
  (let ((echo (app-state-echo app)))
    (set! *company-mode* (not *company-mode*))
    (echo-message! echo (if *company-mode*
                          "Company mode ON" "Company mode OFF"))))

(def (cmd-toggle-ivy-mode app)
  "Toggle ivy-mode (completion framework)."
  (let ((echo (app-state-echo app)))
    (set! *ivy-mode* (not *ivy-mode*))
    (echo-message! echo (if *ivy-mode*
                          "Ivy mode ON" "Ivy mode OFF"))))

(def (cmd-toggle-helm-mode app)
  "Toggle helm-mode (incremental completion framework)."
  (let ((echo (app-state-echo app)))
    (set! *helm-mode* (not *helm-mode*))
    (echo-message! echo (if *helm-mode*
                          "Helm mode ON" "Helm mode OFF"))))

(def (cmd-toggle-projectile-mode app)
  "Toggle projectile-mode (project management)."
  (let ((echo (app-state-echo app)))
    (set! *projectile-mode* (not *projectile-mode*))
    (echo-message! echo (if *projectile-mode*
                          "Projectile mode ON" "Projectile mode OFF"))))

(def (cmd-toggle-evil-mode app)
  "Toggle evil-mode (vim emulation)."
  (let ((echo (app-state-echo app)))
    (set! *evil-mode* (not *evil-mode*))
    (echo-message! echo (if *evil-mode*
                          "Evil mode ON" "Evil mode OFF"))))

(def (cmd-toggle-doom-modeline app)
  "Toggle doom-modeline (fancy mode line)."
  (let ((echo (app-state-echo app)))
    (set! *doom-modeline* (not *doom-modeline*))
    (echo-message! echo (if *doom-modeline*
                          "Doom modeline ON" "Doom modeline OFF"))))

(def (cmd-toggle-treesit-mode app)
  "Toggle treesit-mode (tree-sitter based syntax)."
  (let ((echo (app-state-echo app)))
    (set! *treesit-mode* (not *treesit-mode*))
    (echo-message! echo (if *treesit-mode*
                          "Tree-sitter mode ON" "Tree-sitter mode OFF"))))

(def (cmd-toggle-eglot-mode app)
  "Toggle eglot-mode (LSP client)."
  (let ((echo (app-state-echo app)))
    (set! *eglot-mode* (not *eglot-mode*))
    (echo-message! echo (if *eglot-mode*
                          "Eglot mode ON" "Eglot mode OFF"))))

(def (cmd-toggle-display-time app)
  "Toggle display of time in mode line."
  (let ((echo (app-state-echo app)))
    (set! *display-time* (not *display-time*))
    (echo-message! echo (if *display-time*
                          "Display time ON" "Display time OFF"))))

(def (cmd-toggle-display-battery app)
  "Toggle display of battery status in mode line."
  (let ((echo (app-state-echo app)))
    (set! *display-battery* (not *display-battery*))
    (echo-message! echo (if *display-battery*
                          "Display battery ON" "Display battery OFF"))))
