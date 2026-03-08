;;; -*- Gerbil -*-
;;; Qt commands vcs2 - imenu, project, xref, text operations, line endings
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        ../pregexp-compat
        :gemacs/qt/sci-shim
        :gemacs/core
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
        :gemacs/qt/commands-vcs)

(def (imenu-extract-definitions text lang)
  "Extract symbol definitions from TEXT based on LANG.
Returns list of (name . line-number) pairs."
  (let ((lines (string-split text #\newline))
        (defs []))
    (let loop ((ls lines) (line-num 1))
      (if (null? ls)
        (reverse defs)
        (let ((line (car ls)))
          ;; Match based on language
          (let ((found
                 (cond
                   ;; Gerbil/Scheme/Lisp: (def, (defstruct, (defclass, (defrule, etc.
                   ((memq lang '(scheme gerbil lisp))
                    (cond
                      ;; (def (name ...) or (def name
                      ((and (>= (string-length line) 5)
                            (string-prefix? "(def " line))
                       (let* ((rest (substring line 5 (string-length line)))
                              (rest (if (and (> (string-length rest) 0)
                                             (char=? (string-ref rest 0) #\())
                                      (substring rest 1 (string-length rest))
                                      rest)))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\-) (char=? ch #\_)
                                          (char=? ch #\!) (char=? ch #\?))))
                             (scan (+ i 1))
                             (and (> i 0) (substring rest 0 i))))))
                      ;; (defstruct name, (defclass name, etc.
                      ((or (string-prefix? "(defstruct " line)
                           (string-prefix? "(defclass " line)
                           (string-prefix? "(defrule " line)
                           (string-prefix? "(defsyntax " line)
                           (string-prefix? "(defmethod " line))
                       (let* ((space-pos (string-index line #\space 1))
                              (rest (and space-pos
                                         (substring line (+ space-pos 1)
                                                    (string-length line)))))
                         (and rest
                              (let scan ((i 0))
                                (if (and (< i (string-length rest))
                                         (let ((ch (string-ref rest i)))
                                           (or (char-alphabetic? ch) (char-numeric? ch)
                                               (char=? ch #\-) (char=? ch #\_)
                                               (char=? ch #\!) (char=? ch #\?))))
                                  (scan (+ i 1))
                                  (and (> i 0) (substring rest 0 i)))))))
                      (else #f)))
                   ;; Python: def/class at start of line
                   ((eq? lang 'python)
                    (cond
                      ((string-prefix? "def " line)
                       (let* ((rest (substring line 4 (string-length line))))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\_))))
                             (scan (+ i 1))
                             (and (> i 0) (string-append "def " (substring rest 0 i)))))))
                      ((string-prefix? "class " line)
                       (let* ((rest (substring line 6 (string-length line))))
                         (let scan ((i 0))
                           (if (and (< i (string-length rest))
                                    (let ((ch (string-ref rest i)))
                                      (or (char-alphabetic? ch) (char-numeric? ch)
                                          (char=? ch #\_))))
                             (scan (+ i 1))
                             (and (> i 0) (string-append "class " (substring rest 0 i)))))))
                      (else #f)))
                   ;; C/C++/Java/Go/Rust/JS/TS: function name(
                   ((memq lang '(c cpp java go rust javascript typescript))
                    ;; Look for lines containing "name(" that start at indent 0
                    ;; and don't start with # (preprocessor) or // (comment)
                    (and (> (string-length line) 0)
                         (not (char=? (string-ref line 0) #\#))
                         (not (string-prefix? "//" line))
                         (not (string-prefix? " " line))
                         (not (string-prefix? "\t" line))
                         ;; Look for word( pattern
                         (let ((paren-pos (string-index line #\()))
                           (and paren-pos (> paren-pos 0)
                                ;; Extract the word before (
                                (let scan ((i (- paren-pos 1)))
                                  (if (and (>= i 0)
                                           (let ((ch (string-ref line i)))
                                             (or (char-alphabetic? ch) (char-numeric? ch)
                                                 (char=? ch #\_))))
                                    (scan (- i 1))
                                    (let ((name (substring line (+ i 1) paren-pos)))
                                      (and (> (string-length name) 0)
                                           ;; Skip common keywords
                                           (not (member name '("if" "for" "while" "switch"
                                                               "return" "else" "catch"
                                                               "sizeof" "typeof")))
                                           name))))))))
                   ;; Shell: function name() or name()
                   ((memq lang '(shell bash))
                    (or (and (string-prefix? "function " line)
                             (let* ((rest (substring line 9 (string-length line))))
                               (let scan ((i 0))
                                 (if (and (< i (string-length rest))
                                          (let ((ch (string-ref rest i)))
                                            (or (char-alphabetic? ch) (char-numeric? ch)
                                                (char=? ch #\_))))
                                   (scan (+ i 1))
                                   (and (> i 0) (substring rest 0 i))))))
                        #f))
                   ;; Ruby: def name
                   ((eq? lang 'ruby)
                    (and (string-prefix? "def " line)
                         (let* ((rest (substring line 4 (string-length line))))
                           (let scan ((i 0))
                             (if (and (< i (string-length rest))
                                      (let ((ch (string-ref rest i)))
                                        (or (char-alphabetic? ch) (char-numeric? ch)
                                            (char=? ch #\_) (char=? ch #\?))))
                               (scan (+ i 1))
                               (and (> i 0) (substring rest 0 i)))))))
                   ;; Fallback: look for "def " prefix
                   (else
                    (and (string-prefix? "(def " line)
                         (let* ((rest (substring line 5 (string-length line)))
                                (rest (if (and (> (string-length rest) 0)
                                               (char=? (string-ref rest 0) #\())
                                        (substring rest 1 (string-length rest))
                                        rest)))
                           (let scan ((i 0))
                             (if (and (< i (string-length rest))
                                      (let ((ch (string-ref rest i)))
                                        (or (char-alphabetic? ch) (char-numeric? ch)
                                            (char=? ch #\-) (char=? ch #\_))))
                               (scan (+ i 1))
                               (and (> i 0) (substring rest 0 i))))))))))
            (when found
              (set! defs (cons (cons found line-num) defs)))
            (loop (cdr ls) (+ line-num 1))))))))

(def (cmd-imenu app)
  "List definitions in the current buffer and jump to selected one."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (text (qt-plain-text-edit-text ed))
         (lang (buffer-lexer-lang buf))
         (defs (imenu-extract-definitions text lang))
         (echo (app-state-echo app)))
    (if (null? defs)
      (echo-error! echo "No definitions found")
      (let* ((names (map (lambda (d)
                           (string-append (car d) " (L" (number->string (cdr d)) ")"))
                         defs))
             (choice (qt-echo-read-with-narrowing app "Go to:" names)))
        (when (and choice (> (string-length choice) 0))
          ;; Find the matching definition
          (let ((found (let loop ((ds defs) (ns names))
                         (cond
                           ((null? ds) #f)
                           ((string=? choice (car ns)) (car ds))
                           (else (loop (cdr ds) (cdr ns)))))))
            (when found
              (let* ((line-num (cdr found))
                     (target-pos (text-line-position text line-num)))
                (qt-plain-text-edit-set-cursor-position! ed target-pos)
                (qt-plain-text-edit-ensure-cursor-visible! ed)
                (echo-message! echo (string-append (car found) " — line "
                                                   (number->string line-num)))))))))))

(def (cmd-show-word-count app)
  "Show word count for the entire buffer."
  (cmd-count-words-buffer app))

(def (cmd-show-char-count app)
  "Show character count."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (echo-message! (app-state-echo app)
      (string-append (number->string (string-length text)) " characters"))))

(def (cmd-insert-path-separator app)
  "Insert a path separator."
  (qt-plain-text-edit-insert-text! (current-qt-editor app) "/"))

(def (cmd-maximize-window app)
  "Maximize current window by deleting others."
  (cmd-delete-other-windows app))

(def (cmd-minimize-window app)
  "Minimize the main window."
  (let ((win (qt-frame-main-win (app-state-frame app))))
    (qt-widget-show-minimized! win)
    (echo-message! (app-state-echo app) "Window minimized")))

(def (cmd-delete-matching-lines app)
  "Delete lines matching a pattern (alias for flush-lines)."
  (cmd-flush-lines app))

(def (cmd-delete-non-matching-lines app)
  "Delete lines not matching a pattern (alias for keep-lines)."
  (cmd-keep-lines app))

(def (cmd-copy-matching-lines app)
  "Copy lines matching a pattern to kill ring."
  (let ((pat (qt-echo-read-string app "Copy lines matching: ")))
    (when pat
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (matches (filter (lambda (l) (string-contains l pat)) lines)))
        (if (pair? matches)
          (let ((result (string-join matches "\n")))
            (set! (app-state-kill-ring app) (cons result (app-state-kill-ring app)))
            (echo-message! (app-state-echo app)
              (string-append (number->string (length matches)) " lines copied")))
          (echo-message! (app-state-echo app) "No matching lines"))))))

(def (cmd-count-lines-buffer app)
  "Count lines in the buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline)))
    (echo-message! (app-state-echo app)
      (string-append (number->string (length lines)) " lines"))))

(def (cmd-count-words-paragraph app)
  "Count words in the current paragraph."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         ;; Find paragraph boundaries
         (para-start
           (let loop ((i (- pos 1)))
             (cond
               ((< i 0) 0)
               ((and (char=? (string-ref text i) #\newline)
                     (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
                (+ i 1))
               (else (loop (- i 1))))))
         (para-end
           (let loop ((i pos))
             (cond
               ((>= i len) len)
               ((and (char=? (string-ref text i) #\newline)
                     (< (+ i 1) len) (char=? (string-ref text (+ i 1)) #\newline))
                i)
               (else (loop (+ i 1))))))
         (para (substring text para-start para-end))
         (words (let loop ((i 0) (in-word? #f) (count 0))
                  (if (>= i (string-length para))
                    (if in-word? (+ count 1) count)
                    (let ((ch (string-ref para i)))
                      (if (char-whitespace? ch)
                        (loop (+ i 1) #f (if in-word? (+ count 1) count))
                        (loop (+ i 1) #t count)))))))
    (echo-message! (app-state-echo app)
      (string-append (number->string words) " words in paragraph"))))

(def (cmd-convert-to-unix app)
  "Convert line endings to Unix (LF)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (new-text (let loop ((i 0) (acc []))
                     (cond
                       ((>= i (string-length text))
                        (list->string (reverse acc)))
                       ((and (char=? (string-ref text i) #\return)
                             (< (+ i 1) (string-length text))
                             (char=? (string-ref text (+ i 1)) #\newline))
                        (loop (+ i 2) (cons #\newline acc)))
                       ((char=? (string-ref text i) #\return)
                        (loop (+ i 1) (cons #\newline acc)))
                       (else (loop (+ i 1) (cons (string-ref text i) acc)))))))
    (qt-plain-text-edit-set-text! ed new-text)
    (echo-message! (app-state-echo app) "Converted to Unix line endings")))

(def (cmd-convert-to-dos app)
  "Convert line endings to DOS (CRLF)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         ;; First convert to Unix, then to DOS
         (unix (let loop ((i 0) (acc []))
                 (cond
                   ((>= i (string-length text))
                    (list->string (reverse acc)))
                   ((and (char=? (string-ref text i) #\return)
                         (< (+ i 1) (string-length text))
                         (char=? (string-ref text (+ i 1)) #\newline))
                    (loop (+ i 2) (cons #\newline acc)))
                   ((char=? (string-ref text i) #\return)
                    (loop (+ i 1) (cons #\newline acc)))
                   (else (loop (+ i 1) (cons (string-ref text i) acc))))))
         (dos (let loop ((i 0) (acc []))
                (cond
                  ((>= i (string-length unix))
                   (list->string (reverse acc)))
                  ((char=? (string-ref unix i) #\newline)
                   (loop (+ i 1) (cons #\newline (cons #\return acc))))
                  (else (loop (+ i 1) (cons (string-ref unix i) acc)))))))
    (qt-plain-text-edit-set-text! ed dos)
    (echo-message! (app-state-echo app) "Converted to DOS line endings")))

(def (cmd-show-line-endings app)
  "Show the line ending style of current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (has-cr (string-contains text "\r\n"))
         (style (if has-cr "DOS (CRLF)" "Unix (LF)")))
    (echo-message! (app-state-echo app) (string-append "Line endings: " style))))

(def (cmd-wrap-lines-at-column app)
  "Wrap long lines at a specified column."
  (let ((col-str (qt-echo-read-string app "Wrap at column: ")))
    (when col-str
      (let ((col (string->number col-str)))
        (when (and col (> col 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (lines (string-split text #\newline))
                 (wrapped
                   (apply append
                     (map (lambda (line)
                            (if (<= (string-length line) col)
                              (list line)
                              ;; Break line at word boundaries
                              (let loop ((rest line) (acc []))
                                (if (<= (string-length rest) col)
                                  (reverse (cons rest acc))
                                  (let* ((break-pos
                                           (let bloop ((i col))
                                             (cond
                                               ((<= i 0) col)
                                               ((char=? (string-ref rest i) #\space) i)
                                               (else (bloop (- i 1))))))
                                         (frag (substring rest 0 break-pos))
                                         (remaining (if (< break-pos (string-length rest))
                                                      (substring rest (+ break-pos 1)
                                                                 (string-length rest))
                                                      "")))
                                    (loop remaining (cons frag acc)))))))
                          lines)))
                 (new-text (string-join wrapped "\n")))
            (qt-plain-text-edit-set-text! ed new-text)
            (echo-message! (app-state-echo app)
              (string-append "Lines wrapped at column " col-str))))))))

(def (cmd-strip-line-numbers app)
  "Strip leading line numbers from each line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (stripped (map (lambda (line)
                          ;; Strip leading digits + optional separator (: or . or space)
                          (let loop ((i 0))
                            (cond
                              ((>= i (string-length line)) line)
                              ((char-numeric? (string-ref line i)) (loop (+ i 1)))
                              ((and (> i 0) (memq (string-ref line i) '(#\: #\. #\space #\tab)))
                               (let ((rest (substring line (+ i 1) (string-length line))))
                                 (if (and (> (string-length rest) 0)
                                          (char=? (string-ref rest 0) #\space))
                                   (substring rest 1 (string-length rest))
                                   rest)))
                              (else line))))
                        lines)))
    (qt-plain-text-edit-set-text! ed (string-join stripped "\n"))
    (echo-message! (app-state-echo app) "Line numbers stripped")))

(def (cmd-goto-word-at-point app)
  "Search for the word at point (same as search-forward-word)."
  (cmd-search-forward-word app))

(def (cmd-unindent-region app)
  "Remove one level of indentation from the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (dedented (map (lambda (l)
                              (cond
                                ((and (>= (string-length l) 2)
                                      (char=? (string-ref l 0) #\space)
                                      (char=? (string-ref l 1) #\space))
                                 (substring l 2 (string-length l)))
                                ((and (>= (string-length l) 1)
                                      (char=? (string-ref l 0) #\tab))
                                 (substring l 1 (string-length l)))
                                (else l)))
                            lines))
             (result (string-join dedented "\n")))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region unindented"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-number-region app)
  "Add line numbers to the region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (if mark
      (let* ((start (min pos mark))
             (end (max pos mark))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (numbered (let loop ((ls lines) (n 1) (acc []))
                         (if (null? ls) (reverse acc)
                           (loop (cdr ls) (+ n 1)
                             (cons (string-append (number->string n) ": " (car ls)) acc)))))
             (result (string-join numbered "\n")))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed result)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

;; cmd-insert-kbd-macro and cmd-name-last-kbd-macro moved to qt/commands-edit.ss

(def (cmd-show-environment app)
  "Show environment variables."
  (let* ((fr (app-state-frame app))
         (ed (qt-current-editor fr))
         (out (open-process (list path: "env" arguments: '())))
         (text (read-line out #f)))
    (close-port out)
    (when (string? text)
      (let ((buf (qt-buffer-create! "*Environment*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed text)))))

(def (cmd-show-keybinding-for app)
  "Show keybinding for a command."
  (cmd-where-is app))

(def (cmd-first-error app)
  "Jump to the first search match."
  (let* ((ed (current-qt-editor app))
         (search (app-state-last-search app)))
    (if search
      (let* ((text (qt-plain-text-edit-text ed))
             (found (string-contains text search)))
        (if found
          (begin
            (qt-plain-text-edit-set-cursor-position! ed found)
            (echo-message! (app-state-echo app) (string-append "First: " search)))
          (echo-error! (app-state-echo app) "Not found")))
      (echo-error! (app-state-echo app) "No search"))))

(def (cmd-find-grep app)
  "Run grep on files (using shell grep command)."
  (let ((pat (qt-echo-read-string app "Grep for: ")))
    (when pat
      (let ((dir (qt-echo-read-string app "In directory: ")))
        (when dir
          (let* ((fr (app-state-frame app))
                 (ed (qt-current-editor fr))
                 (out (open-process
                        (list path: "grep" arguments: (list "-rn" pat dir)
                              stderr-redirection: #t)))
                 (text (read-line out #f)))
            (close-port out)
            (let ((buf (qt-buffer-create! "*Grep*" ed #f)))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed (or text "No results")))))))))

;; cmd-project-grep moved below with project commands

(def (project-list-files root)
  "List files in project ROOT using find, excluding common build/vcs dirs."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/find"
                           arguments: (list root
                             "-type" "f"
                             "-not" "-path" "*/.git/*"
                             "-not" "-path" "*/.gerbil/*"
                             "-not" "-path" "*/node_modules/*"
                             "-not" "-path" "*/__pycache__/*"
                             "-not" "-path" "*/target/*"
                             "-not" "-path" "*/.build/*"
                             "-not" "-name" "*.o"
                             "-not" "-name" "*.o1")
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        (if output
          (let loop ((s output) (acc []))
            (let ((nl (string-index s #\newline)))
              (if nl
                (loop (substring s (+ nl 1) (string-length s))
                      (cons (substring s 0 nl) acc))
                (reverse (if (> (string-length s) 0) (cons s acc) acc)))))
          [])))))

(def (cmd-project-find-file app)
  "Find file in the project directory with completion."
  (let* ((root (current-project-root app))
         (files (project-list-files root))
         ;; Make paths relative to project root for nicer display
         (prefix-len (+ (string-length root) (if (char=? (string-ref root (- (string-length root) 1)) #\/) 0 1)))
         (relative-files (map (lambda (f)
                                (if (> (string-length f) prefix-len)
                                  (substring f prefix-len (string-length f))
                                  f))
                              files))
         (input (qt-echo-read-with-narrowing app
                  (string-append "Find file in " (path-strip-directory root) ": ")
                  relative-files)))
    (when (and input (> (string-length input) 0))
      (let ((full-path (path-expand input root)))
        (recent-files-add! full-path)
        (let* ((name (path-strip-directory full-path))
               (fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (qt-buffer-create! name ed full-path)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? full-path)
            (let ((text (read-file-as-string full-path)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)))
            (file-mtime-record! full-path))
          (qt-setup-highlighting! app buf)
          (echo-message! (app-state-echo app) (string-append "Opened: " full-path)))))))

(def (cmd-project-compile app)
  "Compile the project from project root."
  (let* ((root (current-project-root app))
         (default (or (app-state-last-compile app)
                      (cond
                        ((file-exists? (path-expand "gerbil.pkg" root)) "gerbil build")
                        ((file-exists? (path-expand "Makefile" root)) "make")
                        ((file-exists? (path-expand "Cargo.toml" root)) "cargo build")
                        (else "make"))))
         (cmd (qt-echo-read-string app
                (string-append "Compile in " root " [" default "]: "))))
    (when cmd
      (let ((actual-cmd (if (string=? cmd "") default cmd)))
        (set! (app-state-last-compile app) actual-cmd)
        (compilation-run-command! app (string-append "cd " root " && " actual-cmd))))))

(def (cmd-project-grep app)
  "Grep in the project directory."
  (let* ((root (current-project-root app))
         (pattern (qt-echo-read-string app
                    (string-append "Grep in " (path-strip-directory root) ": "))))
    (when (and pattern (> (string-length pattern) 0))
      (grep-run-and-show! app pattern root '("-rn")))))

(def (cmd-reindent-buffer app)
  "Re-indent the entire buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         ;; Simple: re-indent based on paren depth
         (reindented
           (let loop ((ls lines) (depth 0) (acc []))
             (if (null? ls) (reverse acc)
               (let* ((line (car ls))
                      (trimmed (string-trim line))
                      ;; Count closing parens at start
                      (close-first (let cloop ((i 0) (d 0))
                                     (if (>= i (string-length trimmed)) d
                                       (case (string-ref trimmed i)
                                         ((#\) #\] #\}) (cloop (+ i 1) (+ d 1)))
                                         (else d)))))
                      (this-depth (max 0 (- depth close-first)))
                      (indent (make-string (* this-depth 2) #\space))
                      (new-line (string-append indent trimmed))
                      ;; Count net depth change
                      (delta (let dloop ((i 0) (d 0))
                               (if (>= i (string-length trimmed)) d
                                 (case (string-ref trimmed i)
                                   ((#\( #\[ #\{) (dloop (+ i 1) (+ d 1)))
                                   ((#\) #\] #\}) (dloop (+ i 1) (- d 1)))
                                   (else (dloop (+ i 1) d)))))))
                 (loop (cdr ls) (max 0 (+ depth delta)) (cons new-line acc)))))))
    (qt-plain-text-edit-set-text! ed (string-join reindented "\n"))
    (echo-message! (app-state-echo app) "Buffer re-indented")))

(def (cmd-fill-individual-paragraphs app)
  "Fill each paragraph in the region individually."
  (cmd-fill-paragraph app))

;;;============================================================================
;;; Xref find-definitions / find-references (grep-based, non-LSP)

(def (cmd-xref-find-definitions app)
  "Find definitions of symbol at point (alias for goto-definition)."
  (cmd-goto-definition app))

(def (cmd-xref-find-references app)
  "Find references to symbol at point using grep."
  (let* ((ed (current-qt-editor app))
         (sym (symbol-at-point ed)))
    (if (not sym)
      (echo-error! (app-state-echo app) "No symbol at point")
      (let* ((root (current-project-root app))
             (proc (open-process
                     (list path: "/usr/bin/grep"
                           arguments: (list "-rn" sym
                             "--include=*.ss" "--include=*.scm"
                             root)
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (close-port proc)))
        (if (not output)
          (echo-error! (app-state-echo app) (string-append "No references: " sym))
          (let* ((lines (string-split output #\newline))
                 (filtered (filter (lambda (l) (> (string-length l) 0)) lines))
                 (count (length filtered)))
            (echo-message! (app-state-echo app)
              (string-append "References to " sym ": " (number->string count) " matches"))))))))

;;;============================================================================
;;; Number-to-register

(def (cmd-number-to-register app)
  "Store a number in a register."
  (let ((reg (qt-echo-read-string app "Register (a-z): ")))
    (when (and reg (> (string-length reg) 0))
      (let* ((key (string->symbol reg))
             (registers (app-state-registers app))
             (arg (get-prefix-arg app)))
        (hash-put! registers key arg)
        (echo-message! (app-state-echo app)
          (string-append "Register " reg " = " (number->string arg)))))))

