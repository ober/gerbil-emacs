;;; -*- Gerbil -*-
;;; Qt commands search - search, compile, sort, region operations
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gerbil-emacs/qt/sci-shim
        :gerbil-emacs/core
        :gerbil-emacs/editor
        :gerbil-emacs/repl
        :gerbil-emacs/eshell
        :gerbil-emacs/shell
        :gerbil-emacs/terminal
        :gerbil-emacs/qt/buffer
        :gerbil-emacs/qt/window
        :gerbil-emacs/qt/echo
        :gerbil-emacs/qt/highlight
        :gerbil-emacs/qt/modeline
        :gerbil-emacs/qt/commands-core
        :gerbil-emacs/qt/commands-edit)

;;;============================================================================
;;; Mark word
;;;============================================================================

(def (cmd-mark-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (if start
        (let ((buf (current-qt-buffer app)))
          (set! (buffer-mark buf) start)
          (qt-plain-text-edit-set-cursor-position! ed end)
          (echo-message! (app-state-echo app) "Word marked"))
        (echo-message! (app-state-echo app) "No word at point")))))

;;;============================================================================
;;; Save some buffers
;;;============================================================================

(def (cmd-save-some-buffers app)
  (let ((echo (app-state-echo app))
        (saved 0))
    (for-each
      (lambda (buf)
        (let ((path (buffer-file-path buf)))
          (when (and path
                     (buffer-doc-pointer buf)
                     (qt-text-document-modified? (buffer-doc-pointer buf)))
            ;; Find window showing this buffer to get text
            (let loop ((wins (qt-frame-windows (app-state-frame app))))
              (when (pair? wins)
                (if (eq? (qt-edit-window-buffer (car wins)) buf)
                  (let* ((ed (qt-edit-window-editor (car wins)))
                         (txt (qt-plain-text-edit-text ed)))
                    (write-string-to-file path txt)
                    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                    (set! saved (+ saved 1)))
                  (loop (cdr wins))))))))
      (buffer-list))
    (echo-message! echo
      (if (= saved 0) "No buffers need saving"
        (string-append "Saved " (number->string saved) " buffer(s)")))))

;;;============================================================================
;;; Compile
;;;============================================================================

;; Compilation error state
(def *compilation-errors* [])   ; list of (file line col message)
(def *compilation-error-index* -1)

;; Grep results state
(def *grep-results* [])        ; list of (file line text)
(def *grep-result-index* -1)

(def (parse-compilation-errors text)
  "Parse compilation output for file:line:col error locations.
Returns list of (file line col message) tuples."
  (let ((errors [])
        (len (string-length text)))
    (let line-loop ((i 0))
      (when (< i len)
        (let ((line-end (let scan ((j i))
                          (cond
                            ((>= j len) j)
                            ((char=? (string-ref text j) #\newline) j)
                            (else (scan (+ j 1)))))))
          (let ((line (substring text i line-end)))
            (let ((parsed (parse-error-line line)))
              (when parsed
                (set! errors (cons parsed errors)))))
          (line-loop (+ line-end 1)))))
    (reverse errors)))

(def (parse-error-line line)
  "Try to parse a single line for file:line:col patterns.
Returns (file line col message) or #f."
  (let ((len (string-length line)))
    (or
      ;; Pattern 1: Python — File \"path\", line N
      (let ((prefix "File \""))
        (and (> len (string-length prefix))
             (string-prefix? prefix line)
             (let ((quote-end (string-index line #\" (string-length prefix))))
               (and quote-end
                    (let* ((file (substring line (string-length prefix) quote-end))
                           (rest (substring line quote-end len)))
                      (and (string-prefix? "\", line " rest)
                           (let* ((num-start 8)
                                  (num-str (let scan ((j num-start))
                                             (if (and (< j (string-length rest))
                                                      (char-numeric? (string-ref rest j)))
                                               (scan (+ j 1))
                                               (substring rest num-start j)))))
                             (and (> (string-length num-str) 0)
                                  (let ((ln (string->number num-str)))
                                    (and ln
                                         (file-exists? file)
                                         (list file ln 1 (string-trim-both line))))))))))))
      ;; Pattern 2: file:line:col: message (GCC, Clang, Gerbil)
      (let find-first-colon ((start (if (and (> len 2) (char=? (string-ref line 1) #\:)) 2 0)))
        (let ((colon1 (string-index line #\: start)))
          (and colon1
               (> colon1 0)
               (let* ((file (substring line 0 colon1))
                      (rest1 (substring line (+ colon1 1) len)))
                 (let ((line-num-str (let scan ((j 0))
                                       (if (and (< j (string-length rest1))
                                                (char-numeric? (string-ref rest1 j)))
                                         (scan (+ j 1))
                                         (substring rest1 0 j)))))
                   (if (= (string-length line-num-str) 0)
                     (find-first-colon (+ colon1 1))
                     (let ((ln (string->number line-num-str)))
                       (and ln (> ln 0)
                            (or (file-exists? file)
                                (string-prefix? "/" file)
                                (string-prefix? "./" file)
                                (string-prefix? "../" file))
                            (let* ((after-line (string-length line-num-str))
                                   (has-col (and (< after-line (string-length rest1))
                                                 (char=? (string-ref rest1 after-line) #\:)))
                                   (col-and-msg
                                    (if has-col
                                      (let* ((rest2 (substring rest1 (+ after-line 1)
                                                               (string-length rest1)))
                                             (col-str (let scan ((j 0))
                                                        (if (and (< j (string-length rest2))
                                                                 (char-numeric? (string-ref rest2 j)))
                                                          (scan (+ j 1))
                                                          (substring rest2 0 j)))))
                                        (if (> (string-length col-str) 0)
                                          (cons (string->number col-str)
                                                (let ((mstart (string-length col-str)))
                                                  (if (and (< mstart (string-length rest2))
                                                           (char=? (string-ref rest2 mstart) #\:))
                                                    (string-trim-both
                                                      (substring rest2 (+ mstart 1) (string-length rest2)))
                                                    (string-trim-both
                                                      (substring rest2 mstart (string-length rest2))))))
                                          (cons 1 (string-trim-both rest2))))
                                      (let ((msg-start after-line))
                                        (cons 1 (if (< msg-start (string-length rest1))
                                                  (string-trim-both
                                                    (substring rest1 msg-start (string-length rest1)))
                                                  "")))))
                                   (col (car col-and-msg))
                                   (msg (cdr col-and-msg)))
                              (list file ln col
                                    (if (string=? msg "") line msg))))))))))))))

(def (compilation-run-command! app cmd)
  "Run a compile command, display output in *compilation* buffer, parse errors."
  (let* ((echo (app-state-echo app))
         (result+status
          (with-catch
            (lambda (e)
              (cons (string-append "Error: "
                      (with-output-to-string
                        (lambda () (display-exception e))))
                    -1))
            (lambda ()
              (let ((port (open-process
                            (list path: "/bin/sh"
                                  arguments: ["-c" cmd]
                                  stdout-redirection: #t
                                  stderr-redirection: #t
                                  pseudo-terminal: #f))))
                (let* ((output (read-line port #f))
                       (status (process-status port)))
                  (close-port port)
                  (cons (or output "") status))))))
         (result (car result+status))
         (status (cdr result+status))
         (errors (parse-compilation-errors result))
         (header (string-append
                   "-*- Compilation -*-\n"
                   "Command: " cmd "\n"
                   (make-string 60 #\-)
                   "\n\n"))
         (footer (string-append
                   "\n" (make-string 60 #\-) "\n"
                   "Compilation "
                   (if (= status 0) "finished" "exited abnormally")
                   (if (= status 0) ""
                     (string-append " (exit " (number->string status) ")"))
                   (if (null? errors) ""
                     (string-append " — "
                       (number->string (length errors)) " error location(s)"))
                   "\n"))
         (text (string-append header result footer))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (or (buffer-by-name "*compilation*")
                  (qt-buffer-create! "*compilation*" ed #f))))
    (set! *compilation-errors* errors)
    (set! *compilation-error-index* -1)
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! echo
      (string-append "Compilation "
        (if (= status 0) "finished" "failed")
        (if (null? errors) ""
          (string-append " — " (number->string (length errors)) " error(s)"))))))

(def (cmd-compile app)
  "Run a compile command and display output in *compilation* buffer with error parsing."
  (let* ((echo (app-state-echo app))
         (default (or (app-state-last-compile app) "make"))
         (cmd (qt-echo-read-string app
                (string-append "Compile command [" default "]: "))))
    (when cmd
      (let ((actual-cmd (if (string=? cmd "") default cmd)))
        (set! (app-state-last-compile app) actual-cmd)
        (compilation-run-command! app actual-cmd)))))

(def (cmd-recompile app)
  "Re-run the last compile command without prompting."
  (let ((cmd (app-state-last-compile app)))
    (if (not cmd)
      (cmd-compile app)
      (compilation-run-command! app cmd))))

;;;============================================================================
;;; Compile-on-save
;;;============================================================================

(def *compile-on-save* #f)  ;; off by default

(def (compile-on-save-check! app path)
  "If compile-on-save is enabled and PATH is a .ss file in a Gerbil project,
   run gerbil build in the background."
  (when (and *compile-on-save* path
             (string-suffix? ".ss" path))
    ;; Find gerbil.pkg by searching upward
    (let loop ((dir (path-directory path)))
      (when (and (> (string-length dir) 0) (not (string=? dir "/")))
        (let ((pkg (path-expand "gerbil.pkg" dir)))
          (if (file-exists? pkg)
            ;; Found gerbil project — run build
            (begin
              (echo-message! (app-state-echo app) "Auto-compiling...")
              (compilation-run-command! app
                (string-append "cd " dir " && gerbil build")))
            ;; Go up one level
            (let ((parent (path-directory
                            (let ((d (if (string-suffix? "/" dir)
                                       (substring dir 0 (- (string-length dir) 1))
                                       dir)))
                              d))))
              (unless (string=? parent dir)
                (loop parent)))))))))

;;;============================================================================
;;; Flycheck-style live syntax checking
;;;============================================================================

(def *flycheck-mode* #f)        ; global toggle
(def *flycheck-errors* [])      ; list of (file line col message)
(def *flycheck-error-idx* 0)    ; index for next/prev error navigation

(def (flycheck-check! app path)
  "Run gxc -S on a Gerbil file and parse errors. Updates *flycheck-errors*."
  (when (and *flycheck-mode* path (string-suffix? ".ss" path))
    ;; Find GERBIL_LOADPATH from project context
    (let* ((dir (path-directory path))
           (loadpath (flycheck-find-loadpath dir))
           (env-prefix (if loadpath
                         (string-append "GERBIL_LOADPATH=" loadpath " ")
                         ""))
           (cmd (string-append env-prefix "gxc -S " path " 2>&1"))
           (result
             (with-catch
               (lambda (e)
                 (string-append "flycheck error: "
                   (with-output-to-string (lambda () (display-exception e)))))
               (lambda ()
                 (let ((port (open-process
                               (list path: "/bin/sh"
                                     arguments: ["-c" cmd]
                                     stdout-redirection: #t
                                     stderr-redirection: #f
                                     pseudo-terminal: #f))))
                   (let* ((output (read-line port #f))
                          (status (process-status port)))
                     (close-port port)
                     (or output "")))))))
      ;; Parse errors from gxc output
      (let ((errors (flycheck-parse-errors result path)))
        (set! *flycheck-errors* errors)
        (set! *flycheck-error-idx* 0)
        (if (null? errors)
          (echo-message! (app-state-echo app) "Flycheck: no errors")
          (let* ((count (length errors))
                 (first-err (car errors))
                 (msg (string-append "Flycheck: "
                        (number->string count)
                        (if (= count 1) " error" " errors")
                        " — " (cadddr first-err))))
            (echo-error! (app-state-echo app) msg)))))))

(def (flycheck-find-loadpath dir)
  "Search upward from dir for gerbil.pkg and construct GERBIL_LOADPATH."
  (let loop ((d dir))
    (cond
      ((or (string=? d "") (string=? d "/")) #f)
      ((file-exists? (path-expand "gerbil.pkg" d))
       ;; Found project root, check for .gerbil/lib
       (let ((local-lib (path-expand ".gerbil/lib" d)))
         (if (file-exists? local-lib)
           (string-append local-lib ":" (or (getenv "GERBIL_LOADPATH" #f) ""))
           (or (getenv "GERBIL_LOADPATH" #f) #f))))
      (else
       (let ((parent (path-directory
                       (if (string-suffix? "/" d)
                         (substring d 0 (- (string-length d) 1))
                         d))))
         (if (string=? parent d) #f
           (loop parent)))))))

(def (flycheck-parse-errors output file)
  "Parse gxc error output into list of (file line col message).
   Handles formats like:
   - '... form: foo'
   - 'at file:line:col'
   - 'Syntax Error: ...'
   - 'Error: ...' "
  (let* ((lines (string-split output #\newline))
         (errors []))
    (for-each
      (lambda (line)
        (cond
          ;; Match "--- Syntax Error: ..." messages
          ((string-contains line "Syntax Error:")
           (let ((msg (substring line
                        (+ (string-contains line "Syntax Error:") 15)
                        (string-length line))))
             (set! errors (cons (list file 0 0 (string-trim msg)) errors))))
          ;; Match "--- Error: ..."
          ((and (string-prefix? "---" line) (string-contains line "Error:"))
           (let ((msg (substring line 4 (string-length line))))
             (set! errors (cons (list file 0 0 (string-trim msg)) errors))))
          ;; Match "... form: something" or "... detail: something"
          ((and (string-prefix? "..." line)
                (or (string-contains line "form:")
                    (string-contains line "detail:")))
           (let* ((trimmed (string-trim line))
                  (msg (if (> (string-length trimmed) 4)
                         (substring trimmed 4 (string-length trimmed))
                         trimmed)))
             (set! errors (cons (list file 0 0 msg) errors))))))
      lines)
    (reverse errors)))

(def (cmd-flycheck-mode app)
  "Toggle flycheck (live syntax checking on save) for Gerbil files."
  (set! *flycheck-mode* (not *flycheck-mode*))
  (echo-message! (app-state-echo app)
    (if *flycheck-mode* "Flycheck mode enabled" "Flycheck mode disabled")))

(def (cmd-flycheck-next-error app)
  "Jump to the next flycheck error."
  (if (null? *flycheck-errors*)
    (echo-message! (app-state-echo app) "No flycheck errors")
    (let* ((idx (min *flycheck-error-idx* (- (length *flycheck-errors*) 1)))
           (err (list-ref *flycheck-errors* idx))
           (line (cadr err))
           (msg (cadddr err)))
      (when (> line 0)
        ;; Jump to error line
        (let* ((ed (current-qt-editor app))
               (text (qt-plain-text-edit-text ed))
               (target-pos (text-line-position text (- line 1))))
          (qt-plain-text-edit-set-cursor-position! ed target-pos)
          (qt-plain-text-edit-ensure-cursor-visible! ed)))
      (echo-error! (app-state-echo app)
        (string-append "[" (number->string (+ idx 1)) "/"
                       (number->string (length *flycheck-errors*))
                       "] " msg))
      (set! *flycheck-error-idx* (min (+ idx 1) (- (length *flycheck-errors*) 1))))))

(def (cmd-flycheck-prev-error app)
  "Jump to the previous flycheck error."
  (if (null? *flycheck-errors*)
    (echo-message! (app-state-echo app) "No flycheck errors")
    (let* ((idx (max (- *flycheck-error-idx* 1) 0))
           (err (list-ref *flycheck-errors* idx))
           (line (cadr err))
           (msg (cadddr err)))
      (when (> line 0)
        (let* ((ed (current-qt-editor app))
               (text (qt-plain-text-edit-text ed))
               (target-pos (text-line-position text (- line 1))))
          (qt-plain-text-edit-set-cursor-position! ed target-pos)
          (qt-plain-text-edit-ensure-cursor-visible! ed)))
      (echo-error! (app-state-echo app)
        (string-append "[" (number->string (+ idx 1)) "/"
                       (number->string (length *flycheck-errors*))
                       "] " msg))
      (set! *flycheck-error-idx* idx))))

(def (cmd-flycheck-list-errors app)
  "Show all flycheck errors in a buffer."
  (if (null? *flycheck-errors*)
    (echo-message! (app-state-echo app) "No flycheck errors")
    (let* ((fr (app-state-frame app))
           (ed (current-qt-editor app))
           (text (string-join
                   (map (lambda (err)
                          (let ((file (car err))
                                (line (cadr err))
                                (col (caddr err))
                                (msg (cadddr err)))
                            (string-append
                              (path-strip-directory file) ":"
                              (number->string line) ":"
                              (number->string col) ": "
                              msg)))
                        *flycheck-errors*)
                   "\n"))
           (buf (or (buffer-by-name "*Flycheck Errors*")
                    (qt-buffer-create! "*Flycheck Errors*" ed #f))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

(def (cmd-toggle-compile-on-save app)
  "Toggle automatic compilation when saving Gerbil files."
  (set! *compile-on-save* (not *compile-on-save*))
  (echo-message! (app-state-echo app)
    (if *compile-on-save*
      "Compile-on-save enabled"
      "Compile-on-save disabled")))

;;;============================================================================
;;; Where-is (find key binding for command)
;;;============================================================================

(def (cmd-where-is app)
  (let ((input (qt-echo-read-string app "Where is command: ")))
    (when (and input (> (string-length input) 0))
      (let ((sym (string->symbol input))
            (found #f))
        (for-each
          (lambda (entry)
            (let ((key (car entry))
                  (val (cdr entry)))
              (cond
                ((eq? val sym)
                 (set! found key))
                ((hash-table? val)
                 (for-each
                   (lambda (sub-entry)
                     (when (eq? (cdr sub-entry) sym)
                       (set! found (string-append key " " (car sub-entry)))))
                   (keymap-entries val))))))
          (keymap-entries *global-keymap*))
        (if found
          (echo-message! (app-state-echo app)
            (string-append input " is on " found))
          (echo-message! (app-state-echo app)
            (string-append input " is not on any key")))))))

;;;============================================================================
;;; Flush lines / keep lines
;;;============================================================================

(def (cmd-flush-lines app)
  "Delete lines matching a pattern."
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Flush lines matching: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (kept (filter (lambda (l) (not (string-contains l pattern))) lines))
             (removed (- (length lines) (length kept)))
             (new-text (string-join kept "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! echo
          (string-append "Removed " (number->string removed) " lines"))))))

(def (cmd-keep-lines app)
  "Keep only lines matching a pattern."
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Keep lines matching: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (kept (filter (lambda (l) (string-contains l pattern)) lines))
             (new-text (string-join kept "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! echo
          (string-append "Kept " (number->string (length kept)) " lines"))))))

;;;============================================================================
;;; Number lines
;;;============================================================================

(def (cmd-number-lines app)
  "Prefix each line with its line number."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (numbered (let loop ((ls lines) (i 1) (acc []))
                     (if (null? ls) (reverse acc)
                       (loop (cdr ls) (+ i 1)
                             (cons (string-append (number->string i) ": " (car ls))
                                   acc)))))
         (new-text (string-join numbered "\n")))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;;============================================================================
;;; Reverse region
;;;============================================================================

(def (cmd-reverse-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (reversed (reverse lines))
             (new-region (string-join reversed "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region reversed"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Toggle read-only
;;;============================================================================

(def (cmd-toggle-read-only app)
  (let* ((ed (current-qt-editor app))
         (ro (qt-plain-text-edit-read-only? ed)))
    (qt-plain-text-edit-set-read-only! ed (not ro))
    (echo-message! (app-state-echo app)
      (if ro "Read-only OFF" "Read-only ON"))))

;;;============================================================================
;;; Rename buffer
;;;============================================================================

(def (cmd-rename-buffer app)
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (input (qt-echo-read-string app
                  (string-append "Rename buffer (" (buffer-name buf) "): "))))
    (when (and input (> (string-length input) 0))
      (set! (buffer-name buf) input)
      (echo-message! echo (string-append "Buffer renamed to " input)))))

;;;============================================================================
;;; Switch buffer / find file in other window
;;;============================================================================

(def (cmd-switch-buffer-other-window app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (names (buffer-names-mru))
         (name (qt-echo-read-string-with-completion app
                  "Switch to buffer in other window: " names)))
    (when name
      (let ((buf (buffer-by-name name)))
        (when buf
          (buffer-touch! buf)
          (let ((wins (qt-frame-windows fr)))
            (if (> (length wins) 1)
              ;; Switch in the other window
              (begin
                (qt-frame-other-window! fr)
                (let ((ed (current-qt-editor app)))
                  (qt-buffer-attach! ed buf)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) buf)))
              ;; Only one window: split first
              (let ((new-ed (qt-frame-split! fr)))
                (when (app-state-key-handler app)
                  ((app-state-key-handler app) new-ed))
                (qt-buffer-attach! new-ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)))))))))

(def (cmd-find-file-other-window app)
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (filename (qt-echo-read-string app "Find file in other window: ")))
    (when (and filename (> (string-length filename) 0))
      (let ((wins (qt-frame-windows fr)))
        (when (<= (length wins) 1)
          ;; Split first
          (let ((new-ed (qt-frame-split! fr)))
            (when (app-state-key-handler app)
              ((app-state-key-handler app) new-ed))))
        ;; Switch to other window
        (qt-frame-other-window! fr)
        ;; Open file
        (let* ((name (path-strip-directory filename))
               (ed (current-qt-editor app))
               (buf (qt-buffer-create! name ed filename)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (file-exists? filename)
            (let ((text (read-file-as-string filename)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0))))
          (qt-setup-highlighting! app buf)
          (echo-message! echo (string-append "Opened: " filename)))))))

;;;============================================================================
;;; Insert date
;;;============================================================================

(def (cmd-insert-date app)
  (let* ((now (current-time))
         (secs (time->seconds now))
         ;; Simple date string: YYYY-MM-DD HH:MM:SS
         (date-str (with-catch
                     (lambda (e)
                       ;; Fallback: just show seconds since epoch
                       (number->string (inexact->exact (truncate secs))))
                     (lambda ()
                       (let* ((utc (seconds->time secs))
                              (port (open-process
                                      (list path: "/bin/date"
                                            arguments: ["+%Y-%m-%d %H:%M:%S"]
                                            stdout-redirection: #t))))
                         (let ((result (read-line port)))
                           (close-port port)
                           result))))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) date-str)))

;;;============================================================================
;;; Eval buffer / eval region
;;;============================================================================

(def (cmd-eval-buffer app)
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((result error?) (eval-expression-string text)))
      (if error?
        (echo-error! echo result)
        (echo-message! echo (string-append "=> " result))))))

(def (cmd-eval-region app)
  (let* ((echo (app-state-echo app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end)))
        (set! (buffer-mark buf) #f)
        (let-values (((result error?) (eval-expression-string region)))
          (if error?
            (echo-error! echo result)
            (echo-message! echo (string-append "=> " result)))))
      (echo-error! echo "No mark set"))))

;;;============================================================================
;;; Clone buffer / scratch buffer
;;;============================================================================

(def (cmd-clone-buffer app)
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (fr (app-state-frame app))
         (text (qt-plain-text-edit-text ed))
         (name (string-append (buffer-name buf) "<2>"))
         (new-buf (qt-buffer-create! name ed (buffer-file-path buf))))
    (qt-buffer-attach! ed new-buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) new-buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer new-buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append "Cloned to " name))))

(def (cmd-scratch-buffer app)
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (existing (buffer-by-name "*scratch*")))
    (if existing
      (begin
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing))
      (let ((buf (qt-buffer-create! "*scratch*" ed #f)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (qt-plain-text-edit-set-text! ed ";; *scratch*\n")
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))
    (echo-message! (app-state-echo app) "*scratch*")))

;;;============================================================================
;;; Delete duplicate lines
;;;============================================================================

(def (cmd-delete-duplicate-lines app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (seen (make-hash-table))
         (unique (filter (lambda (l)
                           (if (hash-get seen l) #f
                             (begin (hash-put! seen l #t) #t)))
                         lines))
         (removed (- (length lines) (length unique)))
         (new-text (string-join unique "\n")))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append "Removed " (number->string removed) " duplicate lines"))))

;;;============================================================================
;;; Count matches
;;;============================================================================

(def (cmd-count-matches app)
  (let* ((echo (app-state-echo app))
         (pattern (qt-echo-read-string app "Count matches: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (count (let loop ((i 0) (c 0))
                      (let ((pos (string-contains text pattern i)))
                        (if pos
                          (loop (+ pos (string-length pattern)) (+ c 1))
                          c)))))
        (echo-message! echo
          (string-append (number->string count) " occurrences of \"" pattern "\""))))))

;;;============================================================================
;;; Count lines in region
;;;============================================================================

(def (cmd-count-lines-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (chars (string-length region)))
        (echo-message! (app-state-echo app)
          (string-append "Region has " (number->string (length lines))
                         " lines, " (number->string chars) " chars")))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Diff mode with colorized output
;;;============================================================================

(def (qt-highlight-diff! ed)
  "Apply diff-mode highlighting to a QPlainTextEdit.
+lines = green bg, -lines = red bg, @@ headers = blue bg."
  (let ((text (qt-plain-text-edit-text ed))
        (len 0))
    (set! len (string-length text))
    ;; Clear extra selections first
    (qt-extra-selections-clear! ed)
    ;; Walk through lines and highlight
    (let line-loop ((i 0))
      (when (< i len)
        (let* ((line-end (let scan ((j i))
                           (cond
                             ((>= j len) j)
                             ((char=? (string-ref text j) #\newline) j)
                             (else (scan (+ j 1))))))
               (line-len (- line-end i)))
          (when (> line-len 0)
            (let ((ch (string-ref text i)))
              (cond
                ;; +line (added) — green background
                ((and (char=? ch #\+)
                      ;; Skip +++ header line
                      (not (and (> line-len 2)
                                (char=? (string-ref text (+ i 1)) #\+)
                                (char=? (string-ref text (+ i 2)) #\+))))
                 (qt-extra-selection-add-range! ed i line-len
                   220 255 220    ; light green text
                   0 60 0         ; dark green bg
                   bold: #f))
                ;; -line (removed) — red background
                ((and (char=? ch #\-)
                      ;; Skip --- header line
                      (not (and (> line-len 2)
                                (char=? (string-ref text (+ i 1)) #\-)
                                (char=? (string-ref text (+ i 2)) #\-))))
                 (qt-extra-selection-add-range! ed i line-len
                   255 200 200    ; light red text
                   80 0 0         ; dark red bg
                   bold: #f))
                ;; @@ hunk header — blue background
                ((and (char=? ch #\@)
                      (> line-len 1)
                      (char=? (string-ref text (+ i 1)) #\@))
                 (qt-extra-selection-add-range! ed i line-len
                   180 200 255    ; light blue text
                   0 0 80         ; dark blue bg
                   bold: #t))
                ;; --- or +++ file header — bold
                ((or (and (char=? ch #\-)
                          (> line-len 2)
                          (char=? (string-ref text (+ i 1)) #\-)
                          (char=? (string-ref text (+ i 2)) #\-))
                     (and (char=? ch #\+)
                          (> line-len 2)
                          (char=? (string-ref text (+ i 1)) #\+)
                          (char=? (string-ref text (+ i 2)) #\+)))
                 (qt-extra-selection-add-range! ed i line-len
                   255 255 100    ; yellow text
                   40 40 40       ; dark gray bg
                   bold: #t)))))
          (line-loop (+ line-end 1)))))))

(def (cmd-diff-buffer-with-file app)
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (and path (file-exists? path))
      (let* ((ed (current-qt-editor app))
             (current-text (qt-plain-text-edit-text ed))
             (file-text (read-file-as-string path))
             ;; Write current buffer to temp file for diff
             (tmp-path (string-append "/tmp/gerbil-emacs-diff-" (number->string (random-integer 100000))))
             (_ (write-string-to-file tmp-path current-text))
             (result (with-catch
                       (lambda (e) "Error running diff")
                       (lambda ()
                         (let ((port (open-process
                                       (list path: "/usr/bin/diff"
                                             arguments: ["-u" path tmp-path]
                                             stdout-redirection: #t
                                             stderr-redirection: #t
                                             pseudo-terminal: #f))))
                           (let ((output (read-line port #f)))
                             (close-port port)
                             (or output "No differences")))))))
        ;; Clean up temp file
        (with-catch void (lambda () (delete-file tmp-path)))
        ;; Show diff in buffer
        (let* ((fr (app-state-frame app))
               (diff-buf (or (buffer-by-name "*Diff*")
                             (qt-buffer-create! "*Diff*" ed #f))))
          (qt-buffer-attach! ed diff-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
          (qt-plain-text-edit-set-text! ed
            (if (string=? result "") "No differences\n" result))
          (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (qt-highlight-diff! ed)))
      (echo-error! echo "Buffer is not visiting a file"))))

(def (cmd-diff-next-hunk app)
  "Jump to next diff hunk (@@ line)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    ;; Find next @@ after current position
    (let loop ((i (+ pos 1)))
      (cond
        ((>= i (- len 1))
         (echo-message! (app-state-echo app) "No more hunks"))
        ((and (char=? (string-ref text i) #\@)
              (char=? (string-ref text (+ i 1)) #\@)
              ;; Make sure it's at start of line
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (qt-plain-text-edit-set-cursor-position! ed i)
         (qt-plain-text-edit-ensure-cursor-visible! ed))
        (else (loop (+ i 1)))))))

(def (cmd-diff-prev-hunk app)
  "Jump to previous diff hunk (@@ line)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Find the start of current line, then search backward
    (let ((line-start (let scan ((i (max 0 (- pos 1))))
                        (cond
                          ((<= i 0) 0)
                          ((char=? (string-ref text i) #\newline) (+ i 1))
                          (else (scan (- i 1)))))))
      (let loop ((i (- line-start 2)))
        (cond
          ((< i 1)
           (echo-message! (app-state-echo app) "No previous hunks"))
          ((and (char=? (string-ref text i) #\@)
                (char=? (string-ref text (+ i 1)) #\@)
                (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
           (qt-plain-text-edit-set-cursor-position! ed i)
           (qt-plain-text-edit-ensure-cursor-visible! ed))
          (else (loop (- i 1))))))))

;;;============================================================================
;;; Grep buffer (search all matching lines)
;;;============================================================================

(def (cmd-grep-buffer app)
  (let* ((echo (app-state-echo app))
         (query (qt-echo-read-string app "Grep buffer: ")))
    (when (and query (> (string-length query) 0))
      ;; Same as occur but searches case-insensitively
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (q-lower (string-downcase query))
             (matches (let loop ((ls lines) (i 1) (acc []))
                        (if (null? ls) (reverse acc)
                          (if (string-contains (string-downcase (car ls)) q-lower)
                            (loop (cdr ls) (+ i 1)
                                  (cons (string-append (number->string i) ": " (car ls))
                                        acc))
                            (loop (cdr ls) (+ i 1) acc)))))
             (result (if (null? matches)
                       (string-append "No matches for: " query)
                       (string-append (number->string (length matches))
                                      " matches for: " query "\n\n"
                                      (string-join matches "\n")))))
        (let* ((fr (app-state-frame app))
               (buf (or (buffer-by-name "*Grep*")
                        (qt-buffer-create! "*Grep*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; Revert buffer quick (no prompt)
;;;============================================================================

(def (cmd-revert-buffer-quick app)
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (echo (app-state-echo app)))
    (if (and path (file-exists? path))
      (let* ((ed (current-qt-editor app))
             (text (read-file-as-string path)))
        (when text
          (qt-plain-text-edit-set-text! ed text)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (echo-message! echo (string-append "Reverted " path))))
      (echo-error! echo "Buffer is not visiting a file"))))

;;;============================================================================
;;; Shell command on region (M-|)
;;;============================================================================

(def (cmd-shell-command-on-region app)
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((cmd (qt-echo-read-string app "Shell command on region: ")))
        (when (and cmd (> (string-length cmd) 0))
          (let* ((ed (current-qt-editor app))
                 (pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (result (with-catch
                           (lambda (e) (string-append "Error: "
                                         (with-output-to-string
                                           (lambda () (display-exception e)))))
                           (lambda ()
                             (let ((port (open-process
                                           (list path: "/bin/sh"
                                                 arguments: ["-c" cmd]
                                                 stdin-redirection: #t
                                                 stdout-redirection: #t
                                                 stderr-redirection: #t
                                                 pseudo-terminal: #f))))
                               (display region port)
                               (force-output port)
                               (close-output-port port)
                               (let ((output (read-line port #f)))
                                 (close-port port)
                                 (or output "")))))))
            ;; Replace region with result
            (let ((new-text (string-append (substring text 0 start)
                                           result
                                           (substring text end (string-length text)))))
              (qt-plain-text-edit-set-text! ed new-text)
              (qt-plain-text-edit-set-cursor-position! ed start)
              (set! (buffer-mark buf) #f)
              (echo-message! echo "Region filtered")))))
      (echo-error! echo "No mark set"))))

;;;============================================================================
;;; Pipe buffer through shell command
;;;============================================================================

(def (cmd-pipe-buffer app)
  (let* ((echo (app-state-echo app))
         (cmd (qt-echo-read-string app "Pipe buffer through: ")))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (result (with-catch
                       (lambda (e) (string-append "Error: "
                                     (with-output-to-string
                                       (lambda () (display-exception e)))))
                       (lambda ()
                         (let ((port (open-process
                                       (list path: "/bin/sh"
                                             arguments: ["-c" cmd]
                                             stdin-redirection: #t
                                             stdout-redirection: #t
                                             stderr-redirection: #t
                                             pseudo-terminal: #f))))
                           (display text port)
                           (force-output port)
                           (close-output-port port)
                           (let ((output (read-line port #f)))
                             (close-port port)
                             (or output "")))))))
        ;; Show result in *Shell Output*
        (let* ((fr (app-state-frame app))
               (out-buf (or (buffer-by-name "*Shell Output*")
                            (qt-buffer-create! "*Shell Output*" ed #f))))
          (qt-buffer-attach! ed out-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) out-buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer out-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (echo-message! echo "Buffer piped"))))))

;;;============================================================================
;;; Apropos (search commands)
;;;============================================================================

(def (cmd-apropos-command app)
  (let* ((echo (app-state-echo app))
         (query (qt-echo-read-string app "Apropos command: ")))
    (when (and query (> (string-length query) 0))
      (let* ((all-names (map symbol->string (hash-keys *all-commands*)))
             (matches (filter (lambda (n) (string-contains n query)) all-names))
             (sorted (sort matches string<?))
             (result (if (null? sorted)
                       (string-append "No commands matching: " query)
                       (string-append (number->string (length sorted))
                                      " commands matching \"" query "\":\n\n"
                                      (string-join sorted "\n")))))
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (or (buffer-by-name "*Apropos*")
                        (qt-buffer-create! "*Apropos*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;;============================================================================
;;; What page
;;;============================================================================

(def (cmd-what-page app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (page (let loop ((i 0) (p 1))
                 (cond ((>= i pos) p)
                       ((char=? (string-ref text i) #\page) (loop (+ i 1) (+ p 1)))
                       (else (loop (+ i 1) p))))))
    (echo-message! (app-state-echo app)
      (string-append "Page " (number->string page)))))

;;;============================================================================
;;; Async shell command
;;;============================================================================

(def (cmd-async-shell-command app)
  (let* ((echo (app-state-echo app))
         (cmd (qt-echo-read-string app "Async shell command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (let ((port (open-process
                    (list path: "/bin/sh"
                          arguments: ["-c" cmd]
                          stdout-redirection: #f
                          stderr-redirection: #f
                          pseudo-terminal: #f))))
        (echo-message! echo (string-append "Started: " cmd))))))

;;;============================================================================
;;; Checksum (MD5/SHA256 of buffer)
;;;============================================================================

(def (cmd-checksum app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (echo (app-state-echo app)))
    (let ((result (with-catch
                    (lambda (e) "Error computing checksum")
                    (lambda ()
                      (let ((port (open-process
                                    (list path: "/usr/bin/sha256sum"
                                          stdin-redirection: #t
                                          stdout-redirection: #t
                                          pseudo-terminal: #f))))
                        (display text port)
                        (force-output port)
                        (close-output-port port)
                        (let ((output (read-line port)))
                          (close-port port)
                          (or output "")))))))
      (echo-message! echo (string-append "SHA256: " result)))))

;;;============================================================================
;;; S-expression navigation (critical for Lisp editing)
;;;============================================================================

(def (find-matching-close text pos)
  "Find matching close paren/bracket/brace from opening at pos."
  (let* ((len (string-length text))
         (ch (string-ref text pos))
         (close (cond ((char=? ch #\() #\))
                      ((char=? ch #\[) #\])
                      ((char=? ch #\{) #\})
                      (else #f))))
    (if close
      (let loop ((i (+ pos 1)) (depth 1))
        (cond ((>= i len) #f)
              ((= depth 0) i)
              ((char=? (string-ref text i) ch) (loop (+ i 1) (+ depth 1)))
              ((char=? (string-ref text i) close)
               (if (= depth 1) (+ i 1)
                 (loop (+ i 1) (- depth 1))))
              (else (loop (+ i 1) depth))))
      #f)))

(def (find-matching-open text pos)
  "Find matching open paren/bracket/brace scanning backward from pos."
  (let* ((ch (string-ref text pos))
         (open (cond ((char=? ch #\)) #\()
                     ((char=? ch #\]) #\[)
                     ((char=? ch #\}) #\{)
                     (else #f))))
    (if open
      (let loop ((i (- pos 1)) (depth 1))
        (cond ((< i 0) #f)
              ((char=? (string-ref text i) ch) (loop (- i 1) (+ depth 1)))
              ((char=? (string-ref text i) open)
               (if (= depth 1) i
                 (loop (- i 1) (- depth 1))))
              (else (loop (- i 1) depth))))
      #f)))

(def (sexp-end text pos)
  "Find end position of sexp starting at pos."
  (let ((len (string-length text)))
    (if (>= pos len) pos
      (let ((ch (string-ref text pos)))
        (cond
          ;; Opening delimiter - find matching close
          ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
           (or (find-matching-close text pos) len))
          ;; String
          ((char=? ch #\")
           (let loop ((i (+ pos 1)))
             (cond ((>= i len) len)
                   ((char=? (string-ref text i) #\\) (loop (+ i 2)))
                   ((char=? (string-ref text i) #\") (+ i 1))
                   (else (loop (+ i 1))))))
          ;; Word/symbol
          (else
           (let loop ((i pos))
             (if (or (>= i len)
                     (char-whitespace? (string-ref text i))
                     (memv (string-ref text i) '(#\( #\) #\[ #\] #\{ #\})))
               i
               (loop (+ i 1))))))))))

(def (sexp-start text pos)
  "Find start position of sexp ending at pos."
  (if (<= pos 0) 0
    (let* ((i (- pos 1))
           (ch (string-ref text i)))
      (cond
        ;; Closing delimiter
        ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
         (or (find-matching-open text i) 0))
        ;; End of string
        ((char=? ch #\")
         (let loop ((j (- i 1)))
           (cond ((<= j 0) 0)
                 ((and (char=? (string-ref text j) #\")
                       (or (= j 0) (not (char=? (string-ref text (- j 1)) #\\))))
                  j)
                 (else (loop (- j 1))))))
        ;; Word/symbol
        (else
         (let loop ((j i))
           (if (or (<= j 0)
                   (char-whitespace? (string-ref text j))
                   (memv (string-ref text j) '(#\( #\) #\[ #\] #\{ #\})))
             (+ j 1)
             (loop (- j 1)))))))))

(def (skip-whitespace-forward text pos)
  (let ((len (string-length text)))
    (let loop ((i pos))
      (if (or (>= i len) (not (char-whitespace? (string-ref text i))))
        i (loop (+ i 1))))))

(def (skip-whitespace-backward text pos)
  (let loop ((i pos))
    (if (or (<= i 0) (not (char-whitespace? (string-ref text (- i 1)))))
      i (loop (- i 1)))))

(def (cmd-forward-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (skip-whitespace-forward text (qt-plain-text-edit-cursor-position ed)))
         (end (sexp-end text pos)))
    (qt-plain-text-edit-set-cursor-position! ed end)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-backward-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (skip-whitespace-backward text (qt-plain-text-edit-cursor-position ed)))
         (start (sexp-start text pos)))
    (qt-plain-text-edit-set-cursor-position! ed start)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-kill-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd-pos (skip-whitespace-forward text pos))
         (end (sexp-end text fwd-pos)))
    (when (> end pos)
      (let* ((killed (substring text pos end))
             (new-text (string-append (substring text 0 pos)
                                      (substring text end (string-length text)))))
        (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

(def (cmd-backward-kill-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (bwd-pos (skip-whitespace-backward text pos))
         (start (sexp-start text bwd-pos)))
    (when (< start pos)
      (let* ((killed (substring text start pos))
             (new-text (string-append (substring text 0 start)
                                      (substring text pos (string-length text)))))
        (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)))))

(def (cmd-mark-sexp app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd-pos (skip-whitespace-forward text pos))
         (end (sexp-end text fwd-pos))
         (buf (current-qt-buffer app)))
    (set! (buffer-mark buf) pos)
    (qt-plain-text-edit-set-cursor-position! ed end)
    (echo-message! (app-state-echo app) "Sexp marked")))

(def (cmd-mark-defun app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (buf (current-qt-buffer app)))
    ;; Find start of current top-level form
    (let loop ((i pos))
      (cond
        ((< i 0)
         (set! (buffer-mark buf) 0)
         (echo-message! (app-state-echo app) "Defun marked"))
        ((and (char=? (string-ref text i) #\()
              (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
         (let ((end (sexp-end text i)))
           (set! (buffer-mark buf) i)
           (qt-plain-text-edit-set-cursor-position! ed end)
           (echo-message! (app-state-echo app) "Defun marked")))
        (else (loop (- i 1)))))))

(def (cmd-indent-sexp app)
  "Re-indent the sexp after point with 2-space indentation."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (skip-whitespace-forward text (qt-plain-text-edit-cursor-position ed)))
         (end (sexp-end text pos)))
    (when (> end pos)
      (let* ((sexp-text (substring text pos end))
             (lines (string-split sexp-text #\newline))
             ;; Simple re-indent: first line stays, others get 2 spaces per depth
             (indented (if (null? lines) lines
                         (cons (car lines)
                               (map (lambda (l)
                                      (string-append "  " (string-trim l)))
                                    (cdr lines)))))
             (new-sexp (string-join indented "\n"))
             (new-text (string-append (substring text 0 pos)
                                      new-sexp
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

;;;============================================================================
;;; Paredit-style structured editing
;;;============================================================================

(def (find-enclosing-open text pos)
  "Find the position of the innermost opening paren/bracket/brace enclosing POS."
  (let loop ((i (- pos 1)) (depth 0))
    (cond
      ((< i 0) #f)
      ((memv (string-ref text i) '(#\) #\] #\}))
       (loop (- i 1) (+ depth 1)))
      ((memv (string-ref text i) '(#\( #\[ #\{))
       (if (= depth 0) i
         (loop (- i 1) (- depth 1))))
      (else (loop (- i 1) depth)))))

(def (find-enclosing-close text pos)
  "Find the position of the innermost closing paren/bracket/brace enclosing POS."
  (let ((len (string-length text)))
    (let loop ((i pos) (depth 0))
      (cond
        ((>= i len) #f)
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (loop (+ i 1) (+ depth 1)))
        ((memv (string-ref text i) '(#\) #\] #\}))
         (if (= depth 0) i
           (loop (+ i 1) (- depth 1))))
        (else (loop (+ i 1) depth))))))

(def (cmd-paredit-slurp-forward app)
  "Extend enclosing sexp to include the next sexp after the closing delimiter."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (close-pos (find-enclosing-close text pos)))
    (when close-pos
      (let* ((after (skip-whitespace-forward text (+ close-pos 1)))
             (next-end (sexp-end text after)))
        (when (> next-end after)
          ;; Remove closing delimiter, put it after the next sexp
          (let* ((close-char (string (string-ref text close-pos)))
                 (new-text (string-append
                             (substring text 0 close-pos)
                             (substring text (+ close-pos 1) next-end)
                             close-char
                             (substring text next-end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-barf-forward app)
  "Move the last element of enclosing sexp out past the closing delimiter."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (close-pos (find-enclosing-close text pos)))
    (when close-pos
      ;; Find the last sexp inside before the closing paren
      (let* ((before-close (skip-whitespace-backward text close-pos))
             (last-start (sexp-start text before-close)))
        (when (> last-start 0)
          (let* ((open-pos (find-enclosing-open text pos))
                 (close-char (string (string-ref text close-pos))))
            (when (and open-pos (> last-start (+ open-pos 1)))
              ;; Move close-paren to before the last sexp
              (let* ((ws-before (skip-whitespace-backward text last-start))
                     (new-text (string-append
                                 (substring text 0 ws-before)
                                 close-char
                                 (substring text ws-before close-pos)
                                 (substring text (+ close-pos 1) (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (min pos (string-length new-text)))
                (qt-plain-text-edit-ensure-cursor-visible! ed)))))))))

(def (cmd-paredit-wrap-round app)
  "Wrap the sexp at point in parentheses."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd (skip-whitespace-forward text pos))
         (end (sexp-end text fwd)))
    (when (> end fwd)
      (let ((new-text (string-append
                        (substring text 0 fwd) "("
                        (substring text fwd end) ")"
                        (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ fwd 1))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-paredit-wrap-square app)
  "Wrap the sexp at point in square brackets."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd (skip-whitespace-forward text pos))
         (end (sexp-end text fwd)))
    (when (> end fwd)
      (let ((new-text (string-append
                        (substring text 0 fwd) "["
                        (substring text fwd end) "]"
                        (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ fwd 1))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

(def (cmd-paredit-splice-sexp app)
  "Remove the enclosing parens (splice sexp into parent)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (open-pos (find-enclosing-open text pos)))
    (when open-pos
      (let ((close-pos (find-matching-close text open-pos)))
        (when close-pos
          ;; Remove both delimiters
          (let ((new-text (string-append
                            (substring text 0 open-pos)
                            (substring text (+ open-pos 1) (- close-pos 1))
                            (substring text close-pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (- pos 1))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-raise-sexp app)
  "Replace enclosing sexp with the sexp at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd (skip-whitespace-forward text pos))
         (sexp-e (sexp-end text fwd))
         (open-pos (find-enclosing-open text pos)))
    (when (and open-pos (> sexp-e fwd))
      (let ((close-pos (find-matching-close text open-pos)))
        (when close-pos
          (let* ((inner (substring text fwd sexp-e))
                 (new-text (string-append
                             (substring text 0 open-pos)
                             inner
                             (substring text close-pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed open-pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-split-sexp app)
  "Split the enclosing sexp into two at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (open-pos (find-enclosing-open text pos)))
    (when open-pos
      (let ((close-pos (find-enclosing-close text pos))
            (open-ch (string-ref text open-pos)))
        (when close-pos
          (let* ((close-ch (string-ref text close-pos))
                 (new-text (string-append
                             (substring text 0 pos)
                             (string close-ch) " " (string open-ch)
                             (substring text pos (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (+ pos 2))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-paredit-join-sexps app)
  "Join two adjacent sexps into one."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         ;; Find sexp before point (should end with close delimiter)
         (bwd (skip-whitespace-backward text pos)))
    (when (and (> bwd 0)
               (memv (string-ref text (- bwd 1)) '(#\) #\] #\})))
      ;; Find sexp after point (should start with open delimiter)
      (let ((fwd (skip-whitespace-forward text pos)))
        (when (and (< fwd (string-length text))
                   (memv (string-ref text fwd) '(#\( #\[ #\{)))
          ;; Remove the close of first and open of second
          (let ((new-text (string-append
                            (substring text 0 (- bwd 1))
                            " "
                            (substring text (+ fwd 1) (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed (- bwd 1))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

;;;============================================================================
;;; Grep commands (moved from ide for chain ordering)
;;;============================================================================

(def (parse-grep-line line)
  "Parse a grep -n output line. Returns (file line-num text) or #f."
  ;; Format: file:line:text
  (let ((colon1 (string-index line #\:)))
    (and colon1
         (let ((colon2 (string-index line #\: (+ colon1 1))))
           (and colon2
                (let* ((file (substring line 0 colon1))
                       (line-str (substring line (+ colon1 1) colon2))
                       (text (substring line (+ colon2 1) (string-length line)))
                       (line-num (string->number line-str)))
                  (and line-num
                       (list file line-num text))))))))

(def (grep-run-and-show! app pattern dir args)
  "Run grep with ARGS, parse results, show in *Grep* buffer."
  (with-catch
    (lambda (e) (echo-error! (app-state-echo app) "Grep failed"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/grep"
                           arguments: (append args (list pattern dir))
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        ;; Parse results
        (let* ((lines (if output
                       (let loop ((s output) (acc []))
                         (let ((nl (string-index s #\newline)))
                           (if nl
                             (loop (substring s (+ nl 1) (string-length s))
                                   (cons (substring s 0 nl) acc))
                             (reverse (if (> (string-length s) 0)
                                        (cons s acc) acc)))))
                       []))
               (parsed (filter identity (map parse-grep-line lines)))
               (header (string-append "-*- grep -*-\n"
                         "grep " (string-join args " ") " " pattern " " dir "\n\n")))
          ;; Store results for navigation
          (set! *grep-results* parsed)
          (set! *grep-result-index* -1)
          ;; Show in buffer
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app))
                 (result-text (if (null? parsed)
                               (string-append header "No matches found.\n")
                               (string-append header
                                 (number->string (length parsed)) " matches\n\n"
                                 (string-join
                                   (map (lambda (r)
                                          (string-append (car r) ":"
                                            (number->string (cadr r)) ":"
                                            (caddr r)))
                                        parsed)
                                   "\n")
                                 "\n\nPress Enter on a result line to jump to source.")))
                 (grep-buf (or (buffer-by-name "*Grep*")
                               (qt-buffer-create! "*Grep*" ed #f))))
            (qt-buffer-attach! ed grep-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) grep-buf)
            (qt-plain-text-edit-set-text! ed result-text)
            (qt-text-document-set-modified! (buffer-doc-pointer grep-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! (app-state-echo app)
              (string-append (number->string (length parsed)) " matches"))))))))

(def (cmd-grep app)
  "Run grep -rn and show results with navigation."
  (let ((pattern (qt-echo-read-string app "Grep: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let ((dir (qt-echo-read-string app "In directory: ")))
        (when (and dir (> (string-length dir) 0))
          (grep-run-and-show! app pattern dir '("-rn")))))))

(def (cmd-rgrep app)
  "Run recursive grep with file type filter."
  (let ((pattern (qt-echo-read-string app "Rgrep: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let ((include (qt-echo-read-string app "File pattern (e.g. *.ss): ")))
        (when include
          (let ((dir (qt-echo-read-string app "In directory: ")))
            (when (and dir (> (string-length dir) 0))
              (if (and include (> (string-length include) 0))
                (grep-run-and-show! app pattern dir
                  (list "-rn" (string-append "--include=" include)))
                (grep-run-and-show! app pattern dir '("-rn"))))))))))

(def (grep-goto-result! app index)
  "Jump to grep result at INDEX."
  (when (and (>= index 0) (< index (length *grep-results*)))
    (let* ((result (list-ref *grep-results* index))
           (file (car result))
           (line-num (cadr result))
           (echo (app-state-echo app))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      (set! *grep-result-index* index)
      (when (file-exists? file)
        (let* ((name (path-strip-directory file))
               (existing (let loop ((bufs *buffer-list*))
                           (if (null? bufs) #f
                             (let ((b (car bufs)))
                               (if (and (buffer-file-path b)
                                        (string=? (buffer-file-path b) file))
                                 b (loop (cdr bufs)))))))
               (buf (or existing (qt-buffer-create! name ed file))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (not existing)
            (let ((text (read-file-as-string file)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
            (qt-setup-highlighting! app buf))
          ;; Jump to line
          (let* ((text (qt-plain-text-edit-text ed))
                 (pos (text-line-position text line-num)))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-message! echo
            (string-append "Match " (number->string (+ index 1))
              "/" (number->string (length *grep-results*)))))))))

(def (cmd-grep-goto app)
  "Jump from *Grep* buffer line to source location."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (if (not (string=? (buffer-name buf) "*Grep*"))
      (echo-error! (app-state-echo app) "Not in *Grep* buffer")
      ;; Parse current line for file:line:text pattern
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             ;; Find current line
             (line-start (let loop ((i (- pos 1)))
                           (if (or (< i 0) (char=? (string-ref text i) #\newline))
                             (+ i 1) (loop (- i 1)))))
             (line-end (let loop ((i pos))
                         (if (or (>= i (string-length text))
                                 (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
             (line (substring text line-start line-end))
             (parsed (parse-grep-line line)))
        (if parsed
          ;; Find the matching index in *grep-results*
          (let ((idx (let loop ((results *grep-results*) (i 0))
                       (cond
                         ((null? results) 0)
                         ((and (string=? (car (car results)) (car parsed))
                               (= (cadr (car results)) (cadr parsed)))
                          i)
                         (else (loop (cdr results) (+ i 1)))))))
            (grep-goto-result! app idx))
          (echo-error! (app-state-echo app) "No grep result on this line"))))))

(def (cmd-next-grep-result app)
  "Jump to next grep result."
  (if (null? *grep-results*)
    (echo-error! (app-state-echo app) "No grep results")
    (let ((next-idx (+ *grep-result-index* 1)))
      (if (>= next-idx (length *grep-results*))
        (echo-message! (app-state-echo app) "No more matches")
        (grep-goto-result! app next-idx)))))

(def (cmd-previous-grep-result app)
  "Jump to previous grep result."
  (if (null? *grep-results*)
    (echo-error! (app-state-echo app) "No grep results")
    (let ((prev-idx (- *grep-result-index* 1)))
      (if (< prev-idx 0)
        (echo-message! (app-state-echo app) "No previous matches")
        (grep-goto-result! app prev-idx)))))

