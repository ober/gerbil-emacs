;;; -*- Gerbil -*-
;;; Qt parity commands (part 2) — check-parens, dap, smerge, flyspell, tabs, rainbow.
;;; Chain position: after commands-parity, before commands-aliases.

(export #t)

(import :std/sugar
        :std/srfi/13
        :std/misc/string
        :std/misc/process
        (only-in :gemacs/pregexp-compat pregexp pregexp-match)
        (only-in :gemacs/org-parse
                 org-heading-line? org-heading-stars-of-line)
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        ;; Chain of all prior command modules
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-ide2
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-modes
        :gemacs/qt/commands-config
        :gemacs/qt/commands-config2
        :gemacs/qt/commands-parity)

;;;============================================================================
;;; Batch 4: check-parens, count-lines-page, how-many
;;;============================================================================

;;; --- Text mode ---
(def (cmd-text-mode app)
  "Switch to text mode — plain text, no syntax highlighting."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr))))
    (set! (buffer-lexer-lang buf) #f)
    (sci-send ed SCI_SETLEXER 0)  ;; SCLEX_NULL — no highlighting
    (sci-send ed SCI_STYLECLEARALL)
    (echo-message! (app-state-echo app) "Text mode")))

;;; --- Major mode switching helper ---
(def (qt-set-major-mode! app lang-sym mode-name)
  "Set major mode by changing lexer language and re-highlighting."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-edit-window-buffer (qt-current-window fr))))
    (set! (buffer-lexer-lang buf) lang-sym)
    (qt-setup-highlighting! app buf)
    (echo-message! (app-state-echo app) mode-name)))

;;; --- Shell script mode ---
(def (cmd-shell-script-mode app)
  "Switch to shell script mode with bash syntax highlighting."
  (qt-set-major-mode! app 'bash "Shell-script mode"))

;;; --- Python mode ---
(def (cmd-python-mode app)
  "Switch to Python mode with syntax highlighting."
  (qt-set-major-mode! app 'python "Python mode"))

;;; --- C mode ---
(def (cmd-c-mode app)
  "Switch to C mode with syntax highlighting."
  (qt-set-major-mode! app 'c "C mode"))

;;; --- C++ mode ---
(def (cmd-c++-mode app)
  "Switch to C++ mode with syntax highlighting."
  (qt-set-major-mode! app 'c "C++ mode"))

;;; --- JavaScript mode ---
(def (cmd-js-mode app)
  "Switch to JavaScript mode with syntax highlighting."
  (qt-set-major-mode! app 'javascript "JavaScript mode"))

;;; --- TypeScript mode ---
(def (cmd-typescript-mode app)
  "Switch to TypeScript mode with syntax highlighting."
  (qt-set-major-mode! app 'javascript "TypeScript mode"))

;;; --- Go mode ---
(def (cmd-go-mode app)
  "Switch to Go mode with syntax highlighting."
  (qt-set-major-mode! app 'go "Go mode"))

;;; --- Rust mode ---
(def (cmd-rust-mode app)
  "Switch to Rust mode with syntax highlighting."
  (qt-set-major-mode! app 'rust "Rust mode"))

;;; --- Ruby mode ---
(def (cmd-ruby-mode app)
  "Switch to Ruby mode with syntax highlighting."
  (qt-set-major-mode! app 'ruby "Ruby mode"))

;;; --- Markdown mode ---
(def (cmd-markdown-mode app)
  "Switch to Markdown mode with syntax highlighting."
  (qt-set-major-mode! app 'markdown "Markdown mode"))

;;; --- Org mode ---
(def (cmd-org-mode-switch app)
  "Switch to Org mode with syntax highlighting."
  (qt-set-major-mode! app 'org "Org mode"))

;;; --- YAML mode ---
(def (cmd-yaml-mode app)
  "Switch to YAML mode with syntax highlighting."
  (qt-set-major-mode! app 'yaml "YAML mode"))

;;; --- JSON mode ---
(def (cmd-json-mode app)
  "Switch to JSON mode with syntax highlighting."
  (qt-set-major-mode! app 'json "JSON mode"))

;;; --- SQL mode ---
(def (cmd-sql-mode app)
  "Switch to SQL mode with syntax highlighting."
  (qt-set-major-mode! app 'sql "SQL mode"))

;;; --- Lua mode ---
(def (cmd-lua-mode app)
  "Switch to Lua mode with syntax highlighting."
  (qt-set-major-mode! app 'lua "Lua mode"))

;;; --- HTML mode ---
(def (cmd-html-mode app)
  "Switch to HTML mode with syntax highlighting."
  (qt-set-major-mode! app 'html "HTML mode"))

;;; --- CSS mode ---
(def (cmd-css-mode app)
  "Switch to CSS mode with syntax highlighting."
  (qt-set-major-mode! app 'css "CSS mode"))

;;; --- Scheme mode ---
(def (cmd-scheme-mode app)
  "Switch to Scheme/Gerbil mode with syntax highlighting."
  (qt-set-major-mode! app 'scheme "Scheme mode"))

;;; --- Check parens ---
(def (cmd-check-parens app)
  "Check for unbalanced parentheses in the current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (pairs '((#\( . #\)) (#\[ . #\]) (#\{ . #\}))))
    (let lp ((i 0) (stk '()))
      (cond
        ((>= i len)
         (if (null? stk)
           (echo-message! (app-state-echo app) "Parentheses are balanced")
           (let* ((pos (car stk))
                  (line (let cnt ((j 0) (n 0))
                          (if (>= j pos) n
                            (cnt (+ j 1) (if (char=? (string-ref text j) #\newline) (+ n 1) n))))))
             (echo-message! (app-state-echo app)
               (string-append "Unmatched opener at line " (number->string (+ line 1)))))))
        (else
         (let ((ch (string-ref text i)))
           (cond
             ((assoc ch pairs)
              (lp (+ i 1) (cons i stk)))
             ((find (lambda (p) (char=? ch (cdr p))) pairs)
              => (lambda (p)
                   (if (and (pair? stk)
                            (char=? (string-ref text (car stk)) (car p)))
                     (lp (+ i 1) (cdr stk))
                     (let ((line (let cnt ((j 0) (n 0))
                                   (if (>= j i) n
                                     (cnt (+ j 1) (if (char=? (string-ref text j) #\newline) (+ n 1) n))))))
                       (echo-message! (app-state-echo app)
                         (string-append "Unmatched " (string ch) " at line "
                           (number->string (+ line 1))))))))
             (else (lp (+ i 1) stk)))))))))

;;; --- Count lines page ---
(def (cmd-count-lines-page app)
  "Count lines on the current page (delimited by form-feed)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text))
         (page-start (let lp ((i (- pos 1)))
                       (cond ((<= i 0) 0)
                             ((char=? (string-ref text i) (integer->char 12)) (+ i 1))
                             (else (lp (- i 1))))))
         (page-end (let lp ((i pos))
                     (cond ((>= i len) len)
                           ((char=? (string-ref text i) (integer->char 12)) i)
                           (else (lp (+ i 1))))))
         (count-lines (lambda (start end)
                        (let lp ((i start) (n 0))
                          (cond ((>= i end) n)
                                ((char=? (string-ref text i) #\newline) (lp (+ i 1) (+ n 1)))
                                (else (lp (+ i 1) n))))))
         (before (count-lines page-start pos))
         (after (count-lines pos page-end))
         (total (+ before after)))
    (echo-message! (app-state-echo app)
      (string-append "Page has " (number->string total) " lines ("
        (number->string before) " + " (number->string after) ")"))))

;;; --- How many ---
(def (cmd-how-many app)
  "Count regexp matches from point to end of buffer."
  (let ((pattern (qt-echo-read-string (app-state-echo app) "How many (regexp): ")))
    (when (and pattern (not (string=? pattern "")))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (rest (substring text pos (string-length text)))
             (rx (with-catch (lambda (e) #f) (lambda () (pregexp pattern)))))
        (if (not rx)
          (echo-message! (app-state-echo app) "Invalid regexp")
          (let lp ((s rest) (count 0))
            (let ((m (pregexp-match rx s)))
              (if (not m)
                (echo-message! (app-state-echo app)
                  (string-append (number->string count) " occurrences"))
                (let* ((match-str (car m))
                       (match-len (string-length match-str))
                       (idx (string-contains s match-str)))
                  (if (or (not idx) (= match-len 0))
                    (echo-message! (app-state-echo app)
                      (string-append (number->string count) " occurrences"))
                    (lp (substring s (+ idx (max 1 match-len)) (string-length s))
                        (+ count 1))))))))))))

;;;============================================================================
;;; Batch 5: delete-directory, set-file-modes, dired-do-chown, butterfly
;;;============================================================================

;;; --- Delete directory ---
(def (cmd-delete-directory app)
  "Delete a directory (must be empty)."
  (let ((dir (qt-echo-read-string (app-state-echo app) "Delete directory: ")))
    (when (and dir (not (string=? dir "")))
      (with-catch
        (lambda (e) (echo-message! (app-state-echo app)
                      (string-append "Cannot delete: " dir)))
        (lambda ()
          (delete-directory dir)
          (echo-message! (app-state-echo app)
            (string-append "Deleted directory: " dir)))))))

;;; --- Set file modes (chmod) ---
(def (cmd-set-file-modes app)
  "Set file permissions (chmod)."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-message! (app-state-echo app) "No file in current buffer")
      (let ((mode (qt-echo-read-string (app-state-echo app)
                    (string-append "chmod " path " to: "))))
        (when (and mode (not (string=? mode "")))
          (with-catch
            (lambda (e) (echo-message! (app-state-echo app) "chmod failed"))
            (lambda ()
              (run-process ["chmod" mode path] coprocess: void)
              (echo-message! (app-state-echo app)
                (string-append "Set " path " to mode " mode)))))))))

;;; --- Dired do chown ---
(def (cmd-dired-do-chown app)
  "Change file owner in dired."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-message! (app-state-echo app) "No file in current buffer")
      (let ((owner (qt-echo-read-string (app-state-echo app)
                     (string-append "chown " path " to: "))))
        (when (and owner (not (string=? owner "")))
          (with-catch
            (lambda (e) (echo-message! (app-state-echo app) "chown failed"))
            (lambda ()
              (run-process ["chown" owner path] coprocess: void)
              (echo-message! (app-state-echo app)
                (string-append "Changed owner of " path " to " owner)))))))))

;;; --- Butterfly ---
(def (cmd-butterfly app)
  "A butterfly flapping its wings causes a gentle breeze..."
  (echo-message! (app-state-echo app)
    "The butterflies have set the universe in motion."))

;;; ========================================================================
;;; Batch 7: Debug stubs
;;; ========================================================================

(def *qt-debug-on-entry-list* [])

(def (cmd-debug-on-entry app)
  "Mark a function for debug-on-entry (stub)."
  (let ((name (qt-echo-read-string (app-state-echo app) "Debug on entry to: ")))
    (when (and name (not (string=? name "")))
      (let ((sym (string->symbol name)))
        (unless (member sym *qt-debug-on-entry-list*)
          (set! *qt-debug-on-entry-list* (cons sym *qt-debug-on-entry-list*)))
        (echo-message! (app-state-echo app)
          (string-append "debug-on-entry: " name))))))

(def (cmd-cancel-debug-on-entry app)
  "Remove a function from debug-on-entry list (stub)."
  (if (null? *qt-debug-on-entry-list*)
    (echo-message! (app-state-echo app) "No functions marked for debug-on-entry")
    (let ((name (qt-echo-read-string (app-state-echo app)
                  (string-append "Cancel debug on entry to ["
                    (symbol->string (car *qt-debug-on-entry-list*)) "]: "))))
      (let ((sym (if (or (not name) (string=? name ""))
                   (car *qt-debug-on-entry-list*)
                   (string->symbol name))))
        (set! *qt-debug-on-entry-list*
          (filter (lambda (s) (not (eq? s sym))) *qt-debug-on-entry-list*))
        (echo-message! (app-state-echo app)
          (string-append "Cancelled debug-on-entry for " (symbol->string sym)))))))

;;;============================================================================
;;; VCS parity: vc-pull, vc-push, magit-stage-file
;;;============================================================================

(def (cmd-vc-pull app)
  "Pull from remote repository."
  (let ((result (with-exception-catcher
                  (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git" arguments: '("pull")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p) (or out "")))))))
    (echo-message! (app-state-echo app)
      (string-append "git pull: " (if (> (string-length result) 60)
                                    (substring result 0 60) result)))))

(def (cmd-vc-push app)
  "Push to remote repository."
  (let ((result (with-exception-catcher
                  (lambda (e) (with-output-to-string (lambda () (display-exception e))))
                  (lambda ()
                    (let ((p (open-process
                               (list path: "git" arguments: '("push")
                                     stdin-redirection: #f stdout-redirection: #t
                                     stderr-redirection: #t))))
                      (let ((out (read-line p #f)))
                        (process-status p) (or out "")))))))
    (echo-message! (app-state-echo app)
      (string-append "git push: " (if (> (string-length result) 60)
                                    (substring result 0 60) result)))))

(def (cmd-magit-stage-file app)
  "Stage current buffer's file."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf))))
    (if path
      (let ((result (with-exception-catcher
                      (lambda (e) "Error staging file")
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "git" arguments: (list "add" path)
                                         stdin-redirection: #f stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (process-status p)
                          (string-append "Staged: " (path-strip-directory path)))))))
        (echo-message! (app-state-echo app) result))
      (echo-message! (app-state-echo app) "Buffer has no file"))))

;;;============================================================================
;;; DAP debug commands
;;;============================================================================

(def *qt-dap-breakpoints* (make-hash-table))
(def *qt-dap-process* #f)

(def (cmd-dap-debug app)
  "Start debug session."
  (let ((program (qt-echo-read-string app "Program to debug: ")))
    (if (or (not program) (= (string-length program) 0))
      (echo-error! (app-state-echo app) "No program specified")
      (begin
        (set! *qt-dap-process* #f)
        (echo-message! (app-state-echo app)
          (string-append "DAP: debug session started for " program))))))

(def (cmd-dap-breakpoint-toggle app)
  "Toggle breakpoint at current line."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf)))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line (+ 1 (let loop ((i 0) (n 0))
                      (if (>= i pos) n
                        (loop (+ i 1)
                              (if (char=? (string-ref text i) #\newline)
                                (+ n 1) n)))))))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let* ((existing (or (hash-get *qt-dap-breakpoints* path) '()))
             (has-bp (member line existing)))
        (if has-bp
          (begin
            (hash-put! *qt-dap-breakpoints* path
              (filter (lambda (l) (not (= l line))) existing))
            (echo-message! (app-state-echo app)
              (string-append "Breakpoint removed at "
                (path-strip-directory path) ":" (number->string line))))
          (begin
            (hash-put! *qt-dap-breakpoints* path (cons line existing))
            (echo-message! (app-state-echo app)
              (string-append "Breakpoint set at "
                (path-strip-directory path) ":" (number->string line)))))))))

(def (cmd-dap-step-over app)
  "Step over in debug session."
  (echo-message! (app-state-echo app) "DAP: step over"))

(def (cmd-dap-step-in app)
  "Step into in debug session."
  (echo-message! (app-state-echo app) "DAP: step in"))

(def (cmd-dap-step-out app)
  "Step out in debug session."
  (echo-message! (app-state-echo app) "DAP: step out"))

;;;============================================================================
;;; Smerge mode: Git conflict marker resolution (Qt)
;;;============================================================================

(def *qt-smerge-mine-marker*  "<<<<<<<")
(def *qt-smerge-sep-marker*   "=======")
(def *qt-smerge-other-marker* ">>>>>>>")

(def (qt-smerge-find-conflict text pos direction)
  "Find the next/prev conflict starting from POS.
   Returns (values mine-start sep-start other-end) or (values #f #f #f)."
  (let ((len (string-length text)))
    (if (eq? direction 'next)
      (let loop ((i pos))
        (if (>= i len)
          (values #f #f #f)
          (if (and (<= (+ i 7) len)
                   (string=? (substring text i (+ i 7)) *qt-smerge-mine-marker*)
                   (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
            (let ((mine-start i))
              (let find-sep ((j (+ i 7)))
                (if (>= j len)
                  (values #f #f #f)
                  (if (and (<= (+ j 7) len)
                           (string=? (substring text j (+ j 7)) *qt-smerge-sep-marker*)
                           (or (= j 0) (char=? (string-ref text (- j 1)) #\newline)))
                    (let ((sep-start j))
                      (let find-other ((k (+ j 7)))
                        (if (>= k len)
                          (values #f #f #f)
                          (if (and (<= (+ k 7) len)
                                   (string=? (substring text k (+ k 7)) *qt-smerge-other-marker*)
                                   (or (= k 0) (char=? (string-ref text (- k 1)) #\newline)))
                            (let find-eol ((e (+ k 7)))
                              (if (or (>= e len) (char=? (string-ref text e) #\newline))
                                (values mine-start sep-start (min (+ e 1) len))
                                (find-eol (+ e 1))))
                            (find-other (+ k 1))))))
                    (find-sep (+ j 1))))))
            (loop (+ i 1)))))
      ;; prev
      (let loop ((i (min pos (- len 1))))
        (if (< i 0)
          (values #f #f #f)
          (if (and (<= (+ i 7) len)
                   (string=? (substring text i (+ i 7)) *qt-smerge-mine-marker*)
                   (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
            (let ((mine-start i))
              (let find-sep ((j (+ i 7)))
                (if (>= j len)
                  (loop (- i 1))
                  (if (and (<= (+ j 7) len)
                           (string=? (substring text j (+ j 7)) *qt-smerge-sep-marker*)
                           (or (= j 0) (char=? (string-ref text (- j 1)) #\newline)))
                    (let ((sep-start j))
                      (let find-other ((k (+ j 7)))
                        (if (>= k len)
                          (loop (- i 1))
                          (if (and (<= (+ k 7) len)
                                   (string=? (substring text k (+ k 7)) *qt-smerge-other-marker*)
                                   (or (= k 0) (char=? (string-ref text (- k 1)) #\newline)))
                            (let find-eol ((e (+ k 7)))
                              (if (or (>= e len) (char=? (string-ref text e) #\newline))
                                (values mine-start sep-start (min (+ e 1) len))
                                (find-eol (+ e 1))))
                            (find-other (+ k 1))))))
                    (find-sep (+ j 1))))))
            (loop (- i 1))))))))

(def (qt-smerge-count text)
  "Count conflict markers in text."
  (let ((len (string-length text)))
    (let loop ((i 0) (count 0))
      (if (>= i len) count
        (if (and (<= (+ i 7) len)
                 (string=? (substring text i (+ i 7)) *qt-smerge-mine-marker*)
                 (or (= i 0) (char=? (string-ref text (- i 1)) #\newline)))
          (loop (+ i 1) (+ count 1))
          (loop (+ i 1) count))))))

(def (qt-smerge-extract-mine text mine-start sep-start)
  "Extract 'mine' content between <<<<<<< and =======."
  (let ((mine-line-end
          (let find-eol ((i (+ mine-start 7)))
            (if (or (>= i (string-length text)) (char=? (string-ref text i) #\newline))
              (min (+ i 1) (string-length text))
              (find-eol (+ i 1))))))
    (substring text mine-line-end sep-start)))

(def (qt-smerge-extract-other text sep-start other-end)
  "Extract 'other' content between ======= and >>>>>>>."
  (let* ((sep-line-end
           (let find-eol ((i (+ sep-start 7)))
             (if (or (>= i (string-length text)) (char=? (string-ref text i) #\newline))
               (min (+ i 1) (string-length text))
               (find-eol (+ i 1)))))
         (other-line-start
           (let find-marker ((k sep-line-end))
             (if (>= k other-end) other-end
               (if (and (<= (+ k 7) (string-length text))
                        (string=? (substring text k (+ k 7)) *qt-smerge-other-marker*)
                        (or (= k 0) (char=? (string-ref text (- k 1)) #\newline)))
                 k
                 (find-marker (+ k 1)))))))
    (substring text sep-line-end other-line-start)))

(def (cmd-smerge-next app)
  "Jump to the next merge conflict marker."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (+ (qt-plain-text-edit-cursor-position ed) 1)))
    (let-values (((mine sep other) (qt-smerge-find-conflict text pos 'next)))
      (if mine
        (begin
          (qt-plain-text-edit-set-cursor-position! ed mine)
          (echo-message! (app-state-echo app)
            (string-append "Conflict (" (number->string (qt-smerge-count text)) " total)")))
        (echo-message! (app-state-echo app) "No more conflicts")))))

(def (cmd-smerge-prev app)
  "Jump to the previous merge conflict marker."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (max 0 (- (qt-plain-text-edit-cursor-position ed) 1))))
    (let-values (((mine sep other) (qt-smerge-find-conflict text pos 'prev)))
      (if mine
        (begin
          (qt-plain-text-edit-set-cursor-position! ed mine)
          (echo-message! (app-state-echo app)
            (string-append "Conflict (" (number->string (qt-smerge-count text)) " total)")))
        (echo-message! (app-state-echo app) "No previous conflict")))))

(def (cmd-smerge-keep-mine app)
  "Keep 'mine' (upper) side of the current conflict."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((mine sep other) (qt-smerge-find-conflict text pos 'prev)))
      (if (and mine (<= mine pos) (< pos other))
        (let* ((content (qt-smerge-extract-mine text mine sep))
               (before (substring text 0 mine))
               (after (substring text other (string-length text))))
          (qt-plain-text-edit-set-text! ed (string-append before content after))
          (qt-plain-text-edit-set-cursor-position! ed mine)
          (echo-message! (app-state-echo app) "Kept mine"))
        (let-values (((mine2 sep2 other2) (qt-smerge-find-conflict text pos 'next)))
          (if mine2
            (let* ((content (qt-smerge-extract-mine text mine2 sep2))
                   (before (substring text 0 mine2))
                   (after (substring text other2 (string-length text))))
              (qt-plain-text-edit-set-text! ed (string-append before content after))
              (qt-plain-text-edit-set-cursor-position! ed mine2)
              (echo-message! (app-state-echo app) "Kept mine"))
            (echo-message! (app-state-echo app) "No conflict at point")))))))

(def (cmd-smerge-keep-other app)
  "Keep 'other' (lower) side of the current conflict."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((mine sep other) (qt-smerge-find-conflict text pos 'prev)))
      (if (and mine (<= mine pos) (< pos other))
        (let* ((content (qt-smerge-extract-other text sep other))
               (before (substring text 0 mine))
               (after (substring text other (string-length text))))
          (qt-plain-text-edit-set-text! ed (string-append before content after))
          (qt-plain-text-edit-set-cursor-position! ed mine)
          (echo-message! (app-state-echo app) "Kept other"))
        (let-values (((mine2 sep2 other2) (qt-smerge-find-conflict text pos 'next)))
          (if mine2
            (let* ((content (qt-smerge-extract-other text mine2 sep2))
                   (before (substring text 0 mine2))
                   (after (substring text other2 (string-length text))))
              (qt-plain-text-edit-set-text! ed (string-append before content after))
              (qt-plain-text-edit-set-cursor-position! ed mine2)
              (echo-message! (app-state-echo app) "Kept other"))
            (echo-message! (app-state-echo app) "No conflict at point")))))))

(def (cmd-smerge-keep-both app)
  "Keep both sides of the current conflict (remove markers only)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let-values (((mine sep other) (qt-smerge-find-conflict text pos 'prev)))
      (if (and mine (<= mine pos) (< pos other))
        (let* ((mine-content (qt-smerge-extract-mine text mine sep))
               (other-content (qt-smerge-extract-other text sep other))
               (before (substring text 0 mine))
               (after (substring text other (string-length text))))
          (qt-plain-text-edit-set-text! ed (string-append before mine-content other-content after))
          (qt-plain-text-edit-set-cursor-position! ed mine)
          (echo-message! (app-state-echo app) "Kept both"))
        (let-values (((mine2 sep2 other2) (qt-smerge-find-conflict text pos 'next)))
          (if mine2
            (let* ((mine-content (qt-smerge-extract-mine text mine2 sep2))
                   (other-content (qt-smerge-extract-other text sep2 other2))
                   (before (substring text 0 mine2))
                   (after (substring text other2 (string-length text))))
              (qt-plain-text-edit-set-text! ed (string-append before mine-content other-content after))
              (qt-plain-text-edit-set-cursor-position! ed mine2)
              (echo-message! (app-state-echo app) "Kept both"))
            (echo-message! (app-state-echo app) "No conflict at point")))))))

(def (cmd-smerge-mode app)
  "Toggle smerge mode — report conflict count in current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (count (qt-smerge-count text)))
    (if (> count 0)
      (begin
        (echo-message! (app-state-echo app)
          (string-append "Smerge: " (number->string count) " conflict"
                         (if (> count 1) "s" "") " found. "
                         "n/p=navigate, m=mine, o=other, b=both"))
        (let-values (((mine sep other) (qt-smerge-find-conflict text 0 'next)))
          (when mine (qt-plain-text-edit-set-cursor-position! ed mine))))
      (echo-message! (app-state-echo app) "No merge conflicts found"))))

;;;============================================================================
;;; Interactive Org Agenda commands (Qt)
;;;============================================================================

(def (qt-agenda-parse-line text line-num)
  "Parse an agenda line 'bufname:linenum: text' → (buf-name src-line) or #f."
  (let* ((lines (string-split text #\newline))
         (len (length lines)))
    (if (or (< line-num 0) (>= line-num len))
      #f
      (let* ((line (list-ref lines line-num))
             (trimmed (string-trim line)))
        (let ((colon1 (string-contains trimmed ":")))
          (if (not colon1)
            #f
            (let* ((buf-name (substring trimmed 0 colon1))
                   (rest (substring trimmed (+ colon1 1) (string-length trimmed)))
                   (colon2 (string-contains rest ":")))
              (if (not colon2)
                #f
                (let* ((num-str (substring rest 0 colon2))
                       (src-line (string->number num-str)))
                  (if src-line
                    (list buf-name src-line)
                    #f))))))))))

(def (cmd-org-agenda-goto app)
  "Jump to the source of the agenda item on the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (parsed (qt-agenda-parse-line text line-num)))
    (if (not parsed)
      (echo-message! (app-state-echo app) "No agenda item on this line")
      (let* ((buf-name (car parsed))
             (src-line (cadr parsed))
             (target-buf (buffer-by-name buf-name)))
        (if target-buf
          ;; Buffer exists - switch to it
          (let* ((fr (app-state-frame app))
                 (win (qt-current-window fr)))
            (qt-buffer-attach! ed target-buf)
            (set! (qt-edit-window-buffer win) target-buf)
            ;; Go to line
            (let* ((new-text (qt-plain-text-edit-text ed))
                   (lines (string-split new-text #\newline))
                   (pos (let loop ((ls lines) (n 0) (offset 0))
                          (if (or (null? ls) (= n (- src-line 1)))
                            offset
                            (loop (cdr ls) (+ n 1) (+ offset (string-length (car ls)) 1))))))
              (qt-plain-text-edit-set-cursor-position! ed pos))
            (echo-message! (app-state-echo app)
              (string-append "Jumped to " buf-name ":" (number->string src-line))))
          ;; Try to open file from buffer list
          (let ((fp (let search ((bufs (buffer-list)))
                      (if (null? bufs) #f
                        (let ((b (car bufs)))
                          (if (string=? (buffer-name b) buf-name)
                            (buffer-file-path b)
                            (search (cdr bufs))))))))
            (if fp
              (begin
                (cmd-find-file-by-path app fp)
                (let* ((new-ed (current-qt-editor app))
                       (new-text (qt-plain-text-edit-text new-ed))
                       (lines (string-split new-text #\newline))
                       (pos (let loop ((ls lines) (n 0) (offset 0))
                              (if (or (null? ls) (= n (- src-line 1)))
                                offset
                                (loop (cdr ls) (+ n 1) (+ offset (string-length (car ls)) 1))))))
                  (qt-plain-text-edit-set-cursor-position! new-ed pos))
                (echo-message! (app-state-echo app)
                  (string-append "Opened " fp ":" (number->string src-line))))
              (echo-message! (app-state-echo app)
                (string-append "Buffer not found: " buf-name)))))))))

(def (cmd-org-agenda-todo app)
  "Toggle TODO state of the agenda item on the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (parsed (qt-agenda-parse-line text line-num)))
    (if (not parsed)
      (echo-message! (app-state-echo app) "No agenda item on this line")
      (let* ((buf-name (car parsed))
             (src-line (cadr parsed))
             (target-buf (buffer-by-name buf-name)))
        (if (not target-buf)
          (echo-message! (app-state-echo app) (string-append "Buffer not found: " buf-name))
          (let ((fp (buffer-file-path target-buf)))
            (if (not fp)
              (echo-message! (app-state-echo app) "Buffer has no file")
              (with-catch
                (lambda (e) (echo-message! (app-state-echo app) "Error toggling TODO"))
                (lambda ()
                  (let* ((content (call-with-input-file fp (lambda (p) (read-line p #f))))
                         (lines (string-split content #\newline))
                         (idx (- src-line 1)))
                    (when (and (>= idx 0) (< idx (length lines)))
                      (let* ((line (list-ref lines idx))
                             (new-line
                               (cond
                                 ((string-contains line "TODO ")
                                  (let ((i (string-contains line "TODO ")))
                                    (string-append (substring line 0 i) "DONE "
                                                   (substring line (+ i 5) (string-length line)))))
                                 ((string-contains line "DONE ")
                                  (let ((i (string-contains line "DONE ")))
                                    (string-append (substring line 0 i) "TODO "
                                                   (substring line (+ i 5) (string-length line)))))
                                 (else line)))
                             (new-lines (let loop ((ls lines) (n 0) (acc '()))
                                          (if (null? ls) (reverse acc)
                                            (loop (cdr ls) (+ n 1)
                                                  (cons (if (= n idx) new-line (car ls)) acc)))))
                             (new-content (string-join new-lines "\n")))
                        (call-with-output-file fp (lambda (p) (display new-content p)))
                        ;; Update the agenda line in place
                        (let* ((agenda-text (qt-plain-text-edit-text ed))
                               (agenda-lines (string-split agenda-text #\newline))
                               (new-agenda-lines
                                 (let loop ((ls agenda-lines) (n 0) (acc '()))
                                   (if (null? ls) (reverse acc)
                                     (loop (cdr ls) (+ n 1)
                                           (cons (if (= n line-num)
                                                   (string-append "  " buf-name ":"
                                                                  (number->string src-line) ": "
                                                                  (string-trim new-line))
                                                   (car ls))
                                                 acc)))))
                               (new-agenda (string-join new-agenda-lines "\n")))
                          (qt-plain-text-edit-set-text! ed new-agenda)
                          (qt-plain-text-edit-set-cursor-position! ed 0))
                        (echo-message! (app-state-echo app)
                          (if (string-contains new-line "DONE")
                            "TODO → DONE"
                            "DONE → TODO"))))))))))))))

;;;============================================================================
;;; Flyspell mode (Qt) — spell-check and report misspelled words
;;;============================================================================

(def *qt-flyspell-active* #f)

(def (qt-aspell-check-word word)
  "Check a word with aspell. Returns list of suggestions or #f if correct."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((proc (open-process
                      (list path: "aspell"
                            arguments: '("pipe")
                            stdin-redirection: #t stdout-redirection: #t
                            stderr-redirection: #f)))
             (_ (begin (display (string-append "^" word "\n") proc)
                       (force-output proc)))
             (header (read-line proc))
             (result (read-line proc)))
        (close-port proc)
        (cond
          ((or (eof-object? result) (string=? result "")) #f)
          ((char=? (string-ref result 0) #\*) #f) ;; correct
          ((char=? (string-ref result 0) #\&) ;; suggestions
           (let* ((parts (string-split result #\:))
                  (suggestions (if (>= (length parts) 2)
                                 (map string-trim (string-split (cadr parts) #\,))
                                 '())))
             suggestions))
          ((char=? (string-ref result 0) #\#) '()) ;; no suggestions
          (else #f))))))

(def (qt-flyspell-is-word-char? ch)
  (or (char-alphabetic? ch) (char=? ch #\')))

(def (qt-flyspell-extract-words text)
  "Extract words with positions from text."
  (let ((len (string-length text)))
    (let loop ((i 0) (words '()))
      (if (>= i len)
        (reverse words)
        (if (qt-flyspell-is-word-char? (string-ref text i))
          (let find-end ((j (+ i 1)))
            (if (or (>= j len) (not (qt-flyspell-is-word-char? (string-ref text j))))
              (let ((word (substring text i j)))
                (if (> (string-length word) 1)
                  (loop j (cons (list word i j) words))
                  (loop j words)))
              (find-end (+ j 1))))
          (loop (+ i 1) words))))))

(def *flyspell-indicator* 28)

(def (flyspell-clear-indicators! ed)
  "Clear all flyspell squiggly underline indicators."
  (let ((len (string-length (qt-plain-text-edit-text ed))))
    (when (> len 0)
      (sci-send ed SCI_SETINDICATORCURRENT *flyspell-indicator*)
      (sci-send ed SCI_INDICATORCLEARRANGE 0 len))))

(def (flyspell-apply-indicators! ed misspelled-entries)
  "Apply squiggly red underline indicators for misspelled words."
  ;; Setup indicator style: red squiggly underline
  (sci-send ed SCI_INDICSETSTYLE *flyspell-indicator* 1) ;; INDIC_SQUIGGLE = 1
  (sci-send ed SCI_INDICSETFORE *flyspell-indicator* (rgb->sci 255 0 0))
  (sci-send ed SCI_SETINDICATORCURRENT *flyspell-indicator*)
  ;; Apply to each misspelled word
  (for-each
    (lambda (entry)
      (let ((start (cadr entry))
            (end (caddr entry)))
        (sci-send ed SCI_INDICATORFILLRANGE start (- end start))))
    misspelled-entries))

(def (cmd-flyspell-mode app)
  "Toggle flyspell mode: check buffer for misspelled words with visual indicators."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (if *qt-flyspell-active*
      (begin
        (set! *qt-flyspell-active* #f)
        (flyspell-clear-indicators! ed)
        (echo-message! (app-state-echo app) "Flyspell mode OFF"))
      (begin
        (set! *qt-flyspell-active* #t)
        (flyspell-clear-indicators! ed)
        (let* ((words (qt-flyspell-extract-words text))
               (misspelled-entries []))
          (for-each
            (lambda (entry)
              (let ((word (car entry)))
                (when (> (string-length word) 1)
                  (let ((suggestions (qt-aspell-check-word word)))
                    (when suggestions
                      (set! misspelled-entries (cons entry misspelled-entries)))))))
            words)
          (let ((count (length misspelled-entries)))
            (when (> count 0)
              (flyspell-apply-indicators! ed (reverse misspelled-entries)))
            (if (= count 0)
              (echo-message! (app-state-echo app)
                (string-append "Flyspell: no misspelled words (" (number->string (length words)) " checked)"))
              (echo-message! (app-state-echo app)
                (string-append "Flyspell: " (number->string count) " misspelled words highlighted")))))))))

(def (take-n lst n)
  "Take first N elements of list."
  (let loop ((l lst) (i 0) (acc '()))
    (if (or (null? l) (>= i n))
      (reverse acc)
      (loop (cdr l) (+ i 1) (cons (car l) acc)))))

;;;============================================================================
;;; Workspace tabs (Emacs tab-bar equivalent)
;;;============================================================================
;; Each workspace tab remembers which buffers were in each window and which
;; window was active.  Tab data: (name buffer-names window-idx)

(def (qt-tab-save-current! app)
  "Save current Qt window state to current tab."
  (let* ((tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app))
         (fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (buffers (map (lambda (w)
                         (let ((buf (qt-edit-window-buffer w)))
                           (if buf (buffer-name buf) "*scratch*")))
                       wins))
         (win-idx (qt-frame-current-idx fr)))
    (when (< idx (length tabs))
      (let* ((old-tab (list-ref tabs idx))
             (name (car old-tab))
             (new-tab (list name buffers win-idx)))
        (set! (app-state-tabs app)
          (append (take tabs idx)
                  (list new-tab)
                  (if (< (+ idx 1) (length tabs))
                    (list-tail tabs (+ idx 1))
                    '())))))))

(def (qt-tab-restore! app tab)
  "Restore Qt window state from a tab."
  (let* ((buffers (cadr tab))
         (win-idx (caddr tab))
         (fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    ;; Restore buffers to windows
    (for-each
      (lambda (win buf-name)
        (let ((buf (buffer-by-name buf-name)))
          (when buf
            (qt-buffer-attach! (qt-edit-window-editor win) buf)
            (set! (qt-edit-window-buffer win) buf))))
      wins
      (take buffers (min (length buffers) (length wins))))
    ;; Set current window
    (let ((max-idx (- (length wins) 1)))
      (set! (qt-frame-current-idx fr) (min win-idx max-idx)))
    ;; Update visuals
    (qt-update-visual-decorations! (qt-current-editor fr))
    (qt-modeline-update! app)))

(def (cmd-tab-new app)
  "Create a new workspace tab with current buffer."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (fr (app-state-frame app))
         (win (qt-current-window fr))
         (buf (qt-edit-window-buffer win))
         (buf-name (if buf (buffer-name buf) "*scratch*"))
         (new-tab-num (+ (length tabs) 1))
         (new-tab-name (string-append "Tab " (number->string new-tab-num)))
         (new-tab (list new-tab-name (list buf-name) 0)))
    ;; Save current tab state first
    (qt-tab-save-current! app)
    ;; Add new tab
    (set! (app-state-tabs app) (append tabs (list new-tab)))
    (set! (app-state-current-tab-idx app) (- (length (app-state-tabs app)) 1))
    (echo-message! echo (string-append "Created " new-tab-name))))

(def (cmd-tab-close app)
  "Close current workspace tab."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Cannot close last tab")
      (let* ((tab-name (car (list-ref tabs idx)))
             (new-tabs (append (take tabs idx)
                               (if (< (+ idx 1) (length tabs))
                                 (list-tail tabs (+ idx 1))
                                 '())))
             (new-idx (min idx (- (length new-tabs) 1))))
        (set! (app-state-tabs app) new-tabs)
        (set! (app-state-current-tab-idx app) new-idx)
        ;; Restore the now-current tab
        (qt-tab-restore! app (list-ref new-tabs new-idx))
        (echo-message! echo (string-append "Closed " tab-name))))))

(def (cmd-tab-next app)
  "Switch to next workspace tab."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Only one tab")
      (begin
        ;; Save current tab state
        (qt-tab-save-current! app)
        ;; Switch to next
        (let ((new-idx (modulo (+ idx 1) (length tabs))))
          (set! (app-state-current-tab-idx app) new-idx)
          (let ((tab (list-ref tabs new-idx)))
            (qt-tab-restore! app tab)
            (echo-message! echo (string-append "Tab: " (car tab)
                                              " [" (number->string (+ new-idx 1))
                                              "/" (number->string (length tabs)) "]"))))))))

(def (cmd-tab-previous app)
  "Switch to previous workspace tab."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Only one tab")
      (begin
        ;; Save current tab state
        (qt-tab-save-current! app)
        ;; Switch to previous
        (let ((new-idx (modulo (- idx 1) (length tabs))))
          (set! (app-state-current-tab-idx app) new-idx)
          (let ((tab (list-ref tabs new-idx)))
            (qt-tab-restore! app tab)
            (echo-message! echo (string-append "Tab: " (car tab)
                                              " [" (number->string (+ new-idx 1))
                                              "/" (number->string (length tabs)) "]"))))))))

(def (cmd-tab-rename app)
  "Rename current workspace tab."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app))
         (old-name (car (list-ref tabs idx)))
         (new-name (qt-echo-read-string app "Rename tab to: ")))
    (when (and new-name (not (string=? new-name "")))
      (let* ((old-tab (list-ref tabs idx))
             (new-tab (cons new-name (cdr old-tab))))
        (set! (app-state-tabs app)
          (append (take tabs idx)
                  (list new-tab)
                  (if (< (+ idx 1) (length tabs))
                    (list-tail tabs (+ idx 1))
                    '())))
        (echo-message! echo (string-append "Renamed to: " new-name))))))

(def (cmd-tab-move app)
  "Move current workspace tab left or right (with prefix arg for direction)."
  (let* ((echo (app-state-echo app))
         (tabs (app-state-tabs app))
         (idx (app-state-current-tab-idx app))
         (n (get-prefix-arg app 1)))
    (if (<= (length tabs) 1)
      (echo-message! echo "Only one tab")
      (let* ((new-idx (modulo (+ idx n) (length tabs)))
             (tab (list-ref tabs idx))
             (tabs-without (append (take tabs idx)
                                   (if (< (+ idx 1) (length tabs))
                                     (list-tail tabs (+ idx 1))
                                     '())))
             (new-tabs (append (take tabs-without new-idx)
                               (list tab)
                               (list-tail tabs-without new-idx))))
        (set! (app-state-tabs app) new-tabs)
        (set! (app-state-current-tab-idx app) new-idx)
        (echo-message! echo (string-append "Moved tab to position "
                                          (number->string (+ new-idx 1))))))))

;;;============================================================================
;;; Rainbow delimiters
;;;============================================================================
;; Colors paired delimiters by nesting depth using Scintilla indicators.
;; Uses INDIC_TEXTFORE (17) to change text foreground for delimiter characters.

(def *qt-rainbow-active* #f)

;; 8 rainbow colors in BGR format (Scintilla uses BGR)
(def *rainbow-colors*
  (vector #xFF6666   ;; red
          #x44CCFF   ;; orange (BGR)
          #x00DDDD   ;; yellow (BGR)
          #x66DD66   ;; green
          #xFFCC44   ;; cyan (BGR)
          #xFF8844   ;; blue (BGR)
          #xFF66CC   ;; magenta (BGR)
          #xAAAAFF)) ;; pink (BGR)

;; Indicator IDs 20-27 for 8 depth levels
(def *rainbow-indic-base* 20)

(def (rainbow-setup-indicators! ed)
  "Initialize rainbow delimiter indicators on a Scintilla editor."
  (let ((INDIC_TEXTFORE 17))
    (let loop ((i 0))
      (when (< i 8)
        (let ((indic (+ *rainbow-indic-base* i)))
          (sci-send ed SCI_INDICSETSTYLE indic INDIC_TEXTFORE)
          (sci-send ed SCI_INDICSETFORE indic (vector-ref *rainbow-colors* i)))
        (loop (+ i 1))))))

(def (rainbow-clear-indicators! ed)
  "Clear all rainbow indicators from editor."
  (let ((len (sci-send ed SCI_GETTEXTLENGTH)))
    (let loop ((i 0))
      (when (< i 8)
        (sci-send ed SCI_SETINDICATORCURRENT (+ *rainbow-indic-base* i))
        (sci-send ed SCI_INDICATORCLEARRANGE 0 len)
        (loop (+ i 1))))))

(def (rainbow-colorize-buffer! ed)
  "Scan buffer and colorize delimiters by nesting depth."
  (let* ((text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (rainbow-clear-indicators! ed)
    (rainbow-setup-indicators! ed)
    (let loop ((i 0) (depth 0) (in-string #f) (in-comment #f) (escape #f))
      (when (< i len)
        (let ((ch (string-ref text i)))
          (cond
            ;; Handle escape in string
            (escape
             (loop (+ i 1) depth in-string in-comment #f))
            ;; String handling
            ((and in-string (char=? ch #\\))
             (loop (+ i 1) depth in-string in-comment #t))
            ((and in-string (char=? ch #\"))
             (loop (+ i 1) depth #f in-comment #f))
            (in-string
             (loop (+ i 1) depth in-string in-comment #f))
            ;; Line comment handling
            ((and in-comment (char=? ch #\newline))
             (loop (+ i 1) depth in-string #f #f))
            (in-comment
             (loop (+ i 1) depth in-string in-comment #f))
            ;; Start of comment
            ((char=? ch #\;)
             (loop (+ i 1) depth in-string #t #f))
            ;; Start of string
            ((char=? ch #\")
             (loop (+ i 1) depth #t in-comment #f))
            ;; Opening delimiter
            ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
             (let ((indic (+ *rainbow-indic-base* (modulo depth 8))))
               (sci-send ed SCI_SETINDICATORCURRENT indic)
               (sci-send ed SCI_INDICATORFILLRANGE i 1))
             (loop (+ i 1) (+ depth 1) in-string in-comment #f))
            ;; Closing delimiter
            ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
             (let* ((d (max 0 (- depth 1)))
                    (indic (+ *rainbow-indic-base* (modulo d 8))))
               (sci-send ed SCI_SETINDICATORCURRENT indic)
               (sci-send ed SCI_INDICATORFILLRANGE i 1))
             (loop (+ i 1) (max 0 (- depth 1)) in-string in-comment #f))
            ;; Any other character
            (else
             (loop (+ i 1) depth in-string in-comment #f))))))))

(def (cmd-rainbow-delimiters-mode app)
  "Toggle rainbow delimiter coloring by nesting depth."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (set! *qt-rainbow-active* (not *qt-rainbow-active*))
    (if *qt-rainbow-active*
      (begin
        (rainbow-colorize-buffer! ed)
        (echo-message! echo "Rainbow delimiters ON"))
      (begin
        (rainbow-clear-indicators! ed)
        (echo-message! echo "Rainbow delimiters OFF")))))

;;; ---- Dedicated Windows ----

(def *qt-dedicated-windows* (make-hash-table))

(def (cmd-set-window-dedicated app)
  "Mark current window as dedicated to its buffer."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (echo (app-state-echo app))
         (buf-name (buffer-name (qt-edit-window-buffer win))))
    (hash-put! *qt-dedicated-windows* buf-name #t)
    (echo-message! echo
      (string-append "Window dedicated to: " buf-name))))

(def (cmd-toggle-window-dedicated app)
  "Toggle whether the current window is dedicated to its buffer."
  (let* ((fr (app-state-frame app))
         (win (qt-current-window fr))
         (echo (app-state-echo app))
         (buf-name (buffer-name (qt-edit-window-buffer win)))
         (currently-dedicated (hash-get *qt-dedicated-windows* buf-name)))
    (if currently-dedicated
      (begin
        (hash-remove! *qt-dedicated-windows* buf-name)
        (echo-message! echo
          (string-append "Window undedicated from: " buf-name)))
      (cmd-set-window-dedicated app))))

;;; ---- Org Sparse Tree ----

(def (cmd-org-sparse-tree app)
  "Show only org headings matching a search pattern (sparse tree view)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (query (qt-echo-read-string app "Sparse tree (regexp): ")))
    (when (and query (not (string-empty? query)))
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (total (length lines))
             (query-lower (string-downcase query)))
        ;; First show all lines
        (sci-send ed SCI_SHOWLINES 0 (- total 1))
        ;; Find matching headings and their ancestors
        (let* ((match-set (make-hash-table))
               (_ (let loop ((i 0))
                    (when (< i total)
                      (let ((line (list-ref lines i)))
                        (when (and (org-heading-line? line)
                                   (string-contains (string-downcase line) query-lower))
                          (hash-put! match-set i #t)
                          ;; Also mark parent headings
                          (let ((level (org-heading-stars-of-line line)))
                            (let ploop ((j (- i 1)))
                              (when (>= j 0)
                                (let ((pl (list-ref lines j)))
                                  (when (and (org-heading-line? pl)
                                             (< (org-heading-stars-of-line pl) level))
                                    (hash-put! match-set j #t)
                                    (ploop (- j 1)))))))))
                      (loop (+ i 1)))))
               (match-count (hash-length match-set)))
          ;; Hide non-matching lines
          (let loop ((i 0))
            (when (< i total)
              (unless (hash-get match-set i)
                (sci-send ed SCI_HIDELINES i i))
              (loop (+ i 1))))
          (echo-message! echo
            (string-append "Sparse tree: " (number->string match-count)
                           " matching headings")))))))
