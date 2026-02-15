;;; -*- Gerbil -*-
;;; Qt commands ide - insert, copy, buffer management, file ops, describe, VCS
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gerbil-qt/qt
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
        :gerbil-emacs/qt/commands-edit
        :gerbil-emacs/qt/commands-search
        :gerbil-emacs/qt/commands-file
        :gerbil-emacs/qt/commands-sexp)

;;;============================================================================
;;; Insert commands
;;;============================================================================

(def (cmd-insert-pair-braces app)
  "Insert a pair of braces with cursor between."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "{}")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

(def (cmd-insert-pair-quotes app)
  "Insert a pair of double quotes with cursor between."
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-insert-text! ed "\"\"")
    (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))

(def (cmd-insert-newline-above app)
  "Insert a blank line above the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (line-start (line-start-position text line)))
    (qt-plain-text-edit-set-cursor-position! ed line-start)
    (qt-plain-text-edit-insert-text! ed "\n")
    (qt-plain-text-edit-set-cursor-position! ed line-start)))

(def (cmd-insert-newline-below app)
  "Insert a blank line below the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines)) (list-ref lines line) ""))
         (line-start (line-start-position text line))
         (line-end (+ line-start (string-length line-text))))
    (qt-plain-text-edit-set-cursor-position! ed line-end)
    (qt-plain-text-edit-insert-text! ed "\n")))

(def (cmd-insert-comment-separator app)
  "Insert a comment separator line."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-insert-text! ed
      "\n;;; ============================================================\n")))

(def (cmd-insert-line-number app)
  "Insert the current line number at point."
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed))))
    (qt-plain-text-edit-insert-text! ed (number->string line))))

(def (cmd-insert-buffer-filename app)
  "Insert the buffer's filename at point."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (qt-plain-text-edit-insert-text! (current-qt-editor app)
        (path-strip-directory path))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-insert-timestamp app)
  "Insert a timestamp at point."
  (let* ((now (time->seconds (current-time)))
         (ts (number->string (inexact->exact (floor now)))))
    (qt-plain-text-edit-insert-text! (current-qt-editor app) ts)))

(def (cmd-insert-shebang app)
  "Insert a shebang line at the beginning of the buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (if (string-prefix? "#!" text)
      (echo-message! (app-state-echo app) "Shebang already present")
      (let ((shebang (qt-echo-read-string app "Shebang (e.g. /usr/bin/env python3): ")))
        (when shebang
          (let ((new-text (string-append "#!" shebang "\n" text)))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))

;;;============================================================================
;;; Buffer management
;;;============================================================================

(def (cmd-count-buffers app)
  "Show the number of open buffers."
  (let ((n (length (buffer-list))))
    (echo-message! (app-state-echo app)
      (string-append (number->string n) " buffers"))))

(def (cmd-rename-uniquely app)
  "Rename buffer to a unique name."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf))
         (new-name (string-append name "<" (number->string (random-integer 1000)) ">")))
    (set! (buffer-name buf) new-name)
    (echo-message! (app-state-echo app) (string-append "Renamed to " new-name))))

(def (cmd-bury-buffer app)
  "Switch to the next buffer (bury current)."
  (cmd-next-buffer app))

(def (cmd-unbury-buffer app)
  "Switch to the previous buffer."
  (cmd-previous-buffer app))

(def (cmd-append-to-buffer app)
  "Append region to another buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (bufs (buffer-list))
             (names (map buffer-name bufs))
             (target-name (qt-echo-read-string-with-completion app "Append to buffer: " names)))
        (when target-name
          (let ((target-buf (find (lambda (b) (string=? (buffer-name b) target-name)) bufs)))
            (if target-buf
              (begin
                ;; Find editor showing target buffer
                (let ((fr (app-state-frame app)))
                  (let loop ((wins (qt-frame-windows fr)))
                    (when (pair? wins)
                      (if (eq? (qt-edit-window-buffer (car wins)) target-buf)
                        (qt-plain-text-edit-append!
                          (qt-edit-window-editor (car wins)) region)
                        (loop (cdr wins))))))
                (echo-message! (app-state-echo app)
                  (string-append "Appended to " target-name)))
              (echo-error! (app-state-echo app) "Buffer not found"))))))))

;;;============================================================================
;;; File operations
;;;============================================================================

(def (cmd-make-directory app)
  "Create a directory."
  (let ((dir (qt-echo-read-string app "Create directory: ")))
    (when dir
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app)
                      (string-append "Failed: " (with-output-to-string
                        (lambda () (display-exception e))))))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app) (string-append "Created: " dir)))))))

(def (cmd-delete-file app)
  "Delete a file."
  (let ((file (qt-echo-read-string app "Delete file: ")))
    (when file
      (if (file-exists? file)
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "Delete failed"))
          (lambda ()
            (delete-file file)
            (echo-message! (app-state-echo app) (string-append "Deleted: " file))))
        (echo-error! (app-state-echo app) "File not found")))))

(def (cmd-copy-file app)
  "Copy a file."
  (let ((src (qt-echo-read-string app "Copy from: ")))
    (when src
      (let ((dst (qt-echo-read-string app "Copy to: ")))
        (when dst
          (with-catch
            (lambda (e) (echo-error! (app-state-echo app) "Copy failed"))
            (lambda ()
              (let ((text (read-file-as-string src)))
                (when text
                  (call-with-output-file dst
                    (lambda (p) (display text p)))
                  (echo-message! (app-state-echo app)
                    (string-append "Copied " src " -> " dst)))))))))))

(def (cmd-list-directory app)
  "List directory contents in a buffer."
  (let ((dir (qt-echo-read-string app "List directory: ")))
    (when dir
      (if (and (file-exists? dir)
               (eq? 'directory (file-info-type (file-info dir))))
        (dired-open-directory! app dir)
        (echo-error! (app-state-echo app) "Not a directory")))))

(def (cmd-pwd app)
  "Show current working directory."
  (echo-message! (app-state-echo app) (current-directory)))

;;;============================================================================
;;; Dired commands
;;;============================================================================

(def (cmd-dired-create-directory app)
  "Create a new directory from dired."
  (let ((dir (qt-echo-read-string app "New directory: ")))
    (when dir
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Failed to create directory"))
        (lambda ()
          (create-directory dir)
          (echo-message! (app-state-echo app) (string-append "Created: " dir)))))))

(def (cmd-dired-do-rename app)
  "Rename file in dired."
  (let ((old (qt-echo-read-string app "Rename: ")))
    (when old
      (let ((new-name (qt-echo-read-string app "To: ")))
        (when new-name
          (with-catch
            (lambda (e) (echo-error! (app-state-echo app) "Rename failed"))
            (lambda ()
              (rename-file old new-name)
              (echo-message! (app-state-echo app) "Renamed"))))))))

(def (cmd-dired-do-delete app)
  "Delete file in dired."
  (let ((file (qt-echo-read-string app "Delete: ")))
    (when file
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Delete failed"))
        (lambda ()
          (delete-file file)
          (echo-message! (app-state-echo app) "Deleted"))))))

(def (cmd-dired-do-copy app)
  "Copy file in dired."
  (cmd-copy-file app))

;;;============================================================================
;;; Toggle commands (additional)
;;;============================================================================

(def *hl-line-mode* #f)
(def (cmd-toggle-hl-line app)
  "Toggle current line highlighting."
  (set! *hl-line-mode* (not *hl-line-mode*))
  (echo-message! (app-state-echo app)
    (if *hl-line-mode* "Line highlight ON" "Line highlight OFF")))

(def (cmd-toggle-show-tabs app)
  "Toggle tab character visibility."
  (echo-message! (app-state-echo app) "Tab visibility toggled"))

(def (cmd-toggle-show-eol app)
  "Toggle end-of-line marker visibility."
  (echo-message! (app-state-echo app) "EOL markers toggled"))

(def *narrowing-indicator* #f)
(def (cmd-toggle-narrowing-indicator app)
  "Toggle narrowing indicator in modeline."
  (set! *narrowing-indicator* (not *narrowing-indicator*))
  (echo-message! (app-state-echo app)
    (if *narrowing-indicator* "Narrowing indicator ON" "Narrowing indicator OFF")))

(def *debug-on-error* #f)
(def (cmd-toggle-debug-on-error app)
  "Toggle debug-on-error mode."
  (set! *debug-on-error* (not *debug-on-error*))
  (echo-message! (app-state-echo app)
    (if *debug-on-error* "Debug on error ON" "Debug on error OFF")))

(def (cmd-toggle-fold app)
  "Toggle code folding at point (placeholder)."
  (echo-message! (app-state-echo app) "Code folding not supported in Qt plain text mode"))

;;;============================================================================
;;; Info/describe commands
;;;============================================================================

(def (cmd-what-mode app)
  "Show the current buffer's mode."
  (let* ((buf (current-qt-buffer app))
         (lang (buffer-lexer-lang buf)))
    (echo-message! (app-state-echo app)
      (if lang
        (string-append "Mode: " (symbol->string lang))
        "Mode: fundamental"))))

(def (cmd-what-encoding app)
  "Show the current buffer encoding."
  (echo-message! (app-state-echo app) "Encoding: UTF-8"))

(def (cmd-what-line-col app)
  "Show line and column of cursor."
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (col (+ 1 (qt-plain-text-edit-cursor-column ed))))
    (echo-message! (app-state-echo app)
      (string-append "Line " (number->string line)
                     ", Col " (number->string col)))))

(def (cmd-show-file-info app)
  "Show information about the current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-message! (app-state-echo app) (string-append (buffer-name buf) " (no file)"))
      (if (file-exists? path)
        (let* ((info (file-info path))
               (size (file-info-size info)))
          (echo-message! (app-state-echo app)
            (string-append path " (" (number->string size) " bytes)")))
        (echo-message! (app-state-echo app)
          (string-append path " (new file)"))))))

(def (cmd-show-buffer-size app)
  "Show the size of the current buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (size (string-length text)))
    (echo-message! (app-state-echo app)
      (string-append "Buffer size: " (number->string size) " chars"))))

(def (cmd-show-column-number app)
  "Show the current column number."
  (let* ((ed (current-qt-editor app))
         (col (+ 1 (qt-plain-text-edit-cursor-column ed))))
    (echo-message! (app-state-echo app)
      (string-append "Column: " (number->string col)))))

(def (cmd-emacs-version app)
  "Show gerbil-emacs version."
  (echo-message! (app-state-echo app) "gerbil-emacs (Qt backend)"))

;;;============================================================================
;;; Git/VCS commands
;;;============================================================================

(def (run-git-command app args buffer-name)
  "Run a git command and show output in a buffer."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (with-catch
      (lambda (e) (echo-error! (app-state-echo app)
                    (string-append "Git error: " (with-output-to-string
                      (lambda () (display-exception e))))))
      (lambda ()
        (let* ((proc (open-process
                        (list path: "/usr/bin/git"
                              arguments: args
                              directory: dir
                              stdout-redirection: #t
                              stderr-redirection: #t)))
               (output (read-line proc #f))
               (_ (process-status proc)))
          (close-port proc)
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app))
                 (git-buf (qt-buffer-create! buffer-name ed #f)))
            (qt-buffer-attach! ed git-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) git-buf)
            (qt-plain-text-edit-set-text! ed (or output ""))
            (qt-text-document-set-modified! (buffer-doc-pointer git-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))

(def (cmd-show-git-status app)
  "Show git status."
  (run-git-command app '("status") "*Git Status*"))

(def (cmd-show-git-log app)
  "Show git log."
  (run-git-command app '("log" "--oneline" "-30") "*Git Log*"))

(def (cmd-show-git-diff app)
  "Show git diff with syntax coloring."
  (run-git-command app '("diff") "*Git Diff*")
  (qt-highlight-diff! (current-qt-editor app)))

(def (cmd-show-git-blame app)
  "Show git blame for current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (run-git-command app (list "blame" path) "*Git Blame*")
      (echo-error! (app-state-echo app) "Buffer has no file"))))

;;;============================================================================
;;; Magit-style interactive git interface
;;;============================================================================

(def *magit-dir* #f)  ;; working directory for magit operations

(def (magit-run-git args dir)
  "Run a git command and return output as string."
  (with-catch
    (lambda (e) "")
    (lambda ()
      (let* ((proc (open-process
                      (list path: "/usr/bin/git"
                            arguments: args
                            directory: dir
                            stdout-redirection: #t
                            stderr-redirection: #t)))
             (output (read-line proc #f))
             (_ (process-status proc)))
        (close-port proc)
        (or output "")))))

(def (magit-parse-status output)
  "Parse git status --porcelain output into list of (status . filename)."
  (let ((lines (string-split output #\newline)))
    (filter identity
      (map (lambda (line)
             (and (>= (string-length line) 3)
                  (let ((status (substring line 0 2))
                        (file (substring line 3 (string-length line))))
                    (cons (string-trim status) file))))
           lines))))

(def (magit-format-status entries branch)
  "Format magit-style status buffer."
  (let ((out (open-output-string)))
    (display (string-append "Head: " branch "\n\n") out)
    (let ((staged (filter (lambda (e)
                            (let ((s (car e)))
                              (and (> (string-length s) 0)
                                   (not (string=? s "??"))
                                   (not (string=? s ""))
                                   (let ((ch (string-ref s 0)))
                                     (and (not (char=? ch #\space))
                                          (not (char=? ch #\?)))))))
                          entries))
          (unstaged (filter (lambda (e)
                              (let ((s (car e)))
                                (and (> (string-length s) 0)
                                     (not (string=? s "??"))
                                     (>= (string-length s) 2)
                                     (let ((ch (string-ref s (min 1 (- (string-length s) 1)))))
                                       (and (not (char=? ch #\space))
                                            (not (char=? ch #\?)))))))
                            entries))
          (untracked (filter (lambda (e) (string=? (car e) "??")) entries)))
      (when (not (null? staged))
        (display "Staged changes:\n" out)
        (for-each (lambda (e)
                    (display (string-append "  " (car e) " " (cdr e) "\n") out))
                  staged)
        (display "\n" out))
      (when (not (null? unstaged))
        (display "Unstaged changes:\n" out)
        (for-each (lambda (e)
                    (display (string-append "  " (car e) " " (cdr e) "\n") out))
                  unstaged)
        (display "\n" out))
      (when (not (null? untracked))
        (display "Untracked files:\n" out)
        (for-each (lambda (e)
                    (display (string-append "  ?? " (cdr e) "\n") out))
                  untracked)
        (display "\n" out))
      (when (and (null? staged) (null? unstaged) (null? untracked))
        (display "Nothing to commit, working tree clean.\n" out))
      (display "\nKeys: s=stage u=unstage c=commit d=diff g=refresh q=quit\n" out))
    (get-output-string out)))

(def (magit-file-at-point text pos)
  "Extract the filename from the current line in magit buffer."
  (let* ((line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1) (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1)))))
         (line (substring text line-start line-end)))
    ;; Lines look like "  M filename" or "  ?? filename"
    (let ((trimmed (string-trim line)))
      (cond
        ((and (>= (string-length trimmed) 3)
              (string=? (substring trimmed 0 2) "??"))
         (string-trim (substring trimmed 2 (string-length trimmed))))
        ((and (>= (string-length trimmed) 2)
              (memv (string-ref trimmed 0)
                    '(#\M #\A #\D #\R #\C #\U)))
         (string-trim (substring trimmed 1 (string-length trimmed))))
        (else #f)))))

(def (cmd-magit-status app)
  "Open interactive git status buffer (magit-style)."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (set! *magit-dir* dir)
    (let* ((status-output (magit-run-git '("status" "--porcelain") dir))
           (branch-output (magit-run-git '("rev-parse" "--abbrev-ref" "HEAD") dir))
           (branch (string-trim branch-output))
           (entries (magit-parse-status status-output))
           (text (magit-format-status entries branch))
           (ed (current-qt-editor app))
           (fr (app-state-frame app))
           (git-buf (or (buffer-by-name "*Magit*")
                        (qt-buffer-create! "*Magit*" ed #f))))
      (qt-buffer-attach! ed git-buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) git-buf)
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer git-buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (echo-message! (app-state-echo app) "*Magit*"))))

(def (cmd-magit-stage app)
  "Stage the file at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (file (magit-file-at-point text pos)))
        (if file
          (begin
            (magit-run-git (list "add" file) *magit-dir*)
            (cmd-magit-status app)
            (echo-message! (app-state-echo app)
              (string-append "Staged: " file)))
          (echo-error! (app-state-echo app) "No file at point"))))))

(def (cmd-magit-unstage app)
  "Unstage the file at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (file (magit-file-at-point text pos)))
        (if file
          (begin
            (magit-run-git (list "reset" "HEAD" file) *magit-dir*)
            (cmd-magit-status app)
            (echo-message! (app-state-echo app)
              (string-append "Unstaged: " file)))
          (echo-error! (app-state-echo app) "No file at point"))))))

(def (cmd-magit-commit app)
  "Commit staged changes."
  (let ((msg (qt-echo-read-string app "Commit message: ")))
    (when (and msg (> (string-length msg) 0))
      (let ((output (magit-run-git (list "commit" "-m" msg) *magit-dir*)))
        (cmd-magit-status app)
        (echo-message! (app-state-echo app)
          (string-append "Committed: " msg))))))

(def (cmd-magit-diff app)
  "Show diff for file at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (file (magit-file-at-point text pos)))
        (if file
          (let* ((diff-output (magit-run-git (list "diff" file) *magit-dir*))
                 (staged-diff (magit-run-git (list "diff" "--cached" file) *magit-dir*))
                 (full-diff (if (> (string-length staged-diff) 0) staged-diff diff-output))
                 (fr (app-state-frame app))
                 (diff-buf (or (buffer-by-name "*Magit Diff*")
                               (qt-buffer-create! "*Magit Diff*" ed #f))))
            (qt-buffer-attach! ed diff-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
            (qt-plain-text-edit-set-text! ed
              (if (string=? full-diff "") "No differences.\n" full-diff))
            (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (qt-highlight-diff! ed))
          (echo-error! (app-state-echo app) "No file at point"))))))

(def (cmd-magit-stage-all app)
  "Stage all changes."
  (when *magit-dir*
    (magit-run-git '("add" "-A") *magit-dir*)
    (cmd-magit-status app)
    (echo-message! (app-state-echo app) "All changes staged")))

(def (cmd-magit-log app)
  "Show git log in magit buffer."
  (when *magit-dir*
    (let* ((output (magit-run-git '("log" "--oneline" "--graph" "-30") *magit-dir*))
           (ed (current-qt-editor app))
           (fr (app-state-frame app))
           (log-buf (or (buffer-by-name "*Magit Log*")
                        (qt-buffer-create! "*Magit Log*" ed #f))))
      (qt-buffer-attach! ed log-buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) log-buf)
      (qt-plain-text-edit-set-text! ed (or output ""))
      (qt-text-document-set-modified! (buffer-doc-pointer log-buf) #f)
      (qt-plain-text-edit-set-cursor-position! ed 0))))

;;;============================================================================
;;; Text manipulation
;;;============================================================================

(def (cmd-comment-region app)
  "Add comment prefix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (commented (map (lambda (l) (string-append ";; " l)) lines))
             (result (string-join commented "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Commented")))))

(def (cmd-uncomment-region app)
  "Remove comment prefix from each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (uncommented (map (lambda (l)
                                 (cond
                                   ((string-prefix? ";; " l) (substring l 3 (string-length l)))
                                   ((string-prefix? ";" l) (substring l 1 (string-length l)))
                                   ((string-prefix? "# " l) (substring l 2 (string-length l)))
                                   ((string-prefix? "//" l) (substring l 2 (string-length l)))
                                   (else l)))
                               lines))
             (result (string-join uncommented "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Uncommented")))))

(def (cmd-collapse-blank-lines app)
  "Collapse multiple blank lines into one."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (result (let loop ((s text))
                   (let ((idx (string-contains s "\n\n\n")))
                     (if idx
                       (loop (string-append (substring s 0 (+ idx 1))
                                            (substring s (+ idx 2) (string-length s))))
                       s)))))
    (qt-plain-text-edit-set-text! ed result)
    (echo-message! (app-state-echo app) "Blank lines collapsed")))

(def (cmd-remove-blank-lines app)
  "Remove all blank lines from region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (lines (string-split region #\newline))
             (non-blank (filter (lambda (l) (> (string-length (string-trim-both l)) 0)) lines))
             (result (string-join non-blank "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! (app-state-echo app) "Blank lines removed")))))

(def (cmd-delete-trailing-lines app)
  "Delete trailing blank lines at end of buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (trimmed (let loop ((s text))
                    (if (string-suffix? "\n\n" s)
                      (loop (substring s 0 (- (string-length s) 1)))
                      s))))
    (qt-plain-text-edit-set-text! ed trimmed)
    (echo-message! (app-state-echo app) "Trailing lines deleted")))

(def (cmd-trim-lines app)
  "Trim trailing whitespace from all lines."
  (cmd-delete-trailing-whitespace app))

(def (cmd-prefix-lines app)
  "Add a prefix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((prefix (qt-echo-read-string app "Prefix: ")))
        (when prefix
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (prefixed (map (lambda (l) (string-append prefix l)) lines))
                 (result (string-join prefixed "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)))))))

(def (cmd-suffix-lines app)
  "Add a suffix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((suffix (qt-echo-read-string app "Suffix: ")))
        (when suffix
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (suffixed (map (lambda (l) (string-append l suffix)) lines))
                 (result (string-join suffixed "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)))))))

;;;============================================================================
;;; Sort variants
;;;============================================================================

(def (cmd-sort-lines-reverse app)
  "Sort lines in region in reverse order."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines string>?))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted (reverse)")))))

(def (cmd-sort-lines-case-fold app)
  "Sort lines case-insensitively."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines (lambda (a b) (string<? (string-downcase a) (string-downcase b)))))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted (case-insensitive)")))))

(def (cmd-uniquify-lines app)
  "Remove duplicate lines in region."
  (cmd-delete-duplicate-lines app))

(def (cmd-sort-words app)
  "Sort words on the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (words (filter (lambda (s) (> (string-length s) 0))
                            (string-split line-text #\space)))
             (sorted-words (sort words string<?))
             (new-line (string-join sorted-words " "))
             (new-lines (let loop ((ls lines) (i 0) (acc '()))
                          (if (null? ls) (reverse acc)
                            (loop (cdr ls) (+ i 1)
                                  (cons (if (= i line) new-line (car ls)) acc)))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (line-start-position new-text line))
        (echo-message! (app-state-echo app) "Words sorted")))))

;;;============================================================================
;;; Case conversion helpers
;;;============================================================================

(def (cmd-camel-to-snake app)
  "Convert camelCase word at point to snake_case."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\_))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (snake (let loop ((i 0) (acc ""))
                      (if (>= i (string-length word)) acc
                        (let ((ch (string-ref word i)))
                          (if (and (char-upper-case? ch) (> i 0))
                            (loop (+ i 1) (string-append acc "_" (string (char-downcase ch))))
                            (loop (+ i 1) (string-append acc (string (char-downcase ch)))))))))
             (new-text (string-append (substring text 0 start) snake
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length snake)))))))

(def (cmd-snake-to-camel app)
  "Convert snake_case word at point to camelCase."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\_))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (camel (let loop ((i 0) (acc "") (cap? #f))
                      (if (>= i (string-length word)) acc
                        (let ((ch (string-ref word i)))
                          (if (char=? ch #\_)
                            (loop (+ i 1) acc #t)
                            (loop (+ i 1)
                                  (string-append acc (string (if cap? (char-upcase ch) ch)))
                                  #f))))))
             (new-text (string-append (substring text 0 start) camel
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length camel)))))))

;;;============================================================================
;;; Search helpers
;;;============================================================================

(def (cmd-highlight-word-at-point app)
  "Highlight the word at point as search term."
  (cmd-highlight-symbol app))


;;;============================================================================
;;; Wgrep — editable grep results
;;;============================================================================

(def *wgrep-mode* #f) ;; #t when *Grep* buffer is in wgrep edit mode
(def *wgrep-original-lines* []) ;; original grep result lines for diffing

(def (cmd-wgrep-change-to-wgrep-mode app)
  "Make *Grep* buffer editable for wgrep-style batch editing."
  (let ((buf (current-qt-buffer app)))
    (if (not (string=? (buffer-name buf) "*Grep*"))
      (echo-error! (app-state-echo app) "Not in *Grep* buffer")
      (begin
        (set! *wgrep-mode* #t)
        ;; Store original result lines for later comparison
        (set! *wgrep-original-lines*
          (map (lambda (r)
                 (string-append (car r) ":"
                   (number->string (cadr r)) ":"
                   (caddr r)))
               *grep-results*))
        (echo-message! (app-state-echo app)
          "Wgrep mode: edit results, then C-c C-c to apply or C-c C-k to abort")))))

(def (cmd-wgrep-finish-edit app)
  "Apply wgrep changes back to source files."
  (let ((buf (current-qt-buffer app)))
    (if (not (and (string=? (buffer-name buf) "*Grep*") *wgrep-mode*))
      (echo-error! (app-state-echo app) "Not in wgrep mode")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (all-lines (let loop ((s text) (acc []))
                          (let ((nl (string-index s #\newline)))
                            (if nl
                              (loop (substring s (+ nl 1) (string-length s))
                                    (cons (substring s 0 nl) acc))
                              (reverse (if (> (string-length s) 0) (cons s acc) acc))))))
             ;; Extract only file:line:text result lines (skip header)
             (result-lines (filter (lambda (l) (parse-grep-line l)) all-lines))
             (changes 0))
        ;; Group changes by file
        (let ((file-changes (make-hash-table)))
          (for-each
            (lambda (line)
              (let ((parsed (parse-grep-line line)))
                (when parsed
                  (let ((file (car parsed))
                        (line-num (cadr parsed))
                        (new-text (caddr parsed)))
                    ;; Check if line changed from original
                    (let* ((orig (let loop ((results *grep-results*))
                                  (if (null? results) #f
                                    (let ((r (car results)))
                                      (if (and (string=? (car r) file)
                                               (= (cadr r) line-num))
                                        (caddr r)
                                        (loop (cdr results)))))))
                           (changed? (and orig (not (string=? orig new-text)))))
                      (when changed?
                        (let ((existing (or (hash-get file-changes file) [])))
                          (hash-put! file-changes file
                            (cons (cons line-num new-text) existing)))))))))
            result-lines)
          ;; Apply changes to each file
          (hash-for-each
            (lambda (file line-edits)
              (when (file-exists? file)
                (let* ((content (read-file-as-string file))
                       (lines (let loop ((s content) (acc []))
                                (let ((nl (string-index s #\newline)))
                                  (if nl
                                    (loop (substring s (+ nl 1) (string-length s))
                                          (cons (substring s 0 nl) acc))
                                    (reverse (if (> (string-length s) 0) (cons s acc) acc))))))
                       (new-lines
                         (let loop ((ls lines) (i 1) (acc []))
                           (if (null? ls) (reverse acc)
                             (let ((edit (assoc i line-edits)))
                               (loop (cdr ls) (+ i 1)
                                     (cons (if edit (cdr edit) (car ls)) acc))))))
                       (new-content (string-join new-lines "\n")))
                  ;; Add trailing newline if original had one
                  (let ((final (if (and (> (string-length content) 0)
                                       (char=? (string-ref content (- (string-length content) 1))
                                               #\newline))
                                 (string-append new-content "\n")
                                 new-content)))
                    (write-string-to-file file final)
                    (set! changes (+ changes (length line-edits)))))))
            file-changes))
        (set! *wgrep-mode* #f)
        (echo-message! (app-state-echo app)
          (string-append "Applied " (number->string changes) " change(s)"))))))

(def (cmd-wgrep-abort-changes app)
  "Abort wgrep changes and restore original *Grep* buffer."
  (if (not *wgrep-mode*)
    (echo-error! (app-state-echo app) "Not in wgrep mode")
    (begin
      (set! *wgrep-mode* #f)
      ;; Restore original content
      (let* ((ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (header (string-append "-*- grep -*-\n\n"))
             (result-text (if (null? *wgrep-original-lines*)
                            (string-append header "No matches found.\n")
                            (string-append header
                              (number->string (length *wgrep-original-lines*)) " matches\n\n"
                              (string-join *wgrep-original-lines* "\n")
                              "\n\nPress Enter on a result line to jump to source."))))
        (qt-plain-text-edit-set-text! ed result-text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0))
      (echo-message! (app-state-echo app) "Wgrep changes aborted"))))

;;;============================================================================
;;; Quoted insert
;;;============================================================================

(def (cmd-quoted-insert app)
  "Insert the next character literally."
  (echo-message! (app-state-echo app) "C-q: type character to insert literally"))

;;;============================================================================
;;; Quick calc
;;;============================================================================

(def (cmd-quick-calc app)
  "Quick calculator (alias for calc)."
  (cmd-calc app))

;;;============================================================================
;;; Eval and insert
;;;============================================================================

(def (cmd-eval-and-insert app)
  "Evaluate expression and insert result at point."
  (let ((expr (qt-echo-read-string app "Eval and insert: ")))
    (when (and expr (> (string-length expr) 0))
      (let-values (((result error?) (eval-expression-string expr)))
        (if error?
          (echo-error! (app-state-echo app) (string-append "Error: " result))
          (qt-plain-text-edit-insert-text! (current-qt-editor app) result))))))

;;;============================================================================
;;; Shell command insert
;;;============================================================================

(def (cmd-shell-command-insert app)
  "Run shell command and insert output at point."
  (let ((cmd (qt-echo-read-string app "Shell command (insert): ")))
    (when cmd
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Command failed"))
        (lambda ()
          (let* ((proc (open-process
                          (list path: "/bin/sh"
                                arguments: (list "-c" cmd)
                                stdout-redirection: #t)))
                 (output (read-line proc #f))
                 (_ (process-status proc)))
            (close-port proc)
            (when output
              (qt-plain-text-edit-insert-text! (current-qt-editor app) output))))))))

;;;============================================================================
;;; Pipe region
;;;============================================================================

(def (cmd-pipe-region app)
  "Pipe region through shell command."
  (cmd-shell-command-on-region app))


;;;============================================================================
;;; Session persistence (desktop save/restore)
;;;============================================================================

(def *session-path*
  (path-expand ".gerbil-emacs-session" (user-info-home (user-info (user-name)))))

(def (session-save! app)
  "Save current session (open file buffers + positions) to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((ed (current-qt-editor app))
             (fr (app-state-frame app))
             (current-buf (current-qt-buffer app))
             ;; Collect file buffers with positions
             (entries
               (let loop ((bufs *buffer-list*) (acc []))
                 (if (null? bufs) (reverse acc)
                   (let ((buf (car bufs)))
                     (if (buffer-file-path buf)
                       ;; Get position: attach temporarily to read cursor pos
                       (let ((pos (begin
                                    (qt-buffer-attach! ed buf)
                                    (qt-plain-text-edit-cursor-position ed))))
                         (loop (cdr bufs) (cons (cons (buffer-file-path buf) pos) acc)))
                       (loop (cdr bufs) acc)))))))
        ;; Restore current buffer
        (qt-buffer-attach! ed current-buf)
        ;; Write session file
        (call-with-output-file *session-path*
          (lambda (port)
            ;; First line: current buffer path
            (display (or (buffer-file-path current-buf) "") port)
            (newline port)
            ;; Remaining lines: file\tposition
            (for-each
              (lambda (entry)
                (display (car entry) port)
                (display "\t" port)
                (display (number->string (cdr entry)) port)
                (newline port))
              entries)))))))

(def (session-restore-files)
  "Read session file and return list of (file-path . cursor-pos) plus current-file."
  (with-catch
    (lambda (e) (values #f []))
    (lambda ()
      (if (not (file-exists? *session-path*))
        (values #f [])
        (call-with-input-file *session-path*
          (lambda (port)
            (let ((current-file (read-line port)))
              (let loop ((acc []))
                (let ((line (read-line port)))
                  (if (eof-object? line)
                    (values (if (and (string? current-file)
                                    (> (string-length current-file) 0))
                              current-file #f)
                            (reverse acc))
                    (let ((parts (split-by-tab line)))
                      (if (and (= (length parts) 2)
                               (string->number (cadr parts)))
                        (loop (cons (cons (car parts) (string->number (cadr parts))) acc))
                        (loop acc)))))))))))))

(def (cmd-session-save app)
  "Save current session."
  (session-save! app)
  (echo-message! (app-state-echo app) "Session saved"))

(def (cmd-session-restore app)
  "Restore last session."
  (echo-message! (app-state-echo app) "Use --restore flag to restore session on startup"))

;;;============================================================================
;;; Duplicate region
;;;============================================================================

(def (cmd-duplicate-region app)
  "Duplicate the selected region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (cmd-duplicate-line app)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (new-text (string-append (substring text 0 end) region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ end (string-length region)))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region duplicated")))))

;;;============================================================================
;;; Reverse chars/word
;;;============================================================================

(def (cmd-reverse-chars app)
  "Reverse characters in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (reversed (list->string (reverse (string->list region))))
             (new-text (string-append (substring text 0 start) reversed
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Reversed")))))

(def (cmd-reverse-word app)
  "Reverse the word at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1))))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i)))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (reversed (list->string (reverse (string->list word))))
             (new-text (string-append (substring text 0 start) reversed
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed end)))))

;;;============================================================================
;;; Environment
;;;============================================================================

(def (cmd-getenv app)
  "Display an environment variable."
  (let ((var (qt-echo-read-string app "Environment variable: ")))
    (when var
      (let ((val (getenv var #f)))
        (echo-message! (app-state-echo app)
          (if val (string-append var "=" val)
            (string-append var " is not set")))))))

(def (cmd-setenv app)
  "Set an environment variable."
  (let ((var (qt-echo-read-string app "Variable name: ")))
    (when var
      (let ((val (qt-echo-read-string app (string-append var "="))))
        (when val
          (setenv var val)
          (echo-message! (app-state-echo app)
            (string-append "Set " var "=" val)))))))

;;;============================================================================
;;; Batch 7: More missing commands

