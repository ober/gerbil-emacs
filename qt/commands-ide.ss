;;; -*- Gerbil -*-
;;; Qt commands ide - insert, copy, buffer management, file ops, describe, VCS
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/format
        :std/text/base64
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
        :gemacs/qt/magit
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp)

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

;;; Scintilla folding message constants
(def SCI_TOGGLEFOLD  2231)
(def SCI_FOLDALL     2662)
(def SCI_FOLDLINE    2237)
(def SCI_GETFOLDLEVEL 2223)
(def SC_FOLDACTION_CONTRACT 0)
(def SC_FOLDACTION_EXPAND   1)
(def SC_FOLDLEVELHEADERFLAG #x2000)

(def (cmd-toggle-fold app)
  "Toggle code folding at current line."
  (let* ((ed (current-qt-editor app))
         (line (sci-send ed SCI_LINEFROMPOSITION
                         (sci-send ed SCI_GETCURRENTPOS))))
    (sci-send ed SCI_TOGGLEFOLD line 0)))

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
  "Show gemacs version."
  (echo-message! (app-state-echo app) "gemacs (Qt backend)"))

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
;;; Helpers in qt/magit.ss; commands here.
;;;============================================================================

(def *magit-dir* #f)

(def (cmd-magit-status app)
  "Open interactive git status buffer (magit-style) with inline diffs."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (set! *magit-dir* dir)
    (let* ((status-output (magit-run-git '("status" "--porcelain") dir))
           (branch-output (magit-run-git '("rev-parse" "--abbrev-ref" "HEAD") dir))
           (branch (string-trim branch-output))
           (entries (magit-parse-status status-output))
           (text (magit-format-status entries branch dir))
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
  "Stage file or hunk at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (section (magit-find-section text pos)))
        (when (eq? section 'unstaged)
          ;; Try hunk first, then fall back to file
          (let-values (((hunk-file patch) (magit-hunk-at-point text pos)))
            (cond
              (patch
               (let ((result (magit-run-git-stdin patch '("apply" "--cached") *magit-dir*)))
                 (cmd-magit-status app)
                 (if (string-prefix? "error" result)
                   (echo-error! (app-state-echo app) result)
                   (echo-message! (app-state-echo app)
                     (string-append "Staged hunk in: " (or hunk-file "?"))))))
              (else
               (let ((file (magit-file-at-point text pos)))
                 (if file
                   (begin
                     (magit-run-git (list "add" file) *magit-dir*)
                     (cmd-magit-status app)
                     (echo-message! (app-state-echo app)
                       (string-append "Staged: " file)))
                   (echo-error! (app-state-echo app) "No file or hunk at point")))))))
        ;; Allow staging untracked files
        (when (eq? section 'untracked)
          (let ((file (magit-file-at-point text pos)))
            (when file
              (magit-run-git (list "add" file) *magit-dir*)
              (cmd-magit-status app)
              (echo-message! (app-state-echo app)
                (string-append "Staged: " file)))))))))

(def (cmd-magit-unstage app)
  "Unstage file or hunk at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (section (magit-find-section text pos)))
        (when (eq? section 'staged)
          ;; Try hunk first, then fall back to file
          (let-values (((hunk-file patch) (magit-hunk-at-point text pos)))
            (cond
              (patch
               (let ((result (magit-run-git-stdin patch
                               '("apply" "--cached" "--reverse") *magit-dir*)))
                 (cmd-magit-status app)
                 (if (string-prefix? "error" result)
                   (echo-error! (app-state-echo app) result)
                   (echo-message! (app-state-echo app)
                     (string-append "Unstaged hunk in: " (or hunk-file "?"))))))
              (else
               (let ((file (magit-file-at-point text pos)))
                 (if file
                   (begin
                     (magit-run-git (list "reset" "HEAD" file) *magit-dir*)
                     (cmd-magit-status app)
                     (echo-message! (app-state-echo app)
                       (string-append "Unstaged: " file)))
                   (echo-error! (app-state-echo app)
                     "No file or hunk at point")))))))))))

(def (cmd-magit-commit app)
  "Commit staged changes."
  (let ((msg (qt-echo-read-string app "Commit message: ")))
    (when (and msg (> (string-length msg) 0))
      (let ((output (magit-run-git (list "commit" "-m" msg) *magit-dir*)))
        (cmd-magit-status app)
        (echo-message! (app-state-echo app)
          (string-append "Committed: " msg))))))

(def (cmd-magit-diff app)
  "Show diff for file at point or cursor context."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (file (or (let-values (((f _p) (magit-hunk-at-point text pos))) f)
                       (magit-file-at-point text pos))))
        (if file
          (let* ((diff-output (magit-run-git (list "diff" file) *magit-dir*))
                 (staged-diff (magit-run-git (list "diff" "--cached" file) *magit-dir*))
                 (full-diff (string-append
                              (if (> (string-length staged-diff) 0)
                                (string-append "Staged:\n" staged-diff "\n") "")
                              (if (> (string-length diff-output) 0)
                                (string-append "Unstaged:\n" diff-output) "")))
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
  "Show git log."
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

(def (cmd-magit-refresh app)
  "Refresh the magit status buffer."
  (cmd-magit-status app))

(def (cmd-magit-blame app)
  "Show git blame for current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let* ((dir (path-directory path))
             (output (magit-run-git (list "blame" "--" path) dir))
             (ed (current-qt-editor app))
             (fr (app-state-frame app))
             (blame-buf (or (buffer-by-name "*Git Blame*")
                            (qt-buffer-create! "*Git Blame*" ed #f))))
        (qt-buffer-attach! ed blame-buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) blame-buf)
        (qt-plain-text-edit-set-text! ed (if (string=? output "") "No blame info.\n" output))
        (qt-text-document-set-modified! (buffer-doc-pointer blame-buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

(def (cmd-magit-fetch app)
  "Fetch from all remotes."
  (when *magit-dir*
    (magit-run-git '("fetch" "--all") *magit-dir*)
    (echo-message! (app-state-echo app) "Fetched from all remotes")))

(def (cmd-magit-pull app)
  "Pull from remote."
  (when *magit-dir*
    (let ((output (magit-run-git '("pull") *magit-dir*)))
      (echo-message! (app-state-echo app)
        (if (string=? output "") "Pull complete" (string-trim output))))))

(def (cmd-magit-push app)
  "Push to remote."
  (when *magit-dir*
    (let ((output (magit-run-git '("push") *magit-dir*)))
      (echo-message! (app-state-echo app)
        (if (string=? output "") "Pushed" (string-trim output))))))

(def (cmd-magit-rebase app)
  "Rebase onto a branch."
  (let* ((dir (or *magit-dir* (current-directory)))
         (branches (magit-branch-names dir))
         (branch (if (null? branches)
                   (qt-echo-read-string app "Rebase onto (default origin/main): ")
                   (qt-echo-read-with-narrowing app "Rebase onto:" branches)))
         (target (if (or (not branch) (string=? branch "")) "origin/main" branch)))
    (let ((output (magit-run-git (list "rebase" target) dir)))
      (echo-message! (app-state-echo app)
        (if (string=? output "") "Rebase complete" (string-trim output))))))

(def (cmd-magit-merge app)
  "Merge a branch using narrowing."
  (let* ((dir (or *magit-dir* (current-directory)))
         (branches (magit-branch-names dir))
         (branch (if (null? branches)
                   (qt-echo-read-string app "Merge branch: ")
                   (qt-echo-read-with-narrowing app "Merge branch:" branches))))
    (when (and branch (> (string-length branch) 0))
      (let ((output (magit-run-git (list "merge" branch) dir)))
        (echo-message! (app-state-echo app)
          (if (string=? output "") "Merge complete" (string-trim output)))))))

(def (cmd-magit-stash app)
  "Stash changes or show stash list."
  (when *magit-dir*
    (let ((msg (qt-echo-read-string app "Stash message (empty=list stashes): ")))
      (if (or (not msg) (string=? msg ""))
        ;; Show stash list
        (let* ((output (magit-run-git '("stash" "list") *magit-dir*))
               (ed (current-qt-editor app))
               (fr (app-state-frame app))
               (stash-buf (or (buffer-by-name "*Magit Stash*")
                              (qt-buffer-create! "*Magit Stash*" ed #f))))
          (qt-buffer-attach! ed stash-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) stash-buf)
          (qt-plain-text-edit-set-text! ed
            (if (string=? output "") "No stashes.\n" output))
          (qt-text-document-set-modified! (buffer-doc-pointer stash-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))
        ;; Create stash
        (let ((output (magit-run-git (list "stash" "push" "-m" msg) *magit-dir*)))
          (cmd-magit-status app)
          (echo-message! (app-state-echo app)
            (if (string=? output "") "Stashed" (string-trim output))))))))

(def (cmd-magit-stash-pop app)
  "Pop the most recent stash."
  (when *magit-dir*
    (let ((output (magit-run-git '("stash" "pop") *magit-dir*)))
      (cmd-magit-status app)
      (echo-message! (app-state-echo app)
        (if (string=? output "") "Stash popped" (string-trim output))))))

(def (cmd-magit-branch app)
  "Show git branches."
  (let* ((dir (or *magit-dir* (current-directory)))
         (output (magit-run-git '("branch" "-a") dir))
         (ed (current-qt-editor app))
         (fr (app-state-frame app))
         (br-buf (or (buffer-by-name "*Git Branches*")
                     (qt-buffer-create! "*Git Branches*" ed #f))))
    (qt-buffer-attach! ed br-buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) br-buf)
    (qt-plain-text-edit-set-text! ed (if (string=? output "") "No branches\n" output))
    (qt-text-document-set-modified! (buffer-doc-pointer br-buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

(def (cmd-magit-checkout app)
  "Switch git branch using narrowing."
  (let* ((dir (or *magit-dir* (current-directory)))
         (branches (magit-branch-names dir))
         (branch (if (null? branches)
                   (qt-echo-read-string app "Branch: ")
                   (qt-echo-read-with-narrowing app "Checkout branch:" branches))))
    (when (and branch (> (string-length branch) 0))
      (let ((output (magit-run-git (list "checkout" branch) dir)))
        (echo-message! (app-state-echo app)
          (if (string=? output "")
            (string-append "Switched to: " branch)
            (string-trim output)))))))

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
  (path-expand ".gemacs-session" (user-info-home (user-info (user-name)))))

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
;;; Hl-todo — highlight TODO/FIXME/HACK keywords
;;;============================================================================

(def *hl-todo-mode* #f)
(def *hl-todo-keywords* '("TODO" "FIXME" "HACK" "BUG" "XXX" "NOTE"))

(def (cmd-hl-todo-mode app)
  "Toggle hl-todo mode — highlights TODO keywords."
  (set! *hl-todo-mode* (not *hl-todo-mode*))
  (echo-message! (app-state-echo app)
    (if *hl-todo-mode* "HL-todo: on" "HL-todo: off")))

(def (cmd-hl-todo-next app)
  "Jump to next TODO/FIXME/HACK keyword."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (next-pos #f))
    (for-each
      (lambda (kw)
        (let ((found (string-contains text kw (+ pos 1))))
          (when (and found (or (not next-pos) (< found next-pos)))
            (set! next-pos found))))
      *hl-todo-keywords*)
    (if next-pos
      (begin (qt-plain-text-edit-set-cursor-position! ed next-pos)
             (echo-message! (app-state-echo app) "Found TODO keyword"))
      (echo-message! (app-state-echo app) "No more TODO keywords"))))

(def (cmd-hl-todo-previous app)
  "Jump to previous TODO/FIXME/HACK keyword."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (prev-pos #f))
    (for-each
      (lambda (kw)
        (let loop ((search-from 0))
          (let ((found (string-contains text kw search-from)))
            (when (and found (< found pos))
              (when (or (not prev-pos) (> found prev-pos))
                (set! prev-pos found))
              (loop (+ found 1))))))
      *hl-todo-keywords*)
    (if prev-pos
      (begin (qt-plain-text-edit-set-cursor-position! ed prev-pos)
             (echo-message! (app-state-echo app) "Found TODO keyword"))
      (echo-message! (app-state-echo app) "No previous TODO keywords"))))

;;;============================================================================
;;; Shell command framework (plan items 0.3 + 3.2)
;;;============================================================================

(def (shell-command-to-string cmd)
  "Run CMD via /bin/sh and return stdout as a string. Returns empty string on error."
  (with-catch
    (lambda (e) "")
    (lambda ()
      (let* ((proc (open-process
                      (list path: "/bin/sh"
                            arguments: ["-c" cmd]
                            stdout-redirection: #t
                            stderr-redirection: #t
                            pseudo-terminal: #f)))
             (output (read-line proc #f)))
        (process-status proc)
        (close-port proc)
        (or output "")))))

(def (shell-command-to-buffer! app cmd buffer-name . opts)
  "Run CMD, display output in BUFFER-NAME. Options: read-only: #t (default #t)."
  (let* ((read-only? (if (and (pair? opts) (pair? (car opts)))
                       (let ((ro (assoc read-only: opts)))
                         (if ro (cdr ro) #t))
                       #t))
         (result (with-catch
                   (lambda (e)
                     (string-append "Error: "
                       (with-output-to-string (lambda () (display-exception e)))))
                   (lambda ()
                     (shell-command-to-string cmd))))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (or (buffer-by-name buffer-name)
                  (qt-buffer-create! buffer-name ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed result)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (when read-only?
      (qt-plain-text-edit-set-read-only! ed #t))
    (qt-modeline-update! app)
    result))

(def *user-shell-commands* (make-hash-table))

(def (register-shell-command! name prompt command-template buffer-template)
  "Register a user-definable shell command.
   PROMPT is shown when reading input.
   COMMAND-TEMPLATE is a format string with ~a for the input.
   BUFFER-TEMPLATE is a format string for the buffer name."
  (hash-put! *user-shell-commands* name
    (list prompt command-template buffer-template)))

(def (cmd-run-user-shell-command app)
  "Run a registered user shell command by name."
  (let* ((names (sort (hash-keys *user-shell-commands*) string<?))
         (name (if (null? names)
                 (begin (echo-message! (app-state-echo app) "No shell commands registered")
                        #f)
                 (qt-echo-read-string app
                   (string-append "Shell command ("
                     (string-join (map symbol->string names) ", ") "): ")))))
    (when (and name (> (string-length name) 0))
      (let ((entry (hash-get *user-shell-commands* (string->symbol name))))
        (if entry
          (let* ((prompt (car entry))
                 (cmd-template (cadr entry))
                 (buf-template (caddr entry))
                 (input (qt-echo-read-string app prompt)))
            (when (and input (> (string-length input) 0))
              (let ((cmd (format cmd-template input))
                    (buf-name (format buf-template input)))
                (shell-command-to-buffer! app cmd buf-name)
                (echo-message! (app-state-echo app)
                  (string-append "Ran: " cmd)))))
          (echo-message! (app-state-echo app)
            (string-append "Unknown command: " name)))))))

;;;============================================================================
;;; Workspaces / Perspectives (plan item 2.3)
;;;============================================================================

;; Each workspace: (name . buffer-names)
;; buffer-names is a list of buffer name strings visible in this workspace
(def *workspaces* (make-hash-table))  ; name -> list of buffer-name strings
(def *current-workspace* "default")
(def *workspace-buffers* (make-hash-table))  ; workspace -> current-buffer-name

(def (workspace-init! app)
  "Initialize the default workspace with all current buffers."
  (hash-put! *workspaces* "default"
    (map buffer-name (buffer-list)))
  (let ((buf (qt-current-buffer (app-state-frame app))))
    (when buf
      (hash-put! *workspace-buffers* "default" (buffer-name buf)))))

(def (workspace-add-buffer! ws-name buf-name)
  "Add a buffer to a workspace's buffer list."
  (let ((bufs (or (hash-get *workspaces* ws-name) [])))
    (unless (member buf-name bufs)
      (hash-put! *workspaces* ws-name (cons buf-name bufs)))))

(def (workspace-remove-buffer! ws-name buf-name)
  "Remove a buffer from a workspace."
  (let ((bufs (or (hash-get *workspaces* ws-name) [])))
    (hash-put! *workspaces* ws-name
      (filter (lambda (b) (not (string=? b buf-name))) bufs))))

(def (cmd-workspace-create app)
  "Create a new named workspace."
  (let ((name (qt-echo-read-string app "New workspace name: ")))
    (when (and name (> (string-length name) 0))
      (if (hash-get *workspaces* name)
        (echo-message! (app-state-echo app)
          (string-append "Workspace '" name "' already exists"))
        (begin
          (hash-put! *workspaces* name ["*scratch*"])
          (echo-message! (app-state-echo app)
            (string-append "Created workspace: " name)))))))

(def (cmd-workspace-switch app)
  "Switch to a named workspace."
  (let* ((names (sort (hash-keys *workspaces*) string<?))
         (prompt (string-append "Switch workspace ("
                   (string-join names ", ") "): "))
         (name (qt-echo-read-string app prompt)))
    (when (and name (> (string-length name) 0))
      (let ((bufs (hash-get *workspaces* name)))
        (if bufs
          (begin
            ;; Save current workspace's active buffer
            (let ((cur-buf (qt-current-buffer (app-state-frame app))))
              (when cur-buf
                (hash-put! *workspace-buffers* *current-workspace*
                  (buffer-name cur-buf))))
            ;; Switch to new workspace
            (set! *current-workspace* name)
            ;; Restore the workspace's active buffer
            (let* ((active (hash-get *workspace-buffers* name))
                   (target (and active (buffer-by-name active))))
              (when target
                (let* ((fr (app-state-frame app))
                       (ed (current-qt-editor app)))
                  (qt-buffer-attach! ed target)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) target))))
            (echo-message! (app-state-echo app)
              (string-append "Workspace: " name
                " (" (number->string (length bufs)) " buffers)")))
          (echo-message! (app-state-echo app)
            (string-append "No workspace: " name)))))))

(def (cmd-workspace-delete app)
  "Delete a workspace (cannot delete default)."
  (let* ((names (sort (filter (lambda (n) (not (string=? n "default")))
                        (hash-keys *workspaces*)) string<?))
         (name (if (null? names)
                 (begin (echo-message! (app-state-echo app) "No deletable workspaces")
                        #f)
                 (qt-echo-read-string app
                   (string-append "Delete workspace (" (string-join names ", ") "): ")))))
    (when (and name (> (string-length name) 0))
      (cond
        ((string=? name "default")
         (echo-message! (app-state-echo app) "Cannot delete default workspace"))
        ((hash-get *workspaces* name)
         (hash-remove! *workspaces* name)
         (hash-remove! *workspace-buffers* name)
         (when (string=? *current-workspace* name)
           (set! *current-workspace* "default"))
         (echo-message! (app-state-echo app)
           (string-append "Deleted workspace: " name)))
        (else
         (echo-message! (app-state-echo app)
           (string-append "No workspace: " name)))))))

(def (cmd-workspace-add-buffer app)
  "Add current buffer to a workspace."
  (let* ((buf (qt-current-buffer (app-state-frame app)))
         (buf-name (buffer-name buf)))
    (workspace-add-buffer! *current-workspace* buf-name)
    (echo-message! (app-state-echo app)
      (string-append "Added '" buf-name "' to workspace '" *current-workspace* "'"))))

(def (cmd-workspace-list app)
  "List all workspaces and their buffers."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (lines
           (let loop ((names (sort (hash-keys *workspaces*) string<?)) (acc []))
             (if (null? names)
               (reverse acc)
               (let* ((name (car names))
                      (bufs (or (hash-get *workspaces* name) []))
                      (active? (string=? name *current-workspace*))
                      (header (string-append
                                (if active? "* " "  ")
                                name " (" (number->string (length bufs)) " buffers)"))
                      (buf-lines (map (lambda (b) (string-append "    " b)) bufs)))
                 (loop (cdr names)
                   (append (reverse (cons header buf-lines)) acc))))))
         (content (string-join lines "\n"))
         (buf (or (buffer-by-name "*Workspaces*")
                  (qt-buffer-create! "*Workspaces*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed content)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (qt-modeline-update! app)
    (echo-message! (app-state-echo app)
      (string-append "Current: " *current-workspace*))))

;;;============================================================================
;;; Multiple Cursors (Scintilla multi-selection API)
;;;============================================================================

;; Enable multiple selection mode (needed once)
(def (qt-enable-multiple-selection! ed)
  "Enable Scintilla multiple-selection and additional-selection-typing."
  ;; SCI_SETMULTIPLESELECTION=2563, SCI_SETADDITIONALSELECTIONTYPING=2565
  (sci-send ed 2563 1 0)
  (sci-send ed 2565 1 0))

(def (cmd-mc-mark-next app)
  "Add a cursor at the next occurrence of the current selection."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (qt-enable-multiple-selection! ed)
    (let* ((sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
           (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
      (if (= sel-start sel-end)
        (echo-error! echo "Select text first, then mark next")
        (begin
          (sci-send ed SCI_MULTIPLESELECTADDNEXT 0 0)
          (let ((n (sci-send ed SCI_GETSELECTIONS 0 0)))
            (echo-message! echo
              (string-append (number->string n) " cursors"))))))))

(def (cmd-mc-mark-all app)
  "Add cursors at all occurrences of the current selection."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (qt-enable-multiple-selection! ed)
    (let* ((sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
           (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
      (if (= sel-start sel-end)
        (echo-error! echo "Select text first, then mark all")
        (begin
          (sci-send ed SCI_MULTIPLESELECTADDEACH 0 0)
          (let ((n (sci-send ed SCI_GETSELECTIONS 0 0)))
            (echo-message! echo
              (string-append (number->string n) " cursors"))))))))

(def (cmd-mc-skip-and-mark-next app)
  "Skip the current selection and add next occurrence."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
         (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
    (if (= sel-start sel-end)
      (echo-error! echo "Select text first")
      (begin
        (qt-enable-multiple-selection! ed)
        (let ((n (sci-send ed SCI_GETSELECTIONS 0 0)))
          (when (> n 1)
            (let ((main (sci-send ed SCI_GETMAINSELECTION 0 0)))
              (sci-send ed SCI_DROPSELECTIONN main 0))))
        (sci-send ed SCI_MULTIPLESELECTADDNEXT 0 0)
        (let ((n2 (sci-send ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n2) " cursors")))))))

(def (cmd-mc-edit-lines app)
  "Add a cursor at the end of each line in the current selection."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
         (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
    (if (= sel-start sel-end)
      (echo-error! echo "Select a region first")
      (begin
        (qt-enable-multiple-selection! ed)
        (let* ((start-line (sci-send ed SCI_LINEFROMPOSITION sel-start 0))
               (end-line (sci-send ed SCI_LINEFROMPOSITION sel-end 0))
               (num-lines (+ 1 (- end-line start-line))))
          (when (> num-lines 1)
            ;; Set first selection at end of first line
            (let ((eol0 (sci-send ed SCI_GETLINEENDPOSITION start-line 0)))
              (sci-send ed SCI_SETSELECTION eol0 eol0)
              (let loop ((line (+ start-line 1)))
                (when (<= line end-line)
                  (let ((eol (sci-send ed SCI_GETLINEENDPOSITION line 0)))
                    (sci-send ed SCI_ADDSELECTION eol eol)
                    (loop (+ line 1)))))))
          (echo-message! echo
            (string-append (number->string num-lines)
                           " cursors on " (number->string num-lines) " lines")))))))

(def (cmd-mc-unmark-last app)
  "Remove the most recently added cursor."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (n (sci-send ed SCI_GETSELECTIONS 0 0)))
    (if (<= n 1)
      (echo-message! echo "Only one cursor")
      (begin
        (sci-send ed SCI_DROPSELECTIONN (- n 1) 0)
        (echo-message! echo
          (string-append (number->string (- n 1)) " cursors"))))))

(def (cmd-mc-rotate app)
  "Cycle to the next selection as the main cursor."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_ROTATESELECTION 0 0)))

;;;============================================================================
;;; fill-region, insert-buffer, prepend-to-buffer, copy-rectangle-to-register
;;;============================================================================

(def (qt-fill-words words col)
  "Reflow WORDS list to COL width, returning string."
  (if (null? words) ""
    (let loop ((ws (cdr words)) (line (car words)) (lines []))
      (if (null? ws)
        (string-join (reverse (cons line lines)) "\n")
        (let ((next (string-append line " " (car ws))))
          (if (> (string-length next) col)
            (if (string=? line "")
              (loop (cdr ws) "" (cons (car ws) lines))
              (loop ws "" (cons line lines)))
            (loop (cdr ws) next lines)))))))

(def (cmd-fill-region app)
  "Fill (word-wrap) the selected region at fill-column."
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
             (words (filter (lambda (w) (> (string-length w) 0))
                            (string-split (string-trim region) #\space)))
             (filled (qt-fill-words words *fill-column*)))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed filled)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region filled")))))

(def (cmd-insert-buffer app)
  "Insert the contents of another buffer at point."
  (let* ((bufs (buffer-list))
         (names (map buffer-name bufs))
         (target-name (qt-echo-read-string-with-completion app "Insert buffer: " names)))
    (when (and target-name (> (string-length target-name) 0))
      (let ((target-buf (find (lambda (b) (string=? (buffer-name b) target-name)) bufs)))
        (if (not target-buf)
          (echo-error! (app-state-echo app)
            (string-append "No buffer: " target-name))
          ;; Find editor showing target buffer to get its text
          (let* ((fr (app-state-frame app))
                 (target-text
                   (let loop ((wins (qt-frame-windows fr)))
                     (cond
                       ((null? wins) #f)
                       ((eq? (qt-edit-window-buffer (car wins)) target-buf)
                        (qt-plain-text-edit-text (qt-edit-window-editor (car wins))))
                       (else (loop (cdr wins)))))))
            (if target-text
              (let ((ed (current-qt-editor app)))
                (qt-plain-text-edit-insert-text! ed target-text)
                (echo-message! (app-state-echo app)
                  (string-append "Inserted buffer " target-name)))
              (echo-error! (app-state-echo app)
                (string-append "Buffer " target-name " not visible in any window")))))))))

(def (cmd-prepend-to-buffer app)
  "Prepend region to another buffer."
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
             (target-name (qt-echo-read-string-with-completion app "Prepend to buffer: " names)))
        (when target-name
          (let ((target-buf (find (lambda (b) (string=? (buffer-name b) target-name)) bufs)))
            (if target-buf
              (let ((fr (app-state-frame app)))
                (let loop ((wins (qt-frame-windows fr)))
                  (when (pair? wins)
                    (if (eq? (qt-edit-window-buffer (car wins)) target-buf)
                      (let ((target-ed (qt-edit-window-editor (car wins))))
                        ;; Move to beginning and insert
                        (qt-plain-text-edit-set-cursor-position! target-ed 0)
                        (qt-plain-text-edit-insert-text! target-ed region))
                      (loop (cdr wins)))))
                (echo-message! (app-state-echo app)
                  (string-append "Prepended to " target-name)))
              (echo-error! (app-state-echo app) "Buffer not found"))))))))

(def (cmd-copy-rectangle-to-register app)
  "Copy rectangle (region interpreted as column block) to a register."
  (let* ((input (qt-echo-read-string app "Copy rectangle to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (mark (buffer-mark buf)))
        (if (not mark)
          (echo-error! echo "No mark set")
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (lines (string-split text #\newline))
                 (col1 (column-at-position text start))
                 (col2 (column-at-position text end))
                 (left-col (min col1 col2))
                 (right-col (max col1 col2)))
            (let-values (((start-line end-line) (region-line-range text start end)))
              (let ((rect-lines
                      (let loop ((i start-line) (acc []))
                        (if (> i end-line) (reverse acc)
                          (let* ((l (if (< i (length lines)) (list-ref lines i) ""))
                                 (llen (string-length l))
                                 (s (min left-col llen))
                                 (e (min right-col llen)))
                            (loop (+ i 1) (cons (substring l s e) acc)))))))
                (hash-put! (app-state-registers app) reg
                  (string-join rect-lines "\n"))
                (set! (buffer-mark buf) #f)
                (echo-message! echo
                  (string-append "Rectangle copied to register " (string reg)))))))))))


