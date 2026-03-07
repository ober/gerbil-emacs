;;; -*- Gerbil -*-
;;; Qt commands ide - insert, copy, buffer management, file ops, describe, VCS
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/format
        :std/text/base64
        :std/misc/completion
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
        :gemacs/qt/magit
        :gemacs/qt/commands-core
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        (only-in :gemacs/editor-extra-helpers project-current))

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
  "Run a git command async and show output in a buffer."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (magit-run-git/async args dir
      (lambda (output)
        (let* ((ed (current-qt-editor app))
               (fr (app-state-frame app))
               (git-buf (qt-buffer-create! buffer-name ed #f)))
          (qt-buffer-attach! ed git-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) git-buf)
          (qt-plain-text-edit-set-text! ed (or output ""))
          (qt-text-document-set-modified! (buffer-doc-pointer git-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

(def (cmd-show-git-status app)
  "Show git status."
  (run-git-command app '("status") "*Git Status*"))

(def (cmd-show-git-log app)
  "Show git log."
  (run-git-command app '("log" "--oneline" "-30") "*Git Log*"))

(def (cmd-show-git-diff app)
  "Show git diff with syntax coloring."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (magit-run-git/async '("diff") dir
      (lambda (output)
        (let* ((ed (current-qt-editor app))
               (fr (app-state-frame app))
               (git-buf (qt-buffer-create! "*Git Diff*" ed #f)))
          (qt-buffer-attach! ed git-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) git-buf)
          (qt-plain-text-edit-set-text! ed (or output ""))
          (qt-text-document-set-modified! (buffer-doc-pointer git-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (qt-highlight-diff! ed))))))

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

(def (magit-render-status! app status-output branch-output dir)
  "Render magit status buffer from git output (called on UI thread)."
  (let* ((branch (string-trim branch-output))
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
    (echo-message! (app-state-echo app) "*Magit*")))

(def (cmd-magit-status app)
  "Open interactive git status buffer (magit-style) with inline diffs."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf))
         (dir (if path (path-directory path) (current-directory))))
    (set! *magit-dir* dir)
    (echo-message! (app-state-echo app) "Loading git status...")
    ;; Fire two git commands in parallel
    (let ((status-done (make-completion 'status))
          (branch-done (make-completion 'branch)))
      (magit-run-git/async '("status" "--porcelain") dir
        (lambda (output) (completion-post! status-done output)))
      (magit-run-git/async '("rev-parse" "--abbrev-ref" "HEAD") dir
        (lambda (output) (completion-post! branch-done output)))
      ;; Join in background thread, then deliver to UI
      (spawn/name 'magit-status-join
        (lambda ()
          (let ((status-output (completion-wait! status-done))
                (branch-output (completion-wait! branch-done)))
            (ui-queue-push!
              (lambda ()
                (magit-render-status! app status-output branch-output dir)))))))))

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

(def *magit-commit-separator*
  "# --- Do not modify below this line ---")

(def (cmd-magit-commit app)
  "Open commit message buffer with staged diff preview."
  (let ((dir (or *magit-dir* (current-directory))))
    ;; Check for staged changes first
    (magit-run-git/async '("diff" "--cached" "--stat") dir
      (lambda (stat-output)
        (if (string=? (string-trim stat-output) "")
          (echo-error! (app-state-echo app) "Nothing staged to commit")
          ;; Get full staged diff for preview
          (magit-run-git/async '("diff" "--cached") dir
            (lambda (diff-output)
              (ui-queue-push!
                (lambda ()
                  (magit-open-commit-buffer! app stat-output diff-output dir))))))))))

(def (magit-open-commit-buffer! app stat-output diff-output dir)
  "Create the *Magit: Commit* buffer with message area and diff preview."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Magit: Commit*")
                  (qt-buffer-create! "*Magit: Commit*" ed #f)))
         (stat-commented
           (with-output-to-string
             (lambda ()
               (let loop ((i 0))
                 (when (< i (string-length stat-output))
                   (let ((nl (let scan ((j i))
                               (cond ((>= j (string-length stat-output)) j)
                                     ((char=? (string-ref stat-output j) #\newline) j)
                                     (else (scan (+ j 1)))))))
                     (when (> nl i)
                       (display "#   ")
                       (display (substring stat-output i nl))
                       (newline))
                     (loop (+ nl 1))))))))
         (header (string-append
                   "# Write your commit message above the separator line.\n"
                   "# Lines starting with '#' will be ignored.\n"
                   "# C-c C-c to commit, C-c C-k to abort.\n"
                   "#\n"
                   "# Changes to be committed:\n"
                   stat-commented
                   "#\n"))
         (text (string-append "\n" header *magit-commit-separator* "\n\n"
                              diff-output)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    ;; Position cursor at line 1 (before the header comments)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    ;; Store the dir for the commit
    (set! (buffer-file-path buf) dir)
    (echo-message! (app-state-echo app)
      "C-c C-c to commit, C-c C-k to abort")))

(def (cmd-magit-commit-finalize app)
  "Finalize the commit: extract message and run git commit."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit: Commit*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (dir (or (buffer-file-path buf) *magit-dir* (current-directory)))
             ;; Extract message: everything before separator, ignoring # lines
             (sep-pos (string-contains text *magit-commit-separator*))
             (msg-text (if sep-pos (substring text 0 sep-pos) text))
             (msg (string-trim
                    (with-output-to-string
                      (lambda ()
                        (let loop ((i 0))
                          (when (< i (string-length msg-text))
                            (let ((nl (let scan ((j i))
                                        (cond ((>= j (string-length msg-text)) j)
                                              ((char=? (string-ref msg-text j) #\newline) j)
                                              (else (scan (+ j 1)))))))
                              (let ((line (substring msg-text i nl)))
                                (unless (string-prefix? "#" (string-trim line))
                                  (display line)
                                  (newline)))
                              (loop (+ nl 1))))))))))
        (if (string=? msg "")
          (echo-error! (app-state-echo app) "Aborting commit due to empty message")
          (begin
            (let ((git-args (if *magit-amend-mode*
                               (list "commit" "--amend" "-m" msg)
                               (list "commit" "-m" msg))))
            (set! *magit-amend-mode* #f)
            (echo-message! (app-state-echo app) "Committing...")
            (magit-run-git/async git-args dir
              (lambda (output)
                (ui-queue-push!
                  (lambda ()
                    ;; Kill the commit buffer and refresh status
                    (let ((commit-buf (buffer-by-name "*Magit: Commit*")))
                      (when commit-buf
                        (qt-buffer-kill! commit-buf)))
                    (cmd-magit-status app)
                    (echo-message! (app-state-echo app)
                      (string-append "Committed: "
                        (if (> (string-length msg) 60)
                          (string-append (substring msg 0 57) "...")
                          msg))))))))))))))

(def (cmd-magit-commit-abort app)
  "Abort the commit and kill the commit buffer."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit: Commit*")
      (set! *magit-amend-mode* #f)
      (qt-buffer-kill! buf)
      ;; Go back to magit status if it exists
      (let ((magit-buf (buffer-by-name "*Magit*")))
        (when magit-buf
          (let ((ed (current-qt-editor app))
                (fr (app-state-frame app)))
            (qt-buffer-attach! ed magit-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) magit-buf))))
      (echo-message! (app-state-echo app) "Commit aborted"))))

(def *magit-amend-mode* #f)

(def (cmd-magit-amend app)
  "Amend the last commit: open commit buffer pre-filled with previous message."
  (let ((dir (or *magit-dir* (current-directory))))
    ;; Get the last commit message
    (magit-run-git/async '("log" "-1" "--format=%B") dir
      (lambda (prev-msg)
        ;; Get staged diff (or HEAD diff if nothing staged)
        (magit-run-git/async '("diff" "--cached" "--stat") dir
          (lambda (stat-output)
            (let ((use-head? (string=? (string-trim stat-output) "")))
              (magit-run-git/async
                (if use-head? '("diff" "HEAD~1") '("diff" "--cached"))
                dir
                (lambda (diff-output)
                  (ui-queue-push!
                    (lambda ()
                      (set! *magit-amend-mode* #t)
                      (magit-open-commit-buffer! app
                        (if use-head?
                          (or (magit-run-git '("diff" "HEAD~1" "--stat") dir) "")
                          stat-output)
                        diff-output dir)
                      ;; Pre-fill with previous message
                      (let* ((ed (current-qt-editor app))
                             (text (qt-plain-text-edit-text ed))
                             (prev (string-trim prev-msg)))
                        (qt-plain-text-edit-set-text! ed
                          (string-append prev "\n" (substring text 1 (string-length text))))
                        (qt-plain-text-edit-set-cursor-position! ed 0))
                      (echo-message! (app-state-echo app)
                        "Amending. C-c C-c to commit, C-c C-k to abort"))))))))))))

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
          (begin
            (echo-message! (app-state-echo app) "Loading diff...")
            ;; Run both diff commands in parallel
            (let ((unstaged-done (make-completion 'diff))
                  (staged-done (make-completion 'diff-cached)))
              (magit-run-git/async (list "diff" file) *magit-dir*
                (lambda (out) (completion-post! unstaged-done out)))
              (magit-run-git/async (list "diff" "--cached" file) *magit-dir*
                (lambda (out) (completion-post! staged-done out)))
              (spawn/name 'magit-diff-join
                (lambda ()
                  (let ((diff-output (completion-wait! unstaged-done))
                        (staged-diff (completion-wait! staged-done)))
                    (ui-queue-push!
                      (lambda ()
                        (let* ((full-diff (string-append
                                            (if (> (string-length staged-diff) 0)
                                              (string-append "Staged:\n" staged-diff "\n") "")
                                            (if (> (string-length diff-output) 0)
                                              (string-append "Unstaged:\n" diff-output) "")))
                               (ed (current-qt-editor app))
                               (fr (app-state-frame app))
                               (diff-buf (or (buffer-by-name "*Magit Diff*")
                                             (qt-buffer-create! "*Magit Diff*" ed #f))))
                          (qt-buffer-attach! ed diff-buf)
                          (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                          (qt-plain-text-edit-set-text! ed
                            (if (string=? full-diff "") "No differences.\n" full-diff))
                          (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                          (qt-plain-text-edit-set-cursor-position! ed 0)
                          (qt-highlight-diff! ed)))))))))
          (echo-error! (app-state-echo app) "No file at point"))))))

(def (cmd-magit-stage-all app)
  "Stage all changes."
  (when *magit-dir*
    (magit-run-git '("add" "-A") *magit-dir*)
    (cmd-magit-status app)
    (echo-message! (app-state-echo app) "All changes staged")))

(def (cmd-magit-log app)
  "Show interactive git log with commit details on Enter."
  (when *magit-dir*
    (echo-message! (app-state-echo app) "Loading git log...")
    (magit-run-git/async
      '("log" "--format=%h %ad %an  %s" "--date=short" "--graph" "-50")
      *magit-dir*
      (lambda (output)
        (let* ((ed (current-qt-editor app))
               (fr (app-state-frame app))
               (log-buf (or (buffer-by-name "*Magit Log*")
                            (qt-buffer-create! "*Magit Log*" ed #f))))
          (qt-buffer-attach! ed log-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) log-buf)
          (qt-plain-text-edit-set-text! ed (or output ""))
          (qt-text-document-set-modified! (buffer-doc-pointer log-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (echo-message! (app-state-echo app)
            "Enter=show commit, n/p=navigate, q=quit"))))))

(def (magit-log-commit-at-point app)
  "Extract the git commit hash from the current line in *Magit Log*."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    ;; Find the current line
    (let* ((line-start (let loop ((i (min pos (- (string-length text) 1))))
                         (cond ((< i 0) 0)
                               ((char=? (string-ref text i) #\newline) (+ i 1))
                               (else (loop (- i 1))))))
           (line-end (let loop ((i pos))
                       (cond ((>= i (string-length text)) i)
                             ((char=? (string-ref text i) #\newline) i)
                             (else (loop (+ i 1))))))
           (line (substring text line-start line-end)))
      ;; Extract hash: skip graph chars (*/|\ space) then grab hex word
      (let loop ((i 0))
        (cond
          ((>= i (string-length line)) #f)
          ((let ((c (string-ref line i)))
             (or (char=? c #\*) (char=? c #\|) (char=? c #\\)
                 (char=? c #\/) (char=? c #\space))) (loop (+ i 1)))
          (else
            ;; Should be at the hash now
            (let ((end (let scan ((j i))
                         (cond ((>= j (string-length line)) j)
                               ((char-alphabetic? (string-ref line j)) (scan (+ j 1)))
                               ((char-numeric? (string-ref line j)) (scan (+ j 1)))
                               (else j)))))
              (if (>= (- end i) 7)
                (substring line i end)
                #f))))))))

(def (cmd-magit-log-show-commit app)
  "Show the diff for the commit at point in the log buffer."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit Log*")
      (let ((hash (magit-log-commit-at-point app)))
        (if (not hash)
          (echo-error! (app-state-echo app) "No commit hash at point")
          (begin
            (echo-message! (app-state-echo app)
              (string-append "Loading commit " hash "..."))
            (magit-run-git/async (list "show" "--stat" "--patch" hash) *magit-dir*
              (lambda (output)
                (ui-queue-push!
                  (lambda ()
                    (let* ((ed (current-qt-editor app))
                           (fr (app-state-frame app))
                           (diff-buf (or (buffer-by-name "*Magit Commit*")
                                         (qt-buffer-create! "*Magit Commit*" ed #f))))
                      (qt-buffer-attach! ed diff-buf)
                      (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                      (qt-plain-text-edit-set-text! ed (or output ""))
                      (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                      (qt-plain-text-edit-set-cursor-position! ed 0)
                      (qt-highlight-diff! ed)
                      (echo-message! (app-state-echo app)
                        (string-append "Commit " hash)))))))))))))


(def (cmd-magit-refresh app)
  "Refresh the magit status buffer."
  (cmd-magit-status app))

(def (cmd-magit-blame app)
  "Show git blame for current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((dir (path-directory path)))
        (echo-message! (app-state-echo app) "Loading git blame...")
        (magit-run-git/async (list "blame" "--" path) dir
          (lambda (output)
            (let* ((ed (current-qt-editor app))
                   (fr (app-state-frame app))
                   (blame-buf (or (buffer-by-name "*Git Blame*")
                                  (qt-buffer-create! "*Git Blame*" ed #f))))
              (qt-buffer-attach! ed blame-buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) blame-buf)
              (qt-plain-text-edit-set-text! ed (if (string=? output "") "No blame info.\n" output))
              (qt-text-document-set-modified! (buffer-doc-pointer blame-buf) #f)
              (qt-plain-text-edit-set-cursor-position! ed 0))))))))

(def (cmd-magit-fetch app)
  "Fetch from remotes with remote selection."
  (let* ((dir (or *magit-dir* (current-directory)))
         (remotes (magit-remote-names dir))
         (remote (cond
                   ((null? remotes) "origin")
                   ((= (length remotes) 1) (car remotes))
                   (else
                    (let ((r (qt-echo-read-with-narrowing app "Fetch remote:"
                               (cons "--all--" remotes))))
                      (if (or (not r) (string=? r "")) "origin" r)))))
         (args (if (string=? remote "--all--")
                 '("fetch" "--all")
                 (list "fetch" remote))))
    (echo-message! (app-state-echo app)
      (string-append "Fetching " (if (string=? remote "--all--") "all remotes" remote) "..."))
    (magit-run-git/async args dir
      (lambda (output)
        (echo-message! (app-state-echo app)
          (string-append "Fetched "
            (if (string=? remote "--all--") "all remotes" remote)))
        (when (buffer-by-name "*Magit*")
          (cmd-magit-status app))))))

(def (cmd-magit-pull app)
  "Pull from remote with rebase option."
  (let* ((dir (or *magit-dir* (current-directory)))
         (branch (magit-current-branch dir))
         (upstream (magit-upstream-branch dir)))
    (if (not upstream)
      (echo-error! (app-state-echo app)
        (string-append "No upstream for " branch ". Push first with P to set upstream."))
      (begin
        (echo-message! (app-state-echo app)
          (string-append "Pulling " upstream " into " branch "..."))
        (magit-run-git/async '("pull") dir
          (lambda (output)
            (let ((msg (string-trim output)))
              (echo-message! (app-state-echo app)
                (if (string=? msg "") "Pull complete (already up to date)"
                    (let* ((len (string-length msg))
                           (first-line-end (let loop ((i 0))
                                             (cond ((>= i len) len)
                                                   ((char=? (string-ref msg i) #\newline) i)
                                                   (else (loop (+ i 1)))))))
                      (substring msg 0 (min first-line-end 80))))))
            (when (buffer-by-name "*Magit*")
              (cmd-magit-status app))))))))

(def (cmd-magit-push app)
  "Push to remote with upstream setup and force-with-lease option."
  (let* ((dir (or *magit-dir* (current-directory)))
         (branch (magit-current-branch dir))
         (upstream (magit-upstream-branch dir))
         (remotes (magit-remote-names dir))
         (default-remote (if (null? remotes) "origin" (car remotes))))
    (if (not upstream)
      ;; No upstream — offer to set one
      (let* ((remote (if (<= (length remotes) 1) default-remote
                       (let ((r (qt-echo-read-with-narrowing app "Push to remote:" remotes)))
                         (if (or (not r) (string=? r "")) default-remote r)))))
        (echo-message! (app-state-echo app)
          (string-append "Pushing " branch " to " remote " (setting upstream)..."))
        (magit-run-git/async (list "push" "-u" remote branch) dir
          (lambda (output)
            (echo-message! (app-state-echo app)
              (string-append "Pushed " branch " → " remote "/" branch))
            (when (buffer-by-name "*Magit*")
              (cmd-magit-status app)))))
      ;; Has upstream — normal push
      (begin
        (echo-message! (app-state-echo app)
          (string-append "Pushing " branch " → " upstream "..."))
        (magit-run-git/async '("push") dir
          (lambda (output)
            (echo-message! (app-state-echo app)
              (if (string=? output "")
                (string-append "Pushed " branch " → " upstream)
                (string-trim output)))
            (when (buffer-by-name "*Magit*")
              (cmd-magit-status app))))))))

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

(def (cmd-magit-stash-show app)
  "Show the diff of the stash at point."
  (let ((buf (current-qt-buffer app)))
    (when (string=? (buffer-name buf) "*Magit Stash*")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             ;; Find current line and extract stash ref (stash@{N})
             (line-start (let loop ((i (min pos (- (string-length text) 1))))
                           (cond ((< i 0) 0)
                                 ((char=? (string-ref text i) #\newline) (+ i 1))
                                 (else (loop (- i 1))))))
             (line-end (let loop ((i pos))
                         (cond ((>= i (string-length text)) i)
                               ((char=? (string-ref text i) #\newline) i)
                               (else (loop (+ i 1))))))
             (line (substring text line-start line-end))
             ;; Extract stash@{N} from line like "stash@{0}: WIP on master: abc123 msg"
             (stash-ref (let ((colon-pos (string-contains line ":")))
                          (and colon-pos (> colon-pos 0)
                               (substring line 0 colon-pos)))))
        (if (not stash-ref)
          (echo-error! (app-state-echo app) "No stash at point")
          (begin
            (echo-message! (app-state-echo app)
              (string-append "Loading " stash-ref "..."))
            (magit-run-git/async (list "stash" "show" "-p" stash-ref) *magit-dir*
              (lambda (output)
                (ui-queue-push!
                  (lambda ()
                    (let* ((ed (current-qt-editor app))
                           (fr (app-state-frame app))
                           (diff-buf (or (buffer-by-name "*Magit Stash Diff*")
                                         (qt-buffer-create! "*Magit Stash Diff*" ed #f))))
                      (qt-buffer-attach! ed diff-buf)
                      (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                      (qt-plain-text-edit-set-text! ed (or output ""))
                      (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                      (qt-plain-text-edit-set-cursor-position! ed 0)
                      (qt-highlight-diff! ed)
                      (echo-message! (app-state-echo app) stash-ref))))))))))))

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

(def (cmd-magit-cherry-pick app)
  "Cherry-pick a commit using narrowing selection."
  (let* ((dir (or *magit-dir* (current-directory)))
         (commits (magit-recent-commits dir 30))
         (selection (if (null? commits)
                      (qt-echo-read-string app "Cherry-pick commit hash: ")
                      (qt-echo-read-with-narrowing app "Cherry-pick commit:" commits))))
    (when (and selection (> (string-length selection) 0))
      (let* ((hash (let ((sp (let loop ((i 0))
                                (cond ((>= i (string-length selection)) (string-length selection))
                                      ((char=? (string-ref selection i) #\space) i)
                                      (else (loop (+ i 1)))))))
                     (substring selection 0 sp))))
        (echo-message! (app-state-echo app) (string-append "Cherry-picking " hash "..."))
        (magit-run-git/async (list "cherry-pick" hash) dir
          (lambda (output)
            (echo-message! (app-state-echo app)
              (if (string=? output "")
                (string-append "Cherry-picked " hash)
                (string-trim output)))
            (when (buffer-by-name "*Magit*")
              (cmd-magit-status app))))))))

(def (cmd-magit-revert-commit app)
  "Revert a commit using narrowing selection."
  (let* ((dir (or *magit-dir* (current-directory)))
         (commits (magit-recent-commits dir 30))
         (selection (if (null? commits)
                      (qt-echo-read-string app "Revert commit hash: ")
                      (qt-echo-read-with-narrowing app "Revert commit:" commits))))
    (when (and selection (> (string-length selection) 0))
      (let* ((hash (let ((sp (let loop ((i 0))
                                (cond ((>= i (string-length selection)) (string-length selection))
                                      ((char=? (string-ref selection i) #\space) i)
                                      (else (loop (+ i 1)))))))
                     (substring selection 0 sp))))
        (echo-message! (app-state-echo app) (string-append "Reverting " hash "..."))
        (magit-run-git/async (list "revert" "--no-edit" hash) dir
          (lambda (output)
            (echo-message! (app-state-echo app)
              (if (string=? output "")
                (string-append "Reverted " hash)
                (string-trim output)))
            (when (buffer-by-name "*Magit*")
              (cmd-magit-status app))))))))

(def (cmd-magit-worktree app)
  "Manage git worktrees: list, add, or remove."
  (let* ((dir (or *magit-dir* (current-directory)))
         (output (magit-run-git '("worktree" "list") dir))
         (action (qt-echo-read-with-narrowing app "Worktree action:"
                   '("list" "add" "remove"))))
    (when action
      (cond
        ((string=? action "list")
         (let* ((ed (current-qt-editor app))
                (fr (app-state-frame app))
                (wt-buf (or (buffer-by-name "*Worktrees*")
                            (qt-buffer-create! "*Worktrees*" ed #f))))
           (qt-buffer-attach! ed wt-buf)
           (set! (qt-edit-window-buffer (qt-current-window fr)) wt-buf)
           (qt-plain-text-edit-set-text! ed
             (if (string=? output "") "No worktrees\n" output))
           (qt-text-document-set-modified! (buffer-doc-pointer wt-buf) #f)
           (qt-plain-text-edit-set-cursor-position! ed 0)))
        ((string=? action "add")
         (let* ((branches (magit-branch-names dir))
                (branch (qt-echo-read-with-narrowing app "Worktree branch:" branches))
                (path (and branch (> (string-length branch) 0)
                           (qt-echo-read-string app
                             (string-append "Worktree path for " branch ": ")))))
           (when (and path (> (string-length path) 0))
             (let ((result (magit-run-git (list "worktree" "add" path branch) dir)))
               (echo-message! (app-state-echo app)
                 (if (string=? result "")
                   (string-append "Added worktree: " path " [" branch "]")
                   (string-trim result)))))))
        ((string=? action "remove")
         (let* ((wt-lines (let ((lines []))
                            (let scan ((i 0) (start 0))
                              (cond
                                ((>= i (string-length output))
                                 (when (> i start)
                                   (set! lines (cons (substring output start i) lines)))
                                 (reverse lines))
                                ((char=? (string-ref output i) #\newline)
                                 (when (> i start)
                                   (set! lines (cons (substring output start i) lines)))
                                 (scan (+ i 1) (+ i 1)))
                                (else (scan (+ i 1) start))))))
                (selection (if (null? wt-lines)
                             (qt-echo-read-string app "Worktree path to remove: ")
                             (qt-echo-read-with-narrowing app "Remove worktree:" wt-lines))))
           (when (and selection (> (string-length selection) 0))
             ;; Extract path (first field before space)
             (let* ((path (let ((sp (let loop ((i 0))
                                      (cond ((>= i (string-length selection)) (string-length selection))
                                            ((char=? (string-ref selection i) #\space) i)
                                            (else (loop (+ i 1)))))))
                            (substring selection 0 sp)))
                    (result (magit-run-git (list "worktree" "remove" path) dir)))
               (echo-message! (app-state-echo app)
                 (if (string=? result "")
                   (string-append "Removed worktree: " path)
                   (string-trim result)))))))))))

