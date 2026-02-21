;;; -*- Gerbil -*-
;;; Qt parity commands — new feature implementations matching TUI layer.
;;; Chain position: after commands-config, before facade.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
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
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-shell
        :gemacs/qt/commands-modes
        :gemacs/qt/commands-config)

;;;============================================================================
;;; Dired navigation
;;;============================================================================

(def (cmd-dired-jump app)
  "Jump to dired for the current file's directory (C-x C-j)."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf)))
         (dir (if path (path-directory path) ".")))
    (with-catch
      (lambda (e)
        (echo-message! (app-state-echo app)
          (string-append "Error: "
            (with-output-to-string (lambda () (display-exception e))))))
      (lambda ()
        (dired-open-directory! app dir)))))

(def (cmd-dired-up-directory app)
  "Go up to parent directory in dired."
  (let* ((buf (current-qt-buffer app))
         (name (and buf (buffer-name buf)))
         (dir (if (and name (string-suffix? "/" name))
                ;; Dired buffer names end with "/"
                (let ((d (if (string-suffix? "/" name)
                           (substring name 0 (- (string-length name) 1))
                           name)))
                  (path-directory d))
                "..")))
    (with-catch
      (lambda (e)
        (echo-message! (app-state-echo app) "Cannot go up"))
      (lambda ()
        (dired-open-directory! app dir)))))

(def (cmd-dired-do-shell-command app)
  "Run shell command on marked files in dired."
  (let ((cmd (qt-echo-read-string app "Shell command: ")))
    (when (and cmd (not (string-empty? cmd)))
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "Shell command error"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "/bin/sh"
                               arguments: ["-c" cmd]
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f)))
            (process-status proc)
            (close-port proc)
            ;; Show output in a new buffer
            (let* ((fr (app-state-frame app))
                   (ed (current-qt-editor app))
                   (out-buf (qt-buffer-create! "*Shell Command*" ed #f)))
              (qt-buffer-attach! ed out-buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) out-buf)
              (qt-plain-text-edit-set-text! ed (or output ""))
              (qt-text-document-set-modified! (buffer-doc-pointer out-buf) #f)
              (qt-plain-text-edit-set-cursor-position! ed 0))))))))

;;;============================================================================
;;; Help / Apropos
;;;============================================================================

(def (cmd-apropos-emacs app)
  "Search commands by keyword (C-h a)."
  (let ((pattern (qt-echo-read-string app "Apropos: ")))
    (when (and pattern (not (string-empty? pattern)))
      (let* ((cmds (hash->list *all-commands*))
             (matches (filter (lambda (p)
                        (string-contains (symbol->string (car p)) pattern))
                        cmds))
             (lines (map (lambda (p) (symbol->string (car p))) matches)))
        (if (null? lines)
          (echo-message! (app-state-echo app)
            (string-append "No matches for: " pattern))
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app))
                 (out-buf (qt-buffer-create! "*Apropos*" ed #f))
                 (text (string-append
                         "Commands matching \"" pattern "\":\n\n"
                         (string-join (sort lines string<?) "\n"))))
            (qt-buffer-attach! ed out-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) out-buf)
            (qt-plain-text-edit-set-text! ed text)
            (qt-text-document-set-modified! (buffer-doc-pointer out-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))

;;;============================================================================
;;; Comment
;;;============================================================================

(def (cmd-indent-new-comment-line app)
  "Continue comment on new line (M-j)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line-num (qt-plain-text-edit-cursor-line ed))
         ;; Extract current line text
         (lines (string-split text #\newline))
         (line-text (if (< line-num (length lines))
                      (list-ref lines line-num)
                      ""))
         (trimmed (string-trim line-text)))
    (if (and (> (string-length trimmed) 0)
             (or (string-prefix? ";;" trimmed) (string-prefix? "//" trimmed)
                 (string-prefix? "#" trimmed) (string-prefix? "--" trimmed)))
      (let* ((prefix (cond ((string-prefix? ";;" trimmed) ";; ")
                           ((string-prefix? "//" trimmed) "// ")
                           ((string-prefix? "#" trimmed) "# ")
                           ((string-prefix? "--" trimmed) "-- ")
                           (else "")))
             (insert-text (string-append "\n" prefix))
             (before (substring text 0 pos))
             (after (substring text pos (string-length text))))
        (qt-plain-text-edit-set-text! ed
          (string-append before insert-text after))
        (qt-plain-text-edit-set-cursor-position! ed
          (+ pos (string-length insert-text))))
      ;; No comment prefix — just insert a newline
      (let ((before (substring text 0 pos))
            (after (substring text pos (string-length text))))
        (qt-plain-text-edit-set-text! ed
          (string-append before "\n" after))
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

;;;============================================================================
;;; Search
;;;============================================================================

(def (cmd-isearch-backward-regexp app)
  "Regexp search backward (C-M-r)."
  (cmd-search-backward app))

(def (cmd-replace-regexp app)
  "Replace using regexp."
  (cmd-query-replace-regexp app))

;;;============================================================================
;;; Org mode
;;;============================================================================

(def *qt-org-capture-file* #f)

(def (cmd-org-capture app)
  "Capture a note (org-capture)."
  (let ((text (qt-echo-read-string app "Capture: ")))
    (when (and text (not (string-empty? text)))
      (let* ((file (or *qt-org-capture-file*
                       (string-append (or (getenv "HOME") ".")
                                      "/.gemacs-capture.org")))
             (entry (string-append "\n* " text "\n")))
        (with-catch
          (lambda (e)
            (echo-message! (app-state-echo app) "Capture error"))
          (lambda ()
            (call-with-output-file [path: file append: #t]
              (lambda (p) (display entry p)))
            (echo-message! (app-state-echo app)
              (string-append "Captured: " text))))))))

(def (cmd-org-refile app)
  "Refile current heading to another location."
  (let ((target (qt-echo-read-string app "Refile to: ")))
    (when (and target (not (string-empty? target)))
      (echo-message! (app-state-echo app)
        (string-append "Refile: " target " (stub)")))))

(def (cmd-org-time-stamp app)
  "Insert org timestamp."
  (with-catch
    (lambda (e)
      (echo-message! (app-state-echo app) "Timestamp error"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "date"
                           arguments: '("+<%Y-%m-%d %a>")
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (ts (read-line proc)))
        (process-status proc)
        (close-port proc)
        (when (string? ts)
          (let* ((ed (current-qt-editor app))
                 (pos (qt-plain-text-edit-cursor-position ed))
                 (text (qt-plain-text-edit-text ed))
                 (before (substring text 0 pos))
                 (after (substring text pos (string-length text))))
            (qt-plain-text-edit-set-text! ed
              (string-append before ts after))
            (qt-plain-text-edit-set-cursor-position! ed
              (+ pos (string-length ts)))))))))

(def (cmd-org-insert-link app)
  "Insert org link [[url][description]]."
  (let ((url (qt-echo-read-string app "Link URL: ")))
    (when (and url (not (string-empty? url)))
      (let ((desc (qt-echo-read-string app "Description: ")))
        (let* ((ed (current-qt-editor app))
               (link (if (and desc (not (string-empty? desc)))
                       (string-append "[[" url "][" desc "]]")
                       (string-append "[[" url "]]")))
               (pos (qt-plain-text-edit-cursor-position ed))
               (text (qt-plain-text-edit-text ed))
               (before (substring text 0 pos))
               (after (substring text pos (string-length text))))
          (qt-plain-text-edit-set-text! ed
            (string-append before link after))
          (qt-plain-text-edit-set-cursor-position! ed
            (+ pos (string-length link))))))))

(def (cmd-org-narrow-to-subtree app)
  "Narrow to current org subtree."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline)))
    ;; Find heading at or before current line
    (let loop ((l line-num))
      (if (< l 0)
        (echo-message! (app-state-echo app) "No heading found")
        (let ((ln (if (< l (length lines)) (list-ref lines l) "")))
          (if (and (> (string-length ln) 0) (char=? (string-ref ln 0) #\*))
            ;; Found heading; find end of subtree
            (let* ((level (let lp ((i 0))
                     (if (and (< i (string-length ln))
                              (char=? (string-ref ln i) #\*))
                       (lp (+ i 1)) i)))
                   (end (let lp2 ((el (+ l 1)))
                     (if (>= el (length lines)) el
                       (let ((eln (list-ref lines el)))
                         (if (and (> (string-length eln) 0)
                                  (char=? (string-ref eln 0) #\*)
                                  (<= (let lp3 ((j 0))
                                         (if (and (< j (string-length eln))
                                                  (char=? (string-ref eln j) #\*))
                                           (lp3 (+ j 1)) j))
                                       level))
                           el (lp2 (+ el 1))))))))
              (cmd-narrow-to-region app)
              (echo-message! (app-state-echo app)
                (string-append "Narrowed to subtree: " (string-trim ln))))
            (loop (- l 1))))))))

(def (cmd-org-sort app)
  "Sort org entries."
  (echo-message! (app-state-echo app) "Org sort: use M-x sort-lines on region"))

;;;============================================================================
;;; Project
;;;============================================================================

(def (find-project-root-qt dir)
  "Find project root by looking for .git, gerbil.pkg, Makefile, etc."
  (let loop ((d (if (string-suffix? "/" dir) dir (string-append dir "/"))))
    (cond
      ((or (string=? d "/") (string=? d "")) #f)
      ((or (file-exists? (string-append d ".git"))
           (file-exists? (string-append d "gerbil.pkg"))
           (file-exists? (string-append d "Makefile"))
           (file-exists? (string-append d "package.json")))
       d)
      (else (loop (path-directory
                    (substring d 0 (- (string-length d) 1))))))))

(def (cmd-project-switch-to-buffer app)
  "Switch to a buffer in the current project."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf)))
         (root (if path (find-project-root-qt (path-directory path)) #f)))
    (if (not root)
      (cmd-switch-buffer app)
      (let* ((bufs (filter (lambda (b)
                     (let ((fp (buffer-file-path b)))
                       (and fp (string-prefix? root fp))))
                     *buffer-list*))
             (names (map buffer-name bufs)))
        (if (null? names)
          (echo-message! (app-state-echo app) "No project buffers")
          (let ((name (qt-echo-read-string app
                        (string-append "Project buffer [" root "]: "))))
            (when (and name (not (string-empty? name)))
              (let ((target (find (lambda (b)
                              (string=? (buffer-name b) name))
                              bufs)))
                (if target
                  (let* ((fr (app-state-frame app))
                         (ed (current-qt-editor app)))
                    (qt-buffer-attach! ed target)
                    (set! (qt-edit-window-buffer (qt-current-window fr))
                          target))
                  (echo-message! (app-state-echo app)
                    "Buffer not found"))))))))))

(def (cmd-project-kill-buffers app)
  "Kill all buffers in the current project."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf)))
         (root (if path (find-project-root-qt (path-directory path)) #f)))
    (if (not root)
      (echo-message! (app-state-echo app) "Not in a project")
      (let* ((bufs (filter (lambda (b)
                     (let ((fp (buffer-file-path b)))
                       (and fp (string-prefix? root fp))))
                     *buffer-list*))
             (count (length bufs)))
        (for-each (lambda (b)
                    (set! *buffer-list* (remq b *buffer-list*)))
                  bufs)
        (echo-message! (app-state-echo app)
          (string-append "Killed " (number->string count)
                         " project buffers"))))))

;;;============================================================================
;;; Version control
;;;============================================================================

(def (cmd-vc-next-action app)
  "Do the next logical VCS action (C-x v v)."
  (let* ((buf (current-qt-buffer app))
         (path (and buf (buffer-file-path buf))))
    (if (not path)
      (echo-message! (app-state-echo app) "No file for VC")
      (with-catch
        (lambda (e)
          (echo-message! (app-state-echo app) "VC error"))
        (lambda ()
          (let* ((dir (path-directory path))
                 (proc (open-process
                         (list path: "git"
                               arguments: ["status" "--porcelain" "--" path]
                               directory: dir
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (status-line (read-line proc)))
            (process-status proc)
            (close-port proc)
            (cond
              ((eof-object? status-line)
               (echo-message! (app-state-echo app)
                 "File is clean (no changes)"))
              ((or (string-prefix? "??" status-line)
                   (string-prefix? "A " status-line))
               ;; Untracked or added — stage it
               (let ((p2 (open-process
                           (list path: "git"
                                 arguments: ["add" "--" path]
                                 directory: dir
                                 stdin-redirection: #f
                                 stdout-redirection: #t
                                 stderr-redirection: #t))))
                 (process-status p2)
                 (close-port p2)
                 (echo-message! (app-state-echo app)
                   (string-append "Staged: "
                     (path-strip-directory path)))))
              ((or (string-prefix? " M" status-line)
                   (string-prefix? "M " status-line)
                   (string-prefix? "MM" status-line))
               ;; Modified — stage it
               (let ((p2 (open-process
                           (list path: "git"
                                 arguments: ["add" "--" path]
                                 directory: dir
                                 stdin-redirection: #f
                                 stdout-redirection: #t
                                 stderr-redirection: #t))))
                 (process-status p2)
                 (close-port p2)
                 (echo-message! (app-state-echo app)
                   (string-append "Staged: "
                     (path-strip-directory path)))))
              (else
               (echo-message! (app-state-echo app)
                 (string-append "Status: "
                   (string-trim status-line)))))))))))
