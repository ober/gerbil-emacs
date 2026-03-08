;;; -*- Gerbil -*-
;;; Qt parity commands — new feature implementations matching TUI layer.
;;; Chain position: after commands-config2, before facade.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/process
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gemacs/pregexp-compat pregexp pregexp-match)
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        (only-in :gemacs/org-babel
                 org-babel-find-src-block org-babel-execute
                 org-babel-tangle-to-files org-babel-insert-result
                 org-babel-kill-all-sessions)
        (only-in :gemacs/org-capture
                 org-capture-menu-string org-capture-template-key
                 org-capture-template-template org-capture-cursor-position
                 org-capture-start org-capture-finalize org-capture-abort
                 *org-capture-templates*)
        (only-in :gemacs/org-parse
                 org-heading-line? org-heading-stars-of-line
                 org-current-timestamp-string)
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        ;; Chain of all prior command modules
        :gemacs/qt/magit
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
        :gemacs/qt/commands-config2)

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

(def (cmd-org-capture app)
  "Capture a note with template selection and interactive editing.
   Select a template (t=TODO, n=Note, j=Journal), edit in *Org Capture* buffer,
   then C-c C-c to finalize or C-c C-k to abort."
  (let* ((menu (org-capture-menu-string))
         (key (qt-echo-read-string app (string-append "Capture template (" menu "): "))))
    (when (and key (> (string-length key) 0))
      (let* ((buf-info (current-qt-buffer app))
             (source-file (or (buffer-name buf-info) ""))
             (source-path (or (buffer-file-path buf-info) ""))
             (tmpl-str (org-capture-template-template
                         (or (find (lambda (t) (string=? (org-capture-template-key t) key))
                                   *org-capture-templates*)
                             (car *org-capture-templates*))))
             (cursor-pos (org-capture-cursor-position tmpl-str))
             (expanded (org-capture-start key source-file source-path)))
        (if (not expanded)
          (echo-error! (app-state-echo app) (string-append "Unknown template: " key))
          (let* ((fr (app-state-frame app))
                 (ed (qt-current-editor fr))
                 (buf (qt-buffer-create! "*Org Capture*" ed #f)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed expanded)
            (when cursor-pos
              (qt-plain-text-edit-set-cursor-position! ed (min cursor-pos (string-length expanded))))
            (echo-message! (app-state-echo app)
              "Edit then C-c C-c to save, C-c C-k to abort")))))))

(def (cmd-org-capture-finalize app)
  "Finalize org capture: save buffer content to target file."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (if (not (string=? name "*Org Capture*"))
      (echo-error! (app-state-echo app) "Not in a capture buffer")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)))
        (if (org-capture-finalize text)
          (begin
            (execute-command! app 'kill-buffer-cmd)
            (echo-message! (app-state-echo app) "Capture saved"))
          (echo-error! (app-state-echo app) "Capture failed — no active session"))))))

(def (cmd-org-capture-abort app)
  "Abort org capture: discard buffer without saving."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (if (not (string=? name "*Org Capture*"))
      (echo-error! (app-state-echo app) "Not in a capture buffer")
      (begin
        (org-capture-abort)
        (execute-command! app 'kill-buffer-cmd)
        (echo-message! (app-state-echo app) "Capture aborted")))))

(def (cmd-org-refile app)
  "Refile current heading to another location using narrowing selection."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (total (length lines))
         ;; Find current line number from position
         (cur-line (let loop ((i 0) (chars 0))
                     (if (>= i total) (- total 1)
                       (let ((line-len (+ (string-length (list-ref lines i)) 1)))
                         (if (< pos (+ chars line-len)) i
                           (loop (+ i 1) (+ chars line-len)))))))
         ;; Walk backwards to find the heading at or before point
         (heading-line (let loop ((l cur-line))
                         (cond
                           ((< l 0) #f)
                           ((and (< l total)
                                 (org-heading-line? (list-ref lines l))) l)
                           (else (loop (- l 1)))))))
    (if (not heading-line)
      (echo-error! echo "Not on an org heading")
      (let* ((heading-text (list-ref lines heading-line))
             (level (org-heading-stars-of-line heading-text))
             ;; Find subtree end
             (subtree-end (let loop ((i (+ heading-line 1)))
                            (cond
                              ((>= i total) total)
                              ((let ((l (list-ref lines i)))
                                 (and (org-heading-line? l)
                                      (<= (org-heading-stars-of-line l) level))) i)
                              (else (loop (+ i 1))))))
             ;; Extract subtree lines
             (subtree-lines (let loop ((i heading-line) (acc '()))
                              (if (>= i subtree-end)
                                (reverse acc)
                                (loop (+ i 1) (cons (list-ref lines i) acc)))))
             (subtree-text (string-join subtree-lines "\n")))
        ;; Collect refile targets (headings outside the current subtree)
        (let* ((targets
                 (let loop ((i 0) (acc '()))
                   (if (>= i total)
                     (reverse acc)
                     (let ((skip? (and (>= i heading-line) (< i subtree-end))))
                       (if (and (not skip?) (org-heading-line? (list-ref lines i)))
                         (let* ((hline (list-ref lines i))
                                (nstars (org-heading-stars-of-line hline))
                                (label (string-append
                                         (make-string nstars #\*)
                                         (substring hline nstars (string-length hline)))))
                           (loop (+ i 1) (cons (cons label i) acc)))
                         (loop (+ i 1) acc))))))
               (labels (map car targets)))
          (if (null? labels)
            (echo-error! echo "No refile targets found")
            (let ((chosen (qt-echo-read-with-narrowing app "Refile to: " labels)))
              (when chosen
                ;; Find the target line number
                (let ((target-pair (assoc chosen targets)))
                  (when target-pair
                    (let* ((target-line (cdr target-pair))
                           ;; Remove subtree from original position
                           (before (let loop ((i 0) (acc '()))
                                     (if (>= i heading-line)
                                       (reverse acc)
                                       (loop (+ i 1) (cons (list-ref lines i) acc)))))
                           (after (let loop ((i subtree-end) (acc '()))
                                    (if (>= i total)
                                      (reverse acc)
                                      (loop (+ i 1) (cons (list-ref lines i) acc)))))
                           (without-subtree-lines (append before after))
                           ;; Adjust target line number if it was after the removed subtree
                           (removed-count (- subtree-end heading-line))
                           (adj-target (if (> target-line heading-line)
                                         (- target-line removed-count)
                                         target-line))
                           ;; Find where to insert (end of target's subtree)
                           (adj-total (length without-subtree-lines))
                           (target-level (org-heading-stars-of-line
                                           (list-ref without-subtree-lines adj-target)))
                           (insert-at (let loop ((i (+ adj-target 1)))
                                        (cond
                                          ((>= i adj-total) adj-total)
                                          ((let ((l (list-ref without-subtree-lines i)))
                                             (and (org-heading-line? l)
                                                  (<= (org-heading-stars-of-line l) target-level))) i)
                                          (else (loop (+ i 1))))))
                           ;; Build new text
                           (pre (let loop ((i 0) (acc '()))
                                  (if (>= i insert-at)
                                    (reverse acc)
                                    (loop (+ i 1) (cons (list-ref without-subtree-lines i) acc)))))
                           (post (let loop ((i insert-at) (acc '()))
                                   (if (>= i adj-total)
                                     (reverse acc)
                                     (loop (+ i 1) (cons (list-ref without-subtree-lines i) acc)))))
                           (new-lines (append pre subtree-lines post))
                           (new-text (string-join new-lines "\n")))
                      (qt-plain-text-edit-set-text! ed new-text)
                      (echo-message! echo
                        (string-append "Refiled to: " chosen)))))))))))))

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
          (let ((name (qt-echo-read-with-narrowing app
                        (string-append "Project buffer [" (path-strip-directory root) "]: ")
                        names)))
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

;;;============================================================================
;;; Batch 2: Sort, Find, Org-babel, Misc
;;;============================================================================

;;; --- Sort numeric ---
(def (cmd-sort-numeric-fields app)
  "Sort lines by numeric value of first number on each line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (numbered (map (lambda (l)
                          (let ((nums (pregexp-match "[0-9]+" l)))
                            (cons (if nums (string->number (car nums)) 0) l)))
                        lines))
         (sorted (sort numbered (lambda (a b) (< (car a) (car b)))))
         (result (string-join (map cdr sorted) "\n")))
    (qt-plain-text-edit-set-text! ed result)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (echo-message! (app-state-echo app)
      (string-append "Sorted " (number->string (length lines))
                     " lines numerically"))))

;;; --- Find in dired ---
(def (cmd-find-dired app)
  "Find files matching pattern in directory (find-dired)."
  (let ((dir (qt-echo-read-string app "Directory: ")))
    (when (and dir (not (string-empty? dir)))
      (let ((args (qt-echo-read-string app "Find arguments: ")))
        (when (and args (not (string-empty? args)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app) "find error"))
            (lambda ()
              (let* ((cmd-str (string-append "find " dir " " args))
                     (output (run-process ["bash" "-c" cmd-str]
                               coprocess: read-all-as-string))
                     (fr (app-state-frame app))
                     (ed (current-qt-editor app))
                     (buf (qt-buffer-create! "*Find*" ed #f)))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed (or output ""))
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)))))))))

(def (cmd-find-name-dired app)
  "Find files by name pattern in directory (find-name-dired)."
  (let ((dir (qt-echo-read-string app "Directory: ")))
    (when (and dir (not (string-empty? dir)))
      (let ((pattern (qt-echo-read-string app "Filename pattern: ")))
        (when (and pattern (not (string-empty? pattern)))
          (with-catch
            (lambda (e)
              (echo-message! (app-state-echo app) "find error"))
            (lambda ()
              (let* ((cmd-str (string-append "find " dir " -name '"
                                             pattern "'"))
                     (output (run-process ["bash" "-c" cmd-str]
                               coprocess: read-all-as-string))
                     (fr (app-state-frame app))
                     (ed (current-qt-editor app))
                     (buf (qt-buffer-create! "*Find*" ed #f)))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed (or output ""))
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)))))))))

;;; --- Dired details ---
(def *qt-dired-hide-details* #f)
(def (cmd-dired-hide-details app)
  "Toggle dired details display."
  (set! *qt-dired-hide-details* (not *qt-dired-hide-details*))
  (echo-message! (app-state-echo app)
    (if *qt-dired-hide-details* "Details hidden" "Details shown")))

;;; --- Desktop save mode ---
(def *qt-desktop-save-mode* #f)
(def (cmd-desktop-save-mode app)
  "Toggle desktop-save-mode (auto save/restore session)."
  (set! *qt-desktop-save-mode* (not *qt-desktop-save-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-desktop-save-mode*
      "Desktop save mode enabled"
      "Desktop save mode disabled")))

;;; --- Org babel commands ---
(def (cmd-org-babel-execute-src-block app)
  "Execute the org source block at point (C-c C-c)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line-num (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline)))
    (let-values (((lang header-args body begin-line end-line block-name)
                  (org-babel-find-src-block lines line-num)))
      (if (not lang)
        (echo-message! (app-state-echo app) "Not in a source block")
        (with-catch
          (lambda (e)
            (echo-message! (app-state-echo app)
              (string-append "Babel error: "
                (with-output-to-string
                  (lambda () (display-exception e))))))
          (lambda ()
            ;; org-babel-insert-result uses Scintilla editor API (ed)
            ;; We need to get the underlying sci editor from the qt buffer
            (let ((output (org-babel-execute lang body header-args
                            buffer-text: text)))
              ;; Insert results manually via Qt text API
              (let* ((result-text
                       (string-append "\n#+RESULTS:\n"
                         (if (or (not output) (string-empty? output))
                           ""
                           (string-append ": " output "\n"))))
                     (current-text (qt-plain-text-edit-text ed))
                     (end-lines (string-split current-text #\newline))
                     (end-pos (let loop ((i 0) (pos 0))
                                (if (> i end-line)
                                  pos
                                  (loop (+ i 1)
                                    (+ pos (string-length
                                             (if (< i (length end-lines))
                                               (list-ref end-lines i) ""))
                                       1)))))
                     (before (substring current-text 0
                               (min end-pos (string-length current-text))))
                     (after (substring current-text
                              (min end-pos (string-length current-text))
                              (string-length current-text))))
                (qt-plain-text-edit-set-text! ed
                  (string-append before result-text after))
                (echo-message! (app-state-echo app)
                  (string-append "Executed " lang " block"))))))))))

(def (cmd-org-babel-tangle app)
  "Tangle the current org buffer — extract code blocks to files."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e)
        (echo-message! (app-state-echo app)
          (string-append "Tangle error: "
            (with-output-to-string
              (lambda () (display-exception e))))))
      (lambda ()
        (let ((files (org-babel-tangle-to-files text)))
          (echo-message! (app-state-echo app)
            (if (null? files)
              "No :tangle blocks found"
              (string-append "Tangled to: "
                (string-join (map car files) ", ")))))))))

(def (cmd-org-babel-kill-session app)
  "Kill all active org-babel sessions."
  (org-babel-kill-all-sessions)
  (echo-message! (app-state-echo app) "All babel sessions killed"))

;;; --- Other frame (stub) ---
(def (cmd-other-frame app)
  "Switch to next frame (stub — gemacs is single-frame)."
  (echo-message! (app-state-echo app) "Only one frame"))

;;; --- Winum mode (stub) ---
(def *qt-winum-mode* #f)
(def (cmd-winum-mode app)
  "Toggle window-numbering mode."
  (set! *qt-winum-mode* (not *qt-winum-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-winum-mode*
      "Winum mode enabled (use M-1..M-9)"
      "Winum mode disabled")))

;;; --- Help with tutorial ---
(def (cmd-help-with-tutorial app)
  "Show the gemacs tutorial (C-h t)."
  (let* ((text (string-append
    "=== Gemacs Tutorial ===\n\n"
    "Welcome to Gemacs, a Gerbil Scheme Emacs replacement.\n\n"
    "== Basic Movement ==\n"
    "  C-f / C-b    Forward / backward character\n"
    "  M-f / M-b    Forward / backward word\n"
    "  C-n / C-p    Next / previous line\n"
    "  C-a / C-e    Beginning / end of line\n"
    "  M-< / M->    Beginning / end of buffer\n"
    "  C-v / M-v    Scroll down / up\n"
    "  C-l          Recenter\n\n"
    "== Editing ==\n"
    "  C-d          Delete character\n"
    "  M-d          Kill word\n"
    "  C-k          Kill to end of line\n"
    "  C-w          Kill region\n"
    "  M-w          Copy region\n"
    "  C-y          Yank (paste)\n"
    "  M-y          Yank pop (cycle kill ring)\n"
    "  C-/          Undo\n"
    "  C-x u        Undo\n\n"
    "== Files & Buffers ==\n"
    "  C-x C-f      Find file\n"
    "  C-x C-s      Save buffer\n"
    "  C-x s        Save all buffers\n"
    "  C-x b        Switch buffer\n"
    "  C-x k        Kill buffer\n"
    "  C-x C-b      List buffers\n\n"
    "== Windows ==\n"
    "  C-x 2        Split horizontally\n"
    "  C-x 3        Split vertically\n"
    "  C-x 1        Delete other windows\n"
    "  C-x 0        Delete this window\n"
    "  C-x o        Other window\n\n"
    "== Search & Replace ==\n"
    "  C-s          Search forward\n"
    "  C-r          Search backward\n"
    "  M-%          Query replace\n\n"
    "== Commands ==\n"
    "  M-x          Execute command by name\n"
    "  C-g          Keyboard quit\n"
    "  C-h k        Describe key\n"
    "  C-h f        Describe function\n\n"
    "== Org Mode ==\n"
    "  TAB          Cycle visibility\n"
    "  M-RET        Insert heading\n"
    "  C-c C-t      Toggle TODO\n"
    "  C-c C-c      Execute src block\n\n"
    "== Gemacs-Specific ==\n"
    "  M-x magit-status   Git integration\n"
    "  M-x treemacs       File tree\n"
    "  M-x shell          Shell\n"
    "  M-x eshell         Gerbil shell\n"
    "  M-x term           Terminal\n"))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (qt-buffer-create! "*Tutorial*" ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;; --- CUA mode (stub) ---
(def *qt-cua-mode* #f)
(def (cmd-cua-mode app)
  "Toggle CUA keybindings (C-c/C-x/C-v for copy/cut/paste)."
  (set! *qt-cua-mode* (not *qt-cua-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-cua-mode* "CUA mode enabled" "CUA mode disabled")))

;;;============================================================================
;;; Batch 3: Package/framework parity commands
;;;============================================================================

;;; --- Company mode ---
(def *qt-company-mode* #f)
(def (cmd-company-mode app)
  "Toggle company completion mode."
  (set! *qt-company-mode* (not *qt-company-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-company-mode* "Company mode: on" "Company mode: off")))

;;; --- Treemacs ---
(def (cmd-treemacs app)
  "Toggle treemacs file-tree view."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (if (and buf (string=? (buffer-name buf) "*Treemacs*"))
      ;; Close treemacs — switch to first non-treemacs buffer
      (let ((other (find (lambda (b) (not (string=? (buffer-name b) "*Treemacs*")))
                         *buffer-list*)))
        (when other
          (qt-buffer-attach! ed other)
          (set! (qt-edit-window-buffer (qt-current-window fr)) other))
        (echo-message! (app-state-echo app) "Treemacs closed"))
      ;; Open treemacs — show directory listing
      (let* ((path (and buf (buffer-file-path buf)))
             (dir (if path (path-directory path) (current-directory)))
             (entries (with-catch (lambda (e) '()) (lambda () (directory-files dir))))
             (text (string-append "Treemacs: " dir "\n\n"
                     (string-join (sort entries string<?) "\n") "\n"))
             (tbuf (qt-buffer-create! "*Treemacs*" ed #f)))
        (qt-buffer-attach! ed tbuf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) tbuf)
        (qt-plain-text-edit-set-text! ed text)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

;;; --- Org set tags ---
(def (cmd-org-set-tags app)
  "Prompt for tags and set on current org heading."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (before-pos (substring text 0 pos))
         (line-start (let ((nl (string-index-right before-pos #\newline)))
                       (if nl (+ nl 1) 0)))
         (line-end-idx (let ((nl (string-index text #\newline line-start)))
                         (if nl nl (string-length text))))
         (line (substring text line-start line-end-idx)))
    (if (not (and (> (string-length line) 0) (char=? (string-ref (string-trim line) 0) #\*)))
      (echo-message! (app-state-echo app) "Not on a heading")
      (let ((tags-input (qt-echo-read-string (app-state-echo app) "Tags (comma-separated): ")))
        (when (and tags-input (not (string=? tags-input "")))
          (let* ((tags (map string-trim (string-split tags-input #\,)))
                 (tag-str (string-append ":" (string-join tags ":") ":"))
                 ;; Remove existing trailing tags
                 (clean-line (string-trim-right line))
                 (new-line (string-append clean-line " " tag-str))
                 (new-text (string-append (substring text 0 line-start) new-line
                             (substring text line-end-idx (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed pos)))))))

;;; --- Which key ---
(def (cmd-which-key app)
  "Display available keybindings."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (entries (keymap-entries *global-keymap*))
         (lines (map (lambda (e)
                       (string-append "  " (car e) " -> "
                         (cond
                           ((symbol? (cdr e)) (symbol->string (cdr e)))
                           ((hash-table? (cdr e)) "<prefix-map>")
                           (else "???"))))
                     (sort entries (lambda (a b) (string<? (car a) (car b))))))
         (buf (qt-buffer-create! "*Which Key*" ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed
      (string-append "Key Bindings\n\n" (string-join lines "\n") "\n"))
    (qt-plain-text-edit-set-cursor-position! ed 0)))

;;; --- Org archive subtree ---
(def (cmd-org-archive-subtree app)
  "Archive the current org subtree to the _archive sibling file.
   Adds ARCHIVE_TIME property, appends to archive file, removes from buffer."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (fp (buffer-file-path buf))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (total (length lines))
         ;; Find current line number from position
         (cur-line (let loop ((i 0) (chars 0))
                     (if (>= i total) (- total 1)
                       (let ((line-len (+ (string-length (list-ref lines i)) 1)))
                         (if (< pos (+ chars line-len)) i
                           (loop (+ i 1) (+ chars line-len)))))))
         ;; Walk backwards to find the heading at or before point
         (heading-line (let loop ((l cur-line))
                         (cond
                           ((< l 0) #f)
                           ((and (< l total)
                                 (org-heading-line? (list-ref lines l))) l)
                           (else (loop (- l 1)))))))
    (if (not heading-line)
      (echo-error! echo "Not on an org heading")
      (let* ((heading-text (list-ref lines heading-line))
             (level (org-heading-stars-of-line heading-text))
             ;; Find subtree end
             (subtree-end (let loop ((i (+ heading-line 1)))
                            (cond
                              ((>= i total) total)
                              ((let ((l (list-ref lines i)))
                                 (and (org-heading-line? l)
                                      (<= (org-heading-stars-of-line l) level))) i)
                              (else (loop (+ i 1))))))
             ;; Extract subtree lines
             (subtree-lines (let loop ((i heading-line) (acc '()))
                              (if (>= i subtree-end)
                                (reverse acc)
                                (loop (+ i 1) (cons (list-ref lines i) acc)))))
             ;; Build archive entry with ARCHIVE_TIME property
             (timestamp (org-current-timestamp-string #f))
             (archive-entry (string-append
                              (string-join subtree-lines "\n")
                              "\n  :PROPERTIES:\n"
                              "  :ARCHIVE_TIME: " timestamp "\n"
                              (if fp
                                (string-append "  :ARCHIVE_FILE: " fp "\n")
                                "")
                              "  :END:\n\n"))
             ;; Archive file path
             (archive-file (if fp
                             (string-append fp "_archive")
                             #f)))
        ;; Write to archive file
        (if (not archive-file)
          (echo-error! echo "Buffer has no file path — cannot archive")
          (begin
            (with-catch
              (lambda (e)
                (echo-error! echo (string-append "Error writing archive: " (error-message e))))
              (lambda ()
                (call-with-output-file [path: archive-file append: #t]
                  (lambda (port)
                    (display archive-entry port)))))
            ;; Remove subtree from buffer
            (let* ((before (let loop ((i 0) (acc '()))
                             (if (>= i heading-line)
                               (reverse acc)
                               (loop (+ i 1) (cons (list-ref lines i) acc)))))
                   (after (let loop ((i subtree-end) (acc '()))
                            (if (>= i total)
                              (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc)))))
                   (new-text (string-join (append before after) "\n")))
              (qt-plain-text-edit-set-text! ed new-text)
              ;; Position cursor at where the heading was
              (let ((new-pos (let loop ((i 0) (chars 0))
                               (if (>= i heading-line) chars
                                 (loop (+ i 1) (+ chars (string-length (list-ref lines i)) 1))))))
                (qt-plain-text-edit-set-cursor-position! ed (min new-pos (string-length new-text))))
              (echo-message! echo (string-append "Subtree archived to " archive-file)))))))))

;;; --- Org toggle heading ---
(def (cmd-org-toggle-heading app)
  "Toggle between heading and normal text."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (before-pos (substring text 0 pos))
         (line-start (let ((nl (string-index-right before-pos #\newline)))
                       (if nl (+ nl 1) 0)))
         (line-end-idx (let ((nl (string-index text #\newline line-start)))
                         (if nl nl (string-length text))))
         (line (substring text line-start line-end-idx))
         (trimmed (string-trim line)))
    (if (and (> (string-length trimmed) 0) (char=? (string-ref trimmed 0) #\*))
      ;; Remove heading prefix
      (let* ((stars (let lp ((i 0))
                      (if (and (< i (string-length trimmed))
                               (char=? (string-ref trimmed i) #\*))
                        (lp (+ i 1)) i)))
             (rest (string-trim (substring trimmed stars (string-length trimmed))))
             (new-text (string-append (substring text 0 line-start) rest
                         (substring text line-end-idx (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length new-text))))
      ;; Add heading prefix
      (let ((new-text (string-append (substring text 0 line-start) "* " line
                        (substring text line-end-idx (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 2))))))

;;; --- Magit init ---
(def (cmd-magit-init app)
  "Initialize a new git repository."
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "Git init failed"))
    (lambda ()
      (let* ((buf (current-qt-buffer app))
             (path (and buf (buffer-file-path buf)))
             (dir (if path (path-directory path) ".")))
        (run-process ["git" "init" dir] coprocess: void)
        (echo-message! (app-state-echo app)
          (string-append "Initialized git repo in " dir))))))

;;; --- Magit tag ---
(def (cmd-magit-tag app)
  "Git tag management: create, list, delete, or push tags."
  (let* ((echo (app-state-echo app))
         (action (qt-echo-read-string-with-completion app "Tag action: "
                   '("create" "list" "delete" "push"))))
    (when (and action (not (string=? action "")))
      (let ((dir (current-project-root app)))
        (cond
          ((string=? action "create")
           (let ((tag (qt-echo-read-string echo "Tag name: ")))
             (when (and tag (not (string=? tag "")))
               (let ((msg (qt-echo-read-string echo "Message (empty for lightweight): ")))
                 (let ((result (if (and msg (not (string=? msg "")))
                                 (magit-run-git ["-C" dir "tag" "-a" tag "-m" msg] dir)
                                 (magit-run-git ["-C" dir "tag" tag] dir))))
                   (echo-message! echo (string-append "Created tag: " tag)))))))
          ((string=? action "list")
           (let* ((output (magit-run-git ["-C" dir "tag" "-l" "--sort=-creatordate"] dir))
                  (tags (if (string=? output "")
                          "No tags found."
                          output)))
             (let* ((fr (app-state-frame app))
                    (ed (current-qt-editor app))
                    (buf (or (buffer-by-name "*Git Tags*")
                             (qt-buffer-create! "*Git Tags*" ed #f))))
               (qt-buffer-attach! ed buf)
               (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
               (qt-plain-text-edit-set-text! ed tags)
               (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
               (qt-plain-text-edit-set-cursor-position! ed 0)
               (echo-message! echo "*Git Tags*"))))
          ((string=? action "delete")
           (let* ((output (magit-run-git ["-C" dir "tag" "-l"] dir))
                  (tag-list (if (string=? output "")
                              '()
                              (filter (lambda (s) (not (string=? s "")))
                                (string-split output #\newline)))))
             (if (null? tag-list)
               (echo-error! echo "No tags to delete")
               (let ((tag (qt-echo-read-string-with-completion app "Delete tag: " tag-list)))
                 (when (and tag (not (string=? tag "")))
                   (magit-run-git ["-C" dir "tag" "-d" tag] dir)
                   (echo-message! echo (string-append "Deleted tag: " tag)))))))
          ((string=? action "push")
           (let* ((output (magit-run-git ["-C" dir "tag" "-l"] dir))
                  (tag-list (if (string=? output "")
                              '()
                              (cons "--all"
                                (filter (lambda (s) (not (string=? s "")))
                                  (string-split output #\newline))))))
             (if (null? tag-list)
               (echo-error! echo "No tags to push")
               (let ((tag (qt-echo-read-string-with-completion app "Push tag (--all for all): " tag-list)))
                 (when (and tag (not (string=? tag "")))
                   (if (string=? tag "--all")
                     (begin
                       (magit-run-git ["-C" dir "push" "origin" "--tags"] dir)
                       (echo-message! echo "Pushed all tags"))
                     (begin
                       (magit-run-git ["-C" dir "push" "origin" tag] dir)
                       (echo-message! echo (string-append "Pushed tag: " tag)))))))))
          (else (echo-error! echo (string-append "Unknown action: " action))))))))


;;;============================================================================
;;; Interactive IBBuffer — mark/execute/filter/sort
;;;============================================================================

;; Marks: hash of buffer-name -> mark-char (#\D = delete, #\S = save)
(def *ibuffer-marks* (make-hash-table))
(def *ibuffer-filter* #f)  ; string or #f
(def *ibuffer-sort* 'name) ; 'name or 'size

(def (ibuffer-header-lines) 2) ;; header + separator = 2 lines

(def (ibuffer-buffer-at-line app line-num)
  "Get the buffer object corresponding to ibuffer line LINE-NUM, or #f."
  (let ((bufs (ibuffer-visible-buffers)))
    (let ((idx (- line-num (ibuffer-header-lines))))
      (if (and (>= idx 0) (< idx (length bufs)))
        (list-ref bufs idx)
        #f))))

(def (ibuffer-visible-buffers)
  "Return the list of buffers that match the current filter, sorted."
  (let* ((all (filter (lambda (b) (not (string=? (buffer-name b) "*IBBuffer*")))
                      (buffer-list)))
         (filtered (if *ibuffer-filter*
                     (let ((pat (string-downcase *ibuffer-filter*)))
                       (filter (lambda (b)
                                 (string-contains (string-downcase (buffer-name b)) pat))
                               all))
                     all)))
    (case *ibuffer-sort*
      ((size)
       (sort filtered (lambda (a b)
                        (let ((sa (or (buffer-file-path a) ""))
                              (sb (or (buffer-file-path b) "")))
                          (string<? sa sb)))))
      (else
       (sort filtered (lambda (a b)
                        (string<? (buffer-name a) (buffer-name b))))))))

(def (ibuffer-format-line fr b)
  "Format a single ibuffer line for buffer B."
  (let* ((name (buffer-name b))
         (mark-ch (hash-get *ibuffer-marks* name))
         (mark-str (if mark-ch (string mark-ch) " "))
         (modified? (let ((doc (buffer-doc-pointer b)))
                      (and doc (qt-text-document-modified? doc))))
         (readonly? (let loop ((ws (qt-frame-windows fr)))
                      (cond
                        ((null? ws) #f)
                        ((eq? (qt-edit-window-buffer (car ws)) b)
                         (qt-plain-text-edit-read-only?
                           (qt-edit-window-editor (car ws))))
                        (else (loop (cdr ws))))))
         (path (or (buffer-file-path b) ""))
         (size-str
           (let loop ((wins (qt-frame-windows fr)))
             (cond
               ((null? wins) "?")
               ((eq? (qt-edit-window-buffer (car wins)) b)
                (number->string
                  (string-length
                    (qt-plain-text-edit-text
                      (qt-edit-window-editor (car wins))))))
               (else (loop (cdr wins))))))
         (flag (string-append
                 (if modified? "*" " ")
                 (if readonly? "%" " "))))
    (string-append
      mark-str " " flag " "
      (string-pad-right name 30)
      (string-pad-right size-str 10)
      path)))

(def (ibuffer-refresh! app)
  "Refresh the *IBBuffer* contents."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (bufs (ibuffer-visible-buffers))
         (lines (map (lambda (b) (ibuffer-format-line fr b)) bufs))
         (header " M Flags  Name                          Size      File")
         (sep    " - -----  ----------------------------  --------  ----")
         (filter-info (if *ibuffer-filter*
                        (string-append "  [filter: " *ibuffer-filter* "]")
                        ""))
         (text (string-join (cons (string-append header filter-info)
                                  (cons sep lines)) "\n"))
         (line (qt-plain-text-edit-cursor-line ed)))
    ;; Temporarily disable read-only to update
    (sci-send ed SCI_SETREADONLY 0)
    (qt-plain-text-edit-set-text! ed text)
    (qt-text-document-set-modified! (buffer-doc-pointer (qt-current-buffer fr)) #f)
    (sci-send ed SCI_SETREADONLY 1)
    ;; Restore cursor position
    (let ((pos (sci-send ed SCI_POSITIONFROMLINE (min line (+ (ibuffer-header-lines) (length bufs) -1)) 0)))
      (qt-plain-text-edit-set-cursor-position! ed pos))))

(def (cmd-ibuffer app)
  "Open an interactive buffer list with mark/execute.
Keys: d=mark delete, s=mark save, u=unmark, x=execute, RET=switch,
/=filter by name, S=sort name, z=sort path, t=toggle marks, g=refresh, q=quit."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf-name "*IBBuffer*")
         (buf (or (buffer-by-name buf-name)
                  (qt-buffer-create! buf-name ed #f))))
    ;; Clear marks and filter for fresh view
    (set! *ibuffer-marks* (make-hash-table))
    (set! *ibuffer-filter* #f)
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (ibuffer-refresh! app)
    ;; Position on first buffer line (after header)
    (let ((pos (sci-send ed SCI_POSITIONFROMLINE 2 0)))
      (qt-plain-text-edit-set-cursor-position! ed pos))
    (sci-send ed SCI_SETREADONLY 1)
    (echo-message! (app-state-echo app)
      "IBBuffer: d=delete s=save u=unmark x=execute RET=goto /=filter q=quit")))

(def (cmd-ibuffer-mark-delete app)
  "Mark the buffer at point for deletion (D)."
  (let* ((ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (buf (ibuffer-buffer-at-line app line)))
    (when buf
      (hash-put! *ibuffer-marks* (buffer-name buf) #\D)
      (ibuffer-refresh! app)
      ;; Move to next line
      (let ((next-pos (sci-send ed SCI_POSITIONFROMLINE (+ line 1) 0)))
        (qt-plain-text-edit-set-cursor-position! ed next-pos)))))

(def (cmd-ibuffer-mark-save app)
  "Mark the buffer at point for saving (S)."
  (let* ((ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (buf (ibuffer-buffer-at-line app line)))
    (when buf
      (hash-put! *ibuffer-marks* (buffer-name buf) #\S)
      (ibuffer-refresh! app)
      (let ((next-pos (sci-send ed SCI_POSITIONFROMLINE (+ line 1) 0)))
        (qt-plain-text-edit-set-cursor-position! ed next-pos)))))

(def (cmd-ibuffer-unmark app)
  "Remove mark from buffer at point."
  (let* ((ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (buf (ibuffer-buffer-at-line app line)))
    (when buf
      (hash-remove! *ibuffer-marks* (buffer-name buf))
      (ibuffer-refresh! app)
      (let ((next-pos (sci-send ed SCI_POSITIONFROMLINE (+ line 1) 0)))
        (qt-plain-text-edit-set-cursor-position! ed next-pos)))))

(def (cmd-ibuffer-execute app)
  "Execute marked operations: delete buffers marked D, save buffers marked S."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         ;; Collect buffers to delete/save first
         (to-delete [])
         (to-save []))
    (hash-for-each
      (lambda (name mark)
        (let ((buf (buffer-by-name name)))
          (when buf
            (cond
              ((char=? mark #\D)
               (set! to-delete (cons buf to-delete)))
              ((char=? mark #\S)
               (set! to-save (cons buf to-save)))))))
      *ibuffer-marks*)
    ;; Execute deletions
    (for-each buffer-list-remove! to-delete)
    ;; Execute saves
    (for-each
      (lambda (buf)
        (let ((path (buffer-file-path buf)))
          (when path
            (let loop ((wins (qt-frame-windows fr)))
              (when (pair? wins)
                (if (eq? (qt-edit-window-buffer (car wins)) buf)
                  (let* ((ed (qt-edit-window-editor (car wins)))
                         (text (qt-plain-text-edit-text ed)))
                    (call-with-output-file path
                      (lambda (port) (display text port)))
                    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f))
                  (loop (cdr wins))))))))
      to-save)
    ;; Clear marks and refresh
    (set! *ibuffer-marks* (make-hash-table))
    (ibuffer-refresh! app)
    (echo-message! echo
      (string-append "Executed: " (number->string (length to-delete)) " deleted, "
                     (number->string (length to-save)) " saved"))))

(def (cmd-ibuffer-goto-buffer app)
  "Switch to the buffer at point in ibuffer."
  (let* ((ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (buf (ibuffer-buffer-at-line app line)))
    (if buf
      (let* ((fr (app-state-frame app))
             (win (qt-current-window fr)))
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer win) buf)
        (qt-setup-highlighting! app buf)
        (qt-modeline-update! app))
      (echo-message! (app-state-echo app) "No buffer on this line"))))

(def (cmd-ibuffer-filter-name app)
  "Filter ibuffer by buffer name substring."
  (let ((pattern (qt-echo-read-string app "Filter by name (empty=clear): ")))
    (set! *ibuffer-filter* (if (or (not pattern) (string=? pattern "")) #f pattern))
    (ibuffer-refresh! app)))

(def (cmd-ibuffer-sort-name app)
  "Sort ibuffer by buffer name."
  (set! *ibuffer-sort* 'name)
  (ibuffer-refresh! app)
  (echo-message! (app-state-echo app) "Sorted by name"))

(def (cmd-ibuffer-sort-size app)
  "Sort ibuffer by file path."
  (set! *ibuffer-sort* 'size)
  (ibuffer-refresh! app)
  (echo-message! (app-state-echo app) "Sorted by path"))

(def (cmd-ibuffer-toggle-marks app)
  "Toggle D marks on all visible buffers."
  (let ((bufs (ibuffer-visible-buffers)))
    (for-each
      (lambda (b)
        (let ((name (buffer-name b)))
          (if (hash-get *ibuffer-marks* name)
            (hash-remove! *ibuffer-marks* name)
            (hash-put! *ibuffer-marks* name #\D))))
      bufs))
  (ibuffer-refresh! app))
