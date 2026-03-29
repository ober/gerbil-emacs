;;; -*- Gerbil -*-
;;; Additional VCS features: git-timemachine, bug-reference-mode,
;;; transpose-frame, and more.
;;; Split from editor-extra-vcs.ss to stay under 2000-line limit.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers)

;;;============================================================================
;;; Git helper (local to this module)
;;;============================================================================

(def (git-run-output args)
  "Run git command and return stdout as a string, or #f on error."
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

(def (git-run-lines args)
  "Run git command and return stdout as a list of lines."
  (let ((out (git-run-output args)))
    (if (and out (not (string=? (string-trim out) "")))
      (string-split out #\newline)
      '())))

;;;============================================================================
;;; Git Timemachine — browse file history through commits
;;;============================================================================

(def *git-timemachine-revisions* '())   ;; list of (hash . subject)
(def *git-timemachine-index* 0)         ;; current position in revisions
(def *git-timemachine-file* #f)         ;; original file path
(def *git-timemachine-active* #f)       ;; whether timemachine is active

(def (git-timemachine-get-revisions file)
  "Get list of (hash subject date author) for a file."
  (let ((lines (git-run-lines
                 (list "log" "--pretty=format:%H|%s|%ai|%an" "--follow" file))))
    (filter-map
      (lambda (line)
        (let ((parts (string-split line #\|)))
          (if (>= (length parts) 4)
            (list (car parts)           ; hash
                  (cadr parts)          ; subject
                  (caddr parts)         ; date
                  (cadddr parts))       ; author
            #f)))
      lines)))

(def (git-timemachine-show-revision app idx)
  "Display file content at revision idx."
  (when (and (>= idx 0) (< idx (length *git-timemachine-revisions*)))
    (let* ((rev (list-ref *git-timemachine-revisions* idx))
           (hash (car rev))
           (subject (cadr rev))
           (date (caddr rev))
           (author (cadddr rev))
           (content (git-run-output
                      (list "show" (string-append hash ":" *git-timemachine-file*)))))
      (when content
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (total (length *git-timemachine-revisions*))
               (buf-name (string-append "*timemachine: "
                           (path-strip-directory *git-timemachine-file*) "*")))
          (let ((buf (or (buffer-by-name buf-name)
                         (buffer-create! buf-name ed #f))))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer win) buf)
            (editor-set-read-only ed #f)
            (editor-set-text ed content)
            (editor-goto-pos ed 0)
            (editor-set-read-only ed #t)
            (set! *git-timemachine-index* idx)
            (echo-message! (app-state-echo app)
              (string-append "[" (number->string (+ idx 1)) "/"
                             (number->string total) "] "
                             (substring hash 0 (min 8 (string-length hash))) " "
                             date " " author ": " subject))))))))

(def (cmd-git-timemachine app)
  "Enter git-timemachine: browse file history through commits.
   Use n/p for next/prev revision, q to quit."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if (not file)
      (echo-error! (app-state-echo app) "Buffer has no file")
      ;; Get relative path for git show
      (let ((rel-file (let ((root (git-run-output '("rev-parse" "--show-toplevel"))))
                        (if root
                          (let ((root-trimmed (string-trim root)))
                            (if (string-prefix? root-trimmed file)
                              (let ((rel (substring file (+ (string-length root-trimmed) 1)
                                                    (string-length file))))
                                rel)
                              file))
                          file))))
        (let ((revisions (git-timemachine-get-revisions file)))
          (if (null? revisions)
            (echo-message! (app-state-echo app) "No git history for this file")
            (begin
              (set! *git-timemachine-revisions* revisions)
              (set! *git-timemachine-file* rel-file)
              (set! *git-timemachine-active* #t)
              (git-timemachine-show-revision app 0)
              (echo-message! (app-state-echo app)
                (string-append "Git timemachine: "
                  (number->string (length revisions)) " revisions. "
                  "n=next p=prev w=copy-hash q=quit")))))))))

(def (cmd-git-timemachine-next app)
  "Show next (older) revision in timemachine."
  (if (not *git-timemachine-active*)
    (echo-message! (app-state-echo app) "Not in timemachine mode")
    (let ((next-idx (+ *git-timemachine-index* 1)))
      (if (>= next-idx (length *git-timemachine-revisions*))
        (echo-message! (app-state-echo app) "Already at oldest revision")
        (git-timemachine-show-revision app next-idx)))))

(def (cmd-git-timemachine-prev app)
  "Show previous (newer) revision in timemachine."
  (if (not *git-timemachine-active*)
    (echo-message! (app-state-echo app) "Not in timemachine mode")
    (let ((prev-idx (- *git-timemachine-index* 1)))
      (if (< prev-idx 0)
        (echo-message! (app-state-echo app) "Already at newest revision")
        (git-timemachine-show-revision app prev-idx)))))

(def (cmd-git-timemachine-goto app)
  "Jump to a specific revision by number in timemachine."
  (if (not *git-timemachine-active*)
    (echo-message! (app-state-echo app) "Not in timemachine mode")
    (let ((input (app-read-string app
                   (string-append "Revision (1-"
                     (number->string (length *git-timemachine-revisions*)) "): "))))
      (when (and input (not (string=? input "")))
        (let ((n (string->number input)))
          (if (and n (>= n 1) (<= n (length *git-timemachine-revisions*)))
            (git-timemachine-show-revision app (- n 1))
            (echo-error! (app-state-echo app) "Invalid revision number")))))))

(def (cmd-git-timemachine-copy-hash app)
  "Copy the current revision's commit hash to the kill ring."
  (if (not *git-timemachine-active*)
    (echo-message! (app-state-echo app) "Not in timemachine mode")
    (let* ((rev (list-ref *git-timemachine-revisions* *git-timemachine-index*))
           (hash (car rev)))
      (set! (app-state-kill-ring app) (cons hash (app-state-kill-ring app)))
      (echo-message! (app-state-echo app)
        (string-append "Copied: " hash)))))

(def (cmd-git-timemachine-show-diff app)
  "Show diff between current timemachine revision and previous."
  (if (not *git-timemachine-active*)
    (echo-message! (app-state-echo app) "Not in timemachine mode")
    (let* ((rev (list-ref *git-timemachine-revisions* *git-timemachine-index*))
           (hash (car rev))
           (diff (git-run-output
                   (list "diff" (string-append hash "~1") hash "--"
                         *git-timemachine-file*))))
      (if (not diff)
        (echo-message! (app-state-echo app) "No diff available (first commit?)")
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (buf-name "*timemachine-diff*"))
          (let ((buf (or (buffer-by-name buf-name)
                         (buffer-create! buf-name ed #f))))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer win) buf)
            (editor-set-read-only ed #f)
            (editor-set-text ed diff)
            (editor-goto-pos ed 0)
            (editor-set-read-only ed #t)))))))

(def (cmd-git-timemachine-quit app)
  "Exit timemachine mode."
  (set! *git-timemachine-active* #f)
  (set! *git-timemachine-revisions* '())
  (set! *git-timemachine-file* #f)
  (echo-message! (app-state-echo app) "Git timemachine exited"))

(def (cmd-git-timemachine-blame app)
  "Show blame for current timemachine revision."
  (if (not *git-timemachine-active*)
    (echo-message! (app-state-echo app) "Not in timemachine mode")
    (let* ((rev (list-ref *git-timemachine-revisions* *git-timemachine-index*))
           (hash (car rev))
           (blame (git-run-output
                    (list "blame" hash "--" *git-timemachine-file*))))
      (if (not blame)
        (echo-message! (app-state-echo app) "Blame not available")
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (buf-name "*timemachine-blame*"))
          (let ((buf (or (buffer-by-name buf-name)
                         (buffer-create! buf-name ed #f))))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer win) buf)
            (editor-set-read-only ed #f)
            (editor-set-text ed blame)
            (editor-goto-pos ed 0)
            (editor-set-read-only ed #t)))))))

;;;============================================================================
;;; Bug-reference mode — highlight and navigate issue references
;;;============================================================================

(def *bug-reference-mode* #f)
(def *bug-reference-url-format*
  "https://github.com/%s/issues/%s")  ;; %s = project, %s = number
(def *bug-reference-project* #f)       ;; e.g. "user/repo"

(def (bug-reference-detect-project)
  "Auto-detect project from git remote URL."
  (let ((remote (git-run-output '("remote" "get-url" "origin"))))
    (when (and remote (not (string=? (string-trim remote) "")))
      (let ((trimmed (string-trim remote)))
        ;; Handle github.com:user/repo.git or https://github.com/user/repo.git
        (cond
          ((string-contains trimmed "github.com")
           (let* ((parts (string-split trimmed #\:))
                  (path (if (>= (length parts) 2)
                          (cadr parts)
                          (let ((idx (string-contains trimmed "github.com/")))
                            (if idx
                              (substring trimmed (+ idx 11) (string-length trimmed))
                              #f)))))
             (when path
               (let ((cleaned (if (string-suffix? ".git" path)
                               (substring path 0 (- (string-length path) 4))
                               path)))
                 (set! *bug-reference-project* cleaned)))))
          ((string-contains trimmed "gitlab.com")
           (let ((idx (string-contains trimmed "gitlab.com/")))
             (when idx
               (let* ((path (substring trimmed (+ idx 11) (string-length trimmed)))
                      (cleaned (if (string-suffix? ".git" path)
                                 (substring path 0 (- (string-length path) 4))
                                 path)))
                 (set! *bug-reference-project* cleaned)
                 (set! *bug-reference-url-format*
                   "https://gitlab.com/%s/-/issues/%s"))))))))))

(def (cmd-bug-reference-mode app)
  "Toggle bug-reference-mode — highlight #123 issue references.
   Auto-detects GitHub/GitLab project from git remote."
  (set! *bug-reference-mode* (not *bug-reference-mode*))
  (when *bug-reference-mode*
    (bug-reference-detect-project))
  (echo-message! (app-state-echo app)
    (if *bug-reference-mode*
      (if *bug-reference-project*
        (string-append "Bug-reference mode ON (project: " *bug-reference-project* ")")
        "Bug-reference mode ON (no project detected — set with M-x bug-reference-set-project)")
      "Bug-reference mode OFF")))

(def (cmd-bug-reference-set-project app)
  "Set the project identifier for bug-reference-mode (e.g. user/repo)."
  (let ((proj (app-read-string app "Project (user/repo): ")))
    (when (and proj (not (string=? proj "")))
      (set! *bug-reference-project* proj)
      (echo-message! (app-state-echo app)
        (string-append "Bug-reference project set to: " proj)))))

(def (cmd-bug-reference-set-url-format app)
  "Set the URL format for bug-reference-mode.
   Use %s for project and %s for issue number."
  (let ((fmt (app-read-string app "URL format (e.g. https://github.com/%s/issues/%s): ")))
    (when (and fmt (not (string=? fmt "")))
      (set! *bug-reference-url-format* fmt)
      (echo-message! (app-state-echo app)
        (string-append "Bug-reference URL format set")))))

(def (bug-reference-find-at-point ed)
  "Find bug reference (#NNN or GH-NNN) at or near point. Returns number string or #f."
  (let* ((pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Search backward for # or 'GH-'
    (let loop ((i pos))
      (cond
        ((< i 0) #f)
        ((and (char=? (string-ref text i) #\#)
              (< (+ i 1) len)
              (char-numeric? (string-ref text (+ i 1))))
         ;; Found #NNN pattern
         (let num-loop ((j (+ i 1)) (digits '()))
           (if (and (< j len) (char-numeric? (string-ref text j)))
             (num-loop (+ j 1) (cons (string-ref text j) digits))
             (if (null? digits) #f
               (list->string (reverse digits))))))
        ((> (- pos i) 10) #f)  ;; Don't search too far back
        (else (loop (- i 1)))))))

(def (cmd-bug-reference-goto app)
  "Open the bug/issue reference at point in an external browser."
  (if (not *bug-reference-mode*)
    (echo-message! (app-state-echo app) "Bug-reference mode is not enabled")
    (let* ((fr (app-state-frame app))
           (ed (edit-window-editor (current-window fr)))
           (num (bug-reference-find-at-point ed)))
      (if (not num)
        (echo-message! (app-state-echo app) "No bug reference at point")
        (if (not *bug-reference-project*)
          (echo-error! (app-state-echo app) "No project set — use M-x bug-reference-set-project")
          (let ((final-url (string-append
                             "https://github.com/" *bug-reference-project*
                             "/issues/" num)))
            (with-catch
              (lambda (e) (echo-error! (app-state-echo app) "Failed to open URL"))
              (lambda ()
                (open-process
                  (list path: "xdg-open" arguments: (list final-url)
                        stdout-redirection: #f stderr-redirection: #f))
                (echo-message! (app-state-echo app)
                  (string-append "Opening: " final-url))))))))))

(def (cmd-bug-reference-list app)
  "List all bug references in the current buffer."
  (let* ((fr (app-state-frame app))
         (ed (edit-window-editor (current-window fr)))
         (text (editor-get-text ed))
         (len (string-length text))
         (refs '()))
    ;; Scan for #NNN patterns
    (let loop ((i 0))
      (when (< i (- len 1))
        (when (and (char=? (string-ref text i) #\#)
                   (char-numeric? (string-ref text (+ i 1))))
          (let num-loop ((j (+ i 1)) (digits '()))
            (if (and (< j len) (char-numeric? (string-ref text j)))
              (num-loop (+ j 1) (cons (string-ref text j) digits))
              (when (not (null? digits))
                (let* ((num-str (list->string (reverse digits)))
                       (line (+ 1 (editor-line-from-position ed i))))
                  (set! refs (cons (cons num-str line) refs)))))))
        (loop (+ i 1))))
    (if (null? refs)
      (echo-message! (app-state-echo app) "No bug references found")
      (let* ((sorted (sort (lambda (a b) (< (cdr a) (cdr b))) refs))
             (unique (let dedup ((lst sorted) (seen '()) (acc '()))
                       (if (null? lst) (reverse acc)
                         (let ((num (caar lst)))
                           (if (member num seen)
                             (dedup (cdr lst) seen acc)
                             (dedup (cdr lst) (cons num seen)
                                    (cons (car lst) acc)))))))
             (lines (map (lambda (r)
                           (string-append "  #" (car r) " (line " (number->string (cdr r)) ")"))
                         unique))
             (text (string-append "Bug references in buffer:\n\n"
                     (string-join lines "\n") "\n")))
        (open-output-buffer app "*Bug References*" text)))))

;;;============================================================================
;;; Transpose-frame — flip/rotate window layouts
;;;============================================================================

(def (cmd-transpose-frame app)
  "Transpose window layout — swap horizontal and vertical splits.
   With two windows: swaps their buffers."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (< (length wins) 2)
      (echo-message! (app-state-echo app) "Only one window")
      ;; Swap buffers between all windows (rotate)
      (let* ((bufs (map edit-window-buffer wins))
             (rotated (append (cdr bufs) (list (car bufs)))))
        (for-each
          (lambda (win buf)
            (buffer-attach! (edit-window-editor win) buf)
            (set! (edit-window-buffer win) buf))
          wins rotated)
        (echo-message! (app-state-echo app)
          (string-append "Transposed " (number->string (length wins)) " windows"))))))

(def (cmd-flip-frame app)
  "Flip frame — mirror windows vertically (swap top/bottom)."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr)))
    (if (< (length wins) 2)
      (echo-message! (app-state-echo app) "Only one window")
      (let* ((bufs (map edit-window-buffer wins))
             (reversed (reverse bufs)))
        (for-each
          (lambda (win buf)
            (buffer-attach! (edit-window-editor win) buf)
            (set! (edit-window-buffer win) buf))
          wins reversed)
        (echo-message! (app-state-echo app) "Frame flipped")))))

(def (cmd-flop-frame app)
  "Flop frame — mirror windows horizontally (swap left/right).
   Alias for flip-frame in single-split layouts."
  (cmd-flip-frame app))

;;;============================================================================
;;; Git-gutter improvements — show change stats in modeline
;;;============================================================================

(def (cmd-git-diff-stat app)
  "Show git diff statistics for the current file."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if (not file)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((stat (git-run-output (list "diff" "--stat" file))))
        (if (and stat (not (string=? (string-trim stat) "")))
          (echo-message! (app-state-echo app) (string-trim stat))
          (echo-message! (app-state-echo app) "No changes"))))))

(def (cmd-git-diff-buffer app)
  "Show full git diff for the current file in a diff buffer."
  (let* ((buf (current-buffer-from-app app))
         (file (buffer-file-path buf)))
    (if (not file)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((diff (git-run-output (list "diff" file))))
        (if (and diff (not (string=? (string-trim diff) "")))
          (open-output-buffer app
            (string-append "*Diff: " (path-strip-directory file) "*")
            diff)
          (echo-message! (app-state-echo app) "No changes"))))))

;;;============================================================================
;;; Git worktree support
;;;============================================================================

(def (cmd-git-worktree-list app)
  "List all git worktrees."
  (let ((out (git-run-output '("worktree" "list"))))
    (if (not out)
      (echo-error! (app-state-echo app) "Not a git repository")
      (open-output-buffer app "*Git Worktrees*"
        (string-append "Git Worktrees:\n\n" out "\n")))))

(def (cmd-git-worktree-add app)
  "Create a new git worktree."
  (let ((path (app-read-string app "Worktree path: ")))
    (when (and path (not (string=? path "")))
      (let ((branch (app-read-string app "Branch (empty for new): ")))
        (let ((args (if (and branch (not (string=? branch "")))
                      (list "worktree" "add" path branch)
                      (list "worktree" "add" path))))
          (let ((result (git-run-output args)))
            (echo-message! (app-state-echo app)
              (string-append "Created worktree: " path))))))))

(def (cmd-git-worktree-remove app)
  "Remove a git worktree."
  (let ((path (app-read-string app "Worktree path to remove: ")))
    (when (and path (not (string=? path "")))
      (let ((result (git-run-output (list "worktree" "remove" path))))
        (echo-message! (app-state-echo app)
          (string-append "Removed worktree: " path))))))

;;;============================================================================
;;; Git submodule support
;;;============================================================================

(def (cmd-git-submodule-status app)
  "Show status of all git submodules."
  (let ((out (git-run-output '("submodule" "status"))))
    (if (or (not out) (string=? (string-trim out) ""))
      (echo-message! (app-state-echo app) "No submodules")
      (open-output-buffer app "*Git Submodules*"
        (string-append "Submodule Status:\n\n" out "\n")))))

(def (cmd-git-submodule-update app)
  "Update all git submodules."
  (let ((out (git-run-output '("submodule" "update" "--init" "--recursive"))))
    (echo-message! (app-state-echo app)
      (if out "Submodules updated" "Submodule update failed"))))

;;;============================================================================
;;; Git bisect support
;;;============================================================================

(def *git-bisect-active* #f)

(def (cmd-git-bisect-start app)
  "Start git bisect."
  (let ((bad (app-read-string app "Bad commit (default HEAD): "))
        (good (app-read-string app "Good commit: ")))
    (when (and good (not (string=? good "")))
      (let* ((bad-ref (if (or (not bad) (string=? bad "")) "HEAD" bad))
             (result (git-run-output (list "bisect" "start" bad-ref good))))
        (set! *git-bisect-active* #t)
        (echo-message! (app-state-echo app)
          (or result "Bisect started"))))))

(def (cmd-git-bisect-good app)
  "Mark current commit as good in bisect."
  (if (not *git-bisect-active*)
    (echo-message! (app-state-echo app) "No bisect in progress")
    (let ((out (git-run-output '("bisect" "good"))))
      (echo-message! (app-state-echo app) (or out "Bisect: good")))))

(def (cmd-git-bisect-bad app)
  "Mark current commit as bad in bisect."
  (if (not *git-bisect-active*)
    (echo-message! (app-state-echo app) "No bisect in progress")
    (let ((out (git-run-output '("bisect" "bad"))))
      (echo-message! (app-state-echo app) (or out "Bisect: bad")))))

(def (cmd-git-bisect-reset app)
  "End git bisect and return to original HEAD."
  (let ((out (git-run-output '("bisect" "reset"))))
    (set! *git-bisect-active* #f)
    (echo-message! (app-state-echo app) (or out "Bisect reset"))))

(def (cmd-git-bisect-log app)
  "Show git bisect log."
  (if (not *git-bisect-active*)
    (echo-message! (app-state-echo app) "No bisect in progress")
    (let ((out (git-run-output '("bisect" "log"))))
      (if out
        (open-output-buffer app "*Bisect Log*" out)
        (echo-message! (app-state-echo app) "No bisect log")))))

;;;============================================================================
;;; Git shortlog (contributor statistics)
;;;============================================================================

(def (cmd-git-shortlog app)
  "Show contributor summary (git shortlog)."
  (let ((out (git-run-output '("shortlog" "-sn" "--all"))))
    (if (not out)
      (echo-error! (app-state-echo app) "Not a git repository")
      (open-output-buffer app "*Git Contributors*"
        (string-append "Contributors (commits):\n\n" out "\n")))))

;;;============================================================================
;;; Git reflog
;;;============================================================================

(def (cmd-git-reflog app)
  "Show git reflog in a buffer."
  (let ((out (git-run-output '("reflog" "--oneline" "-30"))))
    (if (not out)
      (echo-error! (app-state-echo app) "Not a git repository")
      (open-output-buffer app "*Git Reflog*"
        (string-append "Recent Reflog (last 30):\n\n" out "\n")))))
