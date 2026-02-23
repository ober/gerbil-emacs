;;; -*- Gerbil -*-
;;; Qt parity commands — new feature implementations matching TUI layer.
;;; Chain position: after commands-config, before facade.

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
                 org-babel-tangle-to-files org-babel-insert-result)
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
            (let ((output (org-babel-execute lang body header-args)))
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

;;; --- Org archive subtree (stub) ---
(def (cmd-org-archive-subtree app)
  "Archive the current org subtree."
  (echo-message! (app-state-echo app) "Archive subtree: not yet implemented"))

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
  "Create a git tag."
  (let ((tag (qt-echo-read-string (app-state-echo app) "Tag name: ")))
    (when (and tag (not (string=? tag "")))
      (with-catch
        (lambda (e) (echo-message! (app-state-echo app) "Tag failed"))
        (lambda ()
          (run-process ["git" "tag" tag] coprocess: void)
          (echo-message! (app-state-echo app)
            (string-append "Created tag: " tag)))))))

;;;============================================================================
;;; Batch 4: check-parens, count-lines-page, how-many
;;;============================================================================

;;; --- Text mode ---
(def (cmd-text-mode app)
  "Switch to text mode."
  (echo-message! (app-state-echo app) "Text mode active"))

;;; --- Shell script mode ---
(def (cmd-shell-script-mode app)
  "Switch to shell script mode."
  (echo-message! (app-state-echo app) "Shell script mode active"))

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

(def (cmd-flyspell-mode app)
  "Toggle flyspell mode: check buffer for misspelled words."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (if *qt-flyspell-active*
      (begin
        (set! *qt-flyspell-active* #f)
        (echo-message! (app-state-echo app) "Flyspell mode OFF"))
      (begin
        (set! *qt-flyspell-active* #t)
        (let* ((words (qt-flyspell-extract-words text))
               (misspelled-words '()))
          (for-each
            (lambda (entry)
              (let ((word (car entry)))
                (when (> (string-length word) 1)
                  (let ((suggestions (qt-aspell-check-word word)))
                    (when suggestions
                      (set! misspelled-words (cons word misspelled-words)))))))
            words)
          (let ((count (length misspelled-words)))
            (if (= count 0)
              (echo-message! (app-state-echo app)
                (string-append "Flyspell: no misspelled words (" (number->string (length words)) " checked)"))
              (echo-message! (app-state-echo app)
                (string-append "Flyspell: " (number->string count) " misspelled — "
                               (string-join (take-n (reverse misspelled-words) 5) ", ")
                               (if (> count 5) "..." ""))))))))))

(def (take-n lst n)
  "Take first N elements of list."
  (let loop ((l lst) (i 0) (acc '()))
    (if (or (null? l) (>= i n))
      (reverse acc)
      (loop (cdr l) (+ i 1) (cons (car l) acc)))))
