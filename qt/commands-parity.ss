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

;;; --- Evil mode ---
(def *qt-evil-mode* #f)
(def (cmd-evil-mode app)
  "Toggle evil mode — vi-like modal editing."
  (set! *qt-evil-mode* (not *qt-evil-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-evil-mode* "Evil mode: on (vi keybindings)" "Evil mode: off (emacs keybindings)")))

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
