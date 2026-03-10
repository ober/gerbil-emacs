;;; -*- Gerbil -*-
;;; Qt commands shell2 - sudo save, ediff, mode toggles, xref, eldoc, project, diff hunks
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        (only-in :gemacs/persist theme-settings-save! theme-settings-load!
                 mx-history-save! mx-history-load!)
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
        (only-in :gemacs/qt/magit magit-run-git)
        :gemacs/qt/commands-core
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-file
        :gemacs/qt/commands-file2
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-sexp2
        :gemacs/qt/commands-ide
        :gemacs/qt/commands-ide2
        :gemacs/qt/commands-vcs
        :gemacs/qt/commands-vcs2
        :gemacs/qt/lsp-client
        :gemacs/qt/commands-lsp
        :gemacs/qt/commands-shell)

;;;============================================================================
;;; Sudo save

(def (cmd-sudo-save-buffer app)
  "Save current buffer using sudo (for editing system files)."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)))
        (with-catch
          (lambda (e)
            (echo-error! (app-state-echo app) "Sudo save failed"))
          (lambda ()
            (let ((tmp (string-append "/tmp/.gemacs-sudo-"
                         (number->string (random-integer 999999)))))
              (call-with-output-file tmp
                (lambda (port) (display text port)))
              (let ((p (open-process
                         (list path: "/usr/bin/sudo"
                               arguments: (list "cp" tmp path)
                               stdout-redirection: #f
                               stderr-redirection: #f))))
                (process-status p)
                (close-port p))
              (let ((p2 (open-process
                          (list path: "/bin/rm"
                                arguments: (list "-f" tmp)
                                stdout-redirection: #f
                                stderr-redirection: #f))))
                (process-status p2)
                (close-port p2))
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (echo-message! (app-state-echo app)
                (string-append "Sudo saved: " path)))))))))

;;;============================================================================
;;; Ediff: directories, merge, regions

(def (cmd-ediff-directories app)
  "Compare two directories using diff."
  (let* ((dir1 (qt-echo-read-string app "First directory: "))
         (dir2 (and dir1 (> (string-length dir1) 0)
                    (qt-echo-read-string app "Second directory: "))))
    (when (and dir2 (> (string-length dir2) 0))
      (if (not (and (directory-exists? dir1) (directory-exists? dir2)))
        (echo-error! (app-state-echo app) "One or both directories do not exist")
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "diff failed"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "diff"
                                 arguments: (list "-rq" dir1 dir2)
                                 stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (output (read-line proc #f))
                   (_ (close-port proc))
                   (ed (current-qt-editor app))
                   (fr (app-state-frame app))
                   (buf (or (buffer-by-name "*Ediff Dirs*")
                            (qt-buffer-create! "*Ediff Dirs*" ed #f))))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed
                (string-append "Directory comparison: " dir1 " vs " dir2 "\n"
                               (make-string 60 #\=) "\n\n"
                               (or output "Directories are identical")))
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (qt-plain-text-edit-set-cursor-position! ed 0))))))))

(def (cmd-ediff-merge app)
  "Three-way merge using diff3. Prompts for my file, base, and their file."
  (let* ((file-mine (qt-echo-read-string app "My file: "))
         (file-base (and file-mine (> (string-length file-mine) 0)
                         (qt-echo-read-string app "Base (ancestor) file: ")))
         (file-theirs (and file-base (> (string-length file-base) 0)
                           (qt-echo-read-string app "Their file: "))))
    (when (and file-theirs (> (string-length file-theirs) 0))
      (if (not (and (file-exists? file-mine) (file-exists? file-base)
                    (file-exists? file-theirs)))
        (echo-error! (app-state-echo app) "One or more files do not exist")
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "diff3 failed"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "diff3"
                                 arguments: (list "-m" file-mine file-base file-theirs)
                                 stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (output (read-line proc #f))
                   (status (process-status proc))
                   (_ (close-port proc))
                   (ed (current-qt-editor app))
                   (fr (app-state-frame app))
                   (buf (or (buffer-by-name "*Ediff Merge*")
                            (qt-buffer-create! "*Ediff Merge*" ed #f))))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed
                (string-append "Three-way merge: " file-mine " + " file-base " + " file-theirs "\n"
                               (make-string 60 #\=) "\n"
                               (if (= status 0) "No conflicts.\n\n"
                                 "Conflicts marked with <<<<<<< / ======= / >>>>>>>.\nUse smerge-keep-mine / smerge-keep-other to resolve.\n\n")
                               (or output "Files are identical")))
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (qt-plain-text-edit-set-cursor-position! ed 0)
              (qt-highlight-diff! ed))))))))

(def (qt-diff-refine-words old-line new-line)
  "Compute word-level diff between two lines. Returns annotated string."
  (let* ((old-words (string-split old-line #\space))
         (new-words (string-split new-line #\space))
         (old-len (length old-words))
         (new-len (length new-words))
         (prefix-len (let loop ((i 0))
                       (if (and (< i old-len) (< i new-len)
                                (string=? (list-ref old-words i) (list-ref new-words i)))
                         (loop (+ i 1)) i)))
         (suffix-len (let loop ((i 0))
                       (if (and (< (+ prefix-len i) old-len)
                                (< (+ prefix-len i) new-len)
                                (string=? (list-ref old-words (- old-len 1 i))
                                          (list-ref new-words (- new-len 1 i))))
                         (loop (+ i 1)) i)))
         (prefix (let take-n ((lst old-words) (n prefix-len) (acc []))
                   (if (= n 0) (reverse acc) (take-n (cdr lst) (- n 1) (cons (car lst) acc)))))
         (old-mid-len (max 0 (- old-len prefix-len suffix-len)))
         (new-mid-len (max 0 (- new-len prefix-len suffix-len)))
         (old-mid (let take-n ((lst (list-tail old-words prefix-len)) (n old-mid-len) (acc []))
                    (if (= n 0) (reverse acc) (take-n (cdr lst) (- n 1) (cons (car lst) acc)))))
         (new-mid (let take-n ((lst (list-tail new-words prefix-len)) (n new-mid-len) (acc []))
                    (if (= n 0) (reverse acc) (take-n (cdr lst) (- n 1) (cons (car lst) acc)))))
         (suffix (list-tail old-words (+ prefix-len old-mid-len))))
    (let ((out (open-output-string)))
      (unless (null? prefix)
        (display (string-join prefix " ") out)
        (display " " out))
      (unless (null? old-mid)
        (display "[-" out)
        (display (string-join old-mid " ") out)
        (display "-] " out))
      (unless (null? new-mid)
        (display "{+" out)
        (display (string-join new-mid " ") out)
        (display "+} " out))
      (unless (null? suffix)
        (display (string-join suffix " ") out))
      (string-trim-right (get-output-string out)))))

(def (cmd-diff-refine-hunk app)
  "Refine the current diff hunk with word-level diff annotations."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (current-line-num (let loop ((i 0) (cpos 0))
                             (if (>= i (length lines)) (- (length lines) 1)
                               (let ((line-len (+ (string-length (list-ref lines i)) 1)))
                                 (if (> (+ cpos line-len) pos) i
                                   (loop (+ i 1) (+ cpos line-len)))))))
         (hunk-start (let loop ((i current-line-num))
                       (cond ((< i 0) #f)
                             ((string-prefix? "@@" (list-ref lines i)) i)
                             (else (loop (- i 1))))))
         (hunk-end (if (not hunk-start) (length lines)
                     (let loop ((i (+ hunk-start 1)))
                       (cond ((>= i (length lines)) i)
                             ((string-prefix? "@@" (list-ref lines i)) i)
                             (else (loop (+ i 1))))))))
    (if (not hunk-start)
      (echo-error! (app-state-echo app) "No hunk at point")
      (let refine ((i (+ hunk-start 1)) (out (open-output-string)) (refined 0))
        (cond
          ((>= i hunk-end)
           (if (= refined 0)
             (echo-message! (app-state-echo app) "No paired changes to refine in this hunk")
             (let* ((result (get-output-string out))
                    (buf (or (buffer-by-name "*Refined Hunk*")
                             (qt-buffer-create! "*Refined Hunk*" ed #f)))
                    (display-text (string-append (list-ref lines hunk-start) "\n" result)))
               (qt-buffer-attach! ed buf)
               (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
               (qt-plain-text-edit-set-text! ed display-text)
               (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
               (qt-plain-text-edit-set-cursor-position! ed 0)
               (echo-message! (app-state-echo app)
                 (string-append "Refined " (number->string refined) " line pair(s)")))))
          ((and (< (+ i 1) hunk-end)
                (string-prefix? "-" (list-ref lines i))
                (not (string-prefix? "---" (list-ref lines i)))
                (string-prefix? "+" (list-ref lines (+ i 1)))
                (not (string-prefix? "+++" (list-ref lines (+ i 1)))))
           (let* ((old-line (substring (list-ref lines i) 1 (string-length (list-ref lines i))))
                  (new-line (substring (list-ref lines (+ i 1)) 1 (string-length (list-ref lines (+ i 1)))))
                  (refined-text (qt-diff-refine-words old-line new-line)))
             (display (string-append "  " refined-text "\n") out)
             (refine (+ i 2) out (+ refined 1))))
          (else
           (display (string-append (list-ref lines i) "\n") out)
           (refine (+ i 1) out refined)))))))

(def (cmd-ediff-regions app)
  "Compare current buffer with another buffer."
  (let* ((cur-buf (current-qt-buffer app))
         (cur-name (buffer-name cur-buf))
         (other-name (qt-echo-read-string app "Compare with buffer: ")))
    (when (and other-name (> (string-length other-name) 0))
      (let ((other-buf (buffer-by-name other-name)))
        (if (not other-buf)
          (echo-error! (app-state-echo app) (string-append "Buffer not found: " other-name))
          (let* ((ed (current-qt-editor app))
                 (text1 (qt-plain-text-edit-text ed))
                 (tmp1 "/tmp/gemacs-ediff-1.txt")
                 (tmp2 "/tmp/gemacs-ediff-2.txt"))
            (call-with-output-file tmp1 (lambda (p) (display text1 p)))
            ;; Get other buffer text by temporarily switching
            (qt-buffer-attach! ed other-buf)
            (let ((text2 (qt-plain-text-edit-text ed)))
              (call-with-output-file tmp2 (lambda (p) (display text2 p)))
              ;; Switch back
              (qt-buffer-attach! ed cur-buf)
              (with-catch
                (lambda (e) (echo-error! (app-state-echo app) "diff failed"))
                (lambda ()
                  (let* ((proc (open-process
                                 (list path: "diff"
                                       arguments: (list "-u"
                                                    (string-append "--label=" cur-name)
                                                    (string-append "--label=" other-name)
                                                    tmp1 tmp2)
                                       stdout-redirection: #t
                                       stderr-redirection: #t)))
                         (output (read-line proc #f))
                         (_ (close-port proc))
                         (fr (app-state-frame app))
                         (diff-buf (or (buffer-by-name "*Ediff Regions*")
                                       (qt-buffer-create! "*Ediff Regions*" ed #f))))
                    (qt-buffer-attach! ed diff-buf)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) diff-buf)
                    (qt-plain-text-edit-set-text! ed
                      (or output "Buffers are identical"))
                    (qt-text-document-set-modified! (buffer-doc-pointer diff-buf) #f)
                    (qt-plain-text-edit-set-cursor-position! ed 0)
                    (qt-highlight-diff! ed)))))))))))

;;;============================================================================
;;; Mode toggles (Emacs compatibility aliases)

;; *qt-show-paren-enabled* and *qt-delete-selection-enabled* are defined in highlight.ss
;; and imported through the commands chain

(def (cmd-show-paren-mode app)
  "Toggle show-paren-mode (bracket matching highlight). Enabled by default.
When disabled, cursor-adjacent braces are no longer highlighted."
  (set! *qt-show-paren-enabled* (not *qt-show-paren-enabled*))
  ;; Force visual update to immediately show/hide brace highlights
  (qt-update-visual-decorations! (qt-current-editor (app-state-frame app)))
  (echo-message! (app-state-echo app)
    (if *qt-show-paren-enabled* "Show paren mode enabled" "Show paren mode disabled")))

(def (cmd-delete-selection-mode app)
  "Toggle delete-selection-mode (typed text replaces selection). Default on.
When enabled, typing while region is active replaces the selected text."
  (set! *qt-delete-selection-enabled* (not *qt-delete-selection-enabled*))
  (echo-message! (app-state-echo app)
    (if *qt-delete-selection-enabled* "Delete selection mode enabled" "Delete selection mode disabled")))

;;;============================================================================
;;; Xref additions: find-apropos, go-forward

(def *xref-forward-stack* [])

(def (cmd-xref-find-apropos app)
  "Find symbols matching a prompted pattern in project."
  (let ((pattern (qt-echo-read-string app "Find symbol matching: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((root (current-project-root app))
             (proc (open-process
                     (list path: "/usr/bin/grep"
                           arguments: (list "-rn" pattern
                             "--include=*.ss" "--include=*.scm"
                             root)
                           stdout-redirection: #t
                           stderr-redirection: #t)))
             (output (read-line proc #f))
             (_ (close-port proc)))
        (if (not output)
          (echo-error! (app-state-echo app) (string-append "No matches: " pattern))
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app))
                 (buf (or (buffer-by-name "*Xref Apropos*")
                          (qt-buffer-create! "*Xref Apropos*" ed #f))))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-text! ed
              (string-append "Symbols matching: " pattern "\n\n" output))
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)))))))

(def (cmd-xref-go-back app)
  "Go back in xref history (alias for xref-back)."
  (cmd-xref-back app))

(def (cmd-xref-go-forward app)
  "Go forward in xref history."
  (if (null? *xref-forward-stack*)
    (echo-error! (app-state-echo app) "No forward xref history")
    (let* ((loc (car *xref-forward-stack*))
           (path-or-name (car loc))
           (pos (cdr loc))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      ;; Save current location for back
      (xref-push-location! app)
      (set! *xref-forward-stack* (cdr *xref-forward-stack*))
      ;; Navigate to forward location
      (let ((buf (or (let loop ((bufs *buffer-list*))
                       (if (null? bufs) #f
                         (let ((b (car bufs)))
                           (if (and (buffer-file-path b)
                                    (string=? (buffer-file-path b) path-or-name))
                             b (loop (cdr bufs))))))
                     (buffer-by-name path-or-name))))
        (when buf
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-cursor-position! ed pos)
          (qt-plain-text-edit-ensure-cursor-visible! ed)))
      (echo-message! (app-state-echo app) "Xref: forward"))))

;;;============================================================================
;;; Eldoc mode toggle (wired to the real *eldoc-mode* flag in commands-vcs)

(def (cmd-eldoc-mode app)
  "Toggle eldoc mode — shows function signatures in echo area on idle.
When enabled, displays the signature of the enclosing function for
Scheme/Gerbil/Lisp buffers. Also used by LSP for hover information."
  (set! *eldoc-mode* (not *eldoc-mode*))
  (echo-message! (app-state-echo app)
    (if *eldoc-mode* "Eldoc mode enabled" "Eldoc mode disabled")))

(def (cmd-toggle-global-eldoc app)
  "Toggle global eldoc mode."
  (cmd-eldoc-mode app))

;;;============================================================================
;;; Project: eshell, shell, find-regexp

(def (cmd-project-find-regexp app)
  "Search project files for a regexp using grep."
  (let* ((root (current-project-root app))
         (pattern (qt-echo-read-string app "Project grep: ")))
    (when (and pattern (> (string-length pattern) 0))
      (if (not root)
        (echo-error! (app-state-echo app) "Not in a project")
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "grep failed"))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "/usr/bin/grep"
                                 arguments: (list "-rn" pattern root
                                   "--include=*.ss" "--include=*.scm"
                                   "--include=*.py" "--include=*.js"
                                   "--include=*.go" "--include=*.rs"
                                   "--include=*.c" "--include=*.h"
                                   "--include=*.cpp" "--include=*.hpp"
                                   "--include=*.md" "--include=*.txt")
                                 stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (output (read-line proc #f))
                   (_ (close-port proc))
                   (ed (current-qt-editor app))
                   (fr (app-state-frame app))
                   (buf (or (buffer-by-name (string-append "*Project grep: " pattern "*"))
                            (qt-buffer-create! (string-append "*Project grep: " pattern "*") ed #f))))
              (qt-buffer-attach! ed buf)
              (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
              (qt-plain-text-edit-set-text! ed
                (if output
                  (string-append "Project grep: " pattern "\n\n" output)
                  "No matches found."))
              (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
              (qt-plain-text-edit-set-cursor-position! ed 0))))))))

(def (cmd-project-shell app)
  "Open shell in project root."
  (let ((root (current-project-root app)))
    (if (not root)
      (echo-error! (app-state-echo app) "Not in a project")
      (begin
        (current-directory root)
        (cmd-shell app)
        (echo-message! (app-state-echo app) (string-append "Shell in: " root))))))

(def (cmd-project-eshell app)
  "Open eshell in project root."
  (let ((root (current-project-root app)))
    (if (not root)
      (echo-error! (app-state-echo app) "Not in a project")
      (begin
        (current-directory root)
        (cmd-eshell app)
        (echo-message! (app-state-echo app) (string-append "Eshell in: " root))))))

;;;============================================================================
;;; Diff hunk operations

(def (qt-diff-find-current-hunk ed)
  "Find the @@ line number for the hunk at cursor position."
  (let* ((text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline)))
    ;; Find which line we're on
    (let loop ((ls lines) (line-idx 0) (char-count 0))
      (if (null? ls) #f
        (let ((line-len (+ (string-length (car ls)) 1)))
          (if (>= (+ char-count line-len) pos)
            ;; Found current line; scan backward for @@
            (let scan ((i line-idx))
              (cond
                ((< i 0) #f)
                ((string-prefix? "@@" (list-ref lines i)) i)
                (else (scan (- i 1)))))
            (loop (cdr ls) (+ line-idx 1) (+ char-count line-len))))))))

(def (cmd-diff-mode app)
  "Show diff summary: hunks, additions, deletions."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (additions (length (filter (lambda (l) (and (> (string-length l) 0) (char=? (string-ref l 0) #\+))) lines)))
         (deletions (length (filter (lambda (l) (and (> (string-length l) 0) (char=? (string-ref l 0) #\-))) lines)))
         (hunks (length (filter (lambda (l) (string-prefix? "@@" l)) lines))))
    (echo-message! (app-state-echo app)
      (string-append "Diff: " (number->string hunks) " hunk(s), +"
                     (number->string additions) "/-" (number->string deletions) " lines"))))

(def (cmd-diff-apply-hunk app)
  "Apply the current diff hunk (dry-run via patch --dry-run)."
  (let* ((ed (current-qt-editor app))
         (hunk-line (qt-diff-find-current-hunk ed)))
    (if (not hunk-line)
      (echo-error! (app-state-echo app) "Not in a diff hunk")
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        ;; Extract hunk content
        (let loop ((i hunk-line) (acc []))
          (if (>= i (length lines))
            (let* ((hunk-text (string-join (reverse acc) "\n"))
                   (tmp "/tmp/gemacs-hunk.patch"))
              (with-catch
                (lambda (e) (echo-error! (app-state-echo app) "Failed to apply hunk"))
                (lambda ()
                  (call-with-output-file tmp (lambda (p) (display hunk-text p)))
                  (let* ((proc (open-process
                                 (list path: "patch"
                                       arguments: (list "-p1" "--dry-run" "-i" tmp)
                                       stdout-redirection: #t
                                       stderr-redirection: #t)))
                         (out (read-line proc #f))
                         (_ (close-port proc)))
                    (echo-message! (app-state-echo app)
                      (string-append "Patch: " (or out "ok")))))))
            (let ((line (list-ref lines i)))
              (if (and (> i hunk-line) (string-prefix? "@@" line))
                (loop (length lines) acc)
                (loop (+ i 1) (cons line acc))))))))))

(def (cmd-diff-revert-hunk app)
  "Revert the current diff hunk (reverse patch --dry-run)."
  (let* ((ed (current-qt-editor app))
         (hunk-line (qt-diff-find-current-hunk ed)))
    (if (not hunk-line)
      (echo-error! (app-state-echo app) "Not in a diff hunk")
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline)))
        (let loop ((i hunk-line) (acc []))
          (if (>= i (length lines))
            (let* ((hunk-text (string-join (reverse acc) "\n"))
                   (tmp "/tmp/gemacs-revert-hunk.patch"))
              (with-catch
                (lambda (e) (echo-error! (app-state-echo app) "Failed to revert hunk"))
                (lambda ()
                  (call-with-output-file tmp (lambda (p) (display hunk-text p)))
                  (let* ((proc (open-process
                                 (list path: "patch"
                                       arguments: (list "-p1" "-R" "--dry-run" "-i" tmp)
                                       stdout-redirection: #t
                                       stderr-redirection: #t)))
                         (out (read-line proc #f))
                         (_ (close-port proc)))
                    (echo-message! (app-state-echo app)
                      (string-append "Reverted: " (or out "ok")))))))
            (let ((line (list-ref lines i)))
              (if (and (> i hunk-line) (string-prefix? "@@" line))
                (loop (length lines) acc)
                (loop (+ i 1) (cons line acc))))))))))

;;;============================================================================
;;; File/buffer utilities

(def *qt-new-buffer-counter* 0)

(def (cmd-copy-buffer-file-name app)
  "Copy the full file path of the current buffer to kill ring."
  (let* ((buf (current-qt-buffer app))
         (filepath (buffer-file-path buf)))
    (if (not filepath)
      (echo-message! (app-state-echo app) "Buffer has no file")
      (begin
        (qt-kill-ring-push! app filepath)
        (echo-message! (app-state-echo app) (string-append "Copied: " filepath))))))

(def (cmd-new-empty-buffer app)
  "Create a new empty buffer with a unique name."
  (set! *qt-new-buffer-counter* (+ *qt-new-buffer-counter* 1))
  (let* ((name (string-append "*new-" (number->string *qt-new-buffer-counter*) "*"))
         (ed (current-qt-editor app))
         (fr (app-state-frame app))
         (buf (qt-buffer-create! name ed #f)))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed "")
    (echo-message! (app-state-echo app) (string-append "New buffer: " name))))

(def (cmd-git-log-file app)
  "Show git log for the current file."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if (not path)
      (echo-error! (app-state-echo app) "Buffer has no file")
      (let* ((dir (path-directory path))
             (output (magit-run-git (list "log" "--oneline" "--follow" "-30" path) dir))
             (ed (current-qt-editor app))
             (fr (app-state-frame app))
             (log-buf (or (buffer-by-name (string-append "*Log: " (path-strip-directory path) "*"))
                          (qt-buffer-create! (string-append "*Log: " (path-strip-directory path) "*") ed #f))))
        (qt-buffer-attach! ed log-buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) log-buf)
        (qt-plain-text-edit-set-text! ed
          (string-append "File: " path "\n\n" (if (string=? output "") "Not tracked\n" output)))
        (qt-text-document-set-modified! (buffer-doc-pointer log-buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

(def (cmd-switch-buffer-mru app)
  "Switch to most recently used buffer (excluding current)."
  (let* ((cur-buf (current-qt-buffer app))
         (cur-name (buffer-name cur-buf))
         (bufs (filter (lambda (b) (not (string=? (buffer-name b) cur-name))) *buffer-list*)))
    (if (null? bufs)
      (echo-message! (app-state-echo app) "No other buffers")
      (let* ((target (car bufs))
             (ed (current-qt-editor app))
             (fr (app-state-frame app)))
        (qt-buffer-attach! ed target)
        (set! (qt-edit-window-buffer (qt-current-window fr)) target)
        (echo-message! (app-state-echo app) (string-append "Buffer: " (buffer-name target)))))))

(def (cmd-find-file-ssh app)
  "Open file via SSH using scp."
  (let ((path (qt-echo-read-string app "SSH path (user@host:/path): ")))
    (when (and path (> (string-length path) 0))
      (echo-message! (app-state-echo app) (string-append "Fetching: " path))
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "SSH fetch failed"))
        (lambda ()
          (let* ((tmp (string-append "/tmp/gemacs-ssh-" (number->string (random-integer 99999))))
                 (proc (open-process
                         (list path: "scp"
                               arguments: (list path tmp)
                               stdout-redirection: #t
                               stderr-redirection: #t)))
                 (output (read-line proc #f))
                 (status (process-status proc)))
            (close-port proc)
            (if (and (= status 0) (file-exists? tmp))
              (let* ((content (read-file-as-string tmp))
                     (buf-name (string-append "[SSH] " path))
                     (ed (current-qt-editor app))
                     (fr (app-state-frame app))
                     (buf (qt-buffer-create! buf-name ed #f)))
                (qt-buffer-attach! ed buf)
                (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                (qt-plain-text-edit-set-text! ed content)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
                (qt-plain-text-edit-set-cursor-position! ed 0)
                (qt-setup-highlighting! app buf)
                (echo-message! (app-state-echo app) (string-append "Loaded: " path)))
              (echo-error! (app-state-echo app) "SCP failed or file empty"))))))))

;;; ============================================================
;;; Hungry delete — delete all consecutive whitespace
;;; ============================================================

(def (cmd-hungry-delete-forward app)
  "Delete all consecutive whitespace ahead of point."
  (let* ((ed (qt-current-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let loop ((i pos))
        (if (or (>= i len)
                (not (char-whitespace? (string-ref text i))))
          (if (> i pos)
            (begin
              (sci-send ed SCI_SETTARGETSTART pos)
              (sci-send ed SCI_SETTARGETEND i)
              (sci-send/string ed SCI_REPLACETARGET ""))
            (sci-send ed 2180)) ;; SCI_CLEAR
          (loop (+ i 1)))))))

(def (cmd-hungry-delete-backward app)
  "Delete all consecutive whitespace behind point."
  (let* ((ed (qt-current-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (if (<= pos 0)
      (echo-message! (app-state-echo app) "Beginning of buffer")
      (let loop ((i (- pos 1)))
        (if (or (< i 0)
                (not (char-whitespace? (string-ref text i))))
          (let ((del-start (+ i 1)))
            (if (< del-start pos)
              (begin
                (sci-send ed SCI_SETTARGETSTART del-start)
                (sci-send ed SCI_SETTARGETEND pos)
                (sci-send/string ed SCI_REPLACETARGET ""))
              (sci-send ed 2326))) ;; SCI_DELETEBACK
          (loop (- i 1)))))))

;;; ============================================================
;;; ws-butler — trim trailing whitespace only on changed lines
;;; ============================================================

(def *qt-ws-butler-mode* #f)
(def *qt-ws-butler-original-lines* (make-hash-table))

(def (cmd-ws-butler-mode app)
  "Toggle ws-butler mode: trim trailing whitespace only on modified lines when saving."
  (set! *qt-ws-butler-mode* (not *qt-ws-butler-mode*))
  (when *qt-ws-butler-mode*
    (qt-ws-butler-snapshot! app))
  (echo-message! (app-state-echo app)
    (if *qt-ws-butler-mode*
      "ws-butler mode ON (trim whitespace on changed lines when saving)"
      "ws-butler mode OFF")))

(def (qt-ws-butler-snapshot! app)
  "Save snapshot of current buffer's lines."
  (let* ((ed (qt-current-editor app))
         (buf (qt-edit-window-buffer (qt-current-window (app-state-frame app))))
         (buf-name (if buf (buffer-name buf) "*scratch*"))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (tbl (make-hash-table)))
    (let loop ((ls lines) (n 0))
      (unless (null? ls)
        (hash-put! tbl n (car ls))
        (loop (cdr ls) (+ n 1))))
    (hash-put! *qt-ws-butler-original-lines* buf-name tbl)))

(def (qt-ws-butler-clean! app)
  "Trim trailing whitespace only on lines that changed since last snapshot."
  (when *qt-ws-butler-mode*
    (let* ((ed (qt-current-editor app))
           (buf (qt-edit-window-buffer (qt-current-window (app-state-frame app))))
           (buf-name (if buf (buffer-name buf) "*scratch*"))
           (original (hash-get *qt-ws-butler-original-lines* buf-name))
           (text (qt-plain-text-edit-text ed))
           (lines (string-split text #\newline))
           (cleaned #f))
      (let loop ((ls lines) (n 0) (acc []))
        (if (null? ls)
          (when cleaned
            (let ((result (string-join (reverse acc) "\n"))
                  (pos (qt-plain-text-edit-cursor-position ed)))
              (qt-plain-text-edit-set-text! ed result)
              (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length result)))))
          (let* ((line (car ls))
                 (orig-line (and original (hash-get original n)))
                 (changed? (not (equal? line orig-line)))
                 (trimmed (if (and changed?
                                   (> (string-length line) 0)
                                   (char-whitespace? (string-ref line (- (string-length line) 1))))
                            (begin (set! cleaned #t)
                                   (string-trim-right line))
                            line)))
            (loop (cdr ls) (+ n 1) (cons trimmed acc))))))))

;;; ============================================================
;;; crux-move-beginning-of-line — smart BOL toggle
;;; ============================================================

(def (cmd-crux-move-beginning-of-line app)
  "Smart beginning-of-line: toggle between first non-whitespace and column 0."
  (let* ((ed (qt-current-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         ;; Find start of current line
         (line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1)
                         (loop (- i 1)))))
         ;; Find first non-whitespace on this line
         (first-nonws
           (let loop ((i line-start))
             (if (or (>= i len)
                     (char=? (string-ref text i) #\newline))
               i
               (if (char-whitespace? (string-ref text i))
                 (loop (+ i 1))
                 i)))))
    (if (= pos first-nonws)
      (qt-plain-text-edit-set-cursor-position! ed line-start)
      (qt-plain-text-edit-set-cursor-position! ed first-nonws))))

;;; ============================================================
;;; Isearch match count (anzu-style)
;;; ============================================================

(def (qt-count-search-matches ed pattern)
  "Count total occurrences of pattern in buffer."
  (let* ((text (qt-plain-text-edit-text ed))
         (plen (string-length pattern)))
    (if (<= plen 0) 0
      (let loop ((start 0) (count 0))
        (let ((pos (string-contains text pattern start)))
          (if pos (loop (+ pos 1) (+ count 1)) count))))))

(def (qt-current-match-index ed pattern pos)
  "Return 1-based match index at position."
  (let* ((text (qt-plain-text-edit-text ed))
         (plen (string-length pattern)))
    (if (<= plen 0) 0
      (let loop ((start 0) (n 1))
        (let ((found (string-contains text pattern start)))
          (if (not found) 0
            (if (= found pos) n
              (loop (+ found 1) (+ n 1)))))))))

(def (qt-isearch-count-message ed pattern pos)
  "Return '[3/15]' for current isearch position."
  (let ((total (qt-count-search-matches ed pattern))
        (current (qt-current-match-index ed pattern pos)))
    (if (> total 0)
      (string-append "[" (number->string current) "/" (number->string total) "]")
      "[0/0]")))

;;;============================================================================
;;; Elfeed — RSS/Atom feed reader (Qt)
;;;============================================================================

(def *qt-elfeed-feeds* '())
(def *qt-elfeed-entries* '())

(def (qt-elfeed-db-path)
  (let ((home (getenv "HOME" "/tmp")))
    (string-append home "/.gemacs-elfeed-feeds")))

(def (qt-elfeed-load-feeds!)
  (let ((path (qt-elfeed-db-path)))
    (when (file-exists? path)
      (set! *qt-elfeed-feeds*
        (with-exception-catcher
          (lambda (e) '())
          (lambda ()
            (let ((content (call-with-input-file path
                             (lambda (p) (read-line p #f)))))
              (if (and content (string? content))
                (filter (lambda (s) (> (string-length s) 0))
                  (map string-trim-both
                       (string-split content #\newline)))
                '()))))))))

(def (qt-elfeed-save-feeds!)
  (call-with-output-file (qt-elfeed-db-path)
    (lambda (p)
      (for-each (lambda (url) (display url p) (newline p))
                *qt-elfeed-feeds*))))

(def (qt-elfeed-fetch url)
  "Fetch and parse an RSS/Atom feed URL."
  (with-exception-catcher
    (lambda (e) '())
    (lambda ()
      (let* ((proc (open-process
                     (list path: "curl"
                           arguments: (list "-sL" "-A" "Mozilla/5.0"
                                            "--max-time" "15" url)
                           stdin-redirection: #f
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (xml (read-line proc #f)))
        (process-status proc)
        (if (and xml (string? xml))
          (qt-elfeed-parse xml url)
          '())))))

(def (qt-elfeed-extract-tag xml tag (start 0))
  (let* ((open-tag (string-append "<" tag))
         (close-tag (string-append "</" tag ">"))
         (pos (string-contains xml open-tag start)))
    (if (not pos) #f
      (let ((gt (string-index xml #\> pos)))
        (if (not gt) #f
          (let* ((content-start (+ gt 1))
                 (end-pos (string-contains xml close-tag content-start)))
            (if (not end-pos) #f
              (cons (substring xml content-start end-pos)
                    (+ end-pos (string-length close-tag))))))))))

(def (qt-elfeed-unescape s)
  (let* ((s (string-replace-all s "&amp;" "&"))
         (s (string-replace-all s "&lt;" "<"))
         (s (string-replace-all s "&gt;" ">"))
         (s (string-replace-all s "&quot;" "\""))
         (s (string-replace-all s "&#39;" "'"))
         (s (string-replace-all s "<![CDATA[" ""))
         (s (string-replace-all s "]]>" "")))
    (string-trim-both s)))

(def (qt-elfeed-extract-href content)
  (let ((pos (string-contains content "<link")))
    (if (not pos) ""
      (let ((href-pos (string-contains content "href=" pos)))
        (if (not href-pos) ""
          (let* ((q-start (+ href-pos 5))
                 (quote-char (if (< q-start (string-length content))
                               (string-ref content q-start) #\"))
                 (val-start (+ q-start 1))
                 (val-end (string-index content quote-char val-start)))
            (if val-end (substring content val-start val-end) "")))))))

(def (qt-elfeed-parse xml url)
  "Parse RSS/Atom feed."
  (let ((feed-title
          (let ((t (qt-elfeed-extract-tag xml "title")))
            (if t (qt-elfeed-unescape (car t)) url))))
    (let ((items (qt-elfeed-parse-items xml "item" feed-title)))
      (if (null? items)
        (qt-elfeed-parse-items xml "entry" feed-title)
        items))))

(def (qt-elfeed-parse-items xml tag feed-title)
  (let loop ((start 0) (acc '()))
    (let ((item (qt-elfeed-extract-tag xml tag start)))
      (if (not item) (reverse acc)
        (let* ((content (car item))
               (next (cdr item))
               (title-r (qt-elfeed-extract-tag content "title"))
               (title (if title-r (qt-elfeed-unescape (car title-r)) "(no title)"))
               (link-r (qt-elfeed-extract-tag content "link"))
               (link (if link-r
                       (let ((l (car link-r)))
                         (if (> (string-length l) 0)
                           (qt-elfeed-unescape l)
                           (qt-elfeed-extract-href content)))
                       ""))
               (date-r (or (qt-elfeed-extract-tag content "pubDate")
                           (qt-elfeed-extract-tag content "updated")
                           (qt-elfeed-extract-tag content "published")
                           (qt-elfeed-extract-tag content "dc:date")))
               (date (if date-r (qt-elfeed-unescape (car date-r)) "")))
          (loop next (cons (list title link date feed-title) acc)))))))

(def (cmd-elfeed app)
  "Open Elfeed RSS feed reader."
  (qt-elfeed-load-feeds!)
  (when (null? *qt-elfeed-feeds*)
    (set! *qt-elfeed-feeds*
      '("https://planet.emacslife.com/atom.xml"
        "https://hnrss.org/frontpage")))
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app)))
    (echo-message! (app-state-echo app)
      (string-append "Fetching " (number->string (length *qt-elfeed-feeds*)) " feeds..."))
    (set! *qt-elfeed-entries* '())
    (for-each
      (lambda (url)
        (set! *qt-elfeed-entries*
          (append *qt-elfeed-entries* (qt-elfeed-fetch url))))
      *qt-elfeed-feeds*)
    ;; Display
    (let* ((text (qt-elfeed-format *qt-elfeed-entries*))
           (ed2 (current-qt-editor app))
           (buf (or (buffer-by-name "*elfeed*")
                    (qt-buffer-create! "*elfeed*" ed2 #f))))
      (qt-buffer-attach! ed2 buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed2 text)
      (qt-plain-text-edit-set-cursor-position! ed2 0)
      (echo-message! (app-state-echo app)
        (string-append "Elfeed: " (number->string (length *qt-elfeed-entries*)) " entries")))))

(def (qt-elfeed-format entries)
  (string-append "Elfeed - RSS Feed Reader\n"
    (make-string 60 #\=) "\n\n"
    (string-join
      (map (lambda (e)
             (let ((title (car e))
                   (link (cadr e))
                   (date (caddr e))
                   (feed (cadddr e)))
               (string-append
                 (if (> (string-length date) 16)
                   (substring date 0 16) date)
                 "  " (string-pad-right feed 20) "  " title
                 "\n    " link)))
           entries)
      "\n\n")
    "\n"))

(def (cmd-elfeed-add-feed app)
  "Add an RSS feed URL."
  (let* ((echo (app-state-echo app))
         (url (qt-echo-read-string app "Feed URL: ")))
    (when (and url (> (string-length url) 0))
      (qt-elfeed-load-feeds!)
      (unless (member url *qt-elfeed-feeds*)
        (set! *qt-elfeed-feeds* (cons url *qt-elfeed-feeds*))
        (qt-elfeed-save-feeds!)
        (echo-message! echo (string-append "Added feed: " url))))))

(def (cmd-elfeed-update app)
  "Refresh elfeed feeds."
  (cmd-elfeed app))


;;;============================================================================
;;; Direnv — .envrc integration (Qt)
;;;============================================================================

(def (cmd-direnv-update-environment app)
  "Load .envrc via direnv."
  (let* ((dir (current-directory))
         (envrc (string-append dir "/.envrc")))
    (if (not (file-exists? envrc))
      (echo-message! (app-state-echo app) "No .envrc in current directory")
      (with-exception-catcher
        (lambda (e) (echo-message! (app-state-echo app) "direnv failed"))
        (lambda ()
          (let* ((proc (open-process
                         (list path: "direnv"
                               arguments: (list "export" "bash")
                               directory: dir
                               stdin-redirection: #f
                               stdout-redirection: #t
                               stderr-redirection: #f)))
                 (output (read-line proc #f)))
            (process-status proc)
            (when (and output (string? output))
              (let loop ((rest output) (count 0))
                (let ((pos (string-contains rest "export ")))
                  (if (not pos)
                    (echo-message! (app-state-echo app)
                      (string-append "direnv: loaded " (number->string count) " vars"))
                    (let* ((start (+ pos 7))
                           (nl (or (string-index rest #\newline start)
                                   (string-length rest)))
                           (assign (substring rest start nl))
                           (eq (string-index assign #\=)))
                      (when eq
                        (let ((var (substring assign 0 eq))
                              (val (let ((raw (substring assign (+ eq 1) (string-length assign))))
                                     (if (and (> (string-length raw) 1)
                                              (or (char=? (string-ref raw 0) #\')
                                                  (char=? (string-ref raw 0) #\")))
                                       (substring raw 1 (- (string-length raw) 1))
                                       raw))))
                          (setenv var val)))
                      (loop (substring rest (+ nl 1) (string-length rest))
                            (+ count 1)))))))))))))

(def (cmd-direnv-allow app)
  "Run direnv allow."
  (with-exception-catcher
    (lambda (e) (echo-message! (app-state-echo app) "direnv allow failed"))
    (lambda ()
      (let* ((proc (open-process
                     (list path: "direnv" arguments: (list "allow")
                           directory: (current-directory)
                           stdin-redirection: #f stdout-redirection: #t
                           stderr-redirection: #f)))
             (out (read-line proc #f)))
        (process-status proc)
        (echo-message! (app-state-echo app) "direnv: allowed .envrc")))))

;;;============================================================================
;;; Move text up/down (drag-stuff)
;;;============================================================================

(def (cmd-move-text-up app)
  "Move current line up."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (cur-line (qt-pos-to-line text pos)))
    (when (> cur-line 0)
      (let* ((swapped
               (let loop ((ls lines) (n 0) (acc '()))
                 (cond
                   ((null? ls) (reverse acc))
                   ((= n (- cur-line 1))
                    (if (null? (cdr ls))
                      (reverse (cons (car ls) acc))
                      (loop (cddr ls) (+ n 2)
                            (cons (car ls) (cons (cadr ls) acc)))))
                   (else (loop (cdr ls) (+ n 1) (cons (car ls) acc))))))
             (new-text (string-join swapped "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (qt-line-to-pos new-text (- cur-line 1)))))))

(def (cmd-move-text-down app)
  "Move current line down."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (cur-line (qt-pos-to-line text pos))
         (max-line (- (length lines) 1)))
    (when (< cur-line max-line)
      (let* ((swapped
               (let loop ((ls lines) (n 0) (acc '()))
                 (cond
                   ((null? ls) (reverse acc))
                   ((= n cur-line)
                    (if (null? (cdr ls))
                      (reverse (cons (car ls) acc))
                      (loop (cddr ls) (+ n 2)
                            (cons (car ls) (cons (cadr ls) acc)))))
                   (else (loop (cdr ls) (+ n 1) (cons (car ls) acc))))))
             (new-text (string-join swapped "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (qt-line-to-pos new-text (+ cur-line 1)))))))

(def (qt-pos-to-line text pos)
  "Convert character position to line number."
  (let loop ((i 0) (line 0))
    (if (>= i pos) line
      (if (and (< i (string-length text)) (char=? (string-ref text i) #\newline))
        (loop (+ i 1) (+ line 1))
        (loop (+ i 1) line)))))

(def (qt-line-to-pos text line)
  "Convert line number to character position."
  (let loop ((i 0) (n 0))
    (if (= n line) i
      (if (>= i (string-length text)) i
        (if (char=? (string-ref text i) #\newline)
          (loop (+ i 1) (+ n 1))
          (loop (+ i 1) n))))))


;;;============================================================================
;;; Transient keymaps (Qt)
;;;============================================================================

(def *qt-transient-maps* (make-hash-table))

(def (qt-transient-init!)
  (hash-put! *qt-transient-maps* 'window-resize
    '((#\{ "Shrink horizontal" shrink-window-horizontally)
      (#\} "Grow horizontal" enlarge-window-horizontally)
      (#\^ "Grow vertical" enlarge-window)
      (#\v "Shrink vertical" shrink-window)
      (#\= "Balance" balance-windows)))
  (hash-put! *qt-transient-maps* 'zoom
    '((#\+ "Zoom in" text-scale-increase)
      (#\- "Zoom out" text-scale-decrease)
      (#\0 "Reset" text-scale-adjust)))
  (hash-put! *qt-transient-maps* 'navigate
    '((#\n "Next error" next-error)
      (#\p "Previous error" previous-error)
      (#\N "Next buffer" next-buffer)
      (#\P "Previous buffer" previous-buffer))))

(def (cmd-transient-map app)
  "Show transient keymap menu."
  (qt-transient-init!)
  (let* ((echo (app-state-echo app))
         (names (hash-keys *qt-transient-maps*))
         (choice (qt-echo-read-string app
                   (string-append "Transient ("
                     (string-join (map symbol->string names) "/") "): "))))
    (when (and choice (> (string-length choice) 0))
      (let ((sym (string->symbol choice)))
        (if (not (hash-get *qt-transient-maps* sym))
          (echo-message! echo (string-append "Unknown: " choice))
          (let* ((entries (hash-get *qt-transient-maps* sym))
                 (prompt (string-append (symbol->string sym) ": "
                           (string-join
                             (map (lambda (e)
                                    (string-append (string (car e)) "=" (cadr e)))
                                  entries) " ")))
                 (key-str (qt-echo-read-string app (string-append prompt " > "))))
            (when (and key-str (= (string-length key-str) 1))
              (let* ((ch (string-ref key-str 0))
                     (entry (find (lambda (e) (char=? (car e) ch)) entries)))
                (if entry
                  (let* ((cmd-sym (caddr entry))
                         (cmd (find-command cmd-sym)))
                    (if cmd (cmd app)
                      (echo-message! echo "Command not found")))
                  (echo-message! echo "Unknown key"))))))))))

;;;============================================================================
;;; Terraform integration
;;;============================================================================

(def (cmd-terraform-mode app)
  "Enable Terraform/HCL mode — sets properties highlighting for HCL files."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_SETLEXER SCLEX_PROPERTIES)
    (echo-message! (app-state-echo app) "Terraform mode enabled (properties lexer)")))

(def (cmd-terraform app)
  "Run terraform command interactively."
  (let* ((echo (app-state-echo app))
         (args (qt-echo-read-string app "terraform: ")))
    (when (and args (> (string-length args) 0))
      (let ((output (with-catch
                      (lambda (e) (string-append "Error: " (error-message e)))
                      (lambda ()
                        (let ((p (open-input-process
                                   (list path: "terraform"
                                         arguments: (string-split args #\space)
                                         stderr-redirection: #t))))
                          (let ((result (read-line p #f)))
                            (close-port p)
                            (or result "No output")))))))
        (let* ((ed (current-qt-editor app))
               (fr (app-state-frame app))
               (buf (or (buffer-by-name "*Terraform*")
                        (qt-buffer-create! "*Terraform*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "$ terraform " args "\n\n" output "\n")))))))

(def (cmd-terraform-plan app)
  "Run terraform plan in the current directory."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (dir (let ((fp (buffer-file-path buf)))
                (if fp (path-directory fp) "."))))
    (echo-message! echo "Running terraform plan...")
    (let ((output (with-catch
                    (lambda (e) (string-append "Error: " (error-message e)))
                    (lambda ()
                      (let ((p (open-input-process
                                 (list path: "terraform"
                                       arguments: '("plan" "-no-color")
                                       directory: dir
                                       stderr-redirection: #t))))
                        (let ((result (read-line p #f)))
                          (close-port p)
                          (or result "No output")))))))
      (let* ((ed (current-qt-editor app))
             (fr (app-state-frame app))
             (obuf (or (buffer-by-name "*Terraform Plan*")
                       (qt-buffer-create! "*Terraform Plan*" ed #f))))
        (qt-buffer-attach! ed obuf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) obuf)
        (qt-plain-text-edit-set-text! ed
          (string-append "terraform plan\nDirectory: " dir "\n\n" output "\n"))))))

;;;============================================================================
;;; Docker Compose integration
;;;============================================================================

(def (cmd-docker-compose app)
  "Run docker compose command interactively."
  (let* ((echo (app-state-echo app))
         (args (qt-echo-read-string app "docker compose: ")))
    (when (and args (> (string-length args) 0))
      (let ((output (with-catch
                      (lambda (e) (string-append "Error: " (error-message e)))
                      (lambda ()
                        (let ((p (open-input-process
                                   (list path: "docker"
                                         arguments: (cons "compose" (string-split args #\space))
                                         stderr-redirection: #t))))
                          (let ((result (read-line p #f)))
                            (close-port p)
                            (or result "No output")))))))
        (let* ((ed (current-qt-editor app))
               (fr (app-state-frame app))
               (buf (or (buffer-by-name "*Docker Compose*")
                        (qt-buffer-create! "*Docker Compose*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed
            (string-append "$ docker compose " args "\n\n" output "\n")))))))

(def (cmd-docker-compose-up app)
  "Run docker compose up -d."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (dir (let ((fp (buffer-file-path buf)))
                (if fp (path-directory fp) "."))))
    (echo-message! echo "Running docker compose up...")
    (let ((output (with-catch
                    (lambda (e) (string-append "Error: " (error-message e)))
                    (lambda ()
                      (let ((p (open-input-process
                                 (list path: "docker"
                                       arguments: '("compose" "up" "-d")
                                       directory: dir
                                       stderr-redirection: #t))))
                        (let ((result (read-line p #f)))
                          (close-port p)
                          (or result "No output")))))))
      (let* ((ed (current-qt-editor app))
             (fr (app-state-frame app))
             (obuf (or (buffer-by-name "*Docker Compose*")
                       (qt-buffer-create! "*Docker Compose*" ed #f))))
        (qt-buffer-attach! ed obuf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) obuf)
        (qt-plain-text-edit-set-text! ed
          (string-append "docker compose up -d\nDirectory: " dir "\n\n" output "\n"))))))

(def (cmd-docker-compose-down app)
  "Run docker compose down."
  (let* ((echo (app-state-echo app))
         (buf (current-qt-buffer app))
         (dir (let ((fp (buffer-file-path buf)))
                (if fp (path-directory fp) "."))))
    (let ((output (with-catch
                    (lambda (e) (string-append "Error: " (error-message e)))
                    (lambda ()
                      (let ((p (open-input-process
                                 (list path: "docker"
                                       arguments: '("compose" "down")
                                       directory: dir
                                       stderr-redirection: #t))))
                        (let ((result (read-line p #f)))
                          (close-port p)
                          (or result "No output")))))))
      (echo-message! echo (string-append "docker compose down: " output)))))

;;;============================================================================
;;; project-query-replace: interactive search+replace across project files
;;;============================================================================

(def (pqr-grep-files root pattern)
  "Use grep to find all project files containing pattern. Returns list of paths."
  (with-catch
    (lambda (e) '())
    (lambda ()
      (let* ((proc (open-process
                     (list path: "/usr/bin/grep"
                           arguments: (list "-rli" pattern root
                             "--include=*.ss" "--include=*.scm"
                             "--include=*.py" "--include=*.js" "--include=*.ts"
                             "--include=*.go" "--include=*.rs"
                             "--include=*.c" "--include=*.h"
                             "--include=*.cpp" "--include=*.hpp"
                             "--include=*.rb" "--include=*.java"
                             "--include=*.md" "--include=*.txt")
                           stdout-redirection: #t
                           stderr-redirection: #f)))
             (output (read-line proc #f)))
        (close-port proc)
        (if output
          (filter (lambda (s) (> (string-length s) 0))
                  (string-split output #\newline))
          '())))))

(def (cmd-project-query-replace app)
  "Interactively replace string across all project files (like Emacs project-query-replace)."
  (let* ((root (current-project-root app))
         (echo (app-state-echo app)))
    (if (not root)
      (echo-error! echo "Not in a project")
      (let ((from-str (qt-echo-read-string app "Project query replace: ")))
        (when (and from-str (> (string-length from-str) 0))
          (let ((to-str (qt-echo-read-string app
                          (string-append "Replace \"" from-str "\" with: "))))
            (when to-str
              (echo-message! echo (string-append "Searching " root " ..."))
              (let ((files (pqr-grep-files root from-str)))
                (if (null? files)
                  (echo-message! echo (string-append "No matches for: " from-str))
                  (let* ((first-file (car files))
                         (rest-files (cdr files))
                         (fr (app-state-frame app))
                         (ed (current-qt-editor app)))
                    ;; Set up multi-file qreplace state
                    (set! *qreplace-files-remaining* rest-files)
                    (set! *qreplace-from* from-str)
                    (set! *qreplace-to* to-str)
                    (set! *qreplace-count* 0)
                    (set! *qreplace-app* app)
                    ;; Open first file
                    (with-catch
                      (lambda (e)
                        (echo-error! echo (string-append "Cannot open: " first-file)))
                      (lambda ()
                        (let* ((content (let* ((p (open-input-file first-file))
                                               (s (read-line p #f)))
                                          (close-port p) s))
                               (buf-name (path-strip-directory first-file))
                               (buf (or (buffer-by-name buf-name)
                                        (qt-buffer-create! buf-name ed #f)))
                               (_ (set! (buffer-file-path buf) first-file)))
                          (qt-buffer-attach! ed buf)
                          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                          (when content
                            (qt-plain-text-edit-set-text! ed content)
                            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f))
                          (set! *qreplace-pos* 0)
                          (set! *qreplace-active* #t)
                          (qt-modeline-update! app)
                          ;; Find and show first match
                          (qreplace-show-next! app))))))))))))))

;; cmd-align-regexp is defined in qt/commands-sexp.ss
;; cmd-insert-uuid is defined in qt/commands-file.ss

