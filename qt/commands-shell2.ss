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
;;; Eldoc mode toggle

(def *qt-eldoc-enabled* #f)

(def (cmd-eldoc-mode app)
  "Toggle eldoc mode — shows function signatures in echo area."
  (set! *qt-eldoc-enabled* (not *qt-eldoc-enabled*))
  (echo-message! (app-state-echo app)
    (if *qt-eldoc-enabled* "Eldoc mode: on" "Eldoc mode: off")))

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
