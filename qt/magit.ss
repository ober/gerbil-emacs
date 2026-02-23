;;; -*- Gerbil -*-
;;; Magit helper functions — parsing, formatting, hunk extraction.
;;; Used by qt/commands-ide.ss for the interactive magit status buffer.

(export #t)

(import :std/sugar
        :std/srfi/13)

;;;============================================================================
;;; Git process helpers
;;;============================================================================

(def (magit-run-git args dir)
  "Run git command, return output. Omits process-status to avoid Qt SIGCHLD race."
  (with-catch
    (lambda (e) "")
    (lambda ()
      (let* ((proc (open-process
                      (list path: "/usr/bin/git"
                            arguments: args
                            directory: dir
                            stdout-redirection: #t
                            stderr-redirection: #t)))
             (output (read-line proc #f)))
        (close-port proc)
        (or output "")))))

(def (magit-run-git-stdin input args dir)
  "Run git command with stdin input. Returns output string."
  (with-catch
    (lambda (e) (string-append "error: "
                  (with-output-to-string (lambda () (display-exception e)))))
    (lambda ()
      (let ((proc (open-process
                    (list path: "/usr/bin/git"
                          arguments: args
                          directory: dir
                          stdin-redirection: #t
                          stdout-redirection: #t
                          stderr-redirection: #t))))
        (display input proc)
        (force-output proc)
        (close-output-port proc)
        (let ((out (read-line proc #f)))
          (close-port proc)
          (or out ""))))))

;;;============================================================================
;;; Status parsing and formatting
;;;============================================================================

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

(def (magit-status-label code)
  "Convert porcelain status code to readable label."
  (cond ((string=? code "M") "modified")
        ((string=? code "A") "new file")
        ((string=? code "D") "deleted")
        ((string=? code "R") "renamed")
        ((string=? code "C") "copied")
        ((string=? code "??") "untracked")
        (else code)))

(def (magit-partition-entries entries)
  "Partition entries into (staged unstaged untracked)."
  (let ((staged '()) (unstaged '()) (untracked '()))
    (for-each
      (lambda (e)
        (let ((s (car e)))
          (cond
            ((string=? s "??") (set! untracked (cons e untracked)))
            ((and (> (string-length s) 0)
                  (let ((c (string-ref s 0)))
                    (and (not (char=? c #\space)) (not (char=? c #\?)))))
             (set! staged (cons e staged)))
            ((and (>= (string-length s) 2)
                  (let ((c (string-ref s (min 1 (- (string-length s) 1)))))
                    (and (not (char=? c #\space)) (not (char=? c #\?)))))
             (set! unstaged (cons e unstaged))))))
      entries)
    (values (reverse staged) (reverse unstaged) (reverse untracked))))

(def (magit-format-status entries branch dir)
  "Format magit status buffer with inline diffs."
  (let-values (((staged unstaged untracked) (magit-partition-entries entries)))
    (let ((out (open-output-string)))
      (display (string-append "Head: " branch "\n") out)
      ;; Unstaged changes with inline diffs
      (when (not (null? unstaged))
        (display (string-append "\nUnstaged changes ("
                   (number->string (length unstaged)) "):\n") out)
        (for-each (lambda (e)
                    (let* ((file (cdr e))
                           (label (magit-status-label (car e)))
                           (diff (magit-run-git (list "diff" "--" file) dir)))
                      (display (string-append label "   " file "\n") out)
                      (when (> (string-length diff) 0)
                        (display diff out)
                        (when (not (string-suffix? "\n" diff))
                          (display "\n" out)))))
                  unstaged))
      ;; Staged changes with inline diffs
      (when (not (null? staged))
        (display (string-append "\nStaged changes ("
                   (number->string (length staged)) "):\n") out)
        (for-each (lambda (e)
                    (let* ((file (cdr e))
                           (label (magit-status-label (car e)))
                           (diff (magit-run-git (list "diff" "--cached" "--" file) dir)))
                      (display (string-append label "   " file "\n") out)
                      (when (> (string-length diff) 0)
                        (display diff out)
                        (when (not (string-suffix? "\n" diff))
                          (display "\n" out)))))
                  staged))
      ;; Untracked files
      (when (not (null? untracked))
        (display (string-append "\nUntracked files ("
                   (number->string (length untracked)) "):\n") out)
        (for-each (lambda (e)
                    (display (string-append "  " (cdr e) "\n") out))
                  untracked))
      (when (and (null? staged) (null? unstaged) (null? untracked))
        (display "\nNothing to commit, working tree clean.\n" out))
      (display "\nKeys: s=stage u=unstage S=stage-all c=commit d=diff l=log\n" out)
      (display "      b=branch k=checkout f=fetch F=pull P=push z=stash q=quit\n" out)
      (display "      g=refresh n/p=navigate  (s/u on diff hunk = stage/unstage hunk)\n" out)
      (get-output-string out))))

;;;============================================================================
;;; Cursor context detection
;;;============================================================================

(def (magit-file-at-point text pos)
  "Extract filename from current line in magit buffer."
  (let* ((line-start (let loop ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1) (loop (- i 1)))))
         (line-end (let loop ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1)))))
         (line (substring text line-start line-end))
         (trimmed (string-trim line)))
    (cond
      ((string-prefix? "modified   " trimmed)
       (substring trimmed 11 (string-length trimmed)))
      ((string-prefix? "new file   " trimmed)
       (substring trimmed 11 (string-length trimmed)))
      ((string-prefix? "deleted   " trimmed)
       (substring trimmed 10 (string-length trimmed)))
      ((string-prefix? "renamed   " trimmed)
       (substring trimmed 10 (string-length trimmed)))
      ((string-prefix? "copied   " trimmed)
       (substring trimmed 9 (string-length trimmed)))
      ;; Legacy format: "  M file" or "  ?? file"
      ((and (>= (string-length trimmed) 3)
            (string=? (substring trimmed 0 2) "??"))
       (string-trim (substring trimmed 2 (string-length trimmed))))
      ((and (>= (string-length trimmed) 2)
            (memv (string-ref trimmed 0) '(#\M #\A #\D #\R #\C #\U)))
       (string-trim (substring trimmed 1 (string-length trimmed))))
      (else #f))))

(def (magit-get-line text pos)
  "Get the line at position pos."
  (let* ((start (let loop ((i (max 0 (- pos 1))))
                  (if (or (< i 0) (char=? (string-ref text i) #\newline))
                    (+ i 1) (loop (- i 1)))))
         (end (let loop ((i pos))
                (if (or (>= i (string-length text))
                        (char=? (string-ref text i) #\newline))
                  i (loop (+ i 1))))))
    (substring text start end)))

(def (magit-find-section text pos)
  "Find which section cursor is in: 'staged, 'unstaged, 'untracked, or #f."
  (let ((len (string-length text)))
    (let loop ((i (min pos (- len 1))))
      (cond
        ((< i 0) #f)
        ((char=? (string-ref text i) #\newline)
         (let ((next (+ i 1)))
           (cond
             ((and (<= (+ next 16) len)
                   (string=? (substring text next (+ next 16)) "Unstaged changes"))
              'unstaged)
             ((and (<= (+ next 14) len)
                   (string=? (substring text next (+ next 14)) "Staged changes"))
              'staged)
             ((and (<= (+ next 15) len)
                   (string=? (substring text next (+ next 15)) "Untracked files"))
              'untracked)
             (else (loop (- i 1))))))
        (else (loop (- i 1)))))))

;;;============================================================================
;;; Hunk extraction for git apply
;;;============================================================================

(def (magit-in-diff-line? line)
  "Check if a line is part of a diff block."
  (or (string-prefix? "diff " line)
      (string-prefix? "index " line)
      (string-prefix? "--- " line)
      (string-prefix? "+++ " line)
      (string-prefix? "@@" line)
      (and (> (string-length line) 0)
           (memv (string-ref line 0) '(#\+ #\- #\space)))))

(def (magit--scan-backward-for text pos prefix)
  "Scan backward from pos to find a line starting with prefix. Returns position or #f."
  (let ((len (string-length text)))
    (let loop ((i pos))
      (cond
        ((< i 0) #f)
        ((and (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
              (<= (+ i (string-length prefix)) len)
              (string=? (substring text i (+ i (string-length prefix))) prefix))
         i)
        (else (loop (- i 1)))))))

(def (magit--skip-to-eol text i)
  "Skip to end of line from position i, return position after newline."
  (let ((len (string-length text)))
    (let loop ((j i))
      (cond ((>= j len) j)
            ((char=? (string-ref text j) #\newline) (+ j 1))
            (else (loop (+ j 1)))))))

(def (magit--find-hunk-end text hunk-start)
  "Find end of hunk: next @@ or diff --git or section header or end."
  (let ((len (string-length text)))
    (let loop ((i (+ hunk-start 2)))
      (cond
        ((>= i len) len)
        ((char=? (string-ref text i) #\newline)
         (let ((next (+ i 1)))
           (cond
             ((>= next len) (+ i 1))
             ((and (<= (+ next 2) len)
                   (string=? (substring text next (+ next 2)) "@@"))
              next)
             ((and (<= (+ next 11) len)
                   (string=? (substring text next (+ next 11)) "diff --git "))
              next)
             ((and (<= (+ next 8) len)
                   (or (string-prefix? "Staged " (substring text next (min len (+ next 16))))
                       (string-prefix? "Unstaged" (substring text next (min len (+ next 16))))
                       (string-prefix? "Untrack" (substring text next (min len (+ next 16))))))
              next)
             (else (loop (+ i 1))))))
        (else (loop (+ i 1)))))))

(def (magit-hunk-at-point text pos)
  "Extract a complete patch for the diff hunk at cursor.
   Returns (values file-name patch-string) or (values #f #f)."
  (let ((len (string-length text))
        (line (magit-get-line text pos)))
    (if (not (magit-in-diff-line? line))
      (values #f #f)
      ;; Find diff header
      (let ((diff-start (magit--scan-backward-for text pos "diff --git ")))
        (if (not diff-start)
          (values #f #f)
          (let* ((header-end (magit--skip-to-eol text diff-start))
                 (header-line (substring text diff-start (- header-end 1)))
                 (b-idx (string-contains header-line " b/"))
                 (file-name (and b-idx (substring header-line (+ b-idx 3)
                                         (string-length header-line))))
                 ;; Find +++ line end (end of diff header)
                 (plus3-start (magit--scan-backward-for text pos "+++ "))
                 (plus3-fwd (or plus3-start
                               (magit--scan-backward-for text (+ diff-start 20) "+++ ")))
                 ;; If we can't find +++, search forward from diff-start
                 (plus3-pos (or plus3-fwd
                               (let loop ((i diff-start))
                                 (cond ((>= i (min len (+ diff-start 500))) #f)
                                       ((and (or (= i 0) (char=? (string-ref text (- i 1)) #\newline))
                                             (<= (+ i 4) len)
                                             (string=? (substring text i (+ i 4)) "+++ "))
                                        i)
                                       (else (loop (+ i 1)))))))
                 (header-end-pos (if plus3-pos (magit--skip-to-eol text plus3-pos) header-end))
                 (diff-header (substring text diff-start header-end-pos))
                 ;; Find current hunk start
                 (hunk-start (magit--scan-backward-for text pos "@@"))
                 ;; Find hunk end
                 (hunk-end (and hunk-start (magit--find-hunk-end text hunk-start))))
            (if (and hunk-start hunk-end file-name)
              (let ((hunk-text (substring text hunk-start hunk-end)))
                (values file-name
                        (string-append diff-header hunk-text
                          (if (string-suffix? "\n" hunk-text) "" "\n"))))
              (values file-name #f))))))))

;;;============================================================================
;;; Branch helpers
;;;============================================================================

(def (magit-branch-names dir)
  "Get list of branch names (local + remote) for narrowing."
  (let* ((output (magit-run-git '("branch" "-a" "--format=%(refname:short)") dir))
         (lines (string-split output #\newline)))
    (filter (lambda (s) (> (string-length s) 0)) lines)))
