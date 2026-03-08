;;; -*- Gerbil -*-
;;; Qt commands search2 - grep, git-gutter, org-cycle, dired refresh
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/async
        :gemacs/subprocess
        :gemacs/gsh-subprocess
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
        :gemacs/qt/commands-core
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search)

;;;============================================================================
;;; Grep commands (moved from ide for chain ordering)
;;;============================================================================

(def (parse-grep-line line)
  "Parse a grep -n output line. Returns (file line-num text) or #f."
  ;; Format: file:line:text
  (let ((colon1 (string-index line #\:)))
    (and colon1
         (let ((colon2 (string-index line #\: (+ colon1 1))))
           (and colon2
                (let* ((file (substring line 0 colon1))
                       (line-str (substring line (+ colon1 1) colon2))
                       (text (substring line (+ colon2 1) (string-length line)))
                       (line-num (string->number line-str)))
                  (and line-num
                       (list file line-num text))))))))

(def (grep-shell-quote s)
  "Quote a string for safe shell use."
  (string-append "'" (let loop ((i 0) (acc ""))
                       (if (>= i (string-length s))
                         acc
                         (let ((ch (string-ref s i)))
                           (if (char=? ch #\')
                             (loop (+ i 1) (string-append acc "'\"'\"'"))
                             (loop (+ i 1) (string-append acc (string ch)))))))
                 "'"))

(def (grep-run-and-show! app pattern dir args)
  "Run grep async, parse results, show in *Grep* buffer."
  (echo-message! (app-state-echo app) "Searching...")
  (let* ((grep-cmd (string-append "grep " (string-join args " ") " -- "
                                  (grep-shell-quote pattern) " "
                                  (grep-shell-quote dir) " 2>/dev/null || true")))
    (async-process! grep-cmd
      callback: (lambda (output)
        (let* ((lines (if (and output (> (string-length output) 0))
                        (let loop ((s output) (acc []))
                          (let ((nl (string-index s #\newline)))
                            (if nl
                              (loop (substring s (+ nl 1) (string-length s))
                                    (cons (substring s 0 nl) acc))
                              (reverse (if (> (string-length s) 0)
                                         (cons s acc) acc)))))
                        []))
               (parsed (filter identity (map parse-grep-line lines)))
               (header (string-append "-*- grep -*-\n"
                         "grep " (string-join args " ") " " pattern " " dir "\n\n")))
          (set! *grep-results* parsed)
          (set! *grep-result-index* -1)
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app))
                 (result-text (if (null? parsed)
                               (string-append header "No matches found.\n")
                               (string-append header
                                 (number->string (length parsed)) " matches\n\n"
                                 (string-join
                                   (map (lambda (r)
                                          (string-append (car r) ":"
                                            (number->string (cadr r)) ":"
                                            (caddr r)))
                                        parsed)
                                   "\n")
                                 "\n\nPress Enter on a result line to jump to source.")))
                 (grep-buf (or (buffer-by-name "*Grep*")
                               (qt-buffer-create! "*Grep*" ed #f))))
            (qt-buffer-attach! ed grep-buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) grep-buf)
            (qt-plain-text-edit-set-text! ed result-text)
            (qt-text-document-set-modified! (buffer-doc-pointer grep-buf) #f)
            (qt-plain-text-edit-set-cursor-position! ed 0)
            (echo-message! (app-state-echo app)
              (string-append (number->string (length parsed)) " matches"))))))))

(def (cmd-grep app)
  "Run grep -rn and show results with navigation."
  (let ((pattern (qt-echo-read-string app "Grep: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let ((dir (qt-echo-read-string app "In directory: ")))
        (when (and dir (> (string-length dir) 0))
          (grep-run-and-show! app pattern dir '("-rn")))))))

(def (cmd-rgrep app)
  "Run recursive grep with file type filter."
  (let ((pattern (qt-echo-read-string app "Rgrep: ")))
    (when (and pattern (> (string-length pattern) 0))
      (let ((include (qt-echo-read-string app "File pattern (e.g. *.ss): ")))
        (when include
          (let ((dir (qt-echo-read-string app "In directory: ")))
            (when (and dir (> (string-length dir) 0))
              (if (and include (> (string-length include) 0))
                (grep-run-and-show! app pattern dir
                  (list "-rn" (string-append "--include=" include)))
                (grep-run-and-show! app pattern dir '("-rn"))))))))))

(def (grep-goto-result! app index)
  "Jump to grep result at INDEX."
  (when (and (>= index 0) (< index (length *grep-results*)))
    (let* ((result (list-ref *grep-results* index))
           (file (car result))
           (line-num (cadr result))
           (echo (app-state-echo app))
           (fr (app-state-frame app))
           (ed (current-qt-editor app)))
      (set! *grep-result-index* index)
      (when (file-exists? file)
        (let* ((name (path-strip-directory file))
               (existing (let loop ((bufs *buffer-list*))
                           (if (null? bufs) #f
                             (let ((b (car bufs)))
                               (if (and (buffer-file-path b)
                                        (string=? (buffer-file-path b) file))
                                 b (loop (cdr bufs)))))))
               (buf (or existing (qt-buffer-create! name ed file))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (when (not existing)
            (let ((text (read-file-as-string file)))
              (when text
                (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
            (qt-setup-highlighting! app buf))
          ;; Jump to line
          (let* ((text (qt-plain-text-edit-text ed))
                 (pos (text-line-position text line-num)))
            (qt-plain-text-edit-set-cursor-position! ed pos)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-message! echo
            (string-append "Match " (number->string (+ index 1))
              "/" (number->string (length *grep-results*)))))))))

(def (cmd-grep-goto app)
  "Jump from *Grep* buffer line to source location."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app)))
    (if (not (string=? (buffer-name buf) "*Grep*"))
      (echo-error! (app-state-echo app) "Not in *Grep* buffer")
      ;; Parse current line for file:line:text pattern
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             ;; Find current line
             (line-start (let loop ((i (- pos 1)))
                           (if (or (< i 0) (char=? (string-ref text i) #\newline))
                             (+ i 1) (loop (- i 1)))))
             (line-end (let loop ((i pos))
                         (if (or (>= i (string-length text))
                                 (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
             (line (substring text line-start line-end))
             (parsed (parse-grep-line line)))
        (if parsed
          ;; Find the matching index in *grep-results*
          (let ((idx (let loop ((results *grep-results*) (i 0))
                       (cond
                         ((null? results) 0)
                         ((and (string=? (car (car results)) (car parsed))
                               (= (cadr (car results)) (cadr parsed)))
                          i)
                         (else (loop (cdr results) (+ i 1)))))))
            (grep-goto-result! app idx))
          (echo-error! (app-state-echo app) "No grep result on this line"))))))

(def (cmd-next-grep-result app)
  "Jump to next grep result."
  (if (null? *grep-results*)
    (echo-error! (app-state-echo app) "No grep results")
    (let ((next-idx (+ *grep-result-index* 1)))
      (if (>= next-idx (length *grep-results*))
        (echo-message! (app-state-echo app) "No more matches")
        (grep-goto-result! app next-idx)))))

(def (cmd-previous-grep-result app)
  "Jump to previous grep result."
  (if (null? *grep-results*)
    (echo-error! (app-state-echo app) "No grep results")
    (let ((prev-idx (- *grep-result-index* 1)))
      (if (< prev-idx 0)
        (echo-message! (app-state-echo app) "No previous matches")
        (grep-goto-result! app prev-idx)))))

;;;============================================================================
;;; Parity batch 9: git-gutter, goto-last-edit, highlight, describe-char,
;;; org-cycle/link, revert-no-confirm, save-kill, kill-compilation
;;;============================================================================

;; --- Git gutter ---
(def *qt-git-hunks* (make-hash-table))
(def *qt-git-hunk-idx* (make-hash-table))

(def (qt-parse-git-hunks file-path)
  "Parse git diff for file, return list of (start-line count type)."
  (with-exception-catcher (lambda (e) '())
    (lambda ()
      (let* ((dir (path-directory file-path))
             (p (open-process (list path: "git" arguments: (list "diff" "-U0" "--" file-path)
                   directory: dir stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
             (out (read-line p #f)))
        (process-status p)
        (if (not out) '()
          (let ((lines (string-split out #\newline)))
            (let loop ((ls lines) (acc []))
              (if (null? ls) (reverse acc)
                (let ((l (car ls)))
                  (if (and (> (string-length l) 3) (string-prefix? "@@ " l))
                    (let* ((plus-pos (string-contains l "+"))
                           (rest (and plus-pos (substring l (+ plus-pos 1) (string-length l))))
                           (sp (and rest (string-index rest #\space)))
                           (comma (and rest (string-index rest #\,)))
                           (end (or comma sp (and rest (string-length rest))))
                           (start (and end (string->number (substring rest 0 end))))
                           (cnt (if comma (or (string->number (substring rest (+ comma 1) (or sp (string-length rest)))) 1) 1)))
                      (loop (cdr ls) (if start (cons (list start cnt "change") acc) acc)))
                    (loop (cdr ls) acc)))))))))))

(def (cmd-git-gutter-mode app)
  "Refresh git diff hunks for current buffer."
  (let* ((buf (current-qt-buffer app)) (path (buffer-file-path buf)) (name (buffer-name buf)))
    (if (not path) (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((hunks (qt-parse-git-hunks path)))
        (hash-put! *qt-git-hunks* name hunks) (hash-put! *qt-git-hunk-idx* name 0)
        (echo-message! (app-state-echo app) (string-append (number->string (length hunks)) " hunk(s)"))))))

(def (cmd-git-gutter-next-hunk app)
  "Jump to next git diff hunk."
  (let* ((buf (current-qt-buffer app)) (name (buffer-name buf))
         (hunks (or (hash-get *qt-git-hunks* name) '())))
    (if (null? hunks) (echo-message! (app-state-echo app) "No hunks")
      (let* ((idx (modulo (+ (or (hash-get *qt-git-hunk-idx* name) 0) 1) (length hunks)))
             (hunk (list-ref hunks idx)) (line (car hunk)) (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)) (pos (text-line-position text line)))
        (hash-put! *qt-git-hunk-idx* name idx)
        (qt-plain-text-edit-set-cursor-position! ed pos) (qt-plain-text-edit-ensure-cursor-visible! ed)
        (echo-message! (app-state-echo app)
          (string-append "Hunk " (number->string (+ idx 1)) "/" (number->string (length hunks))))))))

(def (cmd-git-gutter-previous-hunk app)
  "Jump to previous git diff hunk."
  (let* ((buf (current-qt-buffer app)) (name (buffer-name buf))
         (hunks (or (hash-get *qt-git-hunks* name) '())))
    (if (null? hunks) (echo-message! (app-state-echo app) "No hunks")
      (let* ((idx (modulo (- (or (hash-get *qt-git-hunk-idx* name) 0) 1) (length hunks)))
             (hunk (list-ref hunks idx)) (line (car hunk)) (ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed)) (pos (text-line-position text line)))
        (hash-put! *qt-git-hunk-idx* name idx)
        (qt-plain-text-edit-set-cursor-position! ed pos) (qt-plain-text-edit-ensure-cursor-visible! ed)
        (echo-message! (app-state-echo app)
          (string-append "Hunk " (number->string (+ idx 1)) "/" (number->string (length hunks))))))))

(def (cmd-git-gutter-revert-hunk app)
  "Revert file to git HEAD."
  (let* ((buf (current-qt-buffer app)) (path (buffer-file-path buf)) (name (buffer-name buf)))
    (if (not path) (echo-error! (app-state-echo app) "Buffer has no file")
      (with-exception-catcher (lambda (e) (echo-error! (app-state-echo app) "Revert failed"))
        (lambda ()
          (let ((p (open-process (list path: "git" arguments: (list "checkout" "--" path)
                     directory: (path-directory path) stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t))))
            (process-status p)
            (let ((ed (current-qt-editor app)) (text (read-file-as-string path)))
              (when text (qt-plain-text-edit-set-text! ed text)
                (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)))
            (hash-put! *qt-git-hunks* name '())
            (echo-message! (app-state-echo app) "Reverted to git HEAD")))))))

(def (cmd-git-gutter-stage-hunk app)
  "Stage current file (git add)."
  (let* ((buf (current-qt-buffer app)) (path (buffer-file-path buf)))
    (if (not path) (echo-error! (app-state-echo app) "Buffer has no file")
      (with-exception-catcher (lambda (e) (echo-error! (app-state-echo app) "Stage failed"))
        (lambda ()
          (let ((p (open-process (list path: "git" arguments: (list "add" "--" path)
                     directory: (path-directory path) stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t))))
            (process-status p)
            (echo-message! (app-state-echo app) (string-append "Staged: " (path-strip-directory path)))))))))

;; --- Goto last edit ---
(def *qt-last-edit-positions* '())

(def (cmd-goto-last-edit app)
  "Jump to last edit position."
  (if (null? *qt-last-edit-positions*)
    (echo-message! (app-state-echo app) "No edit positions recorded")
    (let* ((entry (car *qt-last-edit-positions*)) (name (car entry)) (pos (cdr entry))
           (ed (current-qt-editor app)))
      (qt-plain-text-edit-set-cursor-position! ed pos)
      (qt-plain-text-edit-ensure-cursor-visible! ed)
      (echo-message! (app-state-echo app) (string-append "Last edit in " name)))))

;; --- Highlight regexp ---
(def (cmd-highlight-regexp app)
  "Highlight text matching a pattern."
  (let ((pat (qt-echo-read-string app "Highlight: ")))
    (when (and pat (> (string-length pat) 0))
      (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
             (tlen (string-length text)) (plen (string-length pat))
             (count (let loop ((i 0) (c 0))
                      (if (> (+ i plen) tlen) c
                        (if (string=? (substring text i (+ i plen)) pat)
                          (loop (+ i 1) (+ c 1)) (loop (+ i 1) c))))))
        (echo-message! (app-state-echo app)
          (string-append "Found " (number->string count) " matches of \"" pat "\""))))))

;; --- Describe char at point ---
(def (cmd-describe-char-at-point app)
  "Describe character at cursor."
  (let* ((ed (current-qt-editor app)) (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)) (len (string-length text)))
    (if (>= pos len) (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos)) (code (char->integer ch)))
        (echo-message! (app-state-echo app)
          (string-append "Char: '" (string ch) "', Code: " (number->string code)
            " (#x" (number->string code 16) ")"))))))

;; --- Org link and open-at-point ---
(def (cmd-org-link app)
  "Insert org link [[url][desc]]."
  (let ((url (qt-echo-read-string app "Link URL: ")))
    (when (and url (> (string-length url) 0))
      (let* ((desc (qt-echo-read-string app "Description: "))
             (link (if (and desc (> (string-length desc) 0))
                     (string-append "[[" url "][" desc "]]") (string-append "[[" url "]]")))
             (ed (current-qt-editor app)) (pos (qt-plain-text-edit-cursor-position ed))
             (text (qt-plain-text-edit-text ed))
             (new (string-append (substring text 0 pos) link (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos (string-length link)))
        (echo-message! (app-state-echo app) "Link inserted")))))

(def (cmd-org-open-at-point app)
  "Open org link at point."
  (let* ((ed (current-qt-editor app)) (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)) (echo (app-state-echo app)))
    (let find-start ((i pos))
      (cond
        ((< i 1) (echo-message! echo "No link at point"))
        ((and (char=? (string-ref text i) #\[) (> i 0) (char=? (string-ref text (- i 1)) #\[))
         (let find-end ((j (+ i 1)))
           (cond
             ((>= j (- (string-length text) 1)) (echo-message! echo "Unclosed link"))
             ((and (char=? (string-ref text j) #\]) (char=? (string-ref text (+ j 1)) #\]))
              (let* ((content (substring text i j))
                     (sep (string-contains content "]["))
                     (url (if sep (substring content 0 sep) content)))
                (cond
                  ((string-prefix? "file:" url)
                   (let ((fpath (substring url 5 (string-length url))))
                     (if (file-exists? fpath)
                       (let* ((fr (app-state-frame app)) (ed2 (current-qt-editor app))
                              (nbuf (qt-buffer-create! (path-strip-directory fpath) ed2 fpath))
                              (ftext (read-file-as-string fpath)))
                         (qt-buffer-attach! ed2 nbuf) (set! (qt-edit-window-buffer (qt-current-window fr)) nbuf)
                         (when ftext (qt-plain-text-edit-set-text! ed2 ftext)
                           (qt-text-document-set-modified! (buffer-doc-pointer nbuf) #f))
                         (echo-message! echo (string-append "Opened: " fpath)))
                       (echo-message! echo (string-append "Not found: " fpath)))))
                  ((or (string-prefix? "http://" url) (string-prefix? "https://" url))
                   (with-exception-catcher (lambda (e) (echo-message! echo "Failed to open URL"))
                     (lambda ()
                       (open-process (list path: "xdg-open" arguments: (list url)
                                      stdin-redirection: #f stdout-redirection: #f stderr-redirection: #f))
                       (echo-message! echo (string-append "Opening: " url)))))
                  (else (echo-message! echo (string-append "Link: " url))))))
             (else (find-end (+ j 1))))))
        (else (find-start (- i 1)))))))

;; --- Org cycle (fold/unfold headings via Scintilla) ---
(def (qt-org-heading? line)
  (and (> (string-length line) 0) (char=? (string-ref line 0) #\*)))

(def (cmd-org-cycle app)
  "Cycle visibility of org heading children."
  (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (cur-line (let lp ((i 0) (p 0))
                     (if (or (>= i (length lines)) (> p pos)) (max 0 (- i 1))
                       (lp (+ i 1) (+ p (string-length (list-ref lines i)) 1))))))
    (if (>= cur-line (length lines))
      (echo-message! (app-state-echo app) "No heading")
      (let ((line (list-ref lines cur-line)))
        (if (not (qt-org-heading? line))
          (echo-message! (app-state-echo app) "Not on a heading")
          (let* ((level (let lp ((i 0)) (if (and (< i (string-length line)) (char=? (string-ref line i) #\*)) (lp (+ i 1)) i)))
                 (end-line (let lp ((i (+ cur-line 1)))
                             (cond ((>= i (length lines)) i)
                                   ((let ((l (list-ref lines i)))
                                      (and (qt-org-heading? l)
                                           (<= (let lp2 ((j 0)) (if (and (< j (string-length l)) (char=? (string-ref l j) #\*)) (lp2 (+ j 1)) j)) level))) i)
                                   (else (lp (+ i 1)))))))
            (if (= end-line (+ cur-line 1))
              (echo-message! (app-state-echo app) "No children")
              ;; Toggle: hide/show children by SCI messages
              (let ((vis (sci-send ed SCI_GETLINEVISIBLE (+ cur-line 1) 0)))
                (if (= vis 1)
                  (let lp ((i (+ cur-line 1))) (when (< i end-line) (sci-send ed SCI_HIDELINES i i) (lp (+ i 1))))
                  (sci-send ed SCI_SHOWLINES (+ cur-line 1) (- end-line 1)))
                (echo-message! (app-state-echo app) (if (= vis 1) "Folded" "Unfolded"))))))))))

(def (cmd-org-shift-tab app)
  "Global org visibility cycling."
  (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline)) (total (length lines)))
    (let* ((some-hidden (let lp ((i 0))
                          (cond ((>= i total) #f)
                                ((and (not (qt-org-heading? (list-ref lines i)))
                                      (= (sci-send ed SCI_GETLINEVISIBLE i 0) 0)) #t)
                                (else (lp (+ i 1)))))))
      (if some-hidden
        (begin (sci-send ed SCI_SHOWLINES 0 (- total 1))
               (echo-message! (app-state-echo app) "All visible"))
        (begin (let lp ((i 0))
                 (when (< i total)
                   (when (not (qt-org-heading? (list-ref lines i)))
                     (sci-send ed SCI_HIDELINES i i))
                   (lp (+ i 1))))
               (echo-message! (app-state-echo app) "Headings only"))))))

;; --- Revert buffer without confirmation ---
(def (cmd-revert-buffer-no-confirm app)
  "Revert buffer from file without asking."
  (let* ((buf (current-qt-buffer app)) (path (buffer-file-path buf)))
    (if (not path) (echo-error! (app-state-echo app) "Buffer has no file")
      (let ((text (read-file-as-string path)))
        (when text
          (let ((ed (current-qt-editor app)))
            (qt-plain-text-edit-set-text! ed text)
            (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
            (echo-message! (app-state-echo app) "Reverted")))))))

;; --- Save all and quit ---
(def (cmd-save-buffers-kill-emacs app)
  "Save modified buffers and quit."
  (echo-message! (app-state-echo app) "Saving and exiting...")
  (set! (app-state-running app) #f))

;; --- Kill compilation ---
(def *qt-compile-proc* #f)

(def (cmd-kill-compilation app)
  "Kill current compilation."
  (if *qt-compile-proc*
    (begin (with-exception-catcher (lambda (e) (void))
             (lambda () (when (port? *qt-compile-proc*) (close-port *qt-compile-proc*))))
      (echo-message! (app-state-echo app) "Compilation killed"))
    (echo-message! (app-state-echo app) "No compilation in progress")))

;; --- Dired refresh ---
(def (cmd-dired-refresh app)
  "Refresh dired buffer listing."
  (let* ((buf (current-qt-buffer app)) (path (buffer-file-path buf)))
    (if (not path) (echo-message! (app-state-echo app) "Not a dired buffer")
      (if (not (file-exists? path)) (echo-message! (app-state-echo app) "Directory not found")
        (with-exception-catcher (lambda (e) (echo-error! (app-state-echo app) "Refresh failed"))
          (lambda ()
            (let* ((p (open-process (list path: "ls" arguments: (list "-la" path)
                        stdin-redirection: #f stdout-redirection: #t stderr-redirection: #t)))
                   (out (read-line p #f)))
              (process-status p)
              (when out
                (let ((ed (current-qt-editor app)))
                  (qt-plain-text-edit-set-text! ed (string-append "Directory: " path "\n\n" out))
                  (qt-plain-text-edit-set-cursor-position! ed 0)
                  (echo-message! (app-state-echo app) "Dired refreshed"))))))))))
