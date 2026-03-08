;;; -*- Gerbil -*-
;;; Qt commands ide2 - text transforms, wgrep, session, workspace, fill, tree
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/format
        :std/misc/completion
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/editor
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/commands-core
        :gemacs/qt/commands-core2
        :gemacs/qt/commands-edit
        :gemacs/qt/commands-edit2
        :gemacs/qt/commands-search
        :gemacs/qt/commands-search2
        :gemacs/qt/commands-file
        :gemacs/qt/commands-file2
        :gemacs/qt/commands-sexp
        :gemacs/qt/commands-sexp2
        (only-in :gemacs/editor-extra-helpers project-current)
        :gemacs/qt/commands-ide)

;;;============================================================================
;;; Text manipulation
;;;============================================================================

(def (cmd-comment-region app)
  "Add comment prefix to each line in region."
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
             (lines (string-split region #\newline))
             (commented (map (lambda (l) (string-append ";; " l)) lines))
             (result (string-join commented "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Commented")))))

(def (cmd-uncomment-region app)
  "Remove comment prefix from each line in region."
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
             (lines (string-split region #\newline))
             (uncommented (map (lambda (l)
                                 (cond
                                   ((string-prefix? ";; " l) (substring l 3 (string-length l)))
                                   ((string-prefix? ";" l) (substring l 1 (string-length l)))
                                   ((string-prefix? "# " l) (substring l 2 (string-length l)))
                                   ((string-prefix? "//" l) (substring l 2 (string-length l)))
                                   (else l)))
                               lines))
             (result (string-join uncommented "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Uncommented")))))

(def (cmd-collapse-blank-lines app)
  "Collapse multiple blank lines into one."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (result (let loop ((s text))
                   (let ((idx (string-contains s "\n\n\n")))
                     (if idx
                       (loop (string-append (substring s 0 (+ idx 1))
                                            (substring s (+ idx 2) (string-length s))))
                       s)))))
    (qt-plain-text-edit-set-text! ed result)
    (echo-message! (app-state-echo app) "Blank lines collapsed")))

(def (cmd-remove-blank-lines app)
  "Remove all blank lines from region or buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf))
         (text (qt-plain-text-edit-text ed)))
    (let-values (((start end)
                  (if mark
                    (let ((pos (qt-plain-text-edit-cursor-position ed)))
                      (values (min mark pos) (max mark pos)))
                    (values 0 (string-length text)))))
      (let* ((region (substring text start end))
             (lines (string-split region #\newline))
             (non-blank (filter (lambda (l) (> (string-length (string-trim-both l)) 0)) lines))
             (result (string-join non-blank "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (when mark (set! (buffer-mark buf) #f))
        (echo-message! (app-state-echo app) "Blank lines removed")))))

(def (cmd-delete-trailing-lines app)
  "Delete trailing blank lines at end of buffer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (trimmed (let loop ((s text))
                    (if (string-suffix? "\n\n" s)
                      (loop (substring s 0 (- (string-length s) 1)))
                      s))))
    (qt-plain-text-edit-set-text! ed trimmed)
    (echo-message! (app-state-echo app) "Trailing lines deleted")))

(def (cmd-trim-lines app)
  "Trim trailing whitespace from all lines."
  (cmd-delete-trailing-whitespace app))

(def (cmd-prefix-lines app)
  "Add a prefix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((prefix (qt-echo-read-string app "Prefix: ")))
        (when prefix
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (prefixed (map (lambda (l) (string-append prefix l)) lines))
                 (result (string-join prefixed "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)))))))

(def (cmd-suffix-lines app)
  "Add a suffix to each line in region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! (app-state-echo app) "No region")
      (let ((suffix (qt-echo-read-string app "Suffix: ")))
        (when suffix
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end))
                 (lines (string-split region #\newline))
                 (suffixed (map (lambda (l) (string-append l suffix)) lines))
                 (result (string-join suffixed "\n"))
                 (new-text (string-append (substring text 0 start) result
                                          (substring text end (string-length text)))))
            (qt-plain-text-edit-set-text! ed new-text)
            (qt-plain-text-edit-set-cursor-position! ed start)
            (set! (buffer-mark buf) #f)))))))

;;;============================================================================
;;; Sort variants
;;;============================================================================

(def (cmd-sort-lines-reverse app)
  "Sort lines in region in reverse order."
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
             (lines (string-split region #\newline))
             (sorted (sort lines string>?))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted (reverse)")))))

(def (cmd-sort-lines-case-fold app)
  "Sort lines case-insensitively."
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
             (lines (string-split region #\newline))
             (sorted (sort lines (lambda (a b) (string<? (string-downcase a) (string-downcase b)))))
             (result (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start) result
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Sorted (case-insensitive)")))))

(def (cmd-uniquify-lines app)
  "Remove duplicate lines in region."
  (cmd-delete-duplicate-lines app))

(def (cmd-sort-words app)
  "Sort words on the current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (words (filter (lambda (s) (> (string-length s) 0))
                            (string-split line-text #\space)))
             (sorted-words (sort words string<?))
             (new-line (string-join sorted-words " "))
             (new-lines (let loop ((ls lines) (i 0) (acc '()))
                          (if (null? ls) (reverse acc)
                            (loop (cdr ls) (+ i 1)
                                  (cons (if (= i line) new-line (car ls)) acc)))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (line-start-position new-text line))
        (echo-message! (app-state-echo app) "Words sorted")))))

;;;============================================================================
;;; Case conversion helpers
;;;============================================================================

(def (cmd-camel-to-snake app)
  "Convert camelCase word at point to snake_case."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\_))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (snake (let loop ((i 0) (acc ""))
                      (if (>= i (string-length word)) acc
                        (let ((ch (string-ref word i)))
                          (if (and (char-upper-case? ch) (> i 0))
                            (loop (+ i 1) (string-append acc "_" (string (char-downcase ch))))
                            (loop (+ i 1) (string-append acc (string (char-downcase ch)))))))))
             (new-text (string-append (substring text 0 start) snake
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length snake)))))))

(def (cmd-snake-to-camel app)
  "Convert snake_case word at point to camelCase."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1)))
                                       (char=? (string-ref text (- i 1)) #\_))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i))
                                     (char=? (string-ref text i) #\_))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (camel (let loop ((i 0) (acc "") (cap? #f))
                      (if (>= i (string-length word)) acc
                        (let ((ch (string-ref word i)))
                          (if (char=? ch #\_)
                            (loop (+ i 1) acc #t)
                            (loop (+ i 1)
                                  (string-append acc (string (if cap? (char-upcase ch) ch)))
                                  #f))))))
             (new-text (string-append (substring text 0 start) camel
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ start (string-length camel)))))))

;;;============================================================================
;;; Search helpers
;;;============================================================================

(def (cmd-highlight-word-at-point app)
  "Highlight the word at point as search term."
  (cmd-highlight-symbol app))


;;;============================================================================
;;; Wgrep — editable grep results
;;;============================================================================

(def *wgrep-mode* #f) ;; #t when *Grep* buffer is in wgrep edit mode
(def *wgrep-original-lines* []) ;; original grep result lines for diffing

(def (cmd-wgrep-change-to-wgrep-mode app)
  "Make *Grep* buffer editable for wgrep-style batch editing."
  (let ((buf (current-qt-buffer app)))
    (if (not (string=? (buffer-name buf) "*Grep*"))
      (echo-error! (app-state-echo app) "Not in *Grep* buffer")
      (begin
        (set! *wgrep-mode* #t)
        ;; Store original result lines for later comparison
        (set! *wgrep-original-lines*
          (map (lambda (r)
                 (string-append (car r) ":"
                   (number->string (cadr r)) ":"
                   (caddr r)))
               *grep-results*))
        (echo-message! (app-state-echo app)
          "Wgrep mode: edit results, then C-c C-c to apply or C-c C-k to abort")))))

(def (cmd-wgrep-finish-edit app)
  "Apply wgrep changes back to source files."
  (let ((buf (current-qt-buffer app)))
    (if (not (and (string=? (buffer-name buf) "*Grep*") *wgrep-mode*))
      (echo-error! (app-state-echo app) "Not in wgrep mode")
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (all-lines (let loop ((s text) (acc []))
                          (let ((nl (string-index s #\newline)))
                            (if nl
                              (loop (substring s (+ nl 1) (string-length s))
                                    (cons (substring s 0 nl) acc))
                              (reverse (if (> (string-length s) 0) (cons s acc) acc))))))
             ;; Extract only file:line:text result lines (skip header)
             (result-lines (filter (lambda (l) (parse-grep-line l)) all-lines))
             (changes 0))
        ;; Group changes by file
        (let ((file-changes (make-hash-table)))
          (for-each
            (lambda (line)
              (let ((parsed (parse-grep-line line)))
                (when parsed
                  (let ((file (car parsed))
                        (line-num (cadr parsed))
                        (new-text (caddr parsed)))
                    ;; Check if line changed from original
                    (let* ((orig (let loop ((results *grep-results*))
                                  (if (null? results) #f
                                    (let ((r (car results)))
                                      (if (and (string=? (car r) file)
                                               (= (cadr r) line-num))
                                        (caddr r)
                                        (loop (cdr results)))))))
                           (changed? (and orig (not (string=? orig new-text)))))
                      (when changed?
                        (let ((existing (or (hash-get file-changes file) [])))
                          (hash-put! file-changes file
                            (cons (cons line-num new-text) existing)))))))))
            result-lines)
          ;; Compute changes for each file, then write in background
          (let ((write-jobs []))
            (hash-for-each
              (lambda (file line-edits)
                (when (file-exists? file)
                  (let* ((content (read-file-as-string file))
                         (lines (let loop ((s content) (acc []))
                                  (let ((nl (string-index s #\newline)))
                                    (if nl
                                      (loop (substring s (+ nl 1) (string-length s))
                                            (cons (substring s 0 nl) acc))
                                      (reverse (if (> (string-length s) 0) (cons s acc) acc))))))
                         (new-lines
                           (let loop ((ls lines) (i 1) (acc []))
                             (if (null? ls) (reverse acc)
                               (let ((edit (assoc i line-edits)))
                                 (loop (cdr ls) (+ i 1)
                                       (cons (if edit (cdr edit) (car ls)) acc))))))
                         (new-content (string-join new-lines "\n")))
                    ;; Add trailing newline if original had one
                    (let ((final (if (and (> (string-length content) 0)
                                         (char=? (string-ref content (- (string-length content) 1))
                                                 #\newline))
                                   (string-append new-content "\n")
                                   new-content)))
                      (set! write-jobs (cons [file . final] write-jobs))
                      (set! changes (+ changes (length line-edits)))))))
              file-changes)
            ;; Write all files in background thread
            (when (pair? write-jobs)
              (spawn/name 'wgrep-write
                (lambda ()
                  (for-each
                    (lambda (job)
                      (with-catch
                        (lambda (e) (gemacs-log! "wgrep write error: " (##object->string e)))
                        (lambda () (write-string-to-file (car job) (cdr job)))))
                    write-jobs))))))
        (set! *wgrep-mode* #f)
        (echo-message! (app-state-echo app)
          (string-append "Applied " (number->string changes) " change(s)"))))))

(def (cmd-wgrep-abort-changes app)
  "Abort wgrep changes and restore original *Grep* buffer."
  (if (not *wgrep-mode*)
    (echo-error! (app-state-echo app) "Not in wgrep mode")
    (begin
      (set! *wgrep-mode* #f)
      ;; Restore original content
      (let* ((ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (header (string-append "-*- grep -*-\n\n"))
             (result-text (if (null? *wgrep-original-lines*)
                            (string-append header "No matches found.\n")
                            (string-append header
                              (number->string (length *wgrep-original-lines*)) " matches\n\n"
                              (string-join *wgrep-original-lines* "\n")
                              "\n\nPress Enter on a result line to jump to source."))))
        (qt-plain-text-edit-set-text! ed result-text)
        (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0))
      (echo-message! (app-state-echo app) "Wgrep changes aborted"))))

;;;============================================================================
;;; Quoted insert
;;;============================================================================

(def *qt-quoted-insert-pending* #f)

(def (cmd-quoted-insert app)
  "Insert the next character literally (C-q). Sets a flag so the next keypress
   is inserted as a literal character instead of being executed as a command."
  (set! *qt-quoted-insert-pending* #t)
  (echo-message! (app-state-echo app) "C-q: "))

(def (qt-quoted-insert-handle! app text)
  "Handle the next key after C-q by inserting it literally."
  (set! *qt-quoted-insert-pending* #f)
  (let ((ed (qt-current-editor (app-state-frame app))))
    (when (and text (> (string-length text) 0))
      (qt-plain-text-edit-insert-text! ed text)
      (echo-message! (app-state-echo app)
        (string-append "Inserted: " text)))))

;;;============================================================================
;;; Quick calc
;;;============================================================================

(def (cmd-quick-calc app)
  "Quick calculator (alias for calc)."
  (cmd-calc app))

;;;============================================================================
;;; Eval and insert
;;;============================================================================

(def (cmd-eval-and-insert app)
  "Evaluate expression and insert result at point."
  (let ((expr (qt-echo-read-string app "Eval and insert: ")))
    (when (and expr (> (string-length expr) 0))
      (let-values (((result error?) (eval-expression-string expr)))
        (if error?
          (echo-error! (app-state-echo app) (string-append "Error: " result))
          (qt-plain-text-edit-insert-text! (current-qt-editor app) result))))))

;;;============================================================================
;;; Shell command insert
;;;============================================================================

(def (cmd-shell-command-insert app)
  "Run shell command and insert output at point."
  (let ((cmd (qt-echo-read-string app "Shell command (insert): ")))
    (when cmd
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Command failed"))
        (lambda ()
          (let* ((proc (open-process
                          (list path: "/bin/sh"
                                arguments: (list "-c" cmd)
                                stdout-redirection: #t)))
                 (output (read-line proc #f))
                 (_ (process-status proc)))
            (close-port proc)
            (when output
              (qt-plain-text-edit-insert-text! (current-qt-editor app) output))))))))

;;;============================================================================
;;; Pipe region
;;;============================================================================

(def (cmd-pipe-region app)
  "Pipe region through shell command."
  (cmd-shell-command-on-region app))


;;;============================================================================
;;; Session persistence (desktop save/restore)
;;;============================================================================

(def *session-path*
  (path-expand ".gemacs-session" (user-info-home (user-info (user-name)))))

(def (session-save! app)
  "Save current session (open file buffers + positions) to disk."
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((ed (current-qt-editor app))
             (fr (app-state-frame app))
             (current-buf (current-qt-buffer app))
             ;; Collect file buffers with positions
             (entries
               (let loop ((bufs *buffer-list*) (acc []))
                 (if (null? bufs) (reverse acc)
                   (let ((buf (car bufs)))
                     (if (buffer-file-path buf)
                       ;; Get position: attach temporarily to read cursor pos
                       (let ((pos (begin
                                    (qt-buffer-attach! ed buf)
                                    (qt-plain-text-edit-cursor-position ed))))
                         (loop (cdr bufs) (cons (cons (buffer-file-path buf) pos) acc)))
                       (loop (cdr bufs) acc)))))))
        ;; Restore current buffer
        (qt-buffer-attach! ed current-buf)
        ;; Write session file
        (call-with-output-file *session-path*
          (lambda (port)
            ;; First line: current buffer path
            (display (or (buffer-file-path current-buf) "") port)
            (newline port)
            ;; Remaining lines: file\tposition
            (for-each
              (lambda (entry)
                (display (car entry) port)
                (display "\t" port)
                (display (number->string (cdr entry)) port)
                (newline port))
              entries)))))))

(def (session-restore-files)
  "Read session file and return list of (file-path . cursor-pos) plus current-file."
  (with-catch
    (lambda (e) (values #f []))
    (lambda ()
      (if (not (file-exists? *session-path*))
        (values #f [])
        (call-with-input-file *session-path*
          (lambda (port)
            (let ((current-file (read-line port)))
              (let loop ((acc []))
                (let ((line (read-line port)))
                  (if (eof-object? line)
                    (values (if (and (string? current-file)
                                    (> (string-length current-file) 0))
                              current-file #f)
                            (reverse acc))
                    (let ((parts (split-by-tab line)))
                      (if (and (= (length parts) 2)
                               (string->number (cadr parts)))
                        (loop (cons (cons (car parts) (string->number (cadr parts))) acc))
                        (loop acc)))))))))))))

(def (cmd-session-save app)
  "Save current session."
  (session-save! app)
  (echo-message! (app-state-echo app) "Session saved"))

(def (cmd-session-restore app)
  "Restore last session."
  (echo-message! (app-state-echo app) "Use --restore flag to restore session on startup"))

;;;============================================================================
;;; Duplicate region
;;;============================================================================

(def (cmd-duplicate-region app)
  "Duplicate the selected region."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (cmd-duplicate-line app)
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (new-text (string-append (substring text 0 end) region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ end (string-length region)))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region duplicated")))))

;;;============================================================================
;;; Reverse chars/word
;;;============================================================================

(def (cmd-reverse-chars app)
  "Reverse characters in region."
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
             (reversed (list->string (reverse (string->list region))))
             (new-text (string-append (substring text 0 start) reversed
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Reversed")))))

(def (cmd-reverse-word app)
  "Reverse the word at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (when (< pos len)
      (let* ((start (let loop ((i pos))
                      (if (or (= i 0)
                              (not (or (char-alphabetic? (string-ref text (- i 1)))
                                       (char-numeric? (string-ref text (- i 1))))))
                        i (loop (- i 1)))))
             (end (let loop ((i pos))
                    (if (or (>= i len)
                            (not (or (char-alphabetic? (string-ref text i))
                                     (char-numeric? (string-ref text i)))))
                      i (loop (+ i 1)))))
             (word (substring text start end))
             (reversed (list->string (reverse (string->list word))))
             (new-text (string-append (substring text 0 start) reversed
                                      (substring text end len))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed end)))))

;;;============================================================================
;;; Environment
;;;============================================================================

(def (cmd-getenv app)
  "Display an environment variable."
  (let ((var (qt-echo-read-string app "Environment variable: ")))
    (when var
      (let ((val (getenv var #f)))
        (echo-message! (app-state-echo app)
          (if val (string-append var "=" val)
            (string-append var " is not set")))))))

(def (cmd-setenv app)
  "Set an environment variable."
  (let ((var (qt-echo-read-string app "Variable name: ")))
    (when var
      (let ((val (qt-echo-read-string app (string-append var "="))))
        (when val
          (setenv var val)
          (echo-message! (app-state-echo app)
            (string-append "Set " var "=" val)))))))

;;;============================================================================
;;; Hl-todo — highlight TODO/FIXME/HACK keywords
;;;============================================================================

(def *hl-todo-mode* #f)
(def *hl-todo-keywords* '("TODO" "FIXME" "HACK" "BUG" "XXX" "NOTE"))

(def (cmd-hl-todo-mode app)
  "Toggle hl-todo mode — highlights TODO keywords."
  (set! *hl-todo-mode* (not *hl-todo-mode*))
  (echo-message! (app-state-echo app)
    (if *hl-todo-mode* "HL-todo: on" "HL-todo: off")))

(def (cmd-hl-todo-next app)
  "Jump to next TODO/FIXME/HACK keyword."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (next-pos #f))
    (for-each
      (lambda (kw)
        (let ((found (string-contains text kw (+ pos 1))))
          (when (and found (or (not next-pos) (< found next-pos)))
            (set! next-pos found))))
      *hl-todo-keywords*)
    (if next-pos
      (begin (qt-plain-text-edit-set-cursor-position! ed next-pos)
             (echo-message! (app-state-echo app) "Found TODO keyword"))
      (echo-message! (app-state-echo app) "No more TODO keywords"))))

(def (cmd-hl-todo-previous app)
  "Jump to previous TODO/FIXME/HACK keyword."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (prev-pos #f))
    (for-each
      (lambda (kw)
        (let loop ((search-from 0))
          (let ((found (string-contains text kw search-from)))
            (when (and found (< found pos))
              (when (or (not prev-pos) (> found prev-pos))
                (set! prev-pos found))
              (loop (+ found 1))))))
      *hl-todo-keywords*)
    (if prev-pos
      (begin (qt-plain-text-edit-set-cursor-position! ed prev-pos)
             (echo-message! (app-state-echo app) "Found TODO keyword"))
      (echo-message! (app-state-echo app) "No previous TODO keywords"))))

;;;============================================================================
;;; Shell command framework (plan items 0.3 + 3.2)
;;;============================================================================

(def (shell-command-to-string cmd)
  "Run CMD via /bin/sh and return stdout as a string. Returns empty string on error."
  (with-catch
    (lambda (e) "")
    (lambda ()
      (let* ((proc (open-process
                      (list path: "/bin/sh"
                            arguments: ["-c" cmd]
                            stdout-redirection: #t
                            stderr-redirection: #t
                            pseudo-terminal: #f)))
             (output (read-line proc #f)))
        (process-status proc)
        (close-port proc)
        (or output "")))))

(def (shell-command-to-buffer! app cmd buffer-name . opts)
  "Run CMD, display output in BUFFER-NAME. Options: read-only: #t (default #t)."
  (let* ((read-only? (if (and (pair? opts) (pair? (car opts)))
                       (let ((ro (assoc read-only: opts)))
                         (if ro (cdr ro) #t))
                       #t))
         (result (with-catch
                   (lambda (e)
                     (string-append "Error: "
                       (with-output-to-string (lambda () (display-exception e)))))
                   (lambda ()
                     (shell-command-to-string cmd))))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (or (buffer-by-name buffer-name)
                  (qt-buffer-create! buffer-name ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed result)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (when read-only?
      (qt-plain-text-edit-set-read-only! ed #t))
    (qt-modeline-update! app)
    result))

(def *user-shell-commands* (make-hash-table))

(def (register-shell-command! name prompt command-template buffer-template)
  "Register a user-definable shell command.
   PROMPT is shown when reading input.
   COMMAND-TEMPLATE is a format string with ~a for the input.
   BUFFER-TEMPLATE is a format string for the buffer name."
  (hash-put! *user-shell-commands* name
    (list prompt command-template buffer-template)))

(def (cmd-run-user-shell-command app)
  "Run a registered user shell command by name."
  (let* ((names (sort (hash-keys *user-shell-commands*) string<?))
         (name (if (null? names)
                 (begin (echo-message! (app-state-echo app) "No shell commands registered")
                        #f)
                 (qt-echo-read-string app
                   (string-append "Shell command ("
                     (string-join (map symbol->string names) ", ") "): ")))))
    (when (and name (> (string-length name) 0))
      (let ((entry (hash-get *user-shell-commands* (string->symbol name))))
        (if entry
          (let* ((prompt (car entry))
                 (cmd-template (cadr entry))
                 (buf-template (caddr entry))
                 (input (qt-echo-read-string app prompt)))
            (when (and input (> (string-length input) 0))
              (let ((cmd (format cmd-template input))
                    (buf-name (format buf-template input)))
                (shell-command-to-buffer! app cmd buf-name)
                (echo-message! (app-state-echo app)
                  (string-append "Ran: " cmd)))))
          (echo-message! (app-state-echo app)
            (string-append "Unknown command: " name)))))))

;;;============================================================================
;;; Workspaces / Perspectives (plan item 2.3)
;;;============================================================================

;; Each workspace: (name . buffer-names)
;; buffer-names is a list of buffer name strings visible in this workspace
(def *workspaces* (make-hash-table))  ; name -> list of buffer-name strings
(def *current-workspace* "default")
(def *workspace-buffers* (make-hash-table))  ; workspace -> current-buffer-name

(def (workspace-init! app)
  "Initialize the default workspace with all current buffers."
  (hash-put! *workspaces* "default"
    (map buffer-name (buffer-list)))
  (let ((buf (qt-current-buffer (app-state-frame app))))
    (when buf
      (hash-put! *workspace-buffers* "default" (buffer-name buf)))))

(def (workspace-add-buffer! ws-name buf-name)
  "Add a buffer to a workspace's buffer list."
  (let ((bufs (or (hash-get *workspaces* ws-name) [])))
    (unless (member buf-name bufs)
      (hash-put! *workspaces* ws-name (cons buf-name bufs)))))

(def (workspace-remove-buffer! ws-name buf-name)
  "Remove a buffer from a workspace."
  (let ((bufs (or (hash-get *workspaces* ws-name) [])))
    (hash-put! *workspaces* ws-name
      (filter (lambda (b) (not (string=? b buf-name))) bufs))))

(def (cmd-workspace-create app)
  "Create a new named workspace."
  (let ((name (qt-echo-read-string app "New workspace name: ")))
    (when (and name (> (string-length name) 0))
      (if (hash-get *workspaces* name)
        (echo-message! (app-state-echo app)
          (string-append "Workspace '" name "' already exists"))
        (begin
          (hash-put! *workspaces* name ["*scratch*"])
          (echo-message! (app-state-echo app)
            (string-append "Created workspace: " name)))))))

(def (cmd-workspace-switch app)
  "Switch to a named workspace."
  (let* ((names (sort (hash-keys *workspaces*) string<?))
         (prompt (string-append "Switch workspace ("
                   (string-join names ", ") "): "))
         (name (qt-echo-read-string app prompt)))
    (when (and name (> (string-length name) 0))
      (let ((bufs (hash-get *workspaces* name)))
        (if bufs
          (begin
            ;; Save current workspace's active buffer
            (let ((cur-buf (qt-current-buffer (app-state-frame app))))
              (when cur-buf
                (hash-put! *workspace-buffers* *current-workspace*
                  (buffer-name cur-buf))))
            ;; Switch to new workspace
            (set! *current-workspace* name)
            ;; Restore the workspace's active buffer
            (let* ((active (hash-get *workspace-buffers* name))
                   (target (and active (buffer-by-name active))))
              (when target
                (let* ((fr (app-state-frame app))
                       (ed (current-qt-editor app)))
                  (qt-buffer-attach! ed target)
                  (set! (qt-edit-window-buffer (qt-current-window fr)) target))))
            (echo-message! (app-state-echo app)
              (string-append "Workspace: " name
                " (" (number->string (length bufs)) " buffers)")))
          (echo-message! (app-state-echo app)
            (string-append "No workspace: " name)))))))

(def (cmd-workspace-delete app)
  "Delete a workspace (cannot delete default)."
  (let* ((names (sort (filter (lambda (n) (not (string=? n "default")))
                        (hash-keys *workspaces*)) string<?))
         (name (if (null? names)
                 (begin (echo-message! (app-state-echo app) "No deletable workspaces")
                        #f)
                 (qt-echo-read-string app
                   (string-append "Delete workspace (" (string-join names ", ") "): ")))))
    (when (and name (> (string-length name) 0))
      (cond
        ((string=? name "default")
         (echo-message! (app-state-echo app) "Cannot delete default workspace"))
        ((hash-get *workspaces* name)
         (hash-remove! *workspaces* name)
         (hash-remove! *workspace-buffers* name)
         (when (string=? *current-workspace* name)
           (set! *current-workspace* "default"))
         (echo-message! (app-state-echo app)
           (string-append "Deleted workspace: " name)))
        (else
         (echo-message! (app-state-echo app)
           (string-append "No workspace: " name)))))))

(def (cmd-workspace-add-buffer app)
  "Add current buffer to a workspace."
  (let* ((buf (qt-current-buffer (app-state-frame app)))
         (buf-name (buffer-name buf)))
    (workspace-add-buffer! *current-workspace* buf-name)
    (echo-message! (app-state-echo app)
      (string-append "Added '" buf-name "' to workspace '" *current-workspace* "'"))))

(def (cmd-workspace-list app)
  "List all workspaces and their buffers."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (lines
           (let loop ((names (sort (hash-keys *workspaces*) string<?)) (acc []))
             (if (null? names)
               (reverse acc)
               (let* ((name (car names))
                      (bufs (or (hash-get *workspaces* name) []))
                      (active? (string=? name *current-workspace*))
                      (header (string-append
                                (if active? "* " "  ")
                                name " (" (number->string (length bufs)) " buffers)"))
                      (buf-lines (map (lambda (b) (string-append "    " b)) bufs)))
                 (loop (cdr names)
                   (append (reverse (cons header buf-lines)) acc))))))
         (content (string-join lines "\n"))
         (buf (or (buffer-by-name "*Workspaces*")
                  (qt-buffer-create! "*Workspaces*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed content)
    (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (qt-modeline-update! app)
    (echo-message! (app-state-echo app)
      (string-append "Current: " *current-workspace*))))

;;;============================================================================
;;; Multiple Cursors (Scintilla multi-selection API)
;;;============================================================================

;; Enable multiple selection mode (needed once)
(def (qt-enable-multiple-selection! ed)
  "Enable Scintilla multiple-selection and additional-selection-typing."
  ;; SCI_SETMULTIPLESELECTION=2563, SCI_SETADDITIONALSELECTIONTYPING=2565
  (sci-send ed 2563 1 0)
  (sci-send ed 2565 1 0))

(def (cmd-mc-mark-next app)
  "Add a cursor at the next occurrence of the current selection."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (qt-enable-multiple-selection! ed)
    (let* ((sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
           (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
      (if (= sel-start sel-end)
        (echo-error! echo "Select text first, then mark next")
        (begin
          (sci-send ed SCI_MULTIPLESELECTADDNEXT 0 0)
          (let ((n (sci-send ed SCI_GETSELECTIONS 0 0)))
            (echo-message! echo
              (string-append (number->string n) " cursors"))))))))

(def (cmd-mc-mark-all app)
  "Add cursors at all occurrences of the current selection."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app)))
    (qt-enable-multiple-selection! ed)
    (let* ((sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
           (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
      (if (= sel-start sel-end)
        (echo-error! echo "Select text first, then mark all")
        (begin
          (sci-send ed SCI_MULTIPLESELECTADDEACH 0 0)
          (let ((n (sci-send ed SCI_GETSELECTIONS 0 0)))
            (echo-message! echo
              (string-append (number->string n) " cursors"))))))))

(def (cmd-mc-skip-and-mark-next app)
  "Skip the current selection and add next occurrence."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
         (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
    (if (= sel-start sel-end)
      (echo-error! echo "Select text first")
      (begin
        (qt-enable-multiple-selection! ed)
        (let ((n (sci-send ed SCI_GETSELECTIONS 0 0)))
          (when (> n 1)
            (let ((main (sci-send ed SCI_GETMAINSELECTION 0 0)))
              (sci-send ed SCI_DROPSELECTIONN main 0))))
        (sci-send ed SCI_MULTIPLESELECTADDNEXT 0 0)
        (let ((n2 (sci-send ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n2) " cursors")))))))

(def (cmd-mc-edit-lines app)
  "Add a cursor at the end of each line in the current selection."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (sel-start (sci-send ed SCI_GETSELECTIONSTART 0 0))
         (sel-end (sci-send ed SCI_GETSELECTIONEND 0 0)))
    (if (= sel-start sel-end)
      (echo-error! echo "Select a region first")
      (begin
        (qt-enable-multiple-selection! ed)
        (let* ((start-line (sci-send ed SCI_LINEFROMPOSITION sel-start 0))
               (end-line (sci-send ed SCI_LINEFROMPOSITION sel-end 0))
               (num-lines (+ 1 (- end-line start-line))))
          (when (> num-lines 1)
            ;; Set first selection at end of first line
            (let ((eol0 (sci-send ed SCI_GETLINEENDPOSITION start-line 0)))
              (sci-send ed SCI_SETSELECTION eol0 eol0)
              (let loop ((line (+ start-line 1)))
                (when (<= line end-line)
                  (let ((eol (sci-send ed SCI_GETLINEENDPOSITION line 0)))
                    (sci-send ed SCI_ADDSELECTION eol eol)
                    (loop (+ line 1)))))))
          (echo-message! echo
            (string-append (number->string num-lines)
                           " cursors on " (number->string num-lines) " lines")))))))

(def (cmd-mc-unmark-last app)
  "Remove the most recently added cursor."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (n (sci-send ed SCI_GETSELECTIONS 0 0)))
    (if (<= n 1)
      (echo-message! echo "Only one cursor")
      (begin
        (sci-send ed SCI_DROPSELECTIONN (- n 1) 0)
        (echo-message! echo
          (string-append (number->string (- n 1)) " cursors"))))))

(def (cmd-mc-rotate app)
  "Cycle to the next selection as the main cursor."
  (let ((ed (current-qt-editor app)))
    (sci-send ed SCI_ROTATESELECTION 0 0)))

;;;============================================================================
;;; fill-region, insert-buffer, prepend-to-buffer, copy-rectangle-to-register
;;;============================================================================

(def (qt-fill-words words col)
  "Reflow WORDS list to COL width, returning string."
  (if (null? words) ""
    (let loop ((ws (cdr words)) (line (car words)) (lines []))
      (if (null? ws)
        (string-join (reverse (cons line lines)) "\n")
        (let ((next (string-append line " " (car ws))))
          (if (> (string-length next) col)
            (if (string=? line "")
              (loop (cdr ws) "" (cons (car ws) lines))
              (loop ws "" (cons line lines)))
            (loop (cdr ws) next lines)))))))

(def (cmd-fill-region app)
  "Fill (word-wrap) the selected region at fill-column."
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
             (words (filter (lambda (w) (> (string-length w) 0))
                            (string-split (string-trim region) #\space)))
             (filled (qt-fill-words words *fill-column*)))
        (qt-plain-text-edit-set-selection! ed start end)
        (qt-plain-text-edit-remove-selected-text! ed)
        (qt-plain-text-edit-insert-text! ed filled)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Region filled")))))

(def (cmd-insert-buffer app)
  "Insert the contents of another buffer at point."
  (let* ((bufs (buffer-list))
         (names (map buffer-name bufs))
         (target-name (qt-echo-read-string-with-completion app "Insert buffer: " names)))
    (when (and target-name (> (string-length target-name) 0))
      (let ((target-buf (find (lambda (b) (string=? (buffer-name b) target-name)) bufs)))
        (if (not target-buf)
          (echo-error! (app-state-echo app)
            (string-append "No buffer: " target-name))
          ;; Find editor showing target buffer to get its text
          (let* ((fr (app-state-frame app))
                 (target-text
                   (let loop ((wins (qt-frame-windows fr)))
                     (cond
                       ((null? wins) #f)
                       ((eq? (qt-edit-window-buffer (car wins)) target-buf)
                        (qt-plain-text-edit-text (qt-edit-window-editor (car wins))))
                       (else (loop (cdr wins)))))))
            (if target-text
              (let ((ed (current-qt-editor app)))
                (qt-plain-text-edit-insert-text! ed target-text)
                (echo-message! (app-state-echo app)
                  (string-append "Inserted buffer " target-name)))
              (echo-error! (app-state-echo app)
                (string-append "Buffer " target-name " not visible in any window")))))))))

(def (cmd-prepend-to-buffer app)
  "Prepend region to another buffer."
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
             (target-name (qt-echo-read-string-with-completion app "Prepend to buffer: " names)))
        (when target-name
          (let ((target-buf (find (lambda (b) (string=? (buffer-name b) target-name)) bufs)))
            (if target-buf
              (let ((fr (app-state-frame app)))
                (let loop ((wins (qt-frame-windows fr)))
                  (when (pair? wins)
                    (if (eq? (qt-edit-window-buffer (car wins)) target-buf)
                      (let ((target-ed (qt-edit-window-editor (car wins))))
                        ;; Move to beginning and insert
                        (qt-plain-text-edit-set-cursor-position! target-ed 0)
                        (qt-plain-text-edit-insert-text! target-ed region))
                      (loop (cdr wins)))))
                (echo-message! (app-state-echo app)
                  (string-append "Prepended to " target-name)))
              (echo-error! (app-state-echo app) "Buffer not found"))))))))

(def (cmd-copy-rectangle-to-register app)
  "Copy rectangle (region interpreted as column block) to a register."
  (let* ((input (qt-echo-read-string app "Copy rectangle to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (mark (buffer-mark buf)))
        (if (not mark)
          (echo-error! echo "No mark set")
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (lines (string-split text #\newline))
                 (col1 (column-at-position text start))
                 (col2 (column-at-position text end))
                 (left-col (min col1 col2))
                 (right-col (max col1 col2)))
            (let-values (((start-line end-line) (region-line-range text start end)))
              (let ((rect-lines
                      (let loop ((i start-line) (acc []))
                        (if (> i end-line) (reverse acc)
                          (let* ((l (if (< i (length lines)) (list-ref lines i) ""))
                                 (llen (string-length l))
                                 (s (min left-col llen))
                                 (e (min right-col llen)))
                            (loop (+ i 1) (cons (substring l s e) acc)))))))
                (hash-put! (app-state-registers app) reg
                  (string-join rect-lines "\n"))
                (set! (buffer-mark buf) #f)
                (echo-message! echo
                  (string-append "Rectangle copied to register " (string reg)))))))))))

;;; Undo tree visualization for Qt layer
(def (cmd-undo-tree-visualize app)
  "Show undo history as a visual tree for the current buffer."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (buf-name (buffer-name buf))
         (echo (app-state-echo app))
         (current-text (qt-plain-text-edit-text ed))
         (history (or (hash-get *undo-history* buf-name) [])))
    ;; Record current state
    (undo-history-record! buf-name current-text)
    (let ((history (or (hash-get *undo-history* buf-name) [])))
      (if (null? history)
        (echo-message! echo "No undo history for this buffer")
        (let* ((now (inexact->exact (floor (time->seconds (current-time)))))
               (fr (app-state-frame app))
               (tree-lines
                 (let loop ((entries history) (i 0) (acc []))
                   (if (null? entries) (reverse acc)
                     (let* ((e (car entries))
                            (ts (car e))
                            (text (cdr e))
                            (tlen (string-length text))
                            (line-count (let lp ((j 0) (c 1))
                                          (cond ((>= j tlen) c)
                                                ((char=? (string-ref text j) #\newline)
                                                 (lp (+ j 1) (+ c 1)))
                                                (else (lp (+ j 1) c)))))
                            (age (- now ts))
                            (age-str (cond
                                       ((< age 60) (string-append (number->string age) "s ago"))
                                       ((< age 3600) (string-append (number->string (quotient age 60)) "m ago"))
                                       ((< age 86400) (string-append (number->string (quotient age 3600)) "h ago"))
                                       (else (string-append (number->string (quotient age 86400)) "d ago"))))
                            (marker (if (= i 0) " *" ""))
                            (connector (if (= i 0) "o" "|"))
                            (preview (let ((sub (substring text 0 (min 50 tlen))))
                                       (let ((nl (string-contains sub "\n")))
                                         (if nl (substring sub 0 nl) sub)))))
                       (loop (cdr entries) (+ i 1)
                         (cons (string-append
                                 "  " connector "-- [" (number->string i) "] "
                                 age-str "  " (number->string tlen) " chars, "
                                 (number->string line-count) " lines" marker
                                 "\n  |   " preview)
                               acc))))))
               (header (string-append
                         "Undo Tree: " buf-name "\n"
                         "Snapshots: " (number->string (length history))
                         "  Use M-x undo-history-restore to restore\n"
                         (make-string 60 #\-) "\n"))
               (content (string-append header (string-join tree-lines "\n") "\n"))
               (tree-buf (or (buffer-by-name "*Undo Tree*")
                             (qt-buffer-create! "*Undo Tree*" ed #f))))
          (qt-buffer-attach! ed tree-buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) tree-buf)
          (qt-plain-text-edit-set-text! ed content)
          (qt-text-document-set-modified! (buffer-doc-pointer tree-buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

;;; Shared state for dired subtree, project tree, terminal per-project
(def *dired-expanded-dirs* (make-hash-table))
(def *project-tree-expanded* (make-hash-table))
(def *project-terminals* (make-hash-table))

(def (project-tree-render dir depth max-depth)
  "Render a project directory tree as text lines."
  (if (> depth max-depth) []
    (with-catch (lambda (e) [])
      (lambda ()
        (let* ((entries (directory-files dir))
               (sorted (sort entries string<?))
               (indent (make-string (* depth 2) #\space)))
          (let loop ((es sorted) (acc []))
            (if (null? es) (reverse acc)
              (let* ((f (car es))
                     (fp (path-expand f dir))
                     (is-dir (and (file-exists? fp)
                                  (eq? (file-info-type (file-info fp)) 'directory)))
                     (is-hidden (and (> (string-length f) 0) (char=? (string-ref f 0) #\.)))
                     (expanded (hash-get *project-tree-expanded* fp)))
                (if is-hidden
                  (loop (cdr es) acc)
                  (let ((line (string-append indent
                                (if is-dir
                                  (string-append (if expanded "v " "> ") f "/")
                                  (string-append "  " f)))))
                    (if (and is-dir expanded)
                      (let ((children (project-tree-render fp (+ depth 1) max-depth)))
                        (loop (cdr es) (append (reverse (cons line children)) acc)))
                      (loop (cdr es) (cons line acc)))))))))))))

;;; Dired subtree toggle for Qt
(def (cmd-dired-subtree-toggle app)
  "Toggle inline expansion of subdirectory under cursor in dired."
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (echo (app-state-echo app))
         (name (buffer-name buf)))
    (if (not (string-prefix? "*Dired:" name))
      (echo-message! echo "Not in a dired buffer")
      (let* ((pos (sci-send ed SCI_GETCURRENTPOS 0 0))
             (line-num (sci-send ed SCI_LINEFROMPOSITION pos 0))
             (ls (sci-send ed SCI_POSITIONFROMLINE line-num 0))
             (le (sci-send ed SCI_GETLINEENDPOSITION line-num 0))
             (line-text (qt-plain-text-edit-text-range ed ls le))
             (trimmed (string-trim line-text))
             (parts (string-split trimmed #\space))
             (last-part (if (pair? parts) (last parts) #f))
             (dir-match (and (> (string-length name) 8)
                             (substring name 8 (- (string-length name) 1)))))
        (when (and last-part (not (string-empty? last-part))
                   (not (member last-part '("." ".." "total"))))
          (let ((full-path (if dir-match (path-expand last-part dir-match) last-part)))
            (if (and (file-exists? full-path) (eq? (file-info-type (file-info full-path)) 'directory))
              (let ((expanded (or (hash-get *dired-expanded-dirs* name) (make-hash-table))))
                (if (hash-get expanded full-path)
                  (begin ;; Collapse
                    (hash-remove! expanded full-path)
                    (hash-put! *dired-expanded-dirs* name expanded)
                    (let rm ((nl (+ line-num 1)))
                      (let* ((ns (sci-send ed SCI_POSITIONFROMLINE nl 0))
                             (ne (sci-send ed SCI_GETLINEENDPOSITION nl 0)))
                        (when (> ne ns)
                          (let ((nt (qt-plain-text-edit-text-range ed ns ne)))
                            (when (string-prefix? "    " nt)
                              (sci-send ed SCI_SETREADONLY 0 0)
                              (let ((de (sci-send ed SCI_POSITIONFROMLINE (+ nl 1) 0)))
                                (sci-send ed SCI_DELETERANGE ns (- de ns)))
                              (sci-send ed SCI_SETREADONLY 1 0)
                              (rm nl))))))
                    (echo-message! echo (string-append "Collapsed: " last-part)))
                  (with-catch (lambda (e) (echo-message! echo "Cannot read directory"))
                    (lambda () ;; Expand
                      (let* ((entries (sort (directory-files full-path) string<?))
                             (lines (map (lambda (f)
                                           (let* ((fp (path-expand f full-path))
                                                  (is-d (and (file-exists? fp) (eq? (file-info-type (file-info fp)) 'directory))))
                                             (string-append "    " (if is-d "d " "  ") f (if is-d "/" ""))))
                                         entries))
                             (ins (string-append "\n" (string-join lines "\n"))))
                        (hash-put! expanded full-path #t)
                        (hash-put! *dired-expanded-dirs* name expanded)
                        (sci-send ed SCI_SETREADONLY 0 0)
                        (sci-send ed SCI_GOTOPOS le 0)
                        (sci-send/string ed SCI_REPLACESEL ins)
                        (sci-send ed SCI_SETREADONLY 1 0)
                        (echo-message! echo (string-append "Expanded: " last-part)))))))
              (echo-message! echo (string-append "Not a directory: " last-part)))))))))

;;; Project tree for Qt
(def (cmd-project-tree app)
  "Show project file tree in a buffer."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (root (project-current app)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (let* ((fr (app-state-frame app))
             (lines (project-tree-render root 0 3))
             (header (string-append "Project: " (path-strip-directory root) "\n"
                                    (make-string 40 #\-) "\n"))
             (content (string-append header (string-join lines "\n") "\n"))
             (tbuf (or (buffer-by-name "*Project Tree*")
                       (qt-buffer-create! "*Project Tree*" ed #f))))
        (qt-buffer-attach! ed tbuf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) tbuf)
        (qt-plain-text-edit-set-text! ed content)
        (qt-text-document-set-modified! (buffer-doc-pointer tbuf) #f)
        (qt-plain-text-edit-set-cursor-position! ed 0)))))

;;; Terminal per-project for Qt
(def (cmd-project-term app)
  "Open or switch to terminal for current project."
  (let* ((echo (app-state-echo app))
         (root (project-current app)))
    (if (not root)
      (echo-message! echo "Not in a project")
      (let* ((term-name (or (hash-get *project-terminals* root)
                            (string-append "*term:" (path-strip-directory root) "*")))
             (existing (buffer-by-name term-name)))
        (if existing
          (let* ((ed (current-qt-editor app))
                 (fr (app-state-frame app)))
            (qt-buffer-attach! ed existing)
            (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
            (echo-message! echo (string-append "Terminal: " (path-strip-directory root))))
          (begin
            (current-directory root)
            (hash-put! *project-terminals* root term-name)
            (execute-command! app 'shell)
            (echo-message! echo (string-append "New terminal in: " root))))))))

;;;============================================================================
;;; Consult-ripgrep — interactive rg with narrowing
;;;============================================================================

(def (rg-available?)
  "Check if rg (ripgrep) is on PATH."
  (with-catch (lambda (e) #f)
    (lambda ()
      (let ((p (open-process (list path: "rg" arguments: '("--version")
                                   stdout-redirection: #t
                                   stderr-redirection: #f))))
        (close-port p)
        (zero? (process-status p))))))

(def (run-rg pattern dir)
  "Run rg and return list of result strings (file:line:col:text)."
  (with-catch
    (lambda (e) [])
    (lambda ()
      (let* ((p (open-process
                  (list path: "rg"
                        arguments: (list "--vimgrep" "--color" "never"
                                         "--max-count" "500" pattern dir)
                        stdout-redirection: #t
                        stderr-redirection: #f)))
             (output (read-line p #f))
             (rc (process-status p)))
        (if (and output (string? output) (> (string-length output) 0))
          (let loop ((s output) (acc []))
            (let ((nl (string-index s #\newline)))
              (if nl
                (loop (substring s (+ nl 1) (string-length s))
                      (cons (substring s 0 nl) acc))
                (reverse (if (> (string-length s) 0) (cons s acc) acc)))))
          [])))))

(def (parse-rg-line line)
  "Parse rg --vimgrep output line: file:line:col:text → (file line-num text)"
  (let ((c1 (string-index line #\:)))
    (and c1
      (let ((c2 (string-index line #\: (+ c1 1))))
        (and c2
          (let ((c3 (string-index line #\: (+ c2 1))))
            (and c3
              (let ((file (substring line 0 c1))
                    (line-str (substring line (+ c1 1) c2))
                    (text (substring line (+ c3 1) (string-length line))))
                (let ((n (string->number line-str)))
                  (and n (list file n (string-trim text))))))))))))

(def (cmd-consult-ripgrep app)
  "Interactive ripgrep search with narrowing. Prompts for pattern,
   runs rg in the project root, then shows results in a narrowing
   popup for interactive filtering."
  (let* ((root (or (project-current app) (current-directory)))
         (pattern (qt-echo-read-string app
                    (string-append "rg in " (path-strip-directory root) ": "))))
    (when (and pattern (> (string-length pattern) 0))
      (echo-message! (app-state-echo app) "Searching...")
      (let ((results (run-rg pattern root)))
        (if (null? results)
          (echo-message! (app-state-echo app) "No matches found")
          (let* ((choice (qt-echo-read-with-narrowing app
                           (string-append "rg [" (number->string (length results))
                                          " matches]: ")
                           results))
                 (parsed (and choice (parse-rg-line choice))))
            (if parsed
              (let ((file (car parsed))
                    (line-num (cadr parsed)))
                ;; Make path absolute if relative
                (let ((abs-file (if (string-prefix? "/" file)
                                  file
                                  (path-expand file root))))
                  ;; Open file and jump to line
                  (when (file-exists? abs-file)
                    (let* ((ed (current-qt-editor app))
                           (fr (app-state-frame app))
                           (name (path-strip-directory abs-file))
                           (existing (let loop ((bufs *buffer-list*))
                                       (if (null? bufs) #f
                                         (let ((b (car bufs)))
                                           (if (and (buffer-file-path b)
                                                    (string=? (buffer-file-path b) abs-file))
                                             b (loop (cdr bufs)))))))
                           (target-buf (or existing
                                           (qt-buffer-create! name ed abs-file))))
                      (qt-buffer-attach! ed target-buf)
                      (set! (qt-edit-window-buffer (qt-current-window fr)) target-buf)
                      (when (not existing)
                        (let ((text (read-file-as-string abs-file)))
                          (when text
                            (qt-plain-text-edit-set-text! ed text)
                            (qt-text-document-set-modified!
                              (buffer-doc-pointer target-buf) #f)))
                        (qt-setup-highlighting! app target-buf))
                      ;; Jump to line
                      (let* ((text (qt-plain-text-edit-text ed))
                             (pos (text-line-position text line-num)))
                        (qt-plain-text-edit-set-cursor-position! ed pos)
                        (qt-plain-text-edit-ensure-cursor-visible! ed))
                      (echo-message! (app-state-echo app)
                        (string-append abs-file ":" (number->string line-num)))))))
              (when choice
                (echo-message! (app-state-echo app) "Could not parse result")))))))))

;;;============================================================================
;;; Consult-bookmark — interactive bookmark jump with narrowing
;;;============================================================================

(def (cmd-consult-bookmark app)
  "Jump to a bookmark using narrowing selection with file info."
  (let* ((bmarks (app-state-bookmarks app))
         (names (sort (hash-keys bmarks) string<?)))
    (if (null? names)
      (echo-message! (app-state-echo app) "No bookmarks set")
      (let* ((display-items
               (map (lambda (name)
                      (let* ((entry (hash-get bmarks name))
                             (file (and entry (list? entry) (>= (length entry) 3)
                                        (cadr entry)))
                             (pos (and entry (list? entry) (>= (length entry) 3)
                                       (caddr entry))))
                        (if file
                          (string-append name " → " (path-strip-directory file)
                                         ":" (number->string (or pos 0)))
                          name)))
                    names))
             (choice (qt-echo-read-with-narrowing app "Bookmark: " display-items)))
        (when choice
          ;; Extract bookmark name (before " → ")
          (let* ((arrow (string-contains choice " → "))
                 (name (if arrow (substring choice 0 arrow) choice))
                 (entry (hash-get bmarks name)))
            (when entry
              (let* ((buf-name (car entry))
                     (file (cadr entry))
                     (pos (caddr entry)))
                ;; Jump to the bookmark location
                (when (and file (file-exists? file))
                  (let* ((ed (current-qt-editor app))
                         (fr (app-state-frame app))
                         (existing (let loop ((bs *buffer-list*))
                                     (if (null? bs) #f
                                       (if (and (buffer-file-path (car bs))
                                                (string=? (buffer-file-path (car bs)) file))
                                         (car bs) (loop (cdr bs))))))
                         (target (or existing
                                     (qt-buffer-create! (path-strip-directory file) ed file))))
                    (qt-buffer-attach! ed target)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) target)
                    (when (not existing)
                      (let ((text (read-file-as-string file)))
                        (when text
                          (qt-plain-text-edit-set-text! ed text)
                          (qt-text-document-set-modified! (buffer-doc-pointer target) #f)))
                      (qt-setup-highlighting! app target))
                    (qt-plain-text-edit-set-cursor-position! ed pos)
                    (qt-plain-text-edit-ensure-cursor-visible! ed)
                    (echo-message! (app-state-echo app)
                      (string-append "Bookmark: " name))))))))))))

;;;============================================================================
;;; goto-address-mode — highlight and browse URLs in buffer
;;;============================================================================

(def *qt-goto-address-active* #f)
(def *qt-goto-address-indicator* 12)

(def (qt-goto-address-setup! ed)
  "Setup indicator style for URL highlighting."
  (sci-send ed SCI_INDICSETSTYLE *qt-goto-address-indicator* 6)  ;; INDIC_HIDDEN=0, INDIC_PLAIN=0, ..., 6=INDIC_BOX
  ;; Use underline style (INDIC_PLAIN = 0) — looks like hyperlinks
  (sci-send ed SCI_INDICSETSTYLE *qt-goto-address-indicator* 0)
  (sci-send ed SCI_INDICSETFORE *qt-goto-address-indicator* #xFF0000))  ;; blue (BGR)

(def (qt-goto-address-clear! ed)
  "Clear all URL indicator highlights."
  (let ((len (sci-send ed SCI_GETTEXTLENGTH)))
    (sci-send ed SCI_SETINDICATORCURRENT *qt-goto-address-indicator*)
    (sci-send ed SCI_INDICATORCLEARRANGE 0 len)))

(def (qt-goto-address-scan! ed)
  "Scan buffer text for URLs and highlight them with indicators."
  (qt-goto-address-clear! ed)
  (qt-goto-address-setup! ed)
  (let* ((text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (sci-send ed SCI_SETINDICATORCURRENT *qt-goto-address-indicator*)
    (let loop ((i 0))
      (when (< i (- len 7))  ;; minimum "http://" length
        (if (and (char=? (string-ref text i) #\h)
                 (or (string-prefix? "http://" (substring text i (min len (+ i 8))))
                     (string-prefix? "https://" (substring text i (min len (+ i 9))))))
          ;; Found URL start — find end
          (let url-end ((j (+ i 7)))
            (if (or (>= j len)
                    (char=? (string-ref text j) #\space)
                    (char=? (string-ref text j) #\tab)
                    (char=? (string-ref text j) #\newline)
                    (char=? (string-ref text j) #\>)
                    (char=? (string-ref text j) #\))
                    (char=? (string-ref text j) #\])
                    (char=? (string-ref text j) #\")
                    (char=? (string-ref text j) #\'))
              (begin
                (sci-send ed SCI_INDICATORFILLRANGE i (- j i))
                (loop j))
              (url-end (+ j 1))))
          (loop (+ i 1)))))))

(def (cmd-goto-address-mode app)
  "Toggle goto-address-mode — highlight URLs in the buffer.
Use browse-url-at-point (C-c RET) to open highlighted URLs."
  (let ((ed (current-qt-editor app)))
    (set! *qt-goto-address-active* (not *qt-goto-address-active*))
    (if *qt-goto-address-active*
      (begin
        (qt-goto-address-scan! ed)
        (echo-message! (app-state-echo app) "Goto-address-mode ON"))
      (begin
        (qt-goto-address-clear! ed)
        (echo-message! (app-state-echo app) "Goto-address-mode OFF")))))

;;;============================================================================
;;; subword-mode — CamelCase-aware word movement
;;;============================================================================

(def *qt-subword-mode* #f)

(def (qt-subword-forward-pos text pos)
  "Find the next subword boundary forward from pos in text."
  (let ((len (string-length text)))
    (if (>= pos len) pos
      (let loop ((i (+ pos 1)))
        (cond
          ((>= i len) i)
          ;; Transition: lowercase → uppercase (camelCase boundary)
          ((and (> i 0)
                (char-lower-case? (string-ref text (- i 1)))
                (char-upper-case? (string-ref text i)))
           i)
          ;; Transition: uppercase → uppercase+lowercase (XMLParser → XML|Parser)
          ((and (> i 1)
                (char-upper-case? (string-ref text (- i 2)))
                (char-upper-case? (string-ref text (- i 1)))
                (char-lower-case? (string-ref text i)))
           (- i 1))
          ;; Transition: letter → non-letter or non-letter → letter
          ((and (> i 0)
                (not (eqv? (char-alphabetic? (string-ref text (- i 1)))
                           (char-alphabetic? (string-ref text i)))))
           ;; Skip whitespace
           (if (char-whitespace? (string-ref text i))
             (let skip-ws ((j i))
               (if (or (>= j len) (not (char-whitespace? (string-ref text j))))
                 j (skip-ws (+ j 1))))
             i))
          (else (loop (+ i 1))))))))

(def (qt-subword-backward-pos text pos)
  "Find the previous subword boundary backward from pos in text."
  (if (<= pos 0) 0
    (let loop ((i (- pos 1)))
      (cond
        ((<= i 0) 0)
        ;; Skip whitespace backward
        ((char-whitespace? (string-ref text i))
         (loop (- i 1)))
        ;; Transition: uppercase → lowercase (coming from right)
        ((and (> i 0)
              (char-lower-case? (string-ref text i))
              (char-upper-case? (string-ref text (- i 1)))
              ;; Don't stop at start of all-uppercase run
              (or (= i 1) (not (char-upper-case? (string-ref text (- i 2))))))
         (- i 1))
        ;; Transition: uppercase sequence before lowercase (XMLParser → |XML)
        ((and (> i 1)
              (char-upper-case? (string-ref text i))
              (char-upper-case? (string-ref text (- i 1)))
              (not (char-upper-case? (string-ref text (- i 2)))))
         (- i 1))
        ;; Transition: non-letter → letter or letter → non-letter
        ((and (> i 0)
              (not (eqv? (char-alphabetic? (string-ref text i))
                         (char-alphabetic? (string-ref text (- i 1))))))
         (if (char-alphabetic? (string-ref text i)) i
           (loop (- i 1))))
        (else (loop (- i 1)))))))

(def (cmd-subword-mode app)
  "Toggle subword-mode — CamelCase-aware word movement.
When enabled, forward-word and backward-word stop at subword boundaries
(e.g., 'myVariableName' → 'my|Variable|Name')."
  (set! *qt-subword-mode* (not *qt-subword-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-subword-mode* "Subword-mode ON" "Subword-mode OFF")))

(def (cmd-subword-forward app)
  "Move forward one subword (CamelCase boundary)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (new-pos (qt-subword-forward-pos text pos)))
    (qt-plain-text-edit-set-cursor-position! ed new-pos)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-subword-backward app)
  "Move backward one subword (CamelCase boundary)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (new-pos (qt-subword-backward-pos text pos)))
    (qt-plain-text-edit-set-cursor-position! ed new-pos)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (cmd-subword-kill app)
  "Kill forward one subword."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (end (qt-subword-forward-pos text pos)))
    (when (> end pos)
      (let ((killed (substring text pos end)))
        (sci-send ed SCI_DELETERANGE pos (- end pos))
        (echo-message! (app-state-echo app)
          (string-append "Killed: " (if (> (string-length killed) 30)
                                      (string-append (substring killed 0 30) "...")
                                      killed)))))))

