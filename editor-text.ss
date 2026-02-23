;;; -*- Gerbil -*-
;;; Text manipulation: sort, bookmarks, rectangles, paren match,
;;; join lines, case, fill paragraph, grep, dabbrev, keyboard macros,
;;; mark ring, registers, zap, transpose

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :std/text/hex
        :std/crypto/digest
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        (only-in :gerbil-scintilla/ffi scintilla-send-message-string)
        :gerbil-scintilla/style
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/subprocess
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/highlight
        :gemacs/editor-core
        :gemacs/editor-ui)

;;;============================================================================
;;; Sort lines (M-^)
;;;============================================================================

(def (cmd-sort-lines app)
  "Sort lines in the buffer (or region if mark is set)."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (text (editor-get-text ed)))
    (if mark
      ;; Sort region only
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines string<?))
             (result (string-join sorted "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app)
          (string-append "Sorted " (number->string (length sorted)) " lines")))
      ;; Sort whole buffer
      (let* ((lines (string-split text #\newline))
             (sorted (sort lines string<?))
             (result (string-join sorted "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! (app-state-echo app)
          (string-append "Sorted " (number->string (length sorted)) " lines"))))))

;;;============================================================================
;;; Bookmarks
;;;============================================================================

(def (cmd-bookmark-set app)
  "Set a bookmark at the current position."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Bookmark name: " row width)))
    (when (and name (> (string-length name) 0))
      (let* ((buf (current-buffer-from-app app))
             (ed (current-editor app))
             (pos (editor-get-current-pos ed)))
        (hash-put! (app-state-bookmarks app) name
                   (cons (buffer-name buf) pos))
        (echo-message! echo (string-append "Bookmark \"" name "\" set"))))))

(def (cmd-bookmark-jump app)
  "Jump to a named bookmark."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (name (echo-read-string echo "Jump to bookmark: " row width)))
    (when (and name (> (string-length name) 0))
      (let ((bm (hash-get (app-state-bookmarks app) name)))
        (if bm
          (let* ((buf-name (car bm))
                 (pos (cdr bm))
                 (buf (buffer-by-name buf-name)))
            (if buf
              (let ((ed (current-editor app)))
                (buffer-attach! ed buf)
                (set! (edit-window-buffer (current-window fr)) buf)
                (editor-goto-pos ed pos)
                (editor-scroll-caret ed)
                (echo-message! echo (string-append "Jumped to \"" name "\"")))
              (echo-error! echo (string-append "Buffer gone: " buf-name))))
          (echo-error! echo (string-append "No bookmark: " name)))))))

(def (cmd-bookmark-list app)
  "Display all bookmarks in a *Bookmarks* buffer."
  (let* ((fr (app-state-frame app))
         (ed (current-editor app))
         (bms (app-state-bookmarks app))
         (entries '()))
    (hash-for-each
      (lambda (name val)
        (set! entries
          (cons (string-append "  " name "\t"
                               (car val) ":"
                               (number->string (cdr val)))
                entries)))
      bms)
    (let* ((sorted (sort entries string<?))
           (text (if (null? sorted)
                   "No bookmarks set.\n"
                   (string-append "Bookmarks:\n\n"
                                  (string-join sorted "\n")
                                  "\n")))
           (buf (or (buffer-by-name "*Bookmarks*")
                    (buffer-create! "*Bookmarks*" ed #f))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer (current-window fr)) buf)
      (editor-set-text ed text)
      (editor-set-save-point ed)
      (editor-goto-pos ed 0)
      (echo-message! (app-state-echo app) "*Bookmarks*"))))

;;;============================================================================
;;; Rectangle operations
;;;============================================================================

(def (get-region-lines app)
  "Get start/end lines and columns from mark and point.
   Returns (values start-line start-col end-line end-col) or #f."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (values #f #f #f #f)
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (start-col (editor-get-column ed start))
             (end-col (editor-get-column ed end)))
        (values start-line start-col end-line end-col)))))

(def (cmd-kill-rectangle app)
  "Kill the rectangle defined by mark and point."
  (let ((echo (app-state-echo app)))
    (let-values (((start-line start-col end-line end-col) (get-region-lines app)))
      (if (not start-line)
        (echo-error! echo "No region (set mark first)")
        (let* ((ed (current-editor app))
               (left-col (min start-col end-col))
               (right-col (max start-col end-col))
               (rect-lines '()))
          ;; Extract and delete rectangle, line by line from bottom to top
          (let loop ((line end-line))
            (when (>= line start-line)
              (let* ((line-start (editor-position-from-line ed line))
                     (line-end (editor-get-line-end-position ed line))
                     (line-text (editor-get-line ed line))
                     (line-len (string-length (string-trim-right line-text #\newline)))
                     (l (min left-col line-len))
                     (r (min right-col line-len))
                     (extracted (if (< l r)
                                  (substring (string-trim-right line-text #\newline) l r)
                                  "")))
                (set! rect-lines (cons extracted rect-lines))
                ;; Delete the rectangle portion of this line
                (when (< l r)
                  (editor-delete-range ed (+ line-start l) (- r l)))
                (loop (- line 1)))))
          ;; Store in rectangle kill ring
          (set! (app-state-rect-kill app) rect-lines)
          (set! (buffer-mark (current-buffer-from-app app)) #f)
          (echo-message! echo
            (string-append "Killed rectangle (" (number->string (length rect-lines)) " lines)")))))))

(def (cmd-delete-rectangle app)
  "Delete the rectangle defined by mark and point (without saving to kill ring)."
  (let ((echo (app-state-echo app)))
    (let-values (((start-line start-col end-line end-col) (get-region-lines app)))
      (if (not start-line)
        (echo-error! echo "No region (set mark first)")
        (let* ((ed (current-editor app))
               (left-col (min start-col end-col))
               (right-col (max start-col end-col)))
          (with-undo-action ed
            (let loop ((line end-line))
              (when (>= line start-line)
                (let* ((line-start (editor-position-from-line ed line))
                       (line-text (editor-get-line ed line))
                       (line-len (string-length (string-trim-right line-text #\newline)))
                       (l (min left-col line-len))
                       (r (min right-col line-len)))
                  (when (< l r)
                    (editor-delete-range ed (+ line-start l) (- r l)))
                  (loop (- line 1))))))
          (set! (buffer-mark (current-buffer-from-app app)) #f)
          (echo-message! echo "Rectangle deleted"))))))

(def (cmd-yank-rectangle app)
  "Yank (paste) the last killed rectangle at point."
  (let* ((echo (app-state-echo app))
         (rk (app-state-rect-kill app)))
    (if (null? rk)
      (echo-error! echo "No rectangle to yank")
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (line (editor-line-from-position ed pos))
             (col (editor-get-column ed pos)))
        (with-undo-action ed
          (let loop ((lines rk) (cur-line line))
            (when (pair? lines)
              (let* ((rect-text (car lines))
                     (line-start (editor-position-from-line ed cur-line))
                     (line-end (editor-get-line-end-position ed cur-line))
                     (line-len (- line-end line-start))
                     ;; Pad line if shorter than insertion column
                     (insert-pos (+ line-start (min col line-len))))
                (when (< line-len col)
                  (editor-insert-text ed line-end
                    (make-string (- col line-len) #\space)))
                (let ((actual-pos (+ line-start col)))
                  (editor-insert-text ed actual-pos rect-text)))
              (loop (cdr lines) (+ cur-line 1)))))
        (echo-message! echo "Rectangle yanked")))))

;;;============================================================================
;;; Go to matching paren
;;;============================================================================

(def (cmd-goto-matching-paren app)
  "Jump to the matching parenthesis/bracket/brace."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (ch-at (send-message ed SCI_GETCHARAT pos 0))
         (ch-before (if (> pos 0) (send-message ed SCI_GETCHARAT (- pos 1) 0) 0)))
    (cond
      ((brace-char? ch-at)
       (let ((match (send-message ed SCI_BRACEMATCH pos 0)))
         (if (>= match 0)
           (editor-goto-pos ed match)
           (echo-error! (app-state-echo app) "No matching paren"))))
      ((brace-char? ch-before)
       (let ((match (send-message ed SCI_BRACEMATCH (- pos 1) 0)))
         (if (>= match 0)
           (editor-goto-pos ed match)
           (echo-error! (app-state-echo app) "No matching paren"))))
      (else
       (echo-error! (app-state-echo app) "Not on a paren")))))

;;;============================================================================
;;; Join line (M-j)
;;;============================================================================

(def (cmd-join-line app)
  "Join the current line with the next line."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-end (editor-get-line-end-position ed line))
         (total (editor-get-line-count ed)))
    (when (< (+ line 1) total)
      ;; Get next line text to find leading whitespace
      (let* ((next-start (editor-position-from-line ed (+ line 1)))
             (text (editor-get-text ed))
             (next-ws-end next-start))
        ;; Skip whitespace at start of next line
        (let skip ((i next-start))
          (when (< i (string-length text))
            (let ((ch (string-ref text i)))
              (when (or (char=? ch #\space) (char=? ch #\tab))
                (set! next-ws-end (+ i 1))
                (skip (+ i 1))))))
        ;; Delete from end of current line to end of whitespace on next line
        ;; and insert a single space
        (with-undo-action ed
          (editor-delete-range ed line-end (- next-ws-end line-end))
          (editor-insert-text ed line-end " "))))))

;;;============================================================================
;;; Delete blank lines (C-x C-o)
;;;============================================================================

(def (cmd-delete-blank-lines app)
  "Delete blank lines around point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (line (editor-line-from-position ed pos))
         (total (editor-get-line-count ed))
         ;; Check if current line is blank
         (line-text (editor-get-line ed line))
         (blank? (lambda (s) (string=? (string-trim s) ""))))
    (if (blank? line-text)
      ;; Find range of blank lines around point
      (let find-start ((l line))
        (let ((start (if (and (> l 0) (blank? (editor-get-line ed (- l 1))))
                       (find-start (- l 1))
                       l)))
          (let find-end ((l line))
            (let ((end (if (and (< (+ l 1) total) (blank? (editor-get-line ed (+ l 1))))
                         (find-end (+ l 1))
                         l)))
              ;; Delete from start of first blank line to end of last + newline
              (let* ((del-start (editor-position-from-line ed start))
                     (del-end (if (< (+ end 1) total)
                                (editor-position-from-line ed (+ end 1))
                                (editor-get-text-length ed))))
                ;; Keep one blank line
                (editor-delete-range ed del-start (- del-end del-start))
                (editor-insert-text ed del-start "\n")
                (echo-message! (app-state-echo app) "Blank lines deleted"))))))
      (echo-message! (app-state-echo app) "Not on a blank line"))))

;;;============================================================================
;;; Indent region
;;;============================================================================

(def (cmd-indent-region app)
  "Indent all lines in region by 2 spaces."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region (set mark first)")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          ;; Insert 2 spaces at beginning of each line, from bottom to top
          (let loop ((line end-line))
            (when (>= line start-line)
              (let ((line-pos (editor-position-from-line ed line)))
                (editor-insert-text ed line-pos "  "))
              (loop (- line 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region indented")))))

;;;============================================================================
;;; Case region
;;;============================================================================

(def (cmd-downcase-region app)
  "Convert region to lowercase."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (editor-get-text ed))
             (region (substring text start end))
             (lower (string-downcase region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start lower))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region downcased")))))

(def (cmd-upcase-region app)
  "Convert region to uppercase."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if (not mark)
      (echo-error! echo "No region")
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (editor-get-text ed))
             (region (substring text start end))
             (upper (string-upcase region)))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start upper))
        (set! (buffer-mark buf) #f)
        (echo-message! echo "Region upcased")))))

;;;============================================================================
;;; Shell command (M-!)
;;;============================================================================

(def (cmd-shell-command app)
  "Run a shell command and display output."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Shell command: " row width)))
    (when (and cmd (> (string-length cmd) 0))
      (echo-message! echo (string-append "Running... (C-g to cancel)"))
      (frame-refresh! fr)
      (let-values (((output _status)
                    (run-process-interruptible
                      cmd tui-peek-event tui-event-key? tui-event-key)))
        (let ((ed (current-editor app)))
          ;; If short output (1 line), show in echo area
          (if (not (string-contains output "\n"))
            (echo-message! echo output)
            ;; Multi-line: show in *Shell Output* buffer
            (let ((buf (or (buffer-by-name "*Shell Output*")
                           (buffer-create! "*Shell Output*" ed #f))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-text ed output)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (echo-message! echo "Shell command done"))))))))

;;;============================================================================
;;; Fill paragraph (M-q) — word wrap at fill-column (80)
;;;============================================================================

(def fill-column 80)

(def (cmd-fill-paragraph app)
  "Fill (word-wrap) the current paragraph at fill-column."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find paragraph boundaries (blank lines or start/end of buffer)
    (let* ((para-start
             (let loop ((i (- pos 1)))
               (cond
                 ((< i 0) 0)
                 ((and (char=? (string-ref text i) #\newline)
                       (or (= i 0)
                           (and (> i 0) (char=? (string-ref text (- i 1)) #\newline))))
                  (+ i 1))
                 (else (loop (- i 1))))))
           (para-end
             (let loop ((i pos))
               (cond
                 ((>= i len) len)
                 ((and (char=? (string-ref text i) #\newline)
                       (< (+ i 1) len)
                       (char=? (string-ref text (+ i 1)) #\newline))
                  i)
                 (else (loop (+ i 1))))))
           (para-text (substring text para-start para-end))
           ;; Collapse whitespace and split into words
           (words (let split ((s para-text) (acc '()))
                    (let ((trimmed (string-trim s)))
                      (if (string=? trimmed "")
                        (reverse acc)
                        ;; Find next word boundary
                        (let find-end ((i 0))
                          (cond
                            ((>= i (string-length trimmed))
                             (reverse (cons trimmed acc)))
                            ((or (char=? (string-ref trimmed i) #\space)
                                 (char=? (string-ref trimmed i) #\newline)
                                 (char=? (string-ref trimmed i) #\tab))
                             (split (substring trimmed i (string-length trimmed))
                                    (cons (substring trimmed 0 i) acc)))
                            (else (find-end (+ i 1)))))))))
           ;; Rebuild with word wrap
           (filled (let loop ((ws words) (line "") (lines '()))
                     (if (null? ws)
                       (string-join (reverse (if (string=? line "") lines
                                                (cons line lines)))
                                   "\n")
                       (let* ((word (car ws))
                              (new-line (if (string=? line "")
                                          word
                                          (string-append line " " word))))
                         (if (> (string-length new-line) fill-column)
                           ;; Wrap
                           (if (string=? line "")
                             ;; Single word longer than fill-column
                             (loop (cdr ws) "" (cons word lines))
                             (loop ws "" (cons line lines)))
                           (loop (cdr ws) new-line lines)))))))
      ;; Replace paragraph text
      (with-undo-action ed
        (editor-delete-range ed para-start (- para-end para-start))
        (editor-insert-text ed para-start filled))
      (echo-message! (app-state-echo app) "Paragraph filled"))))

;;;============================================================================
;;; Grep (M-x grep)
;;;============================================================================

(def (cmd-grep app)
  "Search for a pattern in files using grep, show results."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Grep: " row width)))
    (when (and pattern (> (string-length pattern) 0))
      (let* ((dir (echo-read-string echo "In directory: " row width))
             (search-dir (if (or (not dir) (string=? dir "")) "." dir)))
        (echo-message! echo (string-append "Searching... (C-g to cancel)"))
        (frame-refresh! fr)
        (let* ((ed (current-editor app))
               (grep-cmd (string-append "grep -rn --include='*.ss' --include='*.scm' "
                                        "-- " (shell-quote pattern) " "
                                        (shell-quote search-dir) " 2>&1 || true")))
          (let-values (((output _status)
                        (run-process-interruptible
                          grep-cmd tui-peek-event tui-event-key? tui-event-key)))
            (let* ((text (string-append "-*- Grep -*-\n"
                                        "Pattern: " pattern "\n"
                                        "Directory: " search-dir "\n"
                                        (make-string 60 #\-) "\n\n"
                                        output "\n"))
                   (buf (or (buffer-by-name "*Grep*")
                            (buffer-create! "*Grep*" ed #f))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer (current-window fr)) buf)
              (editor-set-text ed text)
              (editor-set-save-point ed)
              (editor-goto-pos ed 0)
              (echo-message! echo "Grep done"))))))))

(def (shell-quote s)
  "Quote a string for safe shell use."
  (string-append "'" (let loop ((i 0) (acc ""))
                       (if (>= i (string-length s))
                         acc
                         (let ((ch (string-ref s i)))
                           (if (char=? ch #\')
                             (loop (+ i 1) (string-append acc "'\"'\"'"))
                             (loop (+ i 1) (string-append acc (string ch)))))))
                 "'"))

;;;============================================================================
;;; Insert file (C-x i)
;;;============================================================================

(def (cmd-insert-file app)
  "Insert contents of a file at point."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (filename (echo-read-string echo "Insert file: " row width)))
    (when (and filename (> (string-length filename) 0))
      (if (file-exists? filename)
        (let* ((text (read-file-as-string filename))
               (ed (current-editor app))
               (pos (editor-get-current-pos ed)))
          (when text
            (editor-insert-text ed pos text)
            (echo-message! echo (string-append "Inserted " filename))))
        (echo-error! echo (string-append "File not found: " filename))))))

;;;============================================================================
;;; Dynamic abbreviation (M-/)
;;;============================================================================

(def (collect-dabbrev-matches text prefix pos)
  "Collect all words in text matching prefix, ordered by distance from pos."
  (let* ((plen (string-length prefix))
         (tlen (string-length text))
         (matches '()))
    ;; Scan the entire text for words matching the prefix
    (let loop ((i 0))
      (when (< i tlen)
        ;; Find start of word
        (if (or (char-alphabetic? (string-ref text i))
                (char=? (string-ref text i) #\_)
                (char=? (string-ref text i) #\-))
          (let find-end ((j (+ i 1)))
            (if (or (>= j tlen)
                    (not (or (char-alphabetic? (string-ref text j))
                             (char-numeric? (string-ref text j))
                             (char=? (string-ref text j) #\_)
                             (char=? (string-ref text j) #\-)
                             (char=? (string-ref text j) #\?)
                             (char=? (string-ref text j) #\!))))
              (let ((word (substring text i j)))
                (when (and (> (string-length word) plen)
                           (string=? (substring word 0 plen) prefix)
                           (not (= i (- pos plen))))  ; Skip the prefix itself
                  (set! matches (cons (cons (abs (- i pos)) word) matches)))
                (loop j))
              (find-end (+ j 1))))
          (loop (+ i 1)))))
    ;; Sort by distance from cursor, remove duplicates
    (let* ((sorted (sort matches (lambda (a b) (< (car a) (car b)))))
           (words (map cdr sorted)))
      ;; Remove duplicates keeping order
      (let dedup ((ws words) (seen '()) (acc '()))
        (if (null? ws) (reverse acc)
          (if (member (car ws) seen)
            (dedup (cdr ws) seen acc)
            (dedup (cdr ws) (cons (car ws) seen) (cons (car ws) acc))))))))

(def (cmd-dabbrev-expand app)
  "Expand word before point using other words in buffer."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app))
         (state (app-state-dabbrev-state app)))
    (if (and state (pair? state))
      ;; Continue cycling through matches
      (let* ((prefix (car state))
             (remaining (cadr state))
             (last-pos (caddr state))
             (last-len (cadddr state)))
        (if (null? remaining)
          (begin
            (set! (app-state-dabbrev-state app) #f)
            (echo-message! echo "No more expansions"))
          (let* ((next (car remaining))
                 (expand-len (- (string-length next) (string-length prefix))))
            ;; Delete previous expansion
            (editor-delete-range ed (+ last-pos (string-length prefix))
                                (- last-len (string-length prefix)))
            ;; Insert new expansion
            (editor-insert-text ed (+ last-pos (string-length prefix))
                                (substring next (string-length prefix)
                                           (string-length next)))
            (editor-goto-pos ed (+ last-pos (string-length next)))
            (set! (app-state-dabbrev-state app)
              (list prefix (cdr remaining) last-pos (string-length next))))))
      ;; First expansion: find prefix before cursor
      (let find-prefix ((i (- pos 1)) (count 0))
        (if (or (< i 0)
                (not (or (char-alphabetic? (string-ref text i))
                         (char-numeric? (string-ref text i))
                         (char=? (string-ref text i) #\_)
                         (char=? (string-ref text i) #\-)
                         (char=? (string-ref text i) #\?)
                         (char=? (string-ref text i) #\!))))
          ;; Found prefix start
          (let* ((prefix-start (+ i 1))
                 (prefix (substring text prefix-start pos)))
            (if (= (string-length prefix) 0)
              (echo-message! echo "No prefix to expand")
              (let ((matches (collect-dabbrev-matches text prefix pos)))
                (if (null? matches)
                  (echo-message! echo "No expansion found")
                  (let* ((first-match (car matches))
                         (expand-text (substring first-match (string-length prefix)
                                                 (string-length first-match))))
                    (editor-insert-text ed pos expand-text)
                    (editor-goto-pos ed (+ pos (string-length expand-text)))
                    (set! (app-state-dabbrev-state app)
                      (list prefix (cdr matches) prefix-start
                            (string-length first-match))))))))
          (find-prefix (- i 1) (+ count 1)))))))

;;;============================================================================
;;; What cursor position (C-x =)
;;;============================================================================

(def (cmd-what-cursor-position app)
  "Display character information at point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let* ((ch (string-ref text pos))
             (code (char->integer ch))
             (line (+ 1 (editor-line-from-position ed pos)))
             (col (+ 1 (editor-get-column ed pos)))
             (pct (if (= len 0) 0 (quotient (* pos 100) len))))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (string ch)
                         " (" (number->string code) ", #x"
                         (number->string code 16) ")"
                         "  point=" (number->string pos)
                         " of " (number->string len)
                         " (" (number->string pct) "%)"
                         "  line " (number->string line)
                         " col " (number->string col)))))))

;;;============================================================================
;;; Keyboard macros
;;;============================================================================

(def (cmd-start-kbd-macro app)
  "Start recording a keyboard macro."
  (if (app-state-macro-recording app)
    (echo-error! (app-state-echo app) "Already recording")
    (begin
      (set! (app-state-macro-recording app) [])
      (echo-message! (app-state-echo app) "Defining kbd macro..."))))

(def (cmd-end-kbd-macro app)
  "Stop recording and save the keyboard macro."
  (if (not (app-state-macro-recording app))
    (echo-error! (app-state-echo app) "Not recording")
    (begin
      (set! (app-state-macro-last app)
        (reverse (app-state-macro-recording app)))
      (set! (app-state-macro-recording app) #f)
      (echo-message! (app-state-echo app)
        (string-append "Macro defined ("
                       (number->string (length (app-state-macro-last app)))
                       " steps)")))))

(def (cmd-call-last-kbd-macro app)
  "Execute the last recorded keyboard macro."
  (let ((macro (app-state-macro-last app)))
    (if (or (not macro) (null? macro))
      (echo-error! (app-state-echo app) "No macro defined")
      (begin
        (for-each
          (lambda (step)
            (let ((action (car step))
                  (data (cdr step)))
              (case action
                ((command) (execute-command! app data))
                ((self-insert) (cmd-self-insert! app data)))))
          macro)
        (echo-message! (app-state-echo app) "Macro executed")))))

(def (macro-record-step! app action data)
  "If macro recording, record a step."
  (when (app-state-macro-recording app)
    (let ((step (cons action data)))
      (set! (app-state-macro-recording app)
        (cons step (app-state-macro-recording app))))))

(def (cmd-name-last-kbd-macro app)
  "Give a name to the last recorded keyboard macro."
  (let ((macro (app-state-macro-last app)))
    (if (or (not macro) (null? macro))
      (echo-error! (app-state-echo app) "No macro defined")
      (let* ((echo (app-state-echo app))
             (frame (app-state-frame app))
             (row (- (frame-height frame) 1))
             (width (frame-width frame))
             (name (echo-read-string echo "Name for macro: " row width)))
        (when (and name (> (string-length name) 0))
          (hash-put! (app-state-macro-named app) name macro)
          (echo-message! echo (string-append "Macro saved as '" name "'")))))))

(def (cmd-call-named-kbd-macro app)
  "Execute a named keyboard macro."
  (let* ((named (app-state-macro-named app))
         (names (map car (hash->list named))))
    (if (null? names)
      (echo-error! (app-state-echo app) "No named macros")
      (let* ((echo (app-state-echo app))
             (frame (app-state-frame app))
             (row (- (frame-height frame) 1))
             (width (frame-width frame))
             (name (echo-read-string echo "Macro name: " row width)))
        (when (and name (> (string-length name) 0))
          (let ((macro (hash-get named name)))
            (if macro
              (begin
                (for-each
                  (lambda (step)
                    (case (car step)
                      ((command) (execute-command! app (cdr step)))
                      ((self-insert) (cmd-self-insert! app (cdr step)))))
                  macro)
                (echo-message! echo (string-append "Macro '" name "' executed")))
              (echo-error! echo (string-append "No macro named '" name "'")))))))))

(def (cmd-list-kbd-macros app)
  "List all named keyboard macros."
  (let* ((named (app-state-macro-named app))
         (names (sort (map car (hash->list named)) string<?)))
    (if (null? names)
      (echo-message! (app-state-echo app) "No named macros")
      (echo-message! (app-state-echo app)
        (string-append "Macros: " (string-join names ", "))))))

(def (cmd-save-kbd-macros app)
  "Save named keyboard macros to ~/.gemacs-macros."
  (let ((named (app-state-macro-named app)))
    (if (= (hash-length named) 0)
      (echo-message! (app-state-echo app) "No macros to save")
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Error saving macros"))
        (lambda ()
          (call-with-output-file "~/.gemacs-macros"
            (lambda (port)
              (for-each
                (lambda (pair)
                  (write (cons (car pair) (cdr pair)) port)
                  (newline port))
                (hash->list named))))
          (echo-message! (app-state-echo app)
            (string-append "Saved " (number->string (hash-length named)) " macros")))))))

(def (cmd-load-kbd-macros app)
  "Load named keyboard macros from ~/.gemacs-macros."
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "No saved macros found"))
    (lambda ()
      (let ((named (app-state-macro-named app)))
        (call-with-input-file "~/.gemacs-macros"
          (lambda (port)
            (let loop ()
              (let ((datum (read port)))
                (when (not (eof-object? datum))
                  (when (pair? datum)
                    (hash-put! named (car datum) (cdr datum)))
                  (loop))))))
        (echo-message! (app-state-echo app)
          (string-append "Loaded " (number->string (hash-length named)) " macros"))))))

;;;============================================================================
;;; Mark ring
;;;============================================================================

;; Moved to editor-core.ss

;; Moved to editor-core.ss

(def (cmd-pop-mark app)
  "Pop the mark ring and jump to the previous mark position."
  (let ((ring (app-state-mark-ring app)))
    (if (null? ring)
      (echo-error! (app-state-echo app) "Mark ring empty")
      (let* ((entry (car ring))
             (buf-name (car entry))
             (pos (cdr entry))
             (buf (buffer-by-name buf-name))
             (fr (app-state-frame app)))
        (set! (app-state-mark-ring app) (cdr ring))
        (if buf
          (let ((ed (current-editor app)))
            (buffer-attach! ed buf)
            (set! (edit-window-buffer (current-window fr)) buf)
            (editor-goto-pos ed pos)
            (editor-scroll-caret ed)
            (echo-message! (app-state-echo app) "Mark popped"))
          (echo-error! (app-state-echo app)
            (string-append "Buffer gone: " buf-name)))))))

;;;============================================================================
;;; Registers
;;;============================================================================

(def (cmd-copy-to-register app)
  "Save region text to a register (C-x r s)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Copy to register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (ed (current-editor app))
             (buf (current-buffer-from-app app))
             (mark (buffer-mark buf)))
        (if (not mark)
          (echo-error! echo "No mark set")
          (let* ((pos (editor-get-current-pos ed))
                 (start (min pos mark))
                 (end (max pos mark))
                 (text (substring (editor-get-text ed) start end)))
            (hash-put! (app-state-registers app) reg-char text)
            (echo-message! echo
              (string-append "Copied to register " (string reg-char)))))))))

(def (cmd-insert-register app)
  "Insert text from a register (C-x r i)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Insert register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (cond
          ((not val)
           (echo-error! echo
             (string-append "Register " (string reg-char) " is empty")))
          ((string? val)
           (let* ((ed (current-editor app))
                  (pos (editor-get-current-pos ed)))
             (editor-insert-text ed pos val)
             (echo-message! echo
               (string-append "Inserted from register " (string reg-char)))))
          ;; Point register — jump instead
          ((pair? val)
           (let* ((buf-name (car val))
                  (reg-pos (cdr val))
                  (buf (buffer-by-name buf-name))
                  (fr (app-state-frame app)))
             (if buf
               (let ((ed (current-editor app)))
                 (buffer-attach! ed buf)
                 (set! (edit-window-buffer (current-window fr)) buf)
                 (editor-goto-pos ed reg-pos)
                 (editor-scroll-caret ed)
                 (echo-message! echo "Jumped to register"))
               (echo-error! echo
                 (string-append "Buffer gone: " buf-name))))))))))

(def (cmd-point-to-register app)
  "Save current position to a register (C-x r SPC)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Point to register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (ed (current-editor app))
             (buf (current-buffer-from-app app))
             (pos (editor-get-current-pos ed)))
        (hash-put! (app-state-registers app) reg-char
                   (cons (buffer-name buf) pos))
        (echo-message! echo
          (string-append "Position saved to register " (string reg-char)))))))

(def (cmd-jump-to-register app)
  "Jump to a position saved in a register (C-x r j)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Jump to register: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((reg-char (string-ref input 0))
             (val (hash-get (app-state-registers app) reg-char)))
        (cond
          ((not val)
           (echo-error! echo
             (string-append "Register " (string reg-char) " is empty")))
          ((pair? val)
           (let* ((buf-name (car val))
                  (pos (cdr val))
                  (buf (buffer-by-name buf-name)))
             (if buf
               (let ((ed (current-editor app)))
                 (buffer-attach! ed buf)
                 (set! (edit-window-buffer (current-window fr)) buf)
                 (editor-goto-pos ed pos)
                 (editor-scroll-caret ed)
                 (echo-message! echo "Jumped to register"))
               (echo-error! echo
                 (string-append "Buffer gone: " buf-name)))))
          ((string? val)
           ;; Text register — insert it
           (let* ((ed (current-editor app))
                  (pos (editor-get-current-pos ed)))
             (editor-insert-text ed pos val)
             (echo-message! echo
               (string-append "Inserted from register " (string reg-char))))))))))

;;;============================================================================
;;; Backward kill word, zap to char, goto char
;;;============================================================================

(def (cmd-backward-kill-word app)
  "Kill from point backward to the beginning of the previous word."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (when (> pos 0)
      ;; Skip whitespace/non-word chars backward
      (let skip-space ((i (- pos 1)))
        (if (and (>= i 0) (not (word-char? (string-ref text i))))
          (skip-space (- i 1))
          ;; Now skip word chars backward
          (let skip-word ((j i))
            (if (and (>= j 0) (word-char? (string-ref text j)))
              (skip-word (- j 1))
              ;; j+1 is the start of the word
              (let* ((start (+ j 1))
                     (killed (substring text start pos)))
                (when (> (string-length killed) 0)
                  (set! (app-state-kill-ring app)
                    (cons killed (app-state-kill-ring app)))
                  (send-message ed SCI_DELETERANGE start (- pos start)))))))))))

(def (cmd-zap-to-char app)
  "Kill from point to the next occurrence of a character (inclusive)."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Zap to char: " row width)))
    (when (and input (> (string-length input) 0))
      (let* ((target-char (string-ref input 0))
             (ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed))
             ;; Search forward for the character
             (found-pos
               (let loop ((i pos))
                 (cond
                   ((>= i len) #f)
                   ((let ((ch (send-message ed SCI_GETCHARAT i 0)))
                      (= ch (char->integer target-char)))
                    (+ i 1))  ; inclusive of the target char
                   (else (loop (+ i 1)))))))
        (if found-pos
          (let ((text (substring (editor-get-text ed) pos found-pos)))
            ;; Add to kill ring
            (set! (app-state-kill-ring app)
              (cons text (app-state-kill-ring app)))
            ;; Delete the text
            (send-message ed SCI_DELETERANGE pos (- found-pos pos)))
          (echo-error! echo
            (string-append "Character '" (string target-char) "' not found")))))))

(def (cmd-goto-char app)
  "Go to a specific character position in the buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (input (echo-read-string echo "Goto char: " row width)))
    (when input
      (let ((n (string->number input)))
        (if n
          (let ((ed (current-editor app)))
            (editor-goto-pos ed (max 0 (inexact->exact (floor n))))
            (editor-scroll-caret ed))
          (echo-error! echo "Invalid position"))))))

;;;============================================================================
;;; Replace string (non-interactive)
;;;============================================================================

(def (cmd-replace-string app)
  "Non-interactive replace: replace all occurrences of FROM with TO."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (from-str (echo-read-string echo "Replace string: " row width)))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (echo-read-string echo
                      (string-append "Replace \"" from-str "\" with: ")
                      row width)))
        (when to-str
          (let* ((ed (current-editor app))
                 (count 0))
            (send-message ed SCI_BEGINUNDOACTION)
            ;; Search from beginning
            (send-message ed SCI_SETTARGETSTART 0 0)
            (send-message ed SCI_SETTARGETEND (editor-get-text-length ed) 0)
            (send-message ed SCI_SETSEARCHFLAGS 0 0)
            (let loop ()
              (let ((found (send-message/string ed SCI_SEARCHINTARGET from-str)))
                (when (>= found 0)
                  (send-message/string ed SCI_REPLACETARGET to-str)
                  (set! count (+ count 1))
                  ;; Set target for next search
                  (let ((new-start (+ found (string-length to-str))))
                    (send-message ed SCI_SETTARGETSTART new-start 0)
                    (send-message ed SCI_SETTARGETEND (editor-get-text-length ed) 0)
                    (loop)))))
            (send-message ed SCI_ENDUNDOACTION)
            (echo-message! echo
              (string-append "Replaced " (number->string count)
                             " occurrences"))))))))

;;;============================================================================
;;; Transpose words and lines
;;;============================================================================

(def (cmd-transpose-words app)
  "Swap the word before the cursor with the word after."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Find end of current word
    (let find-word2-end ((i pos))
      (if (and (< i len) (word-char? (string-ref text i)))
        (find-word2-end (+ i 1))
        ;; i is end of word2; find start of word2
        (let find-word2-start ((j (- i 1)))
          (if (and (>= j 0) (word-char? (string-ref text j)))
            (find-word2-start (- j 1))
            ;; j+1 is start of word2
            (let ((w2-start (+ j 1))
                  (w2-end i))
              ;; Find end of word1 (skip non-word chars backward from w2-start)
              (let find-word1-end ((k (- w2-start 1)))
                (if (and (>= k 0) (not (word-char? (string-ref text k))))
                  (find-word1-end (- k 1))
                  ;; k is last char of word1
                  (when (>= k 0)
                    (let find-word1-start ((m k))
                      (if (and (>= m 0) (word-char? (string-ref text m)))
                        (find-word1-start (- m 1))
                        ;; m+1 is start of word1
                        (let* ((w1-start (+ m 1))
                               (w1-end (+ k 1))
                               (word1 (substring text w1-start w1-end))
                               (word2 (substring text w2-start w2-end))
                               (between (substring text w1-end w2-start))
                               (new-text (string-append word2 between word1)))
                          (send-message ed SCI_BEGINUNDOACTION)
                          (send-message ed SCI_DELETERANGE w1-start (- w2-end w1-start))
                          (editor-insert-text ed w1-start new-text)
                          (editor-goto-pos ed w2-end)
                          (send-message ed SCI_ENDUNDOACTION))))))))))))))

(def (cmd-transpose-lines app)
  "Swap the current line with the line above."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos)))
    (when (> cur-line 0)
      (let* ((text (editor-get-text ed))
             (cur-start (send-message ed SCI_POSITIONFROMLINE cur-line 0))
             (cur-end (send-message ed SCI_GETLINEENDPOSITION cur-line 0))
             (prev-start (send-message ed SCI_POSITIONFROMLINE (- cur-line 1) 0))
             (prev-end (send-message ed SCI_GETLINEENDPOSITION (- cur-line 1) 0))
             (cur-text (substring text cur-start cur-end))
             (prev-text (substring text prev-start prev-end)))
        (send-message ed SCI_BEGINUNDOACTION)
        ;; Replace current line with previous, and previous with current
        ;; Do it by replacing from prev-start to cur-end
        (send-message ed SCI_DELETERANGE prev-start (- cur-end prev-start))
        (let ((new-text (string-append cur-text "\n" prev-text)))
          (editor-insert-text ed prev-start new-text))
        ;; Move cursor to end of what was the current line (now on the line below)
        (editor-goto-pos ed (+ prev-start (string-length cur-text) 1
                               (string-length prev-text)))
        (send-message ed SCI_ENDUNDOACTION)))))

;;;============================================================================
;;; Just one space, repeat command
;;;============================================================================

(def (cmd-just-one-space app)
  "Delete all spaces and tabs around point, leaving just one space."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Find extent of whitespace around point
    (let* ((start (let back ((i (- pos 1)))
                    (if (and (>= i 0)
                             (let ((ch (string-ref text i)))
                               (or (char=? ch #\space) (char=? ch #\tab))))
                      (back (- i 1))
                      (+ i 1))))
           (end (let fwd ((i pos))
                  (if (and (< i len)
                           (let ((ch (string-ref text i)))
                             (or (char=? ch #\space) (char=? ch #\tab))))
                    (fwd (+ i 1))
                    i))))
      (when (> (- end start) 1)
        (send-message ed SCI_BEGINUNDOACTION)
        (send-message ed SCI_DELETERANGE start (- end start))
        (editor-insert-text ed start " ")
        (editor-goto-pos ed (+ start 1))
        (send-message ed SCI_ENDUNDOACTION)))))

(def (cmd-repeat app)
  "Repeat the last command."
  (let* ((last (app-state-last-command app))
         (count (abs (get-prefix-arg app))))
    (cond
     ((or (not last) (eq? last 'repeat))
      (echo-error! (app-state-echo app) "No command to repeat"))
     ((= count 0)
      (echo-error! (app-state-echo app) "Repeat count must be positive"))
     (else
      (let loop ((i 0))
        (when (< i count)
          (execute-command! app last)
          (loop (+ i 1))))))))

;;;============================================================================
;;; Next/previous error (placeholder — navigate search results)
;;;============================================================================

(def (cmd-next-error app)
  "Jump to next error/match position (wraps search forward)."
  (let* ((ed (current-editor app))
         (search (app-state-last-search app)))
    (if (not search)
      (echo-error! (app-state-echo app) "No previous search")
      (let* ((pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed)))
        ;; Search forward from current position
        (send-message ed SCI_SETTARGETSTART (+ pos 1) 0)
        (send-message ed SCI_SETTARGETEND len 0)
        (send-message ed SCI_SETSEARCHFLAGS 0 0)
        (let ((found (send-message/string ed SCI_SEARCHINTARGET search)))
          (if (>= found 0)
            (begin
              (editor-goto-pos ed found)
              (editor-scroll-caret ed))
            ;; Wrap around
            (begin
              (send-message ed SCI_SETTARGETSTART 0 0)
              (send-message ed SCI_SETTARGETEND pos 0)
              (let ((found2 (send-message/string ed SCI_SEARCHINTARGET search)))
                (if (>= found2 0)
                  (begin
                    (editor-goto-pos ed found2)
                    (editor-scroll-caret ed)
                    (echo-message! (app-state-echo app) "Wrapped"))
                  (echo-error! (app-state-echo app) "No more matches"))))))))))

(def (cmd-previous-error app)
  "Jump to previous error/match position (wraps search backward)."
  (let* ((ed (current-editor app))
         (search (app-state-last-search app)))
    (if (not search)
      (echo-error! (app-state-echo app) "No previous search")
      (let* ((pos (editor-get-current-pos ed))
             (len (editor-get-text-length ed)))
        ;; Search backward: set target from pos-1 back to 0
        (send-message ed SCI_SETTARGETSTART (max 0 (- pos 1)) 0)
        (send-message ed SCI_SETTARGETEND 0 0)
        (send-message ed SCI_SETSEARCHFLAGS 0 0)
        (let ((found (send-message/string ed SCI_SEARCHINTARGET search)))
          (if (>= found 0)
            (begin
              (editor-goto-pos ed found)
              (editor-scroll-caret ed))
            ;; Wrap around from end
            (begin
              (send-message ed SCI_SETTARGETSTART len 0)
              (send-message ed SCI_SETTARGETEND pos 0)
              (let ((found2 (send-message/string ed SCI_SEARCHINTARGET search)))
                (if (>= found2 0)
                  (begin
                    (editor-goto-pos ed found2)
                    (editor-scroll-caret ed)
                    (echo-message! (app-state-echo app) "Wrapped"))
                  (echo-error! (app-state-echo app) "No more matches"))))))))))

;;;============================================================================
;;; Kill whole line, move line up/down
;;;============================================================================

(def (cmd-kill-whole-line app)
  "Kill the entire current line including the newline."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (next-line-start (send-message ed SCI_POSITIONFROMLINE (+ line 1) 0))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; If last line (no next line), delete to end including preceding newline
    (if (= next-line-start 0)
      (let* ((del-start (if (and (> line-start 0)
                                 (char=? (string-ref text (- line-start 1)) #\newline))
                          (- line-start 1) line-start))
             (killed (substring text del-start len)))
        (set! (app-state-kill-ring app)
          (cons killed (app-state-kill-ring app)))
        (send-message ed SCI_DELETERANGE del-start (- len del-start)))
      ;; Normal case: delete from line-start to next-line-start
      (let ((killed (substring text line-start next-line-start)))
        (set! (app-state-kill-ring app)
          (cons killed (app-state-kill-ring app)))
        (send-message ed SCI_DELETERANGE line-start (- next-line-start line-start))))))

(def (cmd-move-line-up app)
  "Move the current line up one position."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos)))
    (when (> cur-line 0)
      (let* ((text (editor-get-text ed))
             (cur-start (send-message ed SCI_POSITIONFROMLINE cur-line 0))
             (cur-end (send-message ed SCI_GETLINEENDPOSITION cur-line 0))
             (prev-start (send-message ed SCI_POSITIONFROMLINE (- cur-line 1) 0))
             (prev-end (send-message ed SCI_GETLINEENDPOSITION (- cur-line 1) 0))
             (cur-text (substring text cur-start cur-end))
             (prev-text (substring text prev-start prev-end))
             (col (- pos cur-start)))
        (send-message ed SCI_BEGINUNDOACTION)
        (send-message ed SCI_DELETERANGE prev-start (- cur-end prev-start))
        (editor-insert-text ed prev-start (string-append cur-text "\n" prev-text))
        ;; Put cursor on same column in moved line
        (editor-goto-pos ed (+ prev-start (min col (string-length cur-text))))
        (send-message ed SCI_ENDUNDOACTION)))))

(def (cmd-move-line-down app)
  "Move the current line down one position."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (line-count (editor-get-line-count ed)))
    (when (< cur-line (- line-count 1))
      (let* ((text (editor-get-text ed))
             (cur-start (send-message ed SCI_POSITIONFROMLINE cur-line 0))
             (cur-end (send-message ed SCI_GETLINEENDPOSITION cur-line 0))
             (next-start (send-message ed SCI_POSITIONFROMLINE (+ cur-line 1) 0))
             (next-end (send-message ed SCI_GETLINEENDPOSITION (+ cur-line 1) 0))
             (cur-text (substring text cur-start cur-end))
             (next-text (substring text next-start next-end))
             (col (- pos cur-start)))
        (send-message ed SCI_BEGINUNDOACTION)
        (send-message ed SCI_DELETERANGE cur-start (- next-end cur-start))
        (editor-insert-text ed cur-start (string-append next-text "\n" cur-text))
        ;; Put cursor on same column in moved line (now on line below)
        (let ((new-line-start (+ cur-start (string-length next-text) 1)))
          (editor-goto-pos ed (+ new-line-start (min col (string-length cur-text)))))
        (send-message ed SCI_ENDUNDOACTION)))))

;;;============================================================================
;;; Pipe buffer to shell, narrow/widen
;;;============================================================================

(def (cmd-pipe-buffer app)
  "Pipe buffer contents to a shell command and show output."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (cmd (echo-read-string echo "Pipe buffer to: " row width)))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((ed (current-editor app))
             (text (editor-get-text ed)))
        (with-catch
          (lambda (e)
            (echo-error! echo (string-append "Error: "
                                (with-output-to-string
                                  (lambda () (display-exception e))))))
          (lambda ()
            (let* ((proc (open-process
                           (list path: "/bin/sh"
                                 arguments: (list "-c" cmd)
                                 stdin-redirection: #t
                                 stdout-redirection: #t
                                 stderr-redirection: #t)))
                   (_ (begin (display text proc)
                             (close-output-port proc)))
                   (output (read-line proc #f))
                   (status (process-status proc)))
              ;; Show output in a new buffer
              (if (and output (> (string-length output) 0))
                (let* ((buf-name "*Shell Output*")
                       (existing (buffer-by-name buf-name)))
                  (when existing
                    (buffer-list-remove! existing))
                  (let ((buf (buffer-create! buf-name ed #f)))
                    (buffer-attach! ed buf)
                    (set! (edit-window-buffer (current-window fr)) buf)
                    (editor-set-text ed output)
                    (editor-goto-pos ed 0)
                    (echo-message! echo
                      (string-append "Pipe complete (exit " (number->string status) ")"))))
                (echo-message! echo
                  (string-append "No output (exit " (number->string status) ")"))))))))))

;; Narrowing state: hash-table of buffer -> (full-text start end)
(def *tui-narrow-state* (make-hash-table))

(def (cmd-narrow-to-region app)
  "Narrow to the current region (between point and mark)."
  (let* ((ed (current-editor app))
         (fr (app-state-frame app))
         (buf (edit-window-buffer (current-window fr)))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (editor-get-text ed))
             (region (substring text start end)))
        ;; Save full text and narrow bounds
        (hash-put! *tui-narrow-state* buf (list text start end))
        ;; Replace buffer text with region only
        (editor-set-text ed region)
        (editor-goto-pos ed 0)
        (editor-set-save-point ed)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Narrowed"))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-widen app)
  "Widen from narrowed region, merging edits back."
  (let* ((fr (app-state-frame app))
         (buf (edit-window-buffer (current-window fr)))
         (state (hash-get *tui-narrow-state* buf)))
    (if state
      (let* ((ed (current-editor app))
             (full-text (car state))
             (start (cadr state))
             (end (caddr state))
             (narrow-text (editor-get-text ed))
             (new-text (string-append
                         (substring full-text 0 start)
                         narrow-text
                         (substring full-text end (string-length full-text)))))
        (editor-set-text ed new-text)
        (editor-goto-pos ed start)
        (editor-set-save-point ed)
        (hash-remove! *tui-narrow-state* buf)
        (echo-message! (app-state-echo app) "Widened"))
      (echo-error! (app-state-echo app) "Buffer is not narrowed"))))

;;;============================================================================
;;; String rectangle, open rectangle
;;;============================================================================

(def (cmd-string-rectangle app)
  "Replace rectangle region with a string on each line."
  (let ((echo (app-state-echo app)))
    (let-values (((start-line start-col end-line end-col) (get-region-lines app)))
      (if (not start-line)
        (echo-error! echo "No region (set mark first)")
        (let* ((fr (app-state-frame app))
               (row (- (frame-height fr) 1))
               (width (frame-width fr))
               (str (echo-read-string echo "String rectangle: " row width)))
          (if (not str)
            (echo-message! echo "Cancelled")
            (let* ((ed (current-editor app))
                   (left-col (min start-col end-col))
                   (right-col (max start-col end-col)))
              (with-undo-action ed
                ;; Process lines from bottom to top to preserve positions
                (let loop ((line end-line))
                  (when (>= line start-line)
                    (let* ((line-start (editor-position-from-line ed line))
                           (line-text (editor-get-line ed line))
                           (line-len (string-length (string-trim-right line-text #\newline)))
                           (l (min left-col line-len))
                           (r (min right-col line-len)))
                      ;; Delete old rectangle portion and insert replacement
                      (when (< l r)
                        (editor-delete-range ed (+ line-start l) (- r l)))
                      (editor-insert-text ed (+ line-start l) str))
                    (loop (- line 1)))))
              (set! (buffer-mark (current-buffer-from-app app)) #f)
              (echo-message! echo "String rectangle done"))))))))

(def (cmd-open-rectangle app)
  "Insert blank space to fill the rectangle region."
  (let ((echo (app-state-echo app)))
    (let-values (((start-line start-col end-line end-col) (get-region-lines app)))
      (if (not start-line)
        (echo-error! echo "No region (set mark first)")
        (let* ((ed (current-editor app))
               (left-col (min start-col end-col))
               (right-col (max start-col end-col))
               (width (- right-col left-col)))
          (when (> width 0)
            (with-undo-action ed
              ;; Insert spaces from bottom to top
              (let loop ((line end-line))
                (when (>= line start-line)
                  (let* ((line-start (editor-position-from-line ed line))
                         (line-text (editor-get-line ed line))
                         (line-len (string-length (string-trim-right line-text #\newline)))
                         (insert-pos (+ line-start (min left-col line-len))))
                    ;; Pad if line is shorter than left-col
                    (when (< line-len left-col)
                      (editor-insert-text ed (+ line-start line-len)
                        (make-string (- left-col line-len) #\space)))
                    (editor-insert-text ed (+ line-start (min left-col line-len))
                      (make-string width #\space)))
                  (loop (- line 1))))))
          (set! (buffer-mark (current-buffer-from-app app)) #f)
          (echo-message! echo
            (string-append "Opened rectangle (" (number->string width) " cols)")))))))

;;;============================================================================
;;; Number lines, reverse region
;;;============================================================================

(def (cmd-number-lines app)
  "Number lines in the region (or whole buffer)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if mark
      ;; Region
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end))
             (num-lines (+ (- end-line start-line) 1))
             (width (string-length (number->string num-lines))))
        (with-undo-action ed
          ;; Insert from bottom to top to preserve positions
          (let loop ((line end-line) (n num-lines))
            (when (>= line start-line)
              (let* ((line-start (editor-position-from-line ed line))
                     (prefix (string-append
                               (let ((s (number->string n)))
                                 (string-append
                                   (make-string (- width (string-length s)) #\space)
                                   s))
                               ": ")))
                (editor-insert-text ed line-start prefix))
              (loop (- line 1) (- n 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! echo
          (string-append "Numbered " (number->string num-lines) " lines")))
      ;; Whole buffer
      (let* ((total (editor-get-line-count ed))
             (width (string-length (number->string total))))
        (with-undo-action ed
          (let loop ((line (- total 1)) (n total))
            (when (>= line 0)
              (let* ((line-start (editor-position-from-line ed line))
                     (prefix (string-append
                               (let ((s (number->string n)))
                                 (string-append
                                   (make-string (- width (string-length s)) #\space)
                                   s))
                               ": ")))
                (editor-insert-text ed line-start prefix))
              (loop (- line 1) (- n 1)))))
        (echo-message! echo
          (string-append "Numbered " (number->string total) " lines"))))))

(def (cmd-reverse-region app)
  "Reverse the order of lines in the region (or whole buffer)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf)))
    (if mark
      ;; Region
      (let* ((pos (editor-get-current-pos ed))
             (start (min mark pos))
             (end (max mark pos))
             (region (substring (editor-get-text ed) start end))
             (lines (string-split region #\newline))
             (reversed (reverse lines))
             (result (string-join reversed "\n")))
        (with-undo-action ed
          (editor-delete-range ed start (- end start))
          (editor-insert-text ed start result))
        (set! (buffer-mark buf) #f)
        (echo-message! echo
          (string-append "Reversed " (number->string (length reversed)) " lines")))
      ;; Whole buffer
      (let* ((text (editor-get-text ed))
             (lines (string-split text #\newline))
             (reversed (reverse lines))
             (result (string-join reversed "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! echo
          (string-append "Reversed " (number->string (length reversed)) " lines"))))))

;;;============================================================================
;;; Flush lines, keep lines
;;;============================================================================

(def (cmd-flush-lines app)
  "Delete lines matching a regexp pattern."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Flush lines matching: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (original-count (length lines))
             (kept (filter (lambda (line) (not (string-contains line pattern))) lines))
             (removed (- original-count (length kept)))
             (result (string-join kept "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! echo
          (string-append "Flushed " (number->string removed) " lines"))))))

(def (cmd-keep-lines app)
  "Keep only lines matching a pattern."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Keep lines matching: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (original-count (length lines))
             (kept (filter (lambda (line) (string-contains line pattern)) lines))
             (removed (- original-count (length kept)))
             (result (string-join kept "\n"))
             (pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (editor-get-text-length ed)))
        (echo-message! echo
          (string-append "Kept " (number->string (length kept))
                         " lines, removed " (number->string removed)))))))

;;;============================================================================
;;; Align regexp
;;;============================================================================

(def (cmd-align-regexp app)
  "Align lines on a substring pattern."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (pattern (echo-read-string echo "Align on: " row width)))
    (if (not pattern)
      (echo-message! echo "Cancelled")
      (let* ((ed (current-editor app))
             (buf (current-buffer-from-app app))
             (mark (buffer-mark buf))
             (pos (editor-get-current-pos ed))
             (text (editor-get-text ed)))
        ;; Determine range
        (let-values (((start end)
                      (if mark
                        (values (min mark pos) (max mark pos))
                        (values 0 (string-length text)))))
          (let* ((region (substring text start end))
                 (lines (string-split region #\newline))
                 ;; Find max column position of the pattern
                 (positions (map (lambda (line)
                                  (let ((idx (string-contains line pattern)))
                                    (or idx -1)))
                                lines))
                 (max-col (apply max (cons 0 (filter (lambda (x) (>= x 0)) positions)))))
            (if (= max-col 0)
              (echo-error! echo (string-append "Pattern not found: " pattern))
              (let* ((aligned
                       (map (lambda (line)
                              (let ((idx (string-contains line pattern)))
                                (if idx
                                  (string-append
                                    (substring line 0 idx)
                                    (make-string (- max-col idx) #\space)
                                    (substring line idx (string-length line)))
                                  line)))
                            lines))
                     (result (string-join aligned "\n")))
                (with-undo-action ed
                  (editor-delete-range ed start (- end start))
                  (editor-insert-text ed start result))
                (when mark (set! (buffer-mark buf) #f))
                (echo-message! echo "Aligned")))))))))

;;;============================================================================
;;; Sort fields
;;;============================================================================

(def (cmd-sort-fields app)
  "Sort lines by whitespace-delimited field number."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (row (- (frame-height fr) 1))
         (width (frame-width fr))
         (field-str (echo-read-string echo "Sort by field #: " row width)))
    (if (not field-str)
      (echo-message! echo "Cancelled")
      (let ((field-num (string->number field-str)))
        (if (not field-num)
          (echo-error! echo "Invalid field number")
          (let* ((ed (current-editor app))
                 (buf (current-buffer-from-app app))
                 (mark (buffer-mark buf))
                 (pos (editor-get-current-pos ed))
                 (text (editor-get-text ed)))
            (let-values (((start end)
                          (if mark
                            (values (min mark pos) (max mark pos))
                            (values 0 (string-length text)))))
              (let* ((region (substring text start end))
                     (lines (string-split region #\newline))
                     (field-idx (- field-num 1))  ; 1-based to 0-based
                     (get-field
                       (lambda (line)
                         (let ((fields (string-split line #\space)))
                           ;; Filter out empty strings from split
                           (let ((fs (filter (lambda (s) (not (string-empty? s))) fields)))
                             (if (< field-idx (length fs))
                               (list-ref fs field-idx)
                               "")))))
                     (sorted (sort lines
                               (lambda (a b)
                                 (string<? (get-field a) (get-field b)))))
                     (result (string-join sorted "\n")))
                (with-undo-action ed
                  (editor-delete-range ed start (- end start))
                  (editor-insert-text ed start result))
                (when mark (set! (buffer-mark buf) #f))
                (echo-message! echo
                  (string-append "Sorted by field " field-str))))))))))

;;;============================================================================
;;; Mark word, mark paragraph, paragraph navigation
;;;============================================================================

(def (cmd-mark-word app)
  "Set mark at end of next word (like M-@ in Emacs)."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Set mark at current position if not already set
    (when (not (buffer-mark buf))
      (set! (buffer-mark buf) pos))
    ;; Find end of word from current pos
    (let skip-nonword ((i pos))
      (if (and (< i len) (not (word-char? (char->integer (string-ref text i)))))
        (skip-nonword (+ i 1))
        (let find-end ((j i))
          (if (and (< j len) (word-char? (char->integer (string-ref text j))))
            (find-end (+ j 1))
            (begin
              (editor-goto-pos ed j)
              (echo-message! (app-state-echo app) "Mark word"))))))))

(def (cmd-mark-paragraph app)
  "Select the current paragraph (M-h)."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find start of paragraph (search backward for blank line or BOF)
    (let find-start ((i pos))
      (let ((start
              (cond
                ((<= i 0) 0)
                ;; Check if we're at start of a blank line
                ((and (> i 1)
                      (char=? (string-ref text (- i 1)) #\newline)
                      (or (= i (string-length text))
                          (char=? (string-ref text i) #\newline)))
                 i)
                (else (find-start (- i 1))))))
        ;; Find end of paragraph (search forward for blank line or EOF)
        (let find-end ((j pos))
          (let ((end
                  (cond
                    ((>= j len) len)
                    ;; Blank line = two consecutive newlines
                    ((and (char=? (string-ref text j) #\newline)
                          (< (+ j 1) len)
                          (char=? (string-ref text (+ j 1)) #\newline))
                     (+ j 1))
                    (else (find-end (+ j 1))))))
            (set! (buffer-mark buf) start)
            (editor-goto-pos ed end)
            (echo-message! (app-state-echo app) "Mark paragraph")))))))

(def (cmd-forward-paragraph app)
  "Move forward to end of next paragraph."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    ;; Skip any blank lines at point
    (let skip-blank ((i pos))
      (if (and (< i len)
               (char=? (string-ref text i) #\newline))
        (skip-blank (+ i 1))
        ;; Now find next blank line or EOF
        (let find-end ((j i))
          (cond
            ((>= j len) (editor-goto-pos ed len))
            ((and (char=? (string-ref text j) #\newline)
                  (< (+ j 1) len)
                  (char=? (string-ref text (+ j 1)) #\newline))
             (editor-goto-pos ed (+ j 1)))
            (else (find-end (+ j 1)))))))))

(def (cmd-backward-paragraph app)
  "Move backward to start of previous paragraph."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    ;; Skip any blank lines at point
    (let skip-blank ((i (max 0 (- pos 1))))
      (if (and (> i 0)
               (char=? (string-ref text i) #\newline))
        (skip-blank (- i 1))
        ;; Now find previous blank line or BOF
        (let find-start ((j i))
          (cond
            ((<= j 0) (editor-goto-pos ed 0))
            ((and (char=? (string-ref text j) #\newline)
                  (> j 0)
                  (char=? (string-ref text (- j 1)) #\newline))
             (editor-goto-pos ed (+ j 1)))
            (else (find-start (- j 1)))))))))

;;;============================================================================
;;; Back to indentation, delete indentation
;;;============================================================================

(def (cmd-back-to-indentation app)
  "Move to first non-whitespace character on current line (M-m)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (text (editor-get-text ed))
         (len (string-length text)))
    (let find-nonws ((i line-start))
      (if (and (< i len)
               (let ((ch (string-ref text i)))
                 (and (not (char=? ch #\newline))
                      (or (char=? ch #\space) (char=? ch #\tab)))))
        (find-nonws (+ i 1))
        (editor-goto-pos ed i)))))

(def (cmd-delete-indentation app)
  "Join current line with previous, removing indentation (M-^)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos)))
    (when (> line 0)
      ;; Go to beginning of current line
      (let* ((line-start (editor-position-from-line ed line))
             (prev-end (editor-get-line-end-position ed (- line 1)))
             (text (editor-get-text ed))
             ;; Find end of whitespace at start of current line
             (ws-end line-start))
        (let skip ((i line-start))
          (when (< i (string-length text))
            (let ((ch (string-ref text i)))
              (when (or (char=? ch #\space) (char=? ch #\tab))
                (set! ws-end (+ i 1))
                (skip (+ i 1))))))
        ;; Delete from end of previous line through whitespace, insert space
        (with-undo-action ed
          (editor-delete-range ed prev-end (- ws-end prev-end))
          (editor-insert-text ed prev-end " "))))))

;;;============================================================================
;;; Whitespace navigation/cleanup
;;;============================================================================

(def (cmd-cycle-spacing app)
  "Cycle between: collapse whitespace to one space, remove all, restore original."
  ;; Simplified: just collapse to single space (same as just-one-space)
  (cmd-just-one-space app))

(def (cmd-fixup-whitespace app)
  "Fix up whitespace around point (collapse multiple spaces/tabs to one)."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find whitespace range around point
    (let find-start ((i (- pos 1)))
      (let ((ws-start
              (if (and (>= i 0)
                       (let ((ch (string-ref text i)))
                         (or (char=? ch #\space) (char=? ch #\tab))))
                (find-start (- i 1))
                (+ i 1))))
        (let find-end ((j pos))
          (let ((ws-end
                  (if (and (< j len)
                           (let ((ch (string-ref text j)))
                             (or (char=? ch #\space) (char=? ch #\tab))))
                    (find-end (+ j 1))
                    j)))
            (when (> (- ws-end ws-start) 1)
              (with-undo-action ed
                (editor-delete-range ed ws-start (- ws-end ws-start))
                (editor-insert-text ed ws-start " ")))))))))

;;;============================================================================
;;; Scintilla autocomplete popup
;;;============================================================================

(def (cmd-complete-at-point app)
  "Show autocomplete popup using Scintilla's built-in autocompletion.
   Collects word candidates from the buffer matching the prefix at point."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find prefix at cursor
    (let* ((prefix-start
             (let loop ((i (- pos 1)))
               (if (or (< i 0)
                       (let ((ch (string-ref text i)))
                         (not (or (char-alphabetic? ch)
                                  (char-numeric? ch)
                                  (char=? ch #\_)
                                  (char=? ch #\-)
                                  (char=? ch #\?)
                                  (char=? ch #\!)))))
                 (+ i 1) (loop (- i 1)))))
           (prefix (substring text prefix-start pos))
           (plen (string-length prefix)))
      (if (= plen 0)
        (echo-message! echo "No prefix to complete")
        ;; Collect unique word candidates from buffer
        (let ((candidates (make-hash-table)))
          (let scan ((i 0))
            (when (< i len)
              ;; Skip non-word chars
              (let skip ((j i))
                (if (or (>= j len)
                        (let ((ch (string-ref text j)))
                          (or (char-alphabetic? ch) (char-numeric? ch)
                              (char=? ch #\_) (char=? ch #\-)
                              (char=? ch #\?) (char=? ch #\!))))
                  ;; Found word start
                  (let word-end ((k j))
                    (if (or (>= k len)
                            (let ((ch (string-ref text k)))
                              (not (or (char-alphabetic? ch) (char-numeric? ch)
                                       (char=? ch #\_) (char=? ch #\-)
                                       (char=? ch #\?) (char=? ch #\!)))))
                      (begin
                        (when (> k j)
                          (let ((word (substring text j k)))
                            (when (and (> (string-length word) plen)
                                       (string-prefix? prefix word)
                                       (not (= j prefix-start)))
                              (hash-put! candidates word #t))))
                        (scan k))
                      (word-end (+ k 1))))
                  (skip (+ j 1))))))
          ;; Show popup if candidates found
          (let ((words (sort (hash-keys candidates) string<?)))
            (if (null? words)
              (echo-message! echo (string-append "No completions for \"" prefix "\""))
              (begin
                ;; Configure autocomplete
                (send-message ed SCI_AUTOCSETSEPARATOR (char->integer #\newline) 0)
                (send-message ed SCI_AUTOCSETIGNORECASE 0 0)
                (send-message ed SCI_AUTOCSETMAXHEIGHT 10 0)
                (send-message ed SCI_AUTOCSETDROPRESTOFWORD 1 0)
                ;; SC_ORDER_PERFORMSORT=2 means we provide sorted list
                (send-message ed SCI_AUTOCSETORDER 1 0)
                ;; Build item list separated by newlines
                (let ((item-list (string-join words "\n")))
                  (scintilla-send-message-string
                    (scintilla-editor-handle ed) SCI_AUTOCSHOW plen item-list))
                (echo-message! echo
                  (string-append (number->string (length words)) " completions"))))))))))

;;;============================================================================
;;; Calltip display (diagnostic tooltips, hover info)
;;;============================================================================

(def (show-calltip! ed pos text . opts)
  "Show a calltip at POS with TEXT. Optional keyword args: bg-color, fg-color.
   Colors are in Scintilla BGR format (use rgb->sci from sci-shim)."
  (let ((bg (if (and (pair? opts) (pair? (cdr opts)))
              (cadr opts)
              #f))
        (fg (if (and (pair? opts) (pair? (cdr opts)) (pair? (cddr opts))
                     (pair? (cdddr opts)))
              (cadddr opts)
              #f)))
    (when bg (send-message ed SCI_CALLTIPSETBACK bg))
    (when fg (send-message ed SCI_CALLTIPSETFORE fg))
    (scintilla-send-message-string
      (scintilla-editor-handle ed) SCI_CALLTIPSHOW
      (max 0 pos) text)))

(def (cancel-calltip! ed)
  "Cancel any active calltip."
  (when (not (zero? (send-message ed SCI_CALLTIPACTIVE)))
    (send-message ed SCI_CALLTIPCANCEL)))

