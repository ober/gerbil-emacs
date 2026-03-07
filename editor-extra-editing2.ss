;;; -*- Gerbil -*-
;;; Multi-cursor, occur, markdown, dired, diff, encoding, word count,
;;; comment-dwim, kill sentence/paragraph, and s-expression navigation.
;;; Split from editor-extra-editing.ss to keep files under 2000 lines.

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
        :gemacs/editor-extra-helpers
        :gemacs/editor-extra-editing)

;;;============================================================================
;;; Real multi-selection commands (using Scintilla multi-selection API)
;;;============================================================================

(def (cmd-mc-real-add-next app)
  "Add a real cursor at the next occurrence of the current selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (editor-selection-empty? ed)
      (echo-error! echo "Select text first, then mark next")
      (begin
        (send-message ed SCI_MULTIPLESELECTADDNEXT 0 0)
        (let ((n (send-message ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n) " cursors")))))))

(def (cmd-mc-real-add-all app)
  "Add real cursors at all occurrences of the current selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (editor-selection-empty? ed)
      (echo-error! echo "Select text first, then mark all")
      (begin
        (send-message ed SCI_MULTIPLESELECTADDEACH 0 0)
        (let ((n (send-message ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n) " cursors")))))))

(def (cmd-mc-skip-and-add-next app)
  "Skip the current selection and add next occurrence."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (editor-selection-empty? ed)
      (echo-error! echo "Select text first")
      (let ((n (send-message ed SCI_GETSELECTIONS 0 0)))
        (when (> n 1)
          ;; Drop the main selection
          (let ((main (send-message ed SCI_GETMAINSELECTION 0 0)))
            (send-message ed SCI_DROPSELECTIONN main 0)))
        ;; Add next
        (send-message ed SCI_MULTIPLESELECTADDNEXT 0 0)
        (let ((n2 (send-message ed SCI_GETSELECTIONS 0 0)))
          (echo-message! echo
            (string-append (number->string n2) " cursors")))))))

(def (cmd-mc-cursors-on-lines app)
  "Add a cursor at the end of each line in the current selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! echo "Select a region first")
      (let* ((start-line (editor-line-from-position ed sel-start))
             (end-line (editor-line-from-position ed sel-end))
             (num-lines (+ 1 (- end-line start-line))))
        (when (> num-lines 1)
          ;; Set first selection at end of first line
          (let ((eol0 (editor-get-line-end-position ed start-line)))
            (send-message ed SCI_SETSELECTION eol0 eol0)
            ;; Add selections at end of subsequent lines
            (let loop ((line (+ start-line 1)))
              (when (<= line end-line)
                (let ((eol (editor-get-line-end-position ed line)))
                  (send-message ed SCI_ADDSELECTION eol eol)
                  (loop (+ line 1)))))))
        (echo-message! echo
          (string-append (number->string num-lines)
                         " cursors on " (number->string num-lines) " lines"))))))

(def (cmd-mc-unmark-last app)
  "Remove the most recently added selection."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (n (send-message ed SCI_GETSELECTIONS 0 0)))
    (if (<= n 1)
      (echo-message! echo "Only one cursor")
      (begin
        (send-message ed SCI_DROPSELECTIONN (- n 1) 0)
        (echo-message! echo
          (string-append (number->string (- n 1)) " cursors"))))))

(def (cmd-mc-rotate app)
  "Cycle to the next selection as the main cursor."
  (let ((ed (current-editor app)))
    (send-message ed SCI_ROTATESELECTION 0 0)))

;;;============================================================================
;;; Occur goto-occurrence (TUI)
;;;============================================================================

(def (occur-parse-source-name text)
  "Parse source buffer name from *Occur* header: 'N matches for \"pat\" in NAME:'"
  (let ((in-pos (string-contains text " in ")))
    (and in-pos
         (let* ((after-in (+ in-pos 4))
                (colon-pos (string-index text #\: after-in)))
           (and colon-pos
                (substring text after-in colon-pos))))))

(def (cmd-occur-goto app)
  "Jump from *Occur* buffer to the source line under cursor."
  (let* ((buf (current-buffer-from-app app))
         (echo (app-state-echo app)))
    (if (not (string=? (buffer-name buf) "*Occur*"))
      (echo-error! echo "Not in *Occur* buffer")
      (let* ((ed (current-editor app))
             (full-text (editor-get-text ed))
             (source-name (occur-parse-source-name full-text)))
        (if (not source-name)
          (echo-error! echo "Cannot determine source buffer")
          (let* ((pos (editor-get-current-pos ed))
                 (line-num (editor-line-from-position ed pos))
                 (line-text (editor-get-line ed line-num)))
            ;; Parse "NNN:text" format
            (let ((colon-pos (string-index line-text #\:)))
              (if (not colon-pos)
                (echo-error! echo "Not on an occur match line")
                (let ((target-line (string->number
                                     (substring line-text 0 colon-pos))))
                  (if (not target-line)
                    (echo-error! echo "Not on an occur match line")
                    ;; Switch to source buffer and jump
                    (let ((source (buffer-by-name source-name)))
                      (if (not source)
                        (echo-error! echo
                          (string-append "Source buffer '"
                                         source-name "' not found"))
                        (let ((fr (app-state-frame app)))
                          (buffer-attach! ed source)
                          (set! (edit-window-buffer (current-window fr)) source)
                          (editor-goto-line ed (- target-line 1))
                          (editor-scroll-caret ed)
                          (echo-message! echo
                            (string-append "Line "
                                           (number->string
                                             target-line))))))))))))))))

(def (cmd-occur-next app)
  "Move to the next match line in *Occur* buffer."
  (let* ((buf (current-buffer-from-app app))
         (echo (app-state-echo app)))
    (when (string=? (buffer-name buf) "*Occur*")
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (total-lines (send-message ed SCI_GETLINECOUNT 0 0))
             (cur-line (editor-line-from-position ed pos)))
        (let loop ((l (+ cur-line 1)))
          (when (< l total-lines)
            (let ((text (editor-get-line ed l)))
              (if (and (> (string-length text) 0)
                       (char-numeric? (string-ref text 0))
                       (string-index text #\:))
                (begin
                  (editor-goto-line ed l)
                  (editor-scroll-caret ed))
                (loop (+ l 1))))))))))

(def (cmd-occur-prev app)
  "Move to the previous match line in *Occur* buffer."
  (let* ((buf (current-buffer-from-app app))
         (echo (app-state-echo app)))
    (when (string=? (buffer-name buf) "*Occur*")
      (let* ((ed (current-editor app))
             (pos (editor-get-current-pos ed))
             (cur-line (editor-line-from-position ed pos)))
        (let loop ((l (- cur-line 1)))
          (when (>= l 0)
            (let ((text (editor-get-line ed l)))
              (if (and (> (string-length text) 0)
                       (char-numeric? (string-ref text 0))
                       (string-index text #\:))
                (begin
                  (editor-goto-line ed l)
                  (editor-scroll-caret ed))
                (loop (- l 1))))))))))

;;;============================================================================
;;; Markdown mode commands
;;;============================================================================

(def (markdown-wrap-selection ed prefix suffix)
  "Wrap current selection with prefix/suffix or insert them at point."
  (if (editor-selection-empty? ed)
    ;; No selection: insert prefix+suffix and place cursor between
    (let ((pos (editor-get-current-pos ed)))
      (editor-insert-text ed pos (string-append prefix suffix))
      (editor-goto-pos ed (+ pos (string-length prefix))))
    ;; Wrap selection
    (let* ((start (editor-get-selection-start ed))
           (end (editor-get-selection-end ed))
           (text (editor-get-text ed))
           (sel (substring text start end)))
      (send-message ed SCI_SETTARGETSTART start 0)
      (send-message ed SCI_SETTARGETEND end 0)
      (send-message/string ed SCI_REPLACETARGET
        (string-append prefix sel suffix)))))

(def (cmd-markdown-bold app)
  "Insert or wrap selection with bold markers **text**."
  (let ((ed (current-editor app)))
    (markdown-wrap-selection ed "**" "**")))

(def (cmd-markdown-italic app)
  "Insert or wrap selection with italic markers *text*."
  (let ((ed (current-editor app)))
    (markdown-wrap-selection ed "*" "*")))

(def (cmd-markdown-code app)
  "Insert or wrap selection with inline code backticks `text`."
  (let ((ed (current-editor app)))
    (markdown-wrap-selection ed "`" "`")))

(def (cmd-markdown-code-block app)
  "Insert a fenced code block."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (lang (app-read-string app "Language: ")))
    (editor-insert-text ed pos
      (string-append "```" (or lang "") "\n\n```\n"))
    (editor-goto-pos ed (+ pos 4 (string-length (or lang ""))))))

(def (cmd-markdown-heading app)
  "Insert or cycle heading level (# through ######)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    ;; Count existing # prefix
    (let ((hashes (let loop ((i 0))
                    (if (and (< i (string-length line-text))
                             (char=? (string-ref line-text i) #\#))
                      (loop (+ i 1)) i))))
      (send-message ed SCI_SETTARGETSTART line-start 0)
      (send-message ed SCI_SETTARGETEND line-end 0)
      (cond
        ((= hashes 0)
         ;; No heading: add #
         (send-message/string ed SCI_REPLACETARGET
           (string-append "# " line-text)))
        ((>= hashes 6)
         ;; Max level: remove all hashes
         (let ((stripped (string-trim line-text)))
           (send-message/string ed SCI_REPLACETARGET
             (let loop ((s stripped))
               (if (and (> (string-length s) 0)
                        (char=? (string-ref s 0) #\#))
                 (loop (substring s 1 (string-length s)))
                 (string-trim s))))))
        (else
         ;; Increase level
         (send-message/string ed SCI_REPLACETARGET
           (string-append "#" line-text)))))))

(def (cmd-markdown-link app)
  "Insert a markdown link [text](url)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-text (if (editor-selection-empty? ed) ""
                     (let* ((s (editor-get-selection-start ed))
                            (e (editor-get-selection-end ed))
                            (text (editor-get-text ed)))
                       (substring text s e))))
         (url (app-read-string app "URL: ")))
    (when (and url (not (string-empty? url)))
      (let* ((text (if (string-empty? sel-text) url sel-text))
             (link (string-append "[" text "](" url ")")))
        (if (editor-selection-empty? ed)
          (editor-insert-text ed (editor-get-current-pos ed) link)
          (let ((start (editor-get-selection-start ed))
                (end (editor-get-selection-end ed)))
            (send-message ed SCI_SETTARGETSTART start 0)
            (send-message ed SCI_SETTARGETEND end 0)
            (send-message/string ed SCI_REPLACETARGET link)))))))

(def (cmd-markdown-image app)
  "Insert a markdown image ![alt](url)."
  (let* ((ed (current-editor app))
         (alt (or (app-read-string app "Alt text: ") ""))
         (url (app-read-string app "Image URL: ")))
    (when (and url (not (string-empty? url)))
      (editor-insert-text ed (editor-get-current-pos ed)
        (string-append "![" alt "](" url ")")))))

(def (cmd-markdown-hr app)
  "Insert a horizontal rule."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "\n---\n")))

(def (cmd-markdown-list-item app)
  "Insert a list item. If current line starts with - or *, continue the list."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    ;; Detect list marker
    (let ((marker (cond
                    ((string-prefix? "- " line-text) "- ")
                    ((string-prefix? "* " line-text) "* ")
                    ((string-prefix? "  - " line-text) "  - ")
                    ((string-prefix? "  * " line-text) "  * ")
                    (else "- "))))
      (editor-goto-pos ed line-end)
      (editor-insert-text ed line-end (string-append "\n" marker)))))

(def (cmd-markdown-checkbox app)
  "Insert a markdown checkbox - [ ] item."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos "- [ ] ")))

(def (cmd-markdown-toggle-checkbox app)
  "Toggle a markdown checkbox between [ ] and [x]."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    (send-message ed SCI_SETTARGETSTART line-start 0)
    (send-message ed SCI_SETTARGETEND line-end 0)
    (cond
      ((string-contains line-text "[ ]")
       (send-message/string ed SCI_REPLACETARGET
         (string-subst line-text "[ ]" "[x]")))
      ((string-contains line-text "[x]")
       (send-message/string ed SCI_REPLACETARGET
         (string-subst line-text "[x]" "[ ]")))
      (else
       (echo-message! (app-state-echo app) "No checkbox on this line")))))

(def (cmd-markdown-table app)
  "Insert a markdown table template."
  (let* ((ed (current-editor app))
         (cols-str (or (app-read-string app "Columns (default 3): ") "3"))
         (cols (or (string->number cols-str) 3))
         (pos (editor-get-current-pos ed)))
    (let* ((header (string-join (make-list cols " Header ") "|"))
           (sep (string-join (make-list cols "--------") "|"))
           (row (string-join (make-list cols "        ") "|"))
           (table (string-append "| " header " |\n| " sep " |\n| " row " |\n")))
      (editor-insert-text ed pos table))))

(def (cmd-markdown-preview-outline app)
  "Show an outline of markdown headings in the current buffer."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (headings
           (let loop ((ls lines) (n 0) (acc []))
             (if (null? ls)
               (reverse acc)
               (let ((l (car ls)))
                 (if (and (> (string-length l) 0) (char=? (string-ref l 0) #\#))
                   (loop (cdr ls) (+ n 1) (cons (cons n l) acc))
                   (loop (cdr ls) (+ n 1) acc)))))))
    (if (null? headings)
      (echo-message! (app-state-echo app) "No headings found")
      (let ((buf-text (string-join
                        (map (lambda (h)
                               (string-append (number->string (+ (car h) 1))
                                              ": " (cdr h)))
                             headings)
                        "\n")))
        (open-output-buffer app "*Markdown Outline*"
          (string-append "Headings\n\n" buf-text "\n"))))))

;;;============================================================================
;;; Dired improvements — mark and operate on files
;;;============================================================================

(def *dired-marks* (make-hash-table)) ;; filename -> #t for marked files

(def (cmd-dired-mark app)
  "Mark the file on the current line in dired."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    ;; Mark the file and add a visual indicator
    (let ((trimmed (string-trim line-text)))
      (when (> (string-length trimmed) 0)
        (hash-put! *dired-marks* trimmed #t)
        ;; Replace the line with marked indicator
        (send-message ed SCI_SETTARGETSTART line-start 0)
        (send-message ed SCI_SETTARGETEND line-end 0)
        (if (string-prefix? "* " line-text)
          #f ;; Already marked
          (send-message/string ed SCI_REPLACETARGET
            (string-append "* " line-text)))
        ;; Move to next line
        (send-message ed 2300 0 0)
        (echo-message! (app-state-echo app)
          (string-append "Marked: " trimmed))))))

(def (cmd-dired-unmark app)
  "Unmark the file on the current line in dired."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (line (send-message ed SCI_LINEFROMPOSITION
                 (editor-get-current-pos ed) 0))
         (line-start (send-message ed SCI_POSITIONFROMLINE line 0))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
         (line-text (if (< line-start line-end)
                      (substring text line-start line-end) "")))
    (when (string-prefix? "* " line-text)
      (let ((fname (substring line-text 2 (string-length line-text))))
        (hash-remove! *dired-marks* (string-trim fname))
        (send-message ed SCI_SETTARGETSTART line-start 0)
        (send-message ed SCI_SETTARGETEND line-end 0)
        (send-message/string ed SCI_REPLACETARGET
          (substring line-text 2 (string-length line-text)))))
    (send-message ed 2300 0 0)))

(def (cmd-dired-unmark-all app)
  "Unmark all marked files in dired."
  (set! *dired-marks* (make-hash-table))
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         ;; Remove all "* " prefixes
         (new-text (string-subst text "\n* " "\n")))
    (let ((new-text2 (if (string-prefix? "* " new-text)
                       (substring new-text 2 (string-length new-text))
                       new-text)))
      (editor-set-text ed new-text2)))
  (echo-message! (app-state-echo app) "All marks cleared"))

(def (cmd-dired-delete-marked app)
  "Delete all marked files in dired."
  (let* ((marked (hash-keys *dired-marks*))
         (count (length marked))
         (echo (app-state-echo app)))
    (if (= count 0)
      (echo-error! echo "No marked files")
      (let ((confirm (app-read-string app
                       (string-append "Delete " (number->string count)
                                      " file(s)? (yes/no): "))))
        (when (and confirm (string=? confirm "yes"))
          (let ((deleted 0))
            (for-each
              (lambda (f)
                (with-catch
                  (lambda (e) #f)
                  (lambda ()
                    (when (file-exists? f)
                      (delete-file f)
                      (set! deleted (+ deleted 1))))))
              marked)
            (set! *dired-marks* (make-hash-table))
            ;; Refresh dired buffer
            (let ((buf (current-buffer-from-app app)))
              (when (and buf (buffer-file-path buf))
                (cmd-dired-refresh app)))
            (echo-message! echo
              (string-append "Deleted " (number->string deleted) " file(s)"))))))))

(def (cmd-dired-refresh app)
  "Refresh the current dired buffer."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (dir (and buf (buffer-file-path buf))))
    (when dir
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Cannot read directory"))
        (lambda ()
          (let-values (((text _entries) (dired-format-listing dir)))
            (editor-set-read-only ed #f)
            (editor-set-text ed text)
            (editor-goto-pos ed 0)
            (editor-set-read-only ed #t)))))))


;;;============================================================================
;;; Diff commands
;;;============================================================================

(def (cmd-diff-two-files app)
  "Diff two files and show the result in a buffer."
  (let* ((echo (app-state-echo app))
         (file1 (app-read-string app "File A: "))
         (file2 (when file1 (app-read-string app "File B: "))))
    (when (and file1 file2
               (not (string-empty? file1)) (not (string-empty? file2)))
      (let ((result (with-catch
                      (lambda (e) (string-append "Error: "
                                    (with-output-to-string
                                      (lambda () (display-exception e)))))
                      (lambda ()
                        (let ((p (open-process
                                   (list path: "diff"
                                         arguments: (list "-u" file1 file2)
                                         stdout-redirection: #t
                                         stderr-redirection: #t))))
                          (let ((out (read-line p #f)))
                            (process-status p)
                            (or out "Files are identical")))))))
        (open-output-buffer app "*Diff*" result)))))

;;;============================================================================
;;; Buffer encoding commands
;;;============================================================================

(def (cmd-set-buffer-encoding app)
  "Set the buffer encoding (display only - all buffers use UTF-8)."
  (let* ((echo (app-state-echo app))
         (enc (app-read-string app "Encoding (utf-8/latin-1/ascii): ")))
    (when enc
      (echo-message! echo (string-append "Encoding set to: " enc
                                          " (note: internally UTF-8)")))))

(def (cmd-convert-line-endings app)
  "Convert line endings in current buffer (unix/dos/mac)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (choice (app-read-string app "Convert to (unix/dos/mac): ")))
    (when choice
      (let ((text (editor-get-text ed)))
        (cond
          ((string=? choice "unix")
           (let ((new-text (string-subst (string-subst text "\r\n" "\n") "\r" "\n")))
             (editor-set-text ed new-text)
             (echo-message! echo "Converted to Unix line endings (LF)")))
          ((string=? choice "dos")
           (let* ((clean (string-subst (string-subst text "\r\n" "\n") "\r" "\n"))
                  (new-text (string-subst clean "\n" "\r\n")))
             (editor-set-text ed new-text)
             (echo-message! echo "Converted to DOS line endings (CRLF)")))
          ((string=? choice "mac")
           (let ((new-text (string-subst (string-subst text "\r\n" "\r") "\n" "\r")))
             (editor-set-text ed new-text)
             (echo-message! echo "Converted to Mac line endings (CR)")))
          (else
           (echo-error! echo "Unknown format. Use unix, dos, or mac.")))))))

;;;============================================================================
;;; Word count / statistics
;;;============================================================================

(def (cmd-buffer-statistics app)
  "Show detailed buffer statistics: lines, words, chars, paragraphs."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (len (string-length text))
         (lines (+ 1 (let loop ((i 0) (count 0))
                       (if (>= i len) count
                         (if (char=? (string-ref text i) #\newline)
                           (loop (+ i 1) (+ count 1))
                           (loop (+ i 1) count))))))
         (words (let loop ((i 0) (count 0) (in-word #f))
                  (if (>= i len) (if in-word (+ count 1) count)
                    (let ((c (string-ref text i)))
                      (if (or (char=? c #\space) (char=? c #\newline)
                              (char=? c #\tab) (char=? c #\return))
                        (loop (+ i 1) (if in-word (+ count 1) count) #f)
                        (loop (+ i 1) count #t))))))
         (paragraphs (let loop ((i 0) (count 0) (prev-newline #f))
                       (if (>= i len) (+ count 1)
                         (let ((c (string-ref text i)))
                           (if (char=? c #\newline)
                             (loop (+ i 1) (if prev-newline (+ count 1) count) #t)
                             (loop (+ i 1) count #f))))))
         (non-blank (let loop ((i 0) (count 0))
                      (if (>= i len) count
                        (if (or (char=? (string-ref text i) #\space)
                                (char=? (string-ref text i) #\newline)
                                (char=? (string-ref text i) #\tab))
                          (loop (+ i 1) count)
                          (loop (+ i 1) (+ count 1)))))))
    (echo-message! (app-state-echo app)
      (string-append "Lines: " (number->string lines)
                     "  Words: " (number->string words)
                     "  Chars: " (number->string len)
                     "  Non-blank: " (number->string non-blank)
                     "  Paragraphs: " (number->string paragraphs)))))

;; ── batch 42: editing preferences and modes ─────────────────────────
(def *auto-fill-comments* #f)
(def *electric-indent-mode* #t)
(def *truncate-partial-width* #f)
(def *inhibit-startup-screen* #f)
(def *visible-cursor* #t)
(def *transient-mark-mode* #t)
(def *global-whitespace-mode* #f)
(def *hide-ifdef-mode* #f)
(def *allout-mode* #f)

(def (cmd-toggle-auto-fill-comments app)
  "Toggle auto-fill for comments only."
  (let ((echo (app-state-echo app)))
    (set! *auto-fill-comments* (not *auto-fill-comments*))
    (echo-message! echo (if *auto-fill-comments*
                          "Auto-fill comments ON" "Auto-fill comments OFF"))))

(def (cmd-toggle-electric-indent-mode app)
  "Toggle electric-indent-mode (auto indent on newline)."
  (let ((echo (app-state-echo app)))
    (set! *electric-indent-mode* (not *electric-indent-mode*))
    (echo-message! echo (if *electric-indent-mode*
                          "Electric indent mode ON" "Electric indent mode OFF"))))

(def (cmd-toggle-truncate-partial-width-windows app)
  "Toggle truncation in partial-width windows."
  (let ((echo (app-state-echo app)))
    (set! *truncate-partial-width* (not *truncate-partial-width*))
    (echo-message! echo (if *truncate-partial-width*
                          "Truncate partial-width ON" "Truncate partial-width OFF"))))

(def (cmd-toggle-inhibit-startup-screen app)
  "Toggle inhibit-startup-screen."
  (let ((echo (app-state-echo app)))
    (set! *inhibit-startup-screen* (not *inhibit-startup-screen*))
    (echo-message! echo (if *inhibit-startup-screen*
                          "Inhibit startup screen ON" "Inhibit startup screen OFF"))))

(def (cmd-toggle-visible-cursor app)
  "Toggle visible cursor in non-selected windows."
  (let ((echo (app-state-echo app)))
    (set! *visible-cursor* (not *visible-cursor*))
    (echo-message! echo (if *visible-cursor*
                          "Visible cursor ON" "Visible cursor OFF"))))

(def (cmd-toggle-transient-mark-mode app)
  "Toggle transient-mark-mode (highlight active region)."
  (let ((echo (app-state-echo app)))
    (set! *transient-mark-mode* (not *transient-mark-mode*))
    (echo-message! echo (if *transient-mark-mode*
                          "Transient mark mode ON" "Transient mark mode OFF"))))

(def (cmd-insert-form-feed app)
  "Insert a form-feed character (^L page break)."
  (let ((ed (current-editor app)))
    (editor-replace-selection ed (string (integer->char 12)))))

(def (cmd-toggle-global-whitespace-mode app)
  "Toggle global-whitespace-mode (show all whitespace)."
  (let ((echo (app-state-echo app)))
    (set! *global-whitespace-mode* (not *global-whitespace-mode*))
    (echo-message! echo (if *global-whitespace-mode*
                          "Global whitespace mode ON" "Global whitespace mode OFF"))))

(def (cmd-toggle-hide-ifdef-mode app)
  "Toggle hide-ifdef-mode (hide #ifdef blocks)."
  (let ((echo (app-state-echo app)))
    (set! *hide-ifdef-mode* (not *hide-ifdef-mode*))
    (echo-message! echo (if *hide-ifdef-mode*
                          "Hide-ifdef mode ON" "Hide-ifdef mode OFF"))))

(def (cmd-toggle-allout-mode app)
  "Toggle allout-mode (outline editing)."
  (let ((echo (app-state-echo app)))
    (set! *allout-mode* (not *allout-mode*))
    (echo-message! echo (if *allout-mode*
                          "Allout mode ON" "Allout mode OFF"))))

;; ── batch 49: global minor mode toggles ─────────────────────────────
(def *indent-guide-global* #f)
(def *rainbow-delimiters-global* #f)
(def *global-display-fill-column* #f)
(def *global-flycheck* #f)
(def *global-company* #f)
(def *global-diff-hl* #f)
(def *global-git-gutter* #f)
(def *global-page-break-lines* #f)
(def *global-anzu* #f)

(def (cmd-toggle-indent-guide-global app)
  "Toggle global indent guides display."
  (let ((echo (app-state-echo app)))
    (set! *indent-guide-global* (not *indent-guide-global*))
    (echo-message! echo (if *indent-guide-global*
                          "Indent guide global ON" "Indent guide global OFF"))))

(def (cmd-toggle-rainbow-delimiters-global app)
  "Toggle global rainbow-delimiters-mode."
  (let ((echo (app-state-echo app)))
    (set! *rainbow-delimiters-global* (not *rainbow-delimiters-global*))
    (echo-message! echo (if *rainbow-delimiters-global*
                          "Rainbow delimiters ON" "Rainbow delimiters OFF"))))

(def (cmd-toggle-global-display-fill-column app)
  "Toggle global display of fill column indicator."
  (let ((echo (app-state-echo app)))
    (set! *global-display-fill-column* (not *global-display-fill-column*))
    (echo-message! echo (if *global-display-fill-column*
                          "Fill column indicator ON" "Fill column indicator OFF"))))

(def (cmd-toggle-global-flycheck app)
  "Toggle global flycheck-mode (on-the-fly syntax checking)."
  (let ((echo (app-state-echo app)))
    (set! *global-flycheck* (not *global-flycheck*))
    (echo-message! echo (if *global-flycheck*
                          "Global flycheck ON" "Global flycheck OFF"))))

(def (cmd-toggle-global-company app)
  "Toggle global company-mode (completion)."
  (let ((echo (app-state-echo app)))
    (set! *global-company* (not *global-company*))
    (echo-message! echo (if *global-company*
                          "Global company ON" "Global company OFF"))))

(def (cmd-toggle-global-diff-hl app)
  "Toggle global diff-hl-mode (VCS diff in fringe)."
  (let ((echo (app-state-echo app)))
    (set! *global-diff-hl* (not *global-diff-hl*))
    (echo-message! echo (if *global-diff-hl*
                          "Global diff-hl ON" "Global diff-hl OFF"))))

(def (cmd-toggle-global-git-gutter app)
  "Toggle global git-gutter-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-git-gutter* (not *global-git-gutter*))
    (echo-message! echo (if *global-git-gutter*
                          "Global git-gutter ON" "Global git-gutter OFF"))))

(def (cmd-toggle-global-page-break-lines app)
  "Toggle global page-break-lines-mode (display ^L as lines)."
  (let ((echo (app-state-echo app)))
    (set! *global-page-break-lines* (not *global-page-break-lines*))
    (echo-message! echo (if *global-page-break-lines*
                          "Page break lines ON" "Page break lines OFF"))))

(def (cmd-toggle-global-anzu app)
  "Toggle global anzu-mode (show search match count)."
  (let ((echo (app-state-echo app)))
    (set! *global-anzu* (not *global-anzu*))
    (echo-message! echo (if *global-anzu*
                          "Global anzu ON" "Global anzu OFF"))))

;; ── batch 54: navigation and editing enhancement toggles ────────────
(def *global-visual-regexp* #f)
(def *global-move-dup* #f)
(def *global-expand-region* #f)
(def *global-multiple-cursors* #f)
(def *global-undo-propose* #f)
(def *global-goto-chg* #f)
(def *global-avy* #f)

(def (cmd-toggle-global-visual-regexp app)
  "Toggle global visual-regexp-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-visual-regexp* (not *global-visual-regexp*))
    (echo-message! echo (if *global-visual-regexp*
                          "Visual regexp ON" "Visual regexp OFF"))))

(def (cmd-toggle-global-move-dup app)
  "Toggle global move-dup-mode (move/duplicate lines)."
  (let ((echo (app-state-echo app)))
    (set! *global-move-dup* (not *global-move-dup*))
    (echo-message! echo (if *global-move-dup*
                          "Move-dup ON" "Move-dup OFF"))))

(def (cmd-toggle-global-expand-region app)
  "Toggle global expand-region integration."
  (let ((echo (app-state-echo app)))
    (set! *global-expand-region* (not *global-expand-region*))
    (echo-message! echo (if *global-expand-region*
                          "Expand-region ON" "Expand-region OFF"))))

(def (cmd-toggle-global-multiple-cursors app)
  "Toggle global multiple-cursors-mode."
  (let ((echo (app-state-echo app)))
    (set! *global-multiple-cursors* (not *global-multiple-cursors*))
    (echo-message! echo (if *global-multiple-cursors*
                          "Multiple cursors ON" "Multiple cursors OFF"))))

(def (cmd-toggle-global-undo-propose app)
  "Toggle global undo-propose-mode (preview undo)."
  (let ((echo (app-state-echo app)))
    (set! *global-undo-propose* (not *global-undo-propose*))
    (echo-message! echo (if *global-undo-propose*
                          "Undo propose ON" "Undo propose OFF"))))

(def (cmd-toggle-global-goto-chg app)
  "Toggle global goto-chg-mode (navigate edit points)."
  (let ((echo (app-state-echo app)))
    (set! *global-goto-chg* (not *global-goto-chg*))
    (echo-message! echo (if *global-goto-chg*
                          "Goto-chg ON" "Goto-chg OFF"))))

(def (cmd-toggle-global-avy app)
  "Toggle global avy-mode (jump to visible text)."
  (let ((echo (app-state-echo app)))
    (set! *global-avy* (not *global-avy*))
    (echo-message! echo (if *global-avy*
                          "Global avy ON" "Global avy OFF"))))

;;; ---- batch 63: fun and entertainment toggles ----

(def *global-nyan-cat* #f)
(def *global-parrot* #f)
(def *global-zone* #f)
(def *global-fireplace* #f)
(def *global-snow* #f)
(def *global-power-mode* #f)
(def *global-animate-typing* #f)

(def (cmd-toggle-global-nyan-cat app)
  "Toggle global nyan-cat-mode (Nyan Cat in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-nyan-cat* (not *global-nyan-cat*))
    (echo-message! echo (if *global-nyan-cat*
                          "Nyan cat ON" "Nyan cat OFF"))))

(def (cmd-toggle-global-parrot app)
  "Toggle global parrot-mode (party parrot in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *global-parrot* (not *global-parrot*))
    (echo-message! echo (if *global-parrot*
                          "Party parrot ON" "Party parrot OFF"))))

(def (cmd-toggle-global-zone app)
  "Toggle global zone-mode (screensaver when idle)."
  (let ((echo (app-state-echo app)))
    (set! *global-zone* (not *global-zone*))
    (echo-message! echo (if *global-zone*
                          "Zone mode ON" "Zone mode OFF"))))

(def (cmd-toggle-global-fireplace app)
  "Toggle global fireplace-mode (cozy fireplace animation)."
  (let ((echo (app-state-echo app)))
    (set! *global-fireplace* (not *global-fireplace*))
    (echo-message! echo (if *global-fireplace*
                          "Fireplace ON" "Fireplace OFF"))))

(def (cmd-toggle-global-snow app)
  "Toggle global snow-mode (let it snow animation)."
  (let ((echo (app-state-echo app)))
    (set! *global-snow* (not *global-snow*))
    (echo-message! echo (if *global-snow*
                          "Snow ON" "Snow OFF"))))

(def (cmd-toggle-global-power-mode app)
  "Toggle global power-mode (screen shake and particles on typing)."
  (let ((echo (app-state-echo app)))
    (set! *global-power-mode* (not *global-power-mode*))
    (echo-message! echo (if *global-power-mode*
                          "Power mode ON" "Power mode OFF"))))

(def (cmd-toggle-global-animate-typing app)
  "Toggle global animate-typing-mode (typing animation effect)."
  (let ((echo (app-state-echo app)))
    (set! *global-animate-typing* (not *global-animate-typing*))
    (echo-message! echo (if *global-animate-typing*
                          "Animate typing ON" "Animate typing OFF"))))

;;; ---- batch 72: data science and environment management toggles ----

(def *global-r-mode* #f)
(def *global-ess* #f)
(def *global-sql-mode* #f)
(def *global-ein* #f)
(def *global-conda* #f)
(def *global-pyvenv* #f)
(def *global-pipenv* #f)

(def (cmd-toggle-global-r-mode app)
  "Toggle global R-mode (R statistics language)."
  (let ((echo (app-state-echo app)))
    (set! *global-r-mode* (not *global-r-mode*))
    (echo-message! echo (if *global-r-mode*
                          "R mode ON" "R mode OFF"))))

(def (cmd-toggle-global-ess app)
  "Toggle global ESS-mode (Emacs Speaks Statistics)."
  (let ((echo (app-state-echo app)))
    (set! *global-ess* (not *global-ess*))
    (echo-message! echo (if *global-ess*
                          "ESS ON" "ESS OFF"))))

(def (cmd-toggle-global-sql-mode app)
  "Toggle global sql-mode (SQL query editing and execution)."
  (let ((echo (app-state-echo app)))
    (set! *global-sql-mode* (not *global-sql-mode*))
    (echo-message! echo (if *global-sql-mode*
                          "SQL mode ON" "SQL mode OFF"))))

(def (cmd-toggle-global-ein app)
  "Toggle global EIN-mode (Jupyter notebook in Emacs)."
  (let ((echo (app-state-echo app)))
    (set! *global-ein* (not *global-ein*))
    (echo-message! echo (if *global-ein*
                          "EIN ON" "EIN OFF"))))

(def (cmd-toggle-global-conda app)
  "Toggle global conda-mode (Conda environment management)."
  (let ((echo (app-state-echo app)))
    (set! *global-conda* (not *global-conda*))
    (echo-message! echo (if *global-conda*
                          "Conda ON" "Conda OFF"))))

(def (cmd-toggle-global-pyvenv app)
  "Toggle global pyvenv-mode (Python virtualenv management)."
  (let ((echo (app-state-echo app)))
    (set! *global-pyvenv* (not *global-pyvenv*))
    (echo-message! echo (if *global-pyvenv*
                          "Pyvenv ON" "Pyvenv OFF"))))

(def (cmd-toggle-global-pipenv app)
  "Toggle global pipenv-mode (Pipenv environment management)."
  (let ((echo (app-state-echo app)))
    (set! *global-pipenv* (not *global-pipenv*))
    (echo-message! echo (if *global-pipenv*
                          "Pipenv ON" "Pipenv OFF"))))

;;;============================================================================
;;; Comment-dwim (M-;) — Do What I Mean with comments
;;;============================================================================

(def (cmd-comment-dwim app)
  "Do What I Mean with comments. Region active: toggle. Blank line: insert comment. Otherwise: toggle current line."
  (let* ((ed (current-editor app))
         (buf (current-buffer-from-app app))
         (text (editor-get-text ed))
         (mark (buffer-mark buf)))
    (if mark
      ;; Region active: toggle comment on region lines
      (let* ((pos (editor-get-current-pos ed))
             (start (min pos mark))
             (end (max pos mark))
             (start-line (editor-line-from-position ed start))
             (end-line (editor-line-from-position ed end)))
        (with-undo-action ed
          (let loop ((l end-line))
            (when (>= l start-line)
              (let* ((ls (editor-position-from-line ed l))
                     (le (editor-get-line-end-position ed l))
                     (lt (substring text ls le))
                     (trimmed (string-trim lt)))
                (if (string-prefix? ";;" trimmed)
                  ;; Uncomment
                  (let ((off (string-contains lt ";;")))
                    (when off
                      (let ((del-len (if (and (< (+ off 2) (string-length lt))
                                              (char=? (string-ref lt (+ off 2)) #\space))
                                       3 2)))
                        (editor-delete-range ed (+ ls off) del-len))))
                  ;; Comment
                  (editor-insert-text ed ls ";; ")))
              (loop (- l 1)))))
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app)
          (string-append "Toggled " (number->string (+ 1 (- end-line start-line))) " lines")))
      ;; No region: check current line
      (let* ((pos (editor-get-current-pos ed))
             (line (editor-line-from-position ed pos))
             (ls (editor-position-from-line ed line))
             (le (editor-get-line-end-position ed line))
             (line-text (substring text ls le))
             (trimmed (string-trim line-text)))
        (cond
          ;; Blank line: insert comment
          ((string=? trimmed "")
           (with-undo-action ed
             (editor-insert-text ed ls ";; "))
           (editor-goto-pos ed (+ ls 3)))
          ;; Already commented: uncomment
          ((string-prefix? ";;" trimmed)
           (let ((off (string-contains line-text ";;")))
             (when off
               (let ((del-len (if (and (< (+ off 2) (string-length line-text))
                                       (char=? (string-ref line-text (+ off 2)) #\space))
                                3 2)))
                 (with-undo-action ed
                   (editor-delete-range ed (+ ls off) del-len))))))
          ;; Not commented: add comment prefix
          (else
           (with-undo-action ed
             (editor-insert-text ed ls ";; "))))))))

;;;============================================================================
;;; Kill sentence / paragraph / subword
;;;============================================================================

(def (tui-sentence-end-pos text pos)
  "Find end of current sentence from pos."
  (let ((len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len) len)
        ((memv (string-ref text i) '(#\. #\? #\!))
         (+ i 1))
        (else (loop (+ i 1)))))))

(def (tui-sentence-start-pos text pos)
  "Find start of current sentence from pos."
  (let loop ((i (- pos 1)))
    (cond
      ((<= i 0) 0)
      ((memv (string-ref text i) '(#\. #\? #\!))
       (let skip-ws ((j (+ i 1)))
         (if (and (< j pos) (char-whitespace? (string-ref text j)))
           (skip-ws (+ j 1))
           j)))
      (else (loop (- i 1))))))

(def (cmd-kill-sentence app)
  "Kill from point to end of sentence."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (end (tui-sentence-end-pos text pos))
         (killed (substring text pos end)))
    (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
    (with-undo-action ed (editor-delete-range ed pos (- end pos)))))

(def (cmd-backward-kill-sentence app)
  "Kill from point back to start of sentence."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (start (tui-sentence-start-pos text pos))
         (killed (substring text start pos)))
    (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
    (with-undo-action ed (editor-delete-range ed start (- pos start)))))

(def (cmd-kill-paragraph app)
  "Kill from point to end of paragraph."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let loop ((i pos) (saw-text? #f))
      (let ((end (cond
                   ((>= i len) len)
                   ((char=? (string-ref text i) #\newline)
                    (if (and saw-text?
                             (or (>= (+ i 1) len)
                                 (char=? (string-ref text (+ i 1)) #\newline)))
                      (+ i 1) #f))
                   (else #f))))
        (if end
          (let ((killed (substring text pos end)))
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (with-undo-action ed (editor-delete-range ed pos (- end pos))))
          (loop (+ i 1) (or saw-text? (not (char=? (string-ref text i) #\newline)))))))))

(def (cmd-kill-subword app)
  "Kill forward to the next subword boundary (camelCase, snake_case)."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let loop ((i (+ pos 1)))
      (let ((at-boundary?
             (or (>= i len)
                 (memv (string-ref text i) '(#\_ #\- #\space #\tab #\newline))
                 (and (> i 0)
                      (char-lower-case? (string-ref text (- i 1)))
                      (char-upper-case? (string-ref text i))))))
        (if at-boundary?
          (let* ((end (min i len))
                 (killed (substring text pos end)))
            (set! (app-state-kill-ring app) (cons killed (app-state-kill-ring app)))
            (with-undo-action ed (editor-delete-range ed pos (- end pos))))
          (loop (+ i 1)))))))

;;;============================================================================
;;; S-expression list navigation: up-list, down-list
;;;============================================================================

(def (cmd-up-list app)
  "Move backward out of one level of parentheses."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed)))
    (let loop ((i (- pos 1)) (depth 0))
      (cond
        ((< i 0)
         (echo-message! (app-state-echo app) "At top level"))
        ((memv (string-ref text i) '(#\) #\] #\}))
         (loop (- i 1) (+ depth 1)))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (if (= depth 0)
           (begin (editor-goto-pos ed i) (editor-scroll-caret ed))
           (loop (- i 1) (- depth 1))))
        (else (loop (- i 1) depth))))))

(def (cmd-down-list app)
  "Move forward into one level of parentheses."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (pos (editor-get-current-pos ed))
         (len (string-length text)))
    (let loop ((i pos))
      (cond
        ((>= i len)
         (echo-message! (app-state-echo app) "No inner list found"))
        ((memv (string-ref text i) '(#\( #\[ #\{))
         (editor-goto-pos ed (+ i 1))
         (editor-scroll-caret ed))
        (else (loop (+ i 1)))))))

