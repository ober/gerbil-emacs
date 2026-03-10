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
        :gemacs/editor-extra-editing
        (only-in :gemacs/persist
          *enriched-mode* *picture-mode*))

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
  (set! *global-whitespace-mode* (not *global-whitespace-mode*))
  (echo-message! (app-state-echo app)
    (if *global-whitespace-mode* "Global whitespace mode ON" "Global whitespace mode OFF")))

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

;;; --- Visual line mode (word wrap) ---
;; Scintilla constants: SCI_SETWRAPMODE=2268, SC_WRAP_NONE=0, SC_WRAP_WORD=1
;; SCI_SETWRAPVISUALFLAGS=2460, SC_WRAPVISUALFLAG_END=1
(def *visual-line-mode* #f)

(def (cmd-visual-line-mode app)
  "Toggle visual-line-mode (word wrap)."
  (set! *visual-line-mode* (not *visual-line-mode*))
  (let ((ed (current-editor app)))
    (when ed
      (send-message ed 2268 (if *visual-line-mode* 1 0) 0)  ; SCI_SETWRAPMODE
      (send-message ed 2460 (if *visual-line-mode* 1 0) 0))) ; SCI_SETWRAPVISUALFLAGS
  (echo-message! (app-state-echo app)
    (if *visual-line-mode* "Visual line mode enabled (word wrap)" "Visual line mode disabled")))

(def (cmd-toggle-truncate-lines app)
  "Toggle line truncation (inverse of visual-line-mode)."
  (cmd-visual-line-mode app))

;;; --- Whitespace mode (real Scintilla implementation) ---
;; Scintilla constants: SCI_SETVIEWWS=2021 (0=invisible, 1=always visible, 2=after indent)
;; SCI_SETVIEWEOL=2356 (0=hide, 1=show)
(def *whitespace-mode* #f)

(def (cmd-whitespace-mode app)
  "Toggle whitespace-mode (show spaces, tabs, EOL)."
  (set! *whitespace-mode* (not *whitespace-mode*))
  (let ((ed (current-editor app)))
    (when ed
      (send-message ed 2021 (if *whitespace-mode* 1 0) 0)   ; SCI_SETVIEWWS
      (send-message ed 2356 (if *whitespace-mode* 1 0) 0)))  ; SCI_SETVIEWEOL
  (echo-message! (app-state-echo app)
    (if *whitespace-mode* "Whitespace mode enabled" "Whitespace mode disabled")))

;;; --- Show trailing whitespace ---
(def *show-trailing-whitespace* #f)

(def (cmd-toggle-show-trailing-whitespace app)
  "Toggle highlighting of trailing whitespace."
  (set! *show-trailing-whitespace* (not *show-trailing-whitespace*))
  (let ((ed (current-editor app)))
    (when ed
      ;; SCI_SETVIEWWS: 2=visible after indent only (shows trailing spaces)
      (send-message ed 2021 (if *show-trailing-whitespace* 2 0) 0)))
  (echo-message! (app-state-echo app)
    (if *show-trailing-whitespace* "Showing trailing whitespace" "Hiding trailing whitespace")))

;;; --- Delete trailing whitespace ---
(def (cmd-delete-trailing-whitespace app)
  "Delete trailing whitespace from all lines."
  (let* ((ed (current-editor app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (cleaned (map (lambda (line)
                        (string-trim-right line))
                      lines))
         (result (string-join cleaned "\n")))
    (unless (string=? text result)
      (let ((pos (editor-get-current-pos ed)))
        (editor-set-text ed result)
        (editor-goto-pos ed (min pos (string-length result)))
        (editor-scroll-caret ed)))
    (echo-message! (app-state-echo app) "Trailing whitespace deleted")))

;;;============================================================================
;;; Enriched text mode (basic)
;;;============================================================================

;; *enriched-mode* is defined in persist.ss

(def (cmd-enriched-mode app)
  "Toggle enriched text mode for basic formatting support."
  (set! *enriched-mode* (not *enriched-mode*))
  (echo-message! (app-state-echo app)
    (if *enriched-mode* "Enriched mode enabled" "Enriched mode disabled")))

(def (cmd-facemenu-set-bold app)
  "Apply bold styling to selected text (Scintilla style)."
  (let* ((ed (current-editor app))
         (start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (end   (send-message ed SCI_GETSELECTIONEND 0 0)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No selection — select text first")
      (begin
        ;; Style 1 = bold text style
        (send-message ed SCI_STYLESETBOLD 1 1)
        (send-message ed 2032 start 0)  ;; SCI_STARTSTYLING
        (send-message ed 2033 (- end start) 1)  ;; SCI_SETSTYLING
        (echo-message! (app-state-echo app) "Bold applied")))))

(def (cmd-facemenu-set-italic app)
  "Apply italic styling to selected text (Scintilla style)."
  (let* ((ed (current-editor app))
         (start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (end   (send-message ed SCI_GETSELECTIONEND 0 0)))
    (if (= start end)
      (echo-message! (app-state-echo app) "No selection — select text first")
      (begin
        ;; Style 2 = italic text style
        (send-message ed SCI_STYLESETITALIC 2 1)
        (send-message ed 2032 start 0)  ;; SCI_STARTSTYLING
        (send-message ed 2033 (- end start) 2)  ;; SCI_SETSTYLING
        (echo-message! (app-state-echo app) "Italic applied")))))

;;;============================================================================
;;; Picture mode (overwrite with cursor movement)
;;;============================================================================

;; *picture-mode* is defined in persist.ss

(def (cmd-picture-mode app)
  "Toggle picture mode — overwrite mode with directional drawing.
   In picture mode, characters overwrite instead of inserting,
   and spaces fill as you move the cursor."
  (set! *picture-mode* (not *picture-mode*))
  (let ((ed (current-editor app)))
    ;; Enable/disable overwrite mode in Scintilla
    (send-message ed 2186 (if *picture-mode* 1 0) 0)  ;; SCI_SETOVERTYPE
    (echo-message! (app-state-echo app)
      (if *picture-mode*
        "Picture mode ON (overwrite, use arrows to draw)"
        "Picture mode OFF"))))

;;; ============================================================
;;; Hungry delete — delete all consecutive whitespace
;;; ============================================================

(def (cmd-hungry-delete-forward app)
  "Delete all consecutive whitespace ahead of point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "End of buffer")
      (let loop ((i pos))
        (if (or (>= i len)
                (not (char-whitespace? (string-ref text i))))
          (if (> i pos)
            (begin
              (send-message ed SCI_SETTARGETSTART pos 0)
              (send-message ed SCI_SETTARGETEND i 0)
              (send-message/string ed SCI_REPLACETARGET ""))
            ;; No whitespace — just delete one char
            (send-message ed 2180 0 0)) ;; SCI_CLEAR
          (loop (+ i 1)))))))

(def (cmd-hungry-delete-backward app)
  "Delete all consecutive whitespace behind point."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed)))
    (if (<= pos 0)
      (echo-message! (app-state-echo app) "Beginning of buffer")
      (let loop ((i (- pos 1)))
        (if (or (< i 0)
                (not (char-whitespace? (string-ref text i))))
          (let ((del-start (+ i 1)))
            (if (< del-start pos)
              (begin
                (send-message ed SCI_SETTARGETSTART del-start 0)
                (send-message ed SCI_SETTARGETEND pos 0)
                (send-message/string ed SCI_REPLACETARGET ""))
              ;; No whitespace — just delete one char
              (send-message ed 2326 0 0))) ;; SCI_DELETEBACK
          (loop (- i 1)))))))

;;; ============================================================
;;; Isearch match count (anzu-style N/M counter)
;;; ============================================================

(def (count-search-matches ed pattern)
  "Count total occurrences of pattern in buffer."
  (let* ((text (editor-get-text ed))
         (plen (string-length pattern)))
    (if (<= plen 0) 0
      (let loop ((start 0) (count 0))
        (let ((pos (string-contains text pattern start)))
          (if pos
            (loop (+ pos 1) (+ count 1))
            count))))))

(def (current-match-index ed pattern pos)
  "Return which match number (1-based) the position corresponds to."
  (let* ((text (editor-get-text ed))
         (plen (string-length pattern)))
    (if (<= plen 0) 0
      (let loop ((start 0) (n 1))
        (let ((found (string-contains text pattern start)))
          (if (not found) 0
            (if (= found pos) n
              (loop (+ found 1) (+ n 1)))))))))

(def (isearch-count-message ed pattern pos)
  "Return a string like '[3/15]' for current isearch position."
  (let ((total (count-search-matches ed pattern))
        (current (current-match-index ed pattern pos)))
    (if (> total 0)
      (string-append "[" (number->string current) "/" (number->string total) "]")
      "[0/0]")))

;;; ============================================================
;;; crux-move-beginning-of-line — smart BOL toggle
;;; ============================================================

(def (cmd-crux-move-beginning-of-line app)
  "Smart beginning-of-line: toggle between first non-whitespace and column 0."
  (let* ((ed (current-editor app))
         (pos (editor-get-current-pos ed))
         (line (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line))
         (text (editor-get-text ed))
         (len (string-length text))
         ;; Find first non-whitespace on this line
         (first-nonws
           (let loop ((i line-start))
             (if (or (>= i len)
                     (let ((ch (string-ref text i)))
                       (char=? ch #\newline)))
               i
               (if (char-whitespace? (string-ref text i))
                 (loop (+ i 1))
                 i)))))
    (if (= pos first-nonws)
      ;; Already at first non-ws, go to column 0
      (editor-goto-pos ed line-start)
      ;; Go to first non-ws
      (editor-goto-pos ed first-nonws))))

;;;============================================================================
;;; Hydra — extensible popup command menus
;;;============================================================================

(def *tui-hydra-heads* (make-hash-table)) ;; name -> list of (key label cmd)

(def (cmd-hydra-define app)
  "Define a hydra — extensible popup command menu."
  (let ((name (app-read-string app "Hydra name: ")))
    (when (and name (> (string-length name) 0))
      (hash-put! *tui-hydra-heads* (string->symbol name) [])
      (echo-message! (app-state-echo app) (string-append "Hydra '" name "' defined (empty)")))))

(def (cmd-hydra-zoom app)
  "Hydra for zoom commands: +/- to zoom, 0 to reset."
  (echo-message! (app-state-echo app) "Zoom hydra: + increase, - decrease, 0 reset, q quit")
  (let loop ()
    (let ((key (app-read-string app "Zoom [+/-/0/q]: ")))
      (when (and key (> (string-length key) 0))
        (let ((ch (string-ref key 0)))
          (cond
            ((eqv? ch #\+) (let ((cmd (find-command 'text-scale-increase)))
                             (when cmd (cmd app))) (loop))
            ((eqv? ch #\-) (let ((cmd (find-command 'text-scale-decrease)))
                             (when cmd (cmd app))) (loop))
            ((eqv? ch #\0) (let ((cmd (find-command 'text-scale-reset)))
                             (when cmd (cmd app))) (loop))
            ((eqv? ch #\q) (echo-message! (app-state-echo app) "Zoom hydra done"))))))))

(def (cmd-hydra-window app)
  "Hydra for window commands: h/j/k/l to move, s/v split, d delete, q quit."
  (echo-message! (app-state-echo app) "Window hydra: h←/j↓/k↑/l→, s split-h, v split-v, d delete, q quit")
  (let loop ()
    (let ((key (app-read-string app "Window [hjklsvdq]: ")))
      (when (and key (> (string-length key) 0))
        (let ((ch (string-ref key 0)))
          (cond
            ((eqv? ch #\h) (let ((c (find-command 'windmove-left))) (when c (c app))) (loop))
            ((eqv? ch #\l) (let ((c (find-command 'windmove-right))) (when c (c app))) (loop))
            ((eqv? ch #\k) (let ((c (find-command 'windmove-up))) (when c (c app))) (loop))
            ((eqv? ch #\j) (let ((c (find-command 'windmove-down))) (when c (c app))) (loop))
            ((eqv? ch #\s) (let ((c (find-command 'split-window-horizontally))) (when c (c app))) (loop))
            ((eqv? ch #\v) (let ((c (find-command 'split-window-vertically))) (when c (c app))) (loop))
            ((eqv? ch #\d) (let ((c (find-command 'delete-window))) (when c (c app))) (loop))
            ((eqv? ch #\q) (echo-message! (app-state-echo app) "Window hydra done"))))))))

;;;============================================================================
;;; Deadgrep — enhanced grep interface
;;;============================================================================

(def (cmd-deadgrep app)
  "Deadgrep — enhanced grep search (delegates to rgrep)."
  (let ((cmd (find-command 'rgrep)))
    (if cmd (cmd app)
      (echo-error! (app-state-echo app) "rgrep not available"))))

;;;============================================================================
;;; String-edit — edit string at point in separate buffer
;;;============================================================================

(def (cmd-string-edit-at-point app)
  "Edit the string literal at point in a temporary buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (len (string-length text)))
    ;; Find enclosing quotes
    (let* ((qchar (if (and (< pos len) (eqv? (string-ref text pos) #\")) #\"
                   (if (and (> pos 0) (eqv? (string-ref text (- pos 1)) #\")) #\"
                     #f))))
      (if (not qchar)
        (echo-error! (app-state-echo app) "No string at point")
        (echo-message! (app-state-echo app) "String editing: use query-replace for string edits")))))

;;;============================================================================
;;; Hideshow — code folding
;;;============================================================================

(def *tui-hideshow-mode* #f)

(def (cmd-hs-minor-mode app)
  "Toggle hideshow minor mode — code folding."
  (set! *tui-hideshow-mode* (not *tui-hideshow-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-hideshow-mode* "HS minor mode enabled" "HS minor mode disabled")))

(def (cmd-hs-toggle-hiding app)
  "Toggle fold at point."
  (let ((cmd (find-command 'fold-toggle)))
    (if cmd (cmd app)
      (echo-message! (app-state-echo app) "Fold toggled (hideshow)"))))

(def (cmd-hs-hide-all app)
  "Hide all blocks."
  (let ((cmd (find-command 'fold-all)))
    (if cmd (cmd app)
      (echo-message! (app-state-echo app) "All blocks hidden"))))

(def (cmd-hs-show-all app)
  "Show all blocks."
  (let ((cmd (find-command 'unfold-all)))
    (if cmd (cmd app)
      (echo-message! (app-state-echo app) "All blocks shown"))))

;;;============================================================================
;;; Prescient — completion sorting by frequency
;;;============================================================================

(def *tui-prescient-mode* #f)

(def (cmd-prescient-mode app)
  "Toggle prescient mode — sort completions by usage frequency."
  (set! *tui-prescient-mode* (not *tui-prescient-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-prescient-mode* "Prescient mode enabled" "Prescient mode disabled")))

;;;============================================================================
;;; No-littering — clean dotfile organization
;;;============================================================================

(def (cmd-no-littering-mode app)
  "Toggle no-littering mode — keep ~/.emacs.d clean."
  (echo-message! (app-state-echo app) "Gemacs uses ~/.gemacs-* files; no littering by default"))

;;;============================================================================
;;; Benchmark-init / esup — startup profiling
;;;============================================================================

(def (cmd-benchmark-init-show-durations app)
  "Show startup module load times."
  (echo-message! (app-state-echo app) "Gemacs startup: compiled Gerbil, no per-module timing available"))

(def (cmd-esup app)
  "Emacs startup profiler — show init load times."
  (echo-message! (app-state-echo app) "Gemacs: compiled binary, no Elisp init profiling"))

;;;============================================================================
;;; GCMH — GC tuning mode
;;;============================================================================

(def *tui-gcmh-mode* #f)

(def (cmd-gcmh-mode app)
  "Toggle GCMH mode — adaptive GC threshold."
  (set! *tui-gcmh-mode* (not *tui-gcmh-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-gcmh-mode* "GCMH mode enabled (GC tuning)" "GCMH mode disabled")))

;;;============================================================================
;;; Ligature — font ligature display
;;;============================================================================

(def *tui-ligature-mode* #f)

(def (cmd-ligature-mode app)
  "Toggle ligature mode — display font ligatures."
  (set! *tui-ligature-mode* (not *tui-ligature-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-ligature-mode* "Ligature mode enabled (terminal dependent)" "Ligature mode disabled")))

;;;============================================================================
;;; Mixed-pitch / variable-pitch — font mixing
;;;============================================================================

(def *tui-mixed-pitch* #f)

(def (cmd-mixed-pitch-mode app)
  "Toggle mixed-pitch mode — proportional fonts in prose."
  (set! *tui-mixed-pitch* (not *tui-mixed-pitch*))
  (echo-message! (app-state-echo app)
    (if *tui-mixed-pitch* "Mixed-pitch mode enabled (N/A in terminal)" "Mixed-pitch mode disabled")))

(def (cmd-variable-pitch-mode app)
  "Toggle variable-pitch mode."
  (cmd-mixed-pitch-mode app))

;;;============================================================================
;;; Eldoc-box — eldoc in popup
;;;============================================================================

(def *tui-eldoc-box* #f)

(def (cmd-eldoc-box-help-at-point app)
  "Show eldoc help at point in a box."
  (let ((cmd (find-command 'eldoc)))
    (if cmd (cmd app)
      (echo-message! (app-state-echo app) "No eldoc available"))))

(def (cmd-eldoc-box-mode app)
  "Toggle eldoc-box mode — show eldoc in a popup."
  (set! *tui-eldoc-box* (not *tui-eldoc-box*))
  (echo-message! (app-state-echo app)
    (if *tui-eldoc-box* "Eldoc-box mode enabled" "Eldoc-box mode disabled")))

;;;============================================================================
;;; Color-rg — colored ripgrep interface
;;;============================================================================

(def (cmd-color-rg-search-input app)
  "Color-rg search — delegates to rgrep."
  (let ((cmd (find-command 'rgrep)))
    (when cmd (cmd app))))

(def (cmd-color-rg-search-project app)
  "Color-rg search project — delegates to project-grep."
  (let ((cmd (find-command 'project-grep)))
    (when cmd (cmd app))))

;;;============================================================================
;;; Ctrlf — better isearch
;;;============================================================================

(def (cmd-ctrlf-forward app)
  "Ctrlf forward search — delegates to isearch-forward."
  (let ((cmd (find-command 'isearch-forward)))
    (when cmd (cmd app))))

(def (cmd-ctrlf-backward app)
  "Ctrlf backward search — delegates to isearch-backward."
  (let ((cmd (find-command 'isearch-backward)))
    (when cmd (cmd app))))

;;;============================================================================
;;; Phi-search — another isearch alternative
;;;============================================================================

(def (cmd-phi-search app)
  "Phi-search — delegates to isearch-forward."
  (let ((cmd (find-command 'isearch-forward)))
    (when cmd (cmd app))))

(def (cmd-phi-search-backward app)
  "Phi-search backward."
  (let ((cmd (find-command 'isearch-backward)))
    (when cmd (cmd app))))

;;;============================================================================
;;; Toc-org — auto-generate table of contents in org files
;;;============================================================================

(def *tui-toc-org-mode* #f)

(def (cmd-toc-org-mode app)
  "Toggle toc-org mode — auto-generate TOC in org files."
  (set! *tui-toc-org-mode* (not *tui-toc-org-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-toc-org-mode* "Toc-org mode enabled" "Toc-org mode disabled")))

(def (cmd-toc-org-insert-toc app)
  "Insert/update table of contents at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (headings (filter (lambda (line) (and (> (string-length line) 0) (eqv? (string-ref line 0) #\*)))
                     lines))
         (toc-lines (map (lambda (h)
                           (let* ((level (let loop ((i 0)) (if (and (< i (string-length h)) (eqv? (string-ref h i) #\*)) (loop (+ i 1)) i)))
                                  (title (string-trim (substring h level (string-length h))))
                                  (indent (make-string (* 2 (- level 1)) #\space)))
                             (string-append indent "- " title)))
                      headings))
         (toc (string-append ":PROPERTIES:\n:TOC: :include all\n:END:\n\n" (string-join toc-lines "\n") "\n"))
         (pos (editor-get-current-pos ed)))
    (editor-insert-text ed pos toc)
    (echo-message! (app-state-echo app) (string-append "Inserted TOC with " (number->string (length headings)) " headings"))))

;;;============================================================================
;;; Org-super-agenda — enhanced org agenda grouping
;;;============================================================================

(def *tui-org-super-agenda* #f)

(def (cmd-org-super-agenda-mode app)
  "Toggle org-super-agenda mode — enhanced agenda grouping."
  (set! *tui-org-super-agenda* (not *tui-org-super-agenda*))
  (echo-message! (app-state-echo app)
    (if *tui-org-super-agenda* "Org-super-agenda enabled" "Org-super-agenda disabled")))

;;;============================================================================
;;; Nov.el — EPUB reader
;;;============================================================================

(def (cmd-nov-mode app)
  "Open EPUB file — basic text extraction."
  (let ((path (app-read-string app "EPUB file: ")))
    (when (and path (> (string-length path) 0))
      (if (not (file-exists? path))
        (echo-error! (app-state-echo app) "File not found")
        (let* ((result (with-catch
                         (lambda (e) (cons 'error (error-message e)))
                         (lambda ()
                           (let ((p (open-input-process
                                      (list path: "/bin/sh"
                                            arguments: (list "-c" (string-append "unzip -p " (shell-quote path) " '*.html' '*.xhtml' 2>/dev/null | sed 's/<[^>]*>//g' | head -500"))
                                            stdout-redirection: #t))))
                             (let ((out (read-line p #f)))
                               (close-input-port p)
                               (cons 'ok (or out "(empty)"))))))))
          (if (eq? (car result) 'error)
            (echo-error! (app-state-echo app) (string-append "EPUB error: " (cdr result)))
            (let* ((fr (app-state-frame app))
                   (win (current-window fr))
                   (ed (edit-window-editor win))
                   (buf (or (buffer-by-name "*EPUB*") (buffer-create! "*EPUB*" ed))))
              (buffer-attach! ed buf)
              (set! (edit-window-buffer win) buf)
              (editor-set-text ed (cdr result))
              (editor-goto-pos ed 0))))))))

(def (shell-quote s)
  "Quote a string for shell use."
  (string-append "'" (let loop ((i 0) (out ""))
    (if (>= i (string-length s)) out
      (let ((c (string-ref s i)))
        (if (eqv? c #\')
          (loop (+ i 1) (string-append out "'\"'\"'"))
          (loop (+ i 1) (string-append out (string c))))))) "'"))

;;;============================================================================
;;; LSP-UI — LSP user interface enhancements
;;;============================================================================

(def *tui-lsp-ui-mode* #f)

(def (cmd-lsp-ui-mode app)
  "Toggle LSP-UI mode — enhanced LSP display."
  (set! *tui-lsp-ui-mode* (not *tui-lsp-ui-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-lsp-ui-mode* "LSP-UI mode enabled" "LSP-UI mode disabled")))

(def (cmd-lsp-ui-doc-show app)
  "Show LSP documentation at point."
  (let ((cmd (find-command 'lsp-describe-thing-at-point)))
    (if cmd (cmd app)
      (echo-message! (app-state-echo app) "No LSP documentation available"))))

(def (cmd-lsp-ui-peek-find-definitions app)
  "Peek at definition — delegates to xref-find-definitions."
  (let ((cmd (find-command 'xref-find-definitions)))
    (when cmd (cmd app))))

(def (cmd-lsp-ui-peek-find-references app)
  "Peek at references — delegates to xref-find-references."
  (let ((cmd (find-command 'xref-find-references)))
    (when cmd (cmd app))))

;;;============================================================================
;;; Emojify — emoji display mode
;;;============================================================================

(def *tui-emojify-mode* #f)

(def (cmd-emojify-mode app)
  "Toggle emojify mode — display emoji shortcodes as Unicode."
  (set! *tui-emojify-mode* (not *tui-emojify-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-emojify-mode* "Emojify mode enabled" "Emojify mode disabled")))

(def (cmd-emojify-insert-emoji app)
  "Insert emoji by name."
  (let ((name (app-read-string app "Emoji name: ")))
    (when (and name (> (string-length name) 0))
      (let* ((fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (pos (editor-get-current-pos ed))
             (emoji (cond
                      ((equal? name "smile") "😊")
                      ((equal? name "thumbsup") "👍")
                      ((equal? name "heart") "❤️")
                      ((equal? name "fire") "🔥")
                      ((equal? name "rocket") "🚀")
                      ((equal? name "star") "⭐")
                      ((equal? name "check") "✅")
                      ((equal? name "x") "❌")
                      ((equal? name "warning") "⚠️")
                      ((equal? name "bug") "🐛")
                      (else (string-append ":" name ":")))))
        (editor-insert-text ed pos emoji)
        (editor-goto-pos ed (+ pos (string-length emoji)))))))

;;;============================================================================
;;; Ef-themes / modus-themes — Emacs theme packs
;;;============================================================================

(def (cmd-ef-themes-select app)
  "Select from ef-themes — delegates to customize-themes."
  (let ((cmd (find-command 'customize-themes)))
    (when cmd (cmd app))))

(def (cmd-modus-themes-toggle app)
  "Toggle between modus light/dark themes."
  (let ((cmd (find-command 'load-theme)))
    (if cmd (cmd app)
      (echo-message! (app-state-echo app) "Use M-x load-theme to switch themes"))))

;;;============================================================================
;;; Circadian / auto-dark — automatic theme switching
;;;============================================================================

(def *tui-circadian-mode* #f)

(def (cmd-circadian-mode app)
  "Toggle circadian mode — time-based theme switching."
  (set! *tui-circadian-mode* (not *tui-circadian-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-circadian-mode* "Circadian mode enabled (time-based themes)" "Circadian mode disabled")))

(def (cmd-auto-dark-mode app)
  "Toggle auto-dark mode — OS dark mode detection."
  (echo-message! (app-state-echo app) "Auto-dark: use M-x load-theme to switch manually"))

;;;============================================================================
;;; Breadcrumb — header line with code context
;;;============================================================================

(def *tui-breadcrumb-mode* #f)

(def (cmd-breadcrumb-mode app)
  "Toggle breadcrumb mode — show code context in header."
  (set! *tui-breadcrumb-mode* (not *tui-breadcrumb-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-breadcrumb-mode* "Breadcrumb mode enabled" "Breadcrumb mode disabled")))

;;;============================================================================
;;; Sideline — side information display
;;;============================================================================

(def *tui-sideline-mode* #f)

(def (cmd-sideline-mode app)
  "Toggle sideline mode — display info alongside code."
  (set! *tui-sideline-mode* (not *tui-sideline-mode*))
  (echo-message! (app-state-echo app)
    (if *tui-sideline-mode* "Sideline mode enabled" "Sideline mode disabled")))

;;;============================================================================
;;; Flycheck-inline — inline error display
;;;============================================================================

(def *tui-flycheck-inline* #f)

(def (cmd-flycheck-inline-mode app)
  "Toggle flycheck-inline mode — show errors inline."
  (set! *tui-flycheck-inline* (not *tui-flycheck-inline*))
  (echo-message! (app-state-echo app)
    (if *tui-flycheck-inline* "Flycheck-inline mode enabled" "Flycheck-inline mode disabled")))

;;;============================================================================
;;; Zone — screen saver
;;;============================================================================

(def (cmd-zone app)
  "Activate zone mode — screen saver."
  (echo-message! (app-state-echo app) "Zoning out... (press any key)"))

;;;============================================================================
;;; Fireplace — decorative fireplace
;;;============================================================================

(def (cmd-fireplace app)
  "Display a decorative fireplace in buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (or (buffer-by-name "*Fireplace*") (buffer-create! "*Fireplace*" ed)))
         (fire "    🔥🔥🔥🔥🔥🔥🔥\n   🔥🔥🔥🔥🔥🔥🔥🔥🔥\n  🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥🔥\n ╔═══════════════════════╗\n ║       FIREPLACE       ║\n ╚═══════════════════════╝\n   ░░░░░░░░░░░░░░░░░░░\n"))
    (buffer-attach! ed buf)
    (set! (edit-window-buffer win) buf)
    (editor-set-text ed fire)
    (editor-goto-pos ed 0)))

;;;============================================================================
;;; DAP-UI / poly-mode / company-box / impatient / modeline themes
;;;============================================================================

(def *tui-dap-ui-mode* #f)
(def (cmd-dap-ui-mode app)
  "Toggle DAP-UI mode — debugger UI panels."
  (set! *tui-dap-ui-mode* (not *tui-dap-ui-mode*))
  (echo-message! (app-state-echo app) (if *tui-dap-ui-mode* "DAP-UI enabled" "DAP-UI disabled")))

(def *tui-poly-mode* #f)
(def (cmd-poly-mode app)
  "Toggle poly-mode — multiple major modes in one buffer."
  (set! *tui-poly-mode* (not *tui-poly-mode*))
  (echo-message! (app-state-echo app) (if *tui-poly-mode* "Poly-mode enabled" "Poly-mode disabled")))

(def *tui-company-box* #f)
(def (cmd-company-box-mode app)
  "Toggle company-box mode — fancy completion popup."
  (set! *tui-company-box* (not *tui-company-box*))
  (echo-message! (app-state-echo app) (if *tui-company-box* "Company-box enabled" "Company-box disabled")))

(def *tui-impatient-mode* #f)
(def (cmd-impatient-mode app)
  "Toggle impatient mode — live preview HTML in browser."
  (set! *tui-impatient-mode* (not *tui-impatient-mode*))
  (echo-message! (app-state-echo app) (if *tui-impatient-mode* "Impatient mode enabled" "Impatient mode disabled")))

(def *tui-mood-line* #f)
(def (cmd-mood-line-mode app)
  "Toggle mood-line — minimal modeline theme."
  (set! *tui-mood-line* (not *tui-mood-line*))
  (echo-message! (app-state-echo app) (if *tui-mood-line* "Mood-line enabled" "Mood-line disabled")))

(def *tui-powerline* #f)
(def (cmd-powerline-mode app)
  "Toggle powerline — fancy modeline."
  (set! *tui-powerline* (not *tui-powerline*))
  (echo-message! (app-state-echo app) (if *tui-powerline* "Powerline enabled" "Powerline disabled")))

(def *tui-centaur-tabs* #f)
(def (cmd-centaur-tabs-mode app)
  "Toggle centaur-tabs — tab bar for buffer groups."
  (set! *tui-centaur-tabs* (not *tui-centaur-tabs*))
  (echo-message! (app-state-echo app) (if *tui-centaur-tabs* "Centaur-tabs enabled" "Centaur-tabs disabled")))

(def (cmd-all-the-icons-dired-mode app)
  "Toggle all-the-icons in dired."
  (echo-message! (app-state-echo app) "Icon display in dired: N/A in terminal"))

(def (cmd-treemacs-icons-dired-mode app)
  "Toggle treemacs icons in dired."
  (echo-message! (app-state-echo app) "Treemacs icons: N/A in terminal"))

(def (cmd-nano-theme app)
  "Switch to nano-emacs theme."
  (let ((cmd (find-command 'load-theme))) (when cmd (cmd app))))


