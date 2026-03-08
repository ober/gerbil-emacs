;;; -*- Gerbil -*-
;;; Qt commands core2 - tab/indent, autocomplete, misc commands, JSON, URL, scroll
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :std/text/json
        :std/net/uri
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/subprocess
        :gemacs/editor
        :gemacs/repl
        :gemacs/eshell
        :gemacs/shell
        :gemacs/terminal
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/persist
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        (only-in :gemacs/editor-core paredit-delimiter? auto-pair-char)
        (only-in :gemacs/gsh-eshell
                 gsh-eshell-buffer? gsh-eshell-prompt
                 eshell-history-prev eshell-history-next)
        :gemacs/qt/commands-core)

;;;============================================================================
;;; Tab / indent
;;;============================================================================

;;;----------------------------------------------------------------------------
;;; Autocomplete support
;;;----------------------------------------------------------------------------

;; Per-editor completer
(def *editor-completers* (make-hash-table))

(def (word-char-for-complete? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)
      (char=? ch #\_) (char=? ch #\-) (char=? ch #\!)
      (char=? ch #\?) (char=? ch #\*) (char=? ch #\>)))

(def (get-word-prefix ed)
  "Get the word prefix before the cursor."
  (let* ((pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (pos (min pos len)))
    (let loop ((i (- pos 1)))
      (if (or (< i 0) (not (word-char-for-complete? (string-ref text i))))
        (if (< (+ i 1) pos)
          (substring text (+ i 1) pos)
          "")
        (loop (- i 1))))))

(def (collect-buffer-words text)
  "Collect unique words from buffer text."
  (let ((words (make-hash-table))
        (len (string-length text)))
    (let loop ((i 0))
      (if (>= i len) (hash-keys words)
        (if (word-char-for-complete? (string-ref text i))
          ;; Start of a word
          (let find-end ((j (+ i 1)))
            (if (or (>= j len) (not (word-char-for-complete? (string-ref text j))))
              (begin
                (when (> (- j i) 1) ;; skip single-char words
                  (hash-put! words (substring text i j) #t))
                (loop j))
              (find-end (+ j 1))))
          (loop (+ i 1)))))))

(def (get-or-create-completer! ed app)
  "Get or create a completer for an editor."
  (or (hash-get *editor-completers* ed)
      (let ((c (qt-completer-create [])))
        (qt-completer-set-case-sensitivity! c #f)
        (qt-completer-set-widget! c ed)
        ;; When completion accepted, insert the remaining text
        (qt-on-completer-activated! c
          (lambda (text)
            (let ((prefix (get-word-prefix ed)))
              (when (> (string-length text) (string-length prefix))
                (qt-plain-text-edit-insert-text! ed
                  (substring text (string-length prefix) (string-length text)))))))
        (hash-put! *editor-completers* ed c)
        c)))

;;;============================================================================
;;; Misc commands
;;;============================================================================

(def (text-line-position text line-num)
  "Find the character position of the start of LINE-NUM (1-based) in TEXT."
  (if (<= line-num 1) 0
    (let loop ((i 0) (line 1))
      (cond
        ((>= i (string-length text)) i)
        ((char=? (string-ref text i) #\newline)
         (if (= (+ line 1) line-num)
           (+ i 1)
           (loop (+ i 1) (+ line 1))))
        (else (loop (+ i 1) line))))))

;;;============================================================================

(def (cmd-keyboard-quit app)
  (quit-flag-set!)
  (kill-active-subprocess!)
  (echo-message! (app-state-echo app) "Quit")
  (set! (app-state-key-state app) (make-initial-key-state))
  ;; Deactivate mark and clear visual selection (Emacs C-g behavior)
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app)))
    (when (buffer-mark buf)
      (set! (buffer-mark buf) #f)
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        (qt-plain-text-edit-set-selection! ed pos pos)))))

;;;============================================================================
;;; Scroll margin commands (Qt)
;;;============================================================================

(def (cmd-set-scroll-margin app)
  "Set the scroll margin (lines to keep visible above/below cursor)."
  (let* ((input (qt-echo-read-string app
                  (string-append "Scroll margin (current "
                                 (number->string *scroll-margin*) "): "))))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (when (and n (>= n 0) (<= n 20))
          (set! *scroll-margin* n)
          (echo-message! (app-state-echo app)
            (string-append "Scroll margin set to " (number->string n))))))))

(def (cmd-toggle-scroll-margin app)
  "Toggle scroll margin between 0 and 3."
  (if (> *scroll-margin* 0)
    (set! *scroll-margin* 0)
    (set! *scroll-margin* 3))
  (echo-message! (app-state-echo app)
    (if (> *scroll-margin* 0)
      (string-append "Scroll margin: " (number->string *scroll-margin*))
      "Scroll margin: off")))

;;;============================================================================
;;; Save-place mode (Qt)
;;;============================================================================

(def (cmd-toggle-save-place-mode app)
  "Toggle save-place mode — remembers cursor position in files."
  (set! *save-place-enabled* (not *save-place-enabled*))
  (echo-message! (app-state-echo app)
    (if *save-place-enabled* "Save-place mode ON" "Save-place mode OFF")))

;;;============================================================================
;;; Require-final-newline (Qt)
;;;============================================================================

(def (cmd-toggle-require-final-newline app)
  "Toggle requiring files to end with a newline on save."
  (set! *require-final-newline* (not *require-final-newline*))
  (echo-message! (app-state-echo app)
    (if *require-final-newline*
      "Require final newline: ON"
      "Require final newline: OFF")))

;;;============================================================================
;;; Centered cursor mode (Qt)
;;;============================================================================

(def (cmd-toggle-centered-cursor-mode app)
  "Toggle centered cursor mode — keeps cursor vertically centered."
  (set! *centered-cursor-mode* (not *centered-cursor-mode*))
  (echo-message! (app-state-echo app)
    (if *centered-cursor-mode*
      "Centered cursor mode ON"
      "Centered cursor mode OFF")))

;;;============================================================================
;;; Tab insertion (Qt)
;;;============================================================================

(def *qt-tab-width* 4)

(def (cmd-tab-to-tab-stop app)
  "Insert spaces (or tab) to the next tab stop."
  (let* ((ed (current-qt-editor app))
         (col (qt-plain-text-edit-cursor-column ed))
         (next-stop (* (+ 1 (quotient col *qt-tab-width*)) *qt-tab-width*))
         (spaces (- next-stop col))
         (str (make-string spaces #\space)))
    (qt-plain-text-edit-insert-text! ed str)))

(def (cmd-set-tab-width app)
  "Set the tab width for the current buffer."
  (let* ((input (qt-echo-read-string app
                  (string-append "Tab width (" (number->string *qt-tab-width*) "): "))))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (if (and n (exact? n) (> n 0) (<= n 16))
          (begin
            (set! *qt-tab-width* n)
            (echo-message! (app-state-echo app)
              (string-append "Tab width: " (number->string n))))
          (echo-error! (app-state-echo app) "Invalid tab width (1-16)"))))))

;;;============================================================================
;;; JSON format / minify / pretty-print
;;;============================================================================

(def (qt-json-pretty-print obj indent)
  "Pretty-print a JSON value with indentation."
  (let ((out (open-output-string)))
    (let pp ((val obj) (level 0))
      (let ((prefix (make-string (* level indent) #\space)))
        (cond
          ((hash-table? val)
           (display "{\n" out)
           (let ((keys (sort (hash-keys val) string<?))
                 (first #t))
             (for-each
               (lambda (k)
                 (unless first (display ",\n" out))
                 (display (make-string (* (+ level 1) indent) #\space) out)
                 (write k out)
                 (display ": " out)
                 (pp (hash-ref val k) (+ level 1))
                 (set! first #f))
               keys))
           (display "\n" out)
           (display prefix out)
           (display "}" out))
          ((list? val)
           (if (null? val)
             (display "[]" out)
             (begin
               (display "[\n" out)
               (let ((first #t))
                 (for-each
                   (lambda (item)
                     (unless first (display ",\n" out))
                     (display (make-string (* (+ level 1) indent) #\space) out)
                     (pp item (+ level 1))
                     (set! first #f))
                   val))
               (display "\n" out)
               (display prefix out)
               (display "]" out))))
          ((string? val) (write val out))
          ((number? val) (display val out))
          ((boolean? val) (display (if val "true" "false") out))
          ((not val) (display "null" out))
          (else (write val out)))))
    (get-output-string out)))

(def (cmd-json-format-buffer app)
  "Pretty-print JSON in the current buffer."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               (formatted (qt-json-pretty-print obj 2))
               (pos (qt-plain-text-edit-cursor-position ed)))
          (qt-plain-text-edit-set-text! ed (string-append formatted "\n"))
          (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length formatted)))
          (echo-message! echo "JSON formatted"))))))

(def (cmd-json-minify-buffer app)
  "Minify JSON in the current buffer (remove whitespace)."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-error! echo "Invalid JSON"))
      (lambda ()
        (let* ((obj (call-with-input-string text read-json))
               (minified (call-with-output-string
                           (lambda (port) (write-json obj port))))
               (pos (qt-plain-text-edit-cursor-position ed)))
          (qt-plain-text-edit-set-text! ed minified)
          (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length minified)))
          (echo-message! echo
            (string-append "JSON minified (" (number->string (string-length minified)) " bytes)")))))))

(def (cmd-json-pretty-print-region app)
  "Pretty-print JSON in selected region using python3."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "Select JSON region first")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end)))
        (with-catch
          (lambda (e) (echo-error! (app-state-echo app) "Invalid JSON"))
          (lambda ()
            (let* ((obj (call-with-input-string region read-json))
                   (formatted (qt-json-pretty-print obj 2))
                   (new-text (string-append
                               (substring text 0 sel-start)
                               formatted
                               (substring text sel-end (string-length text)))))
              (qt-plain-text-edit-set-text! ed new-text)
              (qt-plain-text-edit-set-cursor-position! ed sel-start)
              (echo-message! (app-state-echo app) "JSON formatted"))))))))

;;;============================================================================
;;; URL encode / decode
;;;============================================================================

(def (cmd-url-encode-region app)
  "URL-encode the selected region."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (encoded (uri-encode region))
             (new-text (string-append
                         (substring text 0 sel-start)
                         encoded
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length encoded)))
        (echo-message! (app-state-echo app) "URL encoded")))))

(def (cmd-url-decode-region app)
  "URL-decode the selected region."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-error! (app-state-echo app) "No region selected")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (decoded (uri-decode region))
             (new-text (string-append
                         (substring text 0 sel-start)
                         decoded
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ sel-start (string-length decoded)))
        (echo-message! (app-state-echo app) "URL decoded")))))

;;;============================================================================
;;; Reverse / shuffle lines
;;;============================================================================

(def (qt-reverse-lines-in-string text)
  "Reverse order of lines in a string."
  (let* ((lines (string-split text #\newline))
         (reversed (reverse lines)))
    (string-join reversed "\n")))

(def (cmd-reverse-lines app)
  "Reverse the order of lines in region or entire buffer."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      ;; No selection => reverse entire buffer
      (let* ((text (qt-plain-text-edit-text ed))
             (result (qt-reverse-lines-in-string text)))
        (qt-plain-text-edit-set-text! ed result)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app) "Reversed all lines"))
      ;; Selection => reverse selected region
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (result (qt-reverse-lines-in-string region))
             (new-text (string-append
                         (substring text 0 sel-start)
                         result
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed sel-start)
        (echo-message! (app-state-echo app) "Reversed selected lines")))))

(def (qt-shuffle lst)
  "Fisher-Yates shuffle of a list."
  (let ((vec (list->vector lst)))
    (let loop ((i (- (vector-length vec) 1)))
      (when (> i 0)
        (let* ((j (random-integer (+ i 1)))
               (tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp)
          (loop (- i 1)))))
    (vector->list vec)))

(def (cmd-shuffle-lines app)
  "Randomly shuffle lines in region or entire buffer."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (let* ((text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (shuffled (qt-shuffle lines))
             (result (string-join shuffled "\n")))
        (qt-plain-text-edit-set-text! ed result)
        (qt-plain-text-edit-set-cursor-position! ed 0)
        (echo-message! (app-state-echo app) "Shuffled all lines"))
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (shuffled (qt-shuffle lines))
             (result (string-join shuffled "\n"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         result
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed sel-start)
        (echo-message! (app-state-echo app) "Shuffled selected lines")))))

;;;============================================================================
;;; XML format
;;;============================================================================

(def (cmd-xml-format app)
  "Format XML in the current buffer using xmllint."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (with-catch
      (lambda (e) (echo-error! (app-state-echo app) "xmllint not available"))
      (lambda ()
        (let* ((proc (open-process
                       (list path: "xmllint"
                             arguments: '("--format" "-")
                             stdin-redirection: #t stdout-redirection: #t
                             stderr-redirection: #t)))
               (_ (begin (display text proc) (close-output-port proc)))
               (result (read-line proc #f)))
          (process-status proc)
          (if (and result (> (string-length result) 0))
            (begin
              (qt-plain-text-edit-set-text! ed result)
              (qt-plain-text-edit-set-cursor-position! ed 0)
              (echo-message! (app-state-echo app) "XML formatted"))
            (echo-error! (app-state-echo app) "XML format failed")))))))

;;;============================================================================
;;; Open URL at point
;;;============================================================================

(def (qt-find-url-at-point text pos)
  "Find URL boundaries at position in text."
  (let ((len (string-length text)))
    ;; Scan backward from pos to find start of URL-like string
    (let find-start ((i pos))
      (if (or (<= i 0)
              (memv (string-ref text (- i 1)) '(#\space #\tab #\newline #\( #\) #\[ #\] #\" #\')))
        ;; Check if this looks like a URL
        (let ((candidate (let find-end ((j (max i pos)))
                           (if (or (>= j len)
                                   (memv (string-ref text j) '(#\space #\tab #\newline #\) #\] #\" #\')))
                             (substring text i j)
                             (find-end (+ j 1))))))
          (if (or (string-prefix? "http://" candidate)
                  (string-prefix? "https://" candidate)
                  (string-prefix? "ftp://" candidate))
            (cons i (+ i (string-length candidate)))
            #f))
        (find-start (- i 1))))))

(def (cmd-open-url-at-point app)
  "Open URL at point in external browser."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (let ((url-bounds (qt-find-url-at-point text pos)))
      (if (not url-bounds)
        (echo-error! (app-state-echo app) "No URL at point")
        (let ((url (substring text (car url-bounds) (cdr url-bounds))))
          (with-catch
            (lambda (e) (echo-error! (app-state-echo app) "Failed to open URL"))
            (lambda ()
              (open-process
                (list path: "xdg-open" arguments: (list url)
                      stdin-redirection: #f stdout-redirection: #f
                      stderr-redirection: #f))
              (echo-message! (app-state-echo app) (string-append "Opening: " url)))))))))

;;;============================================================================
;;; Compare windows
;;;============================================================================

(def (cmd-compare-windows app)
  "Compare text in two visible windows and jump to first difference."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr)))
    (if (< (length wins) 2)
      (echo-error! (app-state-echo app) "Need at least 2 windows")
      (let* ((w1 (car wins))
             (w2 (cadr wins))
             (e1 (qt-edit-window-editor w1))
             (e2 (qt-edit-window-editor w2))
             (t1 (qt-plain-text-edit-text e1))
             (t2 (qt-plain-text-edit-text e2))
             (len (min (string-length t1) (string-length t2))))
        (let loop ((i 0))
          (cond
            ((>= i len)
             (if (= (string-length t1) (string-length t2))
               (echo-message! (app-state-echo app) "Windows are identical")
               (echo-message! (app-state-echo app)
                 (string-append "Differ at pos " (number->string i) " (one buffer is shorter)"))))
            ((not (char=? (string-ref t1 i) (string-ref t2 i)))
             (echo-message! (app-state-echo app)
               (string-append "First difference at position " (number->string i))))
            (else (loop (+ i 1)))))))))

;;;============================================================================
;;; Dedent region (remove one level of indentation)
;;;============================================================================

(def (cmd-dedent-region app)
  "Remove one level of indentation from selected region."
  (let* ((ed (current-qt-editor app))
         (sel-start (qt-plain-text-edit-selection-start ed))
         (sel-end (qt-plain-text-edit-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "No selection to dedent")
      (let* ((text (qt-plain-text-edit-text ed))
             (region (substring text sel-start sel-end))
             (lines (string-split region #\newline))
             (dedented (map (lambda (line)
                              (let ((len (string-length line)))
                                (cond
                                  ((and (> len 0) (char=? (string-ref line 0) #\tab))
                                   (substring line 1 len))
                                  ((string-prefix? "    " line)
                                   (substring line 4 len))
                                  ((string-prefix? "  " line)
                                   (substring line 2 len))
                                  (else line))))
                            lines))
             (result (string-join dedented "\n"))
             (new-text (string-append
                         (substring text 0 sel-start)
                         result
                         (substring text sel-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-selection! ed sel-start (+ sel-start (string-length result)))
        (echo-message! (app-state-echo app) "Region dedented")))))

;;;============================================================================
;;; Count words in current line
;;;============================================================================

(def (cmd-count-words-line app)
  "Count words in current line."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines)) (list-ref lines line) "")))
    (let loop ((i 0) (count 0) (in-word #f))
      (if (>= i (string-length line-text))
        (echo-message! (app-state-echo app)
          (string-append "Words in line: " (number->string (if in-word (+ count 1) count))))
        (let ((ch (string-ref line-text i)))
          (if (or (char=? ch #\space) (char=? ch #\tab))
            (loop (+ i 1) (if in-word (+ count 1) count) #f)
            (loop (+ i 1) count #t)))))))

;;;============================================================================
;;; Diff goto source (jump from diff buffer to source)
;;;============================================================================

(def (cmd-diff-goto-source app)
  "Jump from a diff hunk to the corresponding source location."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (lines (string-split text #\newline))
         (cur-line (qt-plain-text-edit-cursor-line ed)))
    ;; Find the @@ hunk header and +++ file header above
    (let find-context ((l cur-line) (file #f) (hunk-line #f))
      (cond
        ((< l 0)
         (if (and file hunk-line)
           ;; Count lines from hunk start to cursor
           (let* ((offset (- cur-line (+ l 1)))
                  (target-line (+ hunk-line offset)))
             (if (file-exists? file)
               (begin
                 (cmd-find-file-by-path app file)
                 (echo-message! (app-state-echo app)
                   (string-append file ":" (number->string target-line))))
               (echo-error! (app-state-echo app) (string-append "File not found: " file))))
           (echo-error! (app-state-echo app) "No diff context found")))
        (else
         (let ((line-text (if (< l (length lines)) (list-ref lines l) "")))
           (cond
             ((and (not hunk-line) (string-prefix? "@@ " line-text))
              ;; Parse @@ -x,y +N,M @@ to get N
              (let* ((plus-pos (string-contains line-text "+"))
                     (comma-pos (and plus-pos (string-contains line-text "," (+ plus-pos 1))))
                     (space-pos (and plus-pos (string-contains line-text " " (+ plus-pos 1))))
                     (end-pos (or comma-pos space-pos (string-length line-text)))
                     (num-str (and plus-pos (substring line-text (+ plus-pos 1) end-pos)))
                     (num (and num-str (string->number num-str))))
                (find-context (- l 1) file (or num 1))))
             ((and (not file) (string-prefix? "+++ " line-text))
              (let* ((path (substring line-text 4 (string-length line-text)))
                     (clean (if (string-prefix? "b/" path) (substring path 2 (string-length path)) path)))
                (find-context (- l 1) clean hunk-line)))
             (else (find-context (- l 1) file hunk-line)))))))))

(def (cmd-find-file-by-path app path)
  "Open file by path (helper for diff-goto-source)."
  (let* ((ed (current-qt-editor app))
         (fr (app-state-frame app))
         (name (path-strip-directory path)))
    (let ((buf (or (buffer-by-name name) (qt-buffer-create! name ed))))
      (when (file-exists? path)
        (let ((content (read-file-as-string path)))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed (or content ""))
          (set! (buffer-file-path buf) path)
          (qt-modeline-update! app))))))

;;;============================================================================
;;; Insert date ISO (alias for consistency with TUI)
;;;============================================================================

(def (cmd-insert-date-iso app)
  "Insert current date in ISO 8601 format (YYYY-MM-DD)."
  (let* ((ed (current-qt-editor app))
         (out (open-process (list path: "date" arguments: '("+%Y-%m-%d"))))
         (result (read-line out)))
    (close-port out)
    (qt-plain-text-edit-insert-text! ed (if (string? result) result "???"))))

;;;============================================================================
;;; Org-mode parity commands
;;;============================================================================

(def (cmd-org-schedule app)
  "Insert SCHEDULED timestamp on next line."
  (let* ((ed (current-qt-editor app))
         (date (qt-echo-read-string app "Schedule date (YYYY-MM-DD): ")))
    (when (and date (> (string-length date) 0))
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (line-end (let loop ((i pos))
                         (if (or (>= i (string-length text))
                                 (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
             (new-text (string-append (substring text 0 line-end)
                                      "\n  SCHEDULED: <" date ">"
                                      (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end 1))
        (echo-message! (app-state-echo app) (string-append "Scheduled: " date))))))

(def (cmd-org-deadline app)
  "Insert DEADLINE timestamp on next line."
  (let* ((ed (current-qt-editor app))
         (date (qt-echo-read-string app "Deadline date (YYYY-MM-DD): ")))
    (when (and date (> (string-length date) 0))
      (let* ((text (qt-plain-text-edit-text ed))
             (pos (qt-plain-text-edit-cursor-position ed))
             (line-end (let loop ((i pos))
                         (if (or (>= i (string-length text))
                                 (char=? (string-ref text i) #\newline))
                           i (loop (+ i 1)))))
             (new-text (string-append (substring text 0 line-end)
                                      "\n  DEADLINE: <" date ">"
                                      (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end 1))
        (echo-message! (app-state-echo app) (string-append "Deadline: " date))))))

(def (cmd-org-insert-src-block app)
  "Insert #+BEGIN_SRC ... #+END_SRC template at point."
  (let* ((ed (current-qt-editor app))
         (lang (qt-echo-read-string app "Language (default: empty): "))
         (lang-str (if (and lang (> (string-length lang) 0))
                     (string-append " " lang) ""))
         (template (string-append "#+BEGIN_SRC" lang-str "\n\n#+END_SRC\n")))
    (qt-plain-text-edit-insert-text! ed template)
    (echo-message! (app-state-echo app) "Source block inserted")))

(def (cmd-org-clock-in app)
  "Insert CLOCK-IN timestamp in :LOGBOOK: drawer."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line-end (let loop ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (loop (+ i 1)))))
         (now (with-exception-catcher (lambda (e) "")
                (lambda ()
                  (let ((p (open-process
                             (list path: "date"
                                   arguments: '("+[%Y-%m-%d %a %H:%M]")))))
                    (let ((out (read-line p)))
                      (close-port p) (or out "")))))))
    (when (> (string-length now) 0)
      (let* ((clock-text (string-append "\n  :LOGBOOK:\n  CLOCK: " now "\n  :END:"))
             (new-text (string-append (substring text 0 line-end)
                                      clock-text
                                      (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ line-end 1))
        (echo-message! (app-state-echo app) (string-append "Clocked in: " now))))))

(def (cmd-org-clock-out app)
  "Close open CLOCK entry with end timestamp."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (echo (app-state-echo app))
         ;; Find last open CLOCK entry
         (clock-line
           (let loop ((i (- (length lines) 1)))
             (cond
               ((< i 0) #f)
               ((let ((l (list-ref lines i)))
                  (and (string-contains l "CLOCK: [")
                       (not (string-contains l "--"))))
                i)
               (else (loop (- i 1)))))))
    (if (not clock-line)
      (echo-message! echo "No open clock entry")
      (let ((now (with-exception-catcher (lambda (e) "")
                   (lambda ()
                     (let ((p (open-process
                                (list path: "date"
                                      arguments: '("+[%Y-%m-%d %a %H:%M]")))))
                       (let ((out (read-line p)))
                         (close-port p) (or out "")))))))
        (when (> (string-length now) 0)
          (let* ((old-line (list-ref lines clock-line))
                 (new-line (string-append old-line "--" now))
                 (new-lines (let loop ((ls lines) (i 0) (acc []))
                              (if (null? ls) (reverse acc)
                                (loop (cdr ls) (+ i 1)
                                      (cons (if (= i clock-line) new-line (car ls)) acc))))))
            (qt-plain-text-edit-set-text! ed (string-join new-lines "\n"))
            (echo-message! echo (string-append "Clocked out: " now))))))))
