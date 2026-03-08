;;; -*- Gerbil -*-
;;; Qt commands sexp2 - tab width, replace string, navigation, case, copy, session
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
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
        :gemacs/qt/commands-search
        :gemacs/qt/commands-search2
        :gemacs/qt/commands-file
        :gemacs/qt/commands-file2
        :gemacs/qt/commands-sexp)

;;;============================================================================
;;; Imenu - jump to definition (multi-language)
;;;============================================================================

;; Note: cmd-imenu is defined near goto-definition with multi-language support

;;;============================================================================
;;; Cycle tab width
;;;============================================================================

(def *tab-width* 4)

(def (cmd-cycle-tab-width app)
  "Cycle tab stop width: 2 -> 4 -> 8 -> 2."
  (set! *tab-width*
    (cond ((= *tab-width* 2) 4)
          ((= *tab-width* 4) 8)
          (else 2)))
  (echo-message! (app-state-echo app)
    (string-append "Tab width: " (number->string *tab-width*))))

;;;============================================================================
;;; Toggle indent tabs mode
;;;============================================================================

(def *indent-tabs-mode* #f)

(def (cmd-toggle-indent-tabs-mode app)
  "Toggle between tabs and spaces for indentation."
  (set! *indent-tabs-mode* (not *indent-tabs-mode*))
  (echo-message! (app-state-echo app)
    (if *indent-tabs-mode* "Indent with tabs" "Indent with spaces")))

;;;============================================================================
;;; Replace string (non-interactive replace-all)
;;;============================================================================

(def (cmd-replace-string app)
  "Replace all occurrences of a string."
  (let ((from (qt-echo-read-string app "Replace string: ")))
    (when from
      (let ((to (qt-echo-read-string app (string-append "Replace \"" from "\" with: "))))
        (when to
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (count 0)
                 (result (let loop ((s text) (acc ""))
                           (let ((idx (string-contains s from)))
                             (if idx
                               (begin
                                 (set! count (+ count 1))
                                 (loop (substring s (+ idx (string-length from)) (string-length s))
                                       (string-append acc (substring s 0 idx) to)))
                               (string-append acc s))))))
            (qt-plain-text-edit-set-text! ed result)
            (echo-message! (app-state-echo app)
              (string-append "Replaced " (number->string count) " occurrences"))))))))

;;;============================================================================
;;; String insert file (alias)
;;;============================================================================

(def (cmd-string-insert-file app)
  "Insert a file's contents (alias for insert-file)."
  (cmd-insert-file app))

;;;============================================================================
;;; Navigation: goto-column, recenter-top/bottom, window positions
;;;============================================================================

(def (cmd-goto-column app)
  "Move cursor to a specified column on the current line."
  (let ((input (qt-echo-read-string app "Go to column: ")))
    (when input
      (let ((col (string->number input)))
        (when (and col (>= col 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (line (qt-plain-text-edit-cursor-line ed))
                 (line-start (line-start-position text line))
                 (lines (string-split text #\newline))
                 (line-len (if (< line (length lines))
                             (string-length (list-ref lines line))
                             0))
                 (target-col (min col line-len)))
            (qt-plain-text-edit-set-cursor-position! ed (+ line-start target-col))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-goto-line-relative app)
  "Move cursor by a relative number of lines."
  (let ((input (qt-echo-read-string app "Relative lines (+/-): ")))
    (when input
      (let ((n (string->number input)))
        (when n
          (let* ((ed (current-qt-editor app))
                 (line (qt-plain-text-edit-cursor-line ed))
                 (text (qt-plain-text-edit-text ed))
                 (total (length (string-split text #\newline)))
                 (target (max 0 (min (- total 1) (+ line n)))))
            (qt-plain-text-edit-set-cursor-position! ed
              (line-start-position text target))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))

(def (cmd-recenter-top app)
  "Scroll so cursor is at the top of the window."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-ensure-cursor-visible! ed)
    (echo-message! (app-state-echo app) "Recentered to top")))

(def (cmd-recenter-bottom app)
  "Scroll so cursor is at the bottom of the window."
  (let ((ed (current-qt-editor app)))
    (qt-plain-text-edit-ensure-cursor-visible! ed)
    (echo-message! (app-state-echo app) "Recentered to bottom")))

;;;============================================================================
;;; Character case commands
;;;============================================================================

(def (cmd-upcase-char app)
  "Upcase the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (char-upcase (string-ref text pos)))
             (new-text (string-append (substring text 0 pos)
                                      (string ch)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-downcase-char app)
  "Downcase the character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (char-downcase (string-ref text pos)))
             (new-text (string-append (substring text 0 pos)
                                      (string ch)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-toggle-case-at-point app)
  "Toggle case of character at point."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (when (< pos (string-length text))
      (let* ((ch (string-ref text pos))
             (toggled (if (char-upper-case? ch)
                        (char-downcase ch)
                        (char-upcase ch)))
             (new-text (string-append (substring text 0 pos)
                                      (string toggled)
                                      (substring text (+ pos 1) (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ pos 1))))))

(def (cmd-capitalize-region app)
  "Capitalize the first letter of each word in region."
  (cmd-upcase-initials-region app))

;;;============================================================================
;;; Copy commands
;;;============================================================================

(def (cmd-copy-buffer-name app)
  "Copy buffer name to kill ring."
  (let* ((buf (current-qt-buffer app))
         (name (buffer-name buf)))
    (set! (app-state-kill-ring app) (cons name (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied: " name))))

(def (cmd-copy-current-line app)
  "Copy the current line to kill ring."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines)) (list-ref lines line) "")))
    (set! (app-state-kill-ring app) (cons line-text (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) "Line copied")))

(def (cmd-copy-word app)
  "Copy word at point to kill ring."
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
             (word (substring text start end)))
        (when (> (string-length word) 0)
          (set! (app-state-kill-ring app) (cons word (app-state-kill-ring app)))
          (echo-message! (app-state-echo app) (string-append "Copied: " word)))))))

(def (cmd-copy-file-path app)
  "Copy file path to kill ring."
  (let* ((buf (current-qt-buffer app))
         (path (buffer-file-path buf)))
    (if path
      (begin
        (set! (app-state-kill-ring app) (cons path (app-state-kill-ring app)))
        (echo-message! (app-state-echo app) (string-append "Copied: " path)))
      (echo-error! (app-state-echo app) "Buffer has no file"))))

(def (cmd-copy-line-number app)
  "Copy current line number to kill ring."
  (let* ((ed (current-qt-editor app))
         (line (+ 1 (qt-plain-text-edit-cursor-line ed)))
         (s (number->string line)))
    (set! (app-state-kill-ring app) (cons s (app-state-kill-ring app)))
    (echo-message! (app-state-echo app) (string-append "Copied line: " s))))

(def (cmd-copy-region-as-kill app)
  "Copy region without deactivating mark (alias for copy-region)."
  (cmd-copy-region app))

(def (cmd-yank-whole-line app)
  "Yank the first item from kill ring as a whole line."
  (let* ((ed (current-qt-editor app))
         (kr (app-state-kill-ring app))
         (text (and (pair? kr) (car kr))))
    (if (not text)
      (echo-error! (app-state-echo app) "Kill ring empty")
      (begin
        ;; Move to beginning of line, insert text + newline
        (let* ((pos (qt-plain-text-edit-cursor-position ed))
               (full (qt-plain-text-edit-text ed))
               (line-start (line-start-position full (qt-plain-text-edit-cursor-line ed))))
          (qt-plain-text-edit-set-cursor-position! ed line-start)
          (qt-plain-text-edit-insert-text! ed (string-append text "\n")))))))

;;;============================================================================
;;; Transpose windows (swap two windows' buffers)
;;;============================================================================

(def (cmd-transpose-windows app)
  "Swap the buffers displayed in the current and next window."
  (let* ((fr (app-state-frame app))
         (wins (qt-frame-windows fr))
         (echo (app-state-echo app)))
    (if (< (length wins) 2)
      (echo-message! echo "Need at least 2 windows to transpose")
      (let* ((cur (qt-current-window fr))
             ;; Find the other window
             (other (let loop ((ws wins))
                      (cond
                        ((null? ws) (car wins))
                        ((not (eq? (car ws) cur)) (car ws))
                        (else (loop (cdr ws))))))
             (buf1 (qt-edit-window-buffer cur))
             (buf2 (qt-edit-window-buffer other))
             (ed1 (qt-edit-window-editor cur))
             (ed2 (qt-edit-window-editor other)))
        ;; Swap buffers
        (qt-buffer-attach! ed1 buf2)
        (qt-buffer-attach! ed2 buf1)
        (set! (qt-edit-window-buffer cur) buf2)
        (set! (qt-edit-window-buffer other) buf1)
        (qt-modeline-update! app)
        (echo-message! echo "Windows transposed")))))

;;;============================================================================
;;; Desktop session management (save/read/clear)
;;;============================================================================

(def (cmd-desktop-save app)
  "Save desktop session (buffer list and files)."
  (let* ((bufs (buffer-list))
         (files (filter (lambda (f) f)
                        (map buffer-file-path bufs)))
         (session-file (string-append (getenv "HOME" "/tmp") "/.gemacs-session")))
    (with-catch
      (lambda (e) (echo-message! (app-state-echo app) "Error saving session"))
      (lambda ()
        (call-with-output-file session-file
          (lambda (port)
            (for-each (lambda (f) (display f port) (newline port)) files)))
        (echo-message! (app-state-echo app)
          (string-append "Session saved: " (number->string (length files)) " files"))))))

(def (cmd-desktop-read app)
  "Restore desktop session from saved file list."
  (let ((session-file (string-append (getenv "HOME" "/tmp") "/.gemacs-session")))
    (if (file-exists? session-file)
      (with-catch
        (lambda (e) (echo-message! (app-state-echo app) "Error reading session"))
        (lambda ()
          (let ((files (call-with-input-file session-file
                         (lambda (port)
                           (let loop ((acc []))
                             (let ((line (read-line port)))
                               (if (eof-object? line)
                                 (reverse acc)
                                 (loop (cons line acc)))))))))
            (let* ((fr (app-state-frame app))
                   (ed (current-qt-editor app))
                   (count 0))
              (for-each
                (lambda (f)
                  (when (file-exists? f)
                    (let ((buf (qt-buffer-create! (path-strip-directory f) ed)))
                      (set! (buffer-file-path buf) f)
                      (set! count (+ count 1)))))
                files)
              (echo-message! (app-state-echo app)
                (string-append "Session restored: " (number->string count) " files"))))))
      (echo-message! (app-state-echo app) "No session file found"))))

(def (cmd-desktop-clear app)
  "Clear saved session file."
  (let ((session-file (string-append (getenv "HOME" "/tmp") "/.gemacs-session")))
    (when (file-exists? session-file)
      (delete-file session-file))
    (echo-message! (app-state-echo app) "Session cleared")))

;;;============================================================================
;;; Savehist commands (user-facing wrappers)
;;;============================================================================

(def *qt-history-file*
  (string-append (getenv "HOME" "/tmp") "/.gemacs-history"))

(def (cmd-savehist-save app)
  "Save command history to disk."
  (with-catch
    (lambda (e) (echo-error! (app-state-echo app) "Error saving history"))
    (lambda ()
      (when (pair? *mx-command-history*)
        (call-with-output-file *qt-history-file*
          (lambda (port)
            (let loop ((h *mx-command-history*) (n 0))
              (when (and (pair? h) (< n 500))
                (display (car h) port) (newline port)
                (loop (cdr h) (+ n 1)))))))
      (echo-message! (app-state-echo app)
        (string-append "History saved: " (number->string (length *mx-command-history*)) " entries")))))

(def (cmd-savehist-load app)
  "Load command history from disk."
  (with-catch
    (lambda (e) (echo-error! (app-state-echo app) "Error loading history"))
    (lambda ()
      (when (file-exists? *qt-history-file*)
        (let ((lines (call-with-input-file *qt-history-file*
                       (lambda (port)
                         (let loop ((acc []))
                           (let ((line (read-line port)))
                             (if (eof-object? line) (reverse acc)
                               (loop (cons line acc)))))))))
          (set! *mx-command-history* lines)))
      (echo-message! (app-state-echo app) "History loaded"))))

(def *savehist-mode-enabled* #f)

(def (cmd-savehist-mode app)
  "Toggle savehist-mode (auto-save/load command history)."
  (set! *savehist-mode-enabled* (not *savehist-mode-enabled*))
  (when *savehist-mode-enabled*
    (cmd-savehist-load app))
  (echo-message! (app-state-echo app)
    (if *savehist-mode-enabled* "savehist-mode ON" "savehist-mode OFF")))

;;;============================================================================
;;; Paredit wrap-curly (missing from Qt)
;;;============================================================================

(def (cmd-paredit-wrap-curly app)
  "Wrap the sexp at point in curly braces."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (fwd (skip-whitespace-forward text pos))
         (end (sexp-end text fwd)))
    (when (> end fwd)
      (let ((new-text (string-append
                        (substring text 0 fwd) "{"
                        (substring text fwd end) "}"
                        (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (+ fwd 1))
        (qt-plain-text-edit-ensure-cursor-visible! ed)))))

;;;============================================================================
;;; Complete-at-point (buffer-word completion)
;;;============================================================================

(def (cmd-complete-at-point app)
  "Show completion popup for the word prefix at point."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
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
          ;; Show Scintilla native autocomplete popup
          (let ((words (sort (hash-keys candidates) string<?)))
            (if (null? words)
              (echo-message! echo (string-append "No completions for \"" prefix "\""))
              (if (= (length words) 1)
                ;; Single match: insert it directly
                (begin
                  (qt-plain-text-edit-set-selection! ed prefix-start pos)
                  (qt-plain-text-edit-remove-selected-text! ed)
                  (qt-plain-text-edit-insert-text! ed (car words))
                  (echo-message! echo (string-append "Completed: " (car words))))
                ;; Multiple: show Scintilla autocomplete popup
                (begin
                  (sci-send ed SCI_AUTOCSETSEPARATOR
                            (char->integer #\newline) 0)
                  (sci-send ed SCI_AUTOCSETIGNORECASE 0 0)
                  (sci-send ed SCI_AUTOCSETMAXHEIGHT 10 0)
                  (sci-send ed SCI_AUTOCSETDROPRESTOFWORD 1 0)
                  (sci-send ed SCI_AUTOCSETORDER 1 0)
                  (sci-send/string ed SCI_AUTOCSHOW
                    (string-join words "\n") plen)
                  (echo-message! echo
                    (string-append (number->string (length words))
                                   " completions")))))))))))

;;;============================================================================
;;; Org-mode parity: agenda, export, priority
;;;============================================================================

(def (cmd-org-agenda app)
  "Scan open buffers for TODO/DONE items and display in *Org Agenda*."
  (let* ((fr (app-state-frame app))
         (ed (current-qt-editor app))
         (items []))
    ;; Scan all buffers via file on disk
    (for-each
      (lambda (buf)
        (let ((fp (buffer-file-path buf))
              (name (buffer-name buf)))
          (when fp
            (with-exception-catcher (lambda (e) (void))
              (lambda ()
                (let* ((content (call-with-input-file fp (lambda (p) (read-line p #f))))
                       (lines (if content (string-split content #\newline) '())))
                  (let loop ((ls lines) (n 1))
                    (when (pair? ls)
                      (let ((l (car ls)))
                        (when (or (string-contains l "TODO ")
                                  (string-contains l "SCHEDULED:")
                                  (string-contains l "DEADLINE:"))
                          (set! items (cons (string-append "  " name ":"
                                                          (number->string n) ": "
                                                          (string-trim l))
                                           items))))
                      (loop (cdr ls) (+ n 1))))))))))
      *buffer-list*)
    ;; Also scan current editor text
    (let* ((text (qt-plain-text-edit-text ed))
           (cur-buf (current-qt-buffer app))
           (cur-name (if cur-buf (buffer-name cur-buf) "*scratch*"))
           (lines (string-split text #\newline)))
      (let loop ((ls lines) (n 1))
        (when (pair? ls)
          (let ((l (car ls)))
            (when (or (string-contains l "TODO ")
                      (string-contains l "SCHEDULED:")
                      (string-contains l "DEADLINE:"))
              (set! items (cons (string-append "  " cur-name ":"
                                              (number->string n) ": "
                                              (string-trim l))
                               items))))
          (loop (cdr ls) (+ n 1)))))
    (let* ((buf (or (buffer-by-name "*Org Agenda*")
                    (qt-buffer-create! "*Org Agenda*" ed #f)))
           (agenda-text (if (null? items)
                          "Org Agenda\n\nNo TODO items found.\n"
                          (string-append "Org Agenda\n\n"
                                        (string-join (reverse items) "\n")
                                        "\n"))))
      (qt-buffer-attach! ed buf)
      (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
      (qt-plain-text-edit-set-text! ed agenda-text)
      (qt-plain-text-edit-set-cursor-position! ed 0)
      (qt-modeline-update! app))))

(def (cmd-org-export app)
  "Export org buffer to plain text (strip markup)."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (exported
           (string-join
             (map (lambda (line)
                    (cond
                      ;; Strip leading *s from headings
                      ((and (> (string-length line) 0)
                            (char=? (string-ref line 0) #\*))
                       (let loop ((i 0))
                         (if (and (< i (string-length line))
                                  (or (char=? (string-ref line i) #\*)
                                      (char=? (string-ref line i) #\space)))
                           (loop (+ i 1))
                           (substring line i (string-length line)))))
                      ;; Strip #+KEYWORDS
                      ((string-prefix? "#+" line) "")
                      (else line)))
                  lines)
             "\n"))
         (fr (app-state-frame app))
         (buf (or (buffer-by-name "*Org Export*")
                  (qt-buffer-create! "*Org Export*" ed #f))))
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    (qt-plain-text-edit-set-text! ed exported)
    (qt-plain-text-edit-set-cursor-position! ed 0)
    (qt-modeline-update! app)
    (echo-message! (app-state-echo app) "Exported to *Org Export*")))

(def (cmd-org-priority app)
  "Cycle priority on heading: none -> [#A] -> [#B] -> [#C] -> none."
  (let* ((ed (current-qt-editor app)) (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (line-start (let lp ((i (- pos 1)))
                       (if (or (< i 0) (char=? (string-ref text i) #\newline))
                         (+ i 1) (lp (- i 1)))))
         (line-end (let lp ((i pos))
                     (if (or (>= i (string-length text))
                             (char=? (string-ref text i) #\newline))
                       i (lp (+ i 1)))))
         (line (substring text line-start line-end)) (echo (app-state-echo app))
         (replace-pri (lambda (old new)
                        (let ((idx (string-contains line old)))
                          (and idx (string-append (substring line 0 idx) new
                                                  (substring line (+ idx (string-length old))
                                                             (string-length line))))))))
    (if (or (= (string-length line) 0) (not (char=? (string-ref line 0) #\*)))
      (echo-message! echo "Not on a heading")
      (let* ((new-line (or (replace-pri "[#A] " "[#B] ")
                           (replace-pri "[#B] " "[#C] ")
                           (replace-pri "[#C] " "")
                           (let lp ((i 0))
                             (if (and (< i (string-length line)) (char=? (string-ref line i) #\*))
                               (lp (+ i 1))
                               (string-append (substring line 0 i) " [#A]"
                                              (substring line i (string-length line)))))))
             (new-text (string-append (substring text 0 line-start) new-line
                                      (substring text line-end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed (min pos (+ line-start (string-length new-line))))
        (echo-message! echo
          (cond ((string-contains new-line "[#A]") "Priority: A")
                ((string-contains new-line "[#B]") "Priority: B")
                ((string-contains new-line "[#C]") "Priority: C")
                (else "Priority: none")))))))

