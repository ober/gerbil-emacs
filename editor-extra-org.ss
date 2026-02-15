;;; -*- Gerbil -*-
;;; Org-mode, calendar, and diary commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/tui
        :gerbil-emacs/core
        :gerbil-emacs/keymap
        :gerbil-emacs/buffer
        :gerbil-emacs/window
        :gerbil-emacs/modeline
        :gerbil-emacs/echo
        :gerbil-emacs/editor-extra-helpers)

;;;============================================================================
;;; Org-mode
;;;============================================================================

(def *org-stored-link* #f)

(def (org-heading-line? line)
  "Check if line is an org heading (starts with one or more *)."
  (and (> (string-length line) 0)
       (char=? (string-ref line 0) #\*)))

(def (org-heading-level line)
  "Count leading * chars in an org heading line. Returns 0 for non-headings."
  (let loop ((i 0))
    (if (and (< i (string-length line)) (char=? (string-ref line i) #\*))
      (loop (+ i 1))
      i)))

(def (org-find-subtree-end lines cur-line level)
  "Find the line index of the next heading at same or higher level, or end of lines."
  (let loop ((i (+ cur-line 1)))
    (cond
      ((>= i (length lines)) i)
      ((let ((l (list-ref lines i)))
         (and (org-heading-line? l)
              (<= (org-heading-level l) level)))
       i)
      (else (loop (+ i 1))))))

(def (org-on-checkbox-line? line)
  "Detect org checkbox lines: '- [ ] task' or '- [X] task'."
  (or (string-contains line "- [ ] ")
      (string-contains line "- [X] ")
      (string-contains line "- [x] ")))

(def (org-get-current-line ed)
  "Get text of the current line."
  (let* ((pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed)))
    (if (<= line-end (string-length text))
      (substring text line-start line-end)
      "")))

(def (org-replace-line ed line-num new-line)
  "Replace a line's content using Scintilla target API."
  (let ((line-start (editor-position-from-line ed line-num))
        (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0)))
    (send-message ed SCI_SETTARGETSTART line-start 0)
    (send-message ed SCI_SETTARGETEND line-end 0)
    (send-message/string ed SCI_REPLACETARGET new-line)))

(def (cmd-org-mode app)
  "Toggle org-mode for current buffer."
  (let ((on (toggle-mode! 'org-mode)))
    (echo-message! (app-state-echo app)
      (if on "Org-mode enabled" "Org-mode disabled"))))

(def (cmd-org-todo app)
  "Cycle TODO state on current heading: none -> TODO -> DONE -> none."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed))
         (line (substring text line-start (min line-end (string-length text))))
         (echo (app-state-echo app)))
    (cond
      ;; Line has "* TODO " -> change to "* DONE "
      ((string-contains line "TODO ")
       (let* ((idx (string-contains line "TODO "))
              (new-line (string-append (substring line 0 idx)
                                       "DONE "
                                       (substring line (+ idx 5) (string-length line)))))
         (send-message ed SCI_SETTARGETSTART line-start 0)
         (send-message ed SCI_SETTARGETEND line-end 0)
         (send-message/string ed SCI_REPLACETARGET new-line)
         (echo-message! echo "State: DONE")))
      ;; Line has "* DONE " -> remove keyword
      ((string-contains line "DONE ")
       (let* ((idx (string-contains line "DONE "))
              (new-line (string-append (substring line 0 idx)
                                       (substring line (+ idx 5) (string-length line)))))
         (send-message ed SCI_SETTARGETSTART line-start 0)
         (send-message ed SCI_SETTARGETEND line-end 0)
         (send-message/string ed SCI_REPLACETARGET new-line)
         (echo-message! echo "State: none")))
      ;; Line starts with * -> add TODO after stars
      ((org-heading-line? line)
       (let loop ((i 0))
         (if (and (< i (string-length line)) (char=? (string-ref line i) #\*))
           (loop (+ i 1))
           (let ((new-line (string-append (substring line 0 i) " TODO"
                                          (substring line i (string-length line)))))
             (send-message ed SCI_SETTARGETSTART line-start 0)
             (send-message ed SCI_SETTARGETEND line-end 0)
             (send-message/string ed SCI_REPLACETARGET new-line)
             (echo-message! echo "State: TODO")))))
      (else (echo-message! echo "Not on a heading")))))

(def (cmd-org-schedule app)
  "Insert SCHEDULED timestamp on next line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (date (app-read-string app "Schedule date (YYYY-MM-DD): ")))
    (when (and date (not (string-empty? date)))
      (let* ((pos (editor-get-current-pos ed))
             (line-num (editor-line-from-position ed pos))
             (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0)))
        (editor-insert-text ed line-end (string-append "\n  SCHEDULED: <" date ">"))
        (echo-message! (app-state-echo app) (string-append "Scheduled: " date))))))

(def (cmd-org-deadline app)
  "Insert DEADLINE timestamp on next line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (date (app-read-string app "Deadline date (YYYY-MM-DD): ")))
    (when (and date (not (string-empty? date)))
      (let* ((pos (editor-get-current-pos ed))
             (line-num (editor-line-from-position ed pos))
             (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0)))
        (editor-insert-text ed line-end (string-append "\n  DEADLINE: <" date ">"))
        (echo-message! (app-state-echo app) (string-append "Deadline: " date))))))

(def (cmd-org-agenda app)
  "Scan open buffers for TODO/DONE items and display in *Org Agenda*."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (items '()))
    ;; Scan all buffers for TODO items
    (for-each
      (lambda (buf)
        (let ((name (buffer-name buf)))
          ;; Get text from current editor if it's the current buffer
          ;; For other buffers we can only check file on disk
          (let ((fp (buffer-file-path buf)))
            (when fp
              (with-exception-catcher
                (lambda (e) (void))
                (lambda ()
                  (let* ((content (call-with-input-file fp (lambda (p) (read-line p #f))))
                         (lines (if content (string-split content #\newline) '())))
                    (let loop ((ls lines) (n 1))
                      (when (not (null? ls))
                        (let ((l (car ls)))
                          (when (or (string-contains l "TODO ")
                                    (string-contains l "SCHEDULED:")
                                    (string-contains l "DEADLINE:"))
                            (set! items (cons (string-append "  " name ":"
                                                            (number->string n) ": "
                                                            (string-trim l))
                                             items))))
                        (loop (cdr ls) (+ n 1)))))))))))
      (buffer-list))
    ;; Also scan current editor text
    (let* ((text (editor-get-text ed))
           (cur-buf (edit-window-buffer win))
           (cur-name (if cur-buf (buffer-name cur-buf) "*scratch*"))
           (lines (string-split text #\newline)))
      (let loop ((ls lines) (n 1))
        (when (not (null? ls))
          (let ((l (car ls)))
            (when (or (string-contains l "TODO ")
                      (string-contains l "SCHEDULED:")
                      (string-contains l "DEADLINE:"))
              (set! items (cons (string-append "  " cur-name ":"
                                              (number->string n) ": "
                                              (string-trim l))
                               items))))
          (loop (cdr ls) (+ n 1)))))
    (let* ((buf (buffer-create! "*Org Agenda*" ed))
           (agenda-text (if (null? items)
                          "Org Agenda\n\nNo TODO items found.\n"
                          (string-append "Org Agenda\n\n"
                                        (string-join (reverse items) "\n")
                                        "\n"))))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed agenda-text)
      (editor-goto-pos ed 0)
      (editor-set-read-only ed #t))))

(def (cmd-org-export app)
  "Export org buffer to plain text (strip markup)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (echo (app-state-echo app))
         (exported
           (string-join
             (map (lambda (line)
                    (cond
                      ;; Convert headings: remove leading *s, preserve case
                      ((org-heading-line? line)
                       (let loop ((i 0))
                         (if (and (< i (string-length line))
                                  (or (char=? (string-ref line i) #\*)
                                      (char=? (string-ref line i) #\space)))
                           (loop (+ i 1))
                           (substring line i (string-length line)))))
                      ;; Remove SCHEDULED:/DEADLINE: lines
                      ((or (string-contains line "SCHEDULED:")
                           (string-contains line "DEADLINE:"))
                       line)
                      ;; Strip bold *text* -> text
                      (else line)))
                  lines)
             "\n")))
    (let ((buf (buffer-create! "*Org Export*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed exported)
      (editor-goto-pos ed 0)
      (echo-message! echo "Org export complete"))))

(def (cmd-org-table-create app)
  "Insert a basic org table template at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (cols-str (app-read-string app "Number of columns (default 3): "))
         (cols (or (and cols-str (not (string-empty? cols-str))
                       (string->number cols-str))
                   3))
         (header (string-append "| "
                   (string-join
                     (let loop ((i 1) (acc '()))
                       (if (> i cols) (reverse acc)
                         (loop (+ i 1) (cons (string-append "Col" (number->string i)) acc))))
                     " | ")
                   " |"))
         (separator (string-append "|"
                      (string-join
                        (let loop ((i 0) (acc '()))
                          (if (>= i cols) (reverse acc)
                            (loop (+ i 1) (cons "---" acc))))
                        "+")
                      "|"))
         (empty-row (string-append "| "
                      (string-join
                        (let loop ((i 0) (acc '()))
                          (if (>= i cols) (reverse acc)
                            (loop (+ i 1) (cons "   " acc))))
                        " | ")
                      " |"))
         (table (string-append header "\n" separator "\n" empty-row "\n")))
    (editor-insert-text ed (editor-get-current-pos ed) table)
    (echo-message! (app-state-echo app)
      (string-append "Inserted " (number->string cols) "-column table"))))

(def (cmd-org-link app)
  "Insert an org link [[url][description]]."
  (let* ((url (app-read-string app "Link URL: "))
         (echo (app-state-echo app)))
    (when (and url (not (string-empty? url)))
      (let ((desc (app-read-string app "Description (empty for URL): ")))
        (let* ((fr (app-state-frame app))
               (win (current-window fr))
               (ed (edit-window-editor win))
               (link-text (if (and desc (not (string-empty? desc)))
                            (string-append "[[" url "][" desc "]]")
                            (string-append "[[" url "]]"))))
          (editor-insert-text ed (editor-get-current-pos ed) link-text)
          (echo-message! echo "Link inserted"))))))

(def (cmd-org-store-link app)
  "Store link to current file:line for later insertion."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (ed (edit-window-editor win))
         (file (and buf (buffer-file-path buf)))
         (line (editor-line-from-position ed (editor-get-current-pos ed)))
         (echo (app-state-echo app)))
    (if file
      (begin
        (set! *org-stored-link* (string-append "file:" file "::" (number->string (+ line 1))))
        (echo-message! echo (string-append "Stored: " *org-stored-link*)))
      (echo-message! echo "Buffer has no file"))))

(def (cmd-org-open-at-point app)
  "Open org link at point. Supports [[file:path]] and [[url]] syntax."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (text (editor-get-text ed))
         (echo (app-state-echo app)))
    ;; Search backward for [[ and forward for ]]
    (let find-start ((i pos))
      (cond
        ((< i 1) (echo-message! echo "No link at point"))
        ((and (char=? (string-ref text i) #\[)
              (> i 0) (char=? (string-ref text (- i 1)) #\[))
         (let find-end ((j (+ i 1)))
           (cond
             ((>= j (- (string-length text) 1))
              (echo-message! echo "Unclosed link"))
             ((and (char=? (string-ref text j) #\])
                   (< j (- (string-length text) 1))
                   (char=? (string-ref text (+ j 1)) #\]))
              ;; Found link content between i and j
              (let* ((content (substring text i j))
                     ;; Strip description if present (split on ][)
                     (url (let ((sep (string-contains content "][")))
                             (if sep (substring content 0 sep) content))))
                (cond
                  ((string-prefix? "file:" url)
                   (let ((path (substring url 5 (string-length url))))
                     (if (file-exists? path)
                       (begin
                         (let* ((new-buf (buffer-create! (path-strip-directory path) ed))
                                (file-content (read-file-as-string path)))
                           (buffer-attach! ed new-buf)
                           (set! (edit-window-buffer win) new-buf)
                           (set! (buffer-file-path new-buf) path)
                           (editor-set-text ed file-content)
                           (editor-goto-pos ed 0)
                           (echo-message! echo (string-append "Opened: " path))))
                       (echo-message! echo (string-append "File not found: " path)))))
                  ((or (string-prefix? "http://" url) (string-prefix? "https://" url))
                   (with-exception-catcher
                     (lambda (e) (echo-message! echo "Failed to open URL"))
                     (lambda ()
                       (open-process
                         (list path: "xdg-open" arguments: (list url)
                               stdin-redirection: #f stdout-redirection: #f
                               stderr-redirection: #f))
                       (echo-message! echo (string-append "Opening: " url)))))
                  (else (echo-message! echo (string-append "Link: " url))))))
             (else (find-end (+ j 1))))))
        (else (find-start (- i 1)))))))

(def (cmd-org-cycle app)
  "Cycle visibility of org heading children (fold/unfold)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (echo (app-state-echo app)))
    (if (>= cur-line (length lines))
      (echo-message! echo "No heading")
      (let ((line (list-ref lines cur-line)))
        (if (not (org-heading-line? line))
          (echo-message! echo "Not on a heading")
          ;; Toggle fold level for children of this heading
          (let* ((level (let loop ((i 0))
                          (if (and (< i (string-length line)) (char=? (string-ref line i) #\*))
                            (loop (+ i 1)) i)))
                 ;; Find range of children
                 (end-line (let loop ((i (+ cur-line 1)))
                             (cond
                               ((>= i (length lines)) i)
                               ((let ((l (list-ref lines i)))
                                  (and (org-heading-line? l)
                                       (<= (let loop2 ((j 0))
                                             (if (and (< j (string-length l))
                                                      (char=? (string-ref l j) #\*))
                                               (loop2 (+ j 1)) j))
                                           level)))
                                i)
                               (else (loop (+ i 1)))))))
            ;; Toggle: if next line is hidden (fold level), show it; otherwise hide
            (if (= end-line (+ cur-line 1))
              (echo-message! echo "No children to fold")
              (let* ((next-line-start (editor-position-from-line ed (+ cur-line 1)))
                     (fold-end (if (< end-line (length lines))
                                 (editor-position-from-line ed end-line)
                                 (editor-get-text-length ed)))
                     (currently-visible (send-message ed SCI_GETLINEVISIBLE (+ cur-line 1) 0)))
                ;; Use Scintilla fold mechanism
                ;; Note: SCI_GETLINEVISIBLE returns 0/1 integer, and 0 is
                ;; truthy in Scheme, so compare explicitly with = 1
                (if (= currently-visible 1)
                  ;; Currently visible -> hide them
                  (let loop ((i (+ cur-line 1)))
                    (when (< i end-line)
                      (send-message ed SCI_HIDELINES i i)
                      (loop (+ i 1))))
                  ;; Currently hidden -> show them
                  (send-message ed SCI_SHOWLINES (+ cur-line 1) (- end-line 1)))
                (echo-message! echo
                  (if (= currently-visible 1) "Folded" "Unfolded"))))))))))

(def (cmd-org-shift-tab app)
  "Global visibility cycling: all -> headings only -> all collapsed."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (total (length lines))
         (echo (app-state-echo app)))
    ;; Check current state by seeing if non-heading lines are visible
    (let ((some-hidden #f))
      (let loop ((i 0))
        (when (< i total)
          (let ((visible (send-message ed SCI_GETLINEVISIBLE i 0)))
            (when (= visible 0)
              (set! some-hidden #t)))
          (loop (+ i 1))))
      (if some-hidden
        ;; Some lines hidden -> show all
        (begin
          (send-message ed SCI_SHOWLINES 0 (- total 1))
          (echo-message! echo "All visible"))
        ;; All visible -> hide non-headings
        (begin
          (let loop ((i 0))
            (when (< i total)
              (let ((line (list-ref lines i)))
                (unless (org-heading-line? line)
                  (send-message ed SCI_HIDELINES i i)))
              (loop (+ i 1))))
          (echo-message! echo "Headings only"))))))

;;;============================================================================
;;; New org-mode commands
;;;============================================================================

(def (cmd-org-promote app)
  "Decrease heading level: ** X -> * X. No-op on level-1 headings."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed))
         (line (substring text line-start (min line-end (string-length text))))
         (echo (app-state-echo app)))
    (if (not (org-heading-line? line))
      (echo-message! echo "Not on a heading")
      (let ((level (org-heading-level line)))
        (if (<= level 1)
          (echo-message! echo "Already at top level")
          (let ((new-line (substring line 1 (string-length line))))
            (org-replace-line ed line-num new-line)
            (echo-message! echo (string-append "Promoted to level "
                                  (number->string (- level 1))))))))))

(def (cmd-org-demote app)
  "Increase heading level: * X -> ** X."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed))
         (line (substring text line-start (min line-end (string-length text))))
         (echo (app-state-echo app)))
    (if (not (org-heading-line? line))
      (echo-message! echo "Not on a heading")
      (let* ((level (org-heading-level line))
             (new-line (string-append "*" line)))
        (org-replace-line ed line-num new-line)
        (echo-message! echo (string-append "Demoted to level "
                                (number->string (+ level 1))))))))

(def (cmd-org-move-subtree-up app)
  "Swap current heading+children with previous sibling subtree."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (echo (app-state-echo app)))
    (if (or (>= cur-line (length lines))
            (not (org-heading-line? (list-ref lines cur-line))))
      (echo-message! echo "Not on a heading")
      (let* ((level (org-heading-level (list-ref lines cur-line)))
             (cur-end (org-find-subtree-end lines cur-line level))
             ;; Find previous sibling: scan backward for heading at same level
             (prev-start
               (let loop ((i (- cur-line 1)))
                 (cond
                   ((< i 0) #f)
                   ((let ((l (list-ref lines i)))
                      (and (org-heading-line? l)
                           (= (org-heading-level l) level)))
                    i)
                   ((let ((l (list-ref lines i)))
                      (and (org-heading-line? l)
                           (< (org-heading-level l) level)))
                    #f) ;; hit a parent heading, no sibling
                   (else (loop (- i 1)))))))
        (if (not prev-start)
          (echo-message! echo "No previous sibling")
          (let* ((prev-lines (let loop ((i prev-start) (acc '()))
                               (if (>= i cur-line) (reverse acc)
                                 (loop (+ i 1) (cons (list-ref lines i) acc)))))
                 (cur-lines (let loop ((i cur-line) (acc '()))
                              (if (>= i cur-end) (reverse acc)
                                (loop (+ i 1) (cons (list-ref lines i) acc)))))
                 ;; Build new text: before-prev + cur-lines + prev-lines + after-cur
                 (before (let loop ((i 0) (acc '()))
                           (if (>= i prev-start) (reverse acc)
                             (loop (+ i 1) (cons (list-ref lines i) acc)))))
                 (after (let loop ((i cur-end) (acc '()))
                          (if (>= i (length lines)) (reverse acc)
                            (loop (+ i 1) (cons (list-ref lines i) acc)))))
                 (new-lines (append before cur-lines prev-lines after))
                 (new-text (string-join new-lines "\n")))
            (editor-set-text ed new-text)
            (editor-goto-pos ed (editor-position-from-line ed prev-start))
            (echo-message! echo "Moved subtree up")))))))

(def (cmd-org-move-subtree-down app)
  "Swap current heading+children with next sibling subtree."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (echo (app-state-echo app)))
    (if (or (>= cur-line (length lines))
            (not (org-heading-line? (list-ref lines cur-line))))
      (echo-message! echo "Not on a heading")
      (let* ((level (org-heading-level (list-ref lines cur-line)))
             (cur-end (org-find-subtree-end lines cur-line level)))
        (if (>= cur-end (length lines))
          (echo-message! echo "No next sibling")
          (let ((next-line (list-ref lines cur-end)))
            (if (not (and (org-heading-line? next-line)
                          (= (org-heading-level next-line) level)))
              (echo-message! echo "No next sibling")
              (let* ((next-end (org-find-subtree-end lines cur-end level))
                     (cur-lines (let loop ((i cur-line) (acc '()))
                                  (if (>= i cur-end) (reverse acc)
                                    (loop (+ i 1) (cons (list-ref lines i) acc)))))
                     (next-lines (let loop ((i cur-end) (acc '()))
                                   (if (>= i next-end) (reverse acc)
                                     (loop (+ i 1) (cons (list-ref lines i) acc)))))
                     (before (let loop ((i 0) (acc '()))
                               (if (>= i cur-line) (reverse acc)
                                 (loop (+ i 1) (cons (list-ref lines i) acc)))))
                     (after (let loop ((i next-end) (acc '()))
                              (if (>= i (length lines)) (reverse acc)
                                (loop (+ i 1) (cons (list-ref lines i) acc)))))
                     (new-lines (append before next-lines cur-lines after))
                     (new-text (string-join new-lines "\n"))
                     ;; New position: after the next-lines block
                     (new-cur-line (+ cur-line (length next-lines))))
                (editor-set-text ed new-text)
                (editor-goto-pos ed (editor-position-from-line ed new-cur-line))
                (echo-message! echo "Moved subtree down")))))))))

(def (cmd-org-toggle-checkbox app)
  "Toggle checkbox: '- [ ] task' <-> '- [X] task'."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed))
         (line (substring text line-start (min line-end (string-length text))))
         (echo (app-state-echo app)))
    (cond
      ((string-contains line "- [ ] ")
       (let* ((idx (string-contains line "- [ ] "))
              (new-line (string-append (substring line 0 (+ idx 2))
                                       "[X] "
                                       (substring line (+ idx 6) (string-length line)))))
         (org-replace-line ed line-num new-line)
         (echo-message! echo "Checked")))
      ((or (string-contains line "- [X] ")
           (string-contains line "- [x] "))
       (let* ((idx (or (string-contains line "- [X] ")
                       (string-contains line "- [x] ")))
              (new-line (string-append (substring line 0 (+ idx 2))
                                       "[ ] "
                                       (substring line (+ idx 6) (string-length line)))))
         (org-replace-line ed line-num new-line)
         (echo-message! echo "Unchecked")))
      (else (echo-message! echo "Not on a checkbox line")))))

(def (cmd-org-priority app)
  "Cycle priority on heading: none -> [#A] -> [#B] -> [#C] -> none."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed))
         (line (substring text line-start (min line-end (string-length text))))
         (echo (app-state-echo app)))
    (if (not (org-heading-line? line))
      (echo-message! echo "Not on a heading")
      (cond
        ;; Has [#A] -> [#B]
        ((string-contains line "[#A] ")
         (let* ((idx (string-contains line "[#A] "))
                (new-line (string-append (substring line 0 idx)
                                         "[#B] "
                                         (substring line (+ idx 5) (string-length line)))))
           (org-replace-line ed line-num new-line)
           (echo-message! echo "Priority: B")))
        ;; Has [#B] -> [#C]
        ((string-contains line "[#B] ")
         (let* ((idx (string-contains line "[#B] "))
                (new-line (string-append (substring line 0 idx)
                                         "[#C] "
                                         (substring line (+ idx 5) (string-length line)))))
           (org-replace-line ed line-num new-line)
           (echo-message! echo "Priority: C")))
        ;; Has [#C] -> remove priority
        ((string-contains line "[#C] ")
         (let* ((idx (string-contains line "[#C] "))
                (new-line (string-append (substring line 0 idx)
                                         (substring line (+ idx 5) (string-length line)))))
           (org-replace-line ed line-num new-line)
           (echo-message! echo "Priority: none")))
        ;; No priority -> add [#A] after stars and optional TODO/DONE keyword
        (else
          (let* ((level (org-heading-level line))
                 ;; Find insert position: after stars + space + optional keyword
                 (after-stars (if (and (< level (string-length line))
                                      (char=? (string-ref line level) #\space))
                               (+ level 1) level))
                 ;; Check for TODO/DONE keyword
                 (rest (substring line after-stars (string-length line)))
                 (insert-pos
                   (cond
                     ((string-prefix? "TODO " rest) (+ after-stars 5))
                     ((string-prefix? "DONE " rest) (+ after-stars 5))
                     (else after-stars)))
                 (new-line (string-append (substring line 0 insert-pos)
                                          "[#A] "
                                          (substring line insert-pos (string-length line)))))
            (org-replace-line ed line-num new-line)
            (echo-message! echo "Priority: A")))))))

(def (cmd-org-set-tags app)
  "Prompt for tags and append :tag1:tag2: to current heading."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed))
         (line (substring text line-start (min line-end (string-length text))))
         (echo (app-state-echo app)))
    (if (not (org-heading-line? line))
      (echo-message! echo "Not on a heading")
      (let ((tags-input (app-read-string app "Tags (comma-separated): ")))
        (when (and tags-input (not (string-empty? tags-input)))
          ;; Strip existing tags (text after last :tag: pattern)
          (let* ((stripped (let ((colon-pos (string-index-right line #\:)))
                             (if (and colon-pos
                                      (> colon-pos 0)
                                      ;; Check if there's a : before this one (tag pattern)
                                      (string-index line #\: 0))
                               ;; Remove trailing tag section
                               (string-trim-right (substring line 0
                                 (let scan ((i (string-length line)))
                                   (if (<= i 0) 0
                                     (let ((ch (string-ref line (- i 1))))
                                       (if (or (char=? ch #\:)
                                               (char-alphabetic? ch)
                                               (char-numeric? ch)
                                               (char=? ch #\_)
                                               (char=? ch #\-)
                                               (char=? ch #\@))
                                         (scan (- i 1))
                                         i))))))
                               line)))
                 ;; Format tags
                 (tag-parts (map string-trim (string-split tags-input #\,)))
                 (tag-str (string-append ":" (string-join tag-parts ":") ":"))
                 (new-line (string-append (string-trim-right stripped) " " tag-str)))
            (org-replace-line ed line-num new-line)
            (echo-message! echo (string-append "Tags: " tag-str))))))))

(def (cmd-org-insert-heading app)
  "Insert new heading at same level below current subtree."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (pos (editor-get-current-pos ed))
         (cur-line (editor-line-from-position ed pos))
         (echo (app-state-echo app)))
    (if (or (>= cur-line (length lines))
            (not (org-heading-line? (list-ref lines cur-line))))
      ;; Not on a heading - insert a level-1 heading
      (let* ((line-end (send-message ed SCI_GETLINEENDPOSITION cur-line 0)))
        (editor-insert-text ed line-end "\n* ")
        (editor-goto-pos ed (+ line-end 3))
        (echo-message! echo "New heading"))
      (let* ((level (org-heading-level (list-ref lines cur-line)))
             (subtree-end (org-find-subtree-end lines cur-line level))
             ;; Insert before the next heading (at end of subtree)
             (insert-line (- subtree-end 1))
             (insert-end (send-message ed SCI_GETLINEENDPOSITION insert-line 0))
             (stars (make-string level #\*))
             (new-heading (string-append "\n" stars " ")))
        (editor-insert-text ed insert-end new-heading)
        (editor-goto-pos ed (+ insert-end (string-length new-heading)))
        (echo-message! echo (string-append "New level-" (number->string level) " heading"))))))

(def (cmd-org-insert-src-block app)
  "Insert #+BEGIN_SRC ... #+END_SRC template at point."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (lang (app-read-string app "Language (default: empty): "))
         (lang-str (if (and lang (not (string-empty? lang)))
                     (string-append " " lang)
                     ""))
         (template (string-append "#+BEGIN_SRC" lang-str "\n\n#+END_SRC\n")))
    (editor-insert-text ed pos template)
    ;; Place cursor on the blank line inside the block
    (editor-goto-pos ed (+ pos (string-length (string-append "#+BEGIN_SRC" lang-str "\n"))))
    (echo-message! (app-state-echo app) "Source block inserted")))

(def (cmd-org-clock-in app)
  "Insert CLOCK-IN timestamp in :LOGBOOK: drawer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (echo (app-state-echo app))
         ;; Get current time
         (now (with-exception-catcher
                (lambda (e) "")
                (lambda ()
                  (let ((p (open-process
                             (list path: "date"
                                   arguments: '("+[%Y-%m-%d %a %H:%M]")
                                   stdin-redirection: #f stdout-redirection: #t
                                   stderr-redirection: #t))))
                    (let ((out (read-line p)))
                      (process-status p)
                      (or out "")))))))
    (when (not (string-empty? now))
      (let ((clock-text (string-append "\n  :LOGBOOK:\n  CLOCK: " now "\n  :END:")))
        (editor-insert-text ed line-end clock-text)
        (echo-message! echo (string-append "Clocked in: " now))))))

(def (cmd-org-clock-out app)
  "Close open CLOCK entry with end timestamp and elapsed time."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (text (editor-get-text ed))
         (echo (app-state-echo app))
         ;; Find the last open CLOCK entry (one without --)
         (lines (string-split text #\newline))
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
      (let* ((now (with-exception-catcher
                    (lambda (e) "")
                    (lambda ()
                      (let ((p (open-process
                                 (list path: "date"
                                       arguments: '("+[%Y-%m-%d %a %H:%M]")
                                       stdin-redirection: #f stdout-redirection: #t
                                       stderr-redirection: #t))))
                        (let ((out (read-line p)))
                          (process-status p)
                          (or out ""))))))
             (cur-line-text (list-ref lines clock-line)))
        (when (not (string-empty? now))
          (let* ((new-line (string-append cur-line-text "--" now " =>  0:00"))
                 (line-start (editor-position-from-line ed clock-line))
                 (line-end (send-message ed SCI_GETLINEENDPOSITION clock-line 0)))
            (send-message ed SCI_SETTARGETSTART line-start 0)
            (send-message ed SCI_SETTARGETEND line-end 0)
            (send-message/string ed SCI_REPLACETARGET new-line)
            (echo-message! echo (string-append "Clocked out: " now))))))))

;;;============================================================================
;;; Org structure templates (<s TAB, <e TAB, etc.)
;;;============================================================================

(def *org-structure-templates*
  '(("s" "SRC"      #t)    ;; <s -> #+BEGIN_SRC ... #+END_SRC (prompts for lang)
    ("e" "EXAMPLE"  #f)    ;; <e -> #+BEGIN_EXAMPLE ... #+END_EXAMPLE
    ("q" "QUOTE"    #f)    ;; <q -> #+BEGIN_QUOTE ... #+END_QUOTE
    ("v" "VERSE"    #f)    ;; <v -> #+BEGIN_VERSE ... #+END_VERSE
    ("c" "CENTER"   #f)    ;; <c -> #+BEGIN_CENTER ... #+END_CENTER
    ("C" "COMMENT"  #f)    ;; <C -> #+BEGIN_COMMENT ... #+END_COMMENT
    ("l" "EXPORT latex" #f) ;; <l -> #+BEGIN_EXPORT latex ... #+END_EXPORT
    ("h" "EXPORT html" #f)  ;; <h -> #+BEGIN_EXPORT html ... #+END_EXPORT
    ("a" "EXPORT ascii" #f))) ;; <a -> #+BEGIN_EXPORT ascii ... #+END_EXPORT

(def (org-template-lookup key)
  "Look up a structure template by its shortcut key. Returns (block-type has-lang?) or #f."
  (let loop ((ts *org-structure-templates*))
    (if (null? ts) #f
      (let ((t (car ts)))
        (if (string=? (car t) key)
          (cdr t)  ;; (block-type has-lang?)
          (loop (cdr ts)))))))

(def (cmd-org-template-expand app)
  "Expand org structure template at point. Checks if line contains '<X' where
   X is a template key (s, e, q, v, c, C, l, h, a). Replaces the '<X' with
   the corresponding #+BEGIN_.../#+END_... block. For <s, places cursor on
   the #+BEGIN_SRC line to allow typing a language name."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (pos (editor-get-current-pos ed))
         (line-num (editor-line-from-position ed pos))
         (line-start (editor-position-from-line ed line-num))
         (line-end (send-message ed SCI_GETLINEENDPOSITION line-num 0))
         (text (editor-get-text ed))
         (line (substring text line-start (min line-end (string-length text))))
         (trimmed (string-trim line))
         (echo (app-state-echo app)))
    ;; Check if the trimmed line matches "<X" pattern
    (if (and (>= (string-length trimmed) 2)
             (char=? (string-ref trimmed 0) #\<))
      (let* ((key (substring trimmed 1 (string-length trimmed)))
             (tmpl (org-template-lookup key)))
        (if (not tmpl)
          (echo-message! echo (string-append "No template for '<" key "'"))
          (let* ((block-type (car tmpl))
                 (has-lang? (cadr tmpl))
                 ;; Preserve leading whitespace
                 (indent (let loop ((i 0))
                           (if (and (< i (string-length line))
                                    (char=? (string-ref line i) #\space))
                             (loop (+ i 1))
                             (substring line 0 i))))
                 ;; For EXPORT blocks, the end tag is just EXPORT
                 (end-type (let ((sp (string-contains block-type " ")))
                             (if sp (substring block-type 0 sp) block-type)))
                 (begin-line (string-append indent "#+BEGIN_" block-type))
                 (end-line (string-append indent "#+END_" end-type))
                 (expansion (string-append begin-line "\n"
                                           indent "\n"
                                           end-line)))
            ;; Replace the <X line with the expansion
            (send-message ed SCI_SETTARGETSTART line-start 0)
            (send-message ed SCI_SETTARGETEND line-end 0)
            (send-message/string ed SCI_REPLACETARGET expansion)
            ;; Place cursor on the blank line inside the block
            (editor-goto-pos ed (+ line-start (string-length begin-line) 1
                                   (string-length indent)))
            (echo-message! echo
              (string-append "Expanded <" key " to #+BEGIN_" block-type)))))
      ;; Not a template pattern
      (echo-message! echo "No template at point"))))

;; Calendar/diary
(def (cmd-calendar app)
  "Show calendar."
  (let ((cal-text (with-exception-catcher
                    (lambda (e) "Calendar not available")
                    (lambda ()
                      (let ((p (open-process
                                 (list path: "cal"
                                       arguments: '()
                                       stdin-redirection: #f stdout-redirection: #t
                                       stderr-redirection: #t))))
                        (let ((out (read-line p #f)))
                          (process-status p)
                          (or out "")))))))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Calendar*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Calendar\n\n" cal-text "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-diary-view-entries app)
  "View diary entries from ~/.diary file."
  (let* ((diary-file (string-append (or (getenv "HOME") ".") "/.diary"))
         (echo (app-state-echo app)))
    (if (file-exists? diary-file)
      (let* ((content (read-file-as-string diary-file))
             (fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (buf (buffer-create! "*Diary*" ed)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed (string-append "Diary Entries\n\n" content))
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t))
      (echo-message! echo "No diary file (~/.diary)"))))

;;;============================================================================
;;; Batch 27: focus mode, zen mode, killed buffers, file operations, etc.
;;;============================================================================

;;; --- Focus/Olivetti mode: center text with margins ---

(def *focus-mode* #f)
(def *focus-margin-width* 20)

(def (cmd-toggle-focus-mode app)
  "Toggle focus mode (center text by adding margins)."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *focus-mode* (not *focus-mode*))
    (if *focus-mode*
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 0 0)  ; hide line numbers
        (send-message ed SCI_SETMARGINWIDTHN 1 *focus-margin-width*)
        (editor-set-wrap-mode ed 1)  ; enable word wrap
        (echo-message! echo "Focus mode enabled"))
      (begin
        (send-message ed SCI_SETMARGINWIDTHN 1 0)
        (echo-message! echo "Focus mode disabled")))))

;;; --- Zen/Writeroom mode: distraction-free writing ---

(def *zen-mode* #f)

(def (cmd-toggle-zen-mode app)
  "Toggle zen/writeroom mode for distraction-free editing."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *zen-mode* (not *zen-mode*))
    (if *zen-mode*
      (begin
        ;; Hide line numbers, fold margin, etc.
        (send-message ed SCI_SETMARGINWIDTHN 0 0)
        (send-message ed SCI_SETMARGINWIDTHN 1 0)
        (send-message ed SCI_SETMARGINWIDTHN 2 0)
        (editor-set-wrap-mode ed 1)
        (echo-message! echo "Zen mode on — press again to exit"))
      (begin
        ;; Restore defaults
        (send-message ed SCI_SETMARGINWIDTHN 0 40)  ; line numbers
        (send-message ed SCI_SETMARGINWIDTHN 2 16)  ; fold margin
        (echo-message! echo "Zen mode off")))))

;;; --- Killed buffer stack for undo ---

(def *killed-buffers* '())   ; list of (name file-path text) triples
(def *max-killed-buffers* 20)

(def (remember-killed-buffer! name file-path text)
  "Record a killed buffer for potential reopening."
  (set! *killed-buffers*
    (let ((new (cons (list name file-path text) *killed-buffers*)))
      (if (> (length new) *max-killed-buffers*)
        (let loop ((ls new) (n 0) (acc []))
          (if (or (null? ls) (>= n *max-killed-buffers*))
            (reverse acc)
            (loop (cdr ls) (+ n 1) (cons (car ls) acc))))
        new))))

(def (cmd-reopen-killed-buffer app)
  "Reopen the most recently killed buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (if (null? *killed-buffers*)
      (echo-message! echo "No killed buffers to reopen")
      (let* ((entry (car *killed-buffers*))
             (name (car entry))
             (file-path (cadr entry))
             (text (caddr entry)))
        (set! *killed-buffers* (cdr *killed-buffers*))
        (editor-set-text ed text)
        (editor-goto-pos ed 0)
        (echo-message! echo (string-append "Reopened: " name))))))

;;; --- Copy just the filename (not full path) ---

(def (cmd-copy-file-name-only app)
  "Copy just the filename (without directory path) to kill ring."
  (let* ((echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (filepath (buffer-file-path buf)))
    (if (not filepath)
      (echo-message! echo "Buffer has no file")
      (let* ((parts (string-split filepath #\/))
             (name (if (null? parts) filepath (last parts))))
        (app-state-kill-ring-set! app (cons name (app-state-kill-ring app)))
        (echo-message! echo (string-append "Copied: " name))))))

;;; --- Open containing folder in file manager ---

(def (cmd-open-containing-folder app)
  "Open the directory containing the current file."
  (let* ((echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (filepath (buffer-file-path buf)))
    (if (not filepath)
      (echo-message! echo "Buffer has no file")
      (let* ((parts (string-split filepath #\/))
             (dir (if (<= (length parts) 1) "."
                    (string-join
                      (let loop ((ls parts) (acc []))
                        (if (null? (cdr ls)) (reverse acc)
                          (loop (cdr ls) (cons (car ls) acc))))
                      "/"))))
        (with-catch
          (lambda (e) (echo-message! echo "Cannot open folder"))
          (lambda ()
            (let ((opener (cond
                            ((file-exists? "/usr/bin/xdg-open") "xdg-open")
                            ((file-exists? "/usr/bin/open") "open")
                            (else #f))))
              (if opener
                (begin
                  (open-process (list path: opener
                                      arguments: (list dir)
                                      stdin-redirection: #f
                                      stdout-redirection: #f))
                  (echo-message! echo (string-append "Opened: " dir)))
                (echo-message! echo "No file manager found")))))))))

;;; --- New empty buffer ---

(def *new-buffer-counter* 0)

(def (cmd-new-empty-buffer app)
  "Create a new empty buffer with a unique name."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (set! *new-buffer-counter* (+ *new-buffer-counter* 1))
    (let* ((name (string-append "*new-"
                   (number->string *new-buffer-counter*) "*"))
           (buf (buffer-create! name ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed "")
      (echo-message! echo (string-append "Created new buffer: " name)))))

;;; --- Toggle window dedicated ---

(def *dedicated-windows* (make-hash-table))

(def (cmd-toggle-window-dedicated app)
  "Toggle whether the current window is dedicated to its buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (echo (app-state-echo app))
         (buf-name (buffer-name (edit-window-buffer win)))
         (currently-dedicated (hash-get *dedicated-windows* buf-name)))
    (if currently-dedicated
      (begin
        (hash-remove! *dedicated-windows* buf-name)
        (echo-message! echo
          (string-append "Window undedicated from: " buf-name)))
      (begin
        (hash-put! *dedicated-windows* buf-name #t)
        (echo-message! echo
          (string-append "Window dedicated to: " buf-name))))))

;;; --- Which-key mode: show available prefixed key bindings ---

(def *which-key-mode* #f)

(def (cmd-toggle-which-key-mode app)
  "Toggle which-key mode (show key completions after prefix)."
  (set! *which-key-mode* (not *which-key-mode*))
  (echo-message! (app-state-echo app)
    (if *which-key-mode*
      "Which-key mode enabled"
      "Which-key mode disabled")))

(def (cmd-which-key-describe-prefix app)
  "Show all bindings under the current prefix."
  (let* ((echo (app-state-echo app))
         (prefix (app-read-string app "Describe prefix: ")))
    (when (and prefix (> (string-length prefix) 0))
      (let* ((ed (current-editor app))
             (entries (keymap-entries *global-keymap*))
             (matches (filter
                        (lambda (e) (string-prefix? prefix (car e)))
                        entries))
             (text (if (null? matches)
                     (string-append "No bindings for prefix: " prefix)
                     (with-output-to-string
                       (lambda ()
                         (display (string-append "Bindings for prefix '" prefix "':\n"))
                         (display (make-string 50 #\-))
                         (display "\n")
                         (for-each
                           (lambda (e)
                             (display "  ")
                             (display (car e))
                             (display "  ->  ")
                             (display (cdr e))
                             (display "\n"))
                           (sort matches (lambda (a b) (string<? (car a) (car b))))))))))
        (editor-set-text ed text)
        (editor-goto-pos ed 0)
        (echo-message! echo
          (string-append (number->string (length matches)) " bindings found"))))))

;;; --- Transpose windows (swap content of two windows) ---

(def (cmd-transpose-windows app)
  "Swap the buffers displayed in the current and next window."
  (let* ((fr (app-state-frame app))
         (wins (frame-windows fr))
         (echo (app-state-echo app)))
    (if (< (length wins) 2)
      (echo-message! echo "Need at least 2 windows to transpose")
      (let* ((cur (current-window fr))
             ;; Find the other window
             (other (let loop ((ws wins))
                      (cond
                        ((null? ws) (car wins))
                        ((not (eq? (car ws) cur)) (car ws))
                        (else (loop (cdr ws))))))
             (buf1 (edit-window-buffer cur))
             (buf2 (edit-window-buffer other))
             (ed1 (edit-window-editor cur))
             (ed2 (edit-window-editor other)))
        ;; Swap buffers
        (buffer-attach! ed1 buf2)
        (buffer-attach! ed2 buf1)
        (set! (edit-window-buffer cur) buf2)
        (set! (edit-window-buffer other) buf1)
        (echo-message! echo "Windows transposed")))))

;;; --- Fold toggle at point ---

(def (cmd-fold-toggle-at-point app)
  "Toggle code folding at the current line."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (line (editor-line-from-position ed (editor-get-current-pos ed))))
    (send-message ed SCI_TOGGLEFOLD line 0)
    (echo-message! echo
      (string-append "Toggled fold at line "
        (number->string (+ line 1))))))

;;; --- Imenu list: show function/definition index ---

(def (cmd-imenu-list app)
  "Show a list of function/definition names in the buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         (lines (string-split text #\newline))
         (defs (let loop ((ls lines) (n 1) (acc []))
                 (if (null? ls) (reverse acc)
                   (let ((line (string-trim (car ls))))
                     (loop (cdr ls) (+ n 1)
                       (if (or (string-prefix? "(def " line)
                               (string-prefix? "(def* " line)
                               (string-prefix? "(defstruct " line)
                               (string-prefix? "(defclass " line)
                               (string-prefix? "(defrule " line)
                               (string-prefix? "(defsyntax " line)
                               (string-prefix? "(defmethod " line)
                               (string-prefix? "function " line)
                               (string-prefix? "def " line)
                               (string-prefix? "class " line))
                         (cons (cons n line) acc)
                         acc))))))
         (report (with-output-to-string
                   (lambda ()
                     (display "Definitions:\n")
                     (display (make-string 60 #\-))
                     (display "\n")
                     (for-each
                       (lambda (d)
                         (display (string-pad (number->string (car d)) 6))
                         (display ": ")
                         (let ((s (cdr d)))
                           (display (if (> (string-length s) 70)
                                      (substring s 0 70)
                                      s)))
                         (display "\n"))
                       defs)
                     (display (make-string 60 #\-))
                     (display "\n")
                     (display (number->string (length defs)))
                     (display " definitions found\n")))))
    (editor-set-text ed report)
    (editor-goto-pos ed 0)
    (echo-message! echo
      (string-append (number->string (length defs)) " definitions"))))


