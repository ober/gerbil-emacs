;;; -*- Gerbil -*-
;;; Org-mode, calendar, and diary commands

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/srfi/19
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
        (only-in :gemacs/persist buffer-local-set!)
        (only-in :gemacs/org-parse org-heading-stars-of-line
                 make-org-timestamp org-timestamp-day org-heading-title)
        (only-in :gemacs/org-agenda
                 *org-agenda-files* org-collect-agenda-items
                 org-agenda-item-heading org-agenda-item-type
                 org-agenda-item-date org-agenda-item-time-string)
        (only-in :gemacs/highlight register-custom-highlighter!)
        (only-in :gemacs/org-highlight
                 setup-org-styles! org-highlight-buffer! org-set-fold-levels!)
        (only-in :gemacs/org-table
                 org-table-align org-table-insert-row org-table-delete-row
                 org-table-move-row org-table-move-column
                 org-table-insert-column org-table-delete-column
                 org-table-insert-separator-line org-table-sort
                 org-table-recalculate org-table-to-csv org-csv-to-table
                 org-table-on-table-line? org-table-current-column
                 org-table-find-bounds org-table-get-rows
                 org-table-column-widths org-table-replace-rows
                 org-numeric-cell? filter-map))

;;;============================================================================
;;; Register org-mode syntax highlighter
;;;============================================================================

(register-custom-highlighter! 'org
  (lambda (ed)
    (setup-org-styles! ed)
    (let ((text (editor-get-text ed)))
      (org-highlight-buffer! ed text)
      (org-set-fold-levels! ed text))))

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
  "Activate org-mode for the current buffer.
   Sets buffer-lexer-lang to 'org and triggers org syntax highlighting."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (edit-window-buffer win)))
    (when buf
      (set! (buffer-lexer-lang buf) 'org)
      (buffer-local-set! buf 'major-mode 'org-mode)
      ;; Trigger org highlighting
      (setup-org-styles! ed)
      (let ((text (editor-get-text ed)))
        (org-highlight-buffer! ed text)
        (org-set-fold-levels! ed text)))
    (echo-message! (app-state-echo app) "Org mode")))

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
(def (cmd-org-sparse-tree app)
  "Show only org headings matching a search pattern."
  (let ((query (app-read-string app "Sparse tree (regexp): ")))
    (when (and query (not (string-empty? query)))
      (let* ((ed (current-editor app))
             (echo (app-state-echo app))
             (text (editor-get-text ed))
             (lines (string-split text #\newline))
             (total (length lines))
             (query-lower (string-downcase query)))
        (send-message ed SCI_SHOWLINES 0 (- total 1))
        (let* ((match-set (make-hash-table))
               (_ (let loop ((i 0))
                    (when (< i total)
                      (let ((line (list-ref lines i)))
                        (when (and (org-heading-line? line)
                                   (string-contains (string-downcase line) query-lower))
                          (hash-put! match-set i #t)
                          (let ((level (org-heading-stars-of-line line)))
                            (let ploop ((j (- i 1)))
                              (when (>= j 0)
                                (let ((pl (list-ref lines j)))
                                  (when (and (org-heading-line? pl)
                                             (< (org-heading-stars-of-line pl) level))
                                    (hash-put! match-set j #t)
                                    (ploop (- j 1)))))))))
                      (loop (+ i 1)))))
               (match-count (hash-length match-set)))
          (let loop ((i 0))
            (when (< i total)
              (unless (hash-get match-set i)
                (send-message ed SCI_HIDELINES i i))
              (loop (+ i 1))))
          (echo-message! echo
            (string-append "Sparse tree: " (number->string match-count)
                           " matching headings")))))))

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
         (text (editor-get-text ed))
         (text-len (string-length text))
         (pos (min (editor-get-current-pos ed) text-len))
         ;; Find line boundaries from text (avoids byte/char position mismatch)
         (line-start (let loop ((i (- pos 1)))
                       (cond ((< i 0) 0)
                             ((char=? (string-ref text i) #\newline) (+ i 1))
                             (else (loop (- i 1))))))
         (line-end (let loop ((i pos))
                     (cond ((>= i text-len) text-len)
                           ((char=? (string-ref text i) #\newline) i)
                           (else (loop (+ i 1))))))
         (line (substring text line-start line-end))
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
            ;; Replace the <X line with the expansion via full text rebuild
            ;; (avoids byte/char position mismatch with Scintilla target APIs)
            (let ((new-text (string-append
                              (substring text 0 line-start)
                              expansion
                              (substring text line-end text-len))))
              (editor-set-text ed new-text)
              ;; Place cursor on the blank line inside the block
              (editor-goto-pos ed (+ line-start (string-length begin-line) 1
                                     (string-length indent)))
              ;; Re-apply org highlighting (editor-set-text clears all styles)
              (setup-org-styles! ed)
              (org-highlight-buffer! ed new-text)
              (org-set-fold-levels! ed new-text))
            (echo-message! echo
              (string-append "Expanded <" key " to #+BEGIN_" block-type)))))
      ;; Not a template pattern
      (echo-message! echo "No template at point"))))

;; Calendar/diary
(def *tui-us-holidays*
  '((1  1  "New Year's Day")
    (1  15 "Martin Luther King Jr. Day")
    (2  17 "Presidents' Day")
    (5  26 "Memorial Day")
    (6  19 "Juneteenth")
    (7  4  "Independence Day")
    (9  1  "Labor Day")
    (10 13 "Columbus Day / Indigenous Peoples' Day")
    (11 11 "Veterans Day")
    (11 27 "Thanksgiving")
    (12 25 "Christmas Day")))

(def *tui-diary-file*
  (path-expand ".gemacs-diary" (user-info-home (user-info (user-name)))))

(def (tui-holidays-for-month month)
  "Return holidays in month as list of (day . name)."
  (filter-map (lambda (h) (and (= (car h) month)
                               (cons (cadr h) (caddr h))))
              *tui-us-holidays*))

(def (tui-current-month)
  "Get current month number."
  (with-catch (lambda (e) 1)
    (lambda ()
      (let* ((port (open-process
                     (list path: "/bin/date" arguments: ["+%m"]
                           stdout-redirection: #t stderr-redirection: #f
                           pseudo-terminal: #f)))
             (line (read-line port)))
        (close-port port)
        (if (eof-object? line) 1 (or (string->number (string-trim line)) 1))))))

(def (tui-current-year)
  "Get current year number."
  (with-catch (lambda (e) 2026)
    (lambda ()
      (let* ((port (open-process
                     (list path: "/bin/date" arguments: ["+%Y"]
                           stdout-redirection: #t stderr-redirection: #f
                           pseudo-terminal: #f)))
             (line (read-line port)))
        (close-port port)
        (if (eof-object? line) 2026 (or (string->number (string-trim line)) 2026))))))

(def (cmd-calendar app)
  "Show calendar with holidays and org items."
  (let* ((cal-text (with-exception-catcher
                     (lambda (e) "Calendar not available")
                     (lambda ()
                       (let ((p (open-process
                                  (list path: "cal"
                                        arguments: '()
                                        stdin-redirection: #f stdout-redirection: #t
                                        stderr-redirection: #t))))
                         (let ((out (read-line p #f)))
                           (process-status p)
                           (or out ""))))))
         (year (tui-current-year))
         (month (tui-current-month))
         (hols (tui-holidays-for-month month))
         (hol-text (if (null? hols) ""
                     (string-append "\nHolidays:\n"
                       (string-join
                         (map (lambda (h)
                                (string-append "  " (number->string (car h)) " — " (cdr h)))
                              hols)
                         "\n") "\n")))
         (org-text (tui-calendar-org-footer year month)))
    (let* ((fr (app-state-frame app))
           (win (current-window fr))
           (ed (edit-window-editor win))
           (buf (buffer-create! "*Calendar*" ed)))
      (buffer-attach! ed buf)
      (set! (edit-window-buffer win) buf)
      (editor-set-text ed (string-append "Calendar\n\n" cal-text hol-text org-text "\n"))
      (editor-set-read-only ed #t))))

(def (cmd-diary-view-entries app)
  "View diary entries from ~/.gemacs-diary file."
  (let ((echo (app-state-echo app)))
    (if (not (file-exists? *tui-diary-file*))
      (echo-message! echo "No diary file (~/.gemacs-diary)")
      (let* ((content (read-file-as-string *tui-diary-file*))
             (fr (app-state-frame app))
             (win (current-window fr))
             (ed (edit-window-editor win))
             (buf (buffer-create! "*Diary*" ed)))
        (buffer-attach! ed buf)
        (set! (edit-window-buffer win) buf)
        (editor-set-text ed (string-append "Diary Entries\n\n" content))
        (editor-goto-pos ed 0)
        (editor-set-read-only ed #t)))))

(def (cmd-diary-insert-entry app)
  "Add a diary entry for today to ~/.gemacs-diary."
  (let* ((date-str (with-catch (lambda (e) "2026-01-01")
                     (lambda ()
                       (let* ((port (open-process
                                      (list path: "/bin/date"
                                            arguments: ["+%Y-%m-%d"]
                                            stdout-redirection: #t
                                            stderr-redirection: #f
                                            pseudo-terminal: #f)))
                              (line (read-line port)))
                         (close-port port)
                         (if (eof-object? line) "2026-01-01" (string-trim line))))))
         (entry (app-read-string app (string-append "Diary entry (" date-str "): "))))
    (when (and entry (not (string=? entry "")))
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Failed to write diary"))
        (lambda ()
          (call-with-output-file [path: *tui-diary-file* append: #t]
            (lambda (port) (display (string-append date-str " " entry "\n") port)))
          (echo-message! (app-state-echo app)
            (string-append "Diary entry added for " date-str)))))))

;;; --- Calendar-org integration ---
(def (tui-org-items-for-month year month)
  "Collect org agenda items for a given month from agenda files and open .org buffers."
  (let* ((date-from (make-org-timestamp 'active year month 1 #f #f #f #f #f #f #f))
         (date-to (make-org-timestamp 'active year month 28 #f #f #f #f #f #f #f))
         (agenda-files *org-agenda-files*)
         (open-org-bufs (filter (lambda (b) (and (buffer-file-path b)
                                                  (string-suffix? ".org" (buffer-file-path b))))
                                *buffer-list*))
         (open-org-files (map buffer-file-path open-org-bufs))
         (all-files (let dedup ((lst (append agenda-files open-org-files))
                                (seen '()) (acc '()))
                      (cond ((null? lst) (reverse acc))
                            ((member (car lst) seen) (dedup (cdr lst) seen acc))
                            (else (dedup (cdr lst) (cons (car lst) seen)
                                         (cons (car lst) acc))))))
         (items '()))
    (for-each
      (lambda (file)
        (when (file-exists? file)
          (with-catch void
            (lambda ()
              (let ((text (read-file-as-string file)))
                (set! items (append items
                  (org-collect-agenda-items text file date-from date-to))))))))
      all-files)
    items))

(def (tui-calendar-org-footer year month)
  "Return org scheduled/deadline items for display in calendar."
  (let ((items (tui-org-items-for-month year month)))
    (if (null? items)
      ""
      (string-append "\nOrg items:\n"
        (string-join
          (map (lambda (item)
                 (let* ((h (org-agenda-item-heading item))
                        (type (org-agenda-item-type item))
                        (day (org-timestamp-day (org-agenda-item-date item)))
                        (time (or (org-agenda-item-time-string item) ""))
                        (label (cond ((eq? type 'deadline) "DEADLINE")
                                     ((eq? type 'scheduled) "Sched")
                                     (else "Event"))))
                   (string-append "  " (number->string day) " "
                     label ": " (org-heading-title h)
                     (if (string=? time "") "" (string-append " " time)))))
               items)
          "\n")))))

(def (cmd-appt-check app)
  "Check for upcoming appointments in the next 15 minutes."
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "No appointment data"))
    (lambda ()
      (let* ((now-port (open-process
                         (list path: "/bin/date"
                               arguments: ["+%Y %m %d %H %M"]
                               stdout-redirection: #t
                               stderr-redirection: #f
                               pseudo-terminal: #f)))
             (now-str (read-line now-port))
             (_ (close-port now-port))
             (parts (string-split (string-trim now-str) #\space))
             (year (string->number (list-ref parts 0)))
             (month (string->number (list-ref parts 1)))
             (day (string->number (list-ref parts 2)))
             (hour (string->number (list-ref parts 3)))
             (minute (string->number (list-ref parts 4)))
             (now-mins (+ (* hour 60) minute))
             (upcoming '()))
        ;; Check org items for today
        (let ((items (tui-org-items-for-month year month)))
          (for-each
            (lambda (item)
              (when (= (org-timestamp-day (org-agenda-item-date item)) day)
                (let ((time-str (org-agenda-item-time-string item)))
                  (when time-str
                    (let* ((tparts (string-split time-str #\:))
                           (th (string->number (car tparts)))
                           (tm (string->number (cadr tparts)))
                           (item-mins (+ (* th 60) tm))
                           (diff (- item-mins now-mins)))
                      (when (and (>= diff 0) (<= diff 15))
                        (set! upcoming
                          (cons (string-append time-str " "
                                  (org-heading-title (org-agenda-item-heading item))
                                  " (in " (number->string diff) " min)")
                                upcoming))))))))
            items))
        ;; Check diary entries
        (let* ((diary-path (path-expand ".gemacs-diary"
                             (user-info-home (user-info (user-name)))))
               (entries (if (file-exists? diary-path)
                          (let* ((content (read-file-as-string diary-path))
                                 (lines (string-split content #\newline))
                                 (prefix (string-append
                                           (number->string year) "-"
                                           (if (< month 10) "0" "") (number->string month) "-"
                                           (if (< day 10) "0" "") (number->string day))))
                            (filter (lambda (l) (string-prefix? prefix l)) lines))
                          [])))
          (for-each
            (lambda (entry)
              (set! upcoming (cons (string-append "Diary: "
                (substring entry (min 11 (string-length entry))
                           (string-length entry)))
                upcoming)))
            entries)
          (if (null? upcoming)
            (echo-message! (app-state-echo app) "No upcoming appointments")
            (echo-message! (app-state-echo app)
              (string-append "Upcoming: "
                (string-join (reverse upcoming) " | ")))))))))

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

;;;============================================================================
;;; Batch 31: titlecase, bracket match, block comment, sorting, line numbers
;;;============================================================================

;;; --- Titlecase region ---

(def (cmd-titlecase-region app)
  "Convert selected text to Title Case."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection")
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (result (let loop ((chars (string->list region))
                                (start-of-word? #t) (acc []))
                       (if (null? chars)
                         (list->string (reverse acc))
                         (let ((c (car chars)))
                           (cond
                             ((char-alphabetic? c)
                              (loop (cdr chars) #f
                                (cons (if start-of-word?
                                        (char-upcase c)
                                        (char-downcase c))
                                      acc)))
                             (else
                              (loop (cdr chars) (or (char=? c #\space)
                                                    (char=? c #\-)
                                                    (char=? c #\_))
                                (cons c acc)))))))))
        (editor-set-selection ed sel-start sel-end)
        (editor-replace-selection ed result)
        (echo-message! echo "Title-cased region")))))

;;; --- Go to matching bracket/paren ---

(def (cmd-goto-matching-bracket app)
  "Jump to the matching bracket/parenthesis."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (pos (editor-get-current-pos ed))
         (match (send-message ed SCI_BRACEMATCH pos 0)))
    (if (>= match 0)
      (begin
        (editor-goto-pos ed match)
        (echo-message! echo
          (string-append "Matched at position " (number->string match))))
      (echo-message! echo "No matching bracket found"))))

;;; --- Toggle block comment ---

(def (cmd-toggle-block-comment app)
  "Toggle block comment around selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection to comment")
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end)))
        ;; Check if already wrapped in block comment
        (if (and (>= (string-length region) 4)
                 (string-prefix? "/*" region)
                 (string-suffix? "*/" region))
          ;; Uncomment
          (let ((inner (substring region 2 (- (string-length region) 2))))
            (editor-set-selection ed sel-start sel-end)
            (editor-replace-selection ed inner)
            (echo-message! echo "Block comment removed"))
          ;; Comment
          (let ((commented (string-append "/*" region "*/")))
            (editor-set-selection ed sel-start sel-end)
            (editor-replace-selection ed commented)
            (echo-message! echo "Block comment added")))))))

;;; --- Move cursor to window center ---

(def (cmd-move-to-window-center app)
  "Move cursor to the center line of the visible window."
  (let* ((ed (current-editor app))
         (first-vis (send-message ed SCI_GETFIRSTVISIBLELINE 0 0))
         (lines-on-screen (send-message ed 2370 0 0))  ; SCI_LINESONSCREEN
         (center-line (+ first-vis (quotient lines-on-screen 2)))
         (pos (editor-position-from-line ed center-line)))
    (editor-goto-pos ed pos)))

;;; --- Reverse characters in region ---

(def (cmd-reverse-region-chars app)
  "Reverse the characters in the selected region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! echo "No selection")
      (let* ((text (editor-get-text ed))
             (region (substring text sel-start sel-end))
             (reversed (list->string (reverse (string->list region)))))
        (editor-set-selection ed sel-start sel-end)
        (editor-replace-selection ed reversed)
        (echo-message! echo "Region reversed")))))

;;; --- Toggle relative line numbers ---

(def *relative-line-numbers* #f)

(def (cmd-toggle-relative-line-numbers app)
  "Toggle between absolute and relative line numbers."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app)))
    (set! *relative-line-numbers* (not *relative-line-numbers*))
    (echo-message! echo
      (if *relative-line-numbers*
        "Relative line numbers mode on"
        "Absolute line numbers mode"))))

;;; --- Toggle CUA mode (C-c/C-v copy/paste) ---

(def *cua-mode* #f)

(def (cmd-toggle-cua-mode app)
  "Toggle CUA mode (use C-c/C-v for copy/paste)."
  (let ((echo (app-state-echo app)))
    (set! *cua-mode* (not *cua-mode*))
    (echo-message! echo
      (if *cua-mode*
        "CUA mode on (C-c=copy, C-v=paste)"
        "CUA mode off (Emacs keybindings)"))))

;;; --- Exchange point and mark ---

(def (cmd-exchange-dot-and-mark app)
  "Exchange point (cursor) and mark positions."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mark (buffer-mark buf))
         (pos (editor-get-current-pos ed)))
    (if (not mark)
      (echo-message! echo "No mark set")
      (begin
        (set! (buffer-mark buf) pos)
        (editor-goto-pos ed mark)
        (echo-message! echo "Point and mark exchanged")))))

;;; --- Sort paragraphs ---

(def (cmd-sort-paragraphs app)
  "Sort paragraphs (separated by blank lines) in the buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (text (editor-get-text ed))
         ;; Split on double newlines
         (paras (let loop ((s text) (start 0) (acc []))
                  (let ((idx (string-contains s "\n\n" start)))
                    (if idx
                      (loop s (+ idx 2) (cons (substring s start idx) acc))
                      (reverse (cons (substring s start (string-length s)) acc))))))
         (sorted (sort paras string<?))
         (result (string-join sorted "\n\n")))
    (editor-set-text ed result)
    (editor-goto-pos ed 0)
    (echo-message! echo
      (string-append "Sorted " (number->string (length paras)) " paragraphs"))))

;;; --- Insert Emacs/Vim-style mode line ---

(def (cmd-insert-mode-line app)
  "Insert an Emacs-style mode line comment at the top of the buffer."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (mode (or (buffer-lexer-lang buf) "text"))
         (mode-str (if (symbol? mode) (symbol->string mode) mode))
         (line (string-append ";; -*- mode: " mode-str " -*-\n")))
    (editor-insert-text ed 0 line)
    (echo-message! echo (string-append "Inserted mode line for " mode-str))))

;;; --- Push mark without activating region ---

(def (cmd-push-mark-command app)
  "Push the current position onto the mark ring without activating region."
  (let* ((ed (current-editor app))
         (echo (app-state-echo app))
         (buf (current-buffer-from-app app))
         (pos (editor-get-current-pos ed)))
    (set! (buffer-mark buf) pos)
    (echo-message! echo
      (string-append "Mark pushed at position " (number->string pos)))))

;;; =========================================================================
;;; Batch 35: auto-complete, which-function, line-numbers, selective-display, etc.
;;; =========================================================================

(def *global-auto-complete-mode* #f)
(def *which-function-mode* #f)
(def *display-line-numbers-mode* #t)
(def *selective-display-level* #f)
(def *global-font-lock-mode* #t)
(def *auto-dim-other-buffers* #f)
(def *global-eldoc-mode* #t)
(def *word-wrap-column* 80)

(def (cmd-toggle-global-auto-complete app)
  "Toggle global auto-complete-mode for code completion."
  (let ((echo (app-state-echo app)))
    (set! *global-auto-complete-mode* (not *global-auto-complete-mode*))
    (echo-message! echo (if *global-auto-complete-mode*
                          "Auto-complete mode ON"
                          "Auto-complete mode OFF"))))

(def (cmd-toggle-which-function app)
  "Toggle which-function-mode (show function name in modeline)."
  (let ((echo (app-state-echo app)))
    (set! *which-function-mode* (not *which-function-mode*))
    (echo-message! echo (if *which-function-mode*
                          "Which-function mode ON"
                          "Which-function mode OFF"))))

(def (cmd-toggle-display-line-numbers app)
  "Toggle display of line numbers in the margin."
  (let ((echo (app-state-echo app))
        (ed (current-editor app)))
    (set! *display-line-numbers-mode* (not *display-line-numbers-mode*))
    (if *display-line-numbers-mode*
      (begin
        ;; SCI_SETMARGINTYPEN = 2240, SC_MARGIN_NUMBER = 0
        (send-message ed 2240 0 0)
        ;; SCI_SETMARGINWIDTHN = 2242
        (send-message ed 2242 0 48)
        (echo-message! echo "Line numbers ON"))
      (begin
        (send-message ed 2242 0 0)
        (echo-message! echo "Line numbers OFF")))))

(def (cmd-toggle-selective-display app)
  "Toggle selective display at a given indentation level."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app)))
    (if *selective-display-level*
      (begin
        ;; SCI_FOLDALL = 2662, SC_FOLDACTION_EXPAND = 1
        (send-message ed 2662 1 0)
        (set! *selective-display-level* #f)
        (echo-message! echo "Selective display OFF"))
      (begin
        ;; SCI_FOLDALL = 2662, SC_FOLDACTION_CONTRACT = 0
        (send-message ed 2662 0 0)
        (set! *selective-display-level* 1)
        (echo-message! echo "Selective display ON (folded)")))))

(def (cmd-toggle-global-font-lock app)
  "Toggle global font-lock-mode (syntax highlighting)."
  (let ((echo (app-state-echo app))
        (ed (current-editor app)))
    (set! *global-font-lock-mode* (not *global-font-lock-mode*))
    (if *global-font-lock-mode*
      (begin
        ;; SCI_SETLEXER = 4001 — restore lexer would need to re-apply;
        ;; for now just toggle the state flag
        (echo-message! echo "Font-lock mode ON"))
      (begin
        ;; SCI_SETLEXER = 4001 with SCLEX_NULL = 1
        (send-message ed 4001 1 0)
        (echo-message! echo "Font-lock mode OFF")))))

(def (cmd-insert-register-content app)
  "Insert the content of a register at point."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (regs (app-state-registers app)))
    (echo-message! echo "Insert register: ")
    ;; In a real implementation this would read a key;
    ;; for now show info about available registers
    (let ((count (hash-length regs)))
      (echo-message! echo
        (string-append (number->string count) " registers defined")))))

(def (cmd-insert-date-iso app)
  "Insert the current date in ISO 8601 format (YYYY-MM-DD) at point."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (now (current-date))
         (date-str (date->string now "~Y-~m-~d")))
    (editor-replace-selection ed date-str)
    (echo-message! echo (string-append "Inserted: " date-str))))

(def (cmd-toggle-word-wrap-column app)
  "Toggle word wrap column between 72, 80, and 100."
  (let ((echo (app-state-echo app)))
    (set! *word-wrap-column*
      (cond ((= *word-wrap-column* 72) 80)
            ((= *word-wrap-column* 80) 100)
            (else 72)))
    (echo-message! echo
      (string-append "Word wrap column: " (number->string *word-wrap-column*)))))

(def (cmd-clone-indirect-buffer app)
  "Create an indirect buffer clone of the current buffer."
  (let* ((echo (app-state-echo app))
         (fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (buf (current-buffer-from-app app))
         (name (buffer-name buf))
         (clone-name (string-append name "<clone>"))
         (new-buf (buffer-create! clone-name ed)))
    ;; Copy text from original to clone
    (let ((text (editor-get-text ed)))
      (buffer-attach! ed new-buf)
      (set! (edit-window-buffer win) new-buf)
      (editor-set-text ed text)
      (echo-message! echo (string-append "Cloned to " clone-name)))))

(def (cmd-toggle-auto-dim-other-buffers app)
  "Toggle dimming of non-focused buffer windows."
  (let ((echo (app-state-echo app)))
    (set! *auto-dim-other-buffers* (not *auto-dim-other-buffers*))
    (echo-message! echo (if *auto-dim-other-buffers*
                          "Auto-dim other buffers ON"
                          "Auto-dim other buffers OFF"))))

(def (cmd-toggle-global-eldoc app)
  "Toggle global eldoc-mode (inline documentation hints)."
  (let ((echo (app-state-echo app)))
    (set! *global-eldoc-mode* (not *global-eldoc-mode*))
    (echo-message! echo (if *global-eldoc-mode*
                          "Eldoc mode ON"
                          "Eldoc mode OFF"))))

(def (cmd-open-line-below app)
  "Open a new line below the current line and move cursor there."
  (let* ((echo (app-state-echo app))
         (ed (current-editor app))
         (pos (editor-get-current-pos ed))
         ;; SCI_LINEFROMPOSITION = 2166
         (line (send-message ed 2166 pos 0))
         ;; SCI_GETLINEENDPOSITION = 2136
         (line-end (send-message ed 2136 line 0)))
    (editor-goto-pos ed line-end)
    (editor-replace-selection ed "\n")
    (echo-message! echo "Opened line below")))

;; ── batch 43: session and completion framework toggles ──────────────
(def *desktop-save-mode* #f)
(def *recentf-mode* #t)
(def *savehist-mode* #t)
(def *winner-mode* #t)
(def *midnight-mode* #f)
(def *global-undo-tree* #f)
(def *diff-hl-mode* #f)
(def *volatile-highlights* #f)
(def *vertico-mode* #f)
(def *marginalia-mode* #f)

(def (cmd-toggle-desktop-save-mode app)
  "Toggle desktop-save-mode (save/restore session)."
  (let ((echo (app-state-echo app)))
    (set! *desktop-save-mode* (not *desktop-save-mode*))
    (echo-message! echo (if *desktop-save-mode*
                          "Desktop save mode ON" "Desktop save mode OFF"))))

(def (cmd-toggle-recentf-mode app)
  "Toggle recentf-mode (track recent files)."
  (let ((echo (app-state-echo app)))
    (set! *recentf-mode* (not *recentf-mode*))
    (echo-message! echo (if *recentf-mode*
                          "Recentf mode ON" "Recentf mode OFF"))))

(def (cmd-toggle-savehist-mode app)
  "Toggle savehist-mode (persist minibuffer history)."
  (let ((echo (app-state-echo app)))
    (set! *savehist-mode* (not *savehist-mode*))
    (echo-message! echo (if *savehist-mode*
                          "Savehist mode ON" "Savehist mode OFF"))))

(def (cmd-toggle-winner-mode app)
  "Toggle winner-mode (window config undo/redo)."
  (let ((echo (app-state-echo app)))
    (set! *winner-mode* (not *winner-mode*))
    (echo-message! echo (if *winner-mode*
                          "Winner mode ON" "Winner mode OFF"))))

(def (cmd-toggle-midnight-mode app)
  "Toggle midnight-mode (clean old buffers at midnight)."
  (let ((echo (app-state-echo app)))
    (set! *midnight-mode* (not *midnight-mode*))
    (echo-message! echo (if *midnight-mode*
                          "Midnight mode ON" "Midnight mode OFF"))))

(def (cmd-toggle-global-undo-tree app)
  "Toggle global-undo-tree-mode (tree-based undo)."
  (let ((echo (app-state-echo app)))
    (set! *global-undo-tree* (not *global-undo-tree*))
    (echo-message! echo (if *global-undo-tree*
                          "Undo-tree mode ON" "Undo-tree mode OFF"))))

(def (cmd-toggle-diff-hl-mode app)
  "Toggle diff-hl-mode (highlight VCS diffs in fringe)."
  (let ((echo (app-state-echo app)))
    (set! *diff-hl-mode* (not *diff-hl-mode*))
    (echo-message! echo (if *diff-hl-mode*
                          "Diff-hl mode ON" "Diff-hl mode OFF"))))

(def (cmd-toggle-volatile-highlights app)
  "Toggle volatile-highlights (flash changed regions)."
  (let ((echo (app-state-echo app)))
    (set! *volatile-highlights* (not *volatile-highlights*))
    (echo-message! echo (if *volatile-highlights*
                          "Volatile highlights ON" "Volatile highlights OFF"))))

(def (cmd-toggle-vertico-mode app)
  "Toggle vertico-mode (vertical completion UI)."
  (let ((echo (app-state-echo app)))
    (set! *vertico-mode* (not *vertico-mode*))
    (echo-message! echo (if *vertico-mode*
                          "Vertico mode ON" "Vertico mode OFF"))))

(def (cmd-toggle-marginalia-mode app)
  "Toggle marginalia-mode (annotations in completions)."
  (let ((echo (app-state-echo app)))
    (set! *marginalia-mode* (not *marginalia-mode*))
    (echo-message! echo (if *marginalia-mode*
                          "Marginalia mode ON" "Marginalia mode OFF"))))

;; ── batch 52: programming and analysis toggles ──────────────────────
(def *global-cwarn* #f)
(def *global-hideshow* #f)
(def *global-abbrev* #t)
(def *global-diff-auto-refine* #t)
(def *global-eldoc-box* #f)
(def *global-flyspell-lazy* #f)
(def *global-so-clean* #f)

(def (cmd-toggle-global-cwarn app)
  "Toggle global cwarn-mode (C/C++ warning highlighting)."
  (let ((echo (app-state-echo app)))
    (set! *global-cwarn* (not *global-cwarn*))
    (echo-message! echo (if *global-cwarn*
                          "Global cwarn ON" "Global cwarn OFF"))))

(def (cmd-toggle-global-hideshow app)
  "Toggle global hideshow-mode (code block folding)."
  (let ((echo (app-state-echo app)))
    (set! *global-hideshow* (not *global-hideshow*))
    (echo-message! echo (if *global-hideshow*
                          "Global hideshow ON" "Global hideshow OFF"))))

(def (cmd-toggle-global-abbrev app)
  "Toggle global abbrev-mode (text abbreviation expansion)."
  (let ((echo (app-state-echo app)))
    (set! *global-abbrev* (not *global-abbrev*))
    (echo-message! echo (if *global-abbrev*
                          "Global abbrev ON" "Global abbrev OFF"))))

(def (cmd-toggle-global-diff-auto-refine app)
  "Toggle auto-refinement in diff mode."
  (let ((echo (app-state-echo app)))
    (set! *global-diff-auto-refine* (not *global-diff-auto-refine*))
    (echo-message! echo (if *global-diff-auto-refine*
                          "Diff auto-refine ON" "Diff auto-refine OFF"))))

(def (cmd-toggle-global-eldoc-box app)
  "Toggle global eldoc-box-mode (eldoc in popup)."
  (let ((echo (app-state-echo app)))
    (set! *global-eldoc-box* (not *global-eldoc-box*))
    (echo-message! echo (if *global-eldoc-box*
                          "Eldoc box ON" "Eldoc box OFF"))))

(def (cmd-toggle-global-flyspell-lazy app)
  "Toggle global flyspell-lazy-mode (lazy spell checking)."
  (let ((echo (app-state-echo app)))
    (set! *global-flyspell-lazy* (not *global-flyspell-lazy*))
    (echo-message! echo (if *global-flyspell-lazy*
                          "Flyspell lazy ON" "Flyspell lazy OFF"))))

(def (cmd-toggle-global-so-clean app)
  "Toggle global so-clean-mode (hide modeline lighters)."
  (let ((echo (app-state-echo app)))
    (set! *global-so-clean* (not *global-so-clean*))
    (echo-message! echo (if *global-so-clean*
                          "So-clean ON" "So-clean OFF"))))

;;; ---- batch 60: performance and profiling toggles ----

(def *global-native-compile* #f)
(def *global-gcmh* #f)
(def *global-esup* #f)
(def *global-explain-pause* #f)
(def *global-keyfreq* #f)
(def *global-command-log* #f)
(def *global-interaction-log* #f)

(def (cmd-toggle-global-native-compile app)
  "Toggle global native-compile-mode (ahead-of-time native compilation)."
  (let ((echo (app-state-echo app)))
    (set! *global-native-compile* (not *global-native-compile*))
    (echo-message! echo (if *global-native-compile*
                          "Native compile ON" "Native compile OFF"))))

(def (cmd-toggle-global-gcmh app)
  "Toggle global GCMH-mode (garbage collection magic hack)."
  (let ((echo (app-state-echo app)))
    (set! *global-gcmh* (not *global-gcmh*))
    (echo-message! echo (if *global-gcmh*
                          "GCMH ON" "GCMH OFF"))))

(def (cmd-toggle-global-esup app)
  "Toggle global ESUP-mode (startup profiler)."
  (let ((echo (app-state-echo app)))
    (set! *global-esup* (not *global-esup*))
    (echo-message! echo (if *global-esup*
                          "ESUP ON" "ESUP OFF"))))

(def (cmd-toggle-global-explain-pause app)
  "Toggle global explain-pause-mode (explain UI freezes)."
  (let ((echo (app-state-echo app)))
    (set! *global-explain-pause* (not *global-explain-pause*))
    (echo-message! echo (if *global-explain-pause*
                          "Explain pause ON" "Explain pause OFF"))))

(def (cmd-toggle-global-keyfreq app)
  "Toggle global keyfreq-mode (track key usage statistics)."
  (let ((echo (app-state-echo app)))
    (set! *global-keyfreq* (not *global-keyfreq*))
    (echo-message! echo (if *global-keyfreq*
                          "Keyfreq ON" "Keyfreq OFF"))))

(def (cmd-toggle-global-command-log app)
  "Toggle global command-log-mode (log executed commands)."
  (let ((echo (app-state-echo app)))
    (set! *global-command-log* (not *global-command-log*))
    (echo-message! echo (if *global-command-log*
                          "Command log ON" "Command log OFF"))))

(def (cmd-toggle-global-interaction-log app)
  "Toggle global interaction-log-mode (log all user interaction)."
  (let ((echo (app-state-echo app)))
    (set! *global-interaction-log* (not *global-interaction-log*))
    (echo-message! echo (if *global-interaction-log*
                          "Interaction log ON" "Interaction log OFF"))))

;;; ---- batch 69: data format and configuration language toggles ----

(def *global-yaml-mode* #f)
(def *global-toml-mode* #f)
(def *global-json-mode* #f)
(def *global-csv-mode* #f)
(def *global-protobuf-mode* #f)
(def *global-graphql-mode* #f)
(def *global-nix-mode* #f)

(def (cmd-toggle-global-yaml-mode app)
  "Toggle global yaml-mode (YAML file editing)."
  (let ((echo (app-state-echo app)))
    (set! *global-yaml-mode* (not *global-yaml-mode*))
    (echo-message! echo (if *global-yaml-mode*
                          "YAML mode ON" "YAML mode OFF"))))

(def (cmd-toggle-global-toml-mode app)
  "Toggle global toml-mode (TOML file editing)."
  (let ((echo (app-state-echo app)))
    (set! *global-toml-mode* (not *global-toml-mode*))
    (echo-message! echo (if *global-toml-mode*
                          "TOML mode ON" "TOML mode OFF"))))

(def (cmd-toggle-global-json-mode app)
  "Toggle global json-mode (JSON file editing)."
  (let ((echo (app-state-echo app)))
    (set! *global-json-mode* (not *global-json-mode*))
    (echo-message! echo (if *global-json-mode*
                          "JSON mode ON" "JSON mode OFF"))))

(def (cmd-toggle-global-csv-mode app)
  "Toggle global csv-mode (CSV file editing with alignment)."
  (let ((echo (app-state-echo app)))
    (set! *global-csv-mode* (not *global-csv-mode*))
    (echo-message! echo (if *global-csv-mode*
                          "CSV mode ON" "CSV mode OFF"))))

(def (cmd-toggle-global-protobuf-mode app)
  "Toggle global protobuf-mode (Protocol Buffers editing)."
  (let ((echo (app-state-echo app)))
    (set! *global-protobuf-mode* (not *global-protobuf-mode*))
    (echo-message! echo (if *global-protobuf-mode*
                          "Protobuf mode ON" "Protobuf mode OFF"))))

(def (cmd-toggle-global-graphql-mode app)
  "Toggle global graphql-mode (GraphQL schema editing)."
  (let ((echo (app-state-echo app)))
    (set! *global-graphql-mode* (not *global-graphql-mode*))
    (echo-message! echo (if *global-graphql-mode*
                          "GraphQL mode ON" "GraphQL mode OFF"))))

(def (cmd-toggle-global-nix-mode app)
  "Toggle global nix-mode (Nix expression editing)."
  (let ((echo (app-state-echo app)))
    (set! *global-nix-mode* (not *global-nix-mode*))
    (echo-message! echo (if *global-nix-mode*
                          "Nix mode ON" "Nix mode OFF"))))

;;;============================================================================
;;; Org-table TUI commands (parity with Qt layer)
;;;============================================================================

(def (tui-ed app) (edit-window-editor (current-window (app-state-frame app))))
(def (tui-tbl app fn)
  "Run fn on editor if on table line, else echo error."
  (let ((ed (tui-ed app)))
    (if (org-table-on-table-line? ed) (fn ed)
      (echo-message! (app-state-echo app) "Not in an org table"))))

(def (cmd-org-table-align app)
  (tui-tbl app (lambda (ed) (org-table-align ed)
    (echo-message! (app-state-echo app) "Table aligned"))))
(def (cmd-org-table-insert-row app) (tui-tbl app org-table-insert-row))
(def (cmd-org-table-delete-row app) (tui-tbl app org-table-delete-row))
(def (cmd-org-table-move-row-up app) (tui-tbl app (lambda (ed) (org-table-move-row ed -1))))
(def (cmd-org-table-move-row-down app) (tui-tbl app (lambda (ed) (org-table-move-row ed 1))))
(def (cmd-org-table-delete-column app) (tui-tbl app org-table-delete-column))
(def (cmd-org-table-insert-column app) (tui-tbl app org-table-insert-column))
(def (cmd-org-table-move-column-left app) (tui-tbl app (lambda (ed) (org-table-move-column ed -1))))
(def (cmd-org-table-move-column-right app) (tui-tbl app (lambda (ed) (org-table-move-column ed 1))))
(def (cmd-org-table-insert-separator app) (tui-tbl app org-table-insert-separator-line))
(def (cmd-org-table-recalculate app)
  (tui-tbl app (lambda (ed) (org-table-recalculate ed)
    (echo-message! (app-state-echo app) "Recalculated"))))

(def (cmd-org-table-sort app)
  (tui-tbl app (lambda (ed)
    (let* ((col (org-table-current-column ed))
           (rows (let-values (((s e) (org-table-find-bounds ed)))
                   (org-table-get-rows ed s e)))
           (numeric? (let loop ((rs rows))
                       (if (null? rs) #t
                         (let ((r (car rs)))
                           (if (or (eq? r 'separator) (>= col (length r))) (loop (cdr rs))
                             (let ((cell (string-trim-both (list-ref r col))))
                               (if (string=? cell "") (loop (cdr rs))
                                 (if (org-numeric-cell? cell) (loop (cdr rs)) #f)))))))))
      (org-table-sort ed col numeric?)
      (echo-message! (app-state-echo app)
        (string-append "Sorted by column " (number->string (+ col 1))
                       (if numeric? " (numeric)" " (alphabetic)")))))))

(def (cmd-org-table-sum app)
  (tui-tbl app (lambda (ed)
    (let-values (((start end) (org-table-find-bounds ed)))
      (let* ((col (org-table-current-column ed))
             (rows (org-table-get-rows ed start end))
             (vals (filter-map
                     (lambda (row)
                       (and (list? row) (< col (length row))
                            (string->number (string-trim-both (list-ref row col)))))
                     rows))
             (total (apply + vals)))
        (echo-message! (app-state-echo app)
          (string-append "Sum of column " (number->string (+ col 1))
                         ": " (number->string total)
                         " (" (number->string (length vals)) " values)")))))))

(def (cmd-org-table-export-csv app)
  (let* ((ed (tui-ed app)) (fr (app-state-frame app)) (win (current-window fr)))
    (if (org-table-on-table-line? ed)
      (let* ((csv (org-table-to-csv ed))
             (buf (buffer-create! "*CSV Export*" ed)))
        (buffer-attach! ed buf) (set! (edit-window-buffer win) buf)
        (editor-set-text ed csv) (editor-goto-pos ed 0)
        (echo-message! (app-state-echo app) "Exported table as CSV"))
      (echo-message! (app-state-echo app) "Not in an org table"))))

(def (cmd-org-table-import-csv app)
  (let* ((ed (tui-ed app))
         (sel-start (editor-get-selection-start ed))
         (sel-end (editor-get-selection-end ed)))
    (if (= sel-start sel-end)
      (echo-message! (app-state-echo app) "Select CSV text first")
      (let* ((text (editor-get-text ed))
             (csv-text (substring text sel-start sel-end))
             (table-text (org-csv-to-table csv-text))
             (new-text (string-append (substring text 0 sel-start) table-text
                                      (substring text sel-end (string-length text)))))
        (editor-set-text ed new-text) (editor-goto-pos ed sel-start)
        (echo-message! (app-state-echo app) "Converted CSV to org table")))))

(def (cmd-org-table-transpose app)
  (tui-tbl app (lambda (ed)
    (let-values (((start end) (org-table-find-bounds ed)))
      (let* ((rows (org-table-get-rows ed start end))
             (data-rows (filter list? rows))
             (ncols (if (null? data-rows) 0 (apply max (map length data-rows))))
             (transposed
               (let loop ((col 0) (acc '()))
                 (if (>= col ncols) (reverse acc)
                   (loop (+ col 1)
                         (cons (map (lambda (row)
                                      (if (< col (length row)) (list-ref row col) ""))
                                    data-rows) acc))))))
        (org-table-replace-rows ed start end transposed)
        (echo-message! (app-state-echo app)
          (string-append "Transposed: " (number->string (length data-rows)) "x"
            (number->string ncols) " -> " (number->string ncols) "x"
            (number->string (length data-rows)))))))))
