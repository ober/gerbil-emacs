;;; -*- Gerbil -*-
;;; Org capture: templates, expansion, refile.
;;; Backend-agnostic (Scintilla API only, no Qt imports).

(export #t)

(import :std/sugar
        (only-in :std/srfi/13
                 string-trim string-contains string-prefix? string-join
                 string-pad-right string-index)
        (only-in :std/srfi/19
                 current-date date->string)
        :std/pregexp
        :std/misc/string
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gemacs/core
        :gemacs/echo
        :gemacs/org-parse)

;;;============================================================================
;;; Capture Template Structure
;;;============================================================================

(defstruct org-capture-template
  (key description type target template)
  transparent: #t)

;; Default templates
(def *org-capture-templates*
  (list
    (make-org-capture-template
      "t" "TODO" 'entry
      '(file+headline "~/org/inbox.org" "Tasks")
      "* TODO %?\n  %U\n")
    (make-org-capture-template
      "n" "Note" 'entry
      '(file "~/org/notes.org")
      "* %?\n  %U\n")
    (make-org-capture-template
      "j" "Journal" 'entry
      '(file+datetree "~/org/journal.org")
      "* %U %?\n")))

;; Capture state
(def *org-capture-active?* #f)
(def *org-capture-target* #f)    ; (list file headline-or-#f)
(def *org-capture-template* #f)  ; string — expanded template

;;;============================================================================
;;; Template Expansion
;;;============================================================================

(def (org-capture-expand-template tmpl . opts)
  "Expand capture template placeholders:
   %? — cursor position (removed, just marks where to place cursor)
   %U — inactive timestamp [2024-01-15 Mon 10:00]
   %T — active timestamp <2024-01-15 Mon 10:00>
   %t — active date-only <2024-01-15 Mon>
   %u — inactive date-only [2024-01-15 Mon]
   %f — current filename
   %F — current full file path
   %% — literal %"
  (let* ((filename (if (and (pair? opts) (car opts)) (car opts) "unknown"))
         (filepath (if (and (pair? opts) (pair? (cdr opts)) (cadr opts))
                     (cadr opts) ""))
         (now (current-date))
         (active-ts (org-current-timestamp-string #t))
         (inactive-ts (org-current-timestamp-string #f))
         (active-date (org-current-date-string))
         ;; Create inactive date by replacing < > with [ ]
         (inactive-date (let ((d (org-current-date-string)))
                          (string-append "[" (substring d 1 (- (string-length d) 1)) "]")))
         (result tmpl))
    ;; Replace %% first to avoid double-expansion
    (set! result (pregexp-replace* "%%" result "__PCNT__"))
    (set! result (pregexp-replace* "%U" result inactive-ts))
    (set! result (pregexp-replace* "%T" result active-ts))
    (set! result (pregexp-replace* "%t" result active-date))
    (set! result (pregexp-replace* "%u" result inactive-date))
    (set! result (pregexp-replace* "%f" result filename))
    (set! result (pregexp-replace* "%F" result filepath))
    ;; Remove %? marker (just a cursor position hint)
    (set! result (pregexp-replace* "%\\?" result ""))
    ;; Restore literal %
    (set! result (pregexp-replace* "__PCNT__" result "%"))
    result))

(def (org-capture-cursor-position tmpl)
  "Return the character offset where %? appears in the expanded template,
   or #f if no %? marker."
  (let ((idx (string-contains tmpl "%?")))
    idx))

;;;============================================================================
;;; Target Resolution
;;;============================================================================

(def (org-resolve-capture-target target)
  "Resolve a capture target spec to (values file-path headline-or-#f).
   Target forms:
     (file path)
     (file+headline path headline)
     (file+datetree path)"
  (cond
    ((and (pair? target) (eq? (car target) 'file))
     (values (expand-capture-path (cadr target)) #f))
    ((and (pair? target) (eq? (car target) 'file+headline))
     (values (expand-capture-path (cadr target)) (caddr target)))
    ((and (pair? target) (eq? (car target) 'file+datetree))
     (values (expand-capture-path (cadr target)) (org-datetree-heading)))
    (else
     (values "~/org/inbox.org" #f))))

(def (expand-capture-path path)
  "Expand ~ to HOME in path."
  (if (string-prefix? "~/" path)
    (string-append (or (getenv "HOME" #f) "/tmp") (substring path 1 (string-length path)))
    path))

(def (org-datetree-heading)
  "Generate a datetree heading for today: ** 2024-01-15 Monday"
  (let ((now (current-date)))
    (date->string now "~Y-~m-~d ~a")))

;;;============================================================================
;;; Capture Operations
;;;============================================================================

(def (org-capture-start template-key source-file source-path)
  "Start a capture session. Returns the expanded template text."
  (let ((tmpl (find (lambda (t) (string=? (org-capture-template-key t) template-key))
                    *org-capture-templates*)))
    (if (not tmpl)
      (begin
        (set! *org-capture-active?* #f)
        #f)
      (let-values (((file headline) (org-resolve-capture-target
                                      (org-capture-template-target tmpl))))
        (set! *org-capture-active?* #t)
        (set! *org-capture-target* (list file headline))
        (let ((expanded (org-capture-expand-template
                          (org-capture-template-template tmpl)
                          source-file source-path)))
          (set! *org-capture-template* expanded)
          expanded)))))

(def (org-capture-finalize text)
  "Finalize capture: append text to target file under target heading.
   Returns #t on success, #f on failure."
  (if (not *org-capture-active?*)
    #f
    (let ((file (car *org-capture-target*))
          (headline (cadr *org-capture-target*)))
      (with-catch
        (lambda (e) #f)
        (lambda ()
          ;; Ensure directory exists
          (let ((dir (path-directory file)))
            (when (and (string? dir) (not (string=? dir ""))
                       (not (file-exists? dir)))
              (create-directory* dir)))
          ;; Read existing content or start fresh
          (let ((existing (if (file-exists? file)
                            (read-file-string file)
                            "")))
            (let ((new-content
                    (if headline
                      ;; Insert under specific heading
                      (org-insert-under-heading existing headline text)
                      ;; Append to end of file
                      (string-append existing
                                     (if (and (> (string-length existing) 0)
                                              (not (string-suffix? "\n" existing)))
                                       "\n" "")
                                     text
                                     (if (string-suffix? "\n" text) "" "\n")))))
              (call-with-output-file file
                (lambda (port) (display new-content port)))))
          ;; Reset state
          (set! *org-capture-active?* #f)
          (set! *org-capture-target* #f)
          (set! *org-capture-template* #f)
          #t)))))

(def (org-capture-abort)
  "Abort capture session."
  (set! *org-capture-active?* #f)
  (set! *org-capture-target* #f)
  (set! *org-capture-template* #f))

;;;============================================================================
;;; Refile
;;;============================================================================

(def (org-refile-targets text)
  "Get list of headings from text as refile targets.
   Returns list of (title . line-number) pairs."
  (let* ((lines (string-split text #\newline))
         (total (length lines)))
    (let loop ((i 0) (result '()))
      (if (>= i total)
        (reverse result)
        (let ((line (list-ref lines i)))
          (if (org-heading-line? line)
            (let-values (((level keyword priority title tags)
                          (org-parse-heading-line line)))
              (let ((t (or title "")))
                (loop (+ i 1) (cons (cons t i) result))))
            (loop (+ i 1) result)))))))

(def (org-extract-subtree text line-num)
  "Extract a subtree starting at line-num from text.
   Returns (values subtree-text remaining-text)."
  (let* ((lines (string-split text #\newline))
         (total (length lines))
         (heading-line (list-ref lines line-num))
         (level (org-heading-stars-of-line heading-line))
         (end (org-find-subtree-end-in-text lines line-num level))
         ;; Extract subtree lines
         (subtree-lines (let loop ((i line-num) (acc '()))
                          (if (>= i end)
                            (reverse acc)
                            (loop (+ i 1) (cons (list-ref lines i) acc)))))
         ;; Build remaining lines (everything except subtree)
         (before (let loop ((i 0) (acc '()))
                   (if (>= i line-num)
                     (reverse acc)
                     (loop (+ i 1) (cons (list-ref lines i) acc)))))
         (after (let loop ((i end) (acc '()))
                  (if (>= i total)
                    (reverse acc)
                    (loop (+ i 1) (cons (list-ref lines i) acc))))))
    (values (string-join subtree-lines "\n")
            (string-join (append before after) "\n"))))

;;;============================================================================
;;; Helper: Insert Under Heading
;;;============================================================================

(def (org-insert-under-heading text heading content)
  "Insert content under the first heading matching `heading` in text.
   If heading not found, append content at end."
  (let* ((lines (string-split text #\newline))
         (total (length lines))
         ;; Find matching heading
         (heading-line
           (let loop ((i 0))
             (cond
               ((>= i total) #f)
               ((and (org-heading-line? (list-ref lines i))
                     (string-contains (list-ref lines i) heading))
                i)
               (else (loop (+ i 1)))))))
    (if (not heading-line)
      ;; Heading not found, append
      (string-append text
                     (if (string-suffix? "\n" text) "" "\n")
                     content
                     (if (string-suffix? "\n" content) "" "\n"))
      ;; Insert after heading (before next heading at same or higher level)
      (let* ((level (org-heading-stars-of-line (list-ref lines heading-line)))
             (insert-at (org-find-subtree-end-in-text lines heading-line level))
             (before-lines (let loop ((i 0) (acc '()))
                             (if (>= i insert-at)
                               (reverse acc)
                               (loop (+ i 1) (cons (list-ref lines i) acc)))))
             (after-lines (let loop ((i insert-at) (acc '()))
                            (if (>= i total)
                              (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))))
        (string-join
          (append before-lines
                  (list content)
                  after-lines)
          "\n")))))

;;;============================================================================
;;; Helper: string-suffix?
;;;============================================================================

(def (string-suffix? suffix str)
  "Check if str ends with suffix."
  (let ((slen (string-length suffix))
        (len (string-length str)))
    (and (>= len slen)
         (string=? (substring str (- len slen) len) suffix))))

;;;============================================================================
;;; Template Menu Formatting
;;;============================================================================

(def (org-capture-menu-string)
  "Format the capture template menu for display."
  (string-join
    (map (lambda (t)
           (string-append "[" (org-capture-template-key t) "] "
                          (org-capture-template-description t)))
         *org-capture-templates*)
    "  "))
