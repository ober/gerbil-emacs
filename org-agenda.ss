;;; -*- Gerbil -*-
;;; Org agenda: daily/weekly views, TODO list, tag search, filtering.
;;; Backend-agnostic (Scintilla API only, no Qt imports).

(export #t)

(import :std/sugar
        :std/sort
        (only-in :std/srfi/13
                 string-trim string-contains string-prefix? string-join
                 string-pad-right string-upcase)
        (only-in :std/srfi/19
                 current-date date->string string->date
                 date-year date-month date-day date-week-day
                 date->time-utc time-utc->date
                 make-time time-type time-second time-nanosecond
                 add-duration time-duration)
        :std/pregexp
        :std/misc/string
        :gemacs/core
        :gemacs/org-parse)

;;;============================================================================
;;; Agenda Item Structure
;;;============================================================================

(defstruct org-agenda-item
  (heading       ; org-heading struct
   type          ; 'scheduled 'deadline 'timestamp
   date          ; org-timestamp
   time-string   ; "HH:MM" or #f
   file          ; source file path
   line)         ; source line number
  transparent: #t)

;;;============================================================================
;;; Global Agenda State
;;;============================================================================

(def *org-agenda-files* '())     ; list of file paths to scan
(def *org-agenda-span* 7)        ; default: weekly view
(def *org-agenda-start-on-weekday* 1) ; 1 = Monday

;;;============================================================================
;;; Agenda Item Collection
;;;============================================================================

(def (org-collect-agenda-items text file-path date-from date-to)
  "Collect agenda items from org text that fall within date range.
   Returns list of org-agenda-item."
  (let* ((headings (org-parse-buffer text))
         (items '()))
    (for-each
      (lambda (h)
        ;; Check SCHEDULED
        (let ((sched (org-heading-scheduled h)))
          (when (and sched (org-timestamp-in-range? sched date-from date-to))
            (set! items (cons (make-org-agenda-item
                                h 'scheduled sched
                                (org-timestamp-time-str sched)
                                file-path (org-heading-line-number h))
                              items))))
        ;; Check DEADLINE
        (let ((dead (org-heading-deadline h)))
          (when (and dead (org-timestamp-in-range? dead date-from date-to))
            (set! items (cons (make-org-agenda-item
                                h 'deadline dead
                                (org-timestamp-time-str dead)
                                file-path (org-heading-line-number h))
                              items)))))
      headings)
    (reverse items)))

(def (org-timestamp-time-str ts)
  "Extract HH:MM string from timestamp, or #f."
  (if (and (org-timestamp-hour ts) (org-timestamp-minute ts))
    (string-append (pad-02 (org-timestamp-hour ts)) ":"
                   (pad-02 (org-timestamp-minute ts)))
    #f))

(def (org-timestamp-in-range? ts date-from date-to)
  "Check if timestamp falls within [date-from, date-to] range (inclusive)."
  (and ts
       (let ((y (org-timestamp-year ts))
             (m (org-timestamp-month ts))
             (d (org-timestamp-day ts))
             (fy (org-timestamp-year date-from))
             (fm (org-timestamp-month date-from))
             (fd (org-timestamp-day date-from))
             (ty (org-timestamp-year date-to))
             (tm (org-timestamp-month date-to))
             (td (org-timestamp-day date-to)))
         (let ((ts-val (+ (* y 10000) (* m 100) d))
               (from-val (+ (* fy 10000) (* fm 100) fd))
               (to-val (+ (* ty 10000) (* tm 100) td)))
           (and (>= ts-val from-val)
                (<= ts-val to-val))))))

;;;============================================================================
;;; Sorting
;;;============================================================================

(def (org-agenda-sort-items items)
  "Sort agenda items by: time first, then priority, then alpha."
  (sort items
    (lambda (a b)
      (let ((ta (or (org-agenda-item-time-string a) "99:99"))
            (tb (or (org-agenda-item-time-string b) "99:99")))
        (if (string=? ta tb)
          ;; Same time: sort by priority
          (let ((pa (or (org-heading-priority (org-agenda-item-heading a)) #\C))
                (pb (or (org-heading-priority (org-agenda-item-heading b)) #\C)))
            (if (eqv? pa pb)
              ;; Same priority: sort by title
              (string<? (org-heading-title (org-agenda-item-heading a))
                        (org-heading-title (org-agenda-item-heading b)))
              (char<? pa pb)))
          (string<? ta tb))))))

;;;============================================================================
;;; Day Formatting
;;;============================================================================

(def *org-agenda-day-names* #("Sunday" "Monday" "Tuesday" "Wednesday"
                               "Thursday" "Friday" "Saturday"))

(def (org-format-agenda-day date-ts items)
  "Format one day of the agenda with its items.
   date-ts is an org-timestamp for the day."
  (let* ((day-name (or (org-timestamp-day-name date-ts)
                       (let ((wday (org-date-weekday
                                     (org-timestamp-year date-ts)
                                     (org-timestamp-month date-ts)
                                     (org-timestamp-day date-ts))))
                         (vector-ref *org-agenda-day-names* wday))))
         (date-str (string-append
                     day-name " "
                     (number->string (org-timestamp-day date-ts)) " "
                     (org-month-name (org-timestamp-month date-ts)) " "
                     (number->string (org-timestamp-year date-ts))))
         (header (string-append date-str "\n"
                                (make-string (string-length date-str) #\-)))
         ;; Filter items for this day
         (day-items (filter
                      (lambda (item)
                        (let ((d (org-agenda-item-date item)))
                          (and (= (org-timestamp-year d) (org-timestamp-year date-ts))
                               (= (org-timestamp-month d) (org-timestamp-month date-ts))
                               (= (org-timestamp-day d) (org-timestamp-day date-ts)))))
                      items))
         (sorted (org-agenda-sort-items day-items))
         (item-lines (map org-format-agenda-item sorted)))
    (if (null? item-lines)
      header
      (string-append header "\n" (string-join item-lines "\n")))))

(def (org-format-agenda-item item)
  "Format a single agenda item for display."
  (let* ((h (org-agenda-item-heading item))
         (type-str (case (org-agenda-item-type item)
                     ((scheduled) "  Sched:")
                     ((deadline)  "  Dead: ")
                     ((timestamp) "       ")
                     (else "       ")))
         (time-str (let ((t (org-agenda-item-time-string item)))
                     (if t (string-append " " t) "      ")))
         (kw (or (org-heading-keyword h) ""))
         (title (org-heading-title h))
         (tags (let ((t (org-heading-tags h)))
                 (if (and t (not (null? t)))
                   (string-append " :" (string-join t ":") ":")
                   ""))))
    (string-append type-str time-str " "
                   (if (string=? kw "") "" (string-append kw " "))
                   title tags)))

(def (org-month-name month)
  "Return month name."
  (vector-ref #("" "January" "February" "March" "April" "May" "June"
                "July" "August" "September" "October" "November" "December")
              month))

;;;============================================================================
;;; Date Arithmetic
;;;============================================================================

(def (org-date-weekday year month day)
  "Compute day of week (0=Sunday) using Zeller's congruence."
  (let* ((m (if (< month 3) (+ month 12) month))
         (y (if (< month 3) (- year 1) year))
         (k (remainder y 100))
         (j (quotient y 100))
         (h (remainder (+ day
                          (quotient (* 13 (+ m 1)) 5)
                          k (quotient k 4)
                          (quotient j 4) (* -2 j))
                       7))
         (d (remainder (+ h 6) 7)))  ; convert: 0=Sat -> 0=Sun
    d))

(def (org-make-date-ts year month day)
  "Create an org-timestamp for a date."
  (make-org-timestamp 'active year month day #f #f #f #f #f #f #f))

(def (org-advance-date-ts date-ts days)
  "Advance an org-timestamp by N days."
  (let* ((d (org-timestamp->date date-ts))
         (t (date->time-utc d))
         (dur (make-time time-duration 0 (* days 86400)))
         (new-t (add-duration t dur))
         (new-d (time-utc->date new-t 0)))
    (make-org-timestamp
      (org-timestamp-type date-ts)
      (date-year new-d) (date-month new-d) (date-day new-d)
      #f #f #f #f #f #f #f)))

(def (org-today-ts)
  "Get today as an org-timestamp."
  (let ((d (current-date)))
    (org-make-date-ts (date-year d) (date-month d) (date-day d))))

;;;============================================================================
;;; Agenda Views
;;;============================================================================

(def (org-agenda-daily-weekly text file-path span)
  "Generate daily/weekly agenda view. span = number of days."
  (let* ((today (org-today-ts))
         ;; Build date range
         (end-date (org-advance-date-ts today (- span 1)))
         ;; Collect items
         (items (org-collect-agenda-items text file-path today end-date))
         ;; Format each day
         (days (let loop ((i 0) (acc '()))
                 (if (>= i span)
                   (reverse acc)
                   (let ((day-ts (org-advance-date-ts today i)))
                     (loop (+ i 1) (cons (org-format-agenda-day day-ts items) acc)))))))
    (string-join days "\n\n")))

(def (org-agenda-todo-list text file-path)
  "Generate list of all TODO items."
  (let* ((headings (org-parse-buffer text))
         (todos (filter
                  (lambda (h)
                    (let ((kw (org-heading-keyword h)))
                      (and kw (not (member kw '("DONE" "CANCELLED"))))))
                  headings))
         (sorted (sort todos
                   (lambda (a b)
                     (let ((pa (or (org-heading-priority a) #\C))
                           (pb (or (org-heading-priority b) #\C)))
                       (if (eqv? pa pb)
                         (string<? (org-heading-title a)
                                   (org-heading-title b))
                         (char<? pa pb)))))))
    (if (null? sorted)
      "No TODO items found."
      (string-append
        "Global TODO list\n"
        "================\n"
        (string-join
          (map (lambda (h)
                 (let ((kw (or (org-heading-keyword h) ""))
                       (title (org-heading-title h))
                       (tags (let ((t (org-heading-tags h)))
                               (if (and t (not (null? t)))
                                 (string-append " :" (string-join t ":") ":")
                                 ""))))
                   (string-append "  " kw " " title tags)))
               sorted)
          "\n")))))

(def (org-agenda-tags-match text file-path tag-expr)
  "Find headings matching tag expression."
  (let* ((headings (org-parse-buffer text))
         (matching (filter
                     (lambda (h)
                       (org-heading-match-tags? h tag-expr))
                     headings)))
    (if (null? matching)
      (string-append "No matches for tag: " tag-expr)
      (string-append
        "Tags match: " tag-expr "\n"
        (make-string (+ 13 (string-length tag-expr)) #\=) "\n"
        (string-join
          (map (lambda (h)
                 (let ((level (org-heading-stars h))
                       (kw (or (org-heading-keyword h) ""))
                       (title (org-heading-title h))
                       (tags (let ((t (org-heading-tags h)))
                               (if (and t (not (null? t)))
                                 (string-append " :" (string-join t ":") ":")
                                 ""))))
                   (string-append "  " (make-string level #\*) " "
                                  (if (string=? kw "") "" (string-append kw " "))
                                  title tags)))
               matching)
          "\n")))))

(def (org-agenda-search text file-path query)
  "Full-text search across headings."
  (let* ((headings (org-parse-buffer text))
         (query-lower (string-downcase query))
         (matching (filter
                     (lambda (h)
                       (string-contains (string-downcase (org-heading-title h))
                                        query-lower))
                     headings)))
    (if (null? matching)
      (string-append "No matches for: " query)
      (string-append
        "Search: " query "\n"
        (make-string (+ 8 (string-length query)) #\=) "\n"
        (string-join
          (map (lambda (h)
                 (let ((kw (or (org-heading-keyword h) ""))
                       (title (org-heading-title h)))
                   (string-append "  "
                                  (if (string=? kw "") "" (string-append kw " "))
                                  title)))
               matching)
          "\n")))))

(def (string-downcase s)
  "Convert string to lowercase."
  (list->string (map char-downcase (string->list s))))

;;;============================================================================
;;; Recurring Items
;;;============================================================================

(def (org-expand-recurring heading date-from date-to)
  "Generate occurrences for a heading with repeating timestamps.
   Returns list of org-agenda-item for each occurrence in [date-from, date-to]."
  (let ((items '()))
    ;; Check scheduled
    (let ((sched (org-heading-scheduled heading)))
      (when (and sched (org-timestamp-repeater sched))
        (let ((occurrences (org-generate-occurrences sched date-from date-to)))
          (for-each
            (lambda (ts)
              (set! items (cons (make-org-agenda-item
                                  heading 'scheduled ts
                                  (org-timestamp-time-str ts)
                                  (org-heading-file-path heading)
                                  (org-heading-line-number heading))
                                items)))
            occurrences))))
    ;; Check deadline
    (let ((dead (org-heading-deadline heading)))
      (when (and dead (org-timestamp-repeater dead))
        (let ((occurrences (org-generate-occurrences dead date-from date-to)))
          (for-each
            (lambda (ts)
              (set! items (cons (make-org-agenda-item
                                  heading 'deadline ts
                                  (org-timestamp-time-str ts)
                                  (org-heading-file-path heading)
                                  (org-heading-line-number heading))
                                items)))
            occurrences))))
    (reverse items)))

(def (org-generate-occurrences ts date-from date-to)
  "Generate all occurrences of a repeating timestamp within range."
  (let ((repeater (org-timestamp-repeater ts)))
    (if (not repeater)
      '()
      (let* ((m (pregexp-match "([.+]+)(\\d+)([hdwmy])" repeater))
             (amount (if m (string->number (list-ref m 2)) 1))
             (unit (if m (list-ref m 3) "d"))
             (days-per (cond
                         ((string=? unit "d") amount)
                         ((string=? unit "w") (* amount 7))
                         ((string=? unit "m") (* amount 30))  ; approximate
                         ((string=? unit "y") (* amount 365)) ; approximate
                         (else amount))))
        ;; Generate from base ts forward
        (let loop ((current ts) (acc '()) (safety 0))
          (if (> safety 365) ; safety limit
            (reverse acc)
            (if (org-timestamp-in-range? current date-from date-to)
              (loop (org-advance-date-ts current days-per)
                    (cons current acc)
                    (+ safety 1))
              ;; Past end of range?
              (let ((cval (+ (* (org-timestamp-year current) 10000)
                             (* (org-timestamp-month current) 100)
                             (org-timestamp-day current)))
                    (tval (+ (* (org-timestamp-year date-to) 10000)
                             (* (org-timestamp-month date-to) 100)
                             (org-timestamp-day date-to))))
                (if (> cval tval)
                  (reverse acc)
                  (loop (org-advance-date-ts current days-per)
                        acc
                        (+ safety 1)))))))))))

;;;============================================================================
;;; Stuck Projects Detection
;;;============================================================================

(def (org-find-stuck-projects text)
  "Find headings with sub-headings but no actionable TODO children.
   Returns list of org-heading."
  (let* ((headings (org-parse-buffer text))
         (total (length headings)))
    (filter
      (lambda (h)
        ;; Must have children and no TODO/NEXT child
        (let* ((level (org-heading-stars h))
               (idx (list-index h headings))
               (has-children? #f)
               (has-todo-child? #f))
          (when idx
            (let loop ((i (+ idx 1)))
              (when (< i total)
                (let* ((child (list-ref headings i))
                       (child-level (org-heading-stars child)))
                  (cond
                    ((<= child-level level) (void)) ; past subtree
                    ((= child-level (+ level 1))
                     (set! has-children? #t)
                     (let ((kw (org-heading-keyword child)))
                       (when (and kw (member kw '("TODO" "NEXT")))
                         (set! has-todo-child? #t)))
                     (loop (+ i 1)))
                    (else (loop (+ i 1))))))))
          (and has-children? (not has-todo-child?))))
      headings)))

(def (list-index item lst)
  "Find index of item in list, or #f."
  (let loop ((i 0) (l lst))
    (cond
      ((null? l) #f)
      ((eq? (car l) item) i)
      (else (loop (+ i 1) (cdr l))))))
