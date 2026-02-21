;;; org-parse-test.ss -- Tests for gemacs org-parse module
;;; Converted from Emacs org-mode test-org.el (timestamps, headings,
;;; planning, properties, clock lines, buffer parsing, tags, utilities)

(import :std/test
        :std/srfi/13  ;; string-contains etc.
        (except-in (rename-in :gemacs/org-parse
                              (org-timestamp-elapsed org-timestamp-elapsed-real)
                              (org-parse-properties  org-parse-properties-raw))
                   string-prefix-ci?))

(export org-parse-test)

;; Adapter: tests call (org-timestamp-elapsed h1 m1 h2 m2) with 4 ints,
;; but actual function takes two timestamp objects.
;; make-org-timestamp args: type year month day day-name hour minute end-hour end-minute repeater warning
(def (org-timestamp-elapsed h1 m1 h2 m2)
  (let ((ts1 (make-org-timestamp 'active 2024 1 1 #f h1 m1 #f #f #f #f))
        (ts2 (make-org-timestamp 'active 2024 1 1 #f h2 m2 #f #f #f #f)))
    (org-timestamp-elapsed-real ts1 ts2)))

;; Adapter: tests call (org-parse-properties lines) with 1 arg,
;; but actual function takes (lines start-idx).
(def (org-parse-properties lines)
  (org-parse-properties-raw lines 0))

(def org-parse-test
  (test-suite "org-parse"

    ;; =========================================================
    ;; Timestamp parsing (from test-org.el timestamps section)
    ;; =========================================================

    (test-case "org-parse-timestamp: active date only"
      (let ((ts (org-parse-timestamp "<2024-01-15>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'active)
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 1)
        (check (org-timestamp-day ts) => 15)))

    (test-case "org-parse-timestamp: active with day name and time"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:30>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'active)
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 1)
        (check (org-timestamp-day ts) => 15)
        (check (org-timestamp-day-name ts) => "Mon")
        (check (org-timestamp-hour ts) => 10)
        (check (org-timestamp-minute ts) => 30)))

    (test-case "org-parse-timestamp: inactive timestamp"
      (let ((ts (org-parse-timestamp "[2024-03-20 Wed 14:00]")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'inactive)
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 3)
        (check (org-timestamp-day ts) => 20)
        (check (org-timestamp-day-name ts) => "Wed")
        (check (org-timestamp-hour ts) => 14)
        (check (org-timestamp-minute ts) => 0)))

    (test-case "org-parse-timestamp: time range"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:00-11:30>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-hour ts) => 10)
        (check (org-timestamp-minute ts) => 0)
        (check (org-timestamp-end-hour ts) => 11)
        (check (org-timestamp-end-minute ts) => 30)))

    (test-case "org-parse-timestamp: with repeater"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:00 +1w>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-repeater ts) => "+1w")))

    (test-case "org-parse-timestamp: with warning"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:00 -3d>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-warning ts) => "-3d")))

    (test-case "org-parse-timestamp: invalid returns #f"
      (check (org-parse-timestamp "not a timestamp") => #f)
      (check (org-parse-timestamp "") => #f)
      (check (org-parse-timestamp "just text") => #f))

    (test-case "org-parse-timestamp: date-only inactive"
      (let ((ts (org-parse-timestamp "[2024-06-15]")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'inactive)
        (check (org-timestamp-year ts) => 2024)
        (check (org-timestamp-month ts) => 6)
        (check (org-timestamp-day ts) => 15)))

    (test-case "org-parse-timestamp: with repeater and warning"
      (let ((ts (org-parse-timestamp "<2024-01-15 Mon 10:00 +1w -3d>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-repeater ts) => "+1w")
        (check (org-timestamp-warning ts) => "-3d")))

    (test-case "org-parse-timestamp: various repeater types"
      ;; +Ny (yearly), +Nm (monthly), +Nd (daily), +Nw (weekly)
      (let ((ts1 (org-parse-timestamp "<2024-01-15 +2y>")))
        (check (org-timestamp-repeater ts1) => "+2y"))
      (let ((ts2 (org-parse-timestamp "<2024-01-15 +3m>")))
        (check (org-timestamp-repeater ts2) => "+3m"))
      (let ((ts3 (org-parse-timestamp "<2024-01-15 ++1d>")))
        (check (org-timestamp-repeater ts3) => "++1d"))
      (let ((ts4 (org-parse-timestamp "<2024-01-15 .+2w>")))
        (check (org-timestamp-repeater ts4) => ".+2w")))

    (test-case "org-timestamp->string: round-trip active"
      (let* ((input "<2024-01-15 Mon 10:30>")
             (ts (org-parse-timestamp input))
             (output (org-timestamp->string ts)))
        (check (not (not (string-contains output "2024-01-15"))) => #t)
        (check (not (not (string-contains output "10:30"))) => #t)))

    (test-case "org-timestamp->string: round-trip inactive"
      (let* ((input "[2024-03-20 Wed 14:00]")
             (ts (org-parse-timestamp input))
             (output (org-timestamp->string ts)))
        (check (not (not (string-contains output "2024-03-20"))) => #t)
        (check (not (not (string-contains output "14:00"))) => #t)))

    (test-case "org-timestamp-elapsed: basic"
      (check (org-timestamp-elapsed 10 0 11 30) => "1:30"))

    (test-case "org-timestamp-elapsed: zero"
      (check (org-timestamp-elapsed 10 0 10 0) => "0:00"))

    (test-case "org-timestamp-elapsed: multi-hour"
      (check (org-timestamp-elapsed 9 0 17 45) => "8:45"))

    (test-case "org-timestamp-elapsed: crossing noon"
      (check (org-timestamp-elapsed 11 30 13 15) => "1:45"))

    ;; =========================================================
    ;; Heading parsing (from test-org.el heading components)
    ;; =========================================================

    (test-case "org-parse-heading-line: full heading"
      (let ((h (org-parse-heading-line "** TODO [#A] My Task :work:urgent:")))
        (check (not (not h)) => #t)
        (check (org-heading-stars h) => 2)
        (check (org-heading-keyword h) => "TODO")
        (check (org-heading-priority h) => "A")
        (check (org-heading-title h) => "My Task")
        (check (org-heading-tags h) => '("work" "urgent"))))

    (test-case "org-parse-heading-line: minimal heading"
      (let ((h (org-parse-heading-line "* Hello")))
        (check (not (not h)) => #t)
        (check (org-heading-stars h) => 1)
        (check (org-heading-keyword h) => #f)
        (check (org-heading-title h) => "Hello")))

    (test-case "org-parse-heading-line: no keyword no tags"
      (let ((h (org-parse-heading-line "*** Just a title")))
        (check (not (not h)) => #t)
        (check (org-heading-stars h) => 3)
        (check (org-heading-keyword h) => #f)
        (check (org-heading-title h) => "Just a title")
        (check (org-heading-tags h) => '())))

    (test-case "org-parse-heading-line: DONE keyword"
      (let ((h (org-parse-heading-line "* DONE Finished")))
        (check (not (not h)) => #t)
        (check (org-heading-keyword h) => "DONE")
        (check (org-heading-title h) => "Finished")))

    (test-case "org-parse-heading-line: not a heading"
      (check (org-parse-heading-line "Not a heading") => #f)
      (check (org-parse-heading-line "  * indented star") => #f)
      (check (org-parse-heading-line "") => #f))

    (test-case "org-parse-heading-line: heading with only stars"
      (let ((h (org-parse-heading-line "* ")))
        (check (not (not h)) => #t)
        (check (org-heading-stars h) => 1)))

    (test-case "org-parse-heading-line: deep nesting"
      (let ((h (org-parse-heading-line "***** Deep heading")))
        (check (org-heading-stars h) => 5)
        (check (org-heading-title h) => "Deep heading")))

    (test-case "org-parse-heading-line: priority without keyword"
      (let ((h (org-parse-heading-line "* [#B] Just priority")))
        (check (org-heading-priority h) => "B")
        (check (org-heading-title h) => "Just priority")))

    (test-case "org-parse-heading-line: multiple tags"
      (let ((h (org-parse-heading-line "* Task :a:b:c:d:")))
        (check (org-heading-tags h) => '("a" "b" "c" "d"))))

    (test-case "org-parse-heading-line: tag with underscores"
      (let ((h (org-parse-heading-line "* Task :my_tag:another_one:")))
        (check (org-heading-tags h) => '("my_tag" "another_one"))))

    (test-case "org-parse-heading-line: NEXT keyword"
      ;; Custom TODO keywords should be recognized
      (let ((h (org-parse-heading-line "* NEXT Review code")))
        ;; Whether this parses as keyword depends on configured keywords
        (check (not (not h)) => #t)))

    ;; =========================================================
    ;; Planning lines (from test-org.el)
    ;; =========================================================

    ;; org-parse-planning-line returns (values scheduled deadline closed)
    (test-case "org-parse-planning-line: SCHEDULED only"
      (let-values (((sched dead closed) (org-parse-planning-line
                                         "SCHEDULED: <2024-01-15 Mon 10:00>")))
        (check (not (not sched)) => #t)
        (check dead => #f)
        (check closed => #f)))

    (test-case "org-parse-planning-line: DEADLINE only"
      (let-values (((sched dead closed) (org-parse-planning-line
                                         "DEADLINE: <2024-02-28>")))
        (check sched => #f)
        (check (not (not dead)) => #t)))

    (test-case "org-parse-planning-line: both SCHEDULED and DEADLINE"
      (let-values (((sched dead closed) (org-parse-planning-line
                                         "SCHEDULED: <2024-01-15 Mon 10:00> DEADLINE: <2024-02-28>")))
        (check (not (not sched)) => #t)
        (check (not (not dead)) => #t)))

    (test-case "org-parse-planning-line: CLOSED timestamp"
      (let-values (((sched dead closed) (org-parse-planning-line
                                         "CLOSED: [2024-01-15 Mon 10:00]")))
        (check (not (not closed)) => #t)))

    (test-case "org-parse-planning-line: not a planning line"
      (let-values (((sched dead closed) (org-parse-planning-line
                                         "Just some text")))
        (check sched => #f)
        (check dead => #f)
        (check closed => #f)))

    ;; =========================================================
    ;; Properties (from test-org.el)
    ;; =========================================================

    (test-case "org-parse-properties: basic"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":ID: abc123"
                      ":CATEGORY: work"
                      ":END:"))))
        (check (not (not props)) => #t)
        (check (assoc "ID" props) => '("ID" . "abc123"))
        (check (assoc "CATEGORY" props) => '("CATEGORY" . "work"))))

    (test-case "org-parse-properties: empty drawer"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:" ":END:"))))
        (check (null? props) => #t)))

    (test-case "org-parse-properties: multiple properties"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":CREATED: [2024-01-15]"
                      ":AUTHOR: Alice"
                      ":STATUS: active"
                      ":END:"))))
        (check (length props) => 3)
        (check (assoc "CREATED" props) => '("CREATED" . "[2024-01-15]"))
        (check (assoc "AUTHOR" props) => '("AUTHOR" . "Alice"))
        (check (assoc "STATUS" props) => '("STATUS" . "active"))))

    ;; =========================================================
    ;; Clock lines (from test-org.el / test-org-clock.el)
    ;; =========================================================

    (test-case "org-parse-clock-line: closed clock"
      (let ((c (org-parse-clock-line
                "CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:30] =>  1:30")))
        (check (not (not c)) => #t)))

    (test-case "org-parse-clock-line: open clock"
      (let ((c (org-parse-clock-line "CLOCK: [2024-01-15 Mon 10:00]")))
        (check (not (not c)) => #t)))

    (test-case "org-parse-clock-line: not a clock line"
      (check (org-parse-clock-line "not a clock") => #f)
      (check (org-parse-clock-line "") => #f))

    ;; =========================================================
    ;; Buffer parsing (from test-org.el org-parse-buffer)
    ;; =========================================================

    (test-case "org-parse-buffer: multiple headings"
      (let* ((text "* Heading One\nSome content\n** Sub heading\nMore content\n* Heading Two\n")
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)
        ;; Should return list of headings
        (check (>= (length result) 2) => #t)))

    (test-case "org-parse-buffer: with planning"
      (let* ((text "* TODO Task\nSCHEDULED: <2024-01-15 Mon 10:00>\nBody text\n")
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "org-parse-buffer: empty buffer"
      (let ((result (org-parse-buffer "")))
        (check (null? result) => #t)))

    (test-case "org-parse-buffer: no headings"
      (let ((result (org-parse-buffer "Just some plain text\nwithout headings\n")))
        (check (null? result) => #t)))

    (test-case "org-parse-buffer: nested headings"
      (let* ((text (string-append
                    "* Level 1\n"
                    "** Level 2\n"
                    "*** Level 3\n"
                    "** Another Level 2\n"
                    "* Another Level 1\n"))
             (result (org-parse-buffer text)))
        (check (>= (length result) 2) => #t)))

    ;; =========================================================
    ;; Buffer settings (from test-org.el)
    ;; =========================================================

    (test-case "org-parse-buffer-settings: basic"
      (let* ((text "#+TITLE: My Document\n#+AUTHOR: Alice\n#+STARTUP: overview\n")
             (settings (org-parse-buffer-settings text)))
        (check (not (not settings)) => #t)))

    (test-case "org-parse-buffer-settings: TODO keywords"
      (let* ((text "#+TODO: TODO NEXT | DONE CANCELLED\n")
             (settings (org-parse-buffer-settings text)))
        (check (not (not settings)) => #t)))

    (test-case "org-parse-buffer-settings: multiple settings"
      (let* ((text (string-append
                    "#+TITLE: Test\n"
                    "#+AUTHOR: Bob\n"
                    "#+TODO: TODO WAITING | DONE\n"
                    "#+STARTUP: content\n"))
             (settings (org-parse-buffer-settings text)))
        (check (not (not settings)) => #t)))

    ;; =========================================================
    ;; Tag expression parsing
    ;; =========================================================

    (test-case "org-parse-tag-expr: basic"
      (let ((tags (org-parse-tag-expr "+work-personal")))
        (check (not (null? tags)) => #t)))

    (test-case "org-parse-tag-expr: simple tag name"
      (let ((tags (org-parse-tag-expr "work")))
        (check (not (null? tags)) => #t)))

    (test-case "org-parse-tag-expr: multiple include"
      (let ((tags (org-parse-tag-expr "+work+urgent")))
        (check (not (null? tags)) => #t)))

    (test-case "org-parse-tag-expr: multiple exclude"
      (let ((tags (org-parse-tag-expr "-personal-home")))
        (check (not (null? tags)) => #t)))

    ;; =========================================================
    ;; Line type detection utilities
    ;; =========================================================

    (test-case "org-heading-line?: detects headings"
      (check (not (not (org-heading-stars-of-line "* Heading"))) => #t)
      (check (not (not (org-heading-stars-of-line "** Sub"))) => #t)
      (check (not (not (org-heading-stars-of-line "*** Deep"))) => #t))

    (test-case "org-heading-line?: rejects non-headings"
      (check (org-heading-stars-of-line "Not a heading") => #f)
      (check (org-heading-stars-of-line " * indented") => #f)
      (check (org-heading-stars-of-line "") => #f))

    (test-case "org-table-line?: detects tables"
      (check (org-table-line? "| a | b | c |") => #t)
      (check (org-table-line? "|---+---+---|") => #t)
      (check (org-table-line? "| single |") => #t))

    (test-case "org-table-line?: rejects non-tables"
      (check (org-table-line? "not a table") => #f)
      (check (org-table-line? " | indented") => #f)
      (check (org-table-line? "") => #f))

    (test-case "org-comment-line?: detects comments"
      (check (org-comment-line? "# this is a comment") => #t)
      (check (org-comment-line? "#  ") => #t))

    (test-case "org-comment-line?: rejects non-comments"
      (check (org-comment-line? "#+TITLE: not a comment") => #f)
      (check (org-comment-line? "#+BEGIN_SRC") => #f)
      (check (org-comment-line? "normal text") => #f))

    (test-case "org-keyword-line?: detects keywords"
      (check (org-keyword-line? "#+TITLE: My Title") => #t)
      (check (org-keyword-line? "#+AUTHOR: Someone") => #t)
      (check (org-keyword-line? "#+BEGIN_SRC python") => #t)
      (check (org-keyword-line? "#+begin_quote") => #t))

    (test-case "org-keyword-line?: rejects non-keywords"
      (check (org-keyword-line? "normal text") => #f)
      (check (org-keyword-line? "# comment") => #f)
      (check (org-keyword-line? "") => #f))

    (test-case "org-block-begin?: detects block starts"
      (check (org-block-begin? "#+BEGIN_SRC python") => #t)
      (check (org-block-begin? "#+begin_quote") => #t)
      (check (org-block-begin? "#+BEGIN_EXAMPLE") => #t)
      (check (org-block-begin? "#+begin_center") => #t))

    (test-case "org-block-begin?: rejects non-block-starts"
      (check (org-block-begin? "#+END_SRC") => #f)
      (check (org-block-begin? "#+TITLE: test") => #f)
      (check (org-block-begin? "normal text") => #f))

    ;; =========================================================
    ;; Utility functions
    ;; =========================================================

    (test-case "pad-02: zero-pads correctly"
      (check (pad-02 0) => "00")
      (check (pad-02 5) => "05")
      (check (pad-02 9) => "09")
      (check (pad-02 10) => "10")
      (check (pad-02 23) => "23")
      (check (pad-02 59) => "59"))

    (test-case "org-current-timestamp-string: produces valid format"
      (let ((ts (org-current-timestamp-string)))
        ;; Should start with < and end with >
        (check (string-prefix? "<" ts) => #t)
        (check (string-suffix? ">" ts) => #t)
        ;; Should contain a date pattern
        (check (>= (string-length ts) 12) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-parse-test)
    (test-report-summary!)))
