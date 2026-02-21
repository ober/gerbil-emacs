;;; org-element-test.ss -- Tests for org element parsing
;;; Converted from Emacs org-mode test-org-element.el
;;; Covers: element type detection, property access, element creation,
;;; parsing of specific element types (headings, timestamps, blocks, etc.)

(import :std/test
        :std/srfi/13
        (except-in (rename-in :gemacs/org-parse
                              (org-parse-properties org-parse-properties-raw))
                   string-prefix-ci?))

(export org-element-test)

;; Adapter: tests call (org-parse-properties lines) with 1 arg,
;; but actual function takes (lines start-idx).
(def (org-parse-properties lines)
  (org-parse-properties-raw lines 0))

(def org-element-test
  (test-suite "org-element"

    ;; =========================================================
    ;; Element type detection
    ;; (from test-org-element.el type/property tests)
    ;; =========================================================

    (test-case "heading element: level detection"
      ;; org-element parsing of headlines at various levels
      (let ((h1 (org-parse-heading-line "* Level 1"))
            (h2 (org-parse-heading-line "** Level 2"))
            (h3 (org-parse-heading-line "*** Level 3"))
            (h4 (org-parse-heading-line "**** Level 4")))
        (check (org-heading-stars h1) => 1)
        (check (org-heading-stars h2) => 2)
        (check (org-heading-stars h3) => 3)
        (check (org-heading-stars h4) => 4)))

    (test-case "heading element: TODO keyword detection"
      (let ((todo (org-parse-heading-line "* TODO Task"))
            (done (org-parse-heading-line "* DONE Finished"))
            (none (org-parse-heading-line "* Plain heading")))
        (check (org-heading-keyword todo) => "TODO")
        (check (org-heading-keyword done) => "DONE")
        (check (org-heading-keyword none) => #f)))

    (test-case "heading element: priority extraction"
      ;; From test-org-element.el parser tests for headlines
      (let ((pri-a (org-parse-heading-line "* [#A] High priority"))
            (pri-b (org-parse-heading-line "* [#B] Medium priority"))
            (pri-c (org-parse-heading-line "* [#C] Low priority"))
            (no-pri (org-parse-heading-line "* No priority")))
        (check (org-heading-priority pri-a) => "A")
        (check (org-heading-priority pri-b) => "B")
        (check (org-heading-priority pri-c) => "C")
        (check (org-heading-priority no-pri) => #f)))

    (test-case "heading element: tag extraction"
      ;; From test-org-element.el parser tests for headlines
      (let ((tagged (org-parse-heading-line "* Task :work:urgent:"))
            (multi (org-parse-heading-line "* Project :a:b:c:d:e:"))
            (none (org-parse-heading-line "* No tags")))
        (check (org-heading-tags tagged) => '("work" "urgent"))
        (check (length (org-heading-tags multi)) => 5)
        (check (org-heading-tags none) => '())))

    (test-case "heading element: title extraction"
      (let ((h (org-parse-heading-line "** TODO [#A] Complex Title :tag1:tag2:")))
        (check (org-heading-title h) => "Complex Title")))

    ;; =========================================================
    ;; Timestamp element parsing
    ;; (from test-org-element.el timestamp parser tests)
    ;; =========================================================

    (test-case "timestamp element: active"
      (let ((ts (org-parse-timestamp "<2012-03-29 Thu>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'active)
        (check (org-timestamp-year ts) => 2012)
        (check (org-timestamp-month ts) => 3)
        (check (org-timestamp-day ts) => 29)))

    (test-case "timestamp element: active with time"
      (let ((ts (org-parse-timestamp "<2012-03-29 Thu 16:40>")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'active)))

    (test-case "timestamp element: inactive"
      (let ((ts (org-parse-timestamp "[2012-03-29 Thu]")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'inactive)))

    (test-case "timestamp element: inactive with time"
      (let ((ts (org-parse-timestamp "[2012-03-29 Thu 16:40]")))
        (check (org-timestamp? ts) => #t)
        (check (org-timestamp-type ts) => 'inactive)))

    (test-case "timestamp element: date range active"
      ;; Ranges like <2012-03-29 Thu>--<2012-03-30 Fri>
      ;; The parser handles the first timestamp
      (let ((ts (org-parse-timestamp "<2012-03-29 Thu>")))
        (check (org-timestamp? ts) => #t)))

    ;; =========================================================
    ;; Clock element parsing
    ;; (from test-org-element.el clock parser tests)
    ;; =========================================================

    (test-case "clock element: running clock"
      (let ((c (org-parse-clock-line "CLOCK: [2012-01-01 Sun 00:01]")))
        (check (not (not c)) => #t)))

    (test-case "clock element: closed clock with duration"
      (let ((c (org-parse-clock-line
                "CLOCK: [2012-01-01 Sun 00:01]--[2012-01-01 Sun 00:02] =>  0:01")))
        (check (not (not c)) => #t)))

    ;; =========================================================
    ;; Planning element parsing
    ;; (from test-org-element.el planning parser tests)
    ;; =========================================================

    ;; org-parse-planning-line returns (values scheduled deadline closed)
    (test-case "planning element: DEADLINE"
      (let-values (((sched dead closed) (org-parse-planning-line "DEADLINE: <2012-03-29 Thu>")))
        (check (not (not dead)) => #t)))

    (test-case "planning element: SCHEDULED"
      (let-values (((sched dead closed) (org-parse-planning-line "SCHEDULED: <2012-03-29 Thu>")))
        (check (not (not sched)) => #t)))

    (test-case "planning element: CLOSED"
      (let-values (((sched dead closed) (org-parse-planning-line "CLOSED: [2012-03-29 Thu]")))
        (check (not (not closed)) => #t)))

    (test-case "planning element: mixed keywords"
      (let-values (((sched dead closed) (org-parse-planning-line
                                         "DEADLINE: <2012-03-29 Thu> SCHEDULED: <2012-03-28 Wed>")))
        (check (not (not dead)) => #t)
        (check (not (not sched)) => #t)))

    ;; =========================================================
    ;; Property drawer parsing
    ;; (from test-org-element.el property drawer parser tests)
    ;; =========================================================

    (test-case "property drawer: standard format"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":PROP: value"
                      ":END:"))))
        (check (not (null? props)) => #t)
        (check (assoc "PROP" props) => '("PROP" . "value"))))

    (test-case "property drawer: multiple properties"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":A: 1"
                      ":B: 2"
                      ":C: 3"
                      ":END:"))))
        (check (length props) => 3)))

    (test-case "property drawer: empty"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:" ":END:"))))
        (check (null? props) => #t)))

    ;; =========================================================
    ;; Line type classification
    ;; (from test-org-element.el type detection)
    ;; =========================================================

    (test-case "line classification: comment"
      (check (org-comment-line? "# A comment") => #t)
      (check (org-comment-line? "#  ") => #t)
      (check (org-comment-line? "#+KEYWORD:") => #f))

    (test-case "line classification: keyword"
      (check (org-keyword-line? "#+TITLE: Title") => #t)
      (check (org-keyword-line? "#+RESULTS:") => #t)
      (check (org-keyword-line? "# comment") => #f))

    (test-case "line classification: block begin"
      (check (org-block-begin? "#+BEGIN_SRC emacs-lisp") => #t)
      (check (org-block-begin? "#+begin_example") => #t)
      (check (org-block-begin? "#+begin_quote") => #t)
      (check (org-block-begin? "#+begin_verse") => #t)
      (check (org-block-begin? "#+begin_center") => #t)
      (check (org-block-begin? "#+BEGIN_COMMENT") => #t)
      (check (org-block-begin? "#+END_SRC") => #f))

    (test-case "line classification: table"
      (check (org-table-line? "| cell |") => #t)
      (check (org-table-line? "| a | b | c |") => #t)
      (check (org-table-line? "text") => #f))

    (test-case "line classification: heading"
      (check (not (not (org-heading-stars-of-line "* H"))) => #t)
      (check (not (not (org-heading-stars-of-line "** H"))) => #t)
      (check (org-heading-stars-of-line "text") => #f))

    ;; =========================================================
    ;; Buffer-level parsing
    ;; (from test-org-element.el interpretation/mapping tests)
    ;; =========================================================

    (test-case "buffer parsing: simple document"
      (let ((result (org-parse-buffer "* H1\nParagraph\n* H2\n")))
        (check (>= (length result) 2) => #t)))

    (test-case "buffer parsing: nested structure"
      (let ((result (org-parse-buffer
                     (string-append
                      "* H1\n"
                      "** H1.1\n"
                      "** H1.2\n"
                      "* H2\n"
                      "** H2.1\n"))))
        (check (not (null? result)) => #t)))

    (test-case "buffer parsing: with properties"
      (let ((result (org-parse-buffer
                     (string-append
                      "* H1\n"
                      ":PROPERTIES:\n"
                      ":ID: abc\n"
                      ":END:\n"
                      "Content\n"))))
        (check (not (null? result)) => #t)))

    (test-case "buffer parsing: with planning"
      (let ((result (org-parse-buffer
                     (string-append
                      "* TODO Task\n"
                      "SCHEDULED: <2024-01-15>\n"
                      "Content\n"))))
        (check (not (null? result)) => #t)))

    (test-case "buffer parsing: with clock"
      (let ((result (org-parse-buffer
                     (string-append
                      "* Task\n"
                      "CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:00] => 1:00\n"))))
        (check (not (null? result)) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-element-test)
    (test-report-summary!)))
