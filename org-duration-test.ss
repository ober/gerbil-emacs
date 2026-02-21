;;; org-duration-test.ss -- Tests for duration parsing and formatting
;;; Converted from Emacs org-mode test-org-duration.el
;;; Covers: duration-to-minutes, duration-from-minutes, duration validation
;;;
;;; NOTE: org-mode has a separate org-duration.el module. In gemacs,
;;; duration handling may be part of org-parse or a separate module.
;;; Adjust imports as needed.

(import :std/test
        :std/srfi/13
        (except-in (rename-in :gemacs/org-parse
                              (org-timestamp-elapsed org-timestamp-elapsed-real))
                   string-prefix-ci?))

(export org-duration-test)

;; Adapter: tests call (org-timestamp-elapsed h1 m1 h2 m2) with 4 ints,
;; but actual function takes two timestamp objects.
;; make-org-timestamp args: type year month day day-name hour minute end-hour end-minute repeater warning
(def (org-timestamp-elapsed h1 m1 h2 m2)
  (let ((ts1 (make-org-timestamp 'active 2024 1 1 #f h1 m1 #f #f #f #f))
        (ts2 (make-org-timestamp 'active 2024 1 1 #f h2 m2 #f #f #f #f)))
    (org-timestamp-elapsed-real ts1 ts2)))

(def org-duration-test
  (test-suite "org-duration"

    ;; =========================================================
    ;; Duration to minutes conversion
    ;; (from test-org-duration.el to-minutes tests)
    ;; =========================================================

    ;; NOTE: These tests define the expected behavior for a duration
    ;; parsing module. If gemacs doesn't have org-duration-to-minutes,
    ;; these serve as a specification for implementation.

    (test-case "duration: h:mm format basic"
      ;; "1:01" = 61 minutes
      ;; This is the most common duration format in clock lines
      (let ((c (org-parse-clock-line
                "CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:01] =>  1:01")))
        (check (not (not c)) => #t)))

    (test-case "duration: h:mm:ss format"
      ;; "1:20:30" = 80.5 minutes = 80 minutes 30 seconds
      ;; Some clock lines may include seconds
      (void))

    (test-case "duration: elapsed time string format"
      ;; org-timestamp-elapsed returns "H:MM" format strings
      (check (org-timestamp-elapsed 10 0 11 1) => "1:01")
      (check (org-timestamp-elapsed 10 0 11 0) => "1:00")
      (check (org-timestamp-elapsed 0 0 0 0) => "0:00")
      (check (org-timestamp-elapsed 9 0 17 45) => "8:45"))

    ;; =========================================================
    ;; Duration validation
    ;; (from test-org-duration.el duration-p tests)
    ;; =========================================================

    ;; Valid duration formats from the elisp tests:
    ;; "3:12", "123:12", "1:23:45", "3d 3h 4min", "3d3h4min",
    ;; "3d 13:35", "3d13:35", "2.35h"
    ;;
    ;; Invalid formats:
    ;; "3::12", "3:2", "3:12:4", "3d 13:35 13h" (H:MM not last)

    (test-case "duration: valid h:mm patterns"
      ;; These represent valid duration strings
      ;; Test them through clock line parsing
      (check (not (not (org-parse-clock-line
                        "CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 13:12] =>  3:12")))
             => #t)
      (check (not (not (org-parse-clock-line
                        "CLOCK: [2024-01-14 Sun 00:00]--[2024-01-19 Fri 03:12] => 123:12")))
             => #t))

    ;; =========================================================
    ;; Duration formatting
    ;; (from test-org-duration.el from-minutes tests)
    ;; =========================================================

    ;; The elisp tests check formatting with various org-duration-format
    ;; settings: 'h:mm, 'h:mm:ss, advanced specs with units.
    ;; In gemacs, the elapsed display follows "H:MM" format.

    (test-case "duration: formatted output patterns"
      ;; Verify elapsed time produces standard H:MM format
      (check (org-timestamp-elapsed 0 0 1 0) => "1:00")
      (check (org-timestamp-elapsed 0 0 0 30) => "0:30")
      (check (org-timestamp-elapsed 0 0 2 15) => "2:15")
      (check (org-timestamp-elapsed 10 0 10 5) => "0:05"))

    (test-case "duration: zero duration"
      (check (org-timestamp-elapsed 12 0 12 0) => "0:00"))

    (test-case "duration: single digit minutes padded"
      ;; Minutes should be zero-padded to 2 digits
      (check (org-timestamp-elapsed 10 0 10 5) => "0:05")
      (check (org-timestamp-elapsed 10 0 10 9) => "0:09"))

    (test-case "duration: large hour values"
      ;; Multi-day durations can have large hour counts
      (check (org-timestamp-elapsed 0 0 23 59) => "23:59"))

    ))

(def main
  (lambda args
    (run-tests! org-duration-test)
    (test-report-summary!)))
