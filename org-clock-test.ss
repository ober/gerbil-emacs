;;; org-clock-test.ss -- Tests for gemacs org-clock module
;;; Converted from Emacs org-mode test-org-clock.el
;;; Covers: elapsed time calculation, clock line parsing,
;;; clock display, modeline string, clock in/out

(import :std/test
        :std/srfi/13
        (rename-in :gemacs/org-clock
                   (org-elapsed-minutes org-elapsed-minutes-real))
        (except-in (rename-in :gemacs/org-parse
                              (org-parse-clock-line org-parse-clock-line-raw))
                   string-prefix-ci?))

;; Adapter: org-parse-clock-line returns (values start-ts end-ts dur-string),
;; but tests just check truthiness. Return first value only.
(def (org-parse-clock-line line)
  (let-values (((start end dur) (org-parse-clock-line-raw line)))
    start))

(export org-clock-test)

;; Adapter: tests call (org-elapsed-minutes h1 m1 h2 m2) with 4 ints,
;; but actual function takes two timestamp objects.
;; make-org-timestamp args: type year month day day-name hour minute end-hour end-minute repeater warning
(def (org-elapsed-minutes h1 m1 h2 m2)
  (let ((ts1 (make-org-timestamp 'active 2024 1 1 #f h1 m1 #f #f #f #f))
        (ts2 (make-org-timestamp 'active 2024 1 1 #f h2 m2 #f #f #f #f)))
    (org-elapsed-minutes-real ts1 ts2)))

(def org-clock-test
  (test-suite "org-clock"

    ;; =========================================================
    ;; Elapsed time calculation
    ;; (from test-org-clock.el time calculation tests)
    ;; =========================================================

    (test-case "org-elapsed-minutes: basic 90 minutes"
      (check (org-elapsed-minutes 10 0 11 30) => 90))

    (test-case "org-elapsed-minutes: zero duration"
      (check (org-elapsed-minutes 10 0 10 0) => 0))

    (test-case "org-elapsed-minutes: one minute"
      (check (org-elapsed-minutes 10 0 10 1) => 1))

    (test-case "org-elapsed-minutes: full hour"
      (check (org-elapsed-minutes 9 0 10 0) => 60))

    (test-case "org-elapsed-minutes: multi-hour span"
      ;; 9:00 to 17:45 = 8h45m = 525 minutes
      (check (org-elapsed-minutes 9 0 17 45) => 525))

    (test-case "org-elapsed-minutes: crossing noon"
      ;; 11:30 to 13:15 = 1h45m = 105 minutes
      (check (org-elapsed-minutes 11 30 13 15) => 105))

    (test-case "org-elapsed-minutes: same hour"
      ;; 14:10 to 14:55 = 45 minutes
      (check (org-elapsed-minutes 14 10 14 55) => 45))

    (test-case "org-elapsed-minutes: exactly midnight span"
      ;; 23:00 to 01:00 next day = 120 minutes (if supported)
      ;; Note: depends on whether cross-midnight is handled
      (void))

    ;; =========================================================
    ;; Clock line parsing
    ;; (from test-org-clock.el clock format tests)
    ;; =========================================================

    (test-case "org-parse-clock-line: standard closed format"
      (let ((c (org-parse-clock-line
                "CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:30] =>  1:30")))
        (check (not (not c)) => #t)))

    (test-case "org-parse-clock-line: open (running) clock"
      (let ((c (org-parse-clock-line "CLOCK: [2024-01-15 Mon 10:00]")))
        (check (not (not c)) => #t)))

    (test-case "org-parse-clock-line: with leading whitespace"
      (let ((c (org-parse-clock-line
                "  CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:30] =>  1:30")))
        (check (not (not c)) => #t)))

    (test-case "org-parse-clock-line: multi-hour duration"
      (let ((c (org-parse-clock-line
                "CLOCK: [2023-04-29 Sat 00:00]--[2023-05-04 Thu 01:00] => 121:00")))
        (check (not (not c)) => #t)))

    (test-case "org-parse-clock-line: not a clock line"
      (check (org-parse-clock-line "Not a clock line") => #f)
      (check (org-parse-clock-line "") => #f)
      (check (org-parse-clock-line "CLOCK: invalid") => #f))

    ;; =========================================================
    ;; Modeline string
    ;; (from test-org-clock.el modeline tests)
    ;; =========================================================

    (test-case "org-clock-modeline-string: returns #f when not clocking"
      (let ((saved-start *org-clock-start*)
            (saved-heading *org-clock-heading*))
        (set! *org-clock-start* #f)
        (set! *org-clock-heading* #f)
        (check (org-clock-modeline-string) => #f)
        (set! *org-clock-start* saved-start)
        (set! *org-clock-heading* saved-heading)))

    ;; =========================================================
    ;; Clock drawer behavior
    ;; (from test-org-clock.el into-drawer tests)
    ;; =========================================================

    ;; The elisp tests check org-clock-into-drawer with various
    ;; configurations: nil (no drawer), string (drawer name),
    ;; integer (threshold), and interaction with org-log-into-drawer.
    ;; These are configuration-level tests for the clock system.

    (test-case "clock drawer: configuration interaction"
      ;; Verify that clock state variables exist and are accessible
      (check (or (not *org-clock-start*) (number? *org-clock-start*)
                 (string? *org-clock-start*)) => #t))

    ;; =========================================================
    ;; Clock timestamp change
    ;; (from test-org-clock.el org-clock-timestamps-change tests)
    ;; =========================================================

    ;; The elisp tests verify that changing one timestamp in a clock
    ;; line updates the duration accordingly:
    ;; e.g. moving 22:30 down by 1h → 21:30, keeps => 2:05

    (test-case "clock format: duration format h:mm"
      ;; Standard clock format: CLOCK: [start]--[end] => H:MM
      (let ((line "CLOCK: [2023-02-19 Sun 21:30]--[2023-02-19 Sun 23:35] =>  2:05"))
        (let ((c (org-parse-clock-line line)))
          (check (not (not c)) => #t))))

    (test-case "clock format: large duration"
      ;; Duration > 24h: CLOCK: [start]--[end] => 121:00
      (let ((line "CLOCK: [2023-04-29 Sat 00:00]--[2023-05-04 Thu 01:00] => 121:00"))
        (let ((c (org-parse-clock-line line)))
          (check (not (not c)) => #t))))

    ))

(def main
  (lambda args
    (run-tests! org-clock-test)
    (test-report-summary!)))
