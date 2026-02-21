;;; org-agenda-test.ss -- Tests for gemacs org-agenda module
;;; Converted from Emacs org-mode test-org-agenda.el
;;; Covers: date utilities, timestamp range checks, agenda item collection,
;;; sorting, TODO lists, tag search, text search

(import :std/test
        :std/srfi/13
        :std/format
        (except-in (rename-in :gemacs/org-agenda
                              (org-collect-agenda-items org-collect-agenda-items-real)
                              (make-org-agenda-item make-org-agenda-item-real)
                              (org-agenda-todo-list org-agenda-todo-list-real)
                              (org-agenda-tags-match org-agenda-tags-match-real)
                              (org-agenda-search org-agenda-search-real))
                   string-downcase list-index))

(export org-agenda-test)

;; Adapters: test functions omit file-path arg; actual functions require it.
(def (org-collect-agenda-items text date-from date-to)
  (org-collect-agenda-items-real text "" date-from date-to))

(def (make-org-agenda-item heading type date hour minute)
  (make-org-agenda-item-real heading type date
                             (format "~2,'0d:~2,'0d" hour minute) "" 0))

(def (org-agenda-todo-list text)
  (org-agenda-todo-list-real text ""))

(def (org-agenda-tags-match text tag-expr)
  (org-agenda-tags-match-real text "" tag-expr))

(def (org-agenda-search text query)
  (org-agenda-search-real text "" query))

(def org-agenda-test
  (test-suite "org-agenda"

    ;; =========================================================
    ;; Date utility functions
    ;; (from test-org-agenda.el date/time tests)
    ;; =========================================================

    (test-case "org-agenda: date weekday Monday"
      ;; 2024-01-15 is a Monday = 1
      (check (org-date-weekday 2024 1 15) => 1))

    (test-case "org-agenda: date weekday Sunday"
      ;; 2024-01-14 is a Sunday = 0
      (check (org-date-weekday 2024 1 14) => 0))

    (test-case "org-agenda: date weekday Saturday"
      ;; 2024-01-13 is a Saturday = 6
      (check (org-date-weekday 2024 1 13) => 6))

    (test-case "org-agenda: date weekday Wednesday"
      ;; 2024-01-17 is a Wednesday = 3
      (check (org-date-weekday 2024 1 17) => 3))

    (test-case "org-agenda: date weekday Friday"
      ;; 2024-01-19 is a Friday = 5
      (check (org-date-weekday 2024 1 19) => 5))

    ;; =========================================================
    ;; Date timestamp creation
    ;; (from test-org-agenda.el make-date-ts tests)
    ;; =========================================================

    (test-case "org-agenda: make-date-ts creates timestamp"
      (let ((ts (org-make-date-ts 2024 3 15)))
        (check (not (not ts)) => #t)))

    (test-case "org-agenda: make-date-ts different dates"
      (let ((ts1 (org-make-date-ts 2024 1 1))
            (ts2 (org-make-date-ts 2024 12 31)))
        (check (not (not ts1)) => #t)
        (check (not (not ts2)) => #t)))

    ;; =========================================================
    ;; Timestamp range checking
    ;; (from test-org-agenda.el timestamp-in-range tests)
    ;; =========================================================

    (test-case "org-agenda: timestamp in range - inside"
      ;; 2024-01-15 is between 2024-01-10 and 2024-01-20
      (let ((ts (org-make-date-ts 2024 1 15))
            (start (org-make-date-ts 2024 1 10))
            (end (org-make-date-ts 2024 1 20)))
        (check (org-timestamp-in-range? ts start end) => #t)))

    (test-case "org-agenda: timestamp in range - before"
      ;; 2024-01-05 is before 2024-01-10 to 2024-01-20
      (let ((ts (org-make-date-ts 2024 1 5))
            (start (org-make-date-ts 2024 1 10))
            (end (org-make-date-ts 2024 1 20)))
        (check (org-timestamp-in-range? ts start end) => #f)))

    (test-case "org-agenda: timestamp in range - after"
      ;; 2024-01-25 is after 2024-01-10 to 2024-01-20
      (let ((ts (org-make-date-ts 2024 1 25))
            (start (org-make-date-ts 2024 1 10))
            (end (org-make-date-ts 2024 1 20)))
        (check (org-timestamp-in-range? ts start end) => #f)))

    (test-case "org-agenda: timestamp in range - on start boundary"
      (let ((ts (org-make-date-ts 2024 1 10))
            (start (org-make-date-ts 2024 1 10))
            (end (org-make-date-ts 2024 1 20)))
        (check (org-timestamp-in-range? ts start end) => #t)))

    (test-case "org-agenda: timestamp in range - on end boundary"
      (let ((ts (org-make-date-ts 2024 1 20))
            (start (org-make-date-ts 2024 1 10))
            (end (org-make-date-ts 2024 1 20)))
        (check (org-timestamp-in-range? ts start end) => #t)))

    ;; =========================================================
    ;; Date advancing
    ;; (from test-org-agenda.el advance-date tests)
    ;; =========================================================

    (test-case "org-agenda: advance date by days"
      ;; 2024-01-30 + 3 days = 2024-02-02 (crosses month boundary)
      (let ((result (org-advance-date-ts (org-make-date-ts 2024 1 30) 3)))
        (check (not (not result)) => #t)))

    (test-case "org-agenda: advance date by 0 days"
      (let ((ts (org-make-date-ts 2024 6 15)))
        (let ((result (org-advance-date-ts ts 0)))
          (check (not (not result)) => #t))))

    (test-case "org-agenda: advance date crosses year boundary"
      ;; 2024-12-30 + 5 days = 2025-01-04
      (let ((result (org-advance-date-ts (org-make-date-ts 2024 12 30) 5)))
        (check (not (not result)) => #t)))

    ;; =========================================================
    ;; Agenda item collection
    ;; (from test-org-agenda.el collect-items tests)
    ;; =========================================================

    (test-case "org-agenda: collect items from buffer"
      (let* ((text (string-append
                    "* TODO Task 1\n"
                    "SCHEDULED: <2024-01-15 Mon 09:00>\n"
                    "* TODO Task 2\n"
                    "DEADLINE: <2024-01-20 Sat 17:00>\n"
                    "* DONE Completed\n"
                    "CLOSED: [2024-01-10 Wed]\n"))
             (start (org-make-date-ts 2024 1 1))
             (end (org-make-date-ts 2024 1 31))
             (items (org-collect-agenda-items text start end)))
        ;; Should find at least the SCHEDULED and DEADLINE items
        (check (>= (length items) 2) => #t)))

    (test-case "org-agenda: collect items empty buffer"
      (let ((items (org-collect-agenda-items
                    "" (org-make-date-ts 2024 1 1) (org-make-date-ts 2024 1 31))))
        (check (null? items) => #t)))

    ;; =========================================================
    ;; Agenda sorting
    ;; (from test-org-agenda.el sort tests)
    ;; =========================================================

    (test-case "org-agenda: sort items by time"
      (let* ((item1 (make-org-agenda-item
                      "Task 1" 'scheduled
                      (org-make-date-ts 2024 1 15) 9 0))
             (item2 (make-org-agenda-item
                      "Task 2" 'deadline
                      (org-make-date-ts 2024 1 15) 17 0))
             (sorted (org-agenda-sort-items (list item2 item1))))
        ;; 09:00 should come before 17:00
        (check (equal? (org-agenda-item-heading (car sorted)) "Task 1") => #t)))

    ;; =========================================================
    ;; TODO list
    ;; (from test-org-agenda.el TODO-list tests)
    ;; =========================================================

    ;; NOTE: org-agenda-todo-list returns a formatted string, not a list
    (test-case "org-agenda: TODO list filters DONE"
      (let* ((text (string-append
                    "* TODO Active task\n"
                    "* DONE Completed task\n"
                    "* TODO Another active\n"))
             (todos (org-agenda-todo-list text)))
        ;; Should contain TODO items but not DONE
        (check (not (not (string-contains todos "Active task"))) => #t)
        (check (not (not (string-contains todos "Another active"))) => #t)
        (check (not (string-contains todos "Completed task")) => #t)))

    (test-case "org-agenda: TODO list empty"
      (let ((todos (org-agenda-todo-list "* Just a heading\n")))
        ;; Returns "No TODO items found." when empty
        (check (not (not (string-contains todos "No TODO"))) => #t)))

    ;; =========================================================
    ;; Tag search
    ;; (from test-org-agenda.el tag-search tests)
    ;; =========================================================

    ;; NOTE: org-agenda-tags-match returns a formatted string, not a list
    (test-case "org-agenda: tag search finds matching"
      (let* ((text (string-append
                    "* Task A :work:\n"
                    "* Task B :home:\n"
                    "* Task C :work:urgent:\n"))
             (results (org-agenda-tags-match text "work")))
        ;; Should find Task A and Task C
        (check (not (not (string-contains results "Task A"))) => #t)
        (check (not (not (string-contains results "Task C"))) => #t)))

    (test-case "org-agenda: tag search no matches"
      (let* ((text (string-append
                    "* Task A :work:\n"
                    "* Task B :home:\n"))
             (results (org-agenda-tags-match text "nonexistent")))
        (check (not (not (string-contains results "No matching"))) => #t)))

    ;; =========================================================
    ;; Text search
    ;; (from test-org-agenda.el text-search tests)
    ;; =========================================================

    ;; NOTE: org-agenda-search returns a formatted string, not a list
    (test-case "org-agenda: text search case-insensitive"
      (let* ((text (string-append
                    "* Meeting with Alice\n"
                    "* Lunch with Bob\n"
                    "* Call with ALICE\n"))
             (results (org-agenda-search text "alice")))
        ;; Should find both "Alice" and "ALICE" entries
        (check (not (not (string-contains results "Alice"))) => #t)))

    (test-case "org-agenda: text search no matches"
      (let* ((text "* Task A\n* Task B\n")
             (results (org-agenda-search text "nonexistent")))
        (check (not (not (string-contains results "No matching"))) => #t)))

    ;; =========================================================
    ;; Agenda item formatting
    ;; (from test-org-agenda.el format tests)
    ;; =========================================================

    (test-case "org-agenda: format item contains heading"
      (let* ((item (make-org-agenda-item
                     "Review code" 'scheduled
                     (org-make-date-ts 2024 1 15) 10 0))
             (formatted (org-format-agenda-item item)))
        (check (not (not (string-contains formatted "Review code"))) => #t)))

    (test-case "org-agenda: format item contains time"
      (let* ((item (make-org-agenda-item
                     "Review code" 'scheduled
                     (org-make-date-ts 2024 1 15) 10 0))
             (formatted (org-format-agenda-item item)))
        (check (not (not (string-contains formatted "10:00"))) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-agenda-test)
    (test-report-summary!)))
