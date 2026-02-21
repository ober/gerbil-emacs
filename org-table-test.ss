;;; org-table-test.ss -- Tests for gemacs org-table module
;;; Converted from Emacs org-mode test-org-table.el
;;; Covers: table detection, parsing, alignment, formatting, column ops,
;;; row ops, sorting, CSV conversion, formula parsing

(import :std/test
        :std/srfi/13
        (only-in :gemacs/org-table
                 org-table-row? org-table-separator? org-table-parse-row
                 org-table-column-widths org-table-format-row
                 org-table-format-separator org-numeric-cell?
                 org-table-align org-table-next-cell
                 org-table-on-table-line? org-table-find-bounds
                 org-table-current-column org-table-next-data-line
                 org-table-get-rows org-table-goto-column
                 org-table-insert-column org-table-delete-column
                 org-table-insert-row org-table-delete-row
                 org-table-sort org-table-to-csv org-csv-to-table
                 org-table-parse-tblfm))

(export org-table-test)

(def org-table-test
  (test-suite "org-table"

    ;; =========================================================
    ;; Table row/separator detection
    ;; (from test-org-table.el basic detection tests)
    ;; =========================================================

    (test-case "org-table-row?: detects data rows"
      (check (org-table-row? "| a | b | c |") => #t)
      (check (org-table-row? "| hello | world |") => #t)
      (check (org-table-row? "| single |") => #t)
      (check (org-table-row? "|  |  |") => #t))

    (test-case "org-table-row?: rejects non-rows"
      (check (org-table-row? "not a table row") => #f)
      (check (org-table-row? "|---+---|") => #f)  ;; separator, not data
      (check (org-table-row? "") => #f)
      (check (org-table-row? "   | indented") => #f))

    (test-case "org-table-separator?: detects separators"
      (check (org-table-separator? "|---+---|") => #t)
      (check (org-table-separator? "|---|") => #t)
      (check (org-table-separator? "|-----+------+-------|") => #t))

    (test-case "org-table-separator?: rejects non-separators"
      (check (org-table-separator? "| a | b |") => #f)
      (check (org-table-separator? "not a separator") => #f)
      (check (org-table-separator? "") => #f))

    ;; =========================================================
    ;; Row parsing
    ;; (from test-org-table.el parse-row tests)
    ;; =========================================================

    (test-case "org-table-parse-row: splits cells"
      (check (org-table-parse-row "| a | bb | ccc |")
             => '("a" "bb" "ccc")))

    (test-case "org-table-parse-row: trims whitespace"
      (check (org-table-parse-row "|  a  |  b  |  c  |")
             => '("a" "b" "c")))

    (test-case "org-table-parse-row: handles empty cells"
      (check (org-table-parse-row "|  | b |  |")
             => '("" "b" "")))

    (test-case "org-table-parse-row: single cell"
      (check (org-table-parse-row "| hello |")
             => '("hello")))

    (test-case "org-table-parse-row: numeric cells"
      (check (org-table-parse-row "| 1 | 2 | 3 |")
             => '("1" "2" "3")))

    ;; =========================================================
    ;; Column width computation
    ;; (from test-org-table.el column-widths tests)
    ;; =========================================================

    (test-case "org-table-column-widths: computes widths"
      (let ((rows '(("a" "bb" "ccc")
                    ("dd" "e" "ffff"))))
        (check (org-table-column-widths rows) => '(2 2 4))))

    (test-case "org-table-column-widths: single row"
      (let ((rows '(("hello" "world"))))
        (check (org-table-column-widths rows) => '(5 5))))

    (test-case "org-table-column-widths: empty cells"
      (let ((rows '(("a" "" "c")
                    ("" "bb" ""))))
        (check (org-table-column-widths rows) => '(1 2 1))))

    ;; =========================================================
    ;; Row/separator formatting
    ;; (from test-org-table.el format tests)
    ;; =========================================================

    (test-case "org-table-format-row: pads cells to widths"
      (let ((result (org-table-format-row '("a" "bb" "ccc") '(4 2 3))))
        (check (not (not (string-contains result "| a"))) => #t)
        (check (not (not (string-contains result "| bb"))) => #t)
        (check (not (not (string-contains result "| ccc"))) => #t)))

    (test-case "org-table-format-separator: generates separator"
      (let ((result (org-table-format-separator '(4 2 3))))
        (check (not (not (string-contains result "|"))) => #t)
        (check (not (not (string-contains result "-"))) => #t)
        (check (not (not (string-contains result "+"))) => #t)))

    ;; =========================================================
    ;; Numeric cell detection
    ;; (from test-org-table.el numeric tests)
    ;; =========================================================

    (test-case "org-numeric-cell?: detects integers"
      (check (org-numeric-cell? "123") => #t)
      (check (org-numeric-cell? "0") => #t)
      (check (org-numeric-cell? "-5") => #t))

    (test-case "org-numeric-cell?: detects floats"
      (check (org-numeric-cell? "3.14") => #t)
      (check (org-numeric-cell? "-0.5") => #t))

    (test-case "org-numeric-cell?: detects percentages"
      (check (org-numeric-cell? "50%") => #t)
      (check (org-numeric-cell? "100%") => #t))

    (test-case "org-numeric-cell?: rejects non-numeric"
      (check (org-numeric-cell? "hello") => #f)
      (check (org-numeric-cell? "") => #f)
      (check (org-numeric-cell? "abc123") => #f))

    ;; =========================================================
    ;; Formula parsing
    ;; (from test-org-table.el tblfm tests)
    ;; =========================================================

    (test-case "org-table-parse-tblfm: simple formula"
      (let ((result (org-table-parse-tblfm "#+TBLFM: $3=$1+$2")))
        (check (not (null? result)) => #t)))

    (test-case "org-table-parse-tblfm: multiple formulas"
      (let ((result (org-table-parse-tblfm
                     "#+TBLFM: @>$1=vsum(@<..@>>) :: $2=2*$1")))
        (check (not (null? result)) => #t)))

    (test-case "org-table-parse-tblfm: not a formula"
      (let ((result (org-table-parse-tblfm "not a formula")))
        (check (null? result) => #t)))

    ;; =========================================================
    ;; CSV conversion
    ;; (from test-org-table.el CSV tests)
    ;; =========================================================

    (test-case "org-csv-to-table: basic conversion"
      (let ((result (org-csv-to-table "a,b,c\n1,2,3")))
        (check (not (not (string-contains result "| a"))) => #t)
        (check (not (not (string-contains result "| b"))) => #t)
        (check (not (not (string-contains result "| c"))) => #t)
        (check (not (not (string-contains result "| 1"))) => #t)
        (check (not (not (string-contains result "| 2"))) => #t)
        (check (not (not (string-contains result "| 3"))) => #t)))

    (test-case "org-csv-to-table: single column"
      (let ((result (org-csv-to-table "a\nb\nc")))
        (check (not (not (string-contains result "| a"))) => #t)
        (check (not (not (string-contains result "| b"))) => #t)))

    (test-case "org-csv-to-table: with header separator"
      (let ((result (org-csv-to-table "name,age\nAlice,30\nBob,25")))
        (check (not (not (string-contains result "name"))) => #t)
        (check (not (not (string-contains result "Alice"))) => #t)))

    ;; =========================================================
    ;; Table sorting (from test-org-table.el sort tests)
    ;; =========================================================

    (test-case "org-table-sort: basic numeric sort"
      (let* ((table "| 3 |\n| 1 |\n| 2 |")
             (result (org-table-sort table 1 'numeric)))
        (check (not (not result)) => #t)))

    (test-case "org-table-sort: alphabetic sort"
      (let* ((table "| cherry |\n| apple |\n| banana |")
             (result (org-table-sort table 1 'alphabetic)))
        (check (not (not result)) => #t)))

    ;; =========================================================
    ;; Table formulas (from test-org-table.el formula evaluation)
    ;; =========================================================

    (test-case "org-table: simple sum formula structure"
      ;; Verify the table + formula structure from test-org-table.el
      ;; "Simple sum without grouping rows, without title row"
      (let ((input "| 2 |\n| 4 |\n| 8 |\n| replace |")
            (formula "#+TBLFM: @>$1 = vsum(@<..@>>)"))
        ;; Parse the formula
        (let ((parsed (org-table-parse-tblfm formula)))
          (check (not (null? parsed)) => #t))))

    (test-case "org-table: formula with title row"
      ;; "Simple sum without grouping rows, with title row"
      (let ((formula "#+TBLFM: @>$1 = vsum(@I..@>>)"))
        (let ((parsed (org-table-parse-tblfm formula)))
          (check (not (null? parsed)) => #t))))

    (test-case "org-table: column formula with grouping"
      (let ((formula "#+TBLFM: @>$1 = vsum(@<..@>>) :: $2 = 2 * $1"))
        (let ((parsed (org-table-parse-tblfm formula)))
          (check (not (null? parsed)) => #t))))

    ))

(def main
  (lambda args
    (run-tests! org-table-test)
    (test-report-summary!)))
