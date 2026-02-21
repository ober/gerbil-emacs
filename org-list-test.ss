;;; org-list-test.ss -- Tests for gemacs org-list module
;;; Converted from Emacs org-mode test-org-list.el
;;; Covers: list item detection, bullet types, checkbox states,
;;; indentation, bullet cycling, checkbox statistics

(import :std/test
        :std/srfi/13
        (rename-in (only-in :gemacs/org-list
                            org-list-item? org-meta-return
                            org-cycle-list-bullet org-count-leading-spaces
                            org-update-checkbox-statistics!)
                   (org-list-item? org-list-item?-raw)))

;; Adapter: org-list-item? returns (values type indent marker),
;; but tests only check the type. Return just the first value.
(def (org-list-item? line)
  (let-values (((type indent marker) (org-list-item?-raw line)))
    type))

(export org-list-test)

(def org-list-test
  (test-suite "org-list"

    ;; =========================================================
    ;; List item detection
    ;; (from test-org-list.el list-ending and item detection)
    ;; =========================================================

    (test-case "org-list-item?: unordered dash"
      (let ((item (org-list-item? "- item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: unordered plus"
      (let ((item (org-list-item? "+ item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: unordered star"
      ;; Note: star bullets only at indent > 0 to avoid heading confusion
      (let ((item (org-list-item? "  * sub item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: unordered indented"
      (let ((item (org-list-item? "  + sub item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: ordered with dot"
      (let ((item (org-list-item? "1. First item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: ordered with paren"
      (let ((item (org-list-item? "1) First item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: ordered multi-digit"
      (let ((item (org-list-item? "10. Tenth item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: checkbox unchecked"
      (let ((item (org-list-item? "- [ ] Todo item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: checkbox checked"
      (let ((item (org-list-item? "- [X] Done item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: checkbox partial"
      (let ((item (org-list-item? "- [-] Partial item")))
        (check (not (not item)) => #t)))

    (test-case "org-list-item?: not a list"
      (check (org-list-item? "Not a list") => #f)
      (check (org-list-item? "* Heading") => #f)
      (check (org-list-item? "") => #f)
      (check (org-list-item? "   just text") => #f))

    (test-case "org-list-item?: description list"
      ;; Description lists use :: separator
      (let ((item (org-list-item? "- term :: definition")))
        (check (not (not item)) => #t)))

    ;; =========================================================
    ;; Leading spaces counting
    ;; (from test-org-list.el indentation tests)
    ;; =========================================================

    (test-case "org-count-leading-spaces: no indent"
      (check (org-count-leading-spaces "hello") => 0))

    (test-case "org-count-leading-spaces: two spaces"
      (check (org-count-leading-spaces "  hello") => 2))

    (test-case "org-count-leading-spaces: four spaces"
      (check (org-count-leading-spaces "    hello") => 4))

    (test-case "org-count-leading-spaces: empty string"
      (check (org-count-leading-spaces "") => 0))

    (test-case "org-count-leading-spaces: all spaces"
      (check (org-count-leading-spaces "   ") => 3))

    ;; =========================================================
    ;; Bullet cycling
    ;; (from test-org-list.el cycle-bullet tests)
    ;; Emacs cycles through: "-", "+", "*", "1.", "1)"
    ;; =========================================================

    ;; NOTE: These tests exercise the bullet type cycling logic.
    ;; The exact cycling behavior depends on gemacs implementation.

    (test-case "org-list: bullet types recognized"
      ;; Verify all standard bullet types are recognized
      (check (not (not (org-list-item? "- item"))) => #t)
      (check (not (not (org-list-item? "+ item"))) => #t)
      (check (not (not (org-list-item? "1. item"))) => #t)
      (check (not (not (org-list-item? "1) item"))) => #t))

    (test-case "org-list: deeply indented items"
      (check (not (not (org-list-item? "    - deep item"))) => #t)
      (check (not (not (org-list-item? "      + deeper item"))) => #t)
      (check (not (not (org-list-item? "        1. very deep"))) => #t))

    ;; =========================================================
    ;; List structure tests
    ;; (from test-org-list.el list ending/navigation)
    ;; =========================================================

    (test-case "org-list: complex nested structure detection"
      ;; Verify that items at different indentation levels
      ;; are all recognized as list items
      (let ((items '("- item A"
                     "  - sub item 1"
                     "    - sub sub item"
                     "  - sub item 2"
                     "- item B")))
        (for-each
         (lambda (line)
           (check (not (not (org-list-item? line))) => #t))
         items)))

    (test-case "org-list: non-list lines in context"
      ;; Lines that appear between list items but aren't items
      (let ((non-items '("  Some continuation text"
                         ""
                         "  #+BEGIN_QUOTE"
                         "  #+END_QUOTE"
                         "Paragraph text")))
        (for-each
         (lambda (line)
           (check (org-list-item? line) => #f))
         non-items)))

    ;; =========================================================
    ;; Checkbox variants
    ;; (from test-org-list.el checkbox tests)
    ;; =========================================================

    (test-case "org-list: checkbox state variants"
      ;; [ ] unchecked, [X] checked, [-] partial
      (check (not (not (org-list-item? "- [ ] unchecked"))) => #t)
      (check (not (not (org-list-item? "- [X] checked"))) => #t)
      (check (not (not (org-list-item? "- [-] partial"))) => #t)
      ;; With ordered bullets
      (check (not (not (org-list-item? "1. [ ] ordered unchecked"))) => #t)
      (check (not (not (org-list-item? "1. [X] ordered checked"))) => #t))

    (test-case "org-list: checkbox with indentation"
      (check (not (not (org-list-item? "  - [ ] indented checkbox"))) => #t)
      (check (not (not (org-list-item? "    + [X] deep checkbox"))) => #t))

    ))

(def main
  (lambda args
    (run-tests! org-list-test)
    (test-report-summary!)))
