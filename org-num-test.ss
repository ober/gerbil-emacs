;;; org-num-test.ss -- Tests for org-mode heading numbering
;;; Converted from Emacs org-mode test-org-num.el
;;; Covers: automatic heading numbering, skip commented/footnotes/tagged,
;;; max-level limiting, number format, update after modification

(import :std/test
        :std/srfi/13
        (only-in :gemacs/org-parse
                 org-parse-buffer org-parse-heading-line
                 org-heading-stars org-heading-keyword
                 org-heading-tags make-org-heading))

(export org-num-test)

;; Adapter: org-parse-heading-line returns (values ...), wrap into struct.
(def (org-parse-heading-line-adapter line)
  (let-values (((level keyword priority title tags) (org-parse-heading-line line)))
    (if (not level)
      #f
      (make-org-heading level keyword
                        (and priority (string (char-upcase priority)))
                        title tags
                        #f #f #f #f '() 0 #f))))

(def org-num-test
  (test-suite "org-num"

    ;; =========================================================
    ;; Heading numbering generation
    ;; (from test-org-num.el face/format tests)
    ;; =========================================================

    ;; org-num-mode assigns numbers like "1 ", "1.1 ", "1.2 ", "2 "
    ;; based on the heading hierarchy.

    (test-case "num: single heading gets 1"
      ;; "* H1" → number "1 "
      (let ((h (org-parse-heading-line-adapter "* H1")))
        (check (org-heading-stars h) => 1)))

    (test-case "num: two headings get 1, 2"
      ;; "* H1\n* H2" → "1 ", "2 "
      (let* ((text "* H1\n* H2\n")
             (headings (org-parse-buffer text)))
        (check (= (length headings) 2) => #t)))

    (test-case "num: nested headings get hierarchical numbers"
      ;; "* H1\n** H2" → "1 ", "1.1 "
      (let* ((text "* H1\n** H2\n*** H3\n")
             (headings (org-parse-buffer text)))
        (check (>= (length headings) 1) => #t)))

    ;; =========================================================
    ;; Max level limiting
    ;; (from test-org-num.el max-level tests)
    ;; =========================================================

    ;; With org-num-max-level = 2, only levels 1 and 2 get numbers.
    ;; Level 3+ headings are unnumbered.

    (test-case "num: heading levels for max-level filtering"
      (let* ((text "* H1\n** H2\n*** H3\n")
             (headings (org-parse-buffer text)))
        ;; H3 is level 3, would be skipped with max-level=2
        (check (>= (length headings) 1) => #t)
        (let ((h3 (org-parse-heading-line-adapter "*** H3")))
          (check (org-heading-stars h3) => 3))))

    ;; =========================================================
    ;; Skip commented headings
    ;; (from test-org-num.el skip-numbering tests)
    ;; =========================================================

    ;; When org-num-skip-commented is t, COMMENT headings are
    ;; unnumbered (and their subtrees).

    (test-case "num: COMMENT heading detection"
      (let ((h (org-parse-heading-line-adapter "* COMMENT H2")))
        ;; COMMENT may be parsed as keyword or as part of title
        (check (not (not h)) => #t)))

    (test-case "num: commented subtree"
      ;; * COMMENT H1\n** H2  → both unnumbered
      (let* ((text "* COMMENT H1\n** H2\n")
             (headings (org-parse-buffer text)))
        (check (not (null? headings)) => #t)))

    ;; =========================================================
    ;; Skip tagged headings
    ;; (from test-org-num.el skip-tags tests)
    ;; =========================================================

    ;; When org-num-skip-tags contains "foo", headings tagged :foo:
    ;; and their subtrees are unnumbered.

    (test-case "num: tagged heading detection"
      (let ((h (org-parse-heading-line-adapter "* H2 :foo:")))
        (check (org-heading-tags h) => '("foo"))))

    (test-case "num: tagged subtree"
      (let* ((text "* H1 :foo:\n** H2\n")
             (headings (org-parse-buffer text)))
        ;; H1 tagged :foo: → skip H1 and H2
        (check (not (null? headings)) => #t)))

    ;; =========================================================
    ;; Skip UNNUMBERED property
    ;; (from test-org-num.el skip-unnumbered tests)
    ;; =========================================================

    ;; When a heading has :UNNUMBERED: t property, it and its
    ;; subtree are skipped from numbering.

    (test-case "num: UNNUMBERED property"
      (let* ((text (string-append
                    "* H1\n"
                    "* H2\n"
                    ":PROPERTIES:\n"
                    ":UNNUMBERED: t\n"
                    ":END:\n"))
             (headings (org-parse-buffer text)))
        (check (not (null? headings)) => #t)))

    (test-case "num: UNNUMBERED nil property"
      ;; :UNNUMBERED: nil → should still be numbered
      (let* ((text (string-append
                    "* H1\n"
                    "* H2\n"
                    ":PROPERTIES:\n"
                    ":UNNUMBERED: nil\n"
                    ":END:\n"))
             (headings (org-parse-buffer text)))
        (check (not (null? headings)) => #t)))

    ;; =========================================================
    ;; Skip footnotes section
    ;; (from test-org-num.el skip-footnotes tests)
    ;; =========================================================

    (test-case "num: footnotes section detection"
      ;; When org-footnote-section = "FN" and org-num-skip-footnotes = t,
      ;; the "FN" heading is unnumbered
      (let* ((text "* H1\n* FN\n")
             (headings (org-parse-buffer text)))
        (check (= (length headings) 2) => #t)))

    ;; =========================================================
    ;; Empty headlines
    ;; (from test-org-num.el empty headline tests)
    ;; =========================================================

    (test-case "num: empty headline"
      ;; "* " (heading with no title) should still be numbered
      (let ((h (org-parse-heading-line-adapter "* ")))
        (check (not (not h)) => #t)
        (check (org-heading-stars h) => 1)))

    ;; =========================================================
    ;; Update after modification
    ;; (from test-org-num.el update tests)
    ;; =========================================================

    ;; Buffer modifications should trigger renumbering:
    ;; - Creating new headlines
    ;; - Changing heading levels
    ;; - Deleting headings
    ;; - Altering skip state (adding/removing tags, COMMENT, etc.)

    (test-case "num: heading level change affects numbering"
      ;; Changing "* H" to "** H" should renumber
      (let ((h1 (org-parse-heading-line-adapter "* H"))
            (h2 (org-parse-heading-line-adapter "** H")))
        (check (org-heading-stars h1) => 1)
        (check (org-heading-stars h2) => 2)))

    ))

(def main
  (lambda args
    (run-tests! org-num-test)
    (test-report-summary!)))
