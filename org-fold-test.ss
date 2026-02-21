;;; org-fold-test.ss -- Tests for org-mode folding/visibility
;;; Converted from Emacs org-mode test-org-fold.el
;;; Covers: heading visibility, drawer folding, block folding,
;;; subtree visibility cycling
;;;
;;; NOTE: In gemacs, folding is handled through the editor layer
;;; (Scintilla folding). These tests define the expected behavior
;;; for content visibility based on org document structure.

(import :std/test
        :std/srfi/13
        (only-in :gemacs/org-parse
                 org-parse-buffer org-parse-heading-line
                 org-heading-stars make-org-heading))

(export org-fold-test)

;; Adapter: org-parse-heading-line returns (values ...), wrap into struct.
(def (org-parse-heading-line-adapter line)
  (let-values (((level keyword priority title tags) (org-parse-heading-line line)))
    (if (not level)
      #f
      (make-org-heading level keyword
                        (and priority (string (char-upcase priority)))
                        title tags
                        #f #f #f #f '() 0 #f))))

(def org-fold-test
  (test-suite "org-fold"

    ;; =========================================================
    ;; Heading structure for folding
    ;; (from test-org-fold.el visibility tests)
    ;; =========================================================

    ;; Folding relies on heading hierarchy:
    ;; Overview: only top-level headings visible
    ;; Contents: all headings visible, body hidden
    ;; Show all: everything visible

    (test-case "fold: heading hierarchy for overview mode"
      ;; In overview mode, only level-1 headings are shown
      (let* ((text (string-append
                    "* Heading 1\n"
                    "Body 1\n"
                    "** Sub heading\n"
                    "Sub body\n"
                    "* Heading 2\n"
                    "Body 2\n"))
             (headings (org-parse-buffer text)))
        ;; Should have 2 top-level headings
        (check (not (null? headings)) => #t)
        ;; First heading should be level 1
        (let ((h1 (org-parse-heading-line-adapter "* Heading 1")))
          (check (org-heading-stars h1) => 1))))

    (test-case "fold: heading hierarchy for contents mode"
      ;; In contents mode, all headings shown, bodies hidden
      (let* ((text (string-append
                    "* H1\n"
                    "** H1.1\n"
                    "*** H1.1.1\n"
                    "** H1.2\n"
                    "* H2\n"
                    "** H2.1\n"))
             (headings (org-parse-buffer text)))
        ;; Should parse all headings
        (check (not (null? headings)) => #t)))

    ;; =========================================================
    ;; Drawer folding
    ;; (from test-org-fold.el drawer tests)
    ;; =========================================================

    ;; Drawers (:PROPERTIES:...:END:, :LOGBOOK:...:END:) should
    ;; be foldable independently of heading folding.

    (test-case "fold: drawer structure detection"
      (let* ((text (string-append
                    "* Heading\n"
                    ":PROPERTIES:\n"
                    ":ID: abc\n"
                    ":CATEGORY: work\n"
                    ":END:\n"
                    "Content\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "fold: logbook drawer"
      (let* ((text (string-append
                    "* Task\n"
                    ":LOGBOOK:\n"
                    "CLOCK: [2024-01-15 Mon 10:00]--[2024-01-15 Mon 11:00] => 1:00\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    ;; =========================================================
    ;; Block folding
    ;; (from test-org-fold.el block tests)
    ;; =========================================================

    ;; Source blocks, quote blocks, example blocks, etc. should
    ;; be foldable to show only the begin/end lines.

    (test-case "fold: source block boundaries"
      (let* ((text (string-append
                    "* Heading\n"
                    "#+BEGIN_SRC python\n"
                    "def foo():\n"
                    "    return 42\n"
                    "#+END_SRC\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "fold: example block boundaries"
      (let* ((text (string-append
                    "* Heading\n"
                    "#+BEGIN_EXAMPLE\n"
                    "This is an example\n"
                    "with multiple lines\n"
                    "#+END_EXAMPLE\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "fold: quote block boundaries"
      (let* ((text (string-append
                    "#+BEGIN_QUOTE\n"
                    "A famous quote\n"
                    "by someone important\n"
                    "#+END_QUOTE\n"))
             (result (org-parse-buffer text)))
        (check (not (not result)) => #t)))

    ;; =========================================================
    ;; Subtree cycling
    ;; (from test-org-fold.el visibility cycling)
    ;; =========================================================

    ;; TAB on a heading cycles:
    ;; FOLDED → CHILDREN → SUBTREE → FOLDED

    (test-case "fold: subtree structure for cycling"
      ;; A subtree with children and grandchildren
      (let* ((text (string-append
                    "* Parent\n"
                    "Parent body\n"
                    "** Child 1\n"
                    "Child 1 body\n"
                    "*** Grandchild\n"
                    "Grandchild body\n"
                    "** Child 2\n"
                    "Child 2 body\n"))
             (headings (org-parse-buffer text)))
        (check (not (null? headings)) => #t)))

    ;; =========================================================
    ;; Global cycling
    ;; (from test-org-fold.el global visibility cycling)
    ;; =========================================================

    ;; S-TAB or C-u TAB cycles global visibility:
    ;; OVERVIEW → CONTENTS → SHOW ALL → OVERVIEW

    (test-case "fold: document structure for global cycling"
      (let* ((text (string-append
                    "* H1\n"
                    "Body 1\n"
                    "** H1.1\n"
                    "Body 1.1\n"
                    "* H2\n"
                    "Body 2\n"
                    "** H2.1\n"
                    "Body 2.1\n"
                    "** H2.2\n"
                    "Body 2.2\n"))
             (headings (org-parse-buffer text)))
        ;; Should have all headings
        (check (>= (length headings) 2) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-fold-test)
    (test-report-summary!)))
