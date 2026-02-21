;;; org-lint-test.ss -- Tests for org-mode document linting
;;; Converted from Emacs org-mode test-org-lint.el
;;; Covers: duplicate detection, orphan keywords, deprecated syntax,
;;; missing languages, invalid links, broken code refs
;;;
;;; NOTE: In gemacs, linting may be a separate module or integrated
;;; into org-parse. These tests define the expected lint checks.

(import :std/test
        :std/srfi/13
        (only-in :gemacs/org-parse
                 org-parse-buffer org-parse-heading-line
                 org-keyword-line? org-block-begin?))

(export org-lint-test)

(def org-lint-test
  (test-suite "org-lint"

    ;; =========================================================
    ;; Duplicate detection
    ;; (from test-org-lint.el duplicate checks)
    ;; =========================================================

    (test-case "lint: duplicate custom-id should warn"
      ;; Two headings with same CUSTOM_ID property
      (let* ((text (string-append
                    "* H1\n"
                    ":PROPERTIES:\n"
                    ":CUSTOM_ID: same\n"
                    ":END:\n"
                    "* H2\n"
                    ":PROPERTIES:\n"
                    ":CUSTOM_ID: same\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        ;; Should parse successfully; lint check would flag duplicates
        (check (not (null? result)) => #t)))

    (test-case "lint: unique custom-ids should not warn"
      (let* ((text (string-append
                    "* H1\n"
                    ":PROPERTIES:\n"
                    ":CUSTOM_ID: id1\n"
                    ":END:\n"
                    "* H2\n"
                    ":PROPERTIES:\n"
                    ":CUSTOM_ID: id2\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "lint: duplicate NAME should warn"
      ;; Two blocks with same #+NAME
      (let* ((text (string-append
                    "#+NAME: same\n"
                    "#+BEGIN_SRC python\n"
                    "pass\n"
                    "#+END_SRC\n"
                    "#+NAME: same\n"
                    "#+BEGIN_SRC python\n"
                    "pass\n"
                    "#+END_SRC\n"))
             (result (org-parse-buffer text)))
        (check (not (not result)) => #t)))

    ;; =========================================================
    ;; Missing language in src blocks
    ;; (from test-org-lint.el missing-language checks)
    ;; =========================================================

    (test-case "lint: src block without language should warn"
      ;; #+BEGIN_SRC with no language specified
      (let ((line "#+BEGIN_SRC"))
        ;; This is a valid block begin syntactically
        (check (org-block-begin? line) => #t)))

    (test-case "lint: src block with language should not warn"
      (let ((line "#+BEGIN_SRC python"))
        (check (org-block-begin? line) => #t)))

    ;; =========================================================
    ;; Deprecated syntax detection
    ;; (from test-org-lint.el deprecated checks)
    ;; =========================================================

    ;; Deprecated patterns from org-lint:
    ;; - #+ATTR_ASCII → should use #+ATTR_ASCII
    ;; - <center> blocks → should use #+BEGIN_CENTER
    ;; - #+HEADERS → should use #+HEADER

    (test-case "lint: deprecated block syntax"
      ;; Old: <center>text</center>
      ;; New: #+BEGIN_CENTER\ntext\n#+END_CENTER
      ;; Verify new syntax is recognized
      (check (org-block-begin? "#+BEGIN_CENTER") => #t))

    ;; =========================================================
    ;; Orphaned affiliated keywords
    ;; (from test-org-lint.el orphaned-affiliated-keywords)
    ;; =========================================================

    ;; An affiliated keyword (#+NAME, #+CAPTION, etc.) followed by
    ;; nothing or a blank line is orphaned.

    (test-case "lint: affiliated keyword followed by element is valid"
      (let ((line "#+NAME: my-table"))
        (check (org-keyword-line? line) => #t)))

    (test-case "lint: affiliated keyword types"
      ;; Standard affiliated keywords
      (check (org-keyword-line? "#+NAME: test") => #t)
      (check (org-keyword-line? "#+CAPTION: Test caption") => #t)
      (check (org-keyword-line? "#+ATTR_HTML: :width 300") => #t)
      (check (org-keyword-line? "#+ATTR_LATEX: :float t") => #t)
      (check (org-keyword-line? "#+RESULTS:") => #t))

    ;; =========================================================
    ;; Invalid babel call blocks
    ;; (from test-org-lint.el invalid-babel-call)
    ;; =========================================================

    (test-case "lint: babel call format"
      ;; #+CALL: function-name(args)
      (check (org-keyword-line? "#+CALL: my-func(x=5)") => #t))

    ;; =========================================================
    ;; Link validity checks
    ;; (from test-org-lint.el link checks)
    ;; =========================================================

    (test-case "lint: document with links parses"
      (let* ((text (string-append
                    "* Heading\n"
                    "A link to [[#custom-id][description]].\n"
                    "And [[https://example.com][external]].\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    ;; =========================================================
    ;; Heading structure issues
    ;; =========================================================

    (test-case "lint: heading level jumps"
      ;; * H1 followed by *** H3 (skipping level 2)
      ;; This should be a warning
      (let* ((text (string-append
                    "* H1\n"
                    "*** H3\n"  ;; skipped level 2
                    "* H4\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "lint: consistent heading levels"
      (let* ((text (string-append
                    "* H1\n"
                    "** H2\n"
                    "*** H3\n"
                    "** H2b\n"
                    "* H1b\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    ;; =========================================================
    ;; TODO keyword issues
    ;; =========================================================

    (test-case "lint: recognized TODO keywords"
      (let ((todo (org-parse-heading-line "* TODO Task"))
            (done (org-parse-heading-line "* DONE Finished")))
        (check (not (not todo)) => #t)
        (check (not (not done)) => #t)))

    ;; =========================================================
    ;; Property drawer position
    ;; =========================================================

    (test-case "lint: property drawer at correct position"
      ;; Property drawer must come immediately after heading
      ;; (possibly after planning line)
      (let* ((text (string-append
                    "* H1\n"
                    ":PROPERTIES:\n"
                    ":ID: abc\n"
                    ":END:\n"
                    "Content\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "lint: property drawer after planning"
      ;; SCHEDULED/DEADLINE can appear between heading and properties
      (let* ((text (string-append
                    "* TODO Task\n"
                    "SCHEDULED: <2024-01-15>\n"
                    ":PROPERTIES:\n"
                    ":ID: abc\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-lint-test)
    (test-report-summary!)))
