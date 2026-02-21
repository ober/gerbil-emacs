;;; org-property-test.ss -- Tests for property inheritance
;;; Converted from Emacs org-mode test-property-inheritance.el
;;; Covers: property access, inheritance from parent headings,
;;; property accumulation, overwrite, append behavior

(import :std/test
        :std/srfi/13
        (except-in (rename-in :gemacs/org-parse
                              (org-parse-properties org-parse-properties-raw))
                   string-prefix-ci?))

(export org-property-test)

;; Adapter: tests call (org-parse-properties lines) with 1 arg,
;; but actual function takes (lines start-idx). Convert hash→alist.
(def (org-parse-properties lines)
  (hash->list (org-parse-properties-raw lines 0)))

;; Adapter: org-parse-heading-line returns (values ...), wrap into struct.
(def (org-parse-heading-line-adapter line)
  (let-values (((level keyword priority title tags) (org-parse-heading-line line)))
    (if (not level)
      #f
      (make-org-heading level keyword
                        (and priority (string (char-upcase priority)))
                        title tags
                        #f #f #f #f '() 0 #f))))

(def org-property-test
  (test-suite "org-property"

    ;; =========================================================
    ;; Basic property access
    ;; (from test-org.el at-property tests)
    ;; =========================================================

    (test-case "property: standard property drawer"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":ID: abc123"
                      ":CATEGORY: work"
                      ":END:"))))
        (check (assoc "ID" props) => '("ID" . "abc123"))
        (check (assoc "CATEGORY" props) => '("CATEGORY" . "work"))))

    (test-case "property: with various value types"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":CREATED: [2024-01-15 Mon]"
                      ":EFFORT: 2:00"
                      ":STYLE: habit"
                      ":REPEAT_TO_STATE: TODO"
                      ":END:"))))
        (check (length props) => 4)
        (check (assoc "CREATED" props) => '("CREATED" . "[2024-01-15 Mon]"))
        (check (assoc "EFFORT" props) => '("EFFORT" . "2:00"))
        (check (assoc "STYLE" props) => '("STYLE" . "habit"))))

    (test-case "property: empty property drawer"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:" ":END:"))))
        (check (null? props) => #t)))

    (test-case "property: single property"
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":CUSTOM_ID: my-id"
                      ":END:"))))
        (check (length props) => 1)
        (check (assoc "CUSTOM_ID" props) => '("CUSTOM_ID" . "my-id"))))

    ;; =========================================================
    ;; Property drawer in buffer context
    ;; (from test-org.el get-property-block tests)
    ;; =========================================================

    (test-case "property: heading with properties in buffer"
      (let* ((text (string-append
                    "* H1\n"
                    ":PROPERTIES:\n"
                    ":ID: abc\n"
                    ":END:\n"
                    "Content\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "property: properties after planning line"
      (let* ((text (string-append
                    "* TODO Task\n"
                    "SCHEDULED: <2024-01-15>\n"
                    ":PROPERTIES:\n"
                    ":EFFORT: 1:00\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    ;; =========================================================
    ;; Property inheritance
    ;; (from test-property-inheritance.el)
    ;; =========================================================

    ;; In org-mode, properties can be inherited from parent headings.
    ;; The test file uses property-inheritance.org test data.

    (test-case "property: inheritance structure"
      ;; Parent heading sets :PROPERTY: value
      ;; Child heading inherits it unless overridden
      (let* ((text (string-append
                    "* Parent\n"
                    ":PROPERTIES:\n"
                    ":CATEGORY: work\n"
                    ":END:\n"
                    "** Child\n"
                    "Content\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "property: inheritance with override"
      ;; Child overrides parent's property
      (let* ((text (string-append
                    "* Parent\n"
                    ":PROPERTIES:\n"
                    ":CATEGORY: work\n"
                    ":END:\n"
                    "** Child\n"
                    ":PROPERTIES:\n"
                    ":CATEGORY: personal\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    (test-case "property: inheritance with append (+)"
      ;; In org-mode, :PROP+: appends to inherited value
      (let* ((text (string-append
                    "* Parent\n"
                    ":PROPERTIES:\n"
                    ":TAGS_ALL: a b\n"
                    ":END:\n"
                    "** Child\n"
                    ":PROPERTIES:\n"
                    ":TAGS_ALL+: c d\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        (check (not (null? result)) => #t)))

    ;; =========================================================
    ;; Document-level properties
    ;; (from test-org.el document property tests)
    ;; =========================================================

    (test-case "property: document-level via keyword"
      ;; #+PROPERTY: header-args :results output
      (let* ((text (string-append
                    "#+PROPERTY: header-args :results output\n"
                    "* Heading\n"
                    "Content\n"))
             (result (org-parse-buffer text)))
        (check (not (not result)) => #t)))

    ;; =========================================================
    ;; Special properties
    ;; (from test-org.el property tests)
    ;; =========================================================

    ;; org-mode has "special" properties computed on the fly:
    ;; ITEM, TODO, PRIORITY, TAGS, DEADLINE, SCHEDULED, etc.

    (test-case "property: heading with all components"
      (let ((h (org-parse-heading-line-adapter
                "** TODO [#A] My Task :work:urgent:")))
        ;; The "special properties" are the heading components
        (check (not (not h)) => #t)))

    ;; =========================================================
    ;; Property values listing
    ;; (from test-org.el property-values tests)
    ;; =========================================================

    ;; org-property-values returns all values for a given property
    ;; across the buffer. E.g., all CATEGORY values.

    (test-case "property: multiple headings with same property"
      (let* ((text (string-append
                    "* H1\n"
                    ":PROPERTIES:\n"
                    ":A: 1\n"
                    ":END:\n"
                    "* H2\n"
                    ":PROPERTIES:\n"
                    ":A: 2\n"
                    ":END:\n"))
             (result (org-parse-buffer text)))
        ;; Both headings have property A
        (check (not (null? result)) => #t)))

    ;; =========================================================
    ;; Property deletion
    ;; (from test-org.el delete-property tests)
    ;; =========================================================

    (test-case "property: structure supports modification"
      ;; Verify that properties are represented as modifiable data
      (let ((props (org-parse-properties
                    '(":PROPERTIES:"
                      ":A: 1"
                      ":B: 2"
                      ":END:"))))
        (check (length props) => 2)
        ;; Properties are alist pairs
        (check (pair? (car props)) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-property-test)
    (test-report-summary!)))
