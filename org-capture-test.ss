;;; org-capture-test.ss -- Tests for gemacs org-capture module
;;; Converted from Emacs org-mode test-org-capture.el
;;; Covers: template expansion, cursor position, capture menu,
;;; refile targets, capture start/abort lifecycle

(import :std/test
        :std/srfi/13
        (except-in (rename-in :gemacs/org-capture
                              (org-capture-start org-capture-start-real))
                   string-suffix?))

(export org-capture-test)

;; Adapter: tests call (org-capture-start) with 0 args,
;; but actual function takes (template-key source-file source-path) — 3 args.
(def (org-capture-start)
  (org-capture-start-real "" "" ""))

(def org-capture-test
  (test-suite "org-capture"

    ;; =========================================================
    ;; Template expansion
    ;; (from test-org-capture.el fill-template tests)
    ;; =========================================================

    (test-case "org-capture: template expansion %U"
      ;; %U should expand to inactive timestamp [YYYY-MM-DD ...]
      ;; %? marks cursor position and should be removed
      (let ((result (org-capture-expand-template "* TODO %?\n  %U")))
        ;; Should contain [ (inactive timestamp start)
        (check (not (not (string-contains result "["))) => #t)
        ;; %? should be removed
        (check (not (string-contains result "%?")) => #t)))

    (test-case "org-capture: template expansion %t"
      ;; %t should expand to active timestamp <YYYY-MM-DD ...>
      (let ((result (org-capture-expand-template "Created: %t")))
        (check (not (not (string-contains result "<"))) => #t)
        (check (not (not (string-contains result ">"))) => #t)))

    (test-case "org-capture: template expansion %T"
      ;; %T should expand to active timestamp with time
      (let ((result (org-capture-expand-template "Time: %T")))
        (check (not (not (string-contains result "<"))) => #t)))

    (test-case "org-capture: template expansion %f"
      ;; %f should expand to filename
      (let ((result (org-capture-expand-template
                     "From: %f" "myfile.org")))
        (check (not (not (string-contains result "myfile.org"))) => #t)))

    (test-case "org-capture: template expansion %%"
      ;; %% should expand to literal %
      (let ((result (org-capture-expand-template "100%% done")))
        (check (not (not (string-contains result "100%"))) => #t)
        ;; Should not have %% in output
        (check (not (string-contains result "%%")) => #t)))

    (test-case "org-capture: template expansion preserves text"
      (let ((result (org-capture-expand-template "* TODO Task\n  Notes here")))
        (check (not (not (string-contains result "TODO Task"))) => #t)
        (check (not (not (string-contains result "Notes here"))) => #t)))

    (test-case "org-capture: template with no placeholders"
      (let ((result (org-capture-expand-template "Plain text without placeholders")))
        (check result => "Plain text without placeholders")))

    ;; =========================================================
    ;; Cursor position
    ;; (from test-org-capture.el cursor-position tests)
    ;; =========================================================

    (test-case "org-capture: cursor position"
      ;; %? marks where cursor should land
      (let ((pos (org-capture-cursor-position "* TODO %?\n  %U")))
        ;; Position should be at the %? location (after "* TODO ")
        (check (= pos 7) => #t)))

    (test-case "org-capture: cursor position at start"
      (let ((pos (org-capture-cursor-position "%?Rest of template")))
        (check (= pos 0) => #t)))

    (test-case "org-capture: cursor position no marker"
      ;; When no %? is present, position is typically at end or 0
      (let ((pos (org-capture-cursor-position "No cursor marker")))
        (check (number? pos) => #t)))

    ;; =========================================================
    ;; Capture menu
    ;; (from test-org-capture.el menu-string tests)
    ;; =========================================================

    (test-case "org-capture: menu string contains keys and descriptions"
      (let ((saved *org-capture-templates*))
        (set! *org-capture-templates*
              '(("t" "TODO" entry "* TODO %?\n  %U")
                ("n" "Note" entry "* %?\n  %U")))
        (let ((menu (org-capture-menu-string)))
          (check (not (not (string-contains menu "[t]"))) => #t)
          (check (not (not (string-contains menu "TODO"))) => #t)
          (check (not (not (string-contains menu "[n]"))) => #t)
          (check (not (not (string-contains menu "Note"))) => #t))
        (set! *org-capture-templates* saved)))

    ;; =========================================================
    ;; Refile targets
    ;; (from test-org-capture.el refile tests)
    ;; =========================================================

    (test-case "org-capture: refile targets extraction"
      (let* ((text (string-append
                    "* Projects\n"
                    "** Project A\n"
                    "** Project B\n"
                    "* Tasks\n"
                    "** Daily\n"))
             (targets (org-refile-targets text)))
        ;; Should extract headings as refile targets
        (check (>= (length targets) 3) => #t)))

    (test-case "org-capture: refile targets empty"
      (let ((targets (org-refile-targets "No headings here\n")))
        (check (null? targets) => #t)))

    ;; =========================================================
    ;; Insert under heading
    ;; (from test-org-capture.el insert-under-heading tests)
    ;; =========================================================

    (test-case "org-capture: insert under heading"
      (let* ((text (string-append
                    "* Tasks\n"
                    "** Existing task\n"
                    "* Notes\n"))
             (result (org-insert-under-heading
                      text "Tasks" "** New task\n")))
        (check (not (not (string-contains result "New task"))) => #t)
        (check (not (not (string-contains result "Existing task"))) => #t)))

    ;; =========================================================
    ;; Capture lifecycle
    ;; (from test-org-capture.el start/abort tests)
    ;; =========================================================

    (test-case "org-capture: start sets active flag"
      (let ((saved *org-capture-active?*))
        (set! *org-capture-active?* #f)
        (org-capture-start)
        (check *org-capture-active?* => #t)
        (org-capture-abort)
        (check *org-capture-active?* => #f)
        (set! *org-capture-active?* saved)))

    (test-case "org-capture: abort restores state"
      (let ((saved *org-capture-active?*))
        (set! *org-capture-active?* #f)
        (org-capture-start)
        (check *org-capture-active?* => #t)
        (org-capture-abort)
        (check *org-capture-active?* => #f)
        (set! *org-capture-active?* saved)))

    ))

(def main
  (lambda args
    (run-tests! org-capture-test)
    (test-report-summary!)))
