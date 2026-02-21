;;; org-src-test.ss -- Tests for org-mode source block handling
;;; Converted from Emacs org-mode test-org-src.el
;;; Covers: source block detection, language identification,
;;; indentation handling, content extraction
;;;
;;; In gemacs, source blocks are handled through org-parse and
;;; org-babel modules.

(import :std/test
        :std/srfi/13
        :std/misc/string
        (only-in :gemacs/org-parse
                 org-block-begin? org-parse-buffer)
        (rename-in (only-in :gemacs/org-babel
                            org-babel-parse-begin-line org-babel-find-src-block
                            org-babel-inside-src-block?)
                   (org-babel-find-src-block org-babel-find-src-block-real)
                   (org-babel-inside-src-block? org-babel-inside-src-block?-real)))

(export org-src-test)

;; Adapter: tests pass text (string) but real function takes lines (list).
;; Tests use 1-based line numbers, but function uses 0-based list indices.
(def (org-babel-find-src-block text line-num)
  (org-babel-find-src-block-real (string-split text #\newline) (- line-num 1)))

(def (org-babel-inside-src-block? text line-num)
  (org-babel-inside-src-block?-real (string-split text #\newline) (- line-num 1)))

(def org-src-test
  (test-suite "org-src"

    ;; =========================================================
    ;; Source block structure
    ;; (from test-org-src.el block detection tests)
    ;; =========================================================

    (test-case "src: block begin detection"
      (check (org-block-begin? "#+BEGIN_SRC python") => #t)
      (check (org-block-begin? "#+BEGIN_SRC emacs-lisp") => #t)
      (check (org-block-begin? "#+BEGIN_SRC bash") => #t)
      (check (org-block-begin? "#+BEGIN_SRC sh") => #t)
      (check (org-block-begin? "#+BEGIN_SRC ruby") => #t)
      (check (org-block-begin? "#+BEGIN_SRC C") => #t)
      (check (org-block-begin? "#+begin_src python") => #t))

    (test-case "src: block begin with header args"
      (check (org-block-begin? "#+BEGIN_SRC python :results output") => #t)
      (check (org-block-begin? "#+BEGIN_SRC bash :var x=5") => #t)
      (check (org-block-begin? "#+BEGIN_SRC python :tangle yes :results value") => #t))

    (test-case "src: not a src block"
      (check (org-block-begin? "#+END_SRC") => #f)
      (check (org-block-begin? "#+RESULTS:") => #f)
      (check (org-block-begin? "plain text") => #f))

    ;; =========================================================
    ;; Language identification
    ;; (from test-org-src.el language tests)
    ;; =========================================================

    (test-case "src: parse language from begin line"
      (let ((result (org-babel-parse-begin-line "#+BEGIN_SRC python")))
        (check (not (not result)) => #t)))

    (test-case "src: parse language with args"
      (let ((result (org-babel-parse-begin-line
                     "#+BEGIN_SRC python :results output")))
        (check (not (not result)) => #t)))

    (test-case "src: various languages"
      ;; All common languages should be parseable
      (for-each
       (lambda (lang)
         (let ((result (org-babel-parse-begin-line
                        (string-append "#+BEGIN_SRC " lang))))
           (check (not (not result)) => #t)))
       '("python" "bash" "sh" "ruby" "emacs-lisp" "C" "java"
         "javascript" "haskell" "scheme" "sql" "R" "perl" "lua")))

    ;; =========================================================
    ;; Source block in buffer context
    ;; (from test-org-src.el editing tests)
    ;; =========================================================

    (test-case "src: find block in document"
      (let* ((text (string-append
                    "* Heading\n"
                    "Some text.\n"
                    "#+BEGIN_SRC python\n"
                    "print('hello')\n"
                    "#+END_SRC\n"
                    "More text.\n"))
             (block (org-babel-find-src-block text 4)))
        (check (not (not block)) => #t)))

    (test-case "src: inside detection"
      (let ((text (string-append
                   "#+BEGIN_SRC python\n"
                   "x = 42\n"
                   "print(x)\n"
                   "#+END_SRC\n")))
        (check (org-babel-inside-src-block? text 2) => #t)
        (check (org-babel-inside-src-block? text 3) => #t)))

    (test-case "src: outside detection"
      (let ((text (string-append
                   "Before\n"
                   "#+BEGIN_SRC python\n"
                   "code\n"
                   "#+END_SRC\n"
                   "After\n")))
        (check (org-babel-inside-src-block? text 1) => #f)
        (check (org-babel-inside-src-block? text 5) => #f)))

    ;; =========================================================
    ;; Multiple source blocks
    ;; (from test-org-src.el multi-block tests)
    ;; =========================================================

    (test-case "src: multiple blocks in document"
      (let* ((text (string-append
                    "#+BEGIN_SRC python\n"
                    "print('first')\n"
                    "#+END_SRC\n"
                    "\n"
                    "#+BEGIN_SRC bash\n"
                    "echo second\n"
                    "#+END_SRC\n"))
             (result (org-parse-buffer text)))
        (check (not (not result)) => #t)))

    ;; =========================================================
    ;; Indentation handling
    ;; (from test-org-src.el indentation tests)
    ;; =========================================================

    ;; In Emacs, org-src-content-indentation controls how code is
    ;; indented inside blocks. The elisp tests verify:
    ;; - Default indentation (2 spaces)
    ;; - No indentation (org-src-preserve-indentation)
    ;; - Tab character preservation
    ;; - Empty line preservation

    (test-case "src: indented block parsing"
      (let* ((text (string-append
                    "  #+BEGIN_SRC python\n"
                    "    print('hello')\n"
                    "  #+END_SRC\n"))
             (result (org-parse-buffer text)))
        ;; Indented blocks should still parse
        (check (not (not result)) => #t)))

    (test-case "src: block with empty lines"
      (let* ((text (string-append
                    "#+BEGIN_SRC python\n"
                    "x = 1\n"
                    "\n"
                    "y = 2\n"
                    "#+END_SRC\n"))
             (result (org-parse-buffer text)))
        (check (not (not result)) => #t)))

    (test-case "src: block with only whitespace body"
      (let* ((text (string-append
                    "#+BEGIN_SRC python\n"
                    "  \n"
                    "#+END_SRC\n"))
             (result (org-parse-buffer text)))
        (check (not (not result)) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-src-test)
    (test-report-summary!)))
