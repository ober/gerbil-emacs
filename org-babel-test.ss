;;; org-babel-test.ss -- Tests for gemacs org-babel module
;;; Converted from Emacs org-mode test-ob.el, test-ob-tangle.el, test-ob-exp.el
;;; Covers: header arg parsing, begin line parsing, src block detection,
;;; result formatting, named blocks, tangling, variable injection,
;;; noweb expansion, C-c C-c context

(import :std/test
        :std/srfi/13
        :std/misc/string
        (except-in (rename-in :gemacs/org-babel
                              (org-babel-inject-variables org-babel-inject-variables-real)
                              (org-babel-find-src-block org-babel-find-src-block-real)
                              (org-babel-inside-src-block? org-babel-inside-src-block?-real)
                              (org-babel-format-result org-babel-format-result-real)
                              (org-ctrl-c-ctrl-c-context org-ctrl-c-ctrl-c-context-real))
                   filter-map))

(export org-babel-test)

;; Adapter: tests call (org-babel-inject-variables body lang vars) with 3 args,
;; but actual function takes (lang vars) — 2 args. Adapter passes lang+vars.
;; Tests pass lang as symbol, function expects string.
(def (org-babel-inject-variables body lang vars)
  (org-babel-inject-variables-real (symbol->string lang) vars))

;; Adapter: tests pass text (string) but real function takes lines (list).
;; Tests use 1-based line numbers, but function uses 0-based list indices.
(def (org-babel-find-src-block text line-num)
  (org-babel-find-src-block-real (string-split text #\newline) (- line-num 1)))

;; Adapter: tests pass text (string) but real function takes lines (list).
(def (org-babel-inside-src-block? text line-num)
  (org-babel-inside-src-block?-real (string-split text #\newline) (- line-num 1)))

;; Adapter: tests pass symbol but real function takes string.
(def (org-babel-format-result output type)
  (org-babel-format-result-real output (symbol->string type)))

;; Adapter: tests pass text (string) but real function takes lines (list).
;; Tests use 1-based line numbers.
(def (org-ctrl-c-ctrl-c-context text line-num)
  (org-ctrl-c-ctrl-c-context-real (string-split text #\newline) (- line-num 1)))

(def org-babel-test
  (test-suite "org-babel"

    ;; =========================================================
    ;; Header argument parsing
    ;; (from test-ob.el header-arg tests)
    ;; =========================================================

    (test-case "org-babel: parse header args basic"
      (let ((args (org-babel-parse-header-args
                   ":var x=5 :results output :dir /tmp")))
        (check (hash-table? args) => #t)
        (check (hash-get args "var") => "x=5")
        (check (hash-get args "results") => "output")
        (check (hash-get args "dir") => "/tmp")))

    (test-case "org-babel: parse header args empty"
      (let ((args (org-babel-parse-header-args "")))
        (check (hash-table? args) => #t)))

    (test-case "org-babel: parse header args multiple vars"
      (let ((args (org-babel-parse-header-args
                   ":var x=5 :var y=hello")))
        (check (hash-table? args) => #t)))

    (test-case "org-babel: parse header args with exports"
      (let ((args (org-babel-parse-header-args
                   ":exports both :results output :tangle yes")))
        (check (hash-get args "exports") => "both")
        (check (hash-get args "tangle") => "yes")))

    (test-case "org-babel: parse header args with session"
      (let ((args (org-babel-parse-header-args
                   ":session *my-session* :results value")))
        (check (hash-get args "session") => "*my-session*")))

    ;; =========================================================
    ;; Begin line parsing
    ;; (from test-ob.el begin-line tests)
    ;; =========================================================

    (test-case "org-babel: parse begin line with language and args"
      (let ((result (org-babel-parse-begin-line
                     "#+BEGIN_SRC python :var x=5")))
        (check (not (not result)) => #t)))

    (test-case "org-babel: parse begin line language only"
      (let ((result (org-babel-parse-begin-line "#+BEGIN_SRC bash")))
        (check (not (not result)) => #t)))

    (test-case "org-babel: parse begin line case insensitive"
      (let ((result (org-babel-parse-begin-line "#+begin_src python")))
        (check (not (not result)) => #t)))

    (test-case "org-babel: parse begin line with multiple args"
      (let ((result (org-babel-parse-begin-line
                     "#+BEGIN_SRC python :var x=5 :results output :dir /tmp")))
        (check (not (not result)) => #t)))

    (test-case "org-babel: parse begin line not a begin line"
      (let ((result (org-babel-parse-begin-line "not a begin line")))
        (check result => #f)))

    ;; =========================================================
    ;; Source block detection
    ;; (from test-ob.el src-block-regexp and find tests)
    ;; =========================================================

    (test-case "org-babel: find src block in text"
      (let* ((text (string-append
                    "Some text\n"
                    "#+BEGIN_SRC python\n"
                    "print('hello')\n"
                    "#+END_SRC\n"
                    "More text\n"))
             (block (org-babel-find-src-block text 2)))
        (check (not (not block)) => #t)))

    (test-case "org-babel: inside src block detection"
      (let ((text (string-append
                   "#+BEGIN_SRC python\n"
                   "print('hello')\n"
                   "#+END_SRC\n")))
        ;; Line 2 (print) should be inside the block
        (check (org-babel-inside-src-block? text 2) => #t)
        ;; Line 1 (BEGIN_SRC) is the begin line
        ;; Line 3 (END_SRC) is the end line
        ))

    (test-case "org-babel: not inside src block"
      (let ((text (string-append
                   "Some text\n"
                   "#+BEGIN_SRC python\n"
                   "print('hello')\n"
                   "#+END_SRC\n"
                   "More text\n")))
        ;; Line 1 and 5 should not be inside any block
        (check (org-babel-inside-src-block? text 1) => #f)
        (check (org-babel-inside-src-block? text 5) => #f)))

    ;; =========================================================
    ;; Result formatting
    ;; (from test-ob.el result formatting tests)
    ;; =========================================================

    (test-case "org-babel: format result output"
      (let ((result (org-babel-format-result "hello\nworld" 'output)))
        (check (not (not (string-contains result ": hello"))) => #t)
        (check (not (not (string-contains result ": world"))) => #t)))

    (test-case "org-babel: format result value"
      (let ((result (org-babel-format-result "42" 'value)))
        (check (not (not (string-contains result "42"))) => #t)))

    (test-case "org-babel: format result empty"
      (let ((result (org-babel-format-result "" 'output)))
        (check (string? result) => #t)))

    (test-case "org-babel: format result multiline output"
      (let ((result (org-babel-format-result "line1\nline2\nline3" 'output)))
        (check (not (not (string-contains result "line1"))) => #t)
        (check (not (not (string-contains result "line3"))) => #t)))

    ;; =========================================================
    ;; Named blocks
    ;; (from test-ob.el named block tests)
    ;; =========================================================

    (test-case "org-babel: find named block"
      (let* ((text (string-append
                    "#+NAME: greet\n"
                    "#+BEGIN_SRC python\n"
                    "return 'hello'\n"
                    "#+END_SRC\n"))
             (block (org-babel-find-named-block text "greet")))
        (check (not (not block)) => #t)))

    (test-case "org-babel: named block not found"
      (let* ((text (string-append
                    "#+NAME: greet\n"
                    "#+BEGIN_SRC python\n"
                    "return 'hello'\n"
                    "#+END_SRC\n"))
             (block (org-babel-find-named-block text "nonexistent")))
        (check block => #f)))

    ;; =========================================================
    ;; Tangling
    ;; (from test-ob-tangle.el)
    ;; =========================================================

    (test-case "org-babel: tangle extraction"
      (let* ((text (string-append
                    "#+BEGIN_SRC bash :tangle /tmp/test.sh\n"
                    "echo hello\n"
                    "#+END_SRC\n"
                    "#+BEGIN_SRC python :tangle /tmp/test.py\n"
                    "print('world')\n"
                    "#+END_SRC\n"))
             (result (org-babel-tangle text)))
        (check (not (null? result)) => #t)))

    (test-case "org-babel: tangle respects :tangle header"
      (let* ((text (string-append
                    "#+BEGIN_SRC bash :tangle /tmp/out.sh\n"
                    "#!/bin/bash\n"
                    "echo hello\n"
                    "#+END_SRC\n"))
             (result (org-babel-tangle text)))
        (check (not (null? result)) => #t)))

    (test-case "org-babel: tangle skips :tangle no"
      (let* ((text (string-append
                    "#+BEGIN_SRC bash :tangle no\n"
                    "echo skipped\n"
                    "#+END_SRC\n"))
             (result (org-babel-tangle text)))
        ;; Blocks with :tangle no should be skipped
        (check (null? result) => #t)))

    ;; =========================================================
    ;; Variable injection
    ;; (from test-ob.el variable injection tests)
    ;; =========================================================

    (test-case "org-babel: variable injection bash"
      (let ((result (org-babel-inject-variables
                     "echo $x" 'bash '(("x" . "5") ("y" . "hello")))))
        (check (not (not (string-contains result "x="))) => #t)))

    (test-case "org-babel: variable injection python"
      (let ((result (org-babel-inject-variables
                     "print(x)" 'python '(("x" . "5")))))
        (check (not (not (string-contains result "x"))) => #t)))

    ;; =========================================================
    ;; Noweb expansion
    ;; (from test-ob.el noweb tests)
    ;; =========================================================

    (test-case "org-babel: noweb expansion"
      (let* ((text (string-append
                    "#+NAME: helper\n"
                    "#+BEGIN_SRC python\n"
                    "def helper():\n"
                    "    return 42\n"
                    "#+END_SRC\n"
                    "\n"
                    "#+BEGIN_SRC python :noweb yes\n"
                    "<<helper>>\n"
                    "print(helper())\n"
                    "#+END_SRC\n"))
             (result (org-babel-expand-noweb text "<<helper>>\nprint(helper())\n")))
        (check (not (not result)) => #t)))

    ;; =========================================================
    ;; C-c C-c context detection
    ;; (from test-ob.el context tests)
    ;; =========================================================

    (test-case "org-babel: C-c C-c context: heading"
      (let ((ctx (org-ctrl-c-ctrl-c-context "* TODO My Task" 1)))
        (check ctx => 'heading)))

    (test-case "org-babel: C-c C-c context: src-block"
      (let* ((text (string-append
                    "#+BEGIN_SRC python\n"
                    "print('hello')\n"
                    "#+END_SRC\n"))
             (ctx (org-ctrl-c-ctrl-c-context text 2)))
        (check ctx => 'src-block)))

    (test-case "org-babel: C-c C-c context: table"
      (let ((ctx (org-ctrl-c-ctrl-c-context "| a | b | c |" 1)))
        (check ctx => 'table)))

    (test-case "org-babel: C-c C-c context: plain text"
      (let ((ctx (org-ctrl-c-ctrl-c-context "Just some text" 1)))
        ;; Plain text has no special context
        (check (or (eq? ctx #f) (eq? ctx 'paragraph) (eq? ctx 'none)) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-babel-test)
    (test-report-summary!)))
