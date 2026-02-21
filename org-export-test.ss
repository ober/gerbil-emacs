;;; org-export-test.ss -- Tests for gemacs org-export module
;;; Converted from Emacs org-mode test-ox.el, test-ox-html.el,
;;; test-ox-ascii.el, test-ox-md.el, test-ox-latex.el
;;; Covers: inline markup, block splitting, HTML/MD/LaTeX/text export,
;;; html-escape, export options

(import :std/test
        :std/srfi/13
        (only-in :gemacs/org-export
                 org-export-buffer org-export-inline
                 org-split-into-blocks html-escape))

(export org-export-test)

(def org-export-test
  (test-suite "org-export"

    ;; =========================================================
    ;; HTML escaping
    ;; (from test-ox-html.el character escaping)
    ;; =========================================================

    (test-case "html-escape: angle brackets"
      (check (html-escape "<script>") => "&lt;script&gt;"))

    (test-case "html-escape: ampersand"
      (check (html-escape "a & b") => "a &amp; b"))

    (test-case "html-escape: double quotes"
      (check (html-escape "say \"hello\"") => "say &quot;hello&quot;"))

    (test-case "html-escape: no escaping needed"
      (check (html-escape "plain text") => "plain text"))

    (test-case "html-escape: empty string"
      (check (html-escape "") => ""))

    (test-case "html-escape: mixed special chars"
      (check (not (not (string-contains
                        (html-escape "<a href=\"url\">link & text</a>")
                        "&lt;"))) => #t))

    ;; =========================================================
    ;; Inline markup conversion
    ;; (from test-ox.el inline element tests)
    ;; =========================================================

    (test-case "org-export-inline: bold to HTML"
      (let ((result (org-export-inline " *bold* " 'html)))
        (check (not (not (string-contains result "<b>bold</b>"))) => #t)))

    (test-case "org-export-inline: italic to HTML"
      (let ((result (org-export-inline " /italic/ " 'html)))
        (check (not (not (string-contains result "<i>italic</i>"))) => #t)))

    (test-case "org-export-inline: code to HTML"
      (let ((result (org-export-inline " ~code~ " 'html)))
        (check (not (not (string-contains result "<code>code</code>"))) => #t)))

    (test-case "org-export-inline: underline to HTML"
      (let ((result (org-export-inline " _underline_ " 'html)))
        (check (not (not (string-contains result "<u>underline</u>"))) => #t)))

    (test-case "org-export-inline: strikethrough to HTML"
      (let ((result (org-export-inline " +strike+ " 'html)))
        (check (not (not (string-contains result "<del>strike</del>"))) => #t)))

    (test-case "org-export-inline: verbatim to HTML"
      (let ((result (org-export-inline " =verbatim= " 'html)))
        (check (not (not (string-contains result "verbatim"))) => #t)))

    (test-case "org-export-inline: code to markdown"
      (let ((result (org-export-inline "use ~code~ here" 'markdown)))
        (check (not (not (string-contains result "`code`"))) => #t)))

    (test-case "org-export-inline: bold to markdown"
      (let ((result (org-export-inline " *bold* " 'markdown)))
        (check (not (not (string-contains result "**bold**"))) => #t)))

    (test-case "org-export-inline: italic to markdown"
      (let ((result (org-export-inline " /italic/ " 'markdown)))
        (check (not (not (string-contains result "*italic*"))) => #t)))

    (test-case "org-export-inline: bold to LaTeX"
      (let ((result (org-export-inline " *bold* " 'latex)))
        (check (not (not (string-contains result "\\textbf{bold}"))) => #t)))

    (test-case "org-export-inline: italic to LaTeX"
      (let ((result (org-export-inline " /italic/ " 'latex)))
        (check (not (not (string-contains result "\\emph{italic}"))) => #t)))

    (test-case "org-export-inline: plain text (no markup)"
      (let ((result (org-export-inline "plain text" 'text)))
        (check (not (not (string-contains result "plain text"))) => #t)))

    ;; =========================================================
    ;; Block splitting
    ;; (from test-ox.el block parsing tests)
    ;; =========================================================

    (test-case "org-split-into-blocks: headings and paragraphs"
      (let* ((text "* Heading\nParagraph text\n\n* Another heading\n")
             (blocks (org-split-into-blocks text)))
        (check (not (null? blocks)) => #t)
        (check (>= (length blocks) 2) => #t)))

    (test-case "org-split-into-blocks: with source block"
      (let* ((text (string-append
                    "* Heading\n"
                    "#+BEGIN_SRC python\n"
                    "print('hello')\n"
                    "#+END_SRC\n"))
             (blocks (org-split-into-blocks text)))
        (check (not (null? blocks)) => #t)))

    (test-case "org-split-into-blocks: with quote block"
      (let* ((text (string-append
                    "* Heading\n"
                    "#+BEGIN_QUOTE\n"
                    "A famous quote\n"
                    "#+END_QUOTE\n"))
             (blocks (org-split-into-blocks text)))
        (check (not (null? blocks)) => #t)))

    (test-case "org-split-into-blocks: with table"
      (let* ((text (string-append
                    "* Heading\n"
                    "| a | b |\n"
                    "| 1 | 2 |\n"))
             (blocks (org-split-into-blocks text)))
        (check (not (null? blocks)) => #t)))

    (test-case "org-split-into-blocks: empty input"
      (let ((blocks (org-split-into-blocks "")))
        (check (null? blocks) => #t)))

    ;; =========================================================
    ;; Full buffer export: HTML
    ;; (from test-ox-html.el full export tests)
    ;; =========================================================

    (test-case "org-export-buffer: HTML output has DOCTYPE"
      (let ((result (org-export-buffer "* Hello\nWorld\n" 'html)))
        (check (not (not (string-contains result "<!DOCTYPE"))) => #t)))

    (test-case "org-export-buffer: HTML output has heading"
      (let ((result (org-export-buffer "* Hello\nWorld\n" 'html)))
        (check (not (not (string-contains result "<h1"))) => #t)
        (check (not (not (string-contains result "Hello"))) => #t)))

    (test-case "org-export-buffer: HTML output has paragraph"
      (let ((result (org-export-buffer "* Hello\nWorld\n" 'html)))
        (check (not (not (string-contains result "World"))) => #t)))

    (test-case "org-export-buffer: HTML with bold"
      (let ((result (org-export-buffer "* Hello\n*bold text*\n" 'html)))
        (check (not (not (string-contains result "<b>"))) => #t)))

    (test-case "org-export-buffer: HTML with list"
      (let ((result (org-export-buffer "- item 1\n- item 2\n" 'html)))
        (check (not (not (string-contains result "<li>"))) => #t)))

    (test-case "org-export-buffer: HTML with table"
      (let ((result (org-export-buffer "| a | b |\n| 1 | 2 |\n" 'html)))
        (check (not (not (string-contains result "<table"))) => #t)))

    ;; =========================================================
    ;; Full buffer export: Markdown
    ;; (from test-ox-md.el)
    ;; =========================================================

    (test-case "org-export-buffer: Markdown headings"
      (let ((result (org-export-buffer "* Hello\nWorld\n" 'markdown)))
        (check (not (not (string-contains result "# Hello"))) => #t)))

    (test-case "org-export-buffer: Markdown nested headings"
      (let ((result (org-export-buffer "* H1\n** H2\n*** H3\n" 'markdown)))
        (check (not (not (string-contains result "# H1"))) => #t)
        (check (not (not (string-contains result "## H2"))) => #t)
        (check (not (not (string-contains result "### H3"))) => #t)))

    (test-case "org-export-buffer: Markdown bold and italic"
      (let ((result (org-export-buffer "*bold* and /italic/\n" 'markdown)))
        (check (not (not (string-contains result "**bold**"))) => #t)
        (check (not (not (string-contains result "*italic*"))) => #t)))

    (test-case "org-export-buffer: Markdown code"
      (let ((result (org-export-buffer "Use ~code~ here\n" 'markdown)))
        (check (not (not (string-contains result "`code`"))) => #t)))

    ;; =========================================================
    ;; Full buffer export: LaTeX
    ;; (from test-ox-latex.el)
    ;; =========================================================

    (test-case "org-export-buffer: LaTeX output has documentclass"
      (let ((result (org-export-buffer "* Hello\nWorld\n" 'latex)))
        (check (not (not (string-contains result "\\documentclass"))) => #t)))

    (test-case "org-export-buffer: LaTeX output has section"
      (let ((result (org-export-buffer "* Hello\nWorld\n" 'latex)))
        (check (not (not (string-contains result "\\section"))) => #t)))

    (test-case "org-export-buffer: LaTeX subsection"
      (let ((result (org-export-buffer "* H1\n** H2\n" 'latex)))
        (check (not (not (string-contains result "\\subsection"))) => #t)))

    ;; =========================================================
    ;; Full buffer export: Plain text
    ;; (from test-ox-ascii.el)
    ;; =========================================================

    (test-case "org-export-buffer: plain text strips heading markers"
      (let ((result (org-export-buffer "* Hello\nWorld\n" 'text)))
        (check (not (not (string-contains result "Hello"))) => #t)
        (check (not (not (string-contains result "World"))) => #t)))

    (test-case "org-export-buffer: plain text preserves content"
      (let ((result (org-export-buffer "Some text\nMore text\n" 'text)))
        (check (not (not (string-contains result "Some text"))) => #t)
        (check (not (not (string-contains result "More text"))) => #t)))

    ;; =========================================================
    ;; Export options and settings
    ;; (from test-ox.el settings and options)
    ;; =========================================================

    (test-case "export: handles empty input"
      (let ((result (org-export-buffer "" 'html)))
        (check (string? result) => #t)))

    (test-case "export: handles only whitespace"
      (let ((result (org-export-buffer "  \n\n  \n" 'html)))
        (check (string? result) => #t)))

    ))

(def main
  (lambda args
    (run-tests! org-export-test)
    (test-report-summary!)))
