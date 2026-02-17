;;; -*- Gerbil -*-
;;; Org export: HTML, Markdown, LaTeX, plain text backends.
;;; Backend-agnostic (pure string processing, no editor deps except for commands).

(export #t)

(import :std/sugar
        (only-in :std/srfi/13
                 string-trim string-contains string-prefix? string-join
                 string-pad-right string-suffix?)
        :std/pregexp
        :std/misc/string
        :gemacs/org-parse)

;;;============================================================================
;;; Inline Markup Conversion
;;;============================================================================

(def (org-export-inline str backend)
  "Convert inline org markup to backend format.
Handles *bold*, /italic/, _underline_, =verbatim=, ~code~, [[link][desc]]."
  (let* ((s str)
         ;; Links first (most complex)
         (s (pregexp-replace* "\\[\\[([^]]+)\\]\\[([^]]+)\\]\\]" s
              (case backend
                ((html)     "<a href=\"\\1\">\\2</a>")
                ((markdown) "[\\2](\\1)")
                ((latex)    "\\\\href{\\1}{\\2}")
                ((text)     "\\2 (\\1)")
                (else       "\\2"))))
         ;; Bare links
         (s (pregexp-replace* "\\[\\[([^]]+)\\]\\]" s
              (case backend
                ((html)     "<a href=\"\\1\">\\1</a>")
                ((markdown) "<\\1>")
                ((latex)    "\\\\url{\\1}")
                (else       "\\1"))))
         ;; Bold *text*
         (s (pregexp-replace* "(?<=[\\s(]|^)\\*([^*]+)\\*(?=[\\s,.)!?]|$)" s
              (case backend
                ((html)     "<b>\\1</b>")
                ((markdown) "**\\1**")
                ((latex)    "\\\\textbf{\\1}")
                (else       "\\1"))))
         ;; Italic /text/
         (s (pregexp-replace* "(?<=[\\s(]|^)/([^/]+)/(?=[\\s,.)!?]|$)" s
              (case backend
                ((html)     "<i>\\1</i>")
                ((markdown) "*\\1*")
                ((latex)    "\\\\textit{\\1}")
                (else       "\\1"))))
         ;; Code ~text~
         (s (pregexp-replace* "~([^~]+)~" s
              (case backend
                ((html)     "<code>\\1</code>")
                ((markdown) "`\\1`")
                ((latex)    "\\\\texttt{\\1}")
                (else       "\\1"))))
         ;; Verbatim =text=
         (s (pregexp-replace* "=([^=]+)=" s
              (case backend
                ((html)     "<code>\\1</code>")
                ((markdown) "`\\1`")
                ((latex)    "\\\\verb|\\1|")
                (else       "\\1"))))
         ;; Underline _text_ (only HTML supports this natively)
         (s (pregexp-replace* "(?<=[\\s(]|^)_([^_]+)_(?=[\\s,.)!?]|$)" s
              (case backend
                ((html)     "<u>\\1</u>")
                ((markdown) "\\1")
                ((latex)    "\\\\underline{\\1}")
                (else       "\\1")))))
    s))

;;;============================================================================
;;; HTML Escape
;;;============================================================================

(def (html-escape str)
  "Escape HTML special characters."
  (let* ((s (pregexp-replace* "&" str "&amp;"))
         (s (pregexp-replace* "<" s "&lt;"))
         (s (pregexp-replace* ">" s "&gt;"))
         (s (pregexp-replace* "\"" s "&quot;")))
    s))

;;;============================================================================
;;; Block Splitting
;;;============================================================================

(def (org-split-into-blocks text)
  "Split org text into blocks. Returns list of (type . content).
Types: heading, paragraph, src-block, example-block, quote-block, table, keyword, comment, blank."
  (let* ((lines (string-split text #\newline))
         (total (length lines))
         (blocks '()))
    (let loop ((i 0) (current-type #f) (current-lines '()))
      (define (flush!)
        (when (and current-type (pair? current-lines))
          (set! blocks (cons (cons current-type (reverse current-lines)) blocks))))
      (if (>= i total)
        (begin (flush!) (reverse blocks))
        (let ((line (list-ref lines i)))
          (cond
            ;; Source block start
            ((org-src-block-line? line)
             (flush!)
             ;; Collect until #+END_SRC
             (let src-loop ((j (+ i 1)) (src-lines (list line)))
               (if (>= j total)
                 (begin
                   (set! blocks (cons (cons 'src-block (reverse src-lines)) blocks))
                   (loop j #f '()))
                 (let ((sl (list-ref lines j)))
                   (if (org-src-block-end? sl)
                     (begin
                       (set! blocks (cons (cons 'src-block (reverse (cons sl src-lines))) blocks))
                       (loop (+ j 1) #f '()))
                     (src-loop (+ j 1) (cons sl src-lines)))))))
            ;; Block begin (quote, example, etc.)
            ((and (org-block-begin? line) (not (org-src-block-line? line)))
             (flush!)
             (let* ((block-type-match (pregexp-match "#\\+[Bb][Ee][Gg][Ii][Nn]_(\\S+)" line))
                    (block-name (if block-type-match
                                  (string-downcase (list-ref block-type-match 1))
                                  "block")))
               (let blk-loop ((j (+ i 1)) (blk-lines (list line)))
                 (if (>= j total)
                   (begin
                     (set! blocks (cons (cons (string->symbol (string-append block-name "-block"))
                                              (reverse blk-lines)) blocks))
                     (loop j #f '()))
                   (let ((bl (list-ref lines j)))
                     (if (org-block-end? bl)
                       (begin
                         (set! blocks (cons (cons (string->symbol (string-append block-name "-block"))
                                                  (reverse (cons bl blk-lines))) blocks))
                         (loop (+ j 1) #f '()))
                       (blk-loop (+ j 1) (cons bl blk-lines))))))))
            ;; Heading
            ((org-heading-line? line)
             (flush!)
             (set! blocks (cons (cons 'heading (list line)) blocks))
             (loop (+ i 1) #f '()))
            ;; Table row
            ((org-table-line? line)
             (when (not (eq? current-type 'table)) (flush!) (set! current-lines '()))
             (loop (+ i 1) 'table (cons line current-lines)))
            ;; Keyword line
            ((org-keyword-line? line)
             (flush!)
             (set! blocks (cons (cons 'keyword (list line)) blocks))
             (loop (+ i 1) #f '()))
            ;; Comment
            ((org-comment-line? line)
             (flush!)
             (set! blocks (cons (cons 'comment (list line)) blocks))
             (loop (+ i 1) #f '()))
            ;; Blank line
            ((string=? (string-trim line) "")
             (flush!)
             (set! blocks (cons (cons 'blank '("")) blocks))
             (loop (+ i 1) #f '()))
            ;; Paragraph text
            (else
             (when (not (eq? current-type 'paragraph)) (flush!) (set! current-lines '()))
             (loop (+ i 1) 'paragraph (cons line current-lines)))))))))

;;;============================================================================
;;; Export Options
;;;============================================================================

(def (org-parse-export-options text)
  "Parse export options from buffer settings. Returns hash-table."
  (let ((settings (org-parse-buffer-settings text)))
    ;; Parse #+OPTIONS: line
    (let ((opts-str (hash-get settings "options")))
      (when opts-str
        (for-each
          (lambda (opt)
            (let ((m (pregexp-match "^(\\w+):(\\S+)$" opt)))
              (when m
                (hash-put! settings (string-append "opt-" (list-ref m 1))
                           (list-ref m 2)))))
          (pregexp-split "\\s+" opts-str))))
    settings))

;;;============================================================================
;;; HTML Backend
;;;============================================================================

(def (org-export-html text)
  "Export org text to HTML."
  (let* ((options (org-parse-export-options text))
         (title (or (hash-get options "title") "Untitled"))
         (author (or (hash-get options "author") ""))
         (blocks (org-split-into-blocks text))
         (toc? (not (string=? (or (hash-get options "opt-toc") "t") "nil")))
         (body-parts '()))
    ;; Generate TOC if enabled
    (when toc?
      (let ((headings (filter (lambda (b) (eq? (car b) 'heading)) blocks)))
        (when (pair? headings)
          (set! body-parts
            (cons (string-append "<nav id=\"table-of-contents\">\n<h2>Table of Contents</h2>\n<ul>\n"
                    (string-join
                     (map (lambda (b)
                            (let-values (((level kw pri title tags)
                                          (org-parse-heading-line (car (cdr b)))))
                              (let ((id (pregexp-replace* "\\s+" (string-downcase title) "-")))
                                (string-append "<li><a href=\"#" id "\">"
                                               (html-escape title) "</a></li>"))))
                          headings)
                     "\n")
                    "\n</ul>\n</nav>\n")
                  body-parts)))))
    ;; Convert blocks
    (for-each
      (lambda (block)
        (let ((type (car block))
              (lines (cdr block)))
          (case type
            ((heading)
             (let-values (((level kw pri title tags)
                           (org-parse-heading-line (car lines))))
               (let* ((hlevel (min level 6))
                      (id (pregexp-replace* "\\s+" (string-downcase title) "-"))
                      (html-title (org-export-inline (html-escape title) 'html))
                      (kw-html (if kw (string-append "<span class=\"todo\">" kw "</span> ") ""))
                      (tag-html (if (pair? tags)
                                  (string-append " <span class=\"tags\">"
                                    (string-join (map (lambda (t)
                                                        (string-append "<span class=\"tag\">" t "</span>"))
                                                      tags) " ")
                                    "</span>")
                                  "")))
                 (set! body-parts
                   (cons (string-append "<h" (number->string hlevel) " id=\"" id "\">"
                                        kw-html html-title tag-html
                                        "</h" (number->string hlevel) ">")
                         body-parts)))))
            ((paragraph)
             (set! body-parts
               (cons (string-append "<p>"
                       (org-export-inline (html-escape (string-join lines " ")) 'html)
                       "</p>")
                     body-parts)))
            ((src-block)
             (let* ((first (car lines))
                    (lang-match (pregexp-match "#\\+[Bb][Ee][Gg][Ii][Nn]_[Ss][Rr][Cc]\\s+(\\S+)" first))
                    (lang (if lang-match (list-ref lang-match 1) ""))
                    (body-lines (cdr (let loop ((ls (cdr lines)) (acc '()))
                                       (if (or (null? ls) (org-src-block-end? (car ls)))
                                         (cons #f (reverse acc))
                                         (loop (cdr ls) (cons (car ls) acc)))))))
               (set! body-parts
                 (cons (string-append "<pre><code class=\"language-" lang "\">"
                         (html-escape (string-join body-lines "\n"))
                         "</code></pre>")
                       body-parts))))
            ((quote-block)
             (let ((body-lines (cdr (let loop ((ls (cdr lines)) (acc '()))
                                      (if (or (null? ls) (org-block-end? (car ls)))
                                        (cons #f (reverse acc))
                                        (loop (cdr ls) (cons (car ls) acc)))))))
               (set! body-parts
                 (cons (string-append "<blockquote>\n<p>"
                         (org-export-inline (html-escape (string-join body-lines "\n")) 'html)
                         "</p>\n</blockquote>")
                       body-parts))))
            ((table)
             (let ((html-rows
                    (map (lambda (line)
                           (if (pregexp-match "^\\|[-+]+\\|?$" (string-trim line))
                             ""  ; skip separators
                             (let ((cells (org-table-parse-row-simple line)))
                               (string-append "<tr>"
                                 (string-join
                                  (map (lambda (c) (string-append "<td>" (html-escape c) "</td>"))
                                       cells)
                                  "")
                                 "</tr>"))))
                         lines)))
               (set! body-parts
                 (cons (string-append "<table>\n"
                         (string-join (filter (lambda (s) (not (string=? s ""))) html-rows) "\n")
                         "\n</table>")
                       body-parts))))
            ((blank) (void))
            ((comment) (void))
            ((keyword) (void))
            (else (void)))))
      blocks)
    ;; Assemble document
    (string-join
     (list "<!DOCTYPE html>"
           "<html>"
           "<head>"
           (string-append "<title>" (html-escape title) "</title>")
           "<meta charset=\"utf-8\">"
           "<style>"
           "body { max-width: 800px; margin: 40px auto; font-family: sans-serif; line-height: 1.6; }"
           "pre { background: #f4f4f4; padding: 1em; overflow-x: auto; }"
           "code { background: #f4f4f4; padding: 2px 4px; }"
           "blockquote { border-left: 3px solid #ccc; margin: 1em 0; padding-left: 1em; }"
           "table { border-collapse: collapse; } td, th { border: 1px solid #ccc; padding: 4px 8px; }"
           ".todo { color: red; font-weight: bold; }"
           ".tag { background: #eee; padding: 1px 4px; border-radius: 3px; font-size: 0.85em; }"
           "</style>"
           "</head>"
           "<body>"
           (string-append "<h1 class=\"title\">" (html-escape title) "</h1>")
           (if (string=? author "") ""
             (string-append "<p class=\"author\">Author: " (html-escape author) "</p>"))
           (string-join (reverse body-parts) "\n")
           "</body>"
           "</html>")
     "\n")))

;;;============================================================================
;;; Markdown Backend
;;;============================================================================

(def (org-export-markdown text)
  "Export org text to GitHub-Flavored Markdown."
  (let* ((blocks (org-split-into-blocks text))
         (parts '()))
    (for-each
      (lambda (block)
        (let ((type (car block))
              (lines (cdr block)))
          (case type
            ((heading)
             (let-values (((level kw pri title tags)
                           (org-parse-heading-line (car lines))))
               (let ((prefix (make-string level #\#))
                     (md-title (org-export-inline title 'markdown))
                     (kw-str (if kw (string-append kw " ") "")))
                 (set! parts (cons (string-append prefix " " kw-str md-title) parts)))))
            ((paragraph)
             (set! parts (cons (org-export-inline (string-join lines "\n") 'markdown) parts)))
            ((src-block)
             (let* ((first (car lines))
                    (lang-match (pregexp-match "#\\+[Bb][Ee][Gg][Ii][Nn]_[Ss][Rr][Cc]\\s+(\\S+)" first))
                    (lang (if lang-match (list-ref lang-match 1) ""))
                    (body-lines (cdr (let loop ((ls (cdr lines)) (acc '()))
                                       (if (or (null? ls) (org-src-block-end? (car ls)))
                                         (cons #f (reverse acc))
                                         (loop (cdr ls) (cons (car ls) acc)))))))
               (set! parts
                 (cons (string-append "```" lang "\n"
                         (string-join body-lines "\n")
                         "\n```")
                       parts))))
            ((quote-block)
             (let ((body-lines (cdr (let loop ((ls (cdr lines)) (acc '()))
                                      (if (or (null? ls) (org-block-end? (car ls)))
                                        (cons #f (reverse acc))
                                        (loop (cdr ls) (cons (car ls) acc)))))))
               (set! parts
                 (cons (string-join (map (lambda (l) (string-append "> " l)) body-lines) "\n")
                       parts))))
            ((table)
             ;; GFM pipe tables
             (let* ((data-lines (filter (lambda (l)
                                          (not (pregexp-match "^\\|[-+]+\\|?$" (string-trim l))))
                                        lines)))
               (when (pair? data-lines)
                 ;; First row is header
                 (let* ((header (car data-lines))
                        (cells (org-table-parse-row-simple header))
                        (sep (string-append "| "
                               (string-join (map (lambda (_) "---") cells) " | ")
                               " |")))
                   (set! parts (cons (string-append header "\n" sep
                                       (if (pair? (cdr data-lines))
                                         (string-append "\n" (string-join (cdr data-lines) "\n"))
                                         ""))
                                     parts))))))
            ((blank) (set! parts (cons "" parts)))
            ((comment) (void))
            ((keyword) (void))
            (else (void)))))
      blocks)
    (string-join (reverse parts) "\n\n")))

;;;============================================================================
;;; LaTeX Backend
;;;============================================================================

(def (latex-escape str)
  "Escape LaTeX special characters."
  (let* ((s (pregexp-replace* "\\\\" str "\\\\textbackslash{}"))
         (s (pregexp-replace* "([&%$#_{}])" s "\\\\\\1"))
         (s (pregexp-replace* "~" s "\\\\textasciitilde{}"))
         (s (pregexp-replace* "\\^" s "\\\\textasciicircum{}")))
    s))

(def (org-export-latex text)
  "Export org text to LaTeX."
  (let* ((options (org-parse-export-options text))
         (title (or (hash-get options "title") "Untitled"))
         (author (or (hash-get options "author") ""))
         (blocks (org-split-into-blocks text))
         (parts '()))
    (for-each
      (lambda (block)
        (let ((type (car block))
              (lines (cdr block)))
          (case type
            ((heading)
             (let-values (((level kw pri title-text tags)
                           (org-parse-heading-line (car lines))))
               (let ((cmd (case level
                            ((1) "\\section")
                            ((2) "\\subsection")
                            ((3) "\\subsubsection")
                            ((4) "\\paragraph")
                            (else "\\subparagraph")))
                     (lt (org-export-inline (latex-escape title-text) 'latex)))
                 (set! parts (cons (string-append cmd "{" lt "}") parts)))))
            ((paragraph)
             (set! parts (cons (org-export-inline (latex-escape (string-join lines "\n")) 'latex)
                               parts)))
            ((src-block)
             (let ((body-lines (cdr (let loop ((ls (cdr lines)) (acc '()))
                                      (if (or (null? ls) (org-src-block-end? (car ls)))
                                        (cons #f (reverse acc))
                                        (loop (cdr ls) (cons (car ls) acc)))))))
               (set! parts
                 (cons (string-append "\\begin{verbatim}\n"
                         (string-join body-lines "\n")
                         "\n\\end{verbatim}")
                       parts))))
            ((quote-block)
             (let ((body-lines (cdr (let loop ((ls (cdr lines)) (acc '()))
                                      (if (or (null? ls) (org-block-end? (car ls)))
                                        (cons #f (reverse acc))
                                        (loop (cdr ls) (cons (car ls) acc)))))))
               (set! parts
                 (cons (string-append "\\begin{quote}\n"
                         (latex-escape (string-join body-lines "\n"))
                         "\n\\end{quote}")
                       parts))))
            ((blank) (set! parts (cons "" parts)))
            (else (void)))))
      blocks)
    (string-join
     (list "\\documentclass{article}"
           "\\usepackage[utf8]{inputenc}"
           "\\usepackage{hyperref}"
           (string-append "\\title{" (latex-escape title) "}")
           (if (string=? author "")
             "\\date{}"
             (string-append "\\author{" (latex-escape author) "}"))
           "\\begin{document}"
           "\\maketitle"
           (string-join (reverse parts) "\n\n")
           "\\end{document}")
     "\n")))

;;;============================================================================
;;; Plain Text Backend
;;;============================================================================

(def (org-export-text text)
  "Export org text to plain text (strips all markup)."
  (let* ((blocks (org-split-into-blocks text))
         (parts '()))
    (for-each
      (lambda (block)
        (let ((type (car block))
              (lines (cdr block)))
          (case type
            ((heading)
             (let-values (((level kw pri title tags)
                           (org-parse-heading-line (car lines))))
               (let ((prefix (make-string (* level 2) #\space)))
                 (set! parts (cons (string-append prefix
                                     (if kw (string-append kw " ") "")
                                     (org-export-inline title 'text))
                                   parts)))))
            ((paragraph)
             (set! parts (cons (org-export-inline (string-join lines "\n") 'text) parts)))
            ((src-block)
             (let ((body-lines (cdr (let loop ((ls (cdr lines)) (acc '()))
                                      (if (or (null? ls) (org-src-block-end? (car ls)))
                                        (cons #f (reverse acc))
                                        (loop (cdr ls) (cons (car ls) acc)))))))
               (set! parts (cons (string-join body-lines "\n") parts))))
            ((table)
             (set! parts (cons (string-join lines "\n") parts)))
            ((blank) (set! parts (cons "" parts)))
            (else (void)))))
      blocks)
    (string-join (reverse parts) "\n\n")))

;;;============================================================================
;;; Export Dispatch
;;;============================================================================

(def (org-export-buffer text backend)
  "Export org text using the specified backend symbol.
Returns the exported string."
  (case backend
    ((html)     (org-export-html text))
    ((markdown) (org-export-markdown text))
    ((latex)    (org-export-latex text))
    ((text)     (org-export-text text))
    (else       (org-export-text text))))

;;;============================================================================
;;; Helper: parse row without importing org-table
;;;============================================================================

(def (org-table-parse-row-simple str)
  "Split '| a | b |' into (\"a\" \"b\"). Minimal version for export."
  (let* ((trimmed (string-trim str))
         (len (string-length trimmed)))
    (if (or (= len 0) (not (char=? (string-ref trimmed 0) #\|)))
      '()
      (let* ((inner (if (and (> len 1) (char=? (string-ref trimmed (- len 1)) #\|))
                      (substring trimmed 1 (- len 1))
                      (substring trimmed 1 len)))
             (parts (string-split inner #\|)))
        (map string-trim parts)))))
