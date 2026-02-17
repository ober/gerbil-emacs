;;; -*- Gerbil -*-
;;; Org syntax highlighting via manual SCI_STARTSTYLING/SCI_SETSTYLING.
;;; Backend-agnostic (Scintilla API only, no Qt imports).

(export #t)

(import :std/sugar
        (only-in :std/srfi/13
                 string-trim string-contains string-prefix? string-index)
        :std/pregexp
        :std/misc/string
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gerbil-scintilla/style
        :gemacs/org-parse)

;;;============================================================================
;;; Scintilla Style Message Constants (not in gerbil-scintilla bindings)
;;;============================================================================

(def SCI_STARTSTYLING   2032)
(def SCI_SETSTYLING     2033)
(def SCI_SETILEXER      4033)  ;; Set ILexer to NULL for manual styling

;;;============================================================================
;;; Org Style IDs (32-58, avoids collision with lexer 0-31 and terminal 64-79)
;;;============================================================================

(def ORG_STYLE_DEFAULT       32)
(def ORG_STYLE_HEADING_1     33)
(def ORG_STYLE_HEADING_2     34)
(def ORG_STYLE_HEADING_3     35)
(def ORG_STYLE_HEADING_4     36)
(def ORG_STYLE_HEADING_5     37)
(def ORG_STYLE_HEADING_6     38)
(def ORG_STYLE_HEADING_7     39)
(def ORG_STYLE_HEADING_8     40)
(def ORG_STYLE_TODO          41)
(def ORG_STYLE_DONE          42)
(def ORG_STYLE_TAGS          43)
(def ORG_STYLE_COMMENT       44)
(def ORG_STYLE_KEYWORD       45)
(def ORG_STYLE_BOLD          46)
(def ORG_STYLE_ITALIC        47)
(def ORG_STYLE_UNDERLINE     48)
(def ORG_STYLE_VERBATIM      49)
(def ORG_STYLE_CODE          50)
(def ORG_STYLE_LINK          51)
(def ORG_STYLE_DATE          52)
(def ORG_STYLE_PROPERTY      53)
(def ORG_STYLE_BLOCK_DELIM   54)
(def ORG_STYLE_BLOCK_BODY    55)
(def ORG_STYLE_CHECKBOX_ON   56)
(def ORG_STYLE_CHECKBOX_OFF  57)
(def ORG_STYLE_TABLE         58)

;;;============================================================================
;;; Color Definitions (Scintilla BGR format)
;;;============================================================================

;; Helper: convert RGB to Scintilla BGR
(def (org-rgb r g b)
  (+ b (* 256 g) (* 65536 r)))

;; Heading colors (decreasing warmth)
(def ORG_COLOR_HEADING_1 (org-rgb 80 120 220))   ; blue
(def ORG_COLOR_HEADING_2 (org-rgb 210 140 50))    ; orange
(def ORG_COLOR_HEADING_3 (org-rgb 60 160 60))     ; green
(def ORG_COLOR_HEADING_4 (org-rgb 50 180 180))    ; cyan
(def ORG_COLOR_HEADING_5 (org-rgb 150 80 180))    ; purple
(def ORG_COLOR_HEADING_6 (org-rgb 180 160 40))    ; yellow
(def ORG_COLOR_HEADING_7 (org-rgb 120 120 120))   ; gray
(def ORG_COLOR_HEADING_8 (org-rgb 100 100 100))   ; dark gray

(def ORG_COLOR_TODO      (org-rgb 220 50 50))     ; red
(def ORG_COLOR_DONE      (org-rgb 50 180 50))     ; green
(def ORG_COLOR_TAGS      (org-rgb 150 80 180))    ; purple
(def ORG_COLOR_COMMENT   (org-rgb 130 130 130))   ; gray
(def ORG_COLOR_KEYWORD   (org-rgb 210 140 50))    ; orange
(def ORG_COLOR_VERBATIM  (org-rgb 50 180 180))    ; cyan
(def ORG_COLOR_CODE      (org-rgb 60 160 60))     ; green
(def ORG_COLOR_LINK      (org-rgb 80 120 220))    ; blue
(def ORG_COLOR_DATE      (org-rgb 180 80 180))    ; magenta
(def ORG_COLOR_PROPERTY  (org-rgb 100 100 100))   ; dim
(def ORG_COLOR_BLOCK     (org-rgb 210 140 50))    ; orange
(def ORG_COLOR_TABLE     (org-rgb 50 180 180))    ; cyan

;;;============================================================================
;;; Style Setup
;;;============================================================================

(def (setup-org-styles! ed)
  "Configure all org-mode style colors and attributes."
  ;; Disable the lexer for manual styling
  (send-message ed SCI_SETILEXER 0)

  ;; Heading styles
  (let ((heading-colors (vector ORG_COLOR_HEADING_1 ORG_COLOR_HEADING_2
                                ORG_COLOR_HEADING_3 ORG_COLOR_HEADING_4
                                ORG_COLOR_HEADING_5 ORG_COLOR_HEADING_6
                                ORG_COLOR_HEADING_7 ORG_COLOR_HEADING_8)))
    (let loop ((i 0))
      (when (< i 8)
        (let ((style (+ ORG_STYLE_HEADING_1 i)))
          (editor-style-set-foreground ed style (vector-ref heading-colors i))
          (editor-style-set-bold ed style #t))
        (loop (+ i 1)))))

  ;; TODO/DONE
  (editor-style-set-foreground ed ORG_STYLE_TODO ORG_COLOR_TODO)
  (editor-style-set-bold ed ORG_STYLE_TODO #t)
  (editor-style-set-foreground ed ORG_STYLE_DONE ORG_COLOR_DONE)
  (editor-style-set-bold ed ORG_STYLE_DONE #t)

  ;; Tags
  (editor-style-set-foreground ed ORG_STYLE_TAGS ORG_COLOR_TAGS)

  ;; Comment
  (editor-style-set-foreground ed ORG_STYLE_COMMENT ORG_COLOR_COMMENT)
  (editor-style-set-italic ed ORG_STYLE_COMMENT #t)

  ;; Keyword (#+TITLE:)
  (editor-style-set-foreground ed ORG_STYLE_KEYWORD ORG_COLOR_KEYWORD)

  ;; Inline markup
  (editor-style-set-bold ed ORG_STYLE_BOLD #t)
  (editor-style-set-italic ed ORG_STYLE_ITALIC #t)
  (editor-style-set-underline ed ORG_STYLE_UNDERLINE #t)
  (editor-style-set-foreground ed ORG_STYLE_VERBATIM ORG_COLOR_VERBATIM)
  (editor-style-set-foreground ed ORG_STYLE_CODE ORG_COLOR_CODE)

  ;; Link
  (editor-style-set-foreground ed ORG_STYLE_LINK ORG_COLOR_LINK)
  (editor-style-set-underline ed ORG_STYLE_LINK #t)

  ;; Date
  (editor-style-set-foreground ed ORG_STYLE_DATE ORG_COLOR_DATE)

  ;; Property drawer
  (editor-style-set-foreground ed ORG_STYLE_PROPERTY ORG_COLOR_PROPERTY)

  ;; Block delimiters and body
  (editor-style-set-foreground ed ORG_STYLE_BLOCK_DELIM ORG_COLOR_BLOCK)
  (editor-style-set-foreground ed ORG_STYLE_BLOCK_BODY ORG_COLOR_PROPERTY)

  ;; Checkboxes
  (editor-style-set-foreground ed ORG_STYLE_CHECKBOX_ON ORG_COLOR_DONE)
  (editor-style-set-foreground ed ORG_STYLE_CHECKBOX_OFF ORG_COLOR_TODO)

  ;; Table
  (editor-style-set-foreground ed ORG_STYLE_TABLE ORG_COLOR_TABLE))

;;;============================================================================
;;; Line-by-Line Highlighting (State Machine)
;;;============================================================================

(def (org-highlight-buffer! ed text)
  "Full-buffer org-mode highlighting with state machine."
  (let* ((lines (string-split text #\newline))
         (total (length lines))
         (pos 0)           ; byte position in buffer
         (state 'normal))  ; 'normal | 'src-block | 'drawer
    (let loop ((i 0) (pos 0) (state 'normal))
      (when (< i total)
        (let* ((line (list-ref lines i))
               (line-len (string-length line))
               (next-pos (+ pos line-len 1))) ; +1 for newline
          (cond
            ;; Block begin
            ((and (eq? state 'normal) (org-block-begin? line))
             (org-style-line! ed pos line-len ORG_STYLE_BLOCK_DELIM)
             (loop (+ i 1) next-pos 'src-block))

            ;; Block end
            ((and (eq? state 'src-block) (org-block-end? line))
             (org-style-line! ed pos line-len ORG_STYLE_BLOCK_DELIM)
             (loop (+ i 1) next-pos 'normal))

            ;; Inside block
            ((eq? state 'src-block)
             (org-style-line! ed pos line-len ORG_STYLE_BLOCK_BODY)
             (loop (+ i 1) next-pos 'src-block))

            ;; Drawer begin
            ((and (eq? state 'normal)
                  (or (pregexp-match "^\\s*:PROPERTIES:" line)
                      (pregexp-match "^\\s*:LOGBOOK:" line)))
             (org-style-line! ed pos line-len ORG_STYLE_PROPERTY)
             (loop (+ i 1) next-pos 'drawer))

            ;; Drawer end
            ((and (eq? state 'drawer)
                  (pregexp-match "^\\s*:END:" line))
             (org-style-line! ed pos line-len ORG_STYLE_PROPERTY)
             (loop (+ i 1) next-pos 'normal))

            ;; Inside drawer
            ((eq? state 'drawer)
             (org-style-line! ed pos line-len ORG_STYLE_PROPERTY)
             (loop (+ i 1) next-pos 'drawer))

            ;; Normal state classifications
            (else
              (org-highlight-normal-line! ed pos line line-len)
              (loop (+ i 1) next-pos 'normal))))))))

(def (org-highlight-normal-line! ed pos line line-len)
  "Highlight a line in normal state."
  (let ((trimmed (string-trim line)))
    (cond
      ;; Heading
      ((org-heading-line? line)
       (org-highlight-heading-line! ed pos line line-len))

      ;; Comment
      ((org-comment-line? line)
       (org-style-line! ed pos line-len ORG_STYLE_COMMENT))

      ;; Keyword (#+KEY:)
      ((org-keyword-line? line)
       (org-style-line! ed pos line-len ORG_STYLE_KEYWORD))

      ;; Table
      ((org-table-line? line)
       (org-style-line! ed pos line-len ORG_STYLE_TABLE))

      ;; Regular line — scan for inline markup
      (else
       ;; Start with default
       (org-style-line! ed pos line-len ORG_STYLE_DEFAULT)
       ;; Apply inline markup
       (org-highlight-inline! ed pos line line-len)))))

;;;============================================================================
;;; Heading Highlighting (sub-ranges for stars, TODO/DONE, priority, tags)
;;;============================================================================

(def (org-highlight-heading-line! ed pos line line-len)
  "Highlight a heading line with sub-ranges."
  (let* ((level (org-heading-stars-of-line line))
         (style (heading-style-for-level level)))
    ;; Style the whole line with heading style
    (org-style-line! ed pos line-len style)

    ;; Highlight TODO/DONE keyword if present
    (let ((m (pregexp-match "^(\\*+)\\s+(TODO|NEXT|DOING|WAITING|HOLD)\\s" line)))
      (when m
        (let* ((stars-len (string-length (list-ref m 1)))
               (kw (list-ref m 2))
               (kw-start (+ stars-len 1))
               (kw-len (string-length kw)))
          (org-style-range! ed (+ pos kw-start) kw-len ORG_STYLE_TODO))))

    (let ((m (pregexp-match "^(\\*+)\\s+(DONE|CANCELLED)\\s" line)))
      (when m
        (let* ((stars-len (string-length (list-ref m 1)))
               (kw (list-ref m 2))
               (kw-start (+ stars-len 1))
               (kw-len (string-length kw)))
          (org-style-range! ed (+ pos kw-start) kw-len ORG_STYLE_DONE))))

    ;; Highlight tags at end of line
    (let ((m (pregexp-match "(:\\S+:)\\s*$" line)))
      (when m
        (let* ((tag-str (list-ref m 1))
               (tag-start (- line-len (string-length tag-str)
                             (- (string-length line)
                                (string-length (string-trim line))))))
          ;; Find where tags actually start
          (let ((tag-pos (string-contains line tag-str)))
            (when tag-pos
              (org-style-range! ed (+ pos tag-pos) (string-length tag-str)
                                ORG_STYLE_TAGS))))))))

(def (heading-style-for-level level)
  "Get style ID for heading level."
  (cond
    ((<= level 0) ORG_STYLE_DEFAULT)
    ((<= level 8) (+ ORG_STYLE_HEADING_1 (- level 1)))
    (else ORG_STYLE_HEADING_8)))

;;;============================================================================
;;; Inline Markup Highlighting
;;;============================================================================

(def (org-highlight-inline! ed pos line line-len)
  "Apply inline markup highlighting to a line."
  ;; Bold *text*
  (org-highlight-markup-pairs! ed pos line "*" ORG_STYLE_BOLD)
  ;; Italic /text/
  (org-highlight-markup-pairs! ed pos line "/" ORG_STYLE_ITALIC)
  ;; Underline _text_
  (org-highlight-markup-pairs! ed pos line "_" ORG_STYLE_UNDERLINE)
  ;; Verbatim =text=
  (org-highlight-markup-pairs! ed pos line "=" ORG_STYLE_VERBATIM)
  ;; Code ~text~
  (org-highlight-markup-pairs! ed pos line "~" ORG_STYLE_CODE)
  ;; Links [[url][desc]]
  (org-highlight-links! ed pos line)
  ;; Dates <2024-01-15> and [2024-01-15]
  (org-highlight-dates! ed pos line)
  ;; Checkboxes
  (org-highlight-checkboxes! ed pos line))

(def (org-highlight-markup-pairs! ed pos line marker style)
  "Find and highlight paired inline markup markers."
  (let* ((marker-char (string-ref marker 0))
         (len (string-length line)))
    (let loop ((i 0))
      (when (< i (- len 2))
        (if (and (char=? (string-ref line i) marker-char)
                 ;; Must be preceded by space/BOL
                 (or (= i 0) (char-whitespace? (string-ref line (- i 1))))
                 ;; Must be followed by non-space
                 (not (char-whitespace? (string-ref line (+ i 1)))))
          ;; Find closing marker
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j len) (loop (+ i 1)))
              ((and (char=? (string-ref line j) marker-char)
                    ;; Preceded by non-space
                    (not (char-whitespace? (string-ref line (- j 1))))
                    ;; Followed by space/EOL/punctuation
                    (or (= j (- len 1))
                        (let ((c (string-ref line (+ j 1))))
                          (or (char-whitespace? c)
                              (memv c '(#\. #\, #\; #\: #\! #\? #\) #\]))))))
               ;; Found pair: style from i to j inclusive
               (org-style-range! ed (+ pos i) (+ (- j i) 1) style)
               (loop (+ j 1)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))))

(def (org-highlight-links! ed pos line)
  "Highlight [[link]] and [[link][desc]] in line."
  (let ((len (string-length line)))
    (let loop ((i 0))
      (when (< i (- len 3))
        (if (and (char=? (string-ref line i) #\[)
                 (< (+ i 1) len)
                 (char=? (string-ref line (+ i 1)) #\[))
          ;; Find ]]
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j (- len 1)) (loop (+ i 1)))
              ((and (char=? (string-ref line j) #\])
                    (char=? (string-ref line (+ j 1)) #\]))
               (org-style-range! ed (+ pos i) (+ (- j i) 2) ORG_STYLE_LINK)
               (loop (+ j 2)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))))

(def (org-highlight-dates! ed pos line)
  "Highlight <date> and [date] timestamps in line."
  (let ((len (string-length line)))
    ;; Active timestamps <...>
    (let loop ((i 0))
      (when (< i (- len 5))
        (if (and (char=? (string-ref line i) #\<)
                 (< (+ i 5) len)
                 (char-numeric? (string-ref line (+ i 1))))
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j len) (loop (+ i 1)))
              ((char=? (string-ref line j) #\>)
               (org-style-range! ed (+ pos i) (+ (- j i) 1) ORG_STYLE_DATE)
               (loop (+ j 1)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))
    ;; Inactive timestamps [...]
    (let loop ((i 0))
      (when (< i (- len 5))
        (if (and (char=? (string-ref line i) #\[)
                 (< (+ i 5) len)
                 (char-numeric? (string-ref line (+ i 1))))
          (let close-loop ((j (+ i 2)))
            (cond
              ((>= j len) (loop (+ i 1)))
              ((char=? (string-ref line j) #\])
               (org-style-range! ed (+ pos i) (+ (- j i) 1) ORG_STYLE_DATE)
               (loop (+ j 1)))
              (else (close-loop (+ j 1)))))
          (loop (+ i 1)))))))

(def (org-highlight-checkboxes! ed pos line)
  "Highlight [X] and [ ] checkboxes."
  (let ((len (string-length line)))
    (let loop ((i 0))
      (when (< i (- len 2))
        (if (char=? (string-ref line i) #\[)
          (cond
            ((and (< (+ i 2) len)
                  (char=? (string-ref line (+ i 2)) #\]))
             (let ((inner (string-ref line (+ i 1))))
               (cond
                 ((or (char=? inner #\X) (char=? inner #\x))
                  (org-style-range! ed (+ pos i) 3 ORG_STYLE_CHECKBOX_ON))
                 ((char=? inner #\space)
                  (org-style-range! ed (+ pos i) 3 ORG_STYLE_CHECKBOX_OFF)))
               (loop (+ i 3))))
            (else (loop (+ i 1))))
          (loop (+ i 1)))))))

;;;============================================================================
;;; Low-Level Styling Primitives
;;;============================================================================

(def (org-style-line! ed pos len style)
  "Style an entire line starting at pos."
  (when (> len 0)
    (send-message ed SCI_STARTSTYLING pos)
    (send-message ed SCI_SETSTYLING len style)))

(def (org-style-range! ed pos len style)
  "Style a range within a line."
  (when (> len 0)
    (send-message ed SCI_STARTSTYLING pos)
    (send-message ed SCI_SETSTYLING len style)))

;;;============================================================================
;;; Fold Levels
;;;============================================================================

(def SCI_SETFOLDLEVEL 2222)
(def SC_FOLDLEVELHEADERFLAG #x2000)
(def SC_FOLDLEVELBASE      #x400)

(def (org-set-fold-levels! ed text)
  "Set SCI_SETFOLDLEVEL per line for native fold margin."
  (let* ((lines (string-split text #\newline))
         (total (length lines)))
    (let loop ((i 0) (cur-level 0))
      (when (< i total)
        (let ((line (list-ref lines i)))
          (if (org-heading-line? line)
            (let ((level (org-heading-stars-of-line line)))
              ;; Heading line gets header flag
              (send-message ed SCI_SETFOLDLEVEL i
                            (bitwise-ior SC_FOLDLEVELBASE
                                         (- level 1)
                                         SC_FOLDLEVELHEADERFLAG))
              (loop (+ i 1) level))
            (begin
              ;; Body line gets current level
              (send-message ed SCI_SETFOLDLEVEL i
                            (+ SC_FOLDLEVELBASE cur-level))
              (loop (+ i 1) cur-level))))))))

;;;============================================================================
;;; Incremental Highlighting
;;;============================================================================

(def (org-highlight-range! ed text start-line end-line)
  "Re-highlight a range of lines. Requires full text for state tracking."
  ;; For simplicity, we re-highlight from the start to build correct state.
  ;; A future optimization could cache state per line.
  (org-highlight-buffer! ed text))
