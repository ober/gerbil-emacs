;;; -*- Gerbil -*-
;;; Org table: parsing, alignment, cell navigation, formulas, CSV.
;;; Backend-agnostic (Scintilla API only, no Qt imports).

(export #t)

(import :std/sugar
        (only-in :std/srfi/13
                 string-trim string-trim-both string-pad string-pad-right string-join
                 string-prefix? string-contains string<?)
        :std/sort
        ./pregexp-compat
        :std/misc/string
        :gerbil-scintilla/scintilla
        :gerbil-scintilla/constants
        :gemacs/core
        :gemacs/org-parse)

;;;============================================================================
;;; Internal helpers (thin wrappers over Scintilla API)
;;;============================================================================

(def (editor-current-line ed)
  "Get current line number from cursor position."
  (editor-line-from-position ed (editor-get-current-pos ed)))

(def (editor-replace-target-text ed text)
  "Replace the target range with text. SCI_SETTARGETSTART/END must be set first."
  (send-message/string ed SCI_REPLACETARGET text))

;;;============================================================================
;;; Table Detection
;;;============================================================================

(def (org-table-row? str)
  "Check if string is an org table row (starts with |)."
  (let ((trimmed (string-trim-both str)))
    (and (> (string-length trimmed) 0)
         (char=? (string-ref trimmed 0) #\|))))

(def (org-table-separator? str)
  "Check if string is a table separator line (|---+---|)."
  (let ((trimmed (string-trim-both str)))
    (and (org-table-row? trimmed)
         (pregexp-match "^\\|[-+]+\\|?$" trimmed)
         #t)))

(def (org-table-on-table-line? ed)
  "Check if cursor is on a table line."
  (let* ((line-num (editor-current-line ed))
         (line (editor-get-line ed line-num)))
    (org-table-row? line)))

;;;============================================================================
;;; Table Parsing
;;;============================================================================

(def (org-table-parse-row str)
  "Split '| a | b | c |' into (\"a\" \"b\" \"c\"). Returns list of trimmed cell strings."
  (let* ((trimmed (string-trim-both str))
         (len (string-length trimmed)))
    (if (or (= len 0) (not (char=? (string-ref trimmed 0) #\|)))
      '()
      ;; Strip leading | and trailing |
      (let* ((inner (if (and (> len 1) (char=? (string-ref trimmed (- len 1)) #\|))
                      (substring trimmed 1 (- len 1))
                      (substring trimmed 1 len)))
             (parts (string-split inner #\|)))
        (map string-trim-both parts)))))

(def (org-table-find-bounds ed)
  "Find the start and end lines of the table containing cursor.
Returns (values start-line end-line) or (values #f #f) if not in a table."
  (let ((cur-line (editor-current-line ed))
        (total (editor-get-line-count ed)))
    (if (not (org-table-row? (editor-get-line ed cur-line)))
      (values #f #f)
      (let ((start (let loop ((i cur-line))
                     (if (and (>= i 0)
                              (org-table-row? (editor-get-line ed i)))
                       (loop (- i 1))
                       (+ i 1))))
            (end (let loop ((i cur-line))
                   (if (and (< i total)
                            (org-table-row? (editor-get-line ed i)))
                     (loop (+ i 1))
                     (- i 1)))))
        (values start end)))))

(def (org-table-get-rows ed start end)
  "Get table rows as list of lists of cell strings. Separators included as 'separator symbol."
  (let loop ((i start) (rows '()))
    (if (> i end)
      (reverse rows)
      (let ((line (editor-get-line ed i)))
        (loop (+ i 1)
              (cons (if (org-table-separator? line)
                      'separator
                      (org-table-parse-row line))
                    rows))))))

(def (org-table-column-widths rows)
  "Compute max column width for each column across all data rows.
Separators are ignored. Returns list of integers."
  (let* ((data-rows (filter list? rows))
         (ncols (if (null? data-rows) 0
                  (apply max (map length data-rows)))))
    (let loop ((col 0) (widths '()))
      (if (>= col ncols)
        (reverse widths)
        (loop (+ col 1)
              (cons (apply max 1
                           (map (lambda (row)
                                  (if (< col (length row))
                                    (string-length (list-ref row col))
                                    0))
                                data-rows))
                    widths))))))

(def (org-numeric-cell? str)
  "Check if a cell value looks numeric (for right-alignment)."
  (and (not (string=? str ""))
       (pregexp-match "^-?[0-9]*\\.?[0-9]+%?$" (string-trim str))
       #t))

;;;============================================================================
;;; Table Current Column
;;;============================================================================

(def (org-table-current-column ed)
  "Determine which column (0-based) the cursor is in within a table row."
  (let* ((pos (editor-get-current-pos ed))
         (line-num (editor-current-line ed))
         (line-start (editor-position-from-line ed line-num))
         (col-offset (- pos line-start))
         (line (editor-get-line ed line-num)))
    (if (not (org-table-row? line))
      0
      ;; Count | characters before cursor position
      (let loop ((i 0) (pipes -1))  ; start at -1 since first | is column boundary
        (if (>= i (min col-offset (string-length line)))
          (max 0 pipes)
          (loop (+ i 1)
                (if (char=? (string-ref line i) #\|)
                  (+ pipes 1)
                  pipes)))))))

;;;============================================================================
;;; Table Alignment (Core Operation)
;;;============================================================================

(def (org-table-format-row cells widths)
  "Format a data row with cells padded to given widths.
Numeric cells are right-aligned, text cells are left-aligned."
  (string-append
   "| "
   (string-join
    (let loop ((i 0) (result '()))
      (if (>= i (length widths))
        (reverse result)
        (let* ((cell (if (< i (length cells)) (list-ref cells i) ""))
               (w (list-ref widths i))
               (padded (if (org-numeric-cell? cell)
                         (string-pad cell w)
                         (string-pad-right cell w))))
          (loop (+ i 1) (cons padded result)))))
    " | ")
   " |"))

(def (org-table-format-separator widths)
  "Format a separator line: |---+---+---|"
  (string-append
   "|"
   (string-join
    (map (lambda (w) (make-string (+ w 2) #\-)) widths)
    "+")
   "|"))

(def (org-table-align ed)
  "Realign the entire table around cursor. Pads cells to uniform widths."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((rows (org-table-get-rows ed start end))
             (widths (org-table-column-widths rows))
             ;; Remember cursor column
             (cur-col (org-table-current-column ed))
             ;; Build new table text
             (new-lines
              (map (lambda (row)
                     (if (eq? row 'separator)
                       (org-table-format-separator widths)
                       (org-table-format-row row widths)))
                   rows))
             (new-text (string-join new-lines "\n"))
             ;; Replace table region
             (start-pos (editor-position-from-line ed start))
             (end-pos (if (< (+ end 1) (editor-get-line-count ed))
                        (editor-position-from-line ed (+ end 1))
                        (editor-get-text-length ed))))
        ;; Replace old table with new — add trailing newline when region
        ;; extends to next line start so we don't eat the line after the table
        (send-message ed SCI_SETTARGETSTART start-pos)
        (send-message ed SCI_SETTARGETEND end-pos)
        (let ((replacement (if (< (+ end 1) (editor-get-line-count ed))
                             (string-append new-text "\n")
                             new-text)))
          (editor-replace-target-text ed replacement))
        ;; Reposition cursor in the same column
        (let* ((cur-line (editor-current-line ed))
               (clamped-line (max start (min cur-line end))))
          (org-table-goto-column ed clamped-line cur-col widths))))))

(def (org-table-goto-column ed line-num col widths)
  "Position cursor at the start of the given column in the given line."
  (let* ((line-start (editor-position-from-line ed line-num))
         ;; Calculate offset: each column is "| " + width + " "
         ;; First column starts at offset 2 (after "| ")
         (offset (let loop ((i 0) (off 2))
                   (if (>= i col)
                     off
                     (loop (+ i 1) (+ off (list-ref widths (min i (- (length widths) 1))) 3))))))
    (editor-goto-pos ed (+ line-start (min offset
                                            (- (editor-line-length ed line-num) 1))))))

;;;============================================================================
;;; Cell Navigation
;;;============================================================================

(def (org-table-next-cell ed)
  "Move to the next cell in the table. Creates a new row at table end."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-line (editor-current-line ed))
             (cur-col (org-table-current-column ed))
             (line (editor-get-line ed cur-line))
             (ncols (length (org-table-parse-row line))))
        (cond
          ;; Not last column: move to next column
          ((< (+ cur-col 1) ncols)
           (org-table-align ed)
           (let ((widths (org-table-column-widths (org-table-get-rows ed start end))))
             (org-table-goto-column ed cur-line (+ cur-col 1) widths)))
          ;; Last column, not last row: move to first column of next data row
          ((< cur-line end)
           (let ((next-line (org-table-next-data-line ed (+ cur-line 1) end)))
             (org-table-align ed)
             (let ((widths (org-table-column-widths (org-table-get-rows ed start end))))
               (org-table-goto-column ed (or next-line (+ end 1)) 0 widths))))
          ;; Last column, last row: create new row
          (else
           (let* ((ncols-actual (length (org-table-column-widths
                                         (org-table-get-rows ed start end))))
                  (empty-cells (make-list ncols-actual ""))
                  (new-row (string-append "| "
                             (string-join empty-cells " | ")
                             " |")))
             ;; Insert new row after current line
             (let ((eol (editor-get-line-end-position ed cur-line)))
               (editor-insert-text ed eol (string-append "\n" new-row))
               (org-table-align ed)
               ;; Move to first column of new row
               (let* ((new-end (+ end 1))
                      (widths (org-table-column-widths
                               (org-table-get-rows ed start new-end))))
                 (org-table-goto-column ed (+ cur-line 1) 0 widths))))))))))

(def (org-table-prev-cell ed)
  "Move to the previous cell in the table."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-line (editor-current-line ed))
             (cur-col (org-table-current-column ed)))
        (org-table-align ed)
        (let ((widths (org-table-column-widths (org-table-get-rows ed start end))))
          (cond
            ;; Not first column: move to previous column
            ((> cur-col 0)
             (org-table-goto-column ed cur-line (- cur-col 1) widths))
            ;; First column, not first row: move to last column of previous data row
            ((> cur-line start)
             (let ((prev (org-table-prev-data-line ed (- cur-line 1) start)))
               (when prev
                 (let* ((row (org-table-parse-row (editor-get-line ed prev)))
                        (last-col (max 0 (- (length row) 1))))
                   (org-table-goto-column ed prev last-col widths)))))))))))

(def (org-table-next-row-same-column ed)
  "Move to the next row in the same column. Creates new row at end."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-line (editor-current-line ed))
             (cur-col (org-table-current-column ed)))
        (cond
          ((< cur-line end)
           (let ((next (org-table-next-data-line ed (+ cur-line 1) end)))
             (org-table-align ed)
             (let ((widths (org-table-column-widths (org-table-get-rows ed start end))))
               (org-table-goto-column ed (or next cur-line) cur-col widths))))
          (else
           ;; At last row: create new row
           (let* ((ncols (length (org-table-column-widths
                                  (org-table-get-rows ed start end))))
                  (empty-cells (make-list ncols ""))
                  (new-row (string-append "| "
                             (string-join empty-cells " | ")
                             " |"))
                  (eol (editor-get-line-end-position ed cur-line)))
             (editor-insert-text ed eol (string-append "\n" new-row))
             (org-table-align ed)
             (let* ((new-end (+ end 1))
                    (widths (org-table-column-widths
                             (org-table-get-rows ed start new-end))))
               (org-table-goto-column ed (+ cur-line 1) cur-col widths)))))))))

(def (org-table-next-data-line ed from-line end)
  "Find next non-separator line starting at from-line. Returns line number or #f."
  (let loop ((i from-line))
    (cond
      ((> i end) #f)
      ((org-table-separator? (editor-get-line ed i))
       (loop (+ i 1)))
      (else i))))

(def (org-table-prev-data-line ed from-line start)
  "Find previous non-separator line. Returns line number or #f."
  (let loop ((i from-line))
    (cond
      ((< i start) #f)
      ((org-table-separator? (editor-get-line ed i))
       (loop (- i 1)))
      (else i))))

;;;============================================================================
;;; Separator Completion
;;;============================================================================

(def (org-table-complete-separator ed)
  "Complete a partial separator line (e.g., '|-' → full separator)."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((widths (org-table-column-widths (org-table-get-rows ed start end)))
             (sep (org-table-format-separator widths))
             (cur-line (editor-current-line ed))
             (line-start (editor-position-from-line ed cur-line))
             (line-end (editor-get-line-end-position ed cur-line)))
        (send-message ed SCI_SETTARGETSTART line-start)
        (send-message ed SCI_SETTARGETEND line-end)
        (editor-replace-target-text ed sep)))))

;;;============================================================================
;;; Column/Row Operations
;;;============================================================================

(def (org-table-move-column ed direction)
  "Move current column left (-1) or right (+1)."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-col (org-table-current-column ed))
             (rows (org-table-get-rows ed start end))
             (widths (org-table-column-widths rows))
             (ncols (length widths))
             (target-col (+ cur-col direction)))
        (when (and (>= target-col 0) (< target-col ncols))
          ;; Swap columns in all rows
          (let ((new-rows (map (lambda (row)
                                 (if (eq? row 'separator)
                                   row
                                   (swap-list-elements row cur-col target-col)))
                               rows)))
            (org-table-replace-rows ed start end new-rows)
            ;; Reposition to moved column
            (let ((new-widths (org-table-column-widths new-rows)))
              (org-table-goto-column ed (editor-current-line ed) target-col new-widths))))))))

(def (org-table-insert-column ed)
  "Insert a new empty column after current column."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-col (org-table-current-column ed))
             (rows (org-table-get-rows ed start end))
             (new-rows (map (lambda (row)
                              (if (eq? row 'separator) row
                                (list-insert row (+ cur-col 1) "")))
                            rows)))
        (org-table-replace-rows ed start end new-rows)))))

(def (org-table-delete-column ed)
  "Delete current column."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-col (org-table-current-column ed))
             (rows (org-table-get-rows ed start end))
             (ncols (length (org-table-column-widths rows))))
        (when (> ncols 1) ; don't delete last column
          (let ((new-rows (map (lambda (row)
                                 (if (eq? row 'separator) row
                                   (list-remove-at row cur-col)))
                               rows)))
            (org-table-replace-rows ed start end new-rows)))))))

(def (org-table-move-row ed direction)
  "Move current row up (-1) or down (+1)."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-line (editor-current-line ed))
             (row-idx (- cur-line start))
             (rows (org-table-get-rows ed start end))
             (target-idx (+ row-idx direction)))
        (when (and (>= target-idx 0) (< target-idx (length rows)))
          (let ((new-rows (swap-list-elements rows row-idx target-idx)))
            (org-table-replace-rows ed start end new-rows)
            ;; Move cursor to the new position
            (editor-goto-pos ed (editor-position-from-line ed (+ start target-idx)))))))))

(def (org-table-insert-row ed)
  "Insert a new empty row above current row."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-line (editor-current-line ed))
             (row-idx (- cur-line start))
             (rows (org-table-get-rows ed start end))
             (ncols (length (org-table-column-widths rows)))
             (empty-row (make-list ncols ""))
             (new-rows (list-insert rows row-idx empty-row)))
        (org-table-replace-rows ed start end new-rows)))))

(def (org-table-delete-row ed)
  "Delete current row."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-line (editor-current-line ed))
             (row-idx (- cur-line start))
             (rows (org-table-get-rows ed start end)))
        (when (> (length rows) 1)
          (let ((new-rows (list-remove-at rows row-idx)))
            (org-table-replace-rows ed start end new-rows)))))))

(def (org-table-insert-separator-line ed)
  "Insert a separator line (|---+---|) below current row."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((cur-line (editor-current-line ed))
             (row-idx (- cur-line start))
             (rows (org-table-get-rows ed start end))
             (new-rows (list-insert rows (+ row-idx 1) 'separator)))
        (org-table-replace-rows ed start end new-rows)))))

;;;============================================================================
;;; Table Sort
;;;============================================================================

(def (org-table-sort ed col-num (numeric? #f))
  "Sort table by column col-num. Separators stay in place, data rows are sorted."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((rows (org-table-get-rows ed start end))
             ;; Separate data rows from separators (with original indices)
             (indexed (let loop ((i 0) (rs rows) (acc '()))
                        (if (null? rs) (reverse acc)
                          (loop (+ i 1) (cdr rs) (cons (cons i (car rs)) acc)))))
             (data-indexed (filter (lambda (p) (list? (cdr p))) indexed))
             (sep-indexed  (filter (lambda (p) (eq? (cdr p) 'separator)) indexed))
             ;; Sort data rows
             (sorted-data (sort (map cdr data-indexed)
                                (lambda (a b)
                                  (let ((va (if (< col-num (length a)) (list-ref a col-num) ""))
                                        (vb (if (< col-num (length b)) (list-ref b col-num) "")))
                                    (if numeric?
                                      (< (or (string->number va) 0)
                                         (or (string->number vb) 0))
                                      (string<? va vb))))))
             ;; Reconstruct: put separators back in their positions
             (result (let loop ((i 0) (seps sep-indexed) (data sorted-data) (acc '()))
                       (cond
                         ((and (null? seps) (null? data)) (reverse acc))
                         ((and (pair? seps) (= (caar seps) i))
                          (loop (+ i 1) (cdr seps) data (cons 'separator acc)))
                         ((pair? data)
                          (loop (+ i 1) seps (cdr data) (cons (car data) acc)))
                         (else (reverse acc))))))
        (org-table-replace-rows ed start end result)))))

;;;============================================================================
;;; Table Replace Helper
;;;============================================================================

(def (org-table-replace-rows ed start end rows)
  "Replace the table region with formatted rows. Aligns the table."
  (let* ((widths (org-table-column-widths rows))
         (new-lines (map (lambda (row)
                           (if (eq? row 'separator)
                             (org-table-format-separator widths)
                             (org-table-format-row row widths)))
                         rows))
         (new-text (string-join new-lines "\n"))
         (start-pos (editor-position-from-line ed start))
         (end-pos (if (< (+ end 1) (editor-get-line-count ed))
                    (editor-position-from-line ed (+ end 1))
                    (editor-get-text-length ed))))
    (send-message ed SCI_SETTARGETSTART start-pos 0)
    (send-message ed SCI_SETTARGETEND end-pos 0)
    (editor-replace-target-text ed new-text)))

;;;============================================================================
;;; Formulas (#+TBLFM:)
;;;============================================================================

(def (org-table-parse-tblfm line)
  "Parse '#+TBLFM: $3=$1+$2::@2$3=10' into list of (target . formula) pairs."
  (let ((m (pregexp-match "^#\\+TBLFM:\\s*(.*)" (string-trim line))))
    (if (not m)
      '()
      (let ((formulas-str (list-ref m 1)))
        (map (lambda (part)
               (let ((eq-pos (string-contains part "=")))
                 (if eq-pos
                   (cons (string-trim (substring part 0 eq-pos))
                         (string-trim (substring part (+ eq-pos 1) (string-length part))))
                   (cons part ""))))
             (pregexp-split "::" formulas-str))))))

(def (org-table-eval-formula formula rows col)
  "Evaluate a simple formula. Supports $N (column ref) and basic +/-/* ."
  ;; Extract column references and compute
  (let ((parts (pregexp-split "([+\\-*/])" formula)))
    (let loop ((ps parts) (result 0) (op '+))
      (if (null? ps)
        (number->string result)
        (let ((part (string-trim (car ps))))
          (cond
            ((string=? part "+") (loop (cdr ps) result +))
            ((string=? part "-") (loop (cdr ps) result -))
            ((string=? part "*") (loop (cdr ps) result *))
            ((string=? part "/") (loop (cdr ps) result
                                       (lambda (a b) (if (= b 0) 0 (/ a b)))))
            ((pregexp-match "^\\$([0-9]+)$" part)
             => (lambda (m)
                  (let* ((ref-col (- (string->number (list-ref m 1)) 1))
                         (vals (filter-map
                                (lambda (row)
                                  (and (list? row)
                                       (< ref-col (length row))
                                       (string->number (string-trim (list-ref row ref-col)))))
                                rows))
                         (sum (apply + vals)))
                    (loop (cdr ps) (op result sum) +))))
            ((string->number part)
             => (lambda (n) (loop (cdr ps) (op result n) +)))
            (else (loop (cdr ps) result op))))))))

(def (org-table-recalculate ed)
  "Recalculate formulas in the #+TBLFM: line below the table."
  (let-values (((start end) (org-table-find-bounds ed)))
    (when (and start end)
      (let* ((tblfm-line-num (+ end 1))
             (total (editor-get-line-count ed)))
        (when (< tblfm-line-num total)
          (let ((tblfm-line (editor-get-line ed tblfm-line-num)))
            (when (string-prefix? "#+TBLFM:" (string-trim tblfm-line))
              (let* ((formulas (org-table-parse-tblfm tblfm-line))
                     (rows (org-table-get-rows ed start end)))
                ;; Apply each formula (simplified: column formulas only)
                (for-each
                  (lambda (pair)
                    (let ((target (car pair))
                          (formula (cdr pair)))
                      ;; Handle $N= formulas (whole column)
                      (let ((tm (pregexp-match "^\\$([0-9]+)$" target)))
                        (when tm
                          (let* ((target-col (- (string->number (list-ref tm 1)) 1))
                                 (val (org-table-eval-formula formula rows target-col)))
                            ;; Set the last data row's column to the result
                            (let loop ((i (- (length rows) 1)))
                              (when (>= i 0)
                                (if (list? (list-ref rows i))
                                  (when (< target-col (length (list-ref rows i)))
                                    (set! (car (list-tail (list-ref rows i) target-col)) val))
                                  (loop (- i 1))))))))))
                  formulas)
                (org-table-replace-rows ed start end rows)))))))))

;;;============================================================================
;;; CSV Import/Export
;;;============================================================================

(def (org-csv-to-table csv-text)
  "Convert CSV text to org table text."
  (let* ((lines (filter (lambda (s) (not (string=? s "")))
                        (string-split csv-text #\newline)))
         (rows (map (lambda (line)
                      (map string-trim (csv-split-line line)))
                    lines))
         (widths (org-table-column-widths rows))
         (table-lines (map (lambda (row) (org-table-format-row row widths)) rows)))
    (string-join table-lines "\n")))

(def (csv-split-line line)
  "Split a CSV line into fields, handling quoted fields."
  (let ((len (string-length line))
        (fields '())
        (current "")
        (in-quote? #f))
    (let loop ((i 0))
      (if (>= i len)
        (reverse (cons current fields))
        (let ((c (string-ref line i)))
          (cond
            ((and (char=? c #\") (not in-quote?))
             (set! in-quote? #t)
             (loop (+ i 1)))
            ((and (char=? c #\") in-quote?)
             (set! in-quote? #f)
             (loop (+ i 1)))
            ((and (char=? c #\,) (not in-quote?))
             (set! fields (cons current fields))
             (set! current "")
             (loop (+ i 1)))
            (else
             (set! current (string-append current (string c)))
             (loop (+ i 1)))))))))

(def (org-table-to-csv ed)
  "Export current table as CSV string."
  (let-values (((start end) (org-table-find-bounds ed)))
    (if (not start) ""
      (let* ((rows (org-table-get-rows ed start end))
             (data-rows (filter list? rows)))
        (string-join
         (map (lambda (row)
                (string-join
                 (map (lambda (cell)
                        (if (string-contains cell ",")
                          (string-append "\"" cell "\"")
                          cell))
                      row)
                 ","))
              data-rows)
         "\n")))))

;;;============================================================================
;;; List Utility Helpers
;;;============================================================================

(def (swap-list-elements lst i j)
  "Swap elements at positions i and j in a list."
  (let ((vec (list->vector lst)))
    (let ((tmp (vector-ref vec i)))
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp))
    (vector->list vec)))

(def (list-insert lst idx elem)
  "Insert elem at position idx in lst."
  (let loop ((i 0) (l lst) (acc '()))
    (if (= i idx)
      (append (reverse acc) (cons elem l))
      (if (null? l)
        (reverse (cons elem acc))
        (loop (+ i 1) (cdr l) (cons (car l) acc))))))

(def (list-remove-at lst idx)
  "Remove element at position idx from lst."
  (let loop ((i 0) (l lst) (acc '()))
    (if (null? l)
      (reverse acc)
      (if (= i idx)
        (append (reverse acc) (cdr l))
        (loop (+ i 1) (cdr l) (cons (car l) acc))))))

(def (filter-map f lst)
  "Map f over lst, filtering out #f results."
  (let loop ((l lst) (acc '()))
    (if (null? l)
      (reverse acc)
      (let ((v (f (car l))))
        (loop (cdr l) (if v (cons v acc) acc))))))
