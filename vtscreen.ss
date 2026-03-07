;;; -*- Gerbil -*-
;;; VT100 virtual terminal screen buffer.
;;;
;;; Maintains a rows×cols character grid. Processes ANSI/VT100 escape
;;; sequences for cursor movement, screen clearing, and scrolling.
;;; Renders the grid to a plain string for display in Scintilla.
;;;
;;; Supported CSI sequences:
;;;   H/f  — Cursor position (row;col or home)
;;;   A    — Cursor up
;;;   B    — Cursor down
;;;   C    — Cursor forward
;;;   D    — Cursor backward
;;;   J    — Erase in display (0=below, 1=above, 2=all, 3=scrollback)
;;;   K    — Erase in line (0=right, 1=left, 2=whole)
;;;   G    — Cursor horizontal absolute
;;;   d    — Cursor vertical absolute
;;;   m    — SGR (colors — tracked but not rendered here)
;;;   r    — Set scrolling region
;;;   L    — Insert lines
;;;   M    — Delete lines
;;;   S    — Scroll up
;;;   T    — Scroll down
;;;   @    — Insert characters
;;;   P    — Delete characters
;;;   X    — Erase characters
;;;   s    — Save cursor position
;;;   u    — Restore cursor position
;;;   ?... h/l — DEC private modes (ignored)
;;;   n    — Device status report (ignored)

(export new-vtscreen
        vtscreen-feed!
        vtscreen-render
        vtscreen-resize!
        vtscreen-rows
        vtscreen-cols
        vtscreen-cursor-row
        vtscreen-cursor-col
        vtscreen-alt-screen?)

(import :std/sugar)

;;;============================================================================
;;; Screen data structure
;;;============================================================================

(defstruct vtscreen
  (rows          ; int — number of rows
   cols          ; int — number of columns
   grid          ; vector of vectors of chars
   cursor-row    ; int — 0-based
   cursor-col    ; int — 0-based
   saved-row     ; int — saved cursor row
   saved-col     ; int — saved cursor col
   scroll-top    ; int — top of scroll region (0-based)
   scroll-bottom ; int — bottom of scroll region (0-based, inclusive)
   wrap-pending  ; bool — deferred wrap at right margin (VT100 autowrap)
   alt-screen?   ; bool — full-screen program detected (alt screen or clear+home)
   last-char     ; char — last graphic char written (for REP/CSI b)
   ;; Parsing state for escape sequences
   parse-state   ; 'normal | 'esc | 'csi | 'osc | 'charset-skip | 'dcs | 'ss-skip
   csi-params    ; string accumulator for CSI parameter bytes
   osc-buf)      ; string accumulator for OSC
  transparent: #t)

(def (new-vtscreen (rows 24) (cols 80))
  "Create a new virtual terminal screen."
  (let ((grid (make-vector rows #f)))
    (let loop ((r 0))
      (when (< r rows)
        (vector-set! grid r (make-vector cols #\space))
        (loop (+ r 1))))
    (make-vtscreen rows cols grid 0 0 0 0 0 (- rows 1) #f #f #\space 'normal "" "")))

(def (vtscreen-resize! vt new-rows new-cols)
  "Resize the virtual screen. Preserves content where possible."
  (let* ((old-rows (vtscreen-rows vt))
         (old-cols (vtscreen-cols vt))
         (old-grid (vtscreen-grid vt))
         (new-grid (make-vector new-rows #f)))
    (let loop ((r 0))
      (when (< r new-rows)
        (let ((new-row (make-vector new-cols #\space)))
          (when (< r old-rows)
            (let ((old-row (vector-ref old-grid r)))
              (let copy ((c 0))
                (when (and (< c new-cols) (< c old-cols))
                  (vector-set! new-row c (vector-ref old-row c))
                  (copy (+ c 1))))))
          (vector-set! new-grid r new-row))
        (loop (+ r 1))))
    (set! (vtscreen-grid vt) new-grid)
    (set! (vtscreen-rows vt) new-rows)
    (set! (vtscreen-cols vt) new-cols)
    (set! (vtscreen-cursor-row vt)
      (min (vtscreen-cursor-row vt) (- new-rows 1)))
    (set! (vtscreen-cursor-col vt)
      (min (vtscreen-cursor-col vt) (- new-cols 1)))
    (set! (vtscreen-scroll-top vt) 0)
    (set! (vtscreen-scroll-bottom vt) (- new-rows 1))
    (set! (vtscreen-wrap-pending vt) #f)))

;;;============================================================================
;;; Grid operations
;;;============================================================================

(def (grid-clear-row! grid row cols (start-col 0))
  "Clear a row from start-col to end with spaces."
  (let ((row-vec (vector-ref grid row)))
    (let loop ((c start-col))
      (when (< c cols)
        (vector-set! row-vec c #\space)
        (loop (+ c 1))))))

(def (grid-clear-row-left! grid row end-col)
  "Clear a row from column 0 to end-col (inclusive)."
  (let ((row-vec (vector-ref grid row)))
    (let loop ((c 0))
      (when (<= c end-col)
        (vector-set! row-vec c #\space)
        (loop (+ c 1))))))

(def (grid-clear-region! grid start-row end-row cols)
  "Clear rows from start-row to end-row (inclusive)."
  (let loop ((r start-row))
    (when (<= r end-row)
      (grid-clear-row! grid r cols)
      (loop (+ r 1)))))

(def (grid-scroll-up! grid top bottom cols (n 1))
  "Scroll lines up within [top..bottom] region by n lines."
  (let loop ((count 0))
    (when (< count n)
      ;; Shift rows up by one within the region
      (let shift ((r top))
        (when (< r bottom)
          (vector-set! grid r (vector-ref grid (+ r 1)))
          (shift (+ r 1))))
      ;; Clear bottom row
      (vector-set! grid bottom (make-vector cols #\space))
      (loop (+ count 1)))))

(def (grid-scroll-down! grid top bottom cols (n 1))
  "Scroll lines down within [top..bottom] region by n lines."
  (let loop ((count 0))
    (when (< count n)
      ;; Shift rows down by one within the region
      (let shift ((r bottom))
        (when (> r top)
          (vector-set! grid r (vector-ref grid (- r 1)))
          (shift (- r 1))))
      ;; Clear top row
      (vector-set! grid top (make-vector cols #\space))
      (loop (+ count 1)))))

;;;============================================================================
;;; Feed data to the virtual screen
;;;============================================================================

(def (vtscreen-feed! vt data)
  "Process a string of terminal output through the VT100 emulator.
   Updates the screen grid in place."
  (let ((len (string-length data)))
    (let loop ((i 0))
      (when (< i len)
        (let ((ch (string-ref data i)))
          (case (vtscreen-parse-state vt)
            ((normal)
             (let ((code (char->integer ch)))
               (cond
                 ;; ESC — start escape sequence
                 ((= code 27)
                  (set! (vtscreen-parse-state vt) 'esc))
                 ;; 8-bit C1: CSI (0x9B)
                 ((= code #x9B)
                  (set! (vtscreen-parse-state vt) 'csi)
                  (set! (vtscreen-csi-params vt) ""))
                 ;; 8-bit C1: OSC (0x9D)
                 ((= code #x9D)
                  (set! (vtscreen-parse-state vt) 'osc)
                  (set! (vtscreen-osc-buf vt) ""))
                 ;; 8-bit C1: DCS (0x90)
                 ((= code #x90)
                  (set! (vtscreen-parse-state vt) 'dcs))
                 ;; 8-bit C1: ST (0x9C) — string terminator (ignore if not in string)
                 ((= code #x9C) (void))
                 ;; Other 8-bit C1 controls (0x80-0x9F) — ignore
                 ((and (>= code #x80) (<= code #x9F)) (void))
                 ;; BEL — bell (ignore)
                 ((= code 7) (void))
                 ;; BS — backspace
                 ((= code 8)
                  (when (> (vtscreen-cursor-col vt) 0)
                    (set! (vtscreen-cursor-col vt)
                      (- (vtscreen-cursor-col vt) 1))))
                 ;; TAB
                 ((= code 9)
                  (let ((col (vtscreen-cursor-col vt)))
                    (set! (vtscreen-cursor-col vt)
                      (min (- (vtscreen-cols vt) 1)
                           (* (+ (quotient col 8) 1) 8)))))
                 ;; LF, VT, FF — line feed (with implicit CR, as in newline mode)
                 ((or (= code 10) (= code 11) (= code 12))
                  (set! (vtscreen-wrap-pending vt) #f)
                  (set! (vtscreen-cursor-col vt) 0)
                  (vt-linefeed! vt))
                 ;; CR — carriage return
                 ((= code 13)
                  (set! (vtscreen-wrap-pending vt) #f)
                  (set! (vtscreen-cursor-col vt) 0))
                 ;; SO/SI — shift out/in (charset switching, ignore)
                 ((or (= code 14) (= code 15)) (void))
                 ;; Other C0 controls — ignore
                 ((< code 32) (void))
                 ;; Printable character (>= 0x20)
                 (else
                  ;; If wrap is pending from previous char at right margin,
                  ;; execute it now before writing the new character
                  (when (vtscreen-wrap-pending vt)
                    (set! (vtscreen-wrap-pending vt) #f)
                    (set! (vtscreen-cursor-col vt) 0)
                    (vt-linefeed! vt))
                  (let ((row (vtscreen-cursor-row vt))
                        (col (vtscreen-cursor-col vt))
                        (cols (vtscreen-cols vt)))
                    ;; Write character at cursor position
                    (when (and (>= row 0) (< row (vtscreen-rows vt))
                               (>= col 0) (< col cols))
                      (vector-set! (vector-ref (vtscreen-grid vt) row) col ch))
                    ;; Record for REP (CSI b)
                    (set! (vtscreen-last-char vt) ch)
                    ;; Advance cursor
                    (if (< col (- cols 1))
                      (set! (vtscreen-cursor-col vt) (+ col 1))
                      ;; At right edge — set pending wrap (deferred)
                      (set! (vtscreen-wrap-pending vt) #t)))))))

            ((esc)
             (cond
               ;; CSI: ESC [
               ((char=? ch #\[)
                (set! (vtscreen-parse-state vt) 'csi)
                (set! (vtscreen-csi-params vt) ""))
               ;; OSC: ESC ]
               ((char=? ch #\])
                (set! (vtscreen-parse-state vt) 'osc)
                (set! (vtscreen-osc-buf vt) ""))
               ;; Save cursor: ESC 7
               ((char=? ch #\7)
                (set! (vtscreen-saved-row vt) (vtscreen-cursor-row vt))
                (set! (vtscreen-saved-col vt) (vtscreen-cursor-col vt))
                (set! (vtscreen-parse-state vt) 'normal))
               ;; Restore cursor: ESC 8
               ((char=? ch #\8)
                (set! (vtscreen-cursor-row vt) (vtscreen-saved-row vt))
                (set! (vtscreen-cursor-col vt) (vtscreen-saved-col vt))
                (set! (vtscreen-parse-state vt) 'normal))
               ;; Reverse index: ESC M
               ((char=? ch #\M)
                (vt-reverse-index! vt)
                (set! (vtscreen-parse-state vt) 'normal))
               ;; DCS: ESC P — Device Control String (VT220)
               ((char=? ch #\P)
                (set! (vtscreen-parse-state vt) 'dcs))
               ;; SS2: ESC N — Single Shift G2 (VT220, skip next char)
               ((char=? ch #\N)
                (set! (vtscreen-parse-state vt) 'ss-skip))
               ;; SS3: ESC O — Single Shift G3 (VT220, skip next char)
               ((char=? ch #\O)
                (set! (vtscreen-parse-state vt) 'ss-skip))
               ;; ESC D — Index (move cursor down, scroll if needed)
               ((char=? ch #\D)
                (vt-linefeed! vt)
                (set! (vtscreen-parse-state vt) 'normal))
               ;; ESC E — Next Line (CR + LF)
               ((char=? ch #\E)
                (set! (vtscreen-cursor-col vt) 0)
                (vt-linefeed! vt)
                (set! (vtscreen-parse-state vt) 'normal))
               ;; ESC c — Full Reset (RIS)
               ((char=? ch #\c)
                (let ((rows (vtscreen-rows vt))
                      (cols (vtscreen-cols vt)))
                  (grid-clear-region! (vtscreen-grid vt) 0 (- rows 1) cols)
                  (set! (vtscreen-cursor-row vt) 0)
                  (set! (vtscreen-cursor-col vt) 0)
                  (set! (vtscreen-scroll-top vt) 0)
                  (set! (vtscreen-scroll-bottom vt) (- rows 1))
                  (set! (vtscreen-wrap-pending vt) #f)
                  (set! (vtscreen-alt-screen? vt) #f)
                  (set! (vtscreen-parse-state vt) 'normal)))
               ;; Character set designation: ESC ( X, ESC ) X — skip next char
               ((memv ch '(#\( #\) #\* #\+))
                (set! (vtscreen-parse-state vt) 'charset-skip))
               ;; Anything else: ignore and return to normal
               (else
                (set! (vtscreen-parse-state vt) 'normal))))

            ((csi)
             (cond
               ;; Parameter/intermediate bytes: 0x20-0x3F
               ((and (char>=? ch #\space) (char<=? ch #\?))
                (set! (vtscreen-csi-params vt)
                  (string-append (vtscreen-csi-params vt) (string ch))))
               ;; Final byte: 0x40-0x7E — execute CSI command
               ((and (char>=? ch #\@) (char<=? ch #\~))
                (vt-csi-dispatch! vt ch (vtscreen-csi-params vt))
                (set! (vtscreen-parse-state vt) 'normal))
               ;; Unexpected: return to normal
               (else
                (set! (vtscreen-parse-state vt) 'normal))))

            ((osc)
             (cond
               ;; BEL terminates OSC
               ((char=? ch (integer->char 7))
                (set! (vtscreen-parse-state vt) 'normal))
               ;; ST (ESC \) terminates OSC — check for ESC
               ((char=? ch (integer->char 27))
                ;; Next char should be \, but we'll just end OSC
                (set! (vtscreen-parse-state vt) 'normal))
               ;; Accumulate (but don't use)
               (else (void))))

            ((charset-skip)
             ;; Eat one character (the charset designator) and return to normal
             (set! (vtscreen-parse-state vt) 'normal))

            ((dcs)
             ;; Device Control String (VT220): consume until ST
             ;; ST = ESC \ or 8-bit ST (0x9C)
             (cond
               ((char=? ch (integer->char #x9C))
                (set! (vtscreen-parse-state vt) 'normal))
               ((char=? ch (integer->char 27))
                ;; ESC starts the ST sequence; next char should be \
                (set! (vtscreen-parse-state vt) 'dcs-esc))
               ;; Accumulate silently
               (else (void))))

            ((dcs-esc)
             ;; After ESC within DCS — expect \ for ST
             (set! (vtscreen-parse-state vt) 'normal))

            ((ss-skip)
             ;; Single Shift (SS2/SS3): skip one character and return to normal
             (set! (vtscreen-parse-state vt) 'normal))

            (else
             (set! (vtscreen-parse-state vt) 'normal))))
        (loop (+ i 1))))))

;;;============================================================================
;;; Cursor movement helpers
;;;============================================================================

(def (vt-linefeed! vt)
  "Handle LF: move cursor down, scroll if at bottom of scroll region."
  (let ((row (vtscreen-cursor-row vt))
        (bottom (vtscreen-scroll-bottom vt)))
    (if (>= row bottom)
      ;; At bottom of scroll region — scroll up
      (grid-scroll-up! (vtscreen-grid vt)
                        (vtscreen-scroll-top vt)
                        bottom
                        (vtscreen-cols vt))
      ;; Not at bottom — just move down
      (set! (vtscreen-cursor-row vt) (+ row 1)))))

(def (vt-reverse-index! vt)
  "Handle reverse index (ESC M): move cursor up, scroll down if at top."
  (let ((row (vtscreen-cursor-row vt))
        (top (vtscreen-scroll-top vt)))
    (if (<= row top)
      ;; At top of scroll region — scroll down
      (grid-scroll-down! (vtscreen-grid vt)
                          top
                          (vtscreen-scroll-bottom vt)
                          (vtscreen-cols vt))
      ;; Not at top — just move up
      (set! (vtscreen-cursor-row vt) (- row 1)))))

;;;============================================================================
;;; CSI command dispatch
;;;============================================================================

(def (parse-csi-params param-str)
  "Parse CSI parameter string into a list of integers.
   Semicolons separate parameters. Empty/missing = 0.
   Leading '?' is stripped (DEC private mode prefix)."
  (let ((s (if (and (> (string-length param-str) 0)
                    (char=? (string-ref param-str 0) #\?))
             (substring param-str 1 (string-length param-str))
             param-str)))
    (if (string=? s "")
      []
      (let loop ((i 0) (start 0) (acc []))
        (cond
          ((>= i (string-length s))
           (reverse (cons (let ((sub (substring s start i)))
                            (if (string=? sub "") 0
                              (or (string->number sub) 0)))
                          acc)))
          ((char=? (string-ref s i) #\;)
           (loop (+ i 1) (+ i 1)
                 (cons (let ((sub (substring s start i)))
                         (if (string=? sub "") 0
                           (or (string->number sub) 0)))
                       acc)))
          (else (loop (+ i 1) start acc)))))))

(def (csi-param params idx default)
  "Get CSI parameter at index, or default if missing/zero."
  (let ((v (if (< idx (length params)) (list-ref params idx) 0)))
    (if (= v 0) default v)))

(def (is-private-mode? param-str)
  "Check if CSI parameter string starts with '?' (DEC private mode)."
  (and (> (string-length param-str) 0)
       (char=? (string-ref param-str 0) #\?)))

(def (vt-csi-dispatch! vt final param-str)
  "Dispatch a CSI sequence."
  ;; Any CSI sequence cancels pending wrap
  (set! (vtscreen-wrap-pending vt) #f)
  (let ((params (parse-csi-params param-str))
        (rows (vtscreen-rows vt))
        (cols (vtscreen-cols vt))
        (grid (vtscreen-grid vt)))
    (case final
      ;; H/f — Cursor position
      ((#\H #\f)
       (let ((row (- (csi-param params 0 1) 1))
             (col (- (csi-param params 1 1) 1)))
         (set! (vtscreen-cursor-row vt) (max 0 (min row (- rows 1))))
         (set! (vtscreen-cursor-col vt) (max 0 (min col (- cols 1))))))

      ;; A — Cursor up
      ((#\A)
       (let ((n (csi-param params 0 1)))
         (set! (vtscreen-cursor-row vt)
           (max (vtscreen-scroll-top vt)
                (- (vtscreen-cursor-row vt) n)))))

      ;; B — Cursor down
      ((#\B)
       (let ((n (csi-param params 0 1)))
         (set! (vtscreen-cursor-row vt)
           (min (vtscreen-scroll-bottom vt)
                (+ (vtscreen-cursor-row vt) n)))))

      ;; C — Cursor forward
      ((#\C)
       (let ((n (csi-param params 0 1)))
         (set! (vtscreen-cursor-col vt)
           (min (- cols 1) (+ (vtscreen-cursor-col vt) n)))))

      ;; D — Cursor backward
      ((#\D)
       (let ((n (csi-param params 0 1)))
         (set! (vtscreen-cursor-col vt)
           (max 0 (- (vtscreen-cursor-col vt) n)))))

      ;; E — Cursor Next Line (move down N, go to column 0)
      ((#\E)
       (let ((n (csi-param params 0 1)))
         (set! (vtscreen-cursor-col vt) 0)
         (set! (vtscreen-cursor-row vt)
           (min (vtscreen-scroll-bottom vt)
                (+ (vtscreen-cursor-row vt) n)))))

      ;; F — Cursor Previous Line (move up N, go to column 0)
      ((#\F)
       (let ((n (csi-param params 0 1)))
         (set! (vtscreen-cursor-col vt) 0)
         (set! (vtscreen-cursor-row vt)
           (max (vtscreen-scroll-top vt)
                (- (vtscreen-cursor-row vt) n)))))

      ;; G — Cursor horizontal absolute
      ((#\G)
       (let ((col (- (csi-param params 0 1) 1)))
         (set! (vtscreen-cursor-col vt)
           (max 0 (min col (- cols 1))))))

      ;; d — Cursor vertical absolute
      ((#\d)
       (let ((row (- (csi-param params 0 1) 1)))
         (set! (vtscreen-cursor-row vt)
           (max 0 (min row (- rows 1))))))

      ;; J — Erase in display
      ((#\J)
       (let ((mode (csi-param params 0 0))
             (crow (vtscreen-cursor-row vt))
             (ccol (vtscreen-cursor-col vt)))
         (cond
           ;; 0: Clear from cursor to end
           ((= mode 0)
            (grid-clear-row! grid crow cols ccol)
            (grid-clear-region! grid (+ crow 1) (- rows 1) cols))
           ;; 1: Clear from beginning to cursor
           ((= mode 1)
            (grid-clear-region! grid 0 (- crow 1) cols)
            (grid-clear-row-left! grid crow ccol))
           ;; 2 or 3: Clear entire screen — indicates full-screen program
           ((or (= mode 2) (= mode 3))
            (set! (vtscreen-alt-screen? vt) #t)
            (grid-clear-region! grid 0 (- rows 1) cols)))))

      ;; K — Erase in line
      ((#\K)
       (let ((mode (csi-param params 0 0))
             (crow (vtscreen-cursor-row vt))
             (ccol (vtscreen-cursor-col vt)))
         (cond
           ;; 0: Clear from cursor to end of line
           ((= mode 0)
            (grid-clear-row! grid crow cols ccol))
           ;; 1: Clear from beginning of line to cursor
           ((= mode 1)
            (grid-clear-row-left! grid crow ccol))
           ;; 2: Clear entire line
           ((= mode 2)
            (grid-clear-row! grid crow cols)))))

      ;; r — Set scrolling region
      ((#\r)
       (let ((top (- (csi-param params 0 1) 1))
             (bot (- (csi-param params 1 rows) 1)))
         (set! (vtscreen-scroll-top vt) (max 0 (min top (- rows 1))))
         (set! (vtscreen-scroll-bottom vt) (max 0 (min bot (- rows 1))))
         ;; Cursor moves to home after setting scroll region
         (set! (vtscreen-cursor-row vt) 0)
         (set! (vtscreen-cursor-col vt) 0)))

      ;; L — Insert lines
      ((#\L)
       (let ((n (csi-param params 0 1))
             (crow (vtscreen-cursor-row vt))
             (bottom (vtscreen-scroll-bottom vt)))
         (when (<= crow bottom)
           (grid-scroll-down! grid crow bottom cols n))))

      ;; M — Delete lines
      ((#\M)
       (let ((n (csi-param params 0 1))
             (crow (vtscreen-cursor-row vt))
             (bottom (vtscreen-scroll-bottom vt)))
         (when (<= crow bottom)
           (grid-scroll-up! grid crow bottom cols n))))

      ;; S — Scroll up
      ((#\S)
       (let ((n (csi-param params 0 1)))
         (grid-scroll-up! grid
                          (vtscreen-scroll-top vt)
                          (vtscreen-scroll-bottom vt)
                          cols n)))

      ;; T — Scroll down
      ((#\T)
       (let ((n (csi-param params 0 1)))
         (grid-scroll-down! grid
                            (vtscreen-scroll-top vt)
                            (vtscreen-scroll-bottom vt)
                            cols n)))

      ;; @ — Insert characters (shift right)
      ((#\@)
       (let* ((n (csi-param params 0 1))
              (crow (vtscreen-cursor-row vt))
              (ccol (vtscreen-cursor-col vt))
              (row-vec (vector-ref grid crow)))
         (let shift ((c (- cols 1)))
           (when (>= c (+ ccol n))
             (vector-set! row-vec c (vector-ref row-vec (- c n)))
             (shift (- c 1))))
         (let clear ((c ccol))
           (when (< c (min (+ ccol n) cols))
             (vector-set! row-vec c #\space)
             (clear (+ c 1))))))

      ;; P — Delete characters (shift left)
      ((#\P)
       (let* ((n (csi-param params 0 1))
              (crow (vtscreen-cursor-row vt))
              (ccol (vtscreen-cursor-col vt))
              (row-vec (vector-ref grid crow)))
         (let shift ((c ccol))
           (when (< c (- cols n))
             (vector-set! row-vec c (vector-ref row-vec (+ c n)))
             (shift (+ c 1))))
         (let clear ((c (max ccol (- cols n))))
           (when (< c cols)
             (vector-set! row-vec c #\space)
             (clear (+ c 1))))))

      ;; X — Erase characters
      ((#\X)
       (let* ((n (csi-param params 0 1))
              (crow (vtscreen-cursor-row vt))
              (ccol (vtscreen-cursor-col vt))
              (row-vec (vector-ref grid crow)))
         (let clear ((c ccol))
           (when (and (< c (+ ccol n)) (< c cols))
             (vector-set! row-vec c #\space)
             (clear (+ c 1))))))

      ;; s — Save cursor position
      ((#\s)
       (set! (vtscreen-saved-row vt) (vtscreen-cursor-row vt))
       (set! (vtscreen-saved-col vt) (vtscreen-cursor-col vt)))

      ;; u — Restore cursor position
      ((#\u)
       (set! (vtscreen-cursor-row vt) (vtscreen-saved-row vt))
       (set! (vtscreen-cursor-col vt) (vtscreen-saved-col vt)))

      ;; m — SGR (Select Graphic Rendition) — ignore for grid rendering
      ((#\m) (void))

      ;; h/l — Set/Reset mode (DEC private modes)
      ((#\h #\l)
       ;; Track alternate screen buffer activation
       (when (is-private-mode? param-str)
         (let ((ps (parse-csi-params param-str)))
           (for-each (lambda (p)
                       (when (or (= p 1049) (= p 1047))
                         (set! (vtscreen-alt-screen? vt) (char=? final #\h))))
                     ps))))

      ;; b — REP: Repeat previous graphic character (VT220)
      ((#\b)
       (let ((n (csi-param params 0 1))
             (ch (vtscreen-last-char vt)))
         (let rep ((j 0))
           (when (< j n)
             (let ((row (vtscreen-cursor-row vt))
                   (col (vtscreen-cursor-col vt)))
               (when (and (>= row 0) (< row rows)
                          (>= col 0) (< col cols))
                 (vector-set! (vector-ref grid row) col ch))
               (if (< col (- cols 1))
                 (set! (vtscreen-cursor-col vt) (+ col 1))
                 (begin
                   (set! (vtscreen-cursor-col vt) 0)
                   (vt-linefeed! vt))))
             (rep (+ j 1))))))

      ;; n — Device status report — ignore
      ((#\n) (void))

      ;; c — Device attributes report — ignore
      ((#\c) (void))

      ;; Anything else: ignore
      (else (void)))))

;;;============================================================================
;;; Render the screen to a string
;;;============================================================================

(def (vtscreen-render vt)
  "Render the virtual screen grid to a string.
   Trailing spaces on each line are trimmed.
   Trailing empty lines are trimmed."
  (let* ((rows (vtscreen-rows vt))
         (cols (vtscreen-cols vt))
         (grid (vtscreen-grid vt)))
    (let ((out (open-output-string)))
      (let ((last-nonempty -1))
        ;; Find last non-empty row
        (let scan ((r (- rows 1)))
          (when (>= r 0)
            (if (row-empty? (vector-ref grid r) cols)
              (scan (- r 1))
              (set! last-nonempty r))))
        ;; Render rows up to last non-empty
        (let loop ((r 0))
          (when (<= r last-nonempty)
            (when (> r 0) (write-char #\newline out))
            (let* ((row-vec (vector-ref grid r))
                   ;; Find last non-space char in row
                   (last-col (let scan ((c (- cols 1)))
                               (if (and (>= c 0)
                                        (char=? (vector-ref row-vec c) #\space))
                                 (scan (- c 1))
                                 c))))
              (let col-loop ((c 0))
                (when (<= c last-col)
                  (write-char (vector-ref row-vec c) out)
                  (col-loop (+ c 1)))))
            (loop (+ r 1)))))
      (get-output-string out))))

(def (row-empty? row-vec cols)
  "Check if a row is all spaces."
  (let loop ((c 0))
    (if (>= c cols) #t
      (if (char=? (vector-ref row-vec c) #\space)
        (loop (+ c 1))
        #f))))
