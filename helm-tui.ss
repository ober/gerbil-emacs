;;; -*- Gerbil -*-
;;; Helm TUI renderer for gemacs
;;;
;;; Renders the helm candidate list in the bottom N rows of the terminal.
;;; Runs a modal input loop that handles navigation, selection, and filtering.

(export helm-tui-run!)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/echo
        :gemacs/helm)

;;;============================================================================
;;; Configuration
;;;============================================================================

(def *helm-tui-height* 12)  ;; number of rows for candidate list + prompt

;;;============================================================================
;;; Face helpers (reuse echo.ss pattern)
;;;============================================================================

(def (face-fg-rgb name)
  (let ((f (face-get name)))
    (if f
      (let ((color-str (face-fg f)))
        (if color-str
          (let-values (((r g b) (parse-hex-color color-str)))
            (+ (arithmetic-shift r 16) (arithmetic-shift g 8) b))
          #xd8d8d8))
      #xd8d8d8)))

(def (face-bg-rgb name)
  (let ((f (face-get name)))
    (if f
      (let ((color-str (face-bg f)))
        (if color-str
          (let-values (((r g b) (parse-hex-color color-str)))
            (+ (arithmetic-shift r 16) (arithmetic-shift g 8) b))
          #x181818))
      #x181818)))

;;;============================================================================
;;; Render the helm display
;;;============================================================================

(def (helm-tui-render! session width height)
  "Render the helm session in the bottom rows of the terminal."
  (let* ((candidates (helm-session-candidates session))
         (cand-count (vector-length candidates))
         (selected (helm-session-selected session))
         (scroll (helm-session-scroll-offset session))
         (pattern (helm-session-pattern session))
         (sources (helm-session-sources session))
         ;; Layout: bottom row = prompt, rows above = candidates
         (list-height (- *helm-tui-height* 1))
         (prompt-row (- height 1))
         (list-start-row (- height *helm-tui-height*))
         ;; Colors
         (fg-normal #xd8d8d8)
         (bg-normal #x1e1e1e)
         (fg-selected #xffffff)
         (bg-selected #x3a3a5a)
         (fg-header #x88aaff)
         (bg-header #x1e1e1e)
         (fg-dim #x888888)
         (fg-prompt #xb0b0b0))

    ;; Draw separator line
    (when (>= list-start-row 0)
      (tui-print! 0 list-start-row fg-dim bg-normal (make-string width #\─)))

    ;; Draw candidates
    (let ((visible-start (max 0 scroll))
          (visible-end (min cand-count (+ scroll list-height))))

      ;; Track current source for headers
      (let loop ((row (+ list-start-row 1))
                 (idx visible-start)
                 (last-source #f))
        (when (and (< row prompt-row) (< idx visible-end))
          (let* ((cand (vector-ref candidates idx))
                 (src (helm-candidate-source cand))
                 (src-name (helm-source-name src))
                 (is-selected (= idx selected))
                 (is-marked (memv idx (helm-session-marked session))))

            ;; Draw source header if source changed
            (if (not (eq? src last-source))
              (begin
                ;; Source header line
                (when (< row (- prompt-row 1))
                  (tui-print! 0 row fg-header bg-header
                              (make-string width #\space))
                  (tui-print! 1 row fg-header bg-header
                              (let ((hdr (string-append " " src-name ":")))
                                (if (> (string-length hdr) (- width 2))
                                  (substring hdr 0 (- width 2))
                                  hdr)))
                  ;; Draw candidate on next row
                  (let ((cand-row (+ row 1)))
                    (when (< cand-row prompt-row)
                      (helm-tui-draw-candidate! cand cand-row width
                                                is-selected is-marked
                                                fg-normal bg-normal
                                                fg-selected bg-selected)
                      (loop (+ cand-row 1) (+ idx 1) src)))))
              ;; Same source — just draw candidate
              (begin
                (helm-tui-draw-candidate! cand row width
                                          is-selected is-marked
                                          fg-normal bg-normal
                                          fg-selected bg-selected)
                (loop (+ row 1) (+ idx 1) last-source))))))

      ;; Clear remaining rows
      (let clear-loop ((row (+ list-start-row 1
                               (min list-height
                                    (+ (- visible-end visible-start)
                                       ;; Account for headers
                                       (count-source-transitions candidates
                                                                 visible-start visible-end))))))
        (when (< row prompt-row)
          (tui-print! 0 row fg-normal bg-normal (make-string width #\space))
          (clear-loop (+ row 1)))))

    ;; Draw prompt line
    (let* ((count-str (string-append "[" (number->string (if (> cand-count 0) (+ selected 1) 0))
                                     "/" (number->string cand-count) "]"))
           (prompt-text (string-append "Pattern: " pattern))
           (suffix (string-append " " count-str))
           (cursor-pos (string-length prompt-text)))
      (tui-print! 0 prompt-row fg-prompt bg-normal (make-string width #\space))
      (tui-print! 0 prompt-row fg-prompt bg-normal
                  (if (> (string-length prompt-text) width)
                    (substring prompt-text 0 width)
                    prompt-text))
      ;; Count in dim color
      (when (< cursor-pos (- width (string-length suffix)))
        (tui-print! (- width (string-length suffix)) prompt-row
                    fg-dim bg-normal suffix))
      ;; Position cursor
      (tui-set-cursor! (min cursor-pos (- width 1)) prompt-row))

    (tui-present!)))

(def (helm-tui-draw-candidate! cand row width is-selected is-marked
                                fg-normal bg-normal fg-selected bg-selected)
  "Draw a single candidate line."
  (let* ((fg (if is-selected fg-selected fg-normal))
         (bg (if is-selected bg-selected bg-normal))
         (prefix (cond (is-marked "* ")
                       (is-selected "> ")
                       (else "  ")))
         (text (helm-candidate-display cand))
         (line (string-append prefix text))
         (display-line (if (> (string-length line) width)
                         (substring line 0 width)
                         line)))
    (tui-print! 0 row fg bg (make-string width #\space))
    (tui-print! 0 row fg bg display-line)))

(def (count-source-transitions candidates start end)
  "Count how many times the source changes between start and end indices."
  (if (or (>= start end) (= (vector-length candidates) 0))
    0
    (let loop ((i (+ start 1)) (count 0)
               (last-src (helm-candidate-source (vector-ref candidates start))))
      (if (>= i end)
        count
        (let ((src (helm-candidate-source (vector-ref candidates i))))
          (if (eq? src last-src)
            (loop (+ i 1) count last-src)
            (loop (+ i 1) (+ count 1) src)))))))

;;;============================================================================
;;; Input loop
;;;============================================================================

(def (helm-tui-run! session)
  "Run the helm TUI session. Returns the selected candidate's real value, or #f if cancelled."
  (let* ((width (tui-width))
         (height (tui-height)))

    ;; Initial render
    (helm-tui-render! session width height)

    ;; Event loop
    (let loop ()
      (let ((ev (tui-poll-event)))
        (cond
          ((not ev) (loop))
          ((not (tui-event-key? ev)) (loop))
          (else
           (let* ((key (tui-event-key ev))
                  (ch  (tui-event-ch ev))
                  (mod (tui-event-mod ev))
                  (alt? (not (zero? (bitwise-and mod TB_MOD_ALT))))
                  (candidates (helm-session-candidates session))
                  (cand-count (vector-length candidates))
                  (selected (helm-session-selected session)))
             (cond
               ;; C-g (0x07) → cancel
               ((= key #x07)
                (echo-message! (make-initial-echo-state) "Quit")
                #f)

               ;; Enter (0x0D) → accept selected
               ((= key #x0D)
                (if (> cand-count 0)
                  (let ((cand (vector-ref candidates selected)))
                    (helm-session-store! session)
                    (helm-candidate-real cand))
                  ;; No candidates — return pattern as-is (for create-buffer etc.)
                  (let ((pat (helm-session-pattern session)))
                    (helm-session-store! session)
                    (if (> (string-length pat) 0) pat #f))))

               ;; C-n / Down → next candidate
               ((or (and (= key 0) (= ch (char->integer #\n)) (not alt?)
                         ;; Check for Ctrl — key=0 and ch=14 (C-n)
                         #f)
                    (= key #x0e)  ;; C-n = 0x0e
                    (= key TB_KEY_ARROW_DOWN))
                (when (> cand-count 0)
                  (set! (helm-session-selected session)
                    (modulo (+ selected 1) cand-count))
                  (helm-ensure-visible! session))
                (helm-tui-render! session width height)
                (loop))

               ;; C-p / Up → previous candidate
               ((or (= key #x10)  ;; C-p = 0x10
                    (= key TB_KEY_ARROW_UP))
                (when (> cand-count 0)
                  (set! (helm-session-selected session)
                    (modulo (- selected 1) cand-count))
                  (helm-ensure-visible! session))
                (helm-tui-render! session width height)
                (loop))

               ;; C-v / PageDown → page down
               ((or (= key #x16)  ;; C-v
                    (= key TB_KEY_PGDN))
                (when (> cand-count 0)
                  (let ((page (- *helm-tui-height* 2)))
                    (set! (helm-session-selected session)
                      (min (- cand-count 1) (+ selected page)))
                    (helm-ensure-visible! session)))
                (helm-tui-render! session width height)
                (loop))

               ;; M-v / PageUp → page up
               ((or (and alt? (= ch (char->integer #\v)))
                    (= key TB_KEY_PGUP))
                (when (> cand-count 0)
                  (let ((page (- *helm-tui-height* 2)))
                    (set! (helm-session-selected session)
                      (max 0 (- selected page)))
                    (helm-ensure-visible! session)))
                (helm-tui-render! session width height)
                (loop))

               ;; M-< → first candidate
               ((and alt? (= ch (char->integer #\<)))
                (set! (helm-session-selected session) 0)
                (set! (helm-session-scroll-offset session) 0)
                (helm-tui-render! session width height)
                (loop))

               ;; M-> → last candidate
               ((and alt? (= ch (char->integer #\>)))
                (when (> cand-count 0)
                  (set! (helm-session-selected session) (- cand-count 1))
                  (helm-ensure-visible! session))
                (helm-tui-render! session width height)
                (loop))

               ;; C-SPC → toggle mark
               ((= key #x00)  ;; C-@ = C-SPC
                (when (> cand-count 0)
                  (let ((marked (helm-session-marked session)))
                    (if (memv selected marked)
                      (set! (helm-session-marked session)
                        (filter (lambda (i) (not (= i selected))) marked))
                      (set! (helm-session-marked session)
                        (cons selected marked))))
                  ;; Move to next after marking
                  (when (< (+ selected 1) cand-count)
                    (set! (helm-session-selected session) (+ selected 1))
                    (helm-ensure-visible! session)))
                (helm-tui-render! session width height)
                (loop))

               ;; Tab → action menu (for now, just accept like RET)
               ((= key #x09)
                (if (> cand-count 0)
                  (let ((cand (vector-ref candidates selected)))
                    (helm-session-store! session)
                    (helm-candidate-real cand))
                  #f))

               ;; C-j → persistent action
               ;; Note: C-j = 0x0a (newline), but we use Enter (0x0d) for accept
               ;; In TUI, C-j is typically 0x0a
               ((= key #x0a)
                (when (and (> cand-count 0))
                  (let* ((cand (vector-ref candidates selected))
                         (src (helm-candidate-source cand))
                         (pa (helm-source-persistent-action src)))
                    (when pa
                      (pa (helm-candidate-real cand)))))
                (helm-tui-render! session width height)
                (loop))

               ;; Backspace → delete last char of pattern
               ((or (= key #x08) (= key #x7F))
                (let ((pat (helm-session-pattern session)))
                  (when (> (string-length pat) 0)
                    (set! (helm-session-pattern session)
                      (substring pat 0 (- (string-length pat) 1)))
                    ;; Re-filter
                    (set! (helm-session-candidates session)
                      (helm-filter-all session))
                    (set! (helm-session-selected session) 0)
                    (set! (helm-session-scroll-offset session) 0)))
                (helm-tui-render! session width height)
                (loop))

               ;; Printable char → append to pattern
               ((> ch 31)
                (set! (helm-session-pattern session)
                  (string-append (helm-session-pattern session)
                                 (string (integer->char ch))))
                ;; Re-filter
                (set! (helm-session-candidates session)
                  (helm-filter-all session))
                (set! (helm-session-selected session) 0)
                (set! (helm-session-scroll-offset session) 0)
                (helm-tui-render! session width height)
                (loop))

               ;; Ignore other keys
               (else (loop))))))))))

;;;============================================================================
;;; Scroll management
;;;============================================================================

(def (helm-ensure-visible! session)
  "Ensure the selected candidate is visible in the scroll window."
  (let* ((selected (helm-session-selected session))
         (scroll (helm-session-scroll-offset session))
         (visible-height (- *helm-tui-height* 2)))  ;; minus prompt and separator
    (cond
      ((< selected scroll)
       (set! (helm-session-scroll-offset session) selected))
      ((>= selected (+ scroll visible-height))
       (set! (helm-session-scroll-offset session)
         (- selected visible-height -1))))))
