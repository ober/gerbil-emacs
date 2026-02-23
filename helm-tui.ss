;;; -*- Gerbil -*-
;;; Helm TUI renderer for gemacs
;;;
;;; Renders the helm candidate list in the bottom N rows of the terminal.
;;; Runs a modal input loop that handles navigation, selection, and filtering.
;;; Features: follow mode, action menu, mark-all, match highlighting, auto-resize.

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

(def *helm-tui-min-height* 4)   ;; minimum rows (prompt + 3 candidates)
(def *helm-tui-max-height* 12)  ;; maximum rows for candidate list + prompt

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
;;; Auto-resize: compute helm height from candidate count
;;;============================================================================

(def (helm-auto-height candidate-count source-count)
  "Compute helm display height based on candidate and source count."
  (let ((needed (+ 2                ;; separator + prompt
                   candidate-count
                   source-count)))   ;; source header lines
    (max *helm-tui-min-height*
         (min *helm-tui-max-height* needed))))

;;;============================================================================
;;; Render the helm display
;;;============================================================================

(def (helm-tui-render! session width height helm-height)
  "Render the helm session in the bottom rows of the terminal."
  (let* ((candidates (helm-session-candidates session))
         (cand-count (vector-length candidates))
         (selected (helm-session-selected session))
         (scroll (helm-session-scroll-offset session))
         (pattern (helm-session-pattern session))
         (sources (helm-session-sources session))
         (follow? (helm-session-follow? session))
         ;; Layout: bottom row = prompt, rows above = candidates
         (list-height (- helm-height 1))
         (prompt-row (- height 1))
         (list-start-row (- height helm-height))
         ;; Colors
         (fg-normal #xd8d8d8)
         (bg-normal #x1e1e1e)
         (fg-selected #xffffff)
         (bg-selected #x3a3a5a)
         (fg-header #x88aaff)
         (bg-header #x252535)
         (fg-dim #x888888)
         (fg-prompt #xb0b0b0)
         (fg-match #xffcc00)         ;; yellow for match highlights
         (fg-marked #xff8888))       ;; red-ish for marked indicator

    ;; Draw separator line
    (when (>= list-start-row 0)
      (tui-print! 0 list-start-row fg-dim bg-normal (make-string width #\─)))

    ;; Compute match positions for highlighting
    (let ((match-pos-cache (make-hash-table)))

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
                   (is-marked (memv idx (helm-session-marked session)))
                   (use-fuzzy? (helm-source-fuzzy? src))
                   ;; Get match positions for this candidate
                   (positions (if (string=? pattern "")
                                []
                                (helm-match-positions pattern
                                  (helm-candidate-display cand)
                                  use-fuzzy?))))

              ;; Draw source header if source changed
              (if (not (eq? src last-source))
                (begin
                  ;; Source header with separator styling
                  (when (< row (- prompt-row 1))
                    (tui-print! 0 row fg-header bg-header
                                (make-string width #\space))
                    (let ((hdr (string-append "─── " src-name " ───")))
                      (tui-print! 1 row fg-header bg-header
                                  (if (> (string-length hdr) (- width 2))
                                    (substring hdr 0 (- width 2))
                                    hdr)))
                    ;; Draw candidate on next row
                    (let ((cand-row (+ row 1)))
                      (when (< cand-row prompt-row)
                        (helm-tui-draw-candidate! cand cand-row width
                                                  is-selected is-marked
                                                  fg-normal bg-normal
                                                  fg-selected bg-selected
                                                  fg-match fg-marked
                                                  positions)
                        (loop (+ cand-row 1) (+ idx 1) src)))))
                ;; Same source — just draw candidate
                (begin
                  (helm-tui-draw-candidate! cand row width
                                            is-selected is-marked
                                            fg-normal bg-normal
                                            fg-selected bg-selected
                                            fg-match fg-marked
                                            positions)
                  (loop (+ row 1) (+ idx 1) last-source))))))

        ;; Clear remaining rows
        (let clear-loop ((row (+ list-start-row 1
                                 (min list-height
                                      (+ (- visible-end visible-start)
                                         (count-source-transitions candidates
                                                                   visible-start visible-end))))))
          (when (< row prompt-row)
            (tui-print! 0 row fg-normal bg-normal (make-string width #\space))
            (clear-loop (+ row 1))))))

    ;; Draw prompt line
    (let* ((count-str (string-append "[" (number->string (if (> cand-count 0) (+ selected 1) 0))
                                     "/" (number->string cand-count) "]"))
           (follow-str (if follow? " [Follow]" ""))
           (prompt-text (string-append "Pattern: " pattern))
           (suffix (string-append follow-str " " count-str))
           (cursor-pos (string-length prompt-text)))
      (tui-print! 0 prompt-row fg-prompt bg-normal (make-string width #\space))
      (tui-print! 0 prompt-row fg-prompt bg-normal
                  (if (> (string-length prompt-text) width)
                    (substring prompt-text 0 width)
                    prompt-text))
      ;; Count and follow indicator in dim color
      (when (< cursor-pos (- width (string-length suffix)))
        (tui-print! (- width (string-length suffix)) prompt-row
                    fg-dim bg-normal suffix))
      ;; Position cursor
      (tui-set-cursor! (min cursor-pos (- width 1)) prompt-row))

    (tui-present!)))

(def (helm-tui-draw-candidate! cand row width is-selected is-marked
                                fg-normal bg-normal fg-selected bg-selected
                                fg-match fg-marked match-positions)
  "Draw a single candidate line with optional match highlighting."
  (let* ((fg (if is-selected fg-selected fg-normal))
         (bg (if is-selected bg-selected bg-normal))
         (prefix (cond (is-marked "* ")
                       (is-selected "> ")
                       (else "  ")))
         (prefix-len (string-length prefix))
         (text (helm-candidate-display cand))
         (line (string-append prefix text))
         (display-line (if (> (string-length line) width)
                         (substring line 0 width)
                         line)))
    ;; Draw base line (background + text)
    (tui-print! 0 row fg bg (make-string width #\space))
    (tui-print! 0 row fg bg display-line)

    ;; Draw marked indicator in accent color
    (when is-marked
      (tui-print! 0 row fg-marked bg "* "))

    ;; Overlay match highlights on matched characters
    (when (and (pair? match-positions) (not (string=? text "")))
      (let ((hl-fg (if is-selected #xffee88 fg-match)))  ;; brighter when selected
        (for-each
          (lambda (pos)
            (let ((col (+ prefix-len pos)))  ;; offset by prefix
              (when (and (>= pos 0) (< pos (string-length text)) (< col width))
                (tui-print! col row hl-fg bg (string (string-ref text pos))))))
          match-positions)))))

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
;;; Follow mode helper
;;;============================================================================

(def (helm-run-follow! session)
  "If follow mode is active and a persistent action exists, run it on current candidate."
  (when (helm-session-follow? session)
    (let* ((candidates (helm-session-candidates session))
           (cand-count (vector-length candidates))
           (selected (helm-session-selected session)))
      (when (> cand-count 0)
        (let* ((cand (vector-ref candidates selected))
               (src (helm-candidate-source cand))
               (pa (helm-source-persistent-action src)))
          (when pa
            (with-catch void (lambda () (pa (helm-candidate-real cand))))))))))

;;;============================================================================
;;; Action menu
;;;============================================================================

(def (helm-show-action-menu! session width height helm-height)
  "Show action menu for the current candidate's source. Returns chosen action or #f."
  (let* ((candidates (helm-session-candidates session))
         (cand-count (vector-length candidates))
         (selected (helm-session-selected session)))
    (if (= cand-count 0)
      #f
      (let* ((cand (vector-ref candidates selected))
             (src (helm-candidate-source cand))
             (actions (helm-source-actions src)))
        (if (or (not actions) (null? actions))
          #f
          ;; Draw action menu in the candidate area
          (let* ((prompt-row (- height 1))
                 (list-start-row (- height helm-height))
                 (fg-normal #xd8d8d8)
                 (bg-menu #x2a2a3a)
                 (fg-title #x88aaff)
                 (fg-dim #x888888))

            ;; Draw menu title
            (tui-print! 0 (+ list-start-row 1) fg-title bg-menu
                        (make-string width #\space))
            (tui-print! 1 (+ list-start-row 1) fg-title bg-menu
                        "Actions (number to select, C-g to cancel):")

            ;; Draw each action with a number
            (let draw-loop ((acts actions) (n 1) (row (+ list-start-row 2)))
              (when (and (pair? acts) (< row prompt-row))
                (tui-print! 0 row fg-normal bg-menu (make-string width #\space))
                (tui-print! 2 row fg-normal bg-menu
                            (string-append (number->string n) ". " (caar acts)))
                (draw-loop (cdr acts) (+ n 1) (+ row 1))))

            ;; Clear remaining rows
            (let clear-loop ((row (+ list-start-row 2 (length actions))))
              (when (< row prompt-row)
                (tui-print! 0 row fg-dim bg-menu (make-string width #\space))
                (clear-loop (+ row 1))))

            (tui-present!)

            ;; Wait for user input
            (let action-loop ()
              (let ((ev (tui-poll-event)))
                (cond
                  ((not ev) (action-loop))
                  ((not (tui-event-key? ev)) (action-loop))
                  (else
                   (let* ((key (tui-event-key ev))
                          (ch (tui-event-ch ev)))
                     (cond
                       ;; C-g → cancel
                       ((= key #x07) #f)
                       ;; Number key 1-9 → select action
                       ((and (>= ch (char->integer #\1))
                             (<= ch (char->integer #\9)))
                        (let ((idx (- ch (char->integer #\1))))
                          (if (< idx (length actions))
                            (cdr (list-ref actions idx))
                            (action-loop))))  ;; invalid number, retry
                       ;; Enter → first action (default)
                       ((= key #x0D)
                        (cdar actions))
                       (else (action-loop))))))))))))))

;;;============================================================================
;;; Input loop
;;;============================================================================

(def (helm-tui-run! session)
  "Run the helm TUI session. Returns the selected candidate's real value, or #f if cancelled."
  (let* ((width (tui-width))
         (height (tui-height))
         ;; Initialize follow mode from first source's follow? flag
         (_ (let ((sources (helm-session-sources session)))
              (when (and (pair? sources) (helm-source-follow? (car sources)))
                (set! (helm-session-follow? session) #t)))))

    ;; Compute initial auto-resize height
    (let ((helm-height (helm-auto-height
                         (vector-length (helm-session-candidates session))
                         (length (helm-session-sources session)))))

      ;; Initial render
      (helm-tui-render! session width height helm-height)

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

                 ;; C-c (0x03) → prefix key: wait for next key
                 ((= key #x03)
                  (let ((ev2 (tui-poll-event)))
                    (when (and ev2 (tui-event-key? ev2))
                      (let ((ch2 (tui-event-ch ev2)))
                        (cond
                          ;; C-c C-f → toggle follow mode
                          ((= ch2 #x06)
                           (set! (helm-session-follow? session)
                             (not (helm-session-follow? session))))))))
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; C-n / Down → next candidate
                 ((or (= key #x0e)  ;; C-n = 0x0e
                      (= key TB_KEY_ARROW_DOWN))
                  (when (> cand-count 0)
                    (set! (helm-session-selected session)
                      (modulo (+ selected 1) cand-count))
                    (helm-ensure-visible! session helm-height))
                  (helm-run-follow! session)
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; C-p / Up → previous candidate
                 ((or (= key #x10)  ;; C-p = 0x10
                      (= key TB_KEY_ARROW_UP))
                  (when (> cand-count 0)
                    (set! (helm-session-selected session)
                      (modulo (- selected 1) cand-count))
                    (helm-ensure-visible! session helm-height))
                  (helm-run-follow! session)
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; C-v / PageDown → page down
                 ((or (= key #x16)  ;; C-v
                      (= key TB_KEY_PGDN))
                  (when (> cand-count 0)
                    (let ((page (- helm-height 2)))
                      (set! (helm-session-selected session)
                        (min (- cand-count 1) (+ selected page)))
                      (helm-ensure-visible! session helm-height)))
                  (helm-run-follow! session)
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; M-v / PageUp → page up
                 ((or (and alt? (= ch (char->integer #\v)))
                      (= key TB_KEY_PGUP))
                  (when (> cand-count 0)
                    (let ((page (- helm-height 2)))
                      (set! (helm-session-selected session)
                        (max 0 (- selected page)))
                      (helm-ensure-visible! session helm-height)))
                  (helm-run-follow! session)
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; M-< → first candidate
                 ((and alt? (= ch (char->integer #\<)))
                  (set! (helm-session-selected session) 0)
                  (set! (helm-session-scroll-offset session) 0)
                  (helm-run-follow! session)
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; M-> → last candidate
                 ((and alt? (= ch (char->integer #\>)))
                  (when (> cand-count 0)
                    (set! (helm-session-selected session) (- cand-count 1))
                    (helm-ensure-visible! session helm-height))
                  (helm-run-follow! session)
                  (helm-tui-render! session width height helm-height)
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
                      (helm-ensure-visible! session helm-height)))
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; M-a → mark all visible candidates
                 ((and alt? (= ch (char->integer #\a)))
                  (when (> cand-count 0)
                    (set! (helm-session-marked session)
                      (let gen ((i 0) (acc []))
                        (if (>= i cand-count)
                          acc
                          (gen (+ i 1) (cons i acc))))))
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; Tab → action menu
                 ((= key #x09)
                  (if (> cand-count 0)
                    (let ((action (helm-show-action-menu! session width height helm-height)))
                      (if action
                        ;; Execute chosen action on selected (or marked) candidates
                        (let* ((marked (helm-session-marked session))
                               (targets (if (pair? marked)
                                          (map (lambda (i) (vector-ref candidates i)) marked)
                                          (list (vector-ref candidates selected)))))
                          (helm-session-store! session)
                          (helm-run-action action targets)
                          (helm-candidate-real (vector-ref candidates selected)))
                        ;; Cancelled action menu — go back to helm
                        (begin
                          (helm-tui-render! session width height helm-height)
                          (loop))))
                    (loop)))

                 ;; C-j → persistent action
                 ((= key #x0a)
                  (when (> cand-count 0)
                    (let* ((cand (vector-ref candidates selected))
                           (src (helm-candidate-source cand))
                           (pa (helm-source-persistent-action src)))
                      (when pa
                        (pa (helm-candidate-real cand)))))
                  (helm-tui-render! session width height helm-height)
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
                      (set! (helm-session-scroll-offset session) 0)
                      ;; Auto-resize
                      (set! helm-height
                        (helm-auto-height
                          (vector-length (helm-session-candidates session))
                          (length (helm-session-sources session))))))
                  (helm-tui-render! session width height helm-height)
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
                  ;; Auto-resize
                  (set! helm-height
                    (helm-auto-height
                      (vector-length (helm-session-candidates session))
                      (length (helm-session-sources session))))
                  (helm-tui-render! session width height helm-height)
                  (loop))

                 ;; Ignore other keys
                 (else (loop)))))))))))

;;;============================================================================
;;; Scroll management
;;;============================================================================

(def (helm-ensure-visible! session helm-height)
  "Ensure the selected candidate is visible in the scroll window."
  (let* ((selected (helm-session-selected session))
         (scroll (helm-session-scroll-offset session))
         (visible-height (- helm-height 2)))  ;; minus prompt and separator
    (cond
      ((< selected scroll)
       (set! (helm-session-scroll-offset session) selected))
      ((>= selected (+ scroll visible-height))
       (set! (helm-session-scroll-offset session)
         (- selected visible-height -1))))))
