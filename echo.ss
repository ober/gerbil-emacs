;;; -*- Gerbil -*-
;;; TUI echo area / minibuffer for gemacs
;;;
;;; The echo area occupies the last terminal row.
;;; It displays messages and handles simple line input for prompts.
;;;
;;; Echo state and message functions are in core.ss.

(export
  (struct-out echo-state)
  make-initial-echo-state
  echo-message!
  echo-error!
  echo-clear!
  echo-draw!
  echo-read-string
  echo-read-string-with-completion
  echo-read-file-with-completion
  *minibuffer-history*
  minibuffer-history-add!)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :gerbil-scintilla/tui
        :gemacs/core)

;;;============================================================================
;;; Minibuffer history
;;;============================================================================

;; Global minibuffer history (most recent first)
(def *minibuffer-history* [])
(def *max-history-size* 100)

(def (minibuffer-history-add! input)
  "Add an input string to the minibuffer history.
   Avoids duplicates at the front and limits size."
  (when (and (string? input) (> (string-length input) 0))
    ;; Remove duplicate if it's already at the front
    (when (and (pair? *minibuffer-history*)
               (string=? (car *minibuffer-history*) input))
      (set! *minibuffer-history* (cdr *minibuffer-history*)))
    ;; Add to front
    (set! *minibuffer-history* (cons input *minibuffer-history*))
    ;; Trim to max size
    (when (> (length *minibuffer-history*) *max-history-size*)
      (set! *minibuffer-history*
        (let loop ((lst *minibuffer-history*) (n 0) (acc []))
          (if (or (null? lst) (>= n *max-history-size*))
            (reverse acc)
            (loop (cdr lst) (+ n 1) (cons (car lst) acc))))))))

;;;============================================================================
;;; Draw the echo area (TUI-specific)
;;;============================================================================

(def (echo-draw! echo row width)
  "Draw the echo area at the given row."
  ;; Clear the row (dark background matching editor theme)
  (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
  ;; Draw message if any
  (let ((msg (echo-state-message echo)))
    (when msg
      (let ((fg (if (echo-state-error? echo)
                  #xff4040    ; bright red for errors
                  #xd8d8d8)) ; light gray for normal messages
            (display-msg (if (> (string-length msg) width)
                           (substring msg 0 width)
                           msg)))
        (tui-print! 0 row fg #x181818 display-msg)))))

;;;============================================================================
;;; Read a string from the user in the echo area (TUI-specific)
;;; Runs a blocking sub-event-loop.
;;; Returns the input string, or #f if cancelled (C-g).
;;;============================================================================

(def (echo-read-string echo prompt row width)
  (echo-clear! echo)
  (let loop ((input "") (hist-idx -1) (saved-input ""))
    ;; Draw prompt + input with history indicator
    (let* ((hist-suffix (if (>= hist-idx 0)
                          (string-append " [" (number->string (+ hist-idx 1))
                                         "/" (number->string (length *minibuffer-history*)) "]")
                          ""))
           (display-str (string-append prompt input))
           (cursor-pos (string-length display-str))
           (display-len (string-length display-str)))
      (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
      (tui-print! 0 row #xd8d8d8 #x181818
                  (if (> display-len width)
                    (substring display-str 0 width)
                    display-str))
      ;; Show history indicator in dim color
      (when (and (>= hist-idx 0) (< cursor-pos width))
        (let ((avail (- width cursor-pos)))
          (tui-print! cursor-pos row #x888888 #x181818
                      (if (> (string-length hist-suffix) avail)
                        (substring hist-suffix 0 avail)
                        hist-suffix))))
      (tui-set-cursor! (min display-len (- width 1)) row)
      (tui-present!))
    ;; Wait for key
    (let ((ev (tui-poll-event)))
      (cond
        ((not ev) (loop input hist-idx saved-input))
        ((not (tui-event-key? ev)) (loop input hist-idx saved-input))
        (else
         (let* ((key (tui-event-key ev))
                (ch  (tui-event-ch ev))
                (mod (tui-event-mod ev))
                (alt? (not (zero? (bitwise-and mod TB_MOD_ALT)))))
           (cond
             ;; C-g (0x07) -> cancel
             ((= key #x07)
              (echo-message! echo "Quit")
              #f)
             ;; Enter (0x0D) -> accept and add to history
             ((= key #x0D)
              (minibuffer-history-add! input)
              input)
             ;; M-p -> previous history entry
             ((and alt? (= ch (char->integer #\p)))
              (let ((hist-len (length *minibuffer-history*)))
                (if (> hist-len 0)
                  (let* ((new-idx (min (+ hist-idx 1) (- hist-len 1)))
                         ;; Save current input when first entering history
                         (saved (if (= hist-idx -1) input saved-input))
                         (entry (list-ref *minibuffer-history* new-idx)))
                    (loop entry new-idx saved))
                  (loop input hist-idx saved-input))))
             ;; M-n -> next history entry (or back to saved input)
             ((and alt? (= ch (char->integer #\n)))
              (cond
                ((> hist-idx 0)
                 (let ((entry (list-ref *minibuffer-history* (- hist-idx 1))))
                   (loop entry (- hist-idx 1) saved-input)))
                ((= hist-idx 0)
                 ;; Return to saved (pre-history) input
                 (loop saved-input -1 saved-input))
                (else
                 (loop input hist-idx saved-input))))
             ;; Backspace (0x08 or 0x7F) -> delete last char
             ((or (= key #x08) (= key #x7F))
              (if (> (string-length input) 0)
                (loop (substring input 0 (- (string-length input) 1)) -1 "")
                (loop input hist-idx saved-input)))
             ;; Printable char -> append (exits history browsing)
             ((> ch 31)
              (loop (string-append input (string (integer->char ch))) -1 ""))
             ;; Ignore other keys
             (else (loop input hist-idx saved-input)))))))))

;;;============================================================================
;;; Read a string with tab-completion (TUI-specific)
;;; completions: sorted list of strings to complete against
;;; Uses fuzzy matching (characters in order, scored by quality).
;;; Returns the input string, or #f if cancelled (C-g).
;;;============================================================================

(def (echo-read-string-with-completion echo prompt completions row width)
  (echo-clear! echo)
  ;; search-pat: when cycling via Tab, holds the original search text
  ;; so the match list stays stable across Tab presses.
  (let loop ((input "") (match-idx 0) (search-pat #f))
    ;; Use search-pattern for matching during cycling, otherwise use input
    (let* ((pattern (or search-pat input))
           (matches (if (string=? pattern "")
                      completions
                      (fuzzy-filter-sort pattern completions)))
           (match-count (length matches))
           (suffix (cond
                     ((string=? input "") "")
                     ((> match-count 0)
                      (string-append " [" (number->string (min (+ match-idx 1) match-count))
                                     "/" (number->string match-count) "]"))
                     (else " [No match]")))
           (cursor-pos (+ (string-length prompt) (string-length input))))
      ;; Draw prompt + input
      (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
      (tui-print! 0 row #xd8d8d8 #x181818
                  (if (> cursor-pos width)
                    (substring (string-append prompt input) 0 width)
                    (string-append prompt input)))
      ;; Show suffix in a dimmer color
      (when (< cursor-pos width)
        (let ((avail (- width cursor-pos)))
          (tui-print! cursor-pos row #x888888 #x181818
                      (if (> (string-length suffix) avail)
                        (substring suffix 0 avail)
                        suffix))))
      (tui-set-cursor! (min cursor-pos (- width 1)) row)
      (tui-present!))
    ;; Wait for key
    (let ((ev (tui-poll-event)))
      (cond
        ((not ev) (loop input match-idx search-pat))
        ((not (tui-event-key? ev)) (loop input match-idx search-pat))
        (else
         (let* ((key (tui-event-key ev))
                (ch  (tui-event-ch ev))
                ;; Recompute matches for key handling
                (pattern (or search-pat input))
                (matches (if (string=? pattern "")
                           completions
                           (fuzzy-filter-sort pattern completions)))
                (match-count (length matches)))
           (cond
             ;; C-g -> cancel
             ((= key #x07)
              (echo-message! echo "Quit")
              #f)
             ;; Enter -> accept
             ((= key #x0D) input)
             ;; Tab -> cycle to next fuzzy completion
             ((= key #x09)
              (if (> match-count 0)
                (let* ((idx (modulo match-idx match-count))
                       (completed (list-ref matches idx)))
                  ;; Save search-pattern on first Tab press
                  (loop completed (+ idx 1) (or search-pat input)))
                (loop input 0 search-pat)))
             ;; Backspace -> delete last char, reset cycling
             ((or (= key #x08) (= key #x7F))
              (if (> (string-length input) 0)
                (loop (substring input 0 (- (string-length input) 1)) 0 #f)
                (loop input 0 search-pat)))
             ;; Printable char -> append, reset cycling
             ((> ch 31)
              (loop (string-append input (string (integer->char ch))) 0 #f))
             ;; Ignore other keys
             (else (loop input match-idx search-pat)))))))))

;;;============================================================================
;;; Read a file path with directory-aware fuzzy completion (TUI-specific)
;;; Supports ~ expansion, directory traversal, and Tab cycling.
;;; Returns the file path string, or #f if cancelled (C-g).
;;;============================================================================

(def (echo-read-file-with-completion echo prompt row width)
  (echo-clear! echo)
  ;; Helper: expand tilde at start of path
  (def (expand-tilde path)
    (if (and (> (string-length path) 0)
             (char=? (string-ref path 0) #\~))
      (let ((home (or (getenv "HOME") "/")))
        (if (= (string-length path) 1)
          (string-append home "/")
          (if (char=? (string-ref path 1) #\/)
            (string-append home (substring path 1 (string-length path)))
            path)))
      path))
  ;; Helper: find last slash position (or #f)
  (def (last-slash-pos str)
    (let loop ((i (- (string-length str) 1)))
      (cond ((< i 0) #f)
            ((char=? (string-ref str i) #\/) i)
            (else (loop (- i 1))))))
  ;; Helper: list directory files safely, sorted
  (def (list-dir dir)
    (with-catch (lambda (e) [])
      (lambda ()
        (sort (directory-files dir) string<?))))
  ;; Helper: parse input into (values dir partial display-prefix)
  (def (parse-input text)
    (let* ((text (if (string=? text "~") "~/" text))
           (expanded (expand-tilde text))
           (slash (last-slash-pos expanded)))
      (if slash
        (let* ((dir (substring expanded 0 (+ slash 1)))
               (partial (substring expanded (+ slash 1) (string-length expanded)))
               (orig-slash (last-slash-pos text))
               (display-prefix (if orig-slash
                                 (substring text 0 (+ orig-slash 1))
                                 "")))
          (values dir partial display-prefix))
        (values (current-directory) expanded ""))))

  (let loop ((input "") (match-idx 0) (search-pat #f)
             (hist-idx -1) (saved-input ""))
    ;; Compute matches for display
    (let-values (((dir partial display-prefix) (parse-input (or search-pat input))))
      (let* ((files (list-dir dir))
             (matches (if (string=? partial "")
                        files
                        (fuzzy-filter-sort partial files)))
             (match-count (length matches))
             (suffix (cond
                       ((string=? input "") "")
                       ((> match-count 0)
                        (string-append " [" (number->string (min (+ match-idx 1) match-count))
                                       "/" (number->string match-count) "]"))
                       (else " [No match]")))
             (cursor-pos (+ (string-length prompt) (string-length input))))
        ;; Draw prompt + input
        (tui-print! 0 row #xd8d8d8 #x181818 (make-string width #\space))
        (tui-print! 0 row #xd8d8d8 #x181818
                    (if (> cursor-pos width)
                      (substring (string-append prompt input) 0 width)
                      (string-append prompt input)))
        (when (< cursor-pos width)
          (let ((avail (- width cursor-pos)))
            (tui-print! cursor-pos row #x888888 #x181818
                        (if (> (string-length suffix) avail)
                          (substring suffix 0 avail)
                          suffix))))
        (tui-set-cursor! (min cursor-pos (- width 1)) row)
        (tui-present!)))
    ;; Wait for key
    (let ((ev (tui-poll-event)))
      (cond
        ((not ev) (loop input match-idx search-pat hist-idx saved-input))
        ((not (tui-event-key? ev)) (loop input match-idx search-pat hist-idx saved-input))
        (else
         (let* ((key (tui-event-key ev))
                (ch  (tui-event-ch ev))
                (mod (tui-event-mod ev))
                (alt? (not (zero? (bitwise-and mod TB_MOD_ALT)))))
           ;; Recompute matches for key handling
           (let-values (((dir partial display-prefix) (parse-input (or search-pat input))))
             (let* ((files (list-dir dir))
                    (matches (if (string=? partial "")
                               files
                               (fuzzy-filter-sort partial files)))
                    (match-count (length matches)))
               (cond
                 ;; C-g -> cancel
                 ((= key #x07)
                  (echo-message! echo "Quit")
                  #f)
                 ;; Enter -> accept, add to history
                 ((= key #x0D)
                  (minibuffer-history-add! input)
                  input)
                 ;; Tab -> fuzzy complete/cycle
                 ((= key #x09)
                  (if (> match-count 0)
                    (let* ((idx (modulo match-idx match-count))
                           (match-name (list-ref matches idx))
                           (full-path (string-append display-prefix match-name))
                           ;; Check if match is a directory for auto-append /
                           (expanded-full (expand-tilde full-path))
                           (is-dir? (with-catch (lambda (e) #f)
                                      (lambda ()
                                        (and (file-exists? expanded-full)
                                             (eq? 'directory
                                                  (file-info-type
                                                   (file-info expanded-full))))))))
                      (if is-dir?
                        ;; Directory: append / and reset search for next Tab
                        (let ((dir-path (if (string-suffix? "/" full-path)
                                          full-path
                                          (string-append full-path "/"))))
                          (loop dir-path 0 #f -1 ""))
                        ;; File: save search-pattern for cycling
                        (loop full-path (+ idx 1) (or search-pat input) -1 "")))
                    (loop input 0 search-pat hist-idx saved-input)))
                 ;; M-p -> previous history
                 ((and alt? (= ch (char->integer #\p)))
                  (let ((hist-len (length *minibuffer-history*)))
                    (if (> hist-len 0)
                      (let* ((new-idx (min (+ hist-idx 1) (- hist-len 1)))
                             (saved (if (= hist-idx -1) input saved-input))
                             (entry (list-ref *minibuffer-history* new-idx)))
                        (loop entry 0 #f new-idx saved))
                      (loop input match-idx search-pat hist-idx saved-input))))
                 ;; M-n -> next history
                 ((and alt? (= ch (char->integer #\n)))
                  (cond
                    ((> hist-idx 0)
                     (loop (list-ref *minibuffer-history* (- hist-idx 1))
                           0 #f (- hist-idx 1) saved-input))
                    ((= hist-idx 0)
                     (loop saved-input 0 #f -1 saved-input))
                    (else
                     (loop input match-idx search-pat hist-idx saved-input))))
                 ;; Backspace -> delete last char, reset cycling
                 ((or (= key #x08) (= key #x7F))
                  (if (> (string-length input) 0)
                    (loop (substring input 0 (- (string-length input) 1)) 0 #f -1 "")
                    (loop input match-idx search-pat hist-idx saved-input)))
                 ;; Printable char -> reset cycling
                 ((> ch 31)
                  (loop (string-append input (string (integer->char ch))) 0 #f -1 ""))
                 ;; Ignore other keys
                 (else (loop input match-idx search-pat hist-idx saved-input)))))))))))
