;;; -*- Gerbil -*-
;;; Qt commands edit - editing, transpose, case, comment, and text manipulation
;;; Part of the qt/commands-*.ss module chain.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/text/base64
        :gemacs/qt/sci-shim
        :gemacs/core
        :gemacs/async
        :gemacs/subprocess
        :gemacs/gsh-subprocess
        :gemacs/editor
        :gemacs/repl
        :gemacs/eshell
        :gemacs/gsh-eshell
        :gemacs/shell
        :gemacs/shell-history
        :gemacs/terminal
        :gemacs/chat
        :gemacs/qt/buffer
        :gemacs/qt/window
        :gemacs/qt/echo
        :gemacs/qt/highlight
        :gemacs/qt/modeline
        :gemacs/qt/image
        :gemacs/qt/commands-core)

;;;============================================================================
;;; Incremental Search (isearch)
;;;============================================================================

;; Isearch state
(def *isearch-active* #f)      ; #f, 'forward, or 'backward
(def *isearch-query* "")       ; current search string
(def *isearch-start-pos* 0)    ; cursor position when isearch started
(def *isearch-app* #f)         ; app-state reference

;; Search highlight colors for current match (bright cyan background)
(def isearch-cur-fg-r #x00) (def isearch-cur-fg-g #x00) (def isearch-cur-fg-b #x00)
(def isearch-cur-bg-r #x00) (def isearch-cur-bg-g #xdd) (def isearch-cur-bg-b #xff)

;; Search highlight colors for other matches (dim yellow background)
(def isearch-oth-fg-r #x00) (def isearch-oth-fg-g #x00) (def isearch-oth-fg-b #x00)
(def isearch-oth-bg-r #xff) (def isearch-oth-bg-g #xcc) (def isearch-oth-bg-b #x00)

(def (isearch-highlight-all! ed query cursor-pos)
  "Highlight all matches. The match at/nearest cursor-pos gets current-match color."
  (when (> (string-length query) 0)
    (let* ((text (qt-plain-text-edit-text ed))
           (len (string-length text))
           (pat-len (string-length query))
           (query-lower (string-downcase query))
           (text-lower (string-downcase text)))
      ;; Find all match positions
      (let loop ((i 0) (positions '()))
        (if (> (+ i pat-len) len)
          ;; Done collecting — now highlight
          (let ((positions (reverse positions)))
            (for-each
              (lambda (pos)
                (if (= pos cursor-pos)
                  ;; Current match — bright cyan
                  (qt-extra-selection-add-range! ed pos pat-len
                    isearch-cur-fg-r isearch-cur-fg-g isearch-cur-fg-b
                    isearch-cur-bg-r isearch-cur-bg-g isearch-cur-bg-b bold: #t)
                  ;; Other matches — dim yellow
                  (qt-extra-selection-add-range! ed pos pat-len
                    isearch-oth-fg-r isearch-oth-fg-g isearch-oth-fg-b
                    isearch-oth-bg-r isearch-oth-bg-g isearch-oth-bg-b bold: #f)))
              positions)
            (qt-extra-selections-apply! ed)
            (length positions))
          ;; Search for next occurrence (case-insensitive)
          (let ((found (string-contains text-lower query-lower i)))
            (if found
              (loop (+ found 1) (cons found positions))
              ;; No more matches
              (loop len positions))))))))

(def (isearch-find-nearest-forward text query from-pos)
  "Find first match at or after from-pos (case-insensitive). Returns position or #f."
  (let* ((text-lower (string-downcase text))
         (query-lower (string-downcase query)))
    (string-contains text-lower query-lower from-pos)))

(def (isearch-find-nearest-backward text query from-pos)
  "Find last match before from-pos (case-insensitive). Returns position or #f."
  (let* ((text-lower (string-downcase text))
         (query-lower (string-downcase query))
         (pat-len (string-length query)))
    ;; Scan backward from from-pos
    (let loop ((i (min from-pos (- (string-length text) pat-len))))
      (cond
        ((< i 0) #f)
        ((string-contains text-lower query-lower i)
         => (lambda (pos) (if (<= pos from-pos) pos (loop (- i 1)))))
        (else (loop (- i 1)))))))

(def (isearch-update! app)
  "Update search display: find match, highlight all, update echo."
  (let* ((ed (current-qt-editor app))
         (echo (app-state-echo app))
         (query *isearch-query*)
         (direction *isearch-active*)
         (prefix (if (eq? direction 'forward) "I-search: " "I-search backward: ")))
    ;; Update visual decorations first (current line, braces)
    (qt-update-visual-decorations! ed)
    (if (= (string-length query) 0)
      ;; Empty query — just show prompt
      (echo-message! echo prefix)
      ;; Non-empty query — find and highlight
      (let* ((text (qt-plain-text-edit-text ed))
             (cur-pos (qt-plain-text-edit-cursor-position ed))
             (match-pos
               (if (eq? direction 'forward)
                 (or (isearch-find-nearest-forward text query cur-pos)
                     ;; Wrap around
                     (isearch-find-nearest-forward text query 0))
                 (or (isearch-find-nearest-backward text query (- cur-pos 1))
                     ;; Wrap around
                     (isearch-find-nearest-backward text query
                       (- (string-length text) 1))))))
        (if match-pos
          (begin
            ;; Move cursor to end of match (forward) or start of match (backward)
            (if (eq? direction 'forward)
              (qt-plain-text-edit-set-cursor-position! ed (+ match-pos (string-length query)))
              (qt-plain-text-edit-set-cursor-position! ed match-pos))
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            ;; Highlight all matches with current match distinguished
            (let ((count (isearch-highlight-all! ed query match-pos)))
              (let ((wrapped (if (eq? direction 'forward)
                               (and (< match-pos *isearch-start-pos*) " [Wrapped]")
                               (and match-pos (> match-pos *isearch-start-pos*) " [Wrapped]"))))
                (echo-message! echo
                  (string-append prefix query
                    (if wrapped wrapped "")
                    " [" (number->string count) " matches]")))))
          ;; Not found
          (begin
            (isearch-highlight-all! ed query -1)  ;; highlight remaining matches in yellow
            (echo-error! echo (string-append "Failing " prefix query))))))))

(def (isearch-next! app direction)
  "Move to next/previous match."
  (set! *isearch-active* direction)
  (let* ((ed (current-qt-editor app))
         (query *isearch-query*)
         (text (qt-plain-text-edit-text ed))
         (cur-pos (qt-plain-text-edit-cursor-position ed)))
    (when (> (string-length query) 0)
      (let ((match-pos
              (if (eq? direction 'forward)
                (or (isearch-find-nearest-forward text query cur-pos)
                    (isearch-find-nearest-forward text query 0))
                (or (isearch-find-nearest-backward text query
                      (- cur-pos (string-length query) 1))
                    (isearch-find-nearest-backward text query
                      (- (string-length text) 1))))))
        (when match-pos
          (if (eq? direction 'forward)
            (qt-plain-text-edit-set-cursor-position! ed (+ match-pos (string-length query)))
            (qt-plain-text-edit-set-cursor-position! ed match-pos))
          (qt-plain-text-edit-ensure-cursor-visible! ed)
          ;; Rehighlight with new current match
          (qt-update-visual-decorations! ed)
          (isearch-highlight-all! ed query match-pos)
          (echo-message! (app-state-echo app)
            (string-append (if (eq? direction 'forward) "I-search: " "I-search backward: ")
                           query)))))))

(def (isearch-exit! app cancel?)
  "Exit isearch mode. If cancel?, restore original cursor position."
  (let ((ed (current-qt-editor app))
        (echo (app-state-echo app)))
    (when cancel?
      (qt-plain-text-edit-set-cursor-position! ed *isearch-start-pos*)
      (qt-plain-text-edit-ensure-cursor-visible! ed))
    ;; Save the query for future searches
    (when (> (string-length *isearch-query*) 0)
      (set! (app-state-last-search app) *isearch-query*))
    ;; Clear state
    (set! *isearch-active* #f)
    (set! *isearch-app* #f)
    ;; Restore visual decorations (clears search highlights)
    (qt-update-visual-decorations! ed)
    (if cancel?
      (echo-message! echo "Quit")
      (echo-clear! echo))))

(def (isearch-handle-key! app code mods text)
  "Handle a key event during isearch mode. Returns #t if handled."
  (cond
    ;; Bare modifier keys (Shift, Ctrl, Alt, Meta, etc.) — ignore
    ;; Qt key codes 0x01000020-0x01000026 are modifier-only events.
    ;; Without this, pressing Ctrl (before S) would exit isearch.
    ((and (>= code #x01000020) (<= code #x01000026))
     #t)
    ;; C-s: search forward / next match
    ((and (= code QT_KEY_S) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (if (= (string-length *isearch-query*) 0)
       ;; Empty query + C-s: use last search
       (let ((last (app-state-last-search app)))
         (when (and last (> (string-length last) 0))
           (set! *isearch-query* last)))
       (void))
     (isearch-next! app 'forward)
     #t)
    ;; C-r: search backward / prev match
    ((and (= code QT_KEY_R) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (if (= (string-length *isearch-query*) 0)
       (let ((last (app-state-last-search app)))
         (when (and last (> (string-length last) 0))
           (set! *isearch-query* last)))
       (void))
     (isearch-next! app 'backward)
     #t)
    ;; C-g: cancel isearch, restore position
    ((and (= code QT_KEY_G) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (isearch-exit! app #t)
     #t)
    ;; Backspace: remove last character from query
    ((= code QT_KEY_BACKSPACE)
     (if (> (string-length *isearch-query*) 0)
       (begin
         (set! *isearch-query*
           (substring *isearch-query* 0 (- (string-length *isearch-query*) 1)))
         ;; Re-search from start position
         (qt-plain-text-edit-set-cursor-position! (current-qt-editor app) *isearch-start-pos*)
         (isearch-update! app))
       ;; Empty query + backspace: exit isearch
       (isearch-exit! app #t))
     #t)
    ;; Enter/Return/Escape: exit isearch, keep position
    ((or (= code QT_KEY_RETURN) (= code QT_KEY_ENTER) (= code QT_KEY_ESCAPE))
     (isearch-exit! app #f)
     #t)
    ;; C-w: yank word at cursor into search query
    ((and (= code QT_KEY_W) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
     (let* ((ed (current-qt-editor app))
            (pos (qt-plain-text-edit-cursor-position ed))
            (text-content (qt-plain-text-edit-text ed))
            (len (string-length text-content)))
       ;; Grab word from cursor position
       (let loop ((end pos))
         (if (and (< end len)
                  (let ((ch (string-ref text-content end)))
                    (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_) (char=? ch #\-))))
           (loop (+ end 1))
           (when (> end pos)
             (set! *isearch-query*
               (string-append *isearch-query* (substring text-content pos end)))
             (isearch-update! app)))))
     #t)
    ;; Regular printable character: add to search query
    ((and text (> (string-length text) 0) (zero? (bitwise-and mods QT_MOD_CTRL)))
     (set! *isearch-query* (string-append *isearch-query* text))
     (isearch-update! app)
     #t)
    ;; Any other key: exit isearch and let normal handling proceed
    (else
     (isearch-exit! app #f)
     #f)))

(def (cmd-search-forward app)
  "Enter incremental search forward mode."
  (set! *isearch-active* 'forward)
  (set! *isearch-query* "")
  (set! *isearch-start-pos* (qt-plain-text-edit-cursor-position (current-qt-editor app)))
  (set! *isearch-app* app)
  (echo-message! (app-state-echo app) "I-search: "))

(def (cmd-search-backward app)
  "Enter incremental search backward mode."
  (set! *isearch-active* 'backward)
  (set! *isearch-query* "")
  (set! *isearch-start-pos* (qt-plain-text-edit-cursor-position (current-qt-editor app)))
  (set! *isearch-app* app)
  (echo-message! (app-state-echo app) "I-search backward: "))
;;;============================================================================
;;; Comment toggle (Scheme: ;; prefix)
;;;============================================================================

(def (qt-replace-line! ed line-num new-line-text)
  "Replace a line by index in a Qt editor. Reconstructs the full text."
  (let* ((text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (new-lines (let loop ((ls lines) (i 0) (acc []))
                      (if (null? ls)
                        (reverse acc)
                        (if (= i line-num)
                          (loop (cdr ls) (+ i 1) (cons new-line-text acc))
                          (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
         (new-text (string-join new-lines "\n"))
         (pos (qt-plain-text-edit-cursor-position ed)))
    (qt-plain-text-edit-set-text! ed new-text)
    (qt-plain-text-edit-set-cursor-position! ed (min pos (string-length new-text)))))

(def (cmd-toggle-comment app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (line (qt-plain-text-edit-cursor-line ed))
         (lines (string-split text #\newline))
         (line-text (if (< line (length lines))
                      (list-ref lines line)
                      ""))
         (trimmed (string-trim line-text)))
    (cond
      ((and (>= (string-length trimmed) 3)
            (string=? (substring trimmed 0 3) ";; "))
       (let ((new-line (let ((cp (string-contains line-text ";; ")))
                         (if cp
                           (string-append (substring line-text 0 cp)
                                          (substring line-text (+ cp 3)
                                                     (string-length line-text)))
                           line-text))))
         (qt-replace-line! ed line new-line)))
      ((and (>= (string-length trimmed) 2)
            (string=? (substring trimmed 0 2) ";;"))
       (let ((new-line (let ((cp (string-contains line-text ";;")))
                         (if cp
                           (string-append (substring line-text 0 cp)
                                          (substring line-text (+ cp 2)
                                                     (string-length line-text)))
                           line-text))))
         (qt-replace-line! ed line new-line)))
      (else
       (qt-replace-line! ed line (string-append ";; " line-text))))))

;;;============================================================================
;;; Transpose chars
;;;============================================================================

(def (cmd-transpose-chars app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (when (>= pos 2)
      (let* ((c1 (string-ref text (- pos 2)))
             (c2 (string-ref text (- pos 1)))
             (new-text (string-append
                         (substring text 0 (- pos 2))
                         (string c2 c1)
                         (substring text pos (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)))))

;;;============================================================================
;;; Word case commands
;;;============================================================================

(def (qt-word-at-point ed)
  "Get word boundaries at cursor in Qt editor."
  (let* ((pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (let skip ((i pos))
      (if (>= i len)
        (values #f #f)
        (let ((ch (string-ref text i)))
          (if (or (char-alphabetic? ch) (char-numeric? ch)
                  (char=? ch #\_) (char=? ch #\-))
            (let find-end ((j (+ i 1)))
              (if (>= j len)
                (values i j)
                (let ((c (string-ref text j)))
                  (if (or (char-alphabetic? c) (char-numeric? c)
                          (char=? c #\_) (char=? c #\-))
                    (find-end (+ j 1))
                    (values i j)))))
            (skip (+ i 1))))))))

(def (cmd-upcase-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when start
        (let* ((text (qt-plain-text-edit-text ed))
               (word (substring text start end))
               (new-text (string-append
                           (substring text 0 start)
                           (string-upcase word)
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed end))))))

(def (cmd-downcase-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when start
        (let* ((text (qt-plain-text-edit-text ed))
               (word (substring text start end))
               (new-text (string-append
                           (substring text 0 start)
                           (string-downcase word)
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed end))))))

(def (cmd-capitalize-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when (and start (< start end))
        (let* ((text (qt-plain-text-edit-text ed))
               (word (substring text start end))
               (cap (string-append
                      (string-upcase (substring word 0 1))
                      (string-downcase (substring word 1 (string-length word)))))
               (new-text (string-append
                           (substring text 0 start)
                           cap
                           (substring text end (string-length text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed end))))))

;;;============================================================================
;;; Kill word
;;;============================================================================

(def (cmd-kill-word app)
  (let ((ed (current-qt-editor app)))
    (let-values (((start end) (qt-word-at-point ed)))
      (when start
        (let* ((pos (qt-plain-text-edit-cursor-position ed))
               (kill-start (min pos start))
               (text (qt-plain-text-edit-text ed))
               (killed (substring text kill-start end))
               (new-text (string-append
                           (substring text 0 kill-start)
                           (substring text end (string-length text)))))
          (set! (app-state-kill-ring app)
                (cons killed (app-state-kill-ring app)))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed kill-start))))))
;;;============================================================================
;;; Query replace
;;;============================================================================

(def (string-replace-all str from to)
  "Replace all occurrences of 'from' with 'to' in 'str'."
  (let ((from-len (string-length from))
        (to-len (string-length to))
        (str-len (string-length str)))
    (if (= from-len 0) str
      (let ((out (open-output-string)))
        (let loop ((i 0))
          (if (> (+ i from-len) str-len)
            (begin (display (substring str i str-len) out)
                   (get-output-string out))
            (if (string=? (substring str i (+ i from-len)) from)
              (begin (display to out)
                     (loop (+ i from-len)))
              (begin (write-char (string-ref str i) out)
                     (loop (+ i 1))))))))))

;;; Interactive query-replace state
(def *qreplace-active* #f)    ; #f or #t
(def *qreplace-from* "")      ; search string
(def *qreplace-to* "")        ; replacement string
(def *qreplace-pos* 0)        ; current search position in text
(def *qreplace-count* 0)      ; number of replacements made
(def *qreplace-app* #f)       ; app-state reference

;; Query-replace highlight colors (red background for current match)
(def qr-cur-fg-r #xff) (def qr-cur-fg-g #xff) (def qr-cur-fg-b #xff)
(def qr-cur-bg-r #xcc) (def qr-cur-bg-g #x33) (def qr-cur-bg-b #x33)

(def (qreplace-find-next! app)
  "Find the next match from current position. Returns match position or #f."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (text-lower (string-downcase text))
         (from-lower (string-downcase *qreplace-from*)))
    (string-contains text-lower from-lower *qreplace-pos*)))

(def (qreplace-highlight-current! app match-pos)
  "Highlight the current match being queried."
  (let ((ed (current-qt-editor app))
        (pat-len (string-length *qreplace-from*)))
    ;; Restore visual decorations first
    (qt-update-visual-decorations! ed)
    ;; Highlight current match
    (qt-extra-selection-add-range! ed match-pos pat-len
      qr-cur-fg-r qr-cur-fg-g qr-cur-fg-b
      qr-cur-bg-r qr-cur-bg-g qr-cur-bg-b bold: #t)
    (qt-extra-selections-apply! ed)
    ;; Move cursor to match and ensure visible
    (qt-plain-text-edit-set-cursor-position! ed match-pos)
    (qt-plain-text-edit-ensure-cursor-visible! ed)))

(def (qreplace-show-next! app)
  "Find and display the next match, or finish if none."
  (let ((match-pos (qreplace-find-next! app)))
    (if match-pos
      (begin
        (qreplace-highlight-current! app match-pos)
        (echo-message! (app-state-echo app)
          (string-append "Replace \"" *qreplace-from* "\" with \""
                         *qreplace-to* "\"? (y/n/!/q) ["
                         (number->string *qreplace-count*) " done]")))
      ;; No more matches
      (qreplace-finish! app))))

(def (qreplace-do-replace! app match-pos)
  "Replace the match at match-pos and advance."
  (let* ((ed (current-qt-editor app))
         (pat-len (string-length *qreplace-from*))
         (repl-len (string-length *qreplace-to*)))
    ;; Select the match text and replace it
    (qt-plain-text-edit-set-selection! ed match-pos (+ match-pos pat-len))
    (qt-plain-text-edit-remove-selected-text! ed)
    (qt-plain-text-edit-set-cursor-position! ed match-pos)
    (qt-plain-text-edit-insert-text! ed *qreplace-to*)
    ;; Advance past replacement
    (set! *qreplace-pos* (+ match-pos repl-len))
    (set! *qreplace-count* (+ *qreplace-count* 1))))

(def (qreplace-replace-all! app)
  "Replace all remaining matches."
  (let loop ()
    (let ((match-pos (qreplace-find-next! app)))
      (when match-pos
        (qreplace-do-replace! app match-pos)
        (loop))))
  (qreplace-finish! app))

(def (qreplace-finish! app)
  "End query-replace mode."
  (set! *qreplace-active* #f)
  (set! *qreplace-app* #f)
  ;; Restore visual decorations
  (qt-update-visual-decorations! (current-qt-editor app))
  (echo-message! (app-state-echo app)
    (string-append "Replaced " (number->string *qreplace-count*) " occurrence"
                   (if (= *qreplace-count* 1) "" "s"))))

(def (qreplace-handle-key! app code mods text)
  "Handle a key event during query-replace mode. Returns #t if handled."
  (let ((match-pos (qreplace-find-next! app)))
    (cond
      ;; No current match — should have been caught, but handle gracefully
      ((not match-pos)
       (qreplace-finish! app)
       #t)
      ;; y or space: replace this match, move to next
      ((and text (or (string=? text "y") (string=? text " ")))
       (qreplace-do-replace! app match-pos)
       (qreplace-show-next! app)
       #t)
      ;; n or Delete: skip this match, move to next
      ((and text (or (string=? text "n") (= code QT_KEY_DELETE) (= code QT_KEY_BACKSPACE)))
       (set! *qreplace-pos* (+ match-pos 1))
       (qreplace-show-next! app)
       #t)
      ;; !: replace all remaining
      ((and text (string=? text "!"))
       (qreplace-replace-all! app)
       #t)
      ;; q or Escape: quit
      ((or (and text (string=? text "q")) (= code QT_KEY_ESCAPE))
       (qreplace-finish! app)
       #t)
      ;; . (period): replace this one and quit
      ((and text (string=? text "."))
       (qreplace-do-replace! app match-pos)
       (qreplace-finish! app)
       #t)
      ;; C-g: cancel
      ((and (= code QT_KEY_G) (= (bitwise-and mods QT_MOD_CTRL) QT_MOD_CTRL))
       (qreplace-finish! app)
       #t)
      ;; Ignore other keys
      (else #t))))

(def (cmd-query-replace app)
  (let* ((echo (app-state-echo app))
         (from-str (qt-echo-read-string app "Query replace: ")))
    (when (and from-str (> (string-length from-str) 0))
      (let ((to-str (qt-echo-read-string app
                      (string-append "Replace \"" from-str "\" with: "))))
        (when to-str
          ;; Enter interactive query-replace mode
          (set! *qreplace-active* #t)
          (set! *qreplace-from* from-str)
          (set! *qreplace-to* to-str)
          (set! *qreplace-pos* (qt-plain-text-edit-cursor-position (current-qt-editor app)))
          (set! *qreplace-count* 0)
          (set! *qreplace-app* app)
          ;; Find and show first match
          (qreplace-show-next! app))))))

;;;============================================================================
;;; Eshell commands
;;;============================================================================

(def eshell-buffer-name "*eshell*")

(def (cmd-eshell app)
  "Open or switch to the *eshell* buffer (powered by gsh)."
  (let ((existing (buffer-by-name eshell-buffer-name)))
    (if existing
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app)))
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
        (echo-message! (app-state-echo app) eshell-buffer-name))
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app))
             (buf (qt-buffer-create! eshell-buffer-name ed #f)))
        (set! (buffer-lexer-lang buf) 'eshell)
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        ;; Initialize gsh environment for this buffer
        (gsh-eshell-init-buffer! buf)
        (let ((welcome (string-append "gsh — Gerbil Shell\n"
                                       "Type commands or 'exit' to close.\n\n"
                                       (gsh-eshell-get-prompt buf))))
          (qt-plain-text-edit-set-text! ed welcome)
          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END))
        (echo-message! (app-state-echo app) "gsh started")))))

(def (cmd-eshell-send app)
  "Process eshell input in Qt backend via gsh."
  (let* ((buf (current-qt-buffer app))
         (env (hash-get *gsh-eshell-state* buf)))
    ;; Fall back to legacy if no gsh env
    (if (not env)
      (cmd-eshell-send-legacy/qt app)
      (let* ((ed (current-qt-editor app))
             (all-text (qt-plain-text-edit-text ed))
             ;; Find the last gsh prompt (use current prompt string for matching)
             (cur-prompt gsh-eshell-prompt)
             (prompt-pos (let ((prompt-len (string-length cur-prompt)))
                           (let loop ((pos (- (string-length all-text) prompt-len)))
                             (cond
                               ((< pos 0) #f)
                               ((string=? (substring all-text pos (+ pos prompt-len)) cur-prompt) pos)
                               (else (loop (- pos 1)))))))
             (end-pos (string-length all-text))
             (input (if (and prompt-pos (> end-pos (+ prompt-pos (string-length cur-prompt))))
                      (substring all-text (+ prompt-pos (string-length cur-prompt)) end-pos)
                      "")))
        ;; Record in shell history before processing
        (let ((trimmed-input (safe-string-trim-both input)))
          (when (> (string-length trimmed-input) 0)
            (gsh-history-add! trimmed-input (current-directory))))
        (qt-plain-text-edit-append! ed "")
        (let-values (((output new-cwd) (gsh-eshell-process-input input buf)))
          (cond
            ((eq? output 'clear)
             (let ((new-prompt (gsh-eshell-get-prompt buf)))
               (qt-plain-text-edit-set-text! ed new-prompt)
               (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)))
            ((eq? output 'exit)
             ;; Kill eshell buffer directly
             (let* ((fr (app-state-frame app))
                    (ed (current-qt-editor app))
                    (other (let loop ((bs (buffer-list)))
                             (cond ((null? bs) #f)
                                   ((eq? (car bs) buf) (loop (cdr bs)))
                                   (else (car bs))))))
               (when other
                 (qt-buffer-attach! ed other)
                 (set! (qt-edit-window-buffer (qt-current-window fr)) other))
               (hash-remove! *gsh-eshell-state* buf)
               (qt-buffer-kill! buf)
               (echo-message! (app-state-echo app) "gsh finished")))
            (else
             (when (and (string? output) (> (string-length output) 0))
               (qt-plain-text-edit-append! ed output))
             (let ((new-prompt (gsh-eshell-get-prompt buf)))
               (qt-plain-text-edit-append! ed new-prompt))
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END))))))))

(def (cmd-eshell-send-legacy/qt app)
  "Legacy eshell input processing for Qt (buffers without gsh env)."
  (let* ((buf (current-qt-buffer app))
         (cwd (hash-get *eshell-state* buf)))
    (when cwd
      (let* ((ed (current-qt-editor app))
             (all-text (qt-plain-text-edit-text ed))
             (prompt-pos (let loop ((pos (- (string-length all-text) (string-length eshell-prompt))))
                           (cond
                             ((< pos 0) #f)
                             ((string=? (substring all-text pos (+ pos (string-length eshell-prompt))) eshell-prompt) pos)
                             (else (loop (- pos 1))))))
             (end-pos (string-length all-text))
             (input (if (and prompt-pos (> end-pos (+ prompt-pos (string-length eshell-prompt))))
                      (substring all-text (+ prompt-pos (string-length eshell-prompt)) end-pos)
                      "")))
        (let ((trimmed-input (safe-string-trim-both input)))
          (when (> (string-length trimmed-input) 0)
            (gsh-history-add! trimmed-input cwd)))
        (qt-plain-text-edit-append! ed "")
        (let-values (((output new-cwd) (eshell-process-input input cwd)))
          (hash-put! *eshell-state* buf new-cwd)
          (cond
            ((eq? output 'clear)
             (qt-plain-text-edit-set-text! ed eshell-prompt)
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END))
            ((eq? output 'exit)
             (let* ((fr (app-state-frame app))
                    (ed (current-qt-editor app))
                    (other (let loop ((bs (buffer-list)))
                             (cond ((null? bs) #f)
                                   ((eq? (car bs) buf) (loop (cdr bs)))
                                   (else (car bs))))))
               (when other
                 (qt-buffer-attach! ed other)
                 (set! (qt-edit-window-buffer (qt-current-window fr)) other))
               (hash-remove! *eshell-state* buf)
               (qt-buffer-kill! buf)
               (echo-message! (app-state-echo app) "Eshell finished")))
            (else
             (when (and (string? output) (> (string-length output) 0))
               (qt-plain-text-edit-append! ed output))
             (qt-plain-text-edit-append! ed eshell-prompt)
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END))))))))

;;;============================================================================
;;; Shell commands
;;;============================================================================

(def shell-buffer-name "*shell*")

(def (cmd-shell app)
  "Open or switch to the *shell* buffer (gsh-backed)."
  (let ((existing (buffer-by-name shell-buffer-name)))
    (if existing
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app)))
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
        (echo-message! (app-state-echo app) shell-buffer-name))
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app))
             (buf (qt-buffer-create! shell-buffer-name ed #f)))
        (set! (buffer-lexer-lang buf) 'shell)
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (with-catch
          (lambda (e)
            (let ((msg (with-output-to-string "" (lambda () (display-exception e)))))
              (gemacs-log! "cmd-shell: gsh init failed: " msg)
              (echo-error! (app-state-echo app)
                (string-append "Shell failed: " msg))))
          (lambda ()
            (let ((ss (shell-start!)))
              (hash-put! *shell-state* buf ss)
              (let ((prompt (shell-prompt ss)))
                (qt-plain-text-edit-set-text! ed prompt)
                (set! (shell-state-prompt-pos ss) (string-length prompt))
                (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)))
            (echo-message! (app-state-echo app) "gsh started")))))))

(def (cmd-shell-send app)
  "Execute the current input line in the shell via gsh.
   Builtins run synchronously, external commands run async via PTY.
   When PTY is busy (e.g. sudo password prompt), sends newline to PTY."
  (let* ((buf (current-qt-buffer app))
         (ss (hash-get *shell-state* buf)))
    (when ss
      ;; If PTY is busy, just send newline to the child process
      (if (shell-pty-busy? ss)
        (shell-send-input! ss "\n")
        (let* ((ed (current-qt-editor app))
               (all-text (qt-plain-text-edit-text ed))
               (prompt-pos (shell-state-prompt-pos ss))
               (end-pos (string-length all-text))
               (input (if (> end-pos prompt-pos)
                        (substring all-text prompt-pos end-pos)
                        "")))
          ;; Record in shell history
          (let ((trimmed-input (safe-string-trim-both input)))
            (when (> (string-length trimmed-input) 0)
              (gsh-history-add! trimmed-input (current-directory))))
          ;; Append newline after user input
          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
          (qt-plain-text-edit-insert-text! ed "\n")
          (let-values (((mode output new-cwd) (shell-execute-async! input ss)))
          (case mode
            ((sync)
             (when (and (string? output) (> (string-length output) 0))
               (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
               (qt-plain-text-edit-insert-text! ed output)
               (unless (char=? (string-ref output (- (string-length output) 1)) #\newline)
                 (qt-plain-text-edit-insert-text! ed "\n")))
             ;; Display prompt after sync command
             (when (hash-get *shell-state* buf)
               (let ((prompt (shell-prompt ss)))
                 (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
                 (qt-plain-text-edit-insert-text! ed prompt)
                 (set! (shell-state-prompt-pos ss)
                   (string-length (qt-plain-text-edit-text ed)))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))))
            ((async)
             ;; Command dispatched to PTY — output arrives via timer polling
             (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
             (qt-plain-text-edit-ensure-cursor-visible! ed))
            ((special)
             (cond
               ((eq? output 'clear)
                (qt-plain-text-edit-set-text! ed "")
                (let ((prompt (shell-prompt ss)))
                  (qt-plain-text-edit-insert-text! ed prompt)
                  (set! (shell-state-prompt-pos ss)
                    (string-length (qt-plain-text-edit-text ed)))
                  (qt-plain-text-edit-ensure-cursor-visible! ed)))
               ((eq? output 'exit)
                (shell-stop! ss)
                (let* ((fr (app-state-frame app))
                       (other (let loop ((bs (buffer-list)))
                                (cond ((null? bs) #f)
                                      ((eq? (car bs) buf) (loop (cdr bs)))
                                      (else (car bs))))))
                  (when other
                    (qt-buffer-attach! ed other)
                    (set! (qt-edit-window-buffer (qt-current-window fr)) other))
                  (hash-remove! *shell-state* buf)
                  (qt-buffer-kill! buf)
                  (echo-message! (app-state-echo app) "Shell exited"))))))))))))
;;;============================================================================
;;; AI Chat commands (Claude CLI integration)
;;;============================================================================

(def qt-chat-buffer-name "*AI Chat*")
(def qt-chat-prompt "\n\nYou: ")

(def (cmd-chat app)
  "Open or switch to the *AI Chat* buffer."
  (let ((existing (buffer-by-name qt-chat-buffer-name)))
    (if existing
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app)))
        (buffer-touch! existing)
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
        (echo-message! (app-state-echo app) qt-chat-buffer-name))
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app))
             (buf (qt-buffer-create! qt-chat-buffer-name ed #f)))
        (set! (buffer-lexer-lang buf) 'chat)
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        (let ((cs (chat-start! (current-directory))))
          (hash-put! *chat-state* buf cs)
          (let ((greeting "Claude AI Chat — Type your message and press Enter.\n\nYou: "))
            (qt-plain-text-edit-set-text! ed greeting)
            (set! (chat-state-prompt-pos cs) (string-length greeting))
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
            (qt-plain-text-edit-ensure-cursor-visible! ed)))
        (echo-message! (app-state-echo app) "AI Chat started")))))

(def (cmd-chat-send app)
  "Extract typed text since prompt and send to Claude CLI."
  (let* ((buf (current-qt-buffer app))
         (cs (hash-get *chat-state* buf)))
    (when cs
      (if (chat-busy? cs)
        (echo-message! (app-state-echo app) "Waiting for response...")
        (let* ((ed (current-qt-editor app))
               (all-text (qt-plain-text-edit-text ed))
               (prompt-pos (chat-state-prompt-pos cs))
               (end-pos (string-length all-text))
               (input (if (> end-pos prompt-pos)
                        (substring all-text prompt-pos end-pos)
                        "")))
          (when (> (string-length (string-trim input)) 0)
            ;; Append label for AI response
            (qt-plain-text-edit-append! ed "\n\nClaude: ")
            ;; Update prompt-pos
            (set! (chat-state-prompt-pos cs)
              (string-length (qt-plain-text-edit-text ed)))
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
            (qt-plain-text-edit-ensure-cursor-visible! ed)
            ;; Send to claude
            (chat-send! cs input)))))))

;;;============================================================================
;;; Dired (directory listing) support
;;;============================================================================

(def (dired-open-directory! app dir-path)
  "Open a directory listing in a new dired buffer."
  (let* ((dir (strip-trailing-slash dir-path))
         (name (string-append dir "/"))
         (fr (app-state-frame app))
         (ed (current-qt-editor app))
         (buf (qt-buffer-create! name ed dir)))
    ;; Mark as dired buffer
    (set! (buffer-lexer-lang buf) 'dired)
    ;; Attach buffer to editor
    (qt-buffer-attach! ed buf)
    (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
    ;; Generate and set listing
    (let-values (((text entries) (dired-format-listing dir)))
      (qt-plain-text-edit-set-text! ed text)
      (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
      ;; Position cursor at first entry (line 3, after header + count + blank)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_DOWN)
      (qt-plain-text-edit-move-cursor! ed QT_CURSOR_START_OF_BLOCK)
      ;; Store entries for navigation
      (hash-put! *dired-entries* buf entries))
    (echo-message! (app-state-echo app) (string-append "Directory: " dir))))

(def (cmd-dired-find-file app)
  "In a dired buffer, open the file or directory under cursor."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (line (qt-plain-text-edit-cursor-line ed))
         (entries (hash-get *dired-entries* buf)))
    (when entries
      (let ((idx (- line 3)))
        (if (or (< idx 0) (>= idx (vector-length entries)))
          (echo-message! (app-state-echo app) "No file on this line")
          (let ((full-path (vector-ref entries idx)))
            (with-catch
              (lambda (e)
                (echo-error! (app-state-echo app)
                             (string-append "Error: "
                               (with-output-to-string
                                 (lambda () (display-exception e))))))
              (lambda ()
                (let ((info (file-info full-path)))
                  (cond
                    ((eq? 'directory (file-info-type info))
                     (dired-open-directory! app full-path))
                    ;; Image file -> open inline in editor area
                    ((image-file? full-path)
                     (let* ((pixmap (qt-pixmap-load full-path)))
                       (if (qt-pixmap-null? pixmap)
                         (begin
                           (qt-pixmap-destroy! pixmap)
                           (echo-error! (app-state-echo app)
                             (string-append "Failed to load image: " full-path)))
                         (let* ((fname (path-strip-directory full-path))
                                (fr (app-state-frame app))
                                (new-buf (qt-buffer-create! fname ed full-path))
                                (orig-w (qt-pixmap-width pixmap))
                                (orig-h (qt-pixmap-height pixmap)))
                           (set! (buffer-lexer-lang new-buf) 'image)
                           (hash-put! *image-buffer-state* new-buf
                             (list pixmap (box 1.0) orig-w orig-h))
                           (qt-buffer-attach! ed new-buf)
                           (set! (qt-edit-window-buffer (qt-current-window fr))
                                 new-buf)
                           (echo-message! (app-state-echo app)
                             (string-append fname " "
                               (number->string orig-w) "x"
                               (number->string orig-h)))))))
                    ;; Regular text file
                    (else
                     (let* ((fname (path-strip-directory full-path))
                            (fr (app-state-frame app))
                            (new-buf (qt-buffer-create! fname ed full-path)))
                       (qt-buffer-attach! ed new-buf)
                       (set! (qt-edit-window-buffer (qt-current-window fr))
                             new-buf)
                       (let ((text (read-file-as-string full-path)))
                         (when text
                           (qt-plain-text-edit-set-text! ed text)
                           (qt-text-document-set-modified!
                             (buffer-doc-pointer new-buf) #f)
                           (qt-plain-text-edit-set-cursor-position! ed 0)))
                       (qt-setup-highlighting! app new-buf)
                       (echo-message! (app-state-echo app)
                                      (string-append "Opened: "
                                                     full-path))))))))))))))

(def (cmd-dired-rename-at-point app)
  "Rename the file under cursor in dired."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (echo (app-state-echo app))
         (line (qt-plain-text-edit-cursor-line ed))
         (entries (hash-get *dired-entries* buf))
         (dir (buffer-file-path buf)))
    (if (not entries)
      (echo-error! echo "Not in a dired buffer")
      (let ((idx (- line 3)))
        (if (or (< idx 0) (>= idx (vector-length entries)))
          (echo-error! echo "Not on a file line")
          (let* ((full-path (vector-ref entries idx))
                 (fname (path-strip-directory full-path))
                 (new-name (qt-echo-read-string app
                             (string-append "Rename " fname " to: "))))
            (when (and new-name (> (string-length new-name) 0))
              (with-catch
                (lambda (e)
                  (echo-error! echo (string-append "Error: "
                    (with-output-to-string (lambda () (display-exception e))))))
                (lambda ()
                  (rename-file full-path (path-expand new-name dir))
                  (dired-open-directory! app dir)
                  (echo-message! echo
                    (string-append "Renamed: " fname " → " new-name)))))))))))

(def (cmd-dired-copy-at-point app)
  "Copy the file under cursor in dired."
  (let* ((buf (current-qt-buffer app))
         (ed (current-qt-editor app))
         (echo (app-state-echo app))
         (line (qt-plain-text-edit-cursor-line ed))
         (entries (hash-get *dired-entries* buf))
         (dir (buffer-file-path buf)))
    (if (not entries)
      (echo-error! echo "Not in a dired buffer")
      (let ((idx (- line 3)))
        (if (or (< idx 0) (>= idx (vector-length entries)))
          (echo-error! echo "Not on a file line")
          (let* ((full-path (vector-ref entries idx))
                 (fname (path-strip-directory full-path))
                 (dest (qt-echo-read-string app
                         (string-append "Copy " fname " to: "))))
            (when (and dest (> (string-length dest) 0))
              (with-catch
                (lambda (e)
                  (echo-error! echo (string-append "Error: "
                    (with-output-to-string (lambda () (display-exception e))))))
                (lambda ()
                  (copy-file full-path (path-expand dest dir))
                  (dired-open-directory! app dir)
                  (echo-message! echo
                    (string-append "Copied: " fname " → " dest)))))))))))

;;;============================================================================
;;; REPL commands
;;;============================================================================

(def repl-buffer-name "*REPL*")

(def (cmd-repl app)
  "Open or switch to the *REPL* buffer."
  (let ((existing (buffer-by-name repl-buffer-name)))
    (if existing
      ;; Switch to existing REPL buffer
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app)))
        (qt-buffer-attach! ed existing)
        (set! (qt-edit-window-buffer (qt-current-window fr)) existing)
        (echo-message! (app-state-echo app) repl-buffer-name))
      ;; Create new REPL buffer
      (let* ((fr (app-state-frame app))
             (ed (current-qt-editor app))
             (buf (qt-buffer-create! repl-buffer-name ed #f)))
        ;; Mark as REPL buffer
        (set! (buffer-lexer-lang buf) 'repl)
        ;; Attach buffer to editor
        (qt-buffer-attach! ed buf)
        (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
        ;; Spawn gxi subprocess
        (let ((rs (repl-start!)))
          (hash-put! *repl-state* buf rs)
          ;; Show prompt immediately — gxi in non-interactive mode (pseudo-terminal: #f)
          ;; does NOT send a startup banner, so the timer would never fire and
          ;; prompt-pos would stay at 999999999, blocking all typing.
          (qt-plain-text-edit-set-text! ed repl-prompt)
          (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
          (set! (repl-state-prompt-pos rs)
            (string-length repl-prompt)))
        (echo-message! (app-state-echo app) "REPL started")))))

(def (cmd-repl-send app)
  "Send the current input line to the gxi subprocess."
  (let* ((buf (current-qt-buffer app))
         (rs (hash-get *repl-state* buf)))
    (when rs
      (let* ((ed (current-qt-editor app))
             (prompt-pos (repl-state-prompt-pos rs))
             (all-text (qt-plain-text-edit-text ed))
             (text-len (string-length all-text))
             ;; Extract user input after the prompt
             (input (if (and (<= prompt-pos text-len) (> text-len prompt-pos))
                      (substring all-text prompt-pos text-len)
                      "")))
        ;; Append newline to the buffer
        (qt-plain-text-edit-append! ed "")
        ;; Send to gxi (even empty input — gxi ignores it and sends new prompt)
        (repl-send! rs input)
        ;; Update prompt-pos to after the newline (output will appear here)
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
        (set! (repl-state-prompt-pos rs)
          (string-length (qt-plain-text-edit-text ed)))))))

(def (cmd-eval-expression app)
  "Prompt for an expression, eval it in-process."
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Eval: ")))
    (when (and input (> (string-length input) 0))
      (let-values (((result error?) (eval-expression-string input)))
        (if error?
          (echo-error! echo result)
          (echo-message! echo result))))))

;;;============================================================================
;;; Load file (M-x load-file)
;;;============================================================================

(def (cmd-load-file app)
  "Prompt for a .ss file path and evaluate all its forms."
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-file-with-completion app "Load file: ")))
    (when (and filename (> (string-length filename) 0))
      (let ((path (expand-filename filename)))
        (if (file-exists? path)
          (let-values (((count err) (load-user-file! path)))
            (if err
              (echo-error! echo (string-append "Error: " err))
              (echo-message! echo (string-append "Loaded " (number->string count)
                                                 " forms from " path))))
          (echo-error! echo (string-append "File not found: " path)))))))

;;;============================================================================
;;; Zoom commands
;;;============================================================================

(def (cmd-zoom-in app)
  (let* ((ed (current-qt-editor app))
         (font (qt-widget-font ed))
         (size (qt-font-point-size font)))
    (qt-font-destroy! font)
    (qt-widget-set-font-size! ed (+ size 1))))

(def (cmd-zoom-out app)
  (let* ((ed (current-qt-editor app))
         (font (qt-widget-font ed))
         (size (qt-font-point-size font)))
    (qt-font-destroy! font)
    (when (> size 6)
      (qt-widget-set-font-size! ed (- size 1)))))

;;;============================================================================
;;; Toggle line numbers
;;;============================================================================

(def *line-numbers-visible* #t)

(def (cmd-toggle-line-numbers app)
  (set! *line-numbers-visible* (not *line-numbers-visible*))
  (let ((fr (app-state-frame app)))
    (for-each
      (lambda (win)
        (let ((lna (qt-edit-window-line-number-area win)))
          (when lna
            (qt-line-number-area-set-visible! lna *line-numbers-visible*))))
      (qt-frame-windows fr)))
  (echo-message! (app-state-echo app)
    (if *line-numbers-visible* "Line numbers ON" "Line numbers OFF")))

;;;============================================================================
;;; Pulse-line: briefly highlight a line after jumps (goto-line, search, etc.)
;;;============================================================================

(def *qt-pulse-indicator* 19)  ; Scintilla indicator number (avoid conflicts)
(def *qt-pulse-editor* #f)     ; editor with active pulse
(def *qt-pulse-countdown* 0)   ; ticks remaining
(def *qt-pulse-last-line* -1)  ; last known cursor line (for jump detection)
(def *qt-pulse-mode* #t)       ; enable/disable pulse-on-jump

(def (qt-pulse-clear! ed)
  "Clear any active pulse indicator."
  (let ((len (sci-send ed SCI_GETLENGTH 0 0)))
    (sci-send ed SCI_SETINDICATORCURRENT *qt-pulse-indicator* 0)
    (sci-send ed SCI_INDICATORCLEARRANGE 0 len))
  (set! *qt-pulse-editor* #f)
  (set! *qt-pulse-countdown* 0))

(def (qt-pulse-line! ed line-num)
  "Flash-highlight the given line number temporarily (~500ms).
Uses Scintilla INDIC_FULLBOX indicator with golden/orange color."
  (let* ((start (sci-send ed SCI_POSITIONFROMLINE line-num 0))
         (end   (sci-send ed SCI_GETLINEENDPOSITION line-num 0))
         (len   (- end start)))
    (when (> len 0)
      ;; Clear any previous pulse
      (when *qt-pulse-editor*
        (qt-pulse-clear! *qt-pulse-editor*))
      ;; Set up indicator style: INDIC_FULLBOX with golden/orange color
      (sci-send ed SCI_INDICSETSTYLE *qt-pulse-indicator* 16)  ; 16 = INDIC_FULLBOX
      (sci-send ed SCI_INDICSETFORE *qt-pulse-indicator* #x00A5FF) ; golden/orange (BGR)
      (sci-send ed 2523 *qt-pulse-indicator* 80)  ; SCI_INDICSETALPHA = 2523
      (sci-send ed SCI_SETINDICATORCURRENT *qt-pulse-indicator* 0)
      (sci-send ed SCI_INDICATORFILLRANGE start len)
      (set! *qt-pulse-editor* ed)
      (set! *qt-pulse-countdown* 10))))  ; 10 * 50ms = 500ms

(def (qt-pulse-tick!)
  "Called each master timer tick. Decrements pulse countdown and clears when done."
  (when (and *qt-pulse-editor* (> *qt-pulse-countdown* 0))
    (set! *qt-pulse-countdown* (- *qt-pulse-countdown* 1))
    (when (<= *qt-pulse-countdown* 0)
      (qt-pulse-clear! *qt-pulse-editor*))))

(def (qt-pulse-check-jump! app)
  "Auto-detect large cursor jumps and pulse the landing line.
Called periodically from the master timer. Pulses when cursor
moves more than 5 lines from the last known position."
  (when *qt-pulse-mode*
    (let* ((fr (app-state-frame app))
           (ed (qt-current-editor fr))
           (pos (qt-plain-text-edit-cursor-position ed))
           (cur-line (sci-send ed SCI_LINEFROMPOSITION pos 0))
           (delta (abs (- cur-line *qt-pulse-last-line*))))
      (when (and (> delta 5) (>= *qt-pulse-last-line* 0))
        (qt-pulse-line! ed cur-line))
      (set! *qt-pulse-last-line* cur-line))))

(def (cmd-toggle-pulse-line app)
  "Toggle pulse-on-jump mode."
  (set! *qt-pulse-mode* (not *qt-pulse-mode*))
  (echo-message! (app-state-echo app)
    (if *qt-pulse-mode* "Pulse-on-jump enabled" "Pulse-on-jump disabled")))

;;;============================================================================
;;; ANSI color rendering for compilation/shell output buffers
;;;============================================================================

;; Standard ANSI color codes → Scintilla RGB (BGR format for Windows compat)
(def *ansi-colors*
  (vector #x000000   ; 0 = black
          #x0000CC   ; 1 = red
          #x00CC00   ; 2 = green
          #x00CCCC   ; 3 = yellow
          #xCC0000   ; 4 = blue
          #xCC00CC   ; 5 = magenta
          #xCCCC00   ; 6 = cyan
          #xCCCCCC)) ; 7 = white

(def *ansi-bright-colors*
  (vector #x666666   ; 0 = bright black (gray)
          #x0000FF   ; 1 = bright red
          #x00FF00   ; 2 = bright green
          #x00FFFF   ; 3 = bright yellow
          #xFF0000   ; 4 = bright blue
          #xFF00FF   ; 5 = bright magenta
          #xFFFF00   ; 6 = bright cyan
          #xFFFFFF)) ; 7 = bright white

(def (ansi-parse-segments text)
  "Parse text with ANSI escape codes into a list of (string fg bg bold?) segments.
Returns (values clean-text segments) where segments is a list of
(start-in-clean length fg-color bg-color bold?)."
  (let* ((len (string-length text))
         (clean (open-output-string))
         (segments [])
         (cur-fg #f)
         (cur-bg #f)
         (cur-bold #f)
         (seg-start 0))
    (let loop ((i 0) (clean-pos 0))
      (cond
        ((>= i len)
         ;; Flush last segment
         (when (and (> clean-pos seg-start) (or cur-fg cur-bg cur-bold))
           (set! segments (cons (list seg-start (- clean-pos seg-start)
                                      cur-fg cur-bg cur-bold) segments)))
         (values (get-output-string clean) (reverse segments)))
        ;; ESC [ sequence
        ((and (char=? (string-ref text i) #\escape)
              (< (+ i 1) len)
              (char=? (string-ref text (+ i 1)) #\[))
         ;; Flush current segment if it has styling
         (when (and (> clean-pos seg-start) (or cur-fg cur-bg cur-bold))
           (set! segments (cons (list seg-start (- clean-pos seg-start)
                                      cur-fg cur-bg cur-bold) segments)))
         ;; Parse SGR parameters
         (let param-loop ((j (+ i 2)) (params []))
           (cond
             ((>= j len) (loop j clean-pos))  ; unterminated
             ((char=? (string-ref text j) #\m) ; end of SGR
              ;; Apply SGR codes
              (let ((codes (reverse params)))
                (for-each
                  (lambda (code)
                    (cond
                      ((= code 0) (set! cur-fg #f) (set! cur-bg #f) (set! cur-bold #f))
                      ((= code 1) (set! cur-bold #t))
                      ((and (>= code 30) (<= code 37))
                       (set! cur-fg (vector-ref
                                      (if cur-bold *ansi-bright-colors* *ansi-colors*)
                                      (- code 30))))
                      ((and (>= code 40) (<= code 47))
                       (set! cur-bg (vector-ref *ansi-colors* (- code 40))))
                      ((and (>= code 90) (<= code 97))
                       (set! cur-fg (vector-ref *ansi-bright-colors* (- code 90))))
                      ((and (>= code 100) (<= code 107))
                       (set! cur-bg (vector-ref *ansi-bright-colors* (- code 100))))))
                  (if (null? codes) [0] codes)))  ; empty = reset
              (set! seg-start clean-pos)
              (loop (+ j 1) clean-pos))
             ((or (char-numeric? (string-ref text j))
                  (char=? (string-ref text j) #\;))
              ;; Accumulate parameter
              (if (char=? (string-ref text j) #\;)
                (param-loop (+ j 1) (cons 0 params))  ; placeholder
                ;; Build number
                (let num-loop ((k j) (n 0))
                  (if (and (< k len) (char-numeric? (string-ref text k)))
                    (num-loop (+ k 1) (+ (* n 10) (- (char->integer (string-ref text k))
                                                      (char->integer #\0))))
                    (begin
                      (set! params (cons n params))
                      (if (and (< k len) (char=? (string-ref text k) #\;))
                        (param-loop (+ k 1) params)
                        (param-loop k params)))))))
             (else
              ;; Unknown char in escape — skip the whole sequence
              (loop (+ j 1) clean-pos)))))
        (else
         ;; Normal character
         (write-char (string-ref text i) clean)
         (loop (+ i 1) (+ clean-pos 1)))))))

(def (qt-apply-ansi-styles! ed segments)
  "Apply ANSI color segments to a Scintilla editor using manual styling.
SEGMENTS is a list of (start length fg bg bold?) from ansi-parse-segments."
  ;; Define styles 40-55 for ANSI colors (avoid conflict with lexer styles 0-39)
  (let ((style-id 40))
    (for-each
      (lambda (seg)
        (let ((start (car seg))
              (len (cadr seg))
              (fg (caddr seg))
              (bg (cadddr seg))
              (bold? (car (cddddr seg))))
          (when (and (> len 0) (<= style-id 55))
            ;; Configure this style
            (when fg (sci-send ed SCI_STYLESETFORE style-id fg))
            (when bg (sci-send ed SCI_STYLESETBACK style-id bg))
            (when bold? (sci-send ed SCI_STYLESETBOLD style-id 1))
            ;; Apply styling to the range
            (sci-send ed SCI_STARTSTYLING start 0)
            (sci-send ed SCI_SETSTYLING len style-id)
            (set! style-id (+ style-id 1)))))
      segments)))

(def (qt-set-text-with-ansi! ed text)
  "Set editor text, stripping ANSI codes and applying color styles.
Returns the clean text (without ANSI codes)."
  ;; Disable lexer to allow manual styling
  (sci-send ed 4033 0 0)  ; SCI_SETILEXER = 4033, set to NULL for manual styling
  (let-values (((clean-text segments) (ansi-parse-segments text)))
    (qt-plain-text-edit-set-text! ed clean-text)
    (when (pair? segments)
      (qt-apply-ansi-styles! ed segments))
    clean-text))

(def (cmd-ansi-color-apply app)
  "Re-render current buffer, converting ANSI escape codes to colors."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed)))
    (qt-set-text-with-ansi! ed text)
    (echo-message! (app-state-echo app) "ANSI colors applied")))
