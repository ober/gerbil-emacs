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
        (let ((trimmed-input (string-trim-both input)))
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
        (let ((trimmed-input (string-trim-both input)))
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
  "Execute the current input line in the shell via gsh."
  (let* ((buf (current-qt-buffer app))
         (ss (hash-get *shell-state* buf)))
    (when ss
      (let* ((ed (current-qt-editor app))
             (all-text (qt-plain-text-edit-text ed))
             (prompt-pos (shell-state-prompt-pos ss))
             (end-pos (string-length all-text))
             (input (if (> end-pos prompt-pos)
                      (substring all-text prompt-pos end-pos)
                      "")))
        ;; Record in shell history
        (let ((trimmed-input (string-trim-both input)))
          (when (> (string-length trimmed-input) 0)
            (gsh-history-add! trimmed-input (current-directory))))
        ;; Append newline after user input
        (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
        (qt-plain-text-edit-insert-text! ed "\n")
        (let-values (((output new-cwd) (shell-execute! input ss)))
          (cond
            ((eq? output 'clear)
             (qt-plain-text-edit-set-text! ed ""))
            ((eq? output 'exit)
             ;; Kill shell buffer and switch to another
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
               (echo-message! (app-state-echo app) "Shell exited")))
            (else
             ;; Append command output
             (when (and (string? output) (> (string-length output) 0))
               (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
               (qt-plain-text-edit-insert-text! ed output)
               (unless (char=? (string-ref output (- (string-length output) 1)) #\newline)
                 (qt-plain-text-edit-insert-text! ed "\n"))))))
        ;; Display new prompt (unless exited)
        (when (hash-get *shell-state* buf)
          (let ((prompt (shell-prompt ss)))
            (qt-plain-text-edit-move-cursor! ed QT_CURSOR_END)
            (qt-plain-text-edit-insert-text! ed prompt)
            (set! (shell-state-prompt-pos ss)
              (string-length (qt-plain-text-edit-text ed)))
            (qt-plain-text-edit-ensure-cursor-visible! ed)))))))
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
;;; Backward kill word
;;;============================================================================

(def (cmd-backward-kill-word app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed)))
    (when (> pos 0)
      ;; Skip whitespace backward
      (let skip-ws ((i (- pos 1)))
        (if (and (>= i 0) (char-whitespace? (string-ref text i)))
          (skip-ws (- i 1))
          ;; Now skip word chars backward
          (let skip-word ((j i))
            (if (and (>= j 0)
                     (let ((ch (string-ref text j)))
                       (or (char-alphabetic? ch) (char-numeric? ch)
                           (char=? ch #\_) (char=? ch #\-))))
              (skip-word (- j 1))
              ;; j+1 is the start of the word
              (let* ((start (+ j 1))
                     (killed (substring text start pos))
                     (new-text (string-append
                                 (substring text 0 start)
                                 (substring text pos (string-length text)))))
                (set! (app-state-kill-ring app)
                      (cons killed (app-state-kill-ring app)))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed start)))))))))

;;;============================================================================
;;; Kill whole line
;;;============================================================================

(def (cmd-kill-whole-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let* ((killed (list-ref lines line))
             (new-lines (let loop ((ls lines) (i 0) (acc []))
                          (if (null? ls) (reverse acc)
                            (if (= i line) (loop (cdr ls) (+ i 1) acc)
                              (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
             (new-text (if (null? new-lines) "" (string-join new-lines "\n"))))
        (set! (app-state-kill-ring app)
              (cons killed (app-state-kill-ring app)))
        (qt-plain-text-edit-set-text! ed new-text)
        (let ((pos (let loop ((i 0) (ln 0))
                     (cond ((= ln line) i)
                           ((>= i (string-length new-text)) i)
                           ((char=? (string-ref new-text i) #\newline)
                            (loop (+ i 1) (+ ln 1)))
                           (else (loop (+ i 1) ln))))))
          (qt-plain-text-edit-set-cursor-position! ed
            (min pos (string-length new-text))))))))

;;;============================================================================
;;; Join line
;;;============================================================================

(def (cmd-join-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< (+ line 1) (length lines))
      (let* ((current (list-ref lines line))
             (next (string-trim (list-ref lines (+ line 1))))
             (joined (string-append (string-trim-right current) " " next))
             (new-lines (let loop ((ls lines) (i 0) (acc []))
                          (cond ((null? ls) (reverse acc))
                                ((= i line)
                                 (loop (if (pair? (cdr ls)) (cddr ls) '())
                                       (+ i 2) (cons joined acc)))
                                (else (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))))))

;;;============================================================================
;;; Just one space
;;;============================================================================

(def (cmd-just-one-space app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (let* ((start (let loop ((i (- pos 1)))
                    (if (and (>= i 0) (char-whitespace? (string-ref text i)))
                      (loop (- i 1))
                      (+ i 1))))
           (end (let loop ((i pos))
                  (if (and (< i len) (char-whitespace? (string-ref text i)))
                    (loop (+ i 1))
                    i)))
           (new-text (string-append (substring text 0 start) " "
                                    (substring text end len))))
      (qt-plain-text-edit-set-text! ed new-text)
      (qt-plain-text-edit-set-cursor-position! ed (+ start 1)))))

;;;============================================================================
;;; Transpose words
;;;============================================================================

(def (cmd-transpose-words app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    ;; Find word before cursor
    (let* ((w1-end (let loop ((i (- pos 1)))
                     (if (and (>= i 0) (char-whitespace? (string-ref text i)))
                       (loop (- i 1)) (+ i 1))))
           (w1-start (let loop ((i (- w1-end 1)))
                       (if (and (>= i 0) (not (char-whitespace? (string-ref text i))))
                         (loop (- i 1)) (+ i 1))))
           ;; Find word after cursor
           (w2-start (let loop ((i pos))
                       (if (and (< i len) (char-whitespace? (string-ref text i)))
                         (loop (+ i 1)) i)))
           (w2-end (let loop ((i w2-start))
                     (if (and (< i len) (not (char-whitespace? (string-ref text i))))
                       (loop (+ i 1)) i))))
      (when (and (< w1-start w1-end) (< w2-start w2-end) (<= w1-end w2-start))
        (let* ((word1 (substring text w1-start w1-end))
               (between (substring text w1-end w2-start))
               (word2 (substring text w2-start w2-end))
               (new-text (string-append (substring text 0 w1-start)
                                        word2 between word1
                                        (substring text w2-end len))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed w2-end))))))

;;;============================================================================
;;; Transpose lines
;;;============================================================================

(def (cmd-transpose-lines app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (and (> line 0) (< line (length lines)))
      (let* ((vec (list->vector lines))
             (tmp (vector-ref vec line)))
        (vector-set! vec line (vector-ref vec (- line 1)))
        (vector-set! vec (- line 1) tmp)
        (let ((new-text (string-join (vector->list vec) "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          ;; Position cursor at start of next line
          (let ((pos (let loop ((i 0) (ln 0))
                       (cond ((= ln (+ line 1)) i)
                             ((>= i (string-length new-text)) i)
                             ((char=? (string-ref new-text i) #\newline)
                              (loop (+ i 1) (+ ln 1)))
                             (else (loop (+ i 1) ln))))))
            (qt-plain-text-edit-set-cursor-position! ed
              (min pos (string-length new-text)))))))))

;;;============================================================================
;;; Move line up / down
;;;============================================================================

(def (line-start-position text line-num)
  "Get character position of the start of a given line number."
  (let loop ((i 0) (ln 0))
    (cond ((= ln line-num) i)
          ((>= i (string-length text)) i)
          ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ ln 1)))
          (else (loop (+ i 1) ln)))))

(def (cmd-move-line-up app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (> line 0)
      (let* ((vec (list->vector lines))
             (tmp (vector-ref vec line)))
        (vector-set! vec line (vector-ref vec (- line 1)))
        (vector-set! vec (- line 1) tmp)
        (let ((new-text (string-join (vector->list vec) "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed
            (line-start-position new-text (- line 1))))))))

(def (cmd-move-line-down app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< (+ line 1) (length lines))
      (let* ((vec (list->vector lines))
             (tmp (vector-ref vec line)))
        (vector-set! vec line (vector-ref vec (+ line 1)))
        (vector-set! vec (+ line 1) tmp)
        (let ((new-text (string-join (vector->list vec) "\n")))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed
            (line-start-position new-text (+ line 1))))))))

;;;============================================================================
;;; Fill paragraph
;;;============================================================================

(def (cmd-fill-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (fill-column 70))
    ;; Find paragraph boundaries (blank lines)
    (let* ((para-start (let loop ((i line))
                         (if (or (<= i 0)
                                 (string=? (string-trim (list-ref lines (- i 1))) ""))
                           i (loop (- i 1)))))
           (para-end (let loop ((i line))
                       (if (or (>= i (- (length lines) 1))
                               (string=? (string-trim (list-ref lines (+ i 1))) ""))
                         (+ i 1) (loop (+ i 1)))))
           ;; Collect paragraph text
           (para-lines (let loop ((i para-start) (acc []))
                         (if (>= i para-end) (reverse acc)
                           (loop (+ i 1) (cons (list-ref lines i) acc)))))
           (para-text (string-join para-lines " "))
           (words (filter (lambda (w) (> (string-length w) 0))
                          (string-split (string-trim para-text) #\space)))
           ;; Reflow
           (filled-lines
             (if (null? words) '("")
               (let loop ((ws (cdr words))
                          (current-line (car words))
                          (acc []))
                 (if (null? ws)
                   (reverse (cons current-line acc))
                   (let ((next (string-append current-line " " (car ws))))
                     (if (<= (string-length next) fill-column)
                       (loop (cdr ws) next acc)
                       (loop (cdr ws) (car ws) (cons current-line acc)))))))))
      ;; Replace paragraph lines
      (let* ((before (let loop ((i 0) (acc []))
                       (if (>= i para-start) (reverse acc)
                         (loop (+ i 1) (cons (list-ref lines i) acc)))))
             (after (let loop ((i para-end) (acc []))
                      (if (>= i (length lines)) (reverse acc)
                        (loop (+ i 1) (cons (list-ref lines i) acc)))))
             (new-lines (append before filled-lines after))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))
        (echo-message! (app-state-echo app) "Filled")))))

;;;============================================================================
;;; Count words
;;;============================================================================

(def (cmd-count-words app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text))
         (num-lines (qt-plain-text-edit-line-count ed))
         (words (let loop ((i 0) (in-word #f) (count 0))
                  (if (>= i len) count
                    (let ((ch (string-ref text i)))
                      (if (char-whitespace? ch)
                        (loop (+ i 1) #f count)
                        (loop (+ i 1) #t (if in-word count (+ count 1)))))))))
    (echo-message! (app-state-echo app)
      (string-append "Buffer has " (number->string num-lines) " lines, "
                     (number->string words) " words, "
                     (number->string len) " characters"))))

;;;============================================================================
;;; What cursor position
;;;============================================================================

(def (cmd-what-cursor-position app)
  "Show detailed info about character at point: char, code (hex/dec/oct), position, line, col."
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app)
        (string-append "point=" (number->string pos) " of " (number->string len) " (EOB)"))
      (let* ((ch (string-ref text pos))
             (code (char->integer ch))
             (hex (number->string code 16))
             (oct (number->string code 8))
             (line (let loop ((i 0) (l 1))
                     (cond ((>= i pos) l)
                           ((char=? (string-ref text i) #\newline) (loop (+ i 1) (+ l 1)))
                           (else (loop (+ i 1) l)))))
             (line-start (let loop ((i (- pos 1)))
                           (cond ((< i 0) 0)
                                 ((char=? (string-ref text i) #\newline) (+ i 1))
                                 (else (loop (- i 1))))))
             (col (- pos line-start))
             (pct (if (= len 0) 0 (round (* 100 (/ pos len))))))
        (echo-message! (app-state-echo app)
          (string-append "Char: " (if (char=? ch #\newline) "^J" (string ch))
                         " (0x" hex ", " (number->string code) ", 0" oct ")"
                         "  point=" (number->string pos)
                         " of " (number->string len)
                         " (" (number->string (inexact->exact pct)) "%)"
                         "  line " (number->string line)
                         " col " (number->string col)))))))

;;;============================================================================
;;; Dynamic abbreviation (dabbrev)
;;;============================================================================

(def (cmd-dabbrev-expand app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (pos (qt-plain-text-edit-cursor-position ed))
         (prefix (get-word-prefix ed)))
    (if (string=? prefix "")
      (echo-message! (app-state-echo app) "No dynamic expansion found")
      (let ((state (app-state-dabbrev-state app)))
        (if (and state
                 (eq? (app-state-last-command app) 'dabbrev-expand)
                 (string=? (car state) prefix))
          ;; Continue cycling
          (let ((matches (cadr state))
                (last-pos (caddr state))
                (last-len (cadddr state)))
            (if (null? matches)
              (begin
                (set! (app-state-dabbrev-state app) #f)
                (echo-message! (app-state-echo app) "No further expansions"))
              (let* ((match (car matches))
                     (suffix (substring match (string-length (car state))
                                        (string-length match)))
                     (cur-text (qt-plain-text-edit-text ed))
                     (new-text (string-append
                                 (substring cur-text 0 last-pos)
                                 suffix
                                 (substring cur-text (+ last-pos last-len)
                                           (string-length cur-text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (+ last-pos (string-length suffix)))
                (set! (app-state-dabbrev-state app)
                  (list (car state) (cdr matches)
                        last-pos (string-length suffix))))))
          ;; Fresh expansion
          (let* ((all-words (collect-buffer-words text))
                 (matches (filter (lambda (w)
                                    (and (> (string-length w) (string-length prefix))
                                         (string-prefix? prefix w)
                                         (not (string=? w prefix))))
                                  all-words))
                 (sorted (sort matches string<?)))
            (if (null? sorted)
              (echo-message! (app-state-echo app) "No dynamic expansion found")
              (let* ((match (car sorted))
                     (suffix (substring match (string-length prefix)
                                        (string-length match)))
                     (new-text (string-append
                                 (substring text 0 pos)
                                 suffix
                                 (substring text pos (string-length text)))))
                (qt-plain-text-edit-set-text! ed new-text)
                (qt-plain-text-edit-set-cursor-position! ed
                  (+ pos (string-length suffix)))
                (set! (app-state-dabbrev-state app)
                  (list prefix (cdr sorted) pos (string-length suffix)))))))))))

;;;============================================================================
;;; Delete blank lines
;;;============================================================================

(def (cmd-delete-blank-lines app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (current-blank? (and (< line (length lines))
                              (string=? (string-trim (list-ref lines line)) ""))))
    (if current-blank?
      ;; On blank line: delete all surrounding blank lines, keep one
      (let* ((start (let loop ((i line))
                      (if (and (> i 0) (string=? (string-trim (list-ref lines (- i 1))) ""))
                        (loop (- i 1)) i)))
             (end (let loop ((i line))
                    (if (and (< (+ i 1) (length lines))
                             (string=? (string-trim (list-ref lines (+ i 1))) ""))
                      (loop (+ i 1)) (+ i 1))))
             (new-lines (append
                          (let loop ((i 0) (acc []))
                            (if (>= i start) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))
                          '("")
                          (let loop ((i end) (acc []))
                            (if (>= i (length lines)) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (line-start-position new-text start)))
      ;; On non-blank line: delete following blank lines
      (let* ((end (let loop ((i (+ line 1)))
                    (if (and (< i (length lines))
                             (string=? (string-trim (list-ref lines i)) ""))
                      (loop (+ i 1)) i)))
             (new-lines (append
                          (let loop ((i 0) (acc []))
                            (if (> i line) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))
                          (let loop ((i end) (acc []))
                            (if (>= i (length lines)) (reverse acc)
                              (loop (+ i 1) (cons (list-ref lines i) acc))))))
             (new-text (string-join new-lines "\n")))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed
          (min (qt-plain-text-edit-cursor-position ed) (string-length new-text)))))))

;;;============================================================================
;;; Insert file
;;;============================================================================

(def (cmd-insert-file app)
  (let* ((echo (app-state-echo app))
         (filename (qt-echo-read-string app "Insert file: ")))
    (when (and filename (> (string-length filename) 0))
      (let ((filename (expand-filename filename)))
        (if (file-exists? filename)
          (let ((content (read-file-as-string filename)))
            (when content
              (qt-plain-text-edit-insert-text! (current-qt-editor app) content)
              (echo-message! echo (string-append "Inserted " filename))))
          (echo-error! echo (string-append "File not found: " filename)))))))

;;;============================================================================
;;; Shell command (M-!)
;;;============================================================================

(def (cmd-shell-command app)
  (let* ((echo (app-state-echo app))
         (cmd (qt-echo-read-string app "Shell command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (echo-message! echo (string-append "Running... (C-g to cancel)"))
      (when *qt-app-ptr* (qt-app-process-events! *qt-app-ptr*))
      (let-values (((result _status)
                    (gsh-run-command/qt
                      cmd (lambda () (when *qt-app-ptr*
                                      (qt-app-process-events! *qt-app-ptr*))))))
        (let* ((fr (app-state-frame app))
               (ed (current-qt-editor app))
               (buf (or (buffer-by-name "*Shell Output*")
                        (qt-buffer-create! "*Shell Output*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0)
          (echo-message! echo (string-append "Shell: " cmd)))))))

;;;============================================================================
;;; Sort lines (in region)
;;;============================================================================

(def (cmd-sort-lines app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (sorted (sort lines string<?))
             (new-region (string-join sorted "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app)
          (string-append "Sorted " (number->string (length lines)) " lines")))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Goto matching paren
;;;============================================================================

(def (cmd-goto-matching-paren app)
  (let* ((ed (current-qt-editor app))
         (pos (qt-plain-text-edit-cursor-position ed))
         (text (qt-plain-text-edit-text ed))
         (len (string-length text)))
    (if (>= pos len)
      (echo-message! (app-state-echo app) "No paren at point")
      (let ((ch (string-ref text pos)))
        (cond
          ;; Opening: scan forward
          ((or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
           (let ((close (cond ((char=? ch #\() #\))
                              ((char=? ch #\[) #\])
                              (else #\}))))
             (let loop ((i (+ pos 1)) (depth 1))
               (cond ((>= i len)
                      (echo-message! (app-state-echo app) "No matching paren"))
                     ((char=? (string-ref text i) ch)
                      (loop (+ i 1) (+ depth 1)))
                     ((char=? (string-ref text i) close)
                      (if (= depth 1)
                        (begin
                          (qt-plain-text-edit-set-cursor-position! ed i)
                          (qt-plain-text-edit-ensure-cursor-visible! ed))
                        (loop (+ i 1) (- depth 1))))
                     (else (loop (+ i 1) depth))))))
          ;; Closing: scan backward
          ((or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
           (let ((open (cond ((char=? ch #\)) #\()
                             ((char=? ch #\]) #\[)
                             (else #\{))))
             (let loop ((i (- pos 1)) (depth 1))
               (cond ((< i 0)
                      (echo-message! (app-state-echo app) "No matching paren"))
                     ((char=? (string-ref text i) ch)
                      (loop (- i 1) (+ depth 1)))
                     ((char=? (string-ref text i) open)
                      (if (= depth 1)
                        (begin
                          (qt-plain-text-edit-set-cursor-position! ed i)
                          (qt-plain-text-edit-ensure-cursor-visible! ed))
                        (loop (- i 1) (- depth 1))))
                     (else (loop (- i 1) depth))))))
          (else
           (echo-message! (app-state-echo app) "No paren at point")))))))

;;;============================================================================
;;; Upcase / downcase region
;;;============================================================================

(def (cmd-upcase-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (new-text (string-append (substring text 0 start)
                                      (string-upcase region)
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

(def (cmd-downcase-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (new-text (string-append (substring text 0 start)
                                      (string-downcase region)
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed pos)
        (set! (buffer-mark buf) #f))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Indent region
;;;============================================================================

(def (cmd-indent-region app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let* ((pos (qt-plain-text-edit-cursor-position ed))
             (start (min mark pos))
             (end (max mark pos))
             (text (qt-plain-text-edit-text ed))
             (region (substring text start end))
             (lines (string-split region #\newline))
             (indented (map (lambda (l) (string-append "  " l)) lines))
             (new-region (string-join indented "\n"))
             (new-text (string-append (substring text 0 start)
                                      new-region
                                      (substring text end (string-length text)))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed start)
        (set! (buffer-mark buf) #f)
        (echo-message! (app-state-echo app) "Indented region"))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Zap to char
;;;============================================================================

(def (cmd-zap-to-char app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Zap to char: ")))
    (when (and input (> (string-length input) 0))
      (let* ((ch (string-ref input 0))
             (ed (current-qt-editor app))
             (pos (qt-plain-text-edit-cursor-position ed))
             (text (qt-plain-text-edit-text ed))
             (len (string-length text)))
        (let loop ((i (+ pos 1)))
          (cond
            ((>= i len)
             (echo-error! echo (string-append "Char not found: " (string ch))))
            ((char=? (string-ref text i) ch)
             (let* ((killed (substring text pos (+ i 1)))
                    (new-text (string-append (substring text 0 pos)
                                             (substring text (+ i 1) len))))
               (set! (app-state-kill-ring app)
                     (cons killed (app-state-kill-ring app)))
               (qt-plain-text-edit-set-text! ed new-text)
               (qt-plain-text-edit-set-cursor-position! ed pos)))
            (else (loop (+ i 1)))))))))

;;;============================================================================
;;; Goto char position
;;;============================================================================

(def (cmd-goto-char app)
  (let* ((echo (app-state-echo app))
         (input (qt-echo-read-string app "Goto char: ")))
    (when (and input (> (string-length input) 0))
      (let ((n (string->number input)))
        (if (and n (>= n 0))
          (let* ((ed (current-qt-editor app))
                 (text (qt-plain-text-edit-text ed))
                 (target (min n (string-length text))))
            (qt-plain-text-edit-set-cursor-position! ed target)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-error! echo "Invalid position"))))))

;;;============================================================================
;;; Zoom reset
;;;============================================================================

(def (cmd-zoom-reset app)
  (qt-widget-set-font-size! (current-qt-editor app) 10)
  (echo-message! (app-state-echo app) "Zoom reset"))

;;;============================================================================
;;; Yank pop (cycle kill ring)
;;;============================================================================

(def (cmd-yank-pop app)
  (if (and (app-state-last-yank-pos app)
           (memq (app-state-last-command app) '(yank yank-pop)))
    (let ((ring (app-state-kill-ring app)))
      (if (null? ring)
        (echo-message! (app-state-echo app) "Kill ring is empty")
        (let* ((ring-len (length ring))
               (idx (modulo (+ (app-state-kill-ring-idx app) 1) ring-len))
               (replacement (list-ref ring idx))
               (ed (current-qt-editor app))
               (full-text (qt-plain-text-edit-text ed))
               (yank-pos (app-state-last-yank-pos app))
               (yank-len (app-state-last-yank-len app))
               (new-text (string-append
                           (substring full-text 0 yank-pos)
                           replacement
                           (substring full-text (+ yank-pos yank-len)
                                     (string-length full-text)))))
          (qt-plain-text-edit-set-text! ed new-text)
          (qt-plain-text-edit-set-cursor-position! ed
            (+ yank-pos (string-length replacement)))
          (set! (app-state-kill-ring-idx app) idx)
          (set! (app-state-last-yank-len app) (string-length replacement)))))
    (echo-message! (app-state-echo app) "Previous command was not a yank")))

;;;============================================================================
;;; Occur (show matching lines)
;;;============================================================================

;; Occur state: remember source buffer for jumping
(def *occur-source-buffer* #f) ; buffer name that occur was run on

(def (cmd-occur app)
  (let* ((echo (app-state-echo app))
         (source-buf (current-qt-buffer app))
         (query (qt-echo-read-string app "Occur: ")))
    (when (and query (> (string-length query) 0))
      (let* ((ed (current-qt-editor app))
             (text (qt-plain-text-edit-text ed))
             (lines (string-split text #\newline))
             (matches (let loop ((ls lines) (i 1) (acc []))
                        (if (null? ls) (reverse acc)
                          (if (string-contains (car ls) query)
                            (loop (cdr ls) (+ i 1)
                                  (cons (string-append (number->string i) ": " (car ls))
                                        acc))
                            (loop (cdr ls) (+ i 1) acc)))))
             (result (if (null? matches)
                       (string-append "No matches for: " query)
                       (string-append (number->string (length matches))
                                      " matches for \"" query
                                      "\" in " (buffer-name source-buf) "\n\n"
                                      (string-join matches "\n")
                                      "\n\nPress Enter on a line to jump to it."))))
        (set! *occur-source-buffer* (buffer-name source-buf))
        (let* ((fr (app-state-frame app))
               (buf (or (buffer-by-name "*Occur*")
                        (qt-buffer-create! "*Occur*" ed #f))))
          (qt-buffer-attach! ed buf)
          (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
          (qt-plain-text-edit-set-text! ed result)
          (qt-text-document-set-modified! (buffer-doc-pointer buf) #f)
          (qt-plain-text-edit-set-cursor-position! ed 0))))))

(def (cmd-occur-goto app)
  "Jump from *Occur* buffer to the source line under cursor."
  (let* ((buf (current-qt-buffer app))
         (echo (app-state-echo app)))
    (if (not (string=? (buffer-name buf) "*Occur*"))
      (echo-error! echo "Not in *Occur* buffer")
      (if (not *occur-source-buffer*)
        (echo-error! echo "No occur source buffer")
        (let* ((ed (current-qt-editor app))
               (text (qt-plain-text-edit-text ed))
               ;; Get current line text
               (pos (qt-plain-text-edit-cursor-position ed))
               (line-start (let scan ((i (max 0 (- pos 1))))
                             (cond
                               ((<= i 0) 0)
                               ((char=? (string-ref text i) #\newline) (+ i 1))
                               (else (scan (- i 1))))))
               (line-end (let scan ((i pos))
                           (cond
                             ((>= i (string-length text)) i)
                             ((char=? (string-ref text i) #\newline) i)
                             (else (scan (+ i 1))))))
               (line (substring text line-start line-end)))
          ;; Parse line number from "NNN: text" format
          (let ((colon-pos (string-index line #\:)))
            (if (not colon-pos)
              (echo-error! echo "Not on an occur match line")
              (let ((line-num-str (substring line 0 colon-pos)))
                (let ((line-num (string->number line-num-str)))
                  (if (not line-num)
                    (echo-error! echo "Not on an occur match line")
                    ;; Switch to source buffer and jump to line
                    (let ((source (buffer-by-name *occur-source-buffer*)))
                      (if (not source)
                        (echo-error! echo
                          (string-append "Source buffer '" *occur-source-buffer* "' not found"))
                        (let* ((fr (app-state-frame app)))
                          (qt-buffer-attach! ed source)
                          (set! (qt-edit-window-buffer (qt-current-window fr)) source)
                          (let* ((src-text (qt-plain-text-edit-text ed))
                                 (target-pos (text-line-position src-text line-num)))
                            (qt-plain-text-edit-set-cursor-position! ed target-pos)
                            (qt-plain-text-edit-ensure-cursor-visible! ed)
                            (echo-message! echo
                              (string-append "Line " (number->string line-num)))))))))))))))))


;;;============================================================================
;;; Keyboard macros
;;;============================================================================

(def (cmd-start-kbd-macro app)
  (set! (app-state-macro-recording app) '())
  (echo-message! (app-state-echo app) "Defining keyboard macro..."))

(def (cmd-end-kbd-macro app)
  (if (app-state-macro-recording app)
    (begin
      (set! (app-state-macro-last app)
            (reverse (app-state-macro-recording app)))
      (set! (app-state-macro-recording app) #f)
      (echo-message! (app-state-echo app) "Keyboard macro defined"))
    (echo-error! (app-state-echo app) "Not defining a macro")))

(def (cmd-call-last-kbd-macro app)
  (let ((macro (app-state-macro-last app)))
    (if macro
      (let ((n (get-prefix-arg app)))
        (let loop ((i 0))
          (when (< i n)
            (for-each
              (lambda (event)
                (case (car event)
                  ((self-insert)
                   (qt-plain-text-edit-insert-text!
                     (current-qt-editor app) (cdr event)))
                  ((command)
                   (execute-command! app (cdr event)))))
              macro)
            (loop (+ i 1)))))
      (echo-error! (app-state-echo app) "No keyboard macro defined"))))

(def (cmd-name-last-kbd-macro app)
  "Give a name to the last recorded keyboard macro."
  (let ((macro (app-state-macro-last app)))
    (if (or (not macro) (null? macro))
      (echo-error! (app-state-echo app) "No macro defined")
      (let ((name (qt-echo-read-string app "Name for macro: ")))
        (when (and name (> (string-length name) 0))
          (hash-put! (app-state-macro-named app) name macro)
          (echo-message! (app-state-echo app)
            (string-append "Macro saved as '" name "'")))))))

(def (cmd-call-named-kbd-macro app)
  "Execute a named keyboard macro."
  (let* ((named (app-state-macro-named app))
         (names (sort (map car (hash->list named)) string<?)))
    (if (null? names)
      (echo-error! (app-state-echo app) "No named macros")
      (let ((name (qt-echo-read-with-narrowing app "Run macro:" names)))
        (when (and name (> (string-length name) 0))
          (let ((macro (hash-get named name)))
            (if macro
              (for-each
                (lambda (event)
                  (case (car event)
                    ((self-insert)
                     (qt-plain-text-edit-insert-text!
                       (current-qt-editor app) (cdr event)))
                    ((command)
                     (execute-command! app (cdr event)))))
                macro)
              (echo-error! (app-state-echo app)
                (string-append "No macro named '" name "'")))))))))

(def (cmd-list-kbd-macros app)
  "List all named keyboard macros."
  (let* ((named (app-state-macro-named app))
         (names (sort (map car (hash->list named)) string<?)))
    (if (null? names)
      (echo-message! (app-state-echo app) "No named macros")
      (echo-message! (app-state-echo app)
        (string-append "Macros: " (string-join names ", "))))))

(def (cmd-save-kbd-macros app)
  "Save named keyboard macros to ~/.gemacs-macros."
  (let ((named (app-state-macro-named app)))
    (if (= (hash-length named) 0)
      (echo-message! (app-state-echo app) "No macros to save")
      (with-catch
        (lambda (e) (echo-error! (app-state-echo app) "Error saving macros"))
        (lambda ()
          (let ((path (path-expand ".gemacs-macros"
                        (user-info-home (user-info (user-name))))))
            (call-with-output-file path
              (lambda (port)
                (for-each
                  (lambda (pair)
                    (write (cons (car pair) (cdr pair)) port)
                    (newline port))
                  (hash->list named)))))
          (echo-message! (app-state-echo app)
            (string-append "Saved " (number->string (hash-length named)) " macros")))))))

(def (cmd-load-kbd-macros app)
  "Load named keyboard macros from ~/.gemacs-macros."
  (with-catch
    (lambda (e) (echo-message! (app-state-echo app) "No saved macros found"))
    (lambda ()
      (let* ((path (path-expand ".gemacs-macros"
                     (user-info-home (user-info (user-name)))))
             (named (app-state-macro-named app)))
        (call-with-input-file path
          (lambda (port)
            (let loop ()
              (let ((datum (read port)))
                (when (not (eof-object? datum))
                  (when (pair? datum)
                    (hash-put! named (car datum) (cdr datum)))
                  (loop))))))
        (echo-message! (app-state-echo app)
          (string-append "Loaded " (number->string (hash-length named)) " macros"))))))

(def (cmd-insert-kbd-macro app)
  "Insert the last keyboard macro as text."
  (let ((macro (app-state-macro-last app)))
    (if macro
      (let* ((ed (current-qt-editor app))
             (text (with-output-to-string
                     (lambda ()
                       (for-each (lambda (entry)
                                   (display "(") (display (car entry))
                                   (display " . ") (display (cdr entry))
                                   (display ")\n"))
                                 macro)))))
        (qt-plain-text-edit-insert-text! ed text))
      (echo-error! (app-state-echo app) "No macro recorded"))))

;;;============================================================================
;;; Repeat last command
;;;============================================================================

(def (cmd-repeat app)
  (let ((last (app-state-last-command app)))
    (if (and last (not (eq? last 'repeat)))
      (execute-command! app last)
      (echo-message! (app-state-echo app) "No command to repeat"))))

;;;============================================================================
;;; Pop mark (jump to mark ring)
;;;============================================================================

(def (cmd-pop-mark app)
  (let ((ring (app-state-mark-ring app)))
    (if (pair? ring)
      (let* ((entry (car ring))
             (buf-name (car entry))
             (position (cdr entry))
             (buf (buffer-by-name buf-name)))
        (set! (app-state-mark-ring app) (cdr ring))
        (if buf
          (let* ((fr (app-state-frame app))
                 (ed (current-qt-editor app)))
            (qt-buffer-attach! ed buf)
            (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
            (qt-plain-text-edit-set-cursor-position! ed position)
            (qt-plain-text-edit-ensure-cursor-visible! ed))
          (echo-error! (app-state-echo app)
                       (string-append "Buffer not found: " buf-name))))
      (echo-message! (app-state-echo app) "Mark ring is empty"))))

;;;============================================================================
;;; Registers
;;;============================================================================

(def (cmd-copy-to-register app)
  (let* ((input (qt-echo-read-string app "Copy to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (ed (current-qt-editor app))
             (buf (current-qt-buffer app))
             (mark (buffer-mark buf)))
        (if mark
          (let* ((pos (qt-plain-text-edit-cursor-position ed))
                 (start (min mark pos))
                 (end (max mark pos))
                 (text (qt-plain-text-edit-text ed))
                 (region (substring text start end)))
            (hash-put! (app-state-registers app) reg region)
            (set! (buffer-mark buf) #f)
            (echo-message! echo
              (string-append "Copied to register " (string reg))))
          (echo-error! echo "No mark set"))))))

(def (cmd-insert-register app)
  (let* ((input (qt-echo-read-string app "Insert register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (val (hash-get (app-state-registers app) reg)))
        (cond
          ((string? val)
           (qt-plain-text-edit-insert-text! (current-qt-editor app) val))
          ((pair? val)
           (echo-error! echo "Register contains a position, not text"))
          (else
           (echo-error! echo
             (string-append "Register " (string reg) " is empty"))))))))

(def (cmd-point-to-register app)
  (let* ((input (qt-echo-read-string app "Point to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (buf (current-qt-buffer app))
             (pos (qt-plain-text-edit-cursor-position (current-qt-editor app))))
        (hash-put! (app-state-registers app) reg
                   (cons (buffer-name buf) pos))
        (echo-message! echo
          (string-append "Saved point to register " (string reg)))))))

(def (cmd-jump-to-register app)
  (let* ((input (qt-echo-read-string app "Jump to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (val (hash-get (app-state-registers app) reg)))
        (cond
          ;; Window configuration register
          ((window-config? val)
           (let* ((fr (app-state-frame app))
                  (entries (window-config-windows val))
                  (target-idx (window-config-current-idx val)))
             ;; Delete all windows except current
             (qt-frame-delete-other-windows! fr)
             ;; Open first buffer in existing window
             (when (pair? entries)
               (let* ((first-entry (car entries))
                      (bname (car first-entry))
                      (pos (caddr first-entry))
                      (buf (buffer-by-name bname))
                      (ed (qt-current-editor fr)))
                 (when buf
                   (qt-buffer-attach! ed buf)
                   (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                   (qt-plain-text-edit-set-cursor-position! ed
                     (min pos (string-length (qt-plain-text-edit-text ed))))
                   (qt-plain-text-edit-ensure-cursor-visible! ed)))
               ;; Split and set up remaining windows
               (let loop ((rest (cdr entries)))
                 (when (pair? rest)
                   (let* ((entry (car rest))
                          (bname (car entry))
                          (pos (caddr entry))
                          (new-ed (qt-frame-split! fr))
                          (buf (buffer-by-name bname)))
                     (when buf
                       (qt-buffer-attach! new-ed buf)
                       (let ((new-win (car (reverse (qt-frame-windows fr)))))
                         (set! (qt-edit-window-buffer new-win) buf))
                       (qt-plain-text-edit-set-cursor-position! new-ed
                         (min pos (string-length (qt-plain-text-edit-text new-ed))))
                       (qt-plain-text-edit-ensure-cursor-visible! new-ed))
                     ;; Install key handler on new editor
                     (when (app-state-key-handler app)
                       ((app-state-key-handler app) new-ed))
                     (loop (cdr rest)))))
               ;; Restore active window index
               (when (< target-idx (length (qt-frame-windows fr)))
                 (set! (qt-frame-current-idx fr) target-idx)))
             (echo-message! echo "Window configuration restored")))
          ;; File register (file . path)
          ((and (pair? val) (eq? (car val) 'file))
           (let ((path (cdr val)))
             (if (file-exists? path)
               (let* ((name (path-strip-directory path))
                      (fr (app-state-frame app))
                      (ed (current-qt-editor app))
                      ;; Reuse existing buffer or create new
                      (existing (buffer-by-name name))
                      (buf (or existing
                               (qt-buffer-create! name ed path))))
                 (qt-buffer-attach! ed buf)
                 (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                 (unless existing
                   (let ((text (read-file-as-string path)))
                     (when text
                       (qt-plain-text-edit-set-text! ed text)
                       (qt-text-document-set-modified!
                         (buffer-doc-pointer buf) #f)
                       (qt-plain-text-edit-set-cursor-position! ed 0))))
                 (echo-message! echo (string-append "Opened file: " path)))
               (echo-error! echo
                 (string-append "File not found: " path)))))
          ;; Point register (buffer-name . position)
          ((pair? val)
           (let ((buf (buffer-by-name (car val))))
             (if buf
               (let* ((fr (app-state-frame app))
                      (ed (current-qt-editor app)))
                 (qt-buffer-attach! ed buf)
                 (set! (qt-edit-window-buffer (qt-current-window fr)) buf)
                 (qt-plain-text-edit-set-cursor-position! ed (cdr val))
                 (qt-plain-text-edit-ensure-cursor-visible! ed))
               (echo-error! echo
                 (string-append "Buffer not found: " (car val))))))
          ((string? val)
           ;; Text register — insert at point
           (qt-plain-text-edit-insert-text! (current-qt-editor app) val))
          (else
           (echo-error! echo
             (string-append "Register " (string reg) " is empty"))))))))

;;;============================================================================
;;; Window configuration registers
;;;============================================================================

(defstruct window-config (windows current-idx) transparent: #t)
;; windows: list of (buffer-name file-path cursor-pos)

(def (cmd-window-configuration-to-register app)
  "Save current window layout to a register."
  (let* ((input (qt-echo-read-string app "Window config to register: "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (fr (app-state-frame app))
             (windows (qt-frame-windows fr))
             (entries
               (map (lambda (win)
                      (let* ((buf (qt-edit-window-buffer win))
                             (ed (qt-edit-window-editor win))
                             (pos (qt-plain-text-edit-cursor-position ed)))
                        (list (buffer-name buf)
                              (buffer-file-path buf)
                              pos)))
                    windows))
             (cfg (make-window-config entries (qt-frame-current-idx fr))))
        (hash-put! (app-state-registers app) reg cfg)
        (echo-message! echo
          (string-append "Window config saved to register " (string reg)))))))

(def (cmd-file-to-register app)
  "Save a file path to a register for quick jumping."
  (let* ((input (qt-echo-read-string app "File to register (register char): "))
         (echo (app-state-echo app)))
    (when (and input (> (string-length input) 0))
      (let* ((reg (string-ref input 0))
             (buf (current-qt-buffer app))
             (path (buffer-file-path buf)))
        (if path
          (begin
            (hash-put! (app-state-registers app) reg (cons 'file path))
            (echo-message! echo
              (string-append "File " path " saved to register " (string reg))))
          (echo-error! echo "Buffer has no file"))))))

;;;============================================================================
;;; Paragraph navigation
;;;============================================================================

(def (cmd-forward-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed))
         (num-lines (length lines)))
    ;; Skip non-blank lines, then skip blank lines
    (let* ((past-text (let loop ((i (+ line 1)))
                        (cond ((>= i num-lines) i)
                              ((string=? (string-trim (list-ref lines i)) "") i)
                              (else (loop (+ i 1))))))
           (target (let loop ((i past-text))
                     (cond ((>= i num-lines) (- num-lines 1))
                           ((not (string=? (string-trim (list-ref lines i)) "")) i)
                           (else (loop (+ i 1)))))))
      (qt-plain-text-edit-set-cursor-position! ed
        (line-start-position text (min target (- num-lines 1))))
      (qt-plain-text-edit-ensure-cursor-visible! ed))))

(def (cmd-backward-paragraph app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    ;; Skip non-blank lines backward, then skip blank lines
    (let* ((past-text (let loop ((i (- line 1)))
                        (cond ((< i 0) 0)
                              ((string=? (string-trim (list-ref lines i)) "") i)
                              (else (loop (- i 1))))))
           (target (let loop ((i past-text))
                     (cond ((<= i 0) 0)
                           ((not (string=? (string-trim (list-ref lines i)) "")) (+ i 1))
                           (else (loop (- i 1)))))))
      (qt-plain-text-edit-set-cursor-position! ed
        (line-start-position text target))
      (qt-plain-text-edit-ensure-cursor-visible! ed))))

;;;============================================================================
;;; Back to indentation
;;;============================================================================

(def (cmd-back-to-indentation app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let* ((line-text (list-ref lines line))
             (indent (let loop ((i 0))
                       (if (and (< i (string-length line-text))
                                (char-whitespace? (string-ref line-text i)))
                         (loop (+ i 1)) i)))
             (line-pos (line-start-position text line)))
        (qt-plain-text-edit-set-cursor-position! ed (+ line-pos indent))))))

;;;============================================================================
;;; Delete indentation (join with previous line)
;;;============================================================================

(def (cmd-delete-indentation app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (> line 0)
      (let* ((prev (string-trim-right (list-ref lines (- line 1))))
             (curr (string-trim (list-ref lines line)))
             (joined (string-append prev " " curr))
             (new-lines (let loop ((ls lines) (i 0) (acc []))
                          (cond ((null? ls) (reverse acc))
                                ((= i (- line 1))
                                 (loop (if (pair? (cdr ls)) (cddr ls) '())
                                       (+ i 2) (cons joined acc)))
                                (else (loop (cdr ls) (+ i 1) (cons (car ls) acc))))))
             (new-text (string-join new-lines "\n"))
             (join-pos (+ (line-start-position new-text (- line 1))
                          (string-length prev))))
        (qt-plain-text-edit-set-text! ed new-text)
        (qt-plain-text-edit-set-cursor-position! ed join-pos)))))

;;;============================================================================
;;; Exchange point and mark
;;;============================================================================

(def (cmd-exchange-point-and-mark app)
  (let* ((ed (current-qt-editor app))
         (buf (current-qt-buffer app))
         (mark (buffer-mark buf)))
    (if mark
      (let ((pos (qt-plain-text-edit-cursor-position ed)))
        (set! (buffer-mark buf) pos)
        (qt-plain-text-edit-set-cursor-position! ed mark)
        (qt-plain-text-edit-ensure-cursor-visible! ed))
      (echo-error! (app-state-echo app) "No mark set"))))

;;;============================================================================
;;; Copy line
;;;============================================================================

(def (cmd-copy-line app)
  (let* ((ed (current-qt-editor app))
         (text (qt-plain-text-edit-text ed))
         (lines (string-split text #\newline))
         (line (qt-plain-text-edit-cursor-line ed)))
    (when (< line (length lines))
      (let ((line-text (list-ref lines line)))
        (set! (app-state-kill-ring app)
              (cons line-text (app-state-kill-ring app)))
        (echo-message! (app-state-echo app) "Line copied")))))

