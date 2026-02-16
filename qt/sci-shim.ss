;;; -*- Gerbil -*-
;;; QScintilla compatibility shim for gerbil-emacs
;;;
;;; Provides qt-plain-text-edit-* compatible functions backed by QScintilla.
;;; Modules import this INSTEAD of :gerbil-qt/qt to get both:
;;;   1) All non-conflicting Qt exports (widgets, layouts, dialogs, etc.)
;;;   2) Scintilla-backed replacements for editor/document/highlight functions
;;;   3) SCI_* constants from :gerbil-scintilla/constants

;;;============================================================================
;;; Exports — re-export Qt and SCI constants, with local definitions overriding
;;;============================================================================

(export #t
  (import: :gerbil-qt/qt)
  (import: :gerbil-scintilla/constants))

;;;============================================================================
;;; Imports
;;;============================================================================

(import (except-in :gerbil-qt/qt
          qt-plain-text-edit-create
          qt-plain-text-edit-set-text!
          qt-plain-text-edit-text
          qt-plain-text-edit-append!
          qt-plain-text-edit-clear!
          qt-plain-text-edit-set-read-only!
          qt-plain-text-edit-read-only?
          qt-plain-text-edit-set-placeholder!
          qt-plain-text-edit-line-count
          qt-plain-text-edit-set-max-block-count!
          qt-plain-text-edit-cursor-line
          qt-plain-text-edit-cursor-column
          qt-plain-text-edit-set-line-wrap!
          qt-on-plain-text-edit-text-changed!
          qt-plain-text-edit-cursor-position
          qt-plain-text-edit-set-cursor-position!
          qt-plain-text-edit-move-cursor!
          qt-plain-text-edit-select-all!
          qt-plain-text-edit-selected-text
          qt-plain-text-edit-selection-start
          qt-plain-text-edit-selection-end
          qt-plain-text-edit-has-selection?
          qt-plain-text-edit-set-selection!
          qt-plain-text-edit-insert-text!
          qt-plain-text-edit-remove-selected-text!
          qt-plain-text-edit-undo!
          qt-plain-text-edit-redo!
          qt-plain-text-edit-can-undo?
          qt-plain-text-edit-cut!
          qt-plain-text-edit-copy!
          qt-plain-text-edit-paste!
          qt-plain-text-edit-text-length
          qt-plain-text-edit-text-range
          qt-plain-text-edit-line-from-position
          qt-plain-text-edit-line-end-position
          qt-plain-text-edit-find-text
          qt-plain-text-edit-ensure-cursor-visible!
          qt-plain-text-edit-center-cursor!
          qt-plain-text-edit-set-document!
          qt-plain-text-edit-document
          qt-plain-text-document-create
          qt-text-document-create
          qt-text-document-destroy!
          qt-text-document-modified?
          qt-text-document-set-modified!
          qt-line-number-area-create
          qt-line-number-area-destroy!
          qt-line-number-area-set-visible!
          qt-line-number-area-set-bg-color!
          qt-line-number-area-set-fg-color!
          qt-syntax-highlighter-create
          qt-syntax-highlighter-destroy!
          qt-syntax-highlighter-add-rule!
          qt-syntax-highlighter-add-keywords!
          qt-syntax-highlighter-add-multiline-rule!
          qt-syntax-highlighter-rehighlight!
          qt-syntax-highlighter-clear-rules!
          qt-extra-selections-clear!
          qt-extra-selection-add-line!
          qt-extra-selection-add-range!
          qt-extra-selections-apply!)
        :gerbil-scintilla/constants
        :gerbil-emacs/core
        :std/sugar)

;;;============================================================================
;;; Constants not in :gerbil-scintilla/constants
;;;============================================================================

;; SCI_REPLACESEL — replaces selection (or inserts at cursor if no selection)
(def SCI_REPLACESEL 2170)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (sci-send sci msg (wparam 0) (lparam 0))
  "Send a Scintilla message to a QScintilla widget."
  (qt-scintilla-send-message sci msg wparam lparam))

(def (sci-send/string sci msg str (wparam 0))
  "Send a Scintilla message with a string parameter."
  (qt-scintilla-send-message/string sci msg str wparam))

(def (sci-recv-string sci msg (wparam 0))
  "Receive a string result from a Scintilla message."
  (qt-scintilla-receive-string sci msg wparam))

(def (rgb->sci r g b)
  "Convert RGB to Scintilla's BGR color format."
  (+ r (* 256 g) (* 65536 b)))

;;;============================================================================
;;; Document ↔ Editor/Buffer tracking
;;;============================================================================

;; Maps doc-pointer → most-recent editor widget
(def *doc-editor-map* (make-hash-table))

;; Maps doc-pointer → buffer struct
(def *doc-buffer-map* (make-hash-table))

(def (doc-editor-register! doc editor)
  (hash-put! *doc-editor-map* doc editor))

(def (doc-buffer-register! doc buf)
  (hash-put! *doc-buffer-map* doc buf))

;;;============================================================================
;;; Editor widget creation
;;;============================================================================

(def (qt-plain-text-edit-create parent: (parent #f))
  "Create a QScintilla widget instead of QPlainTextEdit."
  (qt-scintilla-create parent: parent))

;;;============================================================================
;;; Text operations
;;;============================================================================

(def (qt-plain-text-edit-text sci)
  "Get entire text from QScintilla widget."
  (qt-scintilla-text sci))

(def (qt-plain-text-edit-set-text! sci text)
  "Set entire text in QScintilla widget."
  (qt-scintilla-set-text! sci text))

(def (qt-plain-text-edit-append! sci text)
  "Append text on a new line (matching QPlainTextEdit::appendPlainText behavior)."
  (let ((to-append (string-append "\n" text)))
    (sci-send/string sci SCI_APPENDTEXT to-append
                     (string-length to-append))))

(def (qt-plain-text-edit-clear! sci)
  "Clear all text."
  (sci-send sci SCI_CLEARALL))

(def (qt-plain-text-edit-insert-text! sci text)
  "Insert text at cursor, replacing selection if any."
  (sci-send/string sci SCI_REPLACESEL text))

(def (qt-plain-text-edit-remove-selected-text! sci)
  "Delete the current selection."
  (sci-send/string sci SCI_REPLACESEL ""))

(def (qt-plain-text-edit-text-length sci)
  "Get text length in characters."
  (qt-scintilla-text-length sci))

(def (qt-plain-text-edit-text-range sci start end)
  "Get text between start and end positions."
  (let ((text (qt-scintilla-text sci)))
    (if (and (>= start 0) (<= end (string-length text)) (<= start end))
      (substring text start end)
      "")))

;;;============================================================================
;;; Cursor position
;;;============================================================================

(def (qt-plain-text-edit-cursor-position sci)
  "Get cursor position (byte offset, equals char offset for ASCII)."
  (sci-send sci SCI_GETCURRENTPOS))

(def (qt-plain-text-edit-set-cursor-position! sci pos)
  "Set cursor position and anchor (no selection)."
  (sci-send sci SCI_GOTOPOS pos))

(def (qt-plain-text-edit-cursor-line sci)
  "Get current line number (0-based)."
  (sci-send sci SCI_LINEFROMPOSITION
            (sci-send sci SCI_GETCURRENTPOS)))

(def (qt-plain-text-edit-cursor-column sci)
  "Get current column number (0-based)."
  (sci-send sci SCI_GETCOLUMN
            (sci-send sci SCI_GETCURRENTPOS)))

(def (qt-plain-text-edit-line-count sci)
  "Get total number of lines."
  (sci-send sci SCI_GETLINECOUNT))

(def (qt-plain-text-edit-line-from-position sci pos)
  "Get line number for a byte position."
  (sci-send sci SCI_LINEFROMPOSITION pos))

(def (qt-plain-text-edit-line-end-position sci line)
  "Get byte position of end of a line."
  (sci-send sci SCI_GETLINEENDPOSITION line))

;;;============================================================================
;;; Cursor movement
;;;============================================================================

(def (qt-plain-text-edit-move-cursor! sci op . args)
  "Move cursor using QT_CURSOR_* constants. Optional second arg: QT_KEEP_ANCHOR."
  (let* ((keep-anchor? (and (pair? args) (= (car args) QT_KEEP_ANCHOR)))
         (cur-pos (sci-send sci SCI_GETCURRENTPOS))
         (new-pos
          (cond
            ((= op QT_CURSOR_END)
             (sci-send sci SCI_GETTEXTLENGTH))
            ((= op QT_CURSOR_START)
             0)
            ((or (= op QT_CURSOR_END_OF_LINE) (= op QT_CURSOR_END_OF_BLOCK))
             (let ((line (sci-send sci SCI_LINEFROMPOSITION cur-pos)))
               (sci-send sci SCI_GETLINEENDPOSITION line)))
            ((or (= op QT_CURSOR_START_OF_LINE) (= op QT_CURSOR_START_OF_BLOCK))
             (let ((line (sci-send sci SCI_LINEFROMPOSITION cur-pos)))
               (sci-send sci SCI_POSITIONFROMLINE line)))
            ((= op QT_CURSOR_DOWN)
             (let* ((line (sci-send sci SCI_LINEFROMPOSITION cur-pos))
                    (col (sci-send sci SCI_GETCOLUMN cur-pos))
                    (max-line (- (sci-send sci SCI_GETLINECOUNT) 1))
                    (new-line (min (+ line 1) max-line)))
               (sci-send sci SCI_FINDCOLUMN new-line col)))
            ((= op QT_CURSOR_UP)
             (let* ((line (sci-send sci SCI_LINEFROMPOSITION cur-pos))
                    (col (sci-send sci SCI_GETCOLUMN cur-pos))
                    (new-line (max (- line 1) 0)))
               (sci-send sci SCI_FINDCOLUMN new-line col)))
            ((or (= op QT_CURSOR_RIGHT) (= op QT_CURSOR_NEXT_CHAR))
             (min (+ cur-pos 1) (sci-send sci SCI_GETTEXTLENGTH)))
            ((or (= op QT_CURSOR_LEFT) (= op QT_CURSOR_PREVIOUS_CHAR))
             (max (- cur-pos 1) 0))
            ((or (= op QT_CURSOR_NEXT_WORD) (= op QT_CURSOR_WORD_RIGHT))
             ;; Scan forward past current word, then past whitespace
             (let* ((text (qt-scintilla-text sci))
                    (len (string-length text)))
               (let loop ((i cur-pos) (in-word? #t))
                 (cond
                   ((>= i len) len)
                   ((char-alphabetic? (string-ref text i))
                    (if in-word? (loop (+ i 1) #t) i))
                   ((char-whitespace? (string-ref text i))
                    (loop (+ i 1) #f))
                   (in-word? (loop (+ i 1) #f))
                   (else i)))))
            ((or (= op QT_CURSOR_PREVIOUS_WORD) (= op QT_CURSOR_WORD_LEFT))
             ;; Scan backward past whitespace, then past word
             (let ((text (qt-scintilla-text sci)))
               (let loop ((i (max (- cur-pos 1) 0)) (in-space? #t))
                 (cond
                   ((<= i 0) 0)
                   ((char-whitespace? (string-ref text i))
                    (if in-space? (loop (- i 1) #t) (+ i 1)))
                   ((char-alphabetic? (string-ref text i))
                    (if in-space? (loop (- i 1) #f) (loop (- i 1) #f)))
                   (in-space? (loop (- i 1) #f))
                   (else (+ i 1))))))
            ((or (= op QT_CURSOR_NEXT_BLOCK) (= op QT_CURSOR_PREVIOUS_BLOCK))
             ;; Next/previous block = next/previous line
             (let* ((line (sci-send sci SCI_LINEFROMPOSITION cur-pos))
                    (delta (if (= op QT_CURSOR_NEXT_BLOCK) 1 -1))
                    (max-line (- (sci-send sci SCI_GETLINECOUNT) 1))
                    (new-line (max 0 (min (+ line delta) max-line))))
               (sci-send sci SCI_POSITIONFROMLINE new-line)))
            (else cur-pos))))
    ;; Apply the movement
    (if keep-anchor?
      ;; Keep anchor — extends selection
      (sci-send sci SCI_SETCURRENTPOS new-pos)
      ;; Move anchor too — no selection
      (sci-send sci SCI_GOTOPOS new-pos))))

(def (qt-plain-text-edit-ensure-cursor-visible! sci)
  "Scroll to make cursor visible."
  (sci-send sci SCI_SCROLLCARET))

(def (qt-plain-text-edit-center-cursor! sci)
  "Center the cursor in the view."
  (sci-send sci SCI_SCROLLCARET))

;;;============================================================================
;;; Selection
;;;============================================================================

(def (qt-plain-text-edit-select-all! sci)
  "Select all text."
  (sci-send sci SCI_SELECTALL))

(def (qt-plain-text-edit-selected-text sci)
  "Get currently selected text."
  (sci-recv-string sci SCI_GETSELTEXT))

(def (qt-plain-text-edit-selection-start sci)
  "Get selection start position."
  (sci-send sci SCI_GETSELECTIONSTART))

(def (qt-plain-text-edit-selection-end sci)
  "Get selection end position."
  (sci-send sci SCI_GETSELECTIONEND))

(def (qt-plain-text-edit-has-selection? sci)
  "Check if there is an active selection."
  (not (= (sci-send sci SCI_GETSELECTIONSTART)
           (sci-send sci SCI_GETSELECTIONEND))))

(def (qt-plain-text-edit-set-selection! sci start end)
  "Set selection range."
  (sci-send sci SCI_SETSEL start end))

;;;============================================================================
;;; Undo/Redo/Clipboard
;;;============================================================================

(def (qt-plain-text-edit-undo! sci)
  (sci-send sci SCI_UNDO))

(def (qt-plain-text-edit-redo! sci)
  (sci-send sci SCI_REDO))

(def (qt-plain-text-edit-can-undo? sci)
  (not (= 0 (sci-send sci SCI_CANUNDO))))

(def (qt-plain-text-edit-cut! sci)
  (sci-send sci SCI_CUT))

(def (qt-plain-text-edit-copy! sci)
  (sci-send sci SCI_COPY))

(def (qt-plain-text-edit-paste! sci)
  (sci-send sci SCI_PASTE))

;;;============================================================================
;;; Read-only, wrap, placeholder
;;;============================================================================

(def (qt-plain-text-edit-set-read-only! sci ro?)
  "Set read-only on the DOCUMENT (SCI_SETREADONLY), not the widget.
   Widget-level setReadOnly() persists across buffer switches, breaking
   typing in buffers viewed after a read-only buffer."
  (sci-send sci SCI_SETREADONLY (if ro? 1 0)))

(def (qt-plain-text-edit-read-only? sci)
  (not (zero? (sci-send sci SCI_GETREADONLY))))

(def (qt-plain-text-edit-set-line-wrap! sci wrap?)
  (sci-send sci SCI_SETWRAPMODE
            (if wrap? SC_WRAP_WORD SC_WRAP_NONE)))

(def (qt-plain-text-edit-set-placeholder! sci text)
  ;; No direct Scintilla equivalent — no-op
  (void))

(def (qt-plain-text-edit-set-max-block-count! sci count)
  ;; No direct Scintilla equivalent — no-op
  (void))

;;;============================================================================
;;; Text change signal
;;;============================================================================

(def (qt-on-plain-text-edit-text-changed! sci handler)
  "Register text change handler on QScintilla widget."
  (qt-on-scintilla-text-changed! sci handler))

;;;============================================================================
;;; Search
;;;============================================================================

(def (qt-plain-text-edit-find-text sci text . flags)
  "Search for text. Returns position or -1. Flags: QT_FIND_CASE_SENSITIVE etc."
  (let* ((pos (sci-send sci SCI_GETCURRENTPOS))
         (len (sci-send sci SCI_GETTEXTLENGTH))
         (sci-flags 0))
    ;; Set up search target: from cursor to end (or start if backward)
    (sci-send sci SCI_SETTARGETSTART pos)
    (sci-send sci SCI_SETTARGETEND len)
    (sci-send sci SCI_SETSEARCHFLAGS sci-flags)
    (let ((found (sci-send/string sci SCI_SEARCHINTARGET text
                                  (string-length text))))
      (if (>= found 0) found -1))))

;;;============================================================================
;;; Document management (Scintilla document model)
;;;============================================================================

(def (qt-plain-text-edit-set-document! sci doc)
  "Set the Scintilla document pointer on the editor."
  (sci-send sci SCI_SETDOCPOINTER 0 doc))

(def (qt-plain-text-edit-document sci)
  "Get the current Scintilla document pointer."
  (sci-send sci SCI_GETDOCPOINTER))

(def (qt-plain-text-document-create)
  "No-op — Scintilla documents are created via SCI_CREATEDOCUMENT."
  #f)

(def (qt-text-document-create)
  "No-op — Scintilla documents are created via SCI_CREATEDOCUMENT."
  #f)

(def (qt-text-document-destroy! doc)
  "No-op — Scintilla documents are released via SCI_RELEASEDOCUMENT."
  (void))

(def (qt-text-document-modified? doc)
  "Check if buffer is modified (tracked via save-point signals)."
  (let ((buf (hash-get *doc-buffer-map* doc)))
    (if buf
      (buffer-modified buf)
      #f)))

(def (qt-text-document-set-modified! doc modified?)
  "Set buffer modified state. When clearing, sends SCI_SETSAVEPOINT."
  (let ((buf (hash-get *doc-buffer-map* doc)))
    (when buf
      (set! (buffer-modified buf) modified?)
      (unless modified?
        ;; Tell Scintilla about the save point
        (let ((ed (hash-get *doc-editor-map* doc)))
          (when ed
            (sci-send ed SCI_SETSAVEPOINT)))))))

;;;============================================================================
;;; Line number area — no-ops (QScintilla has built-in margins)
;;;============================================================================

(def (qt-line-number-area-create editor) #f)
(def (qt-line-number-area-destroy! lna) (void))
(def (qt-line-number-area-set-visible! lna visible?) (void))
(def (qt-line-number-area-set-bg-color! lna r g b) (void))
(def (qt-line-number-area-set-fg-color! lna r g b) (void))

;;;============================================================================
;;; Syntax highlighter — no-ops (replaced by Scintilla lexers in highlight.ss)
;;;============================================================================

(def (qt-syntax-highlighter-create doc) #f)
(def (qt-syntax-highlighter-destroy! h) (void))
(def (qt-syntax-highlighter-add-rule! h pattern r g b bold? italic?) (void))
(def (qt-syntax-highlighter-add-keywords! h keywords r g b bold? italic?) (void))
(def (qt-syntax-highlighter-add-multiline-rule! h start end r g b bold? italic?) (void))
(def (qt-syntax-highlighter-rehighlight! h) (void))
(def (qt-syntax-highlighter-clear-rules! h) (void))

;;;============================================================================
;;; Extra selections — Scintilla indicators
;;;============================================================================

;; Indicator IDs for different visual decorations
(def *indic-current-line* 8)
(def *indic-brace-match*  9)
(def *indic-brace-bad*   10)
(def *indic-search*      11)

;; Pending decorations accumulated before apply
(def *pending-decorations* [])

(def (qt-extra-selections-clear! sci)
  "Clear all indicator-based decorations."
  (set! *pending-decorations* [])
  (let ((len (sci-send sci SCI_GETTEXTLENGTH)))
    ;; Clear all our indicator ranges
    (for-each
      (lambda (indic)
        (sci-send sci SCI_SETINDICATORCURRENT indic)
        (sci-send sci SCI_INDICATORCLEARRANGE 0 len))
      (list *indic-current-line* *indic-brace-match*
            *indic-brace-bad* *indic-search*))))

(def (qt-extra-selection-add-line! sci line bg-r bg-g bg-b)
  "Highlight a line using Scintilla's caret line feature."
  ;; Use built-in caret line highlight instead of indicators
  (sci-send sci SCI_SETCARETLINEVISIBLE 1)
  (sci-send sci SCI_SETCARETLINEBACK (rgb->sci bg-r bg-g bg-b)))

(def (qt-extra-selection-add-range! sci pos len
       fg-r fg-g fg-b bg-r bg-g bg-b bold: (bold? #f))
  "Queue a range highlight using Scintilla indicators."
  (set! *pending-decorations*
    (cons (list pos len fg-r fg-g fg-b bg-r bg-g bg-b bold?)
          *pending-decorations*)))

(def (qt-extra-selections-apply! sci)
  "Apply all queued indicator decorations."
  (for-each
    (lambda (dec)
      (let ((pos (car dec))
            (len (cadr dec))
            (bg-r (list-ref dec 5))
            (bg-g (list-ref dec 6))
            (bg-b (list-ref dec 7)))
        ;; Use brace-match indicator for brace highlights,
        ;; search indicator for others
        (sci-send sci SCI_SETINDICATORCURRENT *indic-brace-match*)
        (sci-send sci SCI_INDICSETSTYLE *indic-brace-match* INDIC_ROUNDBOX)
        (sci-send sci SCI_INDICSETFORE *indic-brace-match*
                  (rgb->sci bg-r bg-g bg-b))
        (sci-send sci SCI_INDICATORFILLRANGE pos len)))
    *pending-decorations*)
  (set! *pending-decorations* []))
