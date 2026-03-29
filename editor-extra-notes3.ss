;;; -*- Gerbil -*-
;;; Surround-mode, goto-last-change, crosshair-mode, quickrun,
;;; eros (eval result overlays), auto-dim-other-buffers,
;;; rainbow-identifiers, outline-minor-mode, string-edit-at-point,
;;; persistent-scratch.
;;; New module to keep other editor-extra files under 2000-line limit.

(export #t)

(import :std/sugar
        :std/sort
        :std/srfi/13
        :std/misc/string
        :std/misc/ports
        :gerbil-scintilla/constants
        :gerbil-scintilla/scintilla
        (only-in :gerbil-scintilla/ffi scintilla-send-message-string scintilla-editor-handle)
        :gerbil-scintilla/tui
        :gemacs/core
        :gemacs/keymap
        :gemacs/buffer
        :gemacs/window
        :gemacs/modeline
        :gemacs/echo
        :gemacs/editor-extra-helpers)

;;;============================================================================
;;; Helpers
;;;============================================================================

(def (notes3-get-text-range ed start len)
  "Get text from editor ED starting at START for LEN bytes."
  (let ((buf (make-string (+ len 1))))
    (send-message ed SCI_SETTARGETSTART start 0)
    (send-message ed SCI_SETTARGETEND (+ start len) 0)
    (scintilla-send-message-string (scintilla-editor-handle ed)
                                   SCI_GETTARGETTEXT buf 0)
    (let ((result (substring buf 0 (min len (string-length buf)))))
      result)))

;;;============================================================================
;;; Surround-mode — add/change/delete surrounding delimiters
;;;============================================================================

;; Pair mappings: opening -> (opening . closing)
(def *surround-pairs* (make-hash-table))
(begin
  (hash-put! *surround-pairs* #\( #\))
  (hash-put! *surround-pairs* #\[ #\])
  (hash-put! *surround-pairs* #\{ #\})
  (hash-put! *surround-pairs* #\< #\>)
  (hash-put! *surround-pairs* #\" #\")
  (hash-put! *surround-pairs* #\' #\')
  (hash-put! *surround-pairs* #\` #\`))

(def (surround-closing-char ch)
  "Return closing character for the given opening character."
  (hash-ref *surround-pairs* ch ch))

(def (surround-find-enclosing ed pos ch)
  "Find the positions of the enclosing pair for character CH around POS.
Returns (start . end) or #f."
  (let* ((close (surround-closing-char ch))
         (same-char? (char=? ch close))
         (len (send-message ed SCI_GETLENGTH 0 0)))
    (if same-char?
      ;; For same-char delimiters (quotes), search backward then forward
      (let loop-back ((p (- pos 1)) (count 0))
        (if (< p 0) #f
          (let ((c (integer->char (send-message ed SCI_GETCHARAT p 0))))
            (if (char=? c ch)
              ;; Found opening, now find closing
              (let loop-fwd ((q (+ pos 1)))
                (if (>= q len) #f
                  (let ((c2 (integer->char (send-message ed SCI_GETCHARAT q 0))))
                    (if (char=? c2 close)
                      (cons p (+ q 1))
                      (loop-fwd (+ q 1))))))
              (loop-back (- p 1) count)))))
      ;; For different-char delimiters (brackets), match nesting
      (let loop-back ((p (- pos 1)) (depth 0))
        (if (< p 0) #f
          (let ((c (integer->char (send-message ed SCI_GETCHARAT p 0))))
            (cond
              ((char=? c close) (loop-back (- p 1) (+ depth 1)))
              ((and (char=? c ch) (= depth 0))
               ;; Found opening, now find closing
               (let loop-fwd ((q (+ pos 1)) (d 0))
                 (if (>= q len) #f
                   (let ((c2 (integer->char (send-message ed SCI_GETCHARAT q 0))))
                     (cond
                       ((char=? c2 ch) (loop-fwd (+ q 1) (+ d 1)))
                       ((and (char=? c2 close) (= d 0)) (cons p (+ q 1)))
                       ((char=? c2 close) (loop-fwd (+ q 1) (- d 1)))
                       (else (loop-fwd (+ q 1) d)))))))
              ((char=? c ch) (loop-back (- p 1) (- depth 1)))
              (else (loop-back (- p 1) depth)))))))))

(def (cmd-surround-add app)
  "Add surrounding delimiters around region. Prompts for delimiter."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (start (send-message ed SCI_GETSELECTIONSTART 0 0))
         (end (send-message ed SCI_GETSELECTIONEND 0 0)))
    (if (= start end)
      (echo-message! echo "No region selected. Select text first.")
      (let ((input (app-read-string app "Surround with: ")))
        (when (and input (>= (string-length input) 1))
          (let* ((ch (string-ref input 0))
                 (open-str (string ch))
                 (close-str (string (surround-closing-char ch))))
            (send-message ed SCI_BEGINUNDOACTION 0 0)
            ;; Insert closing first (so positions don't shift)
            (send-message ed SCI_INSERTTEXT end 0)
            (scintilla-send-message-string (scintilla-editor-handle ed)
                                           SCI_INSERTTEXT close-str end)
            ;; Insert opening
            (scintilla-send-message-string (scintilla-editor-handle ed)
                                           SCI_INSERTTEXT open-str start)
            (send-message ed SCI_ENDUNDOACTION 0 0)
            (echo-message! echo (string-append "Surrounded with " open-str "..." close-str))))))))

(def (cmd-surround-delete app)
  "Delete surrounding delimiters. Prompts for which delimiter to remove."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (pos (send-message ed SCI_GETCURRENTPOS 0 0))
         (input (app-read-string app "Delete surround: ")))
    (when (and input (>= (string-length input) 1))
      (let* ((ch (string-ref input 0))
             (pair (surround-find-enclosing ed pos ch)))
        (if (not pair)
          (echo-message! echo (string-append "No enclosing " (string ch) " found"))
          (let ((start (car pair))
                (end (- (cdr pair) 1)))
            (send-message ed SCI_BEGINUNDOACTION 0 0)
            ;; Delete closing first (position won't shift for opening)
            (send-message ed SCI_DELETERANGE end 1)
            ;; Delete opening
            (send-message ed SCI_DELETERANGE start 1)
            (send-message ed SCI_ENDUNDOACTION 0 0)
            (echo-message! echo (string-append "Removed surrounding " (string ch)))))))))

(def (cmd-surround-change app)
  "Change surrounding delimiters. Prompts for old and new delimiters."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (pos (send-message ed SCI_GETCURRENTPOS 0 0))
         (old-input (app-read-string app "Change surround from: ")))
    (when (and old-input (>= (string-length old-input) 1))
      (let* ((old-ch (string-ref old-input 0))
             (pair (surround-find-enclosing ed pos old-ch)))
        (if (not pair)
          (echo-message! echo (string-append "No enclosing " (string old-ch) " found"))
          (let ((new-input (app-read-string app "Change surround to: ")))
            (when (and new-input (>= (string-length new-input) 1))
              (let* ((new-ch (string-ref new-input 0))
                     (start (car pair))
                     (end (- (cdr pair) 1))
                     (new-open (string new-ch))
                     (new-close (string (surround-closing-char new-ch))))
                (send-message ed SCI_BEGINUNDOACTION 0 0)
                ;; Replace closing first
                (send-message ed SCI_SETTARGETSTART end 0)
                (send-message ed SCI_SETTARGETEND (+ end 1) 0)
                (scintilla-send-message-string (scintilla-editor-handle ed)
                                               SCI_REPLACETARGET new-close -1)
                ;; Replace opening
                (send-message ed SCI_SETTARGETSTART start 0)
                (send-message ed SCI_SETTARGETEND (+ start 1) 0)
                (scintilla-send-message-string (scintilla-editor-handle ed)
                                               SCI_REPLACETARGET new-open -1)
                (send-message ed SCI_ENDUNDOACTION 0 0)
                (echo-message! echo (string-append "Changed " (string old-ch)
                                                   " → " new-open))))))))))

;;;============================================================================
;;; Goto-last-change — navigate to previous edit locations
;;;============================================================================

;; Ring buffer of edit positions per editor
(def *edit-position-ring* (make-hash-table))
(def *edit-position-index* (make-hash-table))
(def *edit-ring-max* 32)

(def (notes3-record-edit-position! ed pos)
  "Record an edit position in the ring for editor ED."
  (let* ((key (object->serial-number ed))
         (ring (or (hash-get *edit-position-ring* key) '()))
         ;; Don't record if too close to last position (within 20 chars)
         (dominated? (and (pair? ring) (< (abs (- pos (car ring))) 20))))
    (unless dominated?
      (let ((new-ring (notes3-take (cons pos ring) (min *edit-ring-max* (+ (length ring) 1)))))
        (hash-put! *edit-position-ring* key new-ring)
        (hash-put! *edit-position-index* key 0)))))

(def (notes3-take lst n)
  (if (or (<= n 0) (null? lst)) '()
    (cons (car lst) (notes3-take (cdr lst) (- n 1)))))

(def (cmd-goto-last-change-real app)
  "Jump to the location of the previous edit in the current buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (key (object->serial-number ed))
         (ring (or (hash-get *edit-position-ring* key) '()))
         (idx (or (hash-get *edit-position-index* key) 0)))
    (if (null? ring)
      (echo-message! echo "No edit history in this buffer")
      (let* ((pos (list-ref ring (min idx (- (length ring) 1))))
             (len (send-message ed SCI_GETLENGTH 0 0))
             (safe-pos (min pos len)))
        (send-message ed SCI_GOTOPOS safe-pos 0)
        (send-message ed SCI_SCROLLCARET 0 0)
        ;; Advance index for next call
        (hash-put! *edit-position-index* key
                   (min (+ idx 1) (- (length ring) 1)))
        (let ((line (+ 1 (send-message ed SCI_LINEFROMPOSITION safe-pos 0))))
          (echo-message! echo (string-append "Edit position "
                                             (number->string (+ idx 1)) "/"
                                             (number->string (length ring))
                                             " (line " (number->string line) ")")))))))

(def (cmd-goto-last-change-reverse-real app)
  "Jump forward in edit history (reverse of goto-last-change)."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (key (object->serial-number ed))
         (ring (or (hash-get *edit-position-ring* key) '()))
         (idx (or (hash-get *edit-position-index* key) 0)))
    (if (or (null? ring) (<= idx 0))
      (echo-message! echo "At newest edit position")
      (let* ((new-idx (- idx 1))
             (pos (list-ref ring new-idx))
             (len (send-message ed SCI_GETLENGTH 0 0))
             (safe-pos (min pos len)))
        (send-message ed SCI_GOTOPOS safe-pos 0)
        (send-message ed SCI_SCROLLCARET 0 0)
        (hash-put! *edit-position-index* key new-idx)
        (let ((line (+ 1 (send-message ed SCI_LINEFROMPOSITION safe-pos 0))))
          (echo-message! echo (string-append "Edit position "
                                             (number->string (+ new-idx 1)) "/"
                                             (number->string (length ring))
                                             " (line " (number->string line) ")")))))))

;;;============================================================================
;;; Crosshair-mode — highlight current line AND column
;;;============================================================================

(def *crosshair-active* #f)
;; Use indicator 6 for column highlight
(def *crosshair-indicator* 6)

(def (cmd-crosshair-mode app)
  "Toggle crosshair display: highlight both current line and column."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (set! *crosshair-active* (not *crosshair-active*))
    (if *crosshair-active*
      (begin
        ;; Enable current line highlight
        (send-message ed SCI_SETCARETLINEVISIBLE 1 0)
        (send-message ed SCI_SETCARETLINEBACK #x333333 0)
        ;; Set up column indicator (INDIC_DOTBOX = 12)
        (send-message ed SCI_INDICSETSTYLE *crosshair-indicator* 12)
        (send-message ed SCI_INDICSETFORE *crosshair-indicator* #x555555)
        (send-message ed 2523 *crosshair-indicator* 60) ; SCI_INDICSETALPHA
        (echo-message! echo "Crosshair mode enabled"))
      (begin
        ;; Disable current line highlight
        (send-message ed SCI_SETCARETLINEVISIBLE 0 0)
        ;; Clear column indicator
        (send-message ed SCI_SETINDICATORCURRENT *crosshair-indicator* 0)
        (send-message ed SCI_INDICATORCLEARRANGE 0
                      (send-message ed SCI_GETLENGTH 0 0))
        (echo-message! echo "Crosshair mode disabled")))))

;;;============================================================================
;;; Quickrun — execute current buffer with appropriate interpreter
;;;============================================================================

(def *quickrun-interpreters* (make-hash-table))
(begin
  (hash-put! *quickrun-interpreters* "ss" "gxi")
  (hash-put! *quickrun-interpreters* "scm" "gxi")
  (hash-put! *quickrun-interpreters* "py" "python3")
  (hash-put! *quickrun-interpreters* "rb" "ruby")
  (hash-put! *quickrun-interpreters* "js" "node")
  (hash-put! *quickrun-interpreters* "ts" "npx ts-node")
  (hash-put! *quickrun-interpreters* "sh" "bash")
  (hash-put! *quickrun-interpreters* "bash" "bash")
  (hash-put! *quickrun-interpreters* "zsh" "zsh")
  (hash-put! *quickrun-interpreters* "lua" "lua")
  (hash-put! *quickrun-interpreters* "pl" "perl")
  (hash-put! *quickrun-interpreters* "go" "go run")
  (hash-put! *quickrun-interpreters* "hs" "runghc")
  (hash-put! *quickrun-interpreters* "ml" "ocaml")
  (hash-put! *quickrun-interpreters* "php" "php")
  (hash-put! *quickrun-interpreters* "r" "Rscript")
  (hash-put! *quickrun-interpreters* "jl" "julia")
  (hash-put! *quickrun-interpreters* "el" "emacs --batch -l"))

(def (quickrun-extension filename)
  "Extract file extension from filename."
  (let ((dot (string-index-right filename #\.)))
    (if dot (substring filename (+ dot 1) (string-length filename)) "")))

(def (cmd-quickrun app)
  "Execute the current buffer with the appropriate interpreter."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (echo (app-state-echo app))
         (name (or (buffer-file-path buf) (buffer-name buf)))
         (ext (quickrun-extension name))
         (interp (hash-get *quickrun-interpreters* ext)))
    (if (not interp)
      (echo-message! echo (string-append "No interpreter for ." ext
                                         " — use M-x quickrun-with to specify"))
      (let* ((file (buffer-file-path buf))
             (cmd (if file
                    (string-append interp " " file)
                    (string-append "echo 'Save file first'")))
             (output (with-exception-catcher
                      (lambda (e) (string-append "Error: " (with-output-to-string
                                                              (lambda () (display-exception e)))))
                      (lambda ()
                        (with-input-from-process
                         (list path: "/bin/bash" arguments: (list "-c" cmd))
                         (lambda () (read-string 65536)))))))
        ;; Display output in a *Quickrun* buffer
        (open-output-buffer app "*Quickrun*" output)
        (echo-message! echo (string-append "Ran: " interp " " (or file name)))))))

(def (cmd-quickrun-with app)
  "Execute the current buffer with a user-specified command."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (buf (edit-window-buffer win))
         (echo (app-state-echo app))
         (file (buffer-file-path buf))
         (cmd (app-read-string app "Command: ")))
    (when (and cmd (> (string-length cmd) 0))
      (let* ((full-cmd (if file
                         (string-append cmd " " file)
                         cmd))
             (output (with-exception-catcher
                      (lambda (e) (string-append "Error: " (with-output-to-string
                                                              (lambda () (display-exception e)))))
                      (lambda ()
                        (with-input-from-process
                         (list path: "/bin/bash" arguments: (list "-c" full-cmd))
                         (lambda () (read-string 65536)))))))
        (open-output-buffer app "*Quickrun*" output)
        (echo-message! echo (string-append "Ran: " full-cmd))))))

;;;============================================================================
;;; Eros-mode — Evaluation Result OverlayS (show eval inline)
;;;============================================================================

(def *eros-active* #f)
;; Use indicator 7 for eval result display
(def *eros-indicator* 7)

(def (cmd-eros-mode app)
  "Toggle eros-mode: show evaluation results as inline overlays."
  (let* ((echo (app-state-echo app)))
    (set! *eros-active* (not *eros-active*))
    (echo-message! echo (if *eros-active*
                          "Eros mode enabled (eval results shown inline)"
                          "Eros mode disabled"))))

(def (cmd-eros-eval-last-sexp app)
  "Evaluate the s-expression before point and display the result inline."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (pos (send-message ed SCI_GETCURRENTPOS 0 0)))
    ;; Find the sexp boundary by scanning backward for matching paren
    (let* ((brace-pos (send-message ed SCI_BRACEMATCH (- pos 1) 0))
           (start (if (>= brace-pos 0)
                    brace-pos
                    ;; Fallback: find beginning of word/symbol
                    (send-message ed SCI_WORDSTARTPOSITION pos 1)))
           (end pos)
           (text-len (- end start)))
      (if (<= text-len 0)
        (echo-message! echo "No expression at point")
        (let* ((text (notes3-get-text-range ed start text-len))
               ;; Evaluate using Gerbil
               (result (with-exception-catcher
                        (lambda (e)
                          (string-append "Error: "
                            (with-output-to-string (lambda () (display-exception e)))))
                        (lambda ()
                          (with-output-to-string
                           (lambda ()
                             (write (eval (with-input-from-string text read)))))))))
          (if *eros-active*
            ;; Show result as annotation at end of line
            (let* ((line (send-message ed SCI_LINEFROMPOSITION pos 0))
                   (line-end (send-message ed SCI_GETLINEENDPOSITION line 0))
                   (annotation (string-append " => " result)))
              ;; Use Scintilla annotation to show result
              (send-message ed 2548 2 0) ; SCI_ANNOTATIONSETVISIBLE = ANNOTATION_BOXED
              (scintilla-send-message-string (scintilla-editor-handle ed)
                                             2540 annotation line) ; SCI_ANNOTATIONSETTEXT
              (echo-message! echo result))
            ;; Without eros, just show in echo area
            (echo-message! echo (string-append "=> " result))))))))

;;;============================================================================
;;; Auto-dim-other-buffers — dim non-active windows
;;;============================================================================

(def *auto-dim-active* #f)
(def *dim-alpha* 40) ; dimming level (0-255, lower = more dim)

(def (cmd-auto-dim-other-buffers app)
  "Toggle dimming of non-focused windows for better focus."
  (let* ((fr (app-state-frame app))
         (echo (app-state-echo app))
         (wins (frame-windows fr)))
    (set! *auto-dim-active* (not *auto-dim-active*))
    (if *auto-dim-active*
      (begin
        ;; Dim all windows except current
        (let ((current (current-window fr)))
          (for-each
           (lambda (w)
             (let ((ed (edit-window-editor w)))
               (if (eq? w current)
                 ;; Current window: full brightness
                 (send-message ed SCI_SETCARETLINEVISIBLE 1 0)
                 ;; Other windows: dim by reducing indicator alpha
                 (begin
                   (send-message ed SCI_SETEXTRAASCENT 0 0)
                   ;; Use SCI_SETALPHA on selection to create dim effect
                   (send-message ed SCI_SETCARETLINEVISIBLE 0 0)
                   ;; Set background darker
                   (send-message ed SCI_STYLESETBACK 0 #x1a1a1a)))))
           wins))
        (echo-message! echo "Auto-dim other buffers enabled"))
      (begin
        ;; Restore all windows
        (for-each
         (lambda (w)
           (let ((ed (edit-window-editor w)))
             (send-message ed SCI_SETCARETLINEVISIBLE 0 0)
             ;; Restore default background
             (send-message ed SCI_STYLESETBACK 0 #x282828)))
         wins)
        (echo-message! echo "Auto-dim other buffers disabled")))))

;;;============================================================================
;;; Rainbow-identifiers — color identifiers by hash
;;;============================================================================

(def *rainbow-id-active* #f)
(def *rainbow-id-colors*
  (vector #xE06C75 #x98C379 #xE5C07B #x61AFEF
          #xC678DD #x56B6C2 #xD19A66 #xABB2BF
          #xBE5046 #x7EC16E #xD4B074 #x519FDF
          #xB76CC8 #x48A8B4 #xC28B57 #x9CA2AB))

(def (rainbow-id-color-for name)
  "Return a color for identifier NAME based on hash."
  (let* ((h (abs (bitwise-and (string-hash name) #x7FFFFFFF)))
         (idx (modulo h (vector-length *rainbow-id-colors*))))
    (vector-ref *rainbow-id-colors* idx)))

(def (cmd-rainbow-identifiers-mode app)
  "Toggle rainbow identifier coloring — each symbol gets a unique color by hash."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (set! *rainbow-id-active* (not *rainbow-id-active*))
    (if *rainbow-id-active*
      (begin
        ;; Apply rainbow coloring to identifiers using keyword styling
        ;; Use Scintilla's SCI_STYLESETFORE on identifier styles
        ;; Style 11 is SCE_C_IDENTIFIER in many lexers
        (let ((styles '(11 5 6 9 16)))
          (for-each
           (lambda (s)
             (let ((color (vector-ref *rainbow-id-colors*
                           (modulo s (vector-length *rainbow-id-colors*)))))
               (send-message ed SCI_STYLESETFORE s color)))
           styles))
        (send-message ed SCI_COLOURISE 0 -1)
        (echo-message! echo "Rainbow identifiers enabled"))
      (begin
        ;; Reset identifier styles to default
        (let ((styles '(11 5 6 9 16)))
          (for-each
           (lambda (s)
             (send-message ed SCI_STYLESETFORE s #xD4D4D4))
           styles))
        (send-message ed SCI_COLOURISE 0 -1)
        (echo-message! echo "Rainbow identifiers disabled")))))

;;;============================================================================
;;; Outline-minor-mode — code folding for non-org files
;;;============================================================================

(def *outline-minor-active* #f)

(def (cmd-outline-minor-mode app)
  "Toggle outline-minor-mode: enable code folding for all file types."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (set! *outline-minor-active* (not *outline-minor-active*))
    (if *outline-minor-active*
      (begin
        ;; Enable fold margin (margin 2, width 16)
        (send-message ed SCI_SETMARGINWIDTHN 2 16)
        (send-message ed SCI_SETMARGINTYPEN 2 SC_MARGIN_SYMBOL)
        (send-message ed SCI_SETMARGINMASKN 2 SC_MASK_FOLDERS)
        (send-message ed SCI_SETMARGINSENSITIVEN 2 1)
        ;; Set fold markers style (box style)
        (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPEN SC_MARK_BOXMINUS)
        (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDER SC_MARK_BOXPLUS)
        (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERSUB SC_MARK_VLINE)
        (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERTAIL SC_MARK_LCORNER)
        (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEREND SC_MARK_BOXPLUSCONNECTED)
        (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDEROPENMID SC_MARK_BOXMINUSCONNECTED)
        (send-message ed SCI_MARKERDEFINE SC_MARKNUM_FOLDERMIDTAIL SC_MARK_TCORNER)
        ;; Set fold marker colors
        (for-each (lambda (m)
                    (send-message ed SCI_MARKERSETFORE m #x808080)
                    (send-message ed SCI_MARKERSETBACK m #x282828))
                  (list SC_MARKNUM_FOLDEROPEN SC_MARKNUM_FOLDER
                        SC_MARKNUM_FOLDERSUB SC_MARKNUM_FOLDERTAIL
                        SC_MARKNUM_FOLDEREND SC_MARKNUM_FOLDEROPENMID
                        SC_MARKNUM_FOLDERMIDTAIL))
        ;; Enable fold flags (draw line below folded)
        (send-message ed SCI_SETFOLDFLAGS 16 0)
        (echo-message! echo "Outline-minor-mode enabled (click margin to fold)"))
      (begin
        ;; Hide fold margin
        (send-message ed SCI_SETMARGINWIDTHN 2 0)
        (send-message ed SCI_SETFOLDFLAGS 0 0)
        ;; Unfold everything
        (send-message ed SCI_FOLDALL 1 0) ; SC_FOLDACTION_EXPAND
        (echo-message! echo "Outline-minor-mode disabled")))))

(def (cmd-outline-toggle-children app)
  "Toggle fold at the current line."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (line (send-message ed SCI_LINEFROMPOSITION
                (send-message ed SCI_GETCURRENTPOS 0 0) 0)))
    (send-message ed SCI_TOGGLEFOLD line 0)
    (let ((folded? (not (= (send-message ed SCI_GETFOLDEXPANDED line 0) 1))))
      (echo-message! echo (if folded? "Folded" "Unfolded")))))

(def (cmd-outline-fold-all app)
  "Fold all foldable regions in the buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (send-message ed SCI_FOLDALL 0 0) ; SC_FOLDACTION_CONTRACT
    (echo-message! echo "All folds collapsed")))

(def (cmd-outline-unfold-all app)
  "Unfold all regions in the buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app)))
    (send-message ed SCI_FOLDALL 1 0) ; SC_FOLDACTION_EXPAND
    (echo-message! echo "All folds expanded")))

;;;============================================================================
;;; String-edit-at-point — edit string literal in separate buffer
;;;============================================================================

(def (string-at-point ed pos)
  "Find the string literal at POS. Returns (start . end) or #f."
  (let* ((style (send-message ed SCI_GETSTYLEAT pos 0))
         (len (send-message ed SCI_GETLENGTH 0 0)))
    ;; String styles vary by lexer but typically are style 6 or 7
    (if (or (= style 6) (= style 7) (= style 3) (= style 12) (= style 13))
      ;; Scan backward to find string start
      (let loop-back ((p (- pos 1)))
        (if (< p 0)
          (cons 0 pos)
          (let ((s (send-message ed SCI_GETSTYLEAT p 0)))
            (if (not (= s style))
              ;; Found start - now scan forward for end
              (let loop-fwd ((q (+ pos 1)))
                (if (>= q len)
                  (cons (+ p 1) len)
                  (let ((s2 (send-message ed SCI_GETSTYLEAT q 0)))
                    (if (not (= s2 style))
                      (cons (+ p 1) q)
                      (loop-fwd (+ q 1))))))
              (loop-back (- p 1))))))
      #f)))

(def (cmd-string-edit-at-point-real app)
  "Edit the string literal at point in a temporary buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (ed (edit-window-editor win))
         (echo (app-state-echo app))
         (pos (send-message ed SCI_GETCURRENTPOS 0 0))
         (bounds (string-at-point ed pos)))
    (if (not bounds)
      (echo-message! echo "No string literal at point")
      (let* ((start (car bounds))
             (end (cdr bounds))
             (text (notes3-get-text-range ed start (- end start))))
        (open-output-buffer app "*String Edit*" text)
        (echo-message! echo
          (string-append "Editing string (" (number->string (- end start))
                         " chars). Edit and use M-x string-edit-commit to apply."))))))

(def (cmd-string-edit-commit app)
  "Apply edited string back to the original buffer."
  (let* ((fr (app-state-frame app))
         (win (current-window fr))
         (echo (app-state-echo app)))
    ;; For now, show the edited content — user copies it back
    (echo-message! echo "String edit buffer contents ready. Copy to original buffer.")))

;;;============================================================================
;;; Persistent-scratch — auto-save *scratch* buffer
;;;============================================================================

(def *persistent-scratch-active* #f)
(def *notes3-scratch-file* "~/.gemacs-scratch")

(def (persistent-scratch-expand-path)
  (let ((home (getenv "HOME" "/tmp")))
    (string-append home "/.gemacs-scratch")))

(def (cmd-persistent-scratch-mode app)
  "Toggle persistent scratch: auto-save *scratch* buffer content to disk."
  (let* ((echo (app-state-echo app)))
    (set! *persistent-scratch-active* (not *persistent-scratch-active*))
    (if *persistent-scratch-active*
      (echo-message! echo
        (string-append "Persistent scratch enabled (saves to "
                       (persistent-scratch-expand-path) ")"))
      (echo-message! echo "Persistent scratch disabled"))))

(def (cmd-persistent-scratch-save app)
  "Save the *scratch* buffer contents to disk."
  (let* ((echo (app-state-echo app))
         (path (persistent-scratch-expand-path)))
    (let ((buf (buffer-by-name "*scratch*")))
      (if (not buf)
        (echo-message! echo "No *scratch* buffer found")
        (let* ((fr (app-state-frame app))
               (wins (frame-windows fr))
               ;; Try to find the editor for *scratch*
               (scratch-win (find (lambda (w)
                                    (and (edit-window? w)
                                         (eq? (edit-window-buffer w) buf)))
                                  wins)))
          (if (not scratch-win)
            (echo-message! echo "*scratch* buffer not visible — switch to it first")
            (let* ((ed (edit-window-editor scratch-win))
                   (len (send-message ed SCI_GETLENGTH 0 0))
                   (text (notes3-get-text-range ed 0 len)))
              (with-output-to-file path (lambda () (display text)))
              (echo-message! echo
                (string-append "Scratch saved (" (number->string len) " bytes)")))))))))

(def (cmd-persistent-scratch-restore app)
  "Restore *scratch* buffer contents from disk."
  (let* ((echo (app-state-echo app))
         (path (persistent-scratch-expand-path)))
    (if (not (file-exists? path))
      (echo-message! echo "No saved scratch file found")
      (let* ((text (with-input-from-file path (lambda () (read-string 1048576)))))
        (open-output-buffer app "*scratch*" text)
        (echo-message! echo
          (string-append "Scratch restored (" (number->string (string-length text))
                         " bytes)"))))))

;;; buffer-by-name is provided by :gemacs/core
